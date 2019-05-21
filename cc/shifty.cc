// shifty is a "bitch" interpreter
//
// original:
//   https://github.com/Helen0903/bitch
//
// language description:
//   https://esolangs.org/wiki/bitch
//
// Contrary to its name this interpreter /avoids/ shifting the state
// around to a large extent. See the comment at `class state` below for
// details.
//
// To compile:
//   g++ -Wall -O2 shifty.cc -o shifty -lgmpxx -lgmp

#include <fstream>
#include <iostream>
#include <memory>
#include <vector>
#include <cinttypes>
#include <cstdlib>
#include <cstring>
#include <gmpxx.h>

#define MAX_SHIFT (1 << 24)

namespace bitch {

typedef uint8_t byte;

// Operations

enum OP {
    MARK,  // '>'
    RESET, // '<'
    NOT,   // '~'
    DUMP,  // '%'
    STOP,  // '.'
    READ,  // '\'
    WRITE, // '/'
    LIT,   // nnn

    AND,   // '&'
    OR,    // '|'
    XOR,   // '^'
    SHL,   // '['
    SHR,   // ']'
    CLS,   // '#'

    EQZ,   // ':'
    NEZ,   // ';'
};

class op
{
protected:
    explicit op(OP type_);

public:
    op(op &) = delete;
    ~op();
    void operator delete(void *);

    typedef std::unique_ptr<const class op> uptr;

    const OP type;

    friend uptr parse_op(std::istream &is);
};

op::op(OP type_)
    : type(type_)
{ }

class bop : public op
{
protected:
    explicit bop(OP type_, op::uptr &&next_);

public:
    ~bop() = delete;

    uptr next;

    friend op::uptr parse_op(std::istream &is);
};

bop::bop(OP type_, op::uptr &&next_)
    : op(type_), next(std::move(next_))
{ }

class lit : public op
{
protected:
    explicit lit(const mpz_class &val_);

public:
    ~lit() = delete;

    const mpz_class val;

    friend op::uptr parse_op(std::istream &is);
};

lit::lit(const mpz_class &val_)
    : op(LIT), val(val_)
{ }

op::~op()
{
    // this is the price we pay for shunning a virtual destructor...
    switch (type) {
    case MARK: case RESET: case NOT: case DUMP: case STOP:
    case READ: case WRITE:
        break;
    case LIT:
        static_cast<lit *>(this)->val.~mpz_class();
        break;
    case AND: case OR: case XOR: case SHL: case SHR: case CLS:
    case EQZ: case NEZ:
        static_cast<bop *>(this)->next.~uptr();
        break;
    }
}

void op::operator delete(void *p)
{
    switch (static_cast<op *>(p)->type) {
    case MARK: case RESET: case NOT: case DUMP: case STOP:
    case READ: case WRITE:
        ::operator delete(p, sizeof(op));
        break;
    case LIT:
        ::operator delete(p, sizeof(lit));
        break;
    case AND: case OR: case XOR: case SHL: case SHR: case CLS:
    case EQZ: case NEZ:
        ::operator delete(p, sizeof(bop));
        break;
    }
}

// Parsing

typedef std::vector<op::uptr> program;

class eof
{
public:
    explicit eof();
};

eof::eof()
{ }

op::uptr parse_op(std::istream &is)
{
    char c;

    if (!is.get(c))
        throw eof();

    OP type;

    switch (c) {
    case '>':
        return op::uptr(new op(MARK));
    case '<':
        return op::uptr(new op(RESET));
    case '~':
        return op::uptr(new op(NOT));
    case '%':
        return op::uptr(new op(DUMP));
    case '.':
        return op::uptr(new op(STOP));
    case '\\':
        return op::uptr(new bop(CLS, op::uptr(new op(READ))));
    case '/':
        return op::uptr(new op(WRITE));

    case '&':
        type = AND;
        break;
    case '|':
        type = OR;
        break;
    case '^':
        type = XOR;
        break;
    case '[':
        type = SHL;
        break;
    case ']':
        type = SHR;
        break;
    case '#':
        type = CLS;
        break;

    case ':':
        type = EQZ;
        break;
    case ';':
        type = NEZ;
        break;

    default:
        throw eof();
    };

    switch (is.peek()) {
    case '-':
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9': {
        mpz_class z;
        is >> z;
        return op::uptr(new bop(type, op::uptr(new lit(z))));
    }
    default:
        return op::uptr(new bop(type, parse_op(is)));
    }
}

program parse(std::istream &is)
{
    program prog;
    try {
        for (;;) {
            prog.push_back(std::move(op::uptr(parse_op(is))));
        }
    } catch (eof) {
    };
    return prog;
}

// Program State and Execution

class state
{
    // This class manages the store and the accumulator.
    //
    // The `data` vector contains the store, followed by the accumulator
    // at bit(!) position `pos`. The 'neg' bool indicates whether the
    // accumulator is positive (in which case it is stored directly)
    // or negative (in which case the complement is stored).
    //
    // For example, ]7 adds 7 to the position `pos`. If `neg` is set,
    // it also flips the 7 consecutive bits with index pos..pos+6.

    std::vector<byte> data;
    size_t pos;
    bool neg;

    // the program, program counter, and reset position for '>' / '<'
    program prog;
    size_t pc;
    size_t reset;

    // whether to use character-oriented I/O
    bool char_io;

    mpz_class getz() const;

public:
    state(state &) = delete;
    explicit state(program &&prog_, bool char_io_ = false);

    void dump() const;

    // get the current value of the accumulator, taking `pos` and `neg`
    // into account
    mpz_class getz(size_t ofs) const;
    // replace the value of the accumulator, changing `data` and `neg`
    // as necessary
    void putz(mpz_class z);

    // evaluate a (nested) operation
    mpz_class eval(const op &o) const;
    // execute a (toplevel) operation
    bool exec(const op &o);
    // execute whole program
    void exec();
};

state::state(program &&prog_, bool char_io_)
    : data(), pos(0), neg(false),
      prog(std::move(prog_)), pc(0), reset(0),
      char_io(char_io_)
{
}

void state::dump() const
{
    // TODO: dump some data as well
    std::cerr << "%% pos = " << pos << " neg = " << neg << std::endl;
}

mpz_class state::getz() const
{
    return getz(pos);
}

mpz_class state::getz(size_t ofs) const
{
    mpz_class z(0);
    if (data.size() > ofs/8)
        mpz_import(
            z.get_mpz_t(),       // rop
            data.size() - ofs/8, // count  = number of words
            -1,                  // order  = data endianness = least word first
            1,                   // size   = word size
            0,                   // endian = word endianness = native
            0,                   // nails  = nail bits
            &data[ofs/8]         // op
            );
    z >>= ofs & 7;
    return neg ? ~ z : z;
}

void state::putz(mpz_class z)
{
    neg = (z < 0);
    if (neg)
        z = ~ z;

    mpz_ptr mz = z.get_mpz_t();

    if (pos & 7) {
        // extract and store lower (8 - (pos & 7)) bits.
        data[pos/8] =
            (data[pos/8] & ((1 << (pos & 7)) - 1))
            | (z.get_ui() << (pos & 7));
        z >>= (8 - (pos & 7));
    }
    size_t sz = (mpz_sizeinbase(mz, 2) + 7) / 8;
    data.resize(sz + (pos+7)/8);
    size_t count;
    mpz_export(
        &data[(pos+7)/8],        // rop
        &count,                  // pcount - number of words written
        -1,                      // order  - matches above
        1,                       // size   - matches above
        0,                       // endian - matches above
        0,                       // nails  - matches above
        mz                       // op
        );
    for ( ; count < sz; count++)
        data[(pos+7)/8 + count] = 0;
}

mpz_class state::eval(const op &o) const
{
    switch (o.type) {
    case MARK:
    case RESET:
    case STOP:
        return getz();
    case NOT:
        return ~ getz();
    case DUMP:
        dump();
        return getz();
    case READ: {
        if (char_io) {
            char c;
            if (std::cin.get(c))
                return static_cast<unsigned char>(c);
            else
                return -1;
        } else {
            mpz_class z;
            std::cin >> z;
            if (std::cin.good())
                return z;
            else
                return -1;
        }
    }
    case WRITE: {
        mpz_class z = getz();
        if (char_io)
            std::cout << static_cast<unsigned char>(z.get_ui());
        else
            std::cout << z << std::endl;
        return z;
    }
    case LIT:
        return static_cast<const lit &>(o).val;
    case AND:
        return getz() & eval(*static_cast<const bop&>(o).next);
    case OR:
        return getz() | eval(*static_cast<const bop&>(o).next);
    case XOR:
        return getz() ^ eval(*static_cast<const bop&>(o).next);
    case SHL: {
        mpz_class shiftz = eval(*static_cast<const bop&>(o).next);
        if (shiftz <= 0)
            return getz();
        if (shiftz > MAX_SHIFT) {
            std::cerr << "SHL amount too big: " << shiftz << std::endl;
            std::exit(1);
        }
        size_t shift = shiftz.get_ui();
        mpz_class z;
        if (pos >= shift) {
            z = getz(pos - shift);
        } else {
            mpz_class z = getz(0);
            z <<= shift - pos;
        }
        if (neg)
            // flip bits that were part of the store
            z ^= (mpz_class(1) << shift) - 1;
        return z;
    }
    case SHR: {
        mpz_class shiftz = eval(*static_cast<const bop&>(o).next);
        if (shiftz <= 0)
            return getz();
        if (shiftz > MAX_SHIFT) {
            std::cerr << "SHR amount too big: " << shiftz << std::endl;
            std::exit(1);
        }
        size_t shift = shiftz.get_ui();
        return getz(pos + shift);
    }
    case CLS:
        return eval(*static_cast<const bop&>(o).next);
    case EQZ: {
        mpz_class z = getz();
        if (z == 0)
            return eval(*static_cast<const bop&>(o).next);
        else
            return z;
    }
    case NEZ: {
        mpz_class z = getz();
        if (z != 0)
            return eval(*static_cast<const bop&>(o).next);
        else
            return z;
    }
    }
    // not reached
    return -1;
}

bool state::exec(const op &o)
{
    switch (o.type) {
    case MARK:
        reset = pc;
        break;
    case RESET:
        pc = reset;
        break;
    case NOT:
        neg = !neg;
        break;
    case DUMP:
        dump();
        break;
    case STOP:
        return false;
    case WRITE:
        if (char_io)
            std::cout << static_cast<unsigned char>(getz().get_ui());
        else
            std::cout << getz() << std::endl;
        break;

    case AND:
    case OR:
    case XOR:
        putz(eval(o));
        break;

    case SHL: {
        mpz_class shiftz = eval(*static_cast<const bop&>(o).next);
        if (shiftz <= 0)
            return true;
        if (shiftz > MAX_SHIFT) {
            std::cerr << "SHL amount too big: " << shiftz << std::endl;
            std::exit(1);
        }
        unsigned long shift = shiftz.get_ui();
        if (shift > pos) {
            size_t a = (shift - pos + 7) / 8;
            data.insert(data.begin(), a, 0);
            pos += a*8;
        }
        if (neg) {
            // flip those bits that were part of the store
            data[pos/8] ^= ~ ((1 << (pos & 7)) - 1);
            shift = shift + 8 - (pos & 7);
            pos = pos + 8 - (pos & 7);
            for ( ; shift >= 8; shift -= 8, pos -= 8)
                data[pos/8 - 1] = ~ data[pos/8 - 1];
            pos -= shift;
            data[pos/8] ^= ~ ((1 << (pos & 7)) - 1);
        } else {
            pos -= shift;
        }
        break;
    }
    case SHR: {
        mpz_class shiftz = eval(*static_cast<const bop&>(o).next);
        if (shiftz <= 0)
            return true;
        if (shiftz > MAX_SHIFT) {
            std::cerr << "SHR amount too big: " << shiftz << std::endl;
            std::exit(1);
        }
        unsigned long shift = shiftz.get_ui();
        if ((pos + shift + 7)/8 > data.size())
            data.resize((pos + shift + 7)/8);
        if (neg) {
            // flip those bits that were moved into the store
            data[pos/8] ^= (1 << (pos & 7)) - 1;
            shift += pos & 7;
            pos -= pos & 7;
            for ( ; shift >= 8; shift -= 8, pos += 8)
                data[pos/8] = ~ data[pos/8];
            pos += shift;
            data[pos/8] ^= (1 << (pos & 7)) - 1;
        } else {
            pos += shift;
        }
        break;
    }

    case CLS: {
        mpz_class val = eval(*static_cast<const bop&>(o).next);
        data.clear();
        pos = 0;
        putz(val);
        break;
    }
    case EQZ:
        if (getz() == 0)
            return exec(*static_cast<const bop&>(o).next);
        break;
    case NEZ:
        if (getz() != 0)
            return exec(*static_cast<const bop&>(o).next);
        break;

    case READ:
    case LIT:
        std::cerr << "impossible" << std::endl;
        break;
    }
    return true;
}

void state::exec()
{
    while (pc < prog.size() && exec(*prog[pc++].get())) {
        /* nothing */
    }
}

// Main Worker

static void main(std::istream &is, bool char_io)
{
    state s(parse(is), char_io);
    s.exec();
}

} // namespace bitch

// Main Function

static void usage()
{
    std::cerr <<
        "Usage: shifty [-c] <file>" << std::endl;
    std::exit(1);
}

int main(int argc, char *argv[])
{
    int arg = 1;
    bool char_io = false;

    if (arg < argc && std::strcmp(argv[arg], "-c") == 0) {
        char_io = true;
        arg++;
    }

    if (arg >= argc)
        usage();

    std::ifstream ifs;
    ifs.open(argv[arg], std::ifstream::in);

    if (!ifs.good())
        usage();

    bitch::main(ifs, char_io);

    return 0;
}
