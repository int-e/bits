?
| A Brainfuck interpreter with 16 bit cells and -1 on EOF.
|
| Program is separated from input by an exclamation mark (!).
|
| Limitations:
|   maximum loop nesting depth: 63
|   data area + program size: ~1900 cells
?

?
| Generic memory setup.
?
BB=4, ? word size is B = 2^BB ?
S=11, ? memory size is 2^S words, where S < B - BB ?
?
| * conventions
| 
| A ("accumulator") consists of bits 0.. W-1 of the accumulator
| X ("extended")    consists of bits W..2W-1 of the accumulator
| 
| To use a binary operation, X is loaded with the second operand.
| X is cleared by most operations.
| 
| * memory
| 
| Random access memory is encoded in the storage; for each word, the
| word is stored, followed by its address plus 2*W.
| 
| For example, for bits=16 and bsize = 11:
| 
|   storage = <word 0> 0020 <word 1> 0040 <word 2> ... <word 0x7FE> FFE0
| 
| The n-th word has address 2^(bbits+1)*n, and word 2^bsize - 1 is not
| available.
| 
| The stored addresses are used by the POKE operation.
| 
| * registers
| 
| random access quite a bit harder than accessing fixed addresses, so it's
| useful to repurpose part of the memory for quick-access registers;
| register r is stored at bits W*r..W*(r+1)-1 of the storage.
?
B={1<<BB},       ? word size in bits ?
AB={BB+1},
A={1<<AB},       ? address alignment ?
M(B)={(1<<B)-1},
imm(n)=^^n,
immx(n)=]B imm(n)[B,
cla=&{-1<<B},
clx=&M,
and=&]B,
or =|]B clx,
xor=^]B clx,
not=~clx,
subaux(b,n)=if(b,|[b)for(i=-n,-1,[{-i}^]{b-i}]{-i} if(i<-1,&[{b-i}))&0,
dec(B)=]B|1subaux(0,B)[B,
inc(B)=~]B|1subaux(0,B)~[B~,
sub(B)=]B subaux({B-1},B)[B,
add(B)=~]B~subaux({B-1},B)~[B~,
init_mem=>dec(S)]A|[A;<[{A+AB},
peek=^^[|B clx,
poke=[&M[B cla^^[B^]&M^^]B[B]&M clx,
get(r)=^^[{B*(r+1)}clx,
put(r)=[{B*(r+1)}cla|]{B*(r+1)}]{B*(r+1)},
getx(r)=]B get({r+1})[B,
?
| end of generic memory setup
?
?,init_mem imm(64)immx(42)%poke%peek%,?

?
| A Brainfuck interpreter with 16 bit cells.
|
| limitations: maximum loop nesting depth: 63
| data area + program size: ~1900 cells
?
P=0,
C=1,
D=2,
W=3,
T=4,
U=6,
Z=5,
INIT=
  init_mem
  imm({4*A}) put(P) put(C)
  > 
    get(C)
    ]B ^^\ put({T+1}) [B
    poke
    immx(A) add put(C)
    get(T)
    ^33
  ;<
  get(C)
  immx({128*A})
  add
  put(D)
  imm(1) put(W)
,
LOOP=
  >
    get(D) peek put(Z)
    get(P) peek ^33 :.
    get(P) peek ^43 ;|-1 ~ &1 getx(W) and put(T) get(Z) getx(T) add
      put(Z) get(D) getx(Z) poke
    get(P) peek ^45 ;|-1 ~ &1 getx(W) and put(T) get(Z) getx(T) sub
      put(Z) get(D) getx(Z) poke
    get(P) peek ^44 ;|-1 ~ getx(W) and ;|M put(T)
      ;&\ getx(Z) xor getx(T) and getx(Z) xor
      put(Z) get(D) getx(Z) poke
    get(P) peek ^46 ;|-1 ~ getx(W) and ;|M getx(Z) and ;/
    get(P) peek ^60 ;|-1 ~ getx(W) and ;^{-A^1} getx(D) add put(D)
    get(P) peek ^62 ;|-1 ~ getx(W) and ;^{A^1}  getx(D) add put(D)

    imm(A) getx(C) add getx(P) poke
    imm({2*A}) getx(C) add getx(W) poke

    get(P) peek ^93 ;|-1 ~ &{2*-A} getx(C) add put(C)
    get(P) peek ^93 ;|-1 ~ getx(W) and ;|M put(T)

    imm(A) getx(C) add peek getx(P) xor getx(T) and
      getx(P) xor put(P)
    imm({2*A}) getx(C) add peek put(W)

    get(P) peek ^91 ;|-1 ~ &{2*A} getx(C) add put(C)
    get(P) peek ^91 ;|-1 ~ getx(W) and ;|M put(T)
      get(Z) ;|1 getx(W) and getx(W) xor getx(T) and getx(W) xor put(W)

    get(P) immx(A) add put(P)
  <
,
,INIT LOOP,
