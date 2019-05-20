? map c to t, where p is the previous value of c. result is ^c. ?
T(p,c,t)=^{p^c}:^{c^t}, 
,
>
  \
  ~:.~
  ]8|[8|32
  T(0,97,{65+13})                           ? map 'a'      to 'N'      ?
  for(i=1,12,T({96+i},{97+i},{65+13+i}))    ? map 'b'..'m' to 'O'..'Z' ?
  for(i=0,12,T({96+13+i},{97+13+i},{97+i})) ? map 'n'..'z' to 'a'..'m' ?
  ^{65+25}
  [8&{32|(~32<<8)}^]8
  &255
  /
<
,
