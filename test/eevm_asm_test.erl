-module(eevm_asm_test).

-include_lib("eunit/include/eunit.hrl").

asm_test() ->
  %do not try to run this code, it's just to test assembler
  Asm= <<"
push label1
push a
PUSH 0x10 
sload ; push 1 ; add
dup1
SSTORE
push 0
mstore
push 32
push 0
return
jumpdest label1
">>,
  Code=eevm_asm:assemble(Asm,#{"a" => 65534}),
  CS=size(Code)-1,
  {I1, R1, _}=eevm_dec:decode(Code),
  {I2,_R2, _}=eevm_dec:decode(R1),
  [
   ?assertMatch({push,_,CS},I1),
   ?assertMatch({push,_,65534},I2)
  ].

