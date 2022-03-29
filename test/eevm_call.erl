-module(eevm_call).

-include_lib("eunit/include/eunit.hrl").

call1_test() ->
  Code=eevm:asm(eevm:parse_asm(
<<"
push16 38
push32 0x63000003e8600055716000546001018060005560005260206000F360701B6000
push1 0
mstore
push6 0x5260126000F3
push1 208
shl
push1 32
mstore

dup1
PUSH1 0
PUSH1 0
CREATE

push1 0
push1 0
push1 0
push1 0
push1 0
dup6
push3 262144
call
push1 0
push1 0
push1 0
push1 0
push1 0
dup7
push3 262144
call

returndatasize
dup1
push1 0
push1 0
returndatacopy
push1 0
return

">>)),
  {done,Ret,State}=eevm:runtest(Code,16#100,16#101,0,#{}),
  %Res=maps:with([extra,memory,stack,storage,gas], State),
  [
   ?assertMatch({return,<<1002:256/big>>},Ret),
   ?assertMatch(#{extra:=#{}}, State),
   ?assert(true)
  ].

