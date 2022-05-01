-module(eevm_code).

-include_lib("eunit/include/eunit.hrl").

extcode_test() ->
  Code=eevm:asm([
                 {push,32,16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF},
                 {push,1,0},
                 mstore,
                 {push,32,16#FF60005260206000F30000000000000000000000000000000000000000000000},
                 {push,1,32},
                 mstore,
                 % Create the contract with the constructor code above
                 {push,1,41},
                 {push,1,0},
                 {push,1,0},
                 create, %Puts the new contract address on the stack
                 {dup,1},
                 % The address is on the stack, we can query the size
                 extcodesize,
                 {swap,1},

                 {push,1, 0}, %clear memory
                 {push,1, 0},
                 mstore,
                 {push,1, 0},
                 {push,1, 32},
                 mstore,

                 {push,1, 32},
                 {push,1, 0},
                 {push,1, 0},
                 {dup,4},
                 extcodecopy,

                 {push,1, 8},
                 {push,1, 31},
                 {push,1, 0},
                 {dup,4},
                 extcodecopy
                ]),
  {done,_,State}=eevm:runtest(Code,16#100,16#101,0,#{}),
  Res=maps:with([extra,memory,stack,storage,gas], State),
  [
   ?assertMatch(#{stack:=[_,32]},Res),
   ?assertMatch(#{memory:= <<255,0,0,0,0,0,0,0,
                             255,255,255,255,255,255,255,255,
                             255,255,255,255,255,255,255,255,
                             255,255,255,255,255,255,255,255,_/binary>>},Res)
  ].
