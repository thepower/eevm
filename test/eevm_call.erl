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

pop
returndatasize
dup1
push1 0
push1 0
returndatacopy
push1 0
return

">>)),
  {done,Ret,State}=eevm:runtest(Code,16#100,16#101,0,#{}),
  [
   ?assertMatch({return,<<1002:256/big>>},Ret),
   ?assertMatch(#{extra:=#{}}, State),
   ?assert(true)
  ].

returndatasize_test() ->
  Code=eevm:asm(eevm:parse_asm( <<"
PUSH32 0x7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
PUSH1 0
MSTORE
PUSH32 0xFF6000527FFF60005260206000F3000000000000000000000000000000000000
PUSH1 32
MSTORE
PUSH32 0x000000000060205260296000F300000000000000000000000000000000000000
PUSH1 64
MSTORE

// Create the contract with the constructor code above
PUSH1 77
PUSH1 0
PUSH1 0
CREATE // Puts the new contract address on the stack

// Call the deployed contract
PUSH1   0
PUSH1 0
PUSH1 0
PUSH1 0
DUP5
PUSH4 0xFFFFFFFF
STATICCALL

pop
// Now we should have our return data size of 32
RETURNDATASIZE
    ">>)),

  {done,_,State}=eevm:runtest(Code,16#100,16#101,0,#{}),
  [
   ?assertMatch(#{stack:=[32,_]}, State),
   ?assert(true)
  ].


embedded_call_test() ->
  Code=eevm:asm(eevm:parse_asm( <<"
// First place the parameters in memory
PUSH5 0x68656C6C6F // data
PUSH1 0
MSTORE

// Do the call
PUSH1 0x20 // retSize
PUSH1 0x20 // retOffset
PUSH1 5 // argsSize
PUSH1 27 // argsOffset
PUSH1 2 // address
PUSH4 0xFFFFFFFF // gas
STATICCALL

// Put the result alone on the stack
PUSH1 0x20
MLOAD
    ">>)),

  Sha2 = fun(Data) ->
             Hash=crypto:hash(sha256,Data),
             {1,Hash}
         end,
  {done,_,State}=eevm:eval(Code,
                           #{},
                           #{
                             gas=>1000,
                             get=>#{
                                    code=>fun()->throw(error) end
                                   },
                             embedded_code => #{
                                                2=> Sha2
                                               }
                            }),
  Hash=binary:decode_unsigned(crypto:hash(sha256,<<"hello">>)),
  [
   ?assertMatch(#{stack:=[Hash,_]}, State),
   ?assertMatch(#{stack:=[_,1]}, State),
   ?assert(true)
  ].

