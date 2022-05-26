-module(eevm_call).

-include_lib("eunit/include/eunit.hrl").

sstore_scode() ->
  Code=eevm:asm(eevm:parse_asm(
<<"
push1 0
sload
push1 1
add
dup1
push1 0
mstore
push1 0
sstore

push1 32
push1 0
return
  ">>)),
  Code2Mem=eevm_scratchpad:code2mem(Code,0),

  DeployCode=list_to_binary([
                             Code2Mem,
                             <<"push1 0
                             return">>
                            ]),
  list_to_binary(
    eevm_scratchpad:code2mem(eevm:asm(eevm:parse_asm(DeployCode)),0)
   ).

sstore_code(Call) ->
  C=list_to_binary(
      [
       sstore_scode(),
       <<"
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
">>,
       Call,
       <<"
push1 0
mstore8
returndatasize
dup1
push1 0
push1 1
returndatacopy
push1 1
add
push1 0
return
">>]),
  %io:format("----~n~s~n~n",[C]),
  eevm:asm(eevm:parse_asm(C)).

sstore_static_test() -> %check crash on call sstore inside static call
  Res=lists:map(
  fun(N)->
      Code=sstore_code(N),
      {done,
       {return,Ret},
       #{extra:=X,storage:=St}=_State}=eevm:runtest(Code,16#100,16#101,0,
                                                    #{{16#100,state}=>#{0=>4}}),
      io:format("Call ~10s ~p~n", [N,Ret]),
      io:format("~p~n",[{X,St}]),
      {N,Ret}
  end, [<<"call">>,<<"staticcall">>,<<"callcode">>,<<"delegatecall">>]),
  [
   ?assertMatch({<<"call">>,<<1,1:256/big>>},lists:keyfind(<<"call">>,1,Res)),
   ?assertMatch({<<"staticcall">>,<<0>>},lists:keyfind(<<"staticcall">>,1,Res)),
   ?assertMatch({<<"callcode">>,<<1,5:256/big>>},lists:keyfind(<<"callcode">>,1,Res)),
   ?assertMatch({<<"delegatecall">>,<<1,5:256/big>>},lists:keyfind(<<"delegatecall">>,1,Res)),
   ?assert(true)
  ].



calltestinfo() ->
  Code=eevm:asm(eevm:parse_asm(
<<"
address
push1 0
mstore

origin
push1 32
mstore

caller
push1 64
mstore

callvalue
push1 96
mstore

push1 128
push1 0
return
  ">>)),
  Code2Mem=eevm_scratchpad:code2mem(Code,0),

  DeployCode=list_to_binary([
                             Code2Mem,
                             <<"push1 0
                             return">>
                            ]),
  list_to_binary(
    eevm_scratchpad:code2mem(eevm:asm(eevm:parse_asm(DeployCode)),0)
   ).

callcode(Call) ->
  C=list_to_binary(
      [
       calltestinfo(),
       <<"
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
">>,
       Call,
       <<"
pop
returndatasize
dup1
push1 0
push1 0
returndatacopy
push1 0
return
">>]),
  %io:format("----~n~s~n~n",[C]),
  eevm:asm(eevm:parse_asm(C)).

%  [
%   {parent,
%    16#9bbfed6889322e016e0a02ee459d306fc19545d8, %parent_addr
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %origin
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %caller
%    0
%   },
%   {call,
%    16#230fc3fe9249c6f698bfefea56debde9e1de2934, %child
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %orig_origin
%    16#9bbfed6889322e016e0a02ee459d306fc19545d8, %parent_addr
%    16#0
%   },
%   {staticcall,
%    16#230fc3fe9249c6f698bfefea56debde9e1de2934, %child
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %orig_origin
%    16#9bbfed6889322e016e0a02ee459d306fc19545d8, %parent_addr
%    16#0
%   },
%   {callcode,
%    16#9bbfed6889322e016e0a02ee459d306fc19545d8, %parent_addr
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %orig_origin
%    16#9bbfed6889322e016e0a02ee459d306fc19545d8, %parent_addr
%    16#0
%   },
%   {delegatecall,
%    16#9bbfed6889322e016e0a02ee459d306fc19545d8, %parent_addr
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %orig_origin
%    16#be862ad9abfe6f22bcb087716c7d89a26051f74c, %orig_caller
%    16#0
%   }
%  ].

call2_test() ->
  io:format("Call ~10s ~10s ~10s ~10s ~10s~n", ["","addr","orig","caller","val"]),
  Res=lists:map(
  fun(N)->
      Code=callcode(N),
      {done,
       {return,
        <<A1:256/big,A2:256/big,A3:256/big,A4:256/big>>
       },_State}=eevm:runtest(Code,16#100,16#101,0,#{}),
      io:format("Call ~10s ~10B ~10B ~10B ~10B~n", [N,A1,A2,A3,A4]),
      {N,A1,A2,A3,A4}
  end, [<<"call">>,<<"staticcall">>,<<"callcode">>,<<"delegatecall">>]),
  [
   ?assertMatch({<<"call">>,_,             16#101,16#100,0},lists:keyfind(<<"call">>,1,Res)),
   ?assertMatch({<<"staticcall">>,_,       16#101,16#100,0},lists:keyfind(<<"staticcall">>,1,Res)),
   ?assertMatch({<<"callcode">>,    16#100,16#101,16#100,0},lists:keyfind(<<"callcode">>,1,Res)),
   ?assertMatch({<<"delegatecall">>,16#100,16#101,16#101,0},lists:keyfind(<<"delegatecall">>,1,Res)),
   ?assert(true)
  ].



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

//dup1
//push 0
//push 0
//log1

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


embedded_call2_test() ->
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

embedded_call3_test() ->
  Code=eevm:asm(eevm:parse_asm( <<"
// First place the parameters in memory
PUSH1 0xff // data
PUSH1 0
MSTORE

// Do the call
PUSH1 0x20 // retSize
PUSH1 0x20 // retOffset
PUSH1 1 // argsSize
PUSH1 31 // argsOffset
PUSH1 3 // address
PUSH4 0xFFFFFFFF // gas
STATICCALL

// Put the result alone on the stack
PUSH1 0x20
MLOAD
    ">>)),

  Ripemd = fun(Data) ->
             <<Hash:20/binary,_/binary>> = crypto:hash(ripemd160,Data),
             {1,<<0:96/big,Hash/binary>>}
         end,
  {done,_,State}=eevm:eval(Code,
                           #{},
                           #{
                             gas=>1000,
                             get=>#{
                                    code=>fun()->throw(error) end
                                   },
                             embedded_code => #{
                                                3=> Ripemd
                                               }
                            }),
  Hash=16#2c0c45d3ecab80fe060e5f1d7057cd2f8de5e557,
  [
   ?assertMatch(#{stack:=[Hash,_]}, State),
   ?assertMatch(#{stack:=[_,1]}, State),
   ?assert(true)
  ].

