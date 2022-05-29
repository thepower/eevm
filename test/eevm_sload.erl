-module(eevm_sload).

-include_lib("eunit/include/eunit.hrl").

sload() ->
  Asm= <<"
PUSH1 0x10 
sload
push1 1
add
dup1
push1 0x10
sstore
push1 0
mstore
push1 32
push1 0
return
">>,
  io:format("~s~n",[Asm]),
  Code=eevm_asm:assemble(Asm),
  (Code).

sload_test() ->
  Code=sload(),
  {done,_Ret,State}=eevm:eval(
                     Code,
                     #{},
                     #{gas=>100000000,
                       extra=>#{},
                       sload=>fun(Address,Key,Xtra) ->
                                  io:format("Load key ~p of address ~p~n",
                                            [Key,Address]),
                                  {ok, 35, Xtra}
                              end,
                       data=>#{
                               address=>16#100,
                               callvalue=>0,
                               caller=>16#101,
                               gasprice=>10,
                               origin=>16#100
                              },
                       trace=>whereis(eevm_tracer)
                      }),
  [
   ?assertMatch(#{storage:=#{16:=36}}, State),
   ?assert(true)
  ].

sload2_test() ->
  Code=sload(),
  {done,_Ret,State}=eevm:eval(
                     Code,
                     #{},
                     #{gas=>100000000,
                       extra=>#{},
                       sload=>fun(Key) ->
                                  io:format("Load key ~p of current address~n",
                                            [Key]),
                                  35
                              end,
                       data=>#{
                               address=>16#100,
                               callvalue=>0,
                               caller=>16#101,
                               gasprice=>10,
                               origin=>16#100
                              },
                       trace=>whereis(eevm_tracer)
                      }),
  [
   ?assertMatch(#{storage:=#{16:=36}}, State),
   ?assert(true)
  ].


