-module(evm).
-export([run/3,run/5,load/1]).

load(Filename) ->
  {ok, ERC20Hex } = file:read_file(Filename),
  [ H | _ ] = binary:split(ERC20Hex, <<"\n">>),
  hex:decode(H).

run(Bytecode,Storage,State0) ->
    Dec=dec:decode(Bytecode),
    %fun() -> {ok,E}=ksha3:hash(256, <<"balances(address)">>), <<X:32/big,_/binary>> = E, io:format("~.16B",[X]) end().
    State=maps:merge(#{
                       stack=>[],
                       gas=>0,
                       cd => <<>>,
                       storage=>Storage,
                       memory=><<>>,
                       caller=>16#ff,
                       value=>0,
                       code=>Bytecode
                      },State0),
    interpret:runcode(Dec,State).

run(Bytecode,Storage,State0,Function,Args) ->
    IFun = fun(B) -> {ok,E}=ksha3:hash(256, B), <<X:32/big,_/binary>> = E,X end(Function),
    %io:format("size ~.16B ~B call ~s (~.16B) ~n",[size(Bytecode),size(Bytecode), Function, IFun]),
    CallData = lists:foldl(
                 fun(Arg,Acc) ->
                     <<Acc/binary,Arg:256/big>>
                 end, << IFun:32/big>>, Args),
    %io:format("Call ~s~n",[hex:encode(CallData)]),

    run(Bytecode,Storage,maps:merge(State0, #{ cd => CallData })).

