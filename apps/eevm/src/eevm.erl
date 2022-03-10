-module(eevm).
-behaviour(application).

-export([eval/3,eval/5,load/1]).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eevm_sup:start_link().

stop(_State) ->
    ok.

load(Filename) ->
  {ok, ERC20Hex } = file:read_file(Filename),
  [ H | _ ] = binary:split(ERC20Hex, <<"\n">>),
  hex:decode(H).

eval(Bytecode,Storage,State0) ->
  Logger=fun(Message,Args) ->
             io:format("LOG: ~p~n\targs ~p~n",[Message,Args])
         end,
    State=maps:merge(#{
                       stack=>[],
                       gas=>0,
                       cd => <<>>,
                       storage=>Storage,
                       memory=><<>>,
                       caller=>16#ff,
                       value=>0,
                       code=>Bytecode,
                       logger=>Logger
                      },State0),
    eevm_interpret:run(State).

eval(Bytecode,Storage,State0,Function,Args) ->
    IFun = fun(B) -> {ok,E}=ksha3:hash(256, B), <<X:32/big,_/binary>> = E,X end(Function),
    [FunName,Args1,_]=binary:split(Function,[<<"(">>,<<")">>],[global]),
    ArgTypes=binary:split(Args1,<<",">>,[global]),
    if(Args==[]) ->
        io:format("size ~.16B ~B call ~s (~.16B)~n\t ~s(~p) ~n",[
                                                                 size(Bytecode),
                                                                 size(Bytecode),
                                                                 Function,
                                                                 IFun,
                                                                 FunName,
                                                                 []]);
      true ->
        io:format("size ~.16B ~B call ~s (~.16B)~n\t ~s(~p) ~n",[
                                                                 size(Bytecode),
                                                                 size(Bytecode),
                                                                 Function,
                                                                 IFun,
                                                                 FunName,
                                                                 lists:zip(ArgTypes,Args)])
    end,
    CallData = lists:foldl(
                 fun(Arg,Acc) ->
                     <<Acc/binary,Arg:256/big>>
                 end, << IFun:32/big>>, Args),
    eval(Bytecode,Storage,maps:merge(State0, #{ cd => CallData })).

