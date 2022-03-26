-module(eevm).
-behaviour(application).

-export([eval/3,eval/5]).

-export([start/0,start/2, stop/1]).

start() ->
  application:start(eevm).

start(_StartType, _StartArgs) ->
    eevm_sup:start_link().

stop(_State) ->
    ok.

% {done, [stop|invalid|{revert,Err}|{return,Data}], State1}¶
% {error,[nogas|{jump_to,Dst}|{bad_instruction,Instr}], State1}¶

-spec eval(Bytecode :: binary(),
           Storage :: map(),
           #{'gas':=integer(),
             'logger'=>function(),
             'data':=#{
                       'address'=>integer(),
                       'callvalue'=>integer(),
                       'caller'=>integer(),
                       'gasprice'=>integer(),
                       'origin'=>integer()
                      },
             'cd'=>binary(),
             'sload'=>function(),
             'get'=>#{
                      'balance'=>function(),
                      'code'=>function()
                     },
             'trace'=>pid()|undefined
            }) ->
  {'done', 'stop'|invalid|{revert,binary()}|{return,binary()}, #{
                                                                 gas:=integer(),
                                                                 storage:=#{},
                                                                 memory:=binary() }}
  |
  {'error', 'nogas'|{'jump_to',integer()}|{'bad_instruction',any()}, #{
                                                                       memory:=binary()
                                                                      }}.


eval(Bytecode,Storage,State0) ->
  Logger=fun(Message,Args) ->
             io:format("LOG: ~p~n\targs ~p~n",[Message,Args])
         end,
  Data=maps:merge(
         #{
           address=>16#101,
           callvalue=>0,
           caller=>16#102,
           gasprice=>10,
           origin=>16#102
          },
         maps:get(data,State0,#{})
        ),
    State=maps:merge(#{
                       stack=>[],
                       gas=>0,
                       cd => <<>>,
                       storage=>Storage,
                       memory=><<>>,
                       code=>Bytecode,
                       logger=>Logger
                      },State0#{data=>Data}),
    eevm_interpret:run(State).

eval(Bytecode,Storage,State0,Function,Args) ->
    IFun = fun(B) -> {ok,E}=ksha3:hash(256, B), <<X:32/big,_/binary>> = E,X end(Function),
%    [FunName,Args1,_]=binary:split(Function,[<<"(">>,<<")">>],[global]),
%    ArgTypes=binary:split(Args1,<<",">>,[global]),
%    if(Args==[]) ->
%        io:format("size ~.16B ~B call ~s (~.16B)~n\t ~s(~p) ~n",[
%                                                                 size(Bytecode),
%                                                                 size(Bytecode),
%                                                                 Function,
%                                                                 IFun,
%                                                                 FunName,
%                                                                 []]);
%      true ->
%        io:format("size ~.16B ~B call ~s (~.16B)~n\t ~s(~p) ~n",[
%                                                                 size(Bytecode),
%                                                                 size(Bytecode),
%                                                                 Function,
%                                                                 IFun,
%                                                                 FunName,
%                                                                 lists:zip(ArgTypes,Args)])
%    end,
    CallData = lists:foldl(
                 fun(Arg,Acc) ->
                     <<Acc/binary,Arg:256/big>>
                 end, << IFun:32/big>>, Args),
    eval(Bytecode,Storage,maps:merge(State0, #{ cd => CallData })).

