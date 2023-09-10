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
             'finfun'=>function(),
             'static' => integer(),
             'embedded_code'=>map(),
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
  Logger=fun(Message,Args,Xtra,_) ->
             io:format("LOG: ~p~n\targs ~p~n",[Message,Args]),
             Xtra
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
                       depth=>0,
                       extra=>#{},
                       logger=>Logger
                      },State0#{data=>Data}),
    try
      eevm_interpret:run(State)
    catch throw:{revert,Bin,GasLeft}  ->
            {done, {revert, Bin}, #{ gas=>GasLeft}}
    end.


eval(Bytecode,Storage,State0,Function,Args) ->
    IFun = fun(B) -> {ok,E}=ksha3:hash(256, B), <<X:32/big,_/binary>> = E,X end(Function),
    CallData = lists:foldl(
                 fun(Arg,Acc) ->
                     <<Acc/binary,Arg:256/big>>
                 end, << IFun:32/big>>, Args),
    eval(Bytecode,Storage,maps:merge(State0, #{ cd => CallData })).

