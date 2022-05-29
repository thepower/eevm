-module(eevm_test).

-export([runtest/5]).

runtest(Code,MyAddr,Caller,CValue,Extra) ->
  Ex=maps:put({MyAddr,code},Code,Extra),
  R=eevm:eval(
      Code,
      #{},
      #{gas=>100000000,
        extra=>Ex,
        get=>#{
               code => fun(Addr,Ex0) ->
                           maps:get({Addr,code},Ex0,<<>>)
                       end
              },
        create=>fun(Value, Code1, Ex0) ->
                    Addr=1024+erlang:unique_integer([positive]),
                    Deploy=eevm:eval(Code1,#{},#{gas=>100000,
                                                 extra=>Ex0,
                                                 trace=>whereis(eevm_tracer),
                                                 depth=>100
                                                }),
                    {done,{return,X},#{storage:=StRet,extra:=Ex1}}=Deploy,

                    St2=maps:merge(
                          maps:get({Addr,state},Ex0,#{}),
                          StRet),
                    Ex2=maps:put({Addr,state},St2,
                                 maps:put({Addr,code},X,
                                          maps:put({Addr,value},Value,Ex1)
                                         )
                                ),
                    {#{
                      address => Addr
                     },Ex2}
                end,
        finfun=>fun(_,_,#{data:=#{address:=Addr}, storage:=Stor, extra:=Xtra} = State) ->
                    NewS=maps:merge(
                           maps:get({Addr, state}, Xtra, #{}),
                           Stor
                          ),
                    State#{extra=>Xtra#{{Addr, state} => NewS}}
                end,
        sload=>fun(Addr, Key, State) ->
                   AddrSt=maps:get({Addr,state},State,#{}),
                   Res=maps:get(Key,AddrSt,0),
                   Res
               end,
        data=>#{
                address=>MyAddr,
                callvalue=>CValue,
                caller=>Caller,
                gasprice=>10,
                origin=>Caller
               },
        trace=>whereis(eevm_tracer)
       }),
  R.

