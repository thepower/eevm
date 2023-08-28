-module(eevm_scratchpad).
-compile([export_all,nowarn_export_all]).

%this file contains scrathpad code for experements. It might not work!!!
%some code in this file requires hex module, it was removed from this repo
%to avoid conflict with tpnode code. You can obtain hex.erl at
%https://raw.githubusercontent.com/cleverfox/evm_scratchpad/fc431af59afbf1c2e495e7e4614b696f05db7fe7/src/hex.erl

load(Filename) ->
  {ok, ERC20Hex } = file:read_file(Filename),
  [ H | _ ] = binary:split(ERC20Hex, <<"\n">>),
  hex:decode(H).



sload() ->
  Asm= <<"
PUSH1 0
sload
push1 1
add
dup1
push1 0
sstore
push1 0
mstore
push1 32
push1 0
return
">>,
  io:format("~s~n",[Asm]),
  Code=eevm:asm(eevm:parse_asm(Asm)),
  (Code).

deploysload() ->
  SLoad=sload(),
  Asm= <<"
push4 0xdeadc0de
push1 0
sstore
push",(integer_to_binary(size(SLoad)))/binary," 0x",(hex:encode(SLoad))/binary,"
push1 ",(integer_to_binary((32-size(SLoad))*8))/binary,"
shl
push1 0
mstore
push1 ",(integer_to_binary(size(SLoad)))/binary,"
push1 0
return
">>,
  io:format("~s~n",[Asm]),
  Code=eevm:asm(eevm:parse_asm(Asm)),
  io:format("RESCODE:~n~s~n",[code2mem(Code,0)]),
  Code.

code2mem(Code,Off) ->
  [if(Off==0) ->
       CodeS=size(Code),
       Size=max(1,ceil(ceil(math:log(max(CodeS,2))/math:log(2)) / 8)),
       io_lib:format("push~w ~w~n",[Size,CodeS]);
     true ->
      []
  end,
  if(size(Code)>32) ->
      <<ThisCode:32/binary,Rest/binary>> = Code,
      [
      io_lib:format("push32 0x~s~npush1 ~w~nmstore~n",[hex:encode(ThisCode),Off])
      | code2mem(Rest,Off+32)];
    true ->
      [io_lib:format("push~w 0x~s~npush1 ~w~nshl~npush1 ~w~nmstore~n",
                     [size(Code),hex:encode(Code),8*(32-size(Code)),Off])]
  end].

call() ->
  Code=eevm:asm(eevm:parse_asm(<<"// Create a contract that creates an exception if first word of calldata is 0
PUSH17 0x67600035600757FE5B60005260086018F3
PUSH1 0
MSTORE
PUSH1 17
PUSH1 15
PUSH1 0
CREATE

// Call with no parameters, return 0
PUSH1 0
PUSH1 0
PUSH1 0
PUSH1 0
PUSH1 0
DUP6
PUSH2 0xFFFF
CALL

// Call with non 0 calldata, returns success
PUSH1 0
PUSH1 0
PUSH1 32
PUSH1 0
PUSH1 0
DUP7
PUSH2 0xFFFF
CALL
">>)),

  {done,_,State}=eevm:runtest(Code,16#100,16#101,0,#{}),
  Res=maps:with([extra,memory,stack,storage,gas], State),
  Res.



erc() ->
  Code = load("testdata/erc.hex"),
  Code1= <<Code/binary,1024:256/big>>,
  {done,
   {return,Code2},
   #{storage:=Stor}
  }=eevm:eval(
      Code1,
      #{},
      #{gas=>100000,
        data=>#{
                caller=>16#fff,
                callvalue=>0
               },
        trace=>whereis(eevm_tracer)}),
  R=eevm:eval(Code2,
             Stor,
             #{gas=>100000,
               trace=>whereis(eevm_tracer),
               data=>#{
                       caller=>16#fff
                      }
              },
             <<"transfer(address,uint256)">>,
             [16#FFe,100]),
  case R of 
    {done,{return,Bin},_} ->
      io:format("Return\n"),
      dump(0,Bin);
    _ ->
      ok
  end,
  R.

mkstring(Bin) when size(Bin)==32 ->
  Bin;

mkstring(Bin) when size(Bin)<32 ->
  PadL=32-size(Bin),
  <<Bin/binary,0:(PadL*8)/integer>>.

revert() ->
  case(whereis(eevm_tracer)) of
    PID when is_pid(PID) ->
      eevm_tracer ! {trace, "=============="};
    _ -> ok
  end,
  Code =
hex:decode("6c726576657274656420646174616000557f726576657274206d657373616765000000000000000000000000000000000000600052600e6000fd"),
  Deploy=eevm:eval(Code,
                 #{0=>1234,
                  1=>2345},
                 #{
                   gas=>20024,
                   data=>#{
                           callvalue=>0,
                           caller=>16#c0de
                          },
                   trace=>whereis(eevm_tracer)}),
  io:format("MEMORY\n"),
  dump(0,maps:get(memory,element(3,Deploy))),
  Deploy.


bal() ->
  Code =
  hex:decode("608060405234801561001057600080fd5b506040516020806100fa833981016040525160005560c7806100336000396000f300"),
  Code0= <<Code/binary,1023:256/big>>,
  case(whereis(eevm_tracer)) of
    PID when is_pid(PID) ->
      eevm_tracer ! {trace, "=============="};
    _ -> ok
  end,
  Deploy=eevm:eval(Code0,
                 #{},
                 #{
                   gas=>20024,
                   data=>#{
                           callvalue=>0,
                           caller=>16#c0de
                          },
                   trace=>whereis(eevm_tracer)}),
  io:format("MEMORY\n"),
  dump(0,maps:get(memory,element(3,Deploy))),
  {done, {return, Code1}, #{storage:=Stor}} = Deploy,
  eevm:eval(Code1,
          Stor,
                 #{
                   gas=>20024,
                   data=>#{
                           callvalue=>0,
                           caller=>16#c0de
                          },
                   trace=>whereis(eevm_tracer)}).

tether() ->
  Trace=case(whereis(eevm_tracer)) of
          PID when is_pid(PID) ->
            eevm_tracer ! {trace, "=============="},
            PID;
          _ ->
            undefined
        end,
  Code = load("testdata/TetherToken.hex"),
  CoinSym=mkstring(<<"CoinSym">>),
  Code1= <<Code/binary,(131072):256/big,CoinSym/binary,CoinSym/binary,3:256/big>>,
  Deploy=eevm:eval(Code1,
                 #{},
                 #{
                   gas=>1000000,
                   data=>#{
                           callvalue=>0,
                           caller=>16#c0de
                          },
                   trace=>Trace
                  }),
  %io:format("MEMORY\n"),
  %dump(0,maps:get(memory,element(3,Deploy))),
  {done,{return,Code2},#{storage:=St1}}=Deploy,
  io:format("St1 ~p~n",[St1]),

  GetBal=fun(S,Addr) ->
             {done, {return,<<Res:256/big>>}, _} = eevm:eval(Code2,
              S,
              #{data=>#{caller=>16#c0de},
                gas=>100000},
              <<"balanceOf(address)">>,
              [Addr]),
             Res
         end,
  

  io:format("Bals ~p/~p/~p~n",[
                             GetBal(St1,16#c0de),
                             GetBal(St1,16#100),
                             GetBal(St1,16#200)
                        ]),

  St_Fin=lists:foldl(
    fun({From,Func,Args},St) ->
        {done,R1,#{storage:=St2}}=eevm:eval(
                                     Code2,
                                     St,
                                     #{trace=>Trace,
                                       data=>#{caller=>From},
                                       gas=>100000},
                                     Func,Args),
        io:format("Res ~p~n",[R1]),
        case R1 of
          stop ->
            %io:format("St2 ~p~n",[St2]),
            io:format("Bals ~p/~p/~p~n",[
                                         GetBal(St2,16#c0de),
                                         GetBal(St2,16#100),
                                         GetBal(St2,16#200)
                                        ]),
            St2;
          _ ->
            St
        end
    end, St1, [
               {16#c0de, <<"approve(address,uint256)">>, [16#100,100]},
               {16#100, <<"transferFrom(address,address,uint256)">>, [16#c0de,16#200,94]},
               {16#100, <<"transferFrom(address,address,uint256)">>, [16#c0de,16#100,5]}

              ]),
  St_Fin.

 

coin() ->
  Trace=case(whereis(eevm_tracer)) of
          PID when is_pid(PID) ->
            eevm_tracer ! {trace, "=============="},
            PID;
          _ ->
            undefined
        end,
  Code = load("testdata/Coin.hex"),
  %CoinSym=mkstring(<<"CoinSym">>),
  Code1= <<Code/binary>>,
  Deploy=eevm:eval(Code1,
                 #{},
                 #{
                   gas=>100000,
                   data=>#{
                           callvalue=>0,
                           caller=>16#c0de
                          },
                   trace=>Trace
                  }),
  %io:format("MEMORY\n"),
  %dump(0,maps:get(memory,element(3,Deploy))),
  {done,{return,Code2},#{storage:=Stor}}=Deploy,
  io:format("St1 ~p~n",[Stor]),

  {done,stop,#{storage:=St2}}=eevm:eval(Code2,
              Stor,
              #{trace=>Trace,
                data=>#{caller=>16#c0de},
                gas=>100000},
              <<"mint(address,uint256)">>,
              [16#100,100]),
  io:format("St2 ~p~n",[St2]),

  {done,stop,#{storage:=St3}}=eevm:eval(Code2,
              St2,
              #{trace=>Trace,
                data=>#{caller=>16#c0de},
                gas=>100000},
              <<"mint(address,uint256)">>,
              [16#200,200]),
  io:format("St3 ~p~n",[St3]),

  {done,stop,#{storage:=St4}}=eevm:eval(Code2,
              St3,
              #{trace=>Trace,
                data=>#{caller=>16#200},
                gas=>100000},
              <<"send(address,uint256)">>,
              [16#100,200]),
  io:format("St4 ~p~n",[St4]),
  ok.


erc20() ->
  case(whereis(eevm_tracer)) of
    PID when is_pid(PID) ->
      eevm_tracer ! {trace, "=============="};
    _ -> ok
  end,
  Code = load("testdata/ERC20.hex"),
  CoinName=mkstring(<<"CoinName">>),
  CoinSym=mkstring(<<"CoinSym">>),
  Code1= <<Code/binary,CoinName/binary,CoinSym/binary,CoinName/binary,CoinSym/binary>>,
  Deploy=eevm:eval(Code1,
                 #{},
                 #{
                   gas=>100000,
                   data=>#{
                           callvalue=>10,
                           caller=>16#c0de
                          },
                   trace=>whereis(eevm_tracer)}),
  io:format("MEMORY\n"),
  dump(0,maps:get(memory,element(3,Deploy))),
  Deploy.
%  {done,{return,Code2},State}=Deploy,
%  R=eevm:eval(Code2,State,#{trace=>self()},<<"transfer(address,uint256)">>,[16#FFFe,100]),
%  case R of 
%    {done,{return,Bin},_} ->
%      io:format("RET ~s~n",[hex:encode(Bin)]);
%    _ ->
%      ok
%  end,
%  R.

dump(_,<<>>) ->
  ok;

dump(Off,<<Bin:32/binary,Rest/binary>>) ->
  io:format("~4.16B    ~s~n",[Off,hex:encode(Bin)]),
  dump(Off+32,Rest);

dump(Off,<<Bin/binary>>) ->
  io:format("~4.16B   ~s~n",[Off,hex:encode(Bin)]),
  ok.

mkenc(N) ->
  case eevm_dec:decode(<<N:8,0>>) of
    {Atom,Bin} when is_atom(Atom),is_binary(Bin) ->
      io_lib:format("encode('~s') -> <<16#~.16B:8/big>>;~n",[Atom,N]);
    {{Atom,Len},Bin} when is_binary(Bin) ->
      io_lib:format("encode({~s,~w}) -> <<16#~.16B:8/big>>;~n",[Atom,Len,N]);
    {{Atom,Len,_Val},Bin,_} when is_binary(Bin) ->
      io_lib:format("encode({~s,~w,Data}) -> <<16#~.16B:8/big,Data:~w/big>>;~n",
                    [Atom,Len,N,Len*8]);
    {Int,Bin} when is_integer(Int), is_binary(Bin) ->
      %this allow compile back non fully decoded code
      %io_lib:format("encode(16#~.16B) -> <<16#~.16B:8/big>>;~n",
      %              [Int,Int])
      []
  end.

cg() ->
  cg("src/eevm_enc.erl").

cg(Path) ->
  file:write_file(Path,
  list_to_binary(
    [
     "-module(eevm_enc).\n",
     "%%% ==== [ This file is generated by script, do not edit ] ====\n",
     "-export([encode/1]).\n\n",
    lists:map(fun mkenc/1, lists:seq(0,255)),
    "encode(Instr) -> throw({unknown_instruction,Instr}).\n"
    ]
   )).
