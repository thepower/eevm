-module(eevm_interpret).
-export([run/1,interp/2]).

-define (TRACE(Code), _=fun()->
                          case maps:get(trace,State,undefined) of
                            PID when is_pid(PID) ->
                              PID ! {trace, Code};
                            undefined -> ok
                          end end()).

run(#{code:=Code}=State) ->
  run_next(0,Code,State).

run_next(PC, Code, #{gas:=Gas,stack:=Stack}=State) ->
  if(Gas<1) ->
      {error, nogas, State};
    true ->
      ?TRACE({stack, Stack}),
      {Inst,Rest,NextPC}=case eevm_dec:decode(Code) of
                           {I, RestCode} ->
                             {I, RestCode, PC+1};
                           {I, RestCode, Offset} ->
                             {I, RestCode, PC+Offset}
                         end,
      ?TRACE({opcode, Inst}),

          case interp(Inst,State) of
            #{fin:=Reason}=S2 ->
              {done,Reason,S2};
            {goto,Dst,S2} ->
              <<_:Dst/binary,JmpOpcode:8/integer,NewCode/binary>> =maps:get(code,State),
              case JmpOpcode of
                16#5b -> %jumpdest
                  ?TRACE({jump_ok, Dst}),
                  %io:format(".: went to 0x~.16B~n",[Dst]),
                  run_next(Dst+1,NewCode,S2);
                Other ->
                  ?TRACE({jump_error, Other}),
                  %io:format("Jump to ~p~n",[Other]),
                  {error, {jump_to, Other}, State}
              end;
            {return, Data, S2} ->
              io:format("RAM used ~w~n",[size(maps:get(memory,S2))]),
              {done,{return,Data},S2};
            {error, Reason} ->
              io:format("Error@~p: ~p~n",[PC,Reason]),
              {error, Reason, State};
            S2 ->
              run_next(NextPC,Rest,S2)
          end
      end.


interp(pop,#{stack:=[Val|Stack],gas:=Gas}=State) ->
  ?TRACE({pop, Val}),
  %io:format("pop val 0x~.16B~n\t STACK ~p~n",[Val,Stack]),
  State#{stack=>Stack,gas=>Gas-1};

interp(jumpdest,State) ->
  %io:format("jumpdest @~p~n",[_PC]),
  State;

interp(return,#{stack:=[Off,Len|Stack], memory:=RAM}=State) ->
  %io:format("return with ~w bytes starting ~w stack ~p~n",[Len,Off,Stack]),
  Value=eevm_ram:read(RAM,Off,Len),
  ?TRACE({return, Value}),
  {return,Value,State#{stack=>Stack}};

interp(jump,#{stack:=[Dst|Stack]}=State) ->
  %io:format("jump dst ~p~n",[Dst]),
  {goto,Dst,State#{stack=>Stack}};

interp(jumpi,#{stack:=[Dst,0|Stack]}=State) ->
  %io:format("jumpi dst ~.16B cond false!~n",[Dst]),
  ?TRACE({jumpi, {Dst,false}}),
  State#{stack=>Stack};

interp(jumpi,#{stack:=[Dst,1|Stack]}=State) ->
  %io:format("jumpi dst ~p cond 1~n",[Dst]),
  ?TRACE({jumpi, {Dst,true}}),
  {goto,Dst,State#{stack=>Stack}};

interp(revert,#{memory:=RAM,stack:=[MemOff,MemLen|Stack]}=State) ->
  %io:format("revert Off ~p Len ~p~n",[MemOff,MemLen]),
  Value=eevm_ram:read(RAM,MemOff,MemLen),
  ?TRACE({revert, Value}),
  State#{stack=>[0|Stack],
         fin=>{revert,Value}
        };

interp(stop,State) ->
  State#{
         fin=>stop
        };

interp(shr,#{stack:=[Off,A|Stack]}=State) ->
  Sh=A bsr Off,
  %io:format("shr 0x~.16B = 0x~.16B~n",[A,Sh]),
  State#{stack=>[Sh|Stack]};

interp(shl,#{stack:=[Off,A|Stack]}=State) ->
  Sh=A bsl Off,
  %io:format("shl 0x~.16B = 0x~.16B~n",[A,Sh]),
  State#{stack=>[Sh|Stack]};

interp('not',#{stack:=[A|Stack]}=State) ->
  Not=bnot A,
  %io:format("not 0x~.16B = 0x~.16B~n",[A,Not]),
  %?TRACE({'not', Not}),
  State#{stack=>[Not|Stack]};

interp('and',#{stack:=[A,B|Stack]}=State) ->
  %io:format("and 0x~.16B 0x~.16B~n",[A,B]),
  State#{stack=>[A band B|Stack]};

interp('or',#{stack:=[A,B|Stack]}=State) ->
  %io:format("or 0x~.16B 0x~.16B~n",[A,B]),
  State#{stack=>[A bor B|Stack]};

interp('div',#{stack:=[A,B|Stack]}=State) ->
  %io:format("div 0x~.16B / 0x~.16B~n",[A,B]),
  State#{stack=>[A div B|Stack]};

interp(sub,#{stack:=[A,B|Stack]}=State) ->
  %io:format("sub 0x~.16B 0x~.16B~n",[A,B]),
  State#{stack=>[A - B|Stack]};

interp(mul,#{stack:=[A,B|Stack]}=State) ->
  Res=A*B,
  %io:format("mul 0x~.16B 0x~.16B = 0x~.16B~n",[A,B,Res]),
  State#{stack=>[Res|Stack]};

interp(add,#{stack:=[A,B|Stack]}=State) ->
  Res=A+B,
  %io:format("add 0x~.16B 0x~.16B = 0x~.16B~n",[A,B,Res]),
  State#{stack=>[Res|Stack]};

interp(eq,#{stack:=[A,B|Stack]}=State) ->
  %io:format("eq 0x~.16B 0x~.16B~n",[A,B]),
  State#{stack=>[if A==B -> 1; true -> 0 end|Stack]};

interp(lt,#{stack:=[A,B|Stack]}=State) ->
  io:format("lt 0x~.16B < 0x~.16B~n",[A,B]),
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack]};

%interp(slt,#{stack:=[A,B|Stack]}=State) ->
%  io:format("slt 0x~.16B 0x~.16B~n",[A,B]),
%  State#{stack=>[if A<B -> 1; true -> 0 end|Stack]};

interp(gt,#{stack:=[A,B|Stack]}=State) ->
  %io:format("gt 0x~.16B 0x~.16B~n",[A,B]),
  State#{stack=>[if A>B -> 1; true -> 0 end|Stack]};

interp({swap,1},#{stack:=[A,B|Stack]}=State) ->
  %io:format("STACK ~p~n",[[B,A|Stack]]),
  State#{stack=>[B,A|Stack]};

interp({swap,2},#{stack:=[A,X1,B|Stack]}=State) ->
  %io:format("STACK ~p~n",[[B,X1,A|Stack]]),
  State#{stack=>[B,X1,A|Stack]};

interp({swap,3},#{stack:=[A,X1,X2,B|Stack]}=State) ->
  %io:format("STACK ~p~n",[[B,X1,X2,A|Stack]]),
  State#{stack=>[B,X1,X2,A|Stack]};

interp({swap,4},#{stack:=[A,X1,X2,X3,B|Stack]}=State) ->
  %io:format("STACK ~p~n",[[B,X1,X2,X3,A|Stack]]),
  State#{stack=>[B,X1,X2,X3,A|Stack]};

interp({dup,N},#{stack:=Stack}=State) ->
  Val=lists:nth(N,Stack),
  Stack1=[Val|Stack],
  %io:format("STACK ~p~n",[Stack1]),
  State#{stack=>Stack1};

interp(exp,#{stack:=[A,B|Stack]}=State) ->
  Res=trunc(math:pow(A,B)),
  %io:format("exp 0x~.16B 0x~.16B = 0x~.16B~n",[A,B,Res]),
  State#{stack=>[Res|Stack]};

interp(iszero,#{stack:=[A|Stack]}=State) ->
  %io:format("is zero ~p~n",[A]),
  State#{stack=>[if A==0 -> 1;
                    true -> 0 
                 end|Stack]};

interp(sha3, #{stack:=[Off,Len|Stack],memory:=RAM}=State) ->
  RamData=eevm_ram:read(RAM,Off,Len),
  Hash=crypto:hash(sha256,RamData),
  Value = binary:decode_unsigned(Hash),
  %io:format("==== [ hash of ~w bytes staring ~w~n",[Len,Off]),
  %io:format("   = [ hash of ~s~n",[hex:encode(RamData)]),
  %io:format("   = [         ~s~n",[hex:encode(Hash)]),
  ?TRACE({sha3, {Len,Off,RamData,Hash}}),
  State#{stack=>[Value|Stack]};

interp(callvalue, #{stack:=Stack,value:=Value}=State) ->
  %Value=1000000000,
  %io:format("==== [ callvalue ~.16B~n",[Value]),
  State#{stack=>[Value|Stack]};

interp(caller, #{stack:=Stack,caller:=Value}=State) ->
  %io:format("==== [ caller ~.16B ~n",[Value]),
  State#{stack=>[Value|Stack]};

interp(calldataload, #{stack:=[I|Stack],cd:=CD}=State) ->
  BValue = eevm_ram:read(CD,I,32),
  Value = binary:decode_unsigned(BValue),
  ?TRACE({calldataload, {I,32,BValue,Value}}),
  %io:format("==== [ calldataload ~.16B ~n",[Value]),

  Stack1=[Value|Stack],
  %io:format("\t STACK ~p~n",[Stack1]),
  State#{stack=>Stack1};

interp(calldatasize, #{stack:=Stack,cd:=CD}=State) ->
  Value=size(CD),
  %io:format("==== [ calldatasize ~.16B ~n",[Value]),
  State#{stack=>[Value|Stack]};

interp(codesize, #{stack:=Stack,code:=Code}=State) ->
  Value=size(Code),
  %io:format("==== [ codesize ~.16B stack ~p ~n",[Value,Stack]),
  State#{stack=>[Value|Stack]};

interp({push,_,Value},#{stack:=Stack}=State) ->
  %io:format("push 0x~.16B~n",[Value]),
  State#{stack=>[Value|Stack]};

interp(codecopy,#{stack:=[RAMOff,CodeOff,Len|Stack],memory:=RAM,code:=Code}=State) ->
  Value=eevm_ram:read(Code,CodeOff,Len),
  RAM1=eevm_ram:write(RAM,RAMOff,Value),
  %io:format("CodeCopy ~w bytes from ~p -> ~p~n",[Len,CodeOff,RAMOff]),
  ?TRACE({codecopy, Value}),
  State#{stack=>Stack,memory=>RAM1};

interp(mload,#{stack:=[Offset|Stack],memory:=RAM}=State) ->
  Value=binary:decode_unsigned(eevm_ram:read(RAM,Offset,32)),
  %io:format("mload  offset ~.16B = ~.16B~n",[Offset,Value]),
  ?TRACE({mload, {Offset,Value}}),
  State#{stack=>[Value|Stack]};

interp(mstore,#{stack:=[Ost,Val|Stack],memory:=RAM,gas:=Gas}=State) ->
  %io:format("mstore offset ~.16B val ~.16B~n",[Ost,Val]),
  ?TRACE({mstore, {Ost,Val}}),
  RAM1=eevm_ram:write(RAM,Ost,<<Val:256/big>>),
  State#{stack=>Stack,gas=>Gas-1,memory=>RAM1};


interp(sstore,#{stack:=[Key,Value|Stack], storage:=Storage, gas:=G}=State) ->
  %io:format(":: sstore key 0x~.16B val 0x~.16B~n",[Key,Value]),
  St1=maps:put(Key,Value,Storage),
  GD=case {maps:is_key(Key,Storage),Value=/=0} of
       {false,true} ->
         -20000;
       {true,true} ->
         -5000;
       {false,false} ->
         0;
       {true,false} ->
         +15000
     end,
  G2=G+GD,
  ?TRACE({sstore, {Key,Value,GD}}),
  State#{stack=>Stack,storage=>St1,gas=>G2};

interp(sload,#{stack:=[Key|Stack],storage:=Storage}=State) ->
  Value=maps:get(Key,Storage,0),
  %io:format(":: sload key 0x~.16B val = 0x~.16B~n",[Key,Value]),
  ?TRACE({sload, {Key,Value}}),
  State#{stack=>[Value|Stack]};

interp({log,0},#{stack:=[Offset,Len|Stack]}=State) ->
  io:format("Log0 ~p ~p~n",[Offset,Len]),
  State#{stack=>Stack};

interp({log,1},#{stack:=[Offset,Len,Topic0|Stack]}=State) ->
  io:format("Log1 ~p ~p ~.16B~n",[Offset,Len,Topic0]),
  State#{stack=>Stack};

interp({log,2},#{stack:=[Offset,Len,Topic0,Topic1|Stack]}=State) ->
  io:format("Log2 ~p ~p ~.16B ~.16B~n",[Offset,Len,Topic0,Topic1]),
  State#{stack=>Stack};

interp({log,3},#{stack:=[Offset,Len,Topic0,Topic1, Topic2|Stack]}=State) ->
  io:format("Log3 ~p ~p ~.16B ~.16B ~.16B~n",[Offset,Len,Topic0,Topic1, Topic2]),
  State#{stack=>Stack};

interp({log,4},#{stack:=[Offset,Len,Topic0,Topic1, Topic2, Topic3|Stack]}=State) ->
  io:format("Log4 ~p ~p ~.16B ~.16B ~.16B ~.16B~n",[Offset,Len,Topic0,Topic1, Topic2, Topic3]),
  State#{stack=>Stack};

interp(Instr,_State) ->
  {error,{bad_instruction,Instr}}.

