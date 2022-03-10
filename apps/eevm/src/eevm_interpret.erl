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
      ?TRACE({opcode, {PC, Inst}}),
      case interp(Inst,State) of
        #{fin:=Reason}=S2 ->
          {done,Reason,S2};
        {goto,Dst,S2} ->
          <<_:Dst/binary,JmpOpcode:8/integer,NewCode/binary>> =maps:get(code,State),
          case JmpOpcode of
            16#5b -> %jumpdest
              ?TRACE({jump_ok, Dst}),
              run_next(Dst+1,NewCode,S2);
            Other ->
              ?TRACE({jump_error, Other}),
              {error, {jump_to, Other}, State}
          end;
        {return, Data, S2} ->
          {done,{return,Data},S2};
        {error, Reason} ->
          {error, Reason, State};
        S2 ->
          run_next(NextPC,Rest,S2)
      end
  end.

interp(pop,#{stack:=[Val|Stack],gas:=Gas}=State) ->
  ?TRACE({pop, Val}),
  State#{stack=>Stack,gas=>Gas-1};

interp(jumpdest,State) ->
  State;

interp(return,#{stack:=[Off,Len|Stack], memory:=RAM}=State) ->
  Value=eevm_ram:read(RAM,Off,Len),
  ?TRACE({return, Value}),
  {return,Value,State#{stack=>Stack}};

interp(jump,#{stack:=[Dst|Stack]}=State) ->
  {goto,Dst,State#{stack=>Stack}};

interp(jumpi,#{stack:=[Dst,0|Stack]}=State) ->
  ?TRACE({jumpi, {Dst,false}}),
  State#{stack=>Stack};

interp(jumpi,#{stack:=[Dst,1|Stack]}=State) ->
  ?TRACE({jumpi, {Dst,true}}),
  {goto,Dst,State#{stack=>Stack}};

interp(revert,#{memory:=RAM,stack:=[MemOff,MemLen|Stack]}=State) ->
  Value=eevm_ram:read(RAM,MemOff,MemLen),
  ?TRACE({revert, Value}),
  State#{stack=>[0|Stack],
         fin=>{revert,Value}
        };

interp(invalid,State) ->
  State#{
         fin=>invalid
        };

interp(stop,State) ->
  State#{
         fin=>stop
        };

interp(shr,#{stack:=[Off,A|Stack]}=State) ->
  Sh=A bsr Off,
  State#{stack=>[Sh|Stack]};

interp(shl,#{stack:=[Off,A|Stack]}=State) ->
  Sh=A bsl Off,
  State#{stack=>[Sh|Stack]};

interp('not',#{stack:=[A|Stack]}=State) ->
  Not=bnot A,
  %?TRACE({'not', Not}),
  State#{stack=>[Not|Stack]};

interp('and',#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[A band B|Stack]};

interp('or',#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[A bor B|Stack]};

interp('div',#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[A div B|Stack]};

interp(sub,#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[A - B|Stack]};

interp(mul,#{stack:=[A,B|Stack]}=State) ->
  Res=A*B,
  State#{stack=>[Res|Stack]};

interp(add,#{stack:=[A,B|Stack]}=State) ->
  Res=A+B,
  State#{stack=>[Res|Stack]};

interp(eq,#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[if A==B -> 1; true -> 0 end|Stack]};

interp(lt,#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack]};

interp(slt,#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack]};

interp(gt,#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[if A>B -> 1; true -> 0 end|Stack]};

interp({swap,1},#{stack:=[A,B|Stack]}=State) ->
  State#{stack=>[B,A|Stack]};

interp({swap,2},#{stack:=[A,X1,B|Stack]}=State) ->
  State#{stack=>[B,X1,A|Stack]};

interp({swap,3},#{stack:=[A,X1,X2,B|Stack]}=State) ->
  State#{stack=>[B,X1,X2,A|Stack]};

interp({swap,4},#{stack:=[A,X1,X2,X3,B|Stack]}=State) ->
  State#{stack=>[B,X1,X2,X3,A|Stack]};

interp({swap,5},#{stack:=[A,X1,X2,X3,X4,B|Stack]}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,A|Stack]};

interp({dup,N},#{stack:=Stack}=State) ->
  Val=lists:nth(N,Stack),
  Stack1=[Val|Stack],
  State#{stack=>Stack1};

interp(exp,#{stack:=[A,B|Stack]}=State) ->
  Res=trunc(math:pow(A,B)),
  State#{stack=>[Res|Stack]};

interp(iszero,#{stack:=[A|Stack]}=State) ->
  State#{stack=>[if A==0 -> 1;
                    true -> 0 
                 end|Stack]};

interp(sha3, #{stack:=[Off,Len|Stack],memory:=RAM}=State) ->
  RamData=eevm_ram:read(RAM,Off,Len),
  Hash=crypto:hash(sha256,RamData),
  Value = binary:decode_unsigned(Hash),
  ?TRACE({sha3, {Len,Off,RamData,Hash}}),
  State#{stack=>[Value|Stack]};

interp(callvalue, #{stack:=Stack,value:=Value}=State) ->
  State#{stack=>[Value|Stack]};

interp(caller, #{stack:=Stack,caller:=Value}=State) ->
  State#{stack=>[Value|Stack]};

interp(calldataload, #{stack:=[I|Stack],cd:=CD}=State) ->
  BValue = eevm_ram:read(CD,I,32),
  Value = binary:decode_unsigned(BValue),
  ?TRACE({calldataload, {I,32,BValue,Value}}),
  State#{stack=>[Value|Stack]};

interp(calldatasize, #{stack:=Stack,cd:=CD}=State) ->
  Value=size(CD),
  State#{stack=>[Value|Stack]};

interp(codesize, #{stack:=Stack,code:=Code}=State) ->
  Value=size(Code),
  State#{stack=>[Value|Stack]};

interp({push,_,Value},#{stack:=Stack}=State) ->
  State#{stack=>[Value|Stack]};

interp(codecopy,#{stack:=[RAMOff,CodeOff,Len|Stack],memory:=RAM,code:=Code}=State) ->
  Value=eevm_ram:read(Code,CodeOff,Len),
  RAM1=eevm_ram:write(RAM,RAMOff,Value),
  ?TRACE({codecopy, {Len,CodeOff,RAMOff,Value}}),
  State#{stack=>Stack,memory=>RAM1};

interp(mload,#{stack:=[Offset|Stack],memory:=RAM}=State) ->
  Value=binary:decode_unsigned(eevm_ram:read(RAM,Offset,32)),
  ?TRACE({mload, {Offset,Value}}),
  State#{stack=>[Value|Stack]};

interp(mstore,#{stack:=[Offset,Val|Stack],memory:=RAM,gas:=Gas}=State) ->
  ?TRACE({mstore, {Offset,Val}}),
  RAM1=eevm_ram:write(RAM,Offset,<<Val:256/big>>),
  State#{stack=>Stack,gas=>Gas-1,memory=>RAM1};

interp(sstore,#{stack:=[Key,Value|Stack], storage:=Storage, gas:=G}=State) ->
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
  ?TRACE({sload, {Key,Value}}),
  State#{stack=>[Value|Stack]};

interp({log,0},#{stack:=[Offset,Len|Stack],memory:=RAM,logger:=Logger}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[]),
  State#{stack=>Stack};

interp({log,1},#{stack:=[Offset,Len,Topic0|Stack],memory:=RAM,logger:=Logger}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0]),
  State#{stack=>Stack};

interp({log,2},#{stack:=[Offset,Len,Topic0,Topic1|Stack],memory:=RAM,logger:=Logger}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0,Topic1]),
  State#{stack=>Stack};

interp({log,3},#{stack:=[Offset,Len,Topic0,Topic1, Topic2|Stack],memory:=RAM,logger:=Logger}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0,Topic1,Topic2]),
  State#{stack=>Stack};

interp({log,4},#{stack:=[Offset,Len,Topic0,Topic1, Topic2, Topic3|Stack],memory:=RAM,logger:=Logger}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0,Topic1,Topic2,Topic3]),
  State#{stack=>Stack};

interp(Instr,_State) ->
  {error,{bad_instruction,Instr}}.

