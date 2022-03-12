-module(eevm_interpret).
-export([run/1,interp/2]).

-define (TRACE(Code), _=fun()->
                          case maps:get(trace,State,undefined) of
                            PID when is_pid(PID) ->
                              PID ! {trace, Code};
                            undefined -> ok
                          end end()).
-define(MEM_WORDS(Bin), ((size(Bin) + 31) div 32)).
-define(CMEM, fun() -> NewWords=(?MEM_WORDS(RAM1)-?MEM_WORDS(RAM)),(NewWords*NewWords)+(3*NewWords) end()).

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

interp(shr,#{stack:=[Off,A|Stack], gas:=G}=State) ->
  Sh=A bsr Off,
  State#{stack=>[Sh|Stack], gas=>G-3};

interp(shl,#{stack:=[Off,A|Stack], gas:=G}=State) ->
  Sh=A bsl Off,
  State#{stack=>[Sh|Stack], gas=>G-3};

interp('not',#{stack:=[A|Stack], gas:=G}=State) ->
  Not=bnot A,
  %?TRACE({'not', Not}),
  State#{stack=>[Not|Stack], gas=>G-3};

interp('and',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A band B|Stack], gas=>G-3};

interp('or',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A bor B|Stack], gas=>G-3};

interp('div',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A div B|Stack], gas=>G-5};

interp(sub,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A - B|Stack], gas=>G-3};

interp(mul,#{stack:=[A,B|Stack], gas:=G}=State) ->
  Res=A*B,
  State#{stack=>[Res|Stack], gas=>G-5};

interp(add,#{stack:=[A,B|Stack], gas:=G}=State) ->
  Res=A+B,
  State#{stack=>[Res|Stack],gas=>G-3};

interp(eq,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A==B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(lt,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(slt,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(gt,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A>B -> 1; true -> 0 end|Stack], gas=>G-3};

interp({swap,1},#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,A|Stack], gas=>G-3};

interp({swap,2},#{stack:=[A,X1,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,A|Stack], gas=>G-3};

interp({swap,3},#{stack:=[A,X1,X2,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,A|Stack], gas=>G-3};

interp({swap,4},#{stack:=[A,X1,X2,X3,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,A|Stack], gas=>G-3};

interp({swap,5},#{stack:=[A,X1,X2,X3,X4,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,A|Stack], gas=>G-3};

interp({dup,N},#{stack:=Stack, gas:=G}=State) ->
  Val=lists:nth(N,Stack),
  Stack1=[Val|Stack],
  State#{stack=>Stack1, gas=>G-3};

interp(exp,#{stack:=[A,B|Stack], gas:=G}=State) ->
  Res=trunc(math:pow(A,B)),
  State#{stack=>[Res|Stack], gas=>G-10};

interp(iszero,#{stack:=[A|Stack], gas:=G}=State) ->
  State#{stack=>[if A==0 -> 1;
                    true -> 0 
                 end|Stack], gas=>G-3};

interp(sha3, #{stack:=[Off,Len|Stack],memory:=RAM, gas:=G}=State) ->
  RamData=eevm_ram:read(RAM,Off,Len),
  Hash=crypto:hash(sha256,RamData),
  Value = binary:decode_unsigned(Hash),
  ?TRACE({sha3, {Len,Off,RamData,Hash}}),
  Gas=30+(6*((Len+31) div 32)),
  State#{stack=>[Value|Stack], gas=>G-Gas};

interp(callvalue, #{stack:=Stack,value:=Value, gas:=G}=State) ->
  State#{stack=>[Value|Stack], gas=>G-2};

interp(caller, #{stack:=Stack,caller:=Value, gas:=G}=State) ->
  State#{stack=>[Value|Stack], gas=>G-2};

interp(calldataload, #{stack:=[I|Stack], gas:=G,cd:=CD}=State) ->
  BValue = eevm_ram:read(CD,I,32),
  Value = binary:decode_unsigned(BValue),
  ?TRACE({calldataload, {I,32,BValue,Value}}),
  State#{stack=>[Value|Stack], gas=>G-2};

interp(calldatasize, #{stack:=Stack,cd:=CD, gas:=G}=State) ->
  Value=size(CD),
  State#{stack=>[Value|Stack], gas=>G-2};

interp(codesize, #{stack:=Stack,code:=Code, gas:=G}=State) ->
  Value=size(Code),
  State#{stack=>[Value|Stack], gas=>G-3};

interp({push,_,Value},#{stack:=Stack, gas:=G}=State) ->
  State#{stack=>[Value|Stack], gas=>G-3};

interp(codecopy,#{stack:=[RAMOff,CodeOff,Len|Stack], gas:=G,memory:=RAM,code:=Code}=State) ->
  Value=eevm_ram:read(Code,CodeOff,Len),
  RAM1=eevm_ram:write(RAM,RAMOff,Value),
  ?TRACE({codecopy, {Len,CodeOff,RAMOff,Value}}),
  Gas=3+(3*((Len+31 div 32))) + ?CMEM,
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas};

interp(mload,#{stack:=[Offset|Stack],memory:=RAM, gas:=G}=State) ->
  Value=binary:decode_unsigned(eevm_ram:read(RAM,Offset,32)),
  ?TRACE({mload, {Offset,Value}}),
  State#{stack=>[Value|Stack], gas=>G-3};

interp(mstore,#{stack:=[Offset,Val|Stack],memory:=RAM, gas:=G}=State) ->
  ?TRACE({mstore, {Offset,Val}}),
  RAM1=eevm_ram:write(RAM,Offset,<<Val:256/big>>),
  Gas=3+?CMEM,
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas};

interp(sstore,#{stack:=[Key,Value|Stack], storage:=Storage, gas:=G}=State) ->
  St1=maps:put(Key,Value,Storage),
  Gas=case {maps:is_key(Key,Storage),Value=/=0} of
       {false,true} ->
         20000;
       {true,true} ->
         5000;
       {false,false} ->
         0;
       {true,false} ->
         -15000
     end,
  ?TRACE({sstore, {Key,Value,Gas}}),
  State#{stack=>Stack,storage=>St1,gas=>G-Gas};

interp(sload,#{stack:=[Key|Stack],storage:=Storage, gas:=G}=State) ->
  Value=maps:get(Key,Storage,0),
  ?TRACE({sload, {Key,Value}}),
  State#{stack=>[Value|Stack],gas=>G-800};

interp({log,0},#{stack:=[Offset,Len|Stack],memory:=RAM,logger:=Logger, gas:=G}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[]),
  State#{stack=>Stack,gas=>G-375};

interp({log,1},#{stack:=[Offset,Len,Topic0|Stack],memory:=RAM,logger:=Logger, gas:=G}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0]),
  State#{stack=>Stack,gas=>G-750};

interp({log,2},#{stack:=[Offset,Len,Topic0,Topic1|Stack],memory:=RAM,logger:=Logger, gas:=G}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0,Topic1]),
  State#{stack=>Stack,gas=>G-1125};

interp({log,3},#{stack:=[Offset,Len,Topic0,Topic1, Topic2|Stack],memory:=RAM,logger:=Logger, gas:=G}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0,Topic1,Topic2]),
  State#{stack=>Stack,gas=>G-1500};

interp({log,4},#{stack:=[Offset,Len,Topic0,Topic1, Topic2, Topic3|Stack],memory:=RAM,logger:=Logger, gas:=G}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Logger(BValue,[Topic0,Topic1,Topic2,Topic3]),
  State#{stack=>Stack,gas=>G-1875};

interp(Instr,_State) ->
  {error,{bad_instruction,Instr}}.

