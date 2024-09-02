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
-define(INHERIT_OPTS, [bad_instruction,logger,sload,get,trace,static,cb_beforecall]).

-type callinfo() :: #{
                      'address':=integer(),
                      'callvalue':=integer(),
                      'caller':=integer(),
                      'gasprice':=integer(),
                      'origin':=integer()
                     }.

%sload(Address::integer(), Key::integer(), Xtra::map()) ->
%   {'ok', Value::integer(), NewXtra::map(), 'true'|'false'}
%sstore(Address::integea(), Key::integer(), Value::integer(), Xtra::map()) ->
%   {'ok', NewXtra::map(), 'true'|'false'}

-spec run(#{'code':=binary(),
            'gas':=integer(),
            'stack':=list(),
            'memory':=binary(),
            'storage':=map(),
            'logger'=>function(),
            'data':=callinfo(),
            'cd':=binary(),
            'create':=fun((Value::integer(),Code::binary(),Acc::map()) -> {#{address:=integer()},map()}),
			'sload':=fun((Address::integer(),Key::integer(),Xtra::map(),IntState::map()) ->
						{'ok', Value::integer(), NewXtra::map(), 'true'|'false'}) |
					 fun((Address::integer(),Key::integer(),Xtra::map()) ->
						{'ok', Value::integer(), NewXtra::map()}) | %legacy
					 fun((Address::integer()) -> Value::integer()), %legacy
            'sstore':=fun((Address::integer(),Key::integer(),Value::integer(),Xtra::map(),IntState::map()) ->
					{'ok', NewXtra::map(), 'true'|'false'} ),
            'custom_call':=fun(('call'|'staticcall',
							   AddressFrom::integer(),
							   AddressTo::integer(),
							   Value::integer(),
							   CallData::binary(),
							   Gas::integer(),
							   ExtraData::map(),
							   IntState::map()) ->
					{ 1|0, ReturnData::binary(), GasLeft::integer(), NewXtraData::map() }),
            'get'=>#{
                     'balance'=>function(),
                     'code'=>function()
                    },
            'cb_beforecall' => fun(),
            'finfun' => fun(),
            'static' => integer(),
            'embedded_code'=>map(),
            'extra' := map(),
            'trace'=>pid()|undefined
}) ->
  {'done', 'stop'|'invalid'|{'revert',binary()}|{'return',binary()}, #{
                                                                       gas:=integer(),
                                                                       storage:=#{},
                                                                       memory:=binary(),
                                                                       _ => _}}
  |
  {'error', 'nogas'|{'jump_to',integer()}|{'bad_instruction',any()}, #{
                                                                       memory:=binary(),
                                                                       _ => _
                                                                      }}.
run(#{code:=Code}=State) when is_binary(Code) ->
  run_next(0,Code,State).

allow_opcode(_, selfdestruct) -> false; %does not dupported
allow_opcode(false, _) -> true;
allow_opcode(true, create) -> false;
allow_opcode(true, create2) -> false;
allow_opcode(true, {log,_}) -> false;
allow_opcode(true, sstore) -> false;
allow_opcode(true, _) -> true.

run_next(PC, Code, #{depth:=D,gas:=Gas,stack:=Stack}=State) ->
  if(Gas<1) ->
      finish(error,nogas,State);
    Code == <<>> ->
      finish(done, eof, State);
    true ->
      ?TRACE({stack, D, Stack}),
      {Inst,Rest,NextPC}=case eevm_dec:decode(Code) of
                           {I, RestCode} ->
                             {I, RestCode, PC+1};
                           {I, RestCode, Offset} ->
                             {I, RestCode, PC+Offset}
                         end,
      ?TRACE({opcode, D, {PC, Inst}}),
      case allow_opcode(maps:is_key(static,State), Inst) of
        false ->
          finish(fin, invalid, State#{pc=>PC});
        true ->
          case interp(Inst,State#{pc=>PC}) of
            {fin, Reason, S2} ->
              finish(done,Reason,S2);
            {goto,Dst,S2} ->
              <<_:Dst/binary,JmpOpcode:8/integer,NewCode/binary>> =maps:get(code,State),
              case JmpOpcode of
                16#5b -> %jumpdest
                  ?TRACE({jump_ok, D, Dst}),
                  run_next(Dst+1,NewCode,S2);
                Other ->
                  ?TRACE({jump_error, D, Other}),
                  finish(error, {jump_to, Other}, State)
              end;
            {return, Data, S2} ->
              finish(done,{return,Data},S2);
            {error,{bad_instruction,_}=Reason,S2} ->
              finish(error,Reason,S2);
            #{stack:=_}=S2 ->
              run_next(NextPC,Rest,S2)
          end
      end
  end.

finish(Res, Reason, #{finfun:=FinFun}=State) when is_function(FinFun) ->
  {Res, Reason, FinFun(Res,Reason,State)};

finish(Res, Reason, State) ->
  {Res, Reason, State}.

%-=[ 0x00 ]=-

interp(stop,State) ->
  {fin, stop, State};

interp(add,#{stack:=[A,B|Stack], gas:=G}=State) ->
  Res=A+B,
  State#{stack=>[Res|Stack],gas=>G-3};

interp(mul,#{stack:=[A,B|Stack], gas:=G}=State) ->
  Res=A*B,
  State#{stack=>[Res|Stack], gas=>G-5};

interp(sub,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A - B|Stack], gas=>G-3};

interp('div',#{stack:=[_,0|Stack], gas:=G}=State) ->
  State#{stack=>[0|Stack], gas=>G-5};

interp('div',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A div B|Stack], gas=>G-5};

interp('sdiv',#{stack:=[SA,SB|Stack], gas:=G}=State) ->
  <<A:256/big-signed>> = <<SA:256/big>>,
  <<B:256/big-signed>> = <<SB:256/big>>,
  Div=A div B,
  Res=if Div<0 ->
           binary:decode_unsigned(<<Div:256/big-signed>>);
         true ->
           Div
      end,
  State#{stack=>[Res|Stack], gas=>G-5};


interp('mod',#{stack:=[_,0|Stack], gas:=G}=State) ->
  State#{stack=>[0|Stack], gas=>G-5};

interp('mod',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A rem B|Stack], gas=>G-5};

interp('smod',#{stack:=[SA,SB|Stack], gas:=G}=State) ->
  <<A:256/big-signed>> = <<SA:256/big>>,
  <<B:256/big-signed>> = <<SB:256/big>>,
  Div=A rem B,
  Res=if Div<0 ->
           binary:decode_unsigned(<<Div:256/big-signed>>);
         true ->
           Div
      end,
  State#{stack=>[Res|Stack], gas=>G-5};

interp('addmod',#{stack:=[A,B,N|Stack], gas:=G}=State) ->
  State#{stack=>[(A+B) rem N|Stack], gas=>G-8};

interp('mulmod',#{stack:=[A,B,N|Stack], gas:=G}=State) ->
  State#{stack=>[(A*B) rem N|Stack], gas=>G-8};

interp(exp,#{stack:=[A,B|Stack], gas:=G}=State) ->
  Res=trunc(math:pow(A,B)),
  State#{stack=>[Res|Stack], gas=>G-10};

interp(signextend,#{stack:=[B,X|Stack], gas:=G}=State) ->
  BitLen=(B+1)*8,
  MBitLen=256-BitLen,
  <<_:MBitLen/big,Val:BitLen/big-signed>> = <<X:256/big>>,
  <<Res:256/big>> = <<Val:256/big-signed>>,
  State#{stack=>[Res|Stack], gas=>G-10};

%-=[ 0x10 ]=-

interp(lt,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(slt,#{stack:=[SA,SB|Stack], gas:=G}=State) ->
  <<A:256/big-signed>> = <<SA:256/big>>,
  <<B:256/big-signed>> = <<SB:256/big>>,
  State#{stack=>[if A<B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(gt,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A>B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(sgt,#{stack:=[SA,SB|Stack], gas:=G}=State) ->
  <<A:256/big-signed>> = <<SA:256/big>>,
  <<B:256/big-signed>> = <<SB:256/big>>,
  State#{stack=>[if A>B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(eq,#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[if A==B -> 1; true -> 0 end|Stack], gas=>G-3};

interp(iszero,#{stack:=[A|Stack], gas:=G}=State) ->
  State#{stack=>[if A==0 -> 1;
                    true -> 0
                 end|Stack], gas=>G-3};

interp('and',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A band B|Stack], gas=>G-3};

interp('or',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A bor B|Stack], gas=>G-3};

interp('xor',#{stack:=[A,B|Stack], gas:=G}=State) ->
  State#{stack=>[A bxor B|Stack], gas=>G-3};

interp('not',#{stack:=[A|Stack], gas:=G}=State) ->
  Not=A bxor 16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff,
  %?TRACE({'not', Not}),
  State#{stack=>[Not|Stack], gas=>G-3};

interp('byte',#{stack:=[0,A|Stack], gas:=G}=State) ->
  <<B:8/big,_/binary>> = <<A:256/big>>,
  State#{stack=>[B|Stack], gas=>G-3};

interp('byte',#{stack:=[I,A|Stack], gas:=G}=State) ->
  <<_:I/binary,B:8/big,_/binary>> = <<A:256/big>>,
  State#{stack=>[B|Stack], gas=>G-3};

interp(shl,#{stack:=[Off,A|Stack], gas:=G}=State) ->
  Sh=(A bsl Off) band 16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff,
  State#{stack=>[Sh|Stack], gas=>G-3};

interp(shr,#{stack:=[Off,A|Stack], gas:=G}=State) ->
  Sh=A bsr Off,
  State#{stack=>[Sh|Stack], gas=>G-3};

interp(sar,#{stack:=[Off,SA|Stack], gas:=G}=State) ->
  <<A:256/big-signed>> = <<SA:256/big>>,
  <<Sh:256/big>> = <<(A bsr Off):256/big-signed>>,
  State#{stack=>[Sh|Stack], gas=>G-3};

%-=[ 0x20 ]=-

interp(sha3, #{stack:=[Off,Len|Stack],memory:=RAM, gas:=G, sha3:=Fun}=State) ->
  RamData=eevm_ram:read(RAM,Off,Len),
  Hash=Fun(RamData),
  Value = binary:decode_unsigned(Hash),
  ?TRACE({sha3, {Len,Off,RamData,Hash}}),
  Gas=30+(6*((Len+31) div 32)),
  State#{stack=>[Value|Stack], gas=>G-Gas};

interp(sha3, #{stack:=[Off,Len|Stack],memory:=RAM, gas:=G}=State) ->
  RamData=eevm_ram:read(RAM,Off,Len),
  {ok,Hash}=ksha3:hash(256, RamData),
  Value = binary:decode_unsigned(Hash),
  ?TRACE({sha3, {Len,Off,RamData,Hash}}),
  Gas=30+(6*((Len+31) div 32)),
  State#{stack=>[Value|Stack], gas=>G-Gas};

%-=[ 0x30 ]=-

interp(address, #{stack:=Stack, data:=#{address:=A}, gas:=G}=State) ->
  State#{stack=>[A|Stack], gas=>G-2};

interp(balance, #{stack:=[Address|Stack], gas:=G, get:=#{balance:=GF}, extra:=Xtra}=State) ->
  case GF(Address, Xtra) of
    {ok, Value, NewXtra} ->
      State#{stack=>[Value|Stack], gas=>G-100, extra=>NewXtra};
    Value when is_integer(Value) ->
      State#{stack=>[Value|Stack], gas=>G-100}
  end;

interp(origin, #{stack:=Stack, data:=#{origin:=A}, gas:=G}=State) ->
  State#{stack=>[A|Stack], gas=>G-2};

interp(caller, #{stack:=Stack, data:=#{caller:=A}, gas:=G}=State) ->
  State#{stack=>[A|Stack], gas=>G-2};

interp(callvalue, #{stack:=Stack, data:=#{callvalue:=A}, gas:=G}=State) ->
  State#{stack=>[A|Stack], gas=>G-2};

interp(calldataload, #{stack:=[I|Stack], gas:=G,cd:=CD}=State) ->
  BValue = eevm_ram:read(CD,I,32),
  Value = binary:decode_unsigned(BValue),
  ?TRACE({calldataload, {I,32,BValue,Value}}),
  State#{stack=>[Value|Stack], gas=>G-2};

interp(calldatasize, #{stack:=Stack,cd:=CD, gas:=G}=State) ->
  Value=size(CD),
  State#{stack=>[Value|Stack], gas=>G-2};

interp(calldatacopy, #{stack:=[RAMOff,Off,Len|Stack],cd:=CD, gas:=G, memory:=RAM}=State) ->
  BValue = eevm_ram:read(CD,Off,Len),
  RAM1=eevm_ram:write(RAM,RAMOff,BValue),
  Gas=3+(3*(((Len+31) div 32))) + ?CMEM,
  State#{stack=>Stack, gas=>G-Gas, memory => RAM1};

interp(codesize, #{stack:=Stack,code:=Code, gas:=G}=State) ->
  Value=size(Code),
  State#{stack=>[Value|Stack], gas=>G-2};

interp(codecopy,#{stack:=[RAMOff,CodeOff,Len|Stack], gas:=G,memory:=RAM,code:=Code}=State) ->
  Value=eevm_ram:read(Code,CodeOff,Len),
  RAM1=eevm_ram:write(RAM,RAMOff,Value),
  ?TRACE({codecopy, {Len,CodeOff,RAMOff,Value}}),
  Gas=3+(3*(((Len+31) div 32))) + ?CMEM,
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas};

interp(gasprice, #{stack:=Stack, data:=#{gasprice:=A}, gas:=G}=State) ->
  State#{stack=>[A|Stack], gas=>G-2};

interp(extcodesize, #{stack:=[Address|Stack],
					  gas:=G,
					  get:=#{code:=GF},
					  extra:=OldXtra}=State) when is_function(GF, 3) ->
	{Code,Xtra,_Cached}=GF(Address, OldXtra, State),
	State#{stack=>[size(Code)|Stack], gas=>G-2600, extra=>Xtra};

interp(extcodecopy, #{stack:=[Address,RAMOff,CodeOff,Len|Stack], gas:=G,
					  memory:=RAM,
					  get:=#{code:=GF},
					  extra:=OldXtra}=State) when is_function(GF, 3) ->
	{Code,Xtra,_Cached}=GF(Address, OldXtra, State),
	Value=eevm_ram:read(Code,CodeOff,Len),
	RAM1=eevm_ram:write(RAM,RAMOff,Value),
	?TRACE({extcodecopy, {Len,CodeOff,RAMOff,Value}}),
	Gas=2600+(3*(((Len+31) div 32))) + ?CMEM,
	State#{stack=>Stack,memory=>RAM1, gas=>G-Gas, extra=>Xtra};

interp(extcodesize, #{stack:=[Address|Stack],
                      gas:=G,
                      get:=#{code:=GF},
                     extra:=OldXtra}=State) ->
  {Code,Xtra}=case GF(Address, OldXtra) of
                {ok, Value, NewXtra,_} ->
                  {Value, NewXtra};
                {ok, Value, NewXtra} ->
                  {Value, NewXtra};
                Value when is_binary(Value) ->
                  {Value, OldXtra}
              end,
  State#{stack=>[size(Code)|Stack], gas=>G-2600, extra=>Xtra};

interp(extcodecopy, #{stack:=[Address,RAMOff,CodeOff,Len|Stack], gas:=G,
                      memory:=RAM,
                      get:=#{code:=GF},
                      extra:=OldXtra}=State) ->
  {Code,Xtra}=case GF(Address, OldXtra) of
                {ok, Val, NewXtra,_} ->
                  {Val, NewXtra};
                {ok, Val, NewXtra} ->
                  {Val, NewXtra};
                Val when is_binary(Val) ->
                  {Val, OldXtra}
              end,
  Value=eevm_ram:read(Code,CodeOff,Len),
  RAM1=eevm_ram:write(RAM,RAMOff,Value),
  ?TRACE({extcodecopy, {Len,CodeOff,RAMOff,Value}}),
  Gas=2600+(3*(((Len+31) div 32))) + ?CMEM,
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas, extra=>Xtra};

interp(returndatasize, #{stack:=Stack,gas:=G, return:=Data}=State) ->
  State#{stack=>[size(Data)|Stack], gas=>G-2};

interp(returndatacopy, #{stack:=[RAMOff,DataOff,Len|Stack],gas:=G,
                         memory:=RAM, return:=Data}=State) ->
  Value=eevm_ram:read(Data,DataOff,Len),
  %io:format("Return ~w ~p~n",[DataOff,Value]),
  RAM1=eevm_ram:write(RAM,RAMOff,Value),
  ?TRACE({returndatacopy, {Len,DataOff,RAMOff,Value}}),
  Gas=3+(3*(((Len+31) div 32))) + ?CMEM,
  State#{stack=>Stack, gas=>G-Gas,memory=>RAM1};

interp(extcodehash, #{stack:=[Address|Stack],
                      gas:=G,
                      get:=#{code:=GF},
                      extra:=OldXtra}=State) ->
  {Code,Xtra}=case GF(Address, OldXtra) of
                {ok, Value, NewXtra,_} ->
                  {Value, NewXtra};
                {ok, Value, NewXtra} ->
                  {Value, NewXtra};
                Value when is_binary(Value) ->
                  {Value, OldXtra}
              end,
  {ok,Hash}=ksha3:hash(256, Code),
  State#{stack=>[Hash|Stack], gas=>G-2600, extra=>Xtra};

%-=[ 0x40 ]=-

%-=[ 0x50 ]=-

interp(pop,#{stack:=[Val|Stack],gas:=Gas}=State) ->
  ?TRACE({pop, Val}),
  State#{stack=>Stack,gas=>Gas-1};

interp(mload,#{stack:=[Offset|Stack],memory:=RAM, gas:=G}=State) ->
  Value=binary:decode_unsigned(eevm_ram:read(RAM,Offset,32)),
  ?TRACE({mload, {Offset,Value}}),
  State#{stack=>[Value|Stack], gas=>G-3};

interp(mstore,#{stack:=[Offset,Val|Stack],memory:=RAM, gas:=G}=State) ->
  ?TRACE({mstore, {Offset,Val}}),
  RAM1=eevm_ram:write(RAM,Offset,<<Val:256/big>>),
  Gas=3+?CMEM,
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas};

interp(mstore8,#{stack:=[Offset,Val256|Stack],memory:=RAM, gas:=G}=State) ->
  Val=Val256 band 255,
  ?TRACE({mstore8, {Offset,Val}}),
  RAM1=eevm_ram:write(RAM,Offset,<<Val:8/big>>),
  Gas=3+?CMEM,
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas};

%in case of sload and sstore functions exists do not use internal storage
%sload(Address::integer(), Key::integer(), Xtra::map()) ->
%   {'ok', Value::integer(), NewXtra::map(), 'true'|'false'}
%sstore(Address::integea(), Key::integer(), Value::integer(), Xtra::map()) ->
%   {'ok', NewXtra::map(), 'true'|'false'}

interp(sload,#{stack:=[Key|Stack], data:=#{address:=Addr},
               gas:=G,
               sload:=SLoad, sstore:=SStore,
			   extra:=Xtra}=State) when
	  is_function(SLoad, 4), is_function(SStore, 5) ->
	case SLoad(Addr, Key, Xtra, State) of
		{ok, Value, NewXtra, IsCold } ->
			?TRACE({sload, {Key,Value}}),
			Gas1=case IsCold of
					 true -> G-2100;
					 false -> G-800
				 end,
			State#{stack=>[Value|Stack],
				   gas=>Gas1,
				   extra=>NewXtra}
	end;

interp(sstore,#{stack:=[Key,Value|Stack], data:=#{address:=Addr},
				gas:=G,
				sload:=SLoad, sstore:=SStore,
				extra:=Xtra}=State) when
	  is_function(SLoad, 4), is_function(SStore, 5) ->
	case SStore(Addr, Key, Value, Xtra, State) of
		{ok, NewXtra, PreExists } ->
			Gas=case {PreExists, Value=/=0} of
					{false,true} ->
						20000;
					{true,true} ->
						5000;
					{false,false} ->
						800;
					{true,false} ->
						-15000
				end,
			?TRACE({sstore, {Key,Value,Gas}}),
			%io:format("SSTORE ~s => ~s~n",[hex:encodex(Key),hex:encodex(Value)]),
			State#{stack=>Stack,
				   gas=>G-Gas,
				   extra=>NewXtra}
	end;

%in case of sload function defined we can load data from external database
interp(sload,#{stack:=[Key|Stack],storage:=Storage, data:=#{address:=Addr},
               gas:=G,
               sload:=LoadFun,
               extra:=Xtra}=State) ->
  %io:format("CAll sload ~p~n",[Key]),
  case maps:is_key(Key, Storage) of
    true ->
      Value=maps:get(Key,Storage,0),
      ?TRACE({sload, {Key,Value}}),
      State#{stack=>[Value|Stack],gas=>G-100};
    false ->
      Res=loadhelper(LoadFun, Addr, Key, Xtra),
      case Res of
        {ok, Value, NewXtra} ->
          ?TRACE({sload, {Key,Value}}),
          State#{stack=>[Value|Stack],storage=>maps:put(Key,Value,Storage),gas=>G-2100,extra=>NewXtra};
        {ok, Value, NewXtra,_} ->
          ?TRACE({sload, {Key,Value}}),
          State#{stack=>[Value|Stack],storage=>maps:put(Key,Value,Storage),gas=>G-2100,extra=>NewXtra};
        Value when is_integer(Value) ->
          ?TRACE({sload, {Key,Value}}),
          State#{stack=>[Value|Stack],storage=>maps:put(Key,Value,Storage),gas=>G-2100}
      end
  end;

interp(sload,#{stack:=[Key|Stack],storage:=Storage, gas:=G}=State) ->
  Value=maps:get(Key,Storage,0),
  %io:format("CAll sload ~p~n",[Key]),
  ?TRACE({sload, {Key,Value}}),
  State#{stack=>[Value|Stack],gas=>G-800};

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
  %io:format("SSTORE ~s => ~s~n",[hex:encodex(Key),hex:encodex(Value)]),
  State#{stack=>Stack,storage=>St1,gas=>G-Gas};

interp(jump,#{stack:=[Dst|Stack]}=State) ->
  {goto,Dst,State#{stack=>Stack}};

interp(jumpi,#{stack:=[Dst,0|Stack]}=State) ->
  ?TRACE({jumpi, {Dst,false}}),
  State#{stack=>Stack};

interp(jumpi,#{stack:=[Dst,_NonZero|Stack]}=State) ->
  ?TRACE({jumpi, {Dst,true}}),
  {goto,Dst,State#{stack=>Stack}};

interp(pc, #{stack:=Stack, pc:=PC, gas:=G}=State) ->
  State#{stack=>[PC|Stack], gas=>G-2};

interp(msize, #{stack:=Stack, memory:=RAM, gas:=G}=State) ->
  State#{stack=>[size(RAM)|Stack], gas=>G-2};

interp(gas, #{stack:=Stack, gas:=G}=State) ->
  State#{stack=>[G-2|Stack], gas=>G-2};

interp(jumpdest,State) ->
  State;

interp(mcopy,#{stack:=[DstOff,SrcOff,Size|Stack], memory:=RAM, gas:=G}=State) ->
  ?TRACE({mcopy, {DstOff, SrcOff, Size}}),
  Value=eevm_ram:read(RAM,SrcOff,Size),
  RAM1=eevm_ram:write(RAM,DstOff,Value),
  Gas=3+?CMEM+(3*((Size div 32)+if Size rem 32==0 -> 0; true -> 1 end)),
  State#{stack=>Stack,memory=>RAM1, gas=>G-Gas};

interp(push0,#{stack:=Stack, gas:=G}=State) ->
  State#{stack=>[0|Stack], gas=>G-3};
%-=[ 0x60 ]=-

interp({push,_,Value},#{stack:=Stack, gas:=G}=State) ->
  State#{stack=>[Value|Stack], gas=>G-3};

%-=[ 0x80 ]=-

interp({dup,N},#{stack:=Stack, gas:=G}=State) ->
  Val=lists:nth(N,Stack),
  Stack1=[Val|Stack],
  State#{stack=>Stack1, gas=>G-3};

%-=[ 0x90 ]=-
%
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

interp({swap,6},#{stack:=[A,X1,X2,X3,X4,X5,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,A|Stack], gas=>G-3};

interp({swap,7},#{stack:=[A,X1,X2,X3,X4,X5,X6,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,A|Stack], gas=>G-3};

interp({swap,8},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,A|Stack], gas=>G-3};

interp({swap,9},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,A|Stack], gas=>G-3};

interp({swap,10},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,A|Stack], gas=>G-3};

interp({swap,11},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,A|Stack], gas=>G-3};

interp({swap,12},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,A|Stack], gas=>G-3};

interp({swap,13},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,A|Stack], gas=>G-3};

interp({swap,14},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,A|Stack], gas=>G-3};

interp({swap,15},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,A|Stack], gas=>G-3};

interp({swap,16},#{stack:=[A,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,B|Stack], gas:=G}=State) ->
  State#{stack=>[B,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,A|Stack], gas=>G-3};


%-=[ 0xA0 ]=-

interp({log,0},#{stack:=[Offset,Len|Stack],
                 memory:=RAM,logger:=Logger, gas:=G, extra:=Xtra}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Xtra1=Logger(BValue,[],Xtra,State),
  ?TRACE({log, BValue,[]}),
  State#{stack=>Stack,gas=>G-375,extra=>Xtra1};

interp({log,1},#{stack:=[Offset,Len,Topic0|Stack],
                 memory:=RAM,logger:=Logger, gas:=G, extra:=Xtra}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Xtra1=Logger(BValue,[Topic0],Xtra,State),
  ?TRACE({log, BValue,[Topic0]}),
  State#{stack=>Stack,gas=>G-750,extra=>Xtra1};

interp({log,2},#{stack:=[Offset,Len,Topic0,Topic1|Stack],
                 memory:=RAM,logger:=Logger, gas:=G, extra:=Xtra}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Xtra1=Logger(BValue,[Topic0,Topic1],Xtra,State),
  ?TRACE({log, BValue,[Topic0,Topic1]}),
  State#{stack=>Stack,gas=>G-1125,extra=>Xtra1};

interp({log,3},#{stack:=[Offset,Len,Topic0,Topic1, Topic2|Stack],
                 memory:=RAM,logger:=Logger, gas:=G, extra:=Xtra}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Xtra1=Logger(BValue,[Topic0,Topic1,Topic2],Xtra,State),
  ?TRACE({log, BValue,[Topic0,Topic1,Topic2]}),
  State#{stack=>Stack,gas=>G-1500,extra=>Xtra1};

interp({log,4},#{stack:=[Offset,Len,Topic0,Topic1, Topic2, Topic3|Stack],
                 memory:=RAM,logger:=Logger, gas:=G, extra:=Xtra}=State) ->
  BValue = eevm_ram:read(RAM,Offset,Len),
  Xtra1=Logger(BValue,[Topic0,Topic1,Topic2,Topic3],Xtra,State),
  ?TRACE({log, BValue,[Topic0,Topic1,Topic2,Topic3]}),
  State#{stack=>Stack,gas=>G-1875,extra=>Xtra1};

interp(create, #{stack:=[Value,MemOff,Len|Stack],
                 memory:=RAM,
                 create:=CF,
                 gas:=G,
                 extra:=Xtra}=State) ->
  Code=eevm_ram:read(RAM,MemOff,Len),
  {#{address:=Address},NewExtra}=CF(Value, Code, Xtra),
  Gas=3200,
  State#{stack=>[Address|Stack], gas=>G-Gas, extra=>NewExtra};

interp(create2, #{stack:=[Value,MemOff,Len,_Salt|Stack],
                  memory:=RAM,
                  create:=CF,
                  gas:=G,
                  extra:=Xtra}=State) ->
  Code=eevm_ram:read(RAM,MemOff,Len),
  {#{address:=Address},NewExtra}=CF(Value, Code, Xtra),
  Gas=3200,
  State#{stack=>[Address|Stack], gas=>G-Gas, extra=>NewExtra};

%-=[ 0xF0 ]=-

interp(return,#{stack:=[Off,Len|Stack], memory:=RAM}=State) ->
  Value=eevm_ram:read(RAM,Off,Len),
  ?TRACE({return, Value}),
  {return,Value,State#{stack=>Stack}};

interp(CALL, #{data:=#{address:=From},
               stack:=Stack0,
			   custom_call:=CustomCall,
               memory:=RAM,
               extra:=OldXtra,
               gas:=G0}=State)
  when (CALL == call
		orelse CALL==staticcall
		orelse CALL==delegatecall
		orelse CALL==callcode)
	   andalso is_function(CustomCall, 7) ->
  #{stack:=Stack,
    return_off:=RetOff,
    calldata:=CallData,
	gas:=PassGas,
	address:=Address,
	value:=Value,
    return_len:=MaxRetLen}=call_args(CALL, Stack0, RAM),
  G1=(G0-100),
  KeepGas=G1 div 64,
  GPassed=min(G1-KeepGas,PassGas),

  % CustomCall(call|staticcall, AddressFrom, AddressTo, Value, CallData, Gas, Xtra) ->
  % { 1|0, <<ReturnData/binary>>, GasLeft, Xtra }

  { RetCode,
	ReturnBin,
	GasLeft,
	NewXtra } = CustomCall(CALL, From, Address, Value, CallData,  GPassed, OldXtra),

  Burned=GPassed-GasLeft,

  RAM1=if(size(ReturnBin)>MaxRetLen) ->
			 <<Ret1:MaxRetLen/binary,_/binary>> = ReturnBin,
			 eevm_ram:write(RAM,RetOff,Ret1);
		 true ->
			 eevm_ram:write(RAM,RetOff,ReturnBin)
	   end,

  State#{stack=>[RetCode|Stack],
		 gas=>G1-Burned,
		 return=>ReturnBin,
		 memory=>RAM1,
		 extra=>NewXtra};


interp(CALL, #{data:=#{address:=Self}=Data,
               storage:=Storage0,
               stack:=[PassGas,Address|_]=Stack0,
               get:=#{code:=GF},
               memory:=RAM,
               extra:=OldXtra,
               gas:=G0}=State)
  when CALL == call; CALL==staticcall; CALL==delegatecall; CALL==callcode ->
  #{stack:=Stack,
    return_off:=RetOff,
    calldata:=_CD,
    return_len:=MaxRetLen}=CallArgs=call_args(CALL, Stack0, RAM),
  EmbeddedFun=maps:get(Address,maps:get(embedded_code,State,#{}),undefined),
  G1=(G0-100),
  KeepGas=G1 div 64,
  GPassed=min(G1-KeepGas,PassGas),

  if(is_function(EmbeddedFun)) ->
      {GasLeft, RetCode, ReturnBin, Xtra} = call_embedded(CALL, GPassed, CallArgs#{data=>Data}, EmbeddedFun, OldXtra),
      Burned=GPassed-GasLeft,

      RAM1=if(size(ReturnBin)>MaxRetLen) ->
               <<Ret1:MaxRetLen/binary,_/binary>> = ReturnBin,
               eevm_ram:write(RAM,RetOff,Ret1);
             true ->
               eevm_ram:write(RAM,RetOff,ReturnBin)
           end,
      State#{stack => [RetCode|Stack],
             gas   => G1-Burned,
             extra => Xtra,
             return=>ReturnBin,
             memory=>RAM1};
    true ->
      {Code,Xtra}=case
					  if is_function(GF,2) -> GF(Address, OldXtra);
						 is_function(GF,3) -> GF(Address, OldXtra, State)
					  end of
					  {ok, Value, NewXtra1,_} ->
                      if is_binary(Value) ->
                           {Value, NewXtra1};
                         true ->
                           {<<>>, NewXtra1}
					  end;
                    {ok, Value, NewXtra1} ->
                      if is_binary(Value) ->
                           {Value, NewXtra1};
                         true ->
                           {<<>>, NewXtra1}
                      end;
                    Value when is_binary(Value) ->
                      {Value, OldXtra}
                  end,
      SameStorage=Address==Self orelse CALL==callcode orelse CALL==delegatecall,
      {Stor0,Xtra0}=if SameStorage ->
                         {Storage0,Xtra};
                       true ->
                         StorToSave=maps:merge(
                                      maps:get({Self,stor},Xtra,#{}),
                                      Storage0),
                         Changes=[Self|maps:get(changed,Xtra,[])],
                         Xtra00=Xtra#{
                                  changed => Changes,
                                  {Self,stor} => StorToSave
                                 },
                         {maps:get({Address,stor},Xtra,#{}),Xtra00}
                    end,
      {GasLeft,
       RetCode,
       ReturnBin,
       NewXtra,
       Stor1}=try
                Xtra1=case maps:is_key(cb_beforecall, State) of
                        false -> Xtra0;
                        true ->
                          Fun=maps:get(cb_beforecall, State),
                          Fun(CALL,Self,Code,GPassed, CallArgs, Xtra0)
                      end,

                call_ext(CALL, Code, GPassed, CallArgs, Stor0, Xtra1, State)
              catch throw:{cancel_call,_Reason} ->
                      {GPassed, 0, <<>>, Xtra0, Stor0}
              end,

      Stor2=if SameStorage ->
                 Stor1;
               true ->
                 maps:get({Self,stor},NewXtra)
            end,
      NewXtra2=if SameStorage ->
                    NewXtra;
                  true ->
                    Changes2=[Address|maps:get(changed,NewXtra,[])],
                    NewXtra#{changed => Changes2,
                             {Address,stor} =>
                             maps:merge(
                               maps:get({Address,stor},NewXtra,#{}),
                               Stor1)
                            }
               end,

      Burned=GPassed-GasLeft,

      RAM1=if(size(ReturnBin)>MaxRetLen) ->
               <<Ret1:MaxRetLen/binary,_/binary>> = ReturnBin,
               eevm_ram:write(RAM,RetOff,Ret1);
             true ->
               eevm_ram:write(RAM,RetOff,ReturnBin)
           end,

      State#{stack=>[RetCode|Stack],
             gas=>G1-Burned,
             storage=>Stor2,
             return=>ReturnBin,
             memory=>RAM1,
             extra=>NewXtra2}
  end;

interp(revert,#{memory:=RAM,stack:=[MemOff,MemLen|Stack]}=State) ->
  Value=eevm_ram:read(RAM,MemOff,MemLen),
  ?TRACE({revert, Value}),
  {fin,{revert,Value},
   State#{stack=>[0|Stack]}
  };

interp(invalid,State) ->
  {fin, invalid, State};

interp(Instr,#{bad_instruction:=BIF}=State) when is_function(BIF,2) ->
  BIF(Instr,State);

interp(Instr,State) ->
  {error,{bad_instruction,Instr},State}.

loadhelper(LoadFun, Addr, Key, State) ->
  case erlang:fun_info(LoadFun,arity) of
    {arity, 1} ->
      LoadFun(Key);
    {arity, 3} ->
      LoadFun(Addr, Key, State)
  end.

%% ==== [ call methods ] ====

call_args(call, [Gas,Address,CallValue,ArgOff,ArgLen,RetOff,RetLen|Stack], RAM) ->
  CallData=eevm_ram:read(RAM,ArgOff,ArgLen),
  #{
    gas => Gas,
    address => Address,
    value => CallValue,
    calldata => CallData,
    return_off => RetOff,
    return_len => RetLen,
    stack => Stack
   };

call_args(callcode, [Gas,Address,CallValue,ArgOff,ArgLen,RetOff,RetLen|Stack], RAM) ->
  CallData=eevm_ram:read(RAM,ArgOff,ArgLen),
  #{
    gas => Gas,
    address => Address,
    value => CallValue,
    calldata => CallData,
    return_off => RetOff,
    return_len => RetLen,
    stack => Stack
   };

call_args(delegatecall, [Gas,Address,ArgOff,ArgLen,RetOff,RetLen|Stack], RAM) ->
  CallData=eevm_ram:read(RAM,ArgOff,ArgLen),
  #{
    gas => Gas,
    address => Address,
    calldata => CallData,
    value => 0,
    return_off => RetOff,
    return_len => RetLen,
    stack => Stack
   };

call_args(staticcall, [Gas,Address,ArgOff,ArgLen,RetOff,RetLen|Stack], RAM) ->
  CallData=eevm_ram:read(RAM,ArgOff,ArgLen),
  #{
    gas => Gas,
    address => Address,
    value => 0,
    calldata => CallData,
    return_off => RetOff,
    return_len => RetLen,
    stack => Stack
   }.

call_embedded(_Method, Gas, #{calldata:=CallData}, EmbeddedFun, State) when
    is_function(EmbeddedFun, 2)->
  {RetCode, ReturnBin, State2}=EmbeddedFun(CallData, State),
  {Gas-100, RetCode, ReturnBin, State2};

call_embedded(_Method, Gas, #{calldata:=CallData}=All, EmbeddedFun, State) when
    is_function(EmbeddedFun, 3)->
  {RetCode, ReturnBin, State2}=EmbeddedFun(CallData, All, State),
  {Gas-100, RetCode, ReturnBin, State2};

call_embedded(_Method, Gas, #{calldata:=CallData}, EmbeddedFun, State) ->
  {RetCode, ReturnBin}=EmbeddedFun(CallData),
  {Gas-100, RetCode, ReturnBin, State}.


-spec callinfo(Method::atom(), CallArgs::map(), OldData::callinfo()) -> callinfo().

callinfo(staticcall, #{address:=Address}=_CallArgs, Data) ->
  Data#{
    address=>Address,
    caller=>maps:get(address, Data),
    value=>0
   };

callinfo(callcode, _CallArgs, Data) ->
  Data#{
    callvalue => 0,
    caller => maps:get(address, Data)
   };

callinfo(delegatecall, #{}, Data) ->
  Data#{
   };

callinfo(call, #{address:=Address, value:=Value}, Data) ->
  Data#{
    address=>Address,
    caller=>maps:get(address, Data),
    callvalue=>Value
   }.

call_ext(_Method,
         <<>>,
         Gas,
         #{address:=Address},
         Stor0,
         Xtra,
         #{depth:=D}=State) ->
  ?TRACE({callret, D, Address,nocode,<<>>}),
  {Gas, 1, <<>>, Xtra, Stor0};

call_ext(Method=staticcall,
         Code,
         Gas,
         #{calldata:=CD,address:=Address}=CallArgs,
         Stor0,
         Xtra,
         #{depth:=D,data:=Data}=State) ->

  ?TRACE({Method, D, Address,CallArgs}),

  %#{'sload'=>fun(),'static'=>integer(),'embedded_code'=>map(),'get'=>#{'balance'=>fun(),'code'=>fun()},'trace'=>pid() | 'undefined'}

  case eevm:eval(Code,
                 Stor0,
                 maps:merge(
                   #{gas=>Gas,
                     cd=>CD,
                     depth=>D+1,
                     data=>callinfo(Method,CallArgs,Data),
                     extra=>Xtra,
                     static=>true
                    },
                   maps:with(?INHERIT_OPTS,State)
                  )) of
    {done,Res,
     #{gas:=GasLeft,
       storage:=Stor1,
       extra:=Xtra1}} ->
      {Bin,RetVal}=case Res of
                     {return, Bin1} -> {Bin1,1};
                     eof -> {<<1>>,1};
                     stop -> {<<1>>,1};
                     _ -> {<<0>>,0}
                   end,
      ?TRACE({callret, D, Address,Res,Bin}),
      %io:format("Callcode ret {done,~p,...} -> ~p~n",[Res, RetVal]),

      {GasLeft, RetVal, Bin, Xtra1, Stor1};
    {fin, invalid, _} ->
      {0, 0, <<>>, Xtra, Stor0}
  end;

call_ext(Method,
         Code,
         Gas,
         #{calldata:=CD,address:=Address,value:=_Value}=CallArgs,
         Stor0,
         Xtra,
         #{depth:=D,data:=Data}=State) ->

  {done,Res,
   #{gas:=GasLeft,
     storage:=Stor1,
     extra:=Xtra1}=_S2}=eevm:eval(Code,
                                 Stor0,
                                 maps:merge(
                                   #{gas=>Gas,
                                     cd=>CD,
                                     depth=>D+1,
                                     data=>callinfo(Method, CallArgs, Data),
                                     extra=>Xtra
                                    },
                                   maps:with(?INHERIT_OPTS,State)
                                  )),
  {Bin,RetVal}=case Res of
                 {return, Bin1} -> {Bin1,1};
                 {revert,Bin1} ->
                   ?TRACE({callret, D, Address,revert,Bin1}),
                   case maps:is_key(revert_throws,State) of
                     true ->
                       throw({revert, Bin1,GasLeft});
                     false ->
                       {Bin1, 0}
                   end;
                 eof -> {<<1>>,1};
                 stop -> {<<1>>,1};
                 _ -> {<<0>>,0}
               end,
  ?TRACE({callret, D, Address,Res,Bin}),
  %io:format("Callcode ret {done,~p,...} -> ~p~n",[Res, RetVal]),

  if Stor0==Stor1 -> %stor not changed
       {GasLeft, RetVal, Bin, Xtra1, Stor0};
     true ->
       Changes=[Address|maps:get(changed,Xtra1,[])],
       Stor2=maps:merge(maps:get({Address,stor},Xtra1,#{}),Stor1),
       {GasLeft, RetVal, Bin,
        maps:put(changed, Changes,
                 maps:put({Address,stor},Stor2,Xtra1)
                ),
        Stor1
       }
  end.

