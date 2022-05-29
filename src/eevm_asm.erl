-module(eevm_asm).
-export([disassemble/1, assemble/1, assemble/2]).

-export([parse_asm/1, parse_asm/2, asm/1, code2mem/2]).

disassemble(Code) when is_binary(Code) ->
  lists:flatten(
  lists:map(
    fun({push,N,V}) ->
        io_lib:format("push~w 0x~.16B~n",[N,V]);
       ({Opcode,N}) when is_atom(Opcode) ->
        io_lib:format("~s~w~n",[Opcode,N]);
       (Opcode) when is_atom(Opcode) ->
        io_lib:format("~s~n",[Opcode])
    end,
    eevm_dec:decode2list(Code)
   )).

assemble(Text) ->
  assemble(Text, #{}).

assemble(Text, Data) ->
  asm( parse_asm(Text,Data)).

estimate_size(Src) when is_list(Src) ->
  lists:foldl(
  fun({push,N,_}, Acc) ->
      Acc+N+1;
     ({pushlbl,_}, Acc) ->
      Acc+4;
     (_, Acc) ->
      Acc+1
  end, 0, Src).

bytes_for_integer(N) ->
  trunc(math:ceil(math:log(N+1)/math:log(256))).

asm(Src) when is_list(Src) ->
  EstimateSize=estimate_size(Src)+16,
  JLen=bytes_for_integer(EstimateSize),

  {_PC, Code, Labels, IsLblFound} = lists:foldl(
    fun({pushlbl,_Label}=P, {PC, List, LAcc, _}) ->
        {PC+1+JLen, List++[P], LAcc, true};
       ({jumpdest,Label}, {PC, List, LAcc, Found}) ->
        R=eevm_enc:encode(jumpdest),
        {PC+size(R), List++[R], LAcc#{Label=>PC}, Found};
       (X, {PC, List, LAcc, Found}) ->
        R=eevm_enc:encode(X),
        {PC+size(R),List++[R], LAcc, Found}
    end, {0, [], #{}, false}, Src),
  case IsLblFound of
    false ->
      list_to_binary(Code);
    true ->
      list_to_binary(
        lists:map(
        fun({pushlbl, Label}) ->
            eevm_enc:encode({push,JLen,maps:get(Label, Labels)});
           (Bin) when is_binary(Bin) ->
            Bin
        end, Code))
  end.

parseval("0x"++V1, _Data) ->
  list_to_integer(V1,16);
parseval(Value, Data) ->
  try
    list_to_integer(Value)
  catch error:badarg ->
          case maps:is_key(Value, Data) of
            true ->
              maps:get(Value, Data);
            false ->
              Value
          end
  end.


parse_line(["push",Value], Data) ->
  Val=parseval(Value, Data),
  if is_integer(Val) ->
       Size=max(1,ceil(ceil(math:log(max(Val,2))/math:log(2)) / 8)),
       {push,Size,Val};
     is_list(Val) ->
       {pushlbl, Val}
  end;


parse_line(["push"++Len,Value], Data) ->
  Val=parseval(Value, Data),
  if is_integer(Val) ->
       {push,list_to_integer(Len),Val};
     is_list(Val) ->
       {pushlbl, Val}
  end;

parse_line(["log"++N], _Data) ->
  {log,list_to_integer(N)};

parse_line(["dup"++N], _Data) ->
  {dup,list_to_integer(N)};

parse_line(["swap"++N], _Data) ->
  {swap,list_to_integer(N)};

parse_line(["jumpdest",Label], _Data) ->
  {jumpdest, Label};

parse_line([Operator], _Data) ->
  list_to_atom(Operator).

parse_asm(Code) when is_binary(Code) ->
  parse_asm(Code, #{}).

parse_asm(Code, Data) when is_binary(Code) ->
  process_labels(
    lists:flatten(
    lists:filtermap(
      fun(Line) ->
          L1=iolist_to_binary(re:replace(Line,"\/\/.*",<<>>)),
          L2=iolist_to_binary(re:replace(L1,"(^\s+|\s+$)",<<>>,[global])),
          L3=iolist_to_binary(re:replace(L2,"\s+",<<" ">>,[global])),
          if(L3==<<>>) ->
              false;
            true ->
              LineList=binary:split(L3, <<";">>, [global]),
              {true,
               lists:map(
                 fun(LL0) ->
                     LL=iolist_to_binary(re:replace(LL0,"(^\s+|\s+$)",<<>>,[global])),
                     List = string:split(
                       string:lowercase(
                         binary_to_list(LL)
                        )," ",all),
                     parse_line(List, Data)
                 end, LineList)
              }
          end
      end,
      binary:split(Code,<<"\n">>,[global])
     ))).

process_labels(List) ->
  Labels=lists:foldl(
    fun({jumpdest,Label}, Acc) ->
        Acc#{Label => 1};
       (_, Acc) ->
        Acc
    end, #{}, List),
  lists:foreach(
    fun({pushlbl, Label}) ->
        case maps:is_key(Label, Labels) of
          true -> ok;
          false -> throw({bad_label, Label})
        end;
       (_) ->
        ok
    end, List),
  List.

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
      io_lib:format("push32 0x~s~npush1 ~w~nmstore~n",[tohex(ThisCode),Off])
      | code2mem(Rest,Off+32)];
    true ->
      [io_lib:format("push~w 0x~s~npush1 ~w~nshl~npush1 ~w~nmstore~n",
                     [size(Code),tohex(Code),8*(32-size(Code)),Off])]
  end].

tohex(Bin) ->
  io_lib:format("~.16B",[binary:decode_unsigned(Bin)]).
