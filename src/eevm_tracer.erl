-module(eevm_tracer).
-export([run/0, grepn/1, grep/1, stop/0]).

run() ->
  register(eevm_tracer,self()).

stop() ->
  unregister(eevm_tracer).

%loop() ->
%  receive
%    {trace, {stack,Stack}} ->
%      io:format("Stack ~p~n",[Stack]),
%      loop();
%    {trace, {opcode, {PC, {push,_,Val}}}} ->
%      io:format("\t@~.16B op {push,~p} (~.16B)~n",[PC,Val,Val]),
%      loop();
%    {trace, {opcode, {PC, Code}}} ->
%      io:format("\t@~.16B op ~p~n",[PC,Code]),
%      loop();
%    {trace, {jumpi, {To, Cond}}} ->
%      io:format("Jump to ~.16B cond ~p~n",[To, Cond]),
%      loop();
%    {trace, {jump_ok, To}} ->
%      io:format("Jumped to ~.16B ok~n",[To]),
%      loop();
%    {trace, Other} ->
%      io:format("Trace ~p~n",[Other]),
%      ?MODULE:loop();
%    Other ->
%      io:format("Other: ~p~n",[Other])
%  after 10000 -> timeout
%  end.



grep(Opcodes) ->
  receive {trace, D} ->
            case grep1(Opcodes, D) of
              true ->
                [disp(D)|grep(Opcodes)];
              false ->
                grep(Opcodes)
            end
  after 0 ->
          []
  end.

grepn(Opcodes) ->
  receive {trace, D} ->
            case grep1(Opcodes, D) of
              false ->
                [disp(D)|grepn(Opcodes)];
              true ->
                grepn(Opcodes)
            end
  after 0 ->
          []
  end.

disp(D) -> D.

grep1(O,{stack, D, _Stack}) ->
  lists:member(stack,O) orelse lists:member({stack, D}, O);
grep1(O,{opcode, D, {_PC,_Inst}}) ->
  lists:member(opcode,O) orelse lists:member({stack, D}, O);
grep1(O,{Inst, _, _})when Inst==jump_ok orelse Inst==jump_error  ->
  lists:member(jmp,O);
grep1(O,{jumpi, _}) ->
  lists:member(jmp,O);
grep1(O,{Inst, _D, _, _, _}) ->
  lists:member(Inst,O);
grep1(O,{Inst, _D, _, _}) ->
  lists:member(Inst,O);
grep1(O,{Inst, _D, _}) ->
  lists:member(Inst,O);
grep1(O,{Inst, _}) ->
  lists:member(Inst,O).


