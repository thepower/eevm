-module(tracer).
-export([run/0]).

run() ->
  register(evm_tracer,self()),
  loop().


loop() ->
  receive
    {trace, {stack,Stack}} ->
      io:format("Stack ~p~n",[Stack]),
      loop();
    {trace, {opcode, {PC, {push,_,Val}}}} ->
      io:format("\t~.16B op {push,~p} (~.16B)~n",[PC,Val,Val]),
      loop();
    {trace, {opcode, {PC, Code}}} ->
      io:format("\t~.16B op ~p~n",[PC,Code]),
      loop();
    {trace, {jumpi, {To, Cond}}} ->
      io:format("Jump to ~.16B cond ~p~n",[To, Cond]),
      loop();
    {trace, {jump_ok, To}} ->
      io:format("Jumped to ~.16B ok~n",[To]),
      loop();
    {trace, Other} ->
      io:format("Trace ~p~n",[Other]),
      loop();
    Other ->
      io:format("Other: ~p~n",[Other])
  end.



