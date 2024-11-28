-module(eevm_ram).
-export([read/3,write/3]).

read(RAM,Offset,Len) when is_binary(RAM),
                          is_integer(Offset),
                          is_integer(Len),
                          Len<8*1048576 %8mb is ready mad amount for evm
                          ->
  if size(RAM)>=Offset+Len ->
       <<_:Offset/binary,Match:Len/binary,_/binary>> = RAM,
       Match;
     size(RAM)>Offset ->
       <<_:Offset/binary,Match/binary>> = RAM,
       Pad= <<0:((Len-size(Match))*8)/big>>,
       <<Match/binary,Pad/binary>>;
     true ->
       try
         <<0:(Len*8)/big>>
         catch Ec:Ee:S ->
        logger:error("ram error read(..,~w,~w)",[Offset,Len]),
        erlang:raise(Ec,Ee,S)
       end
  end;
read(_,Offset,Len) ->
  logger:error("ram error read(..,~w,~w)",[Offset,Len]),
  throw(badarg).

write(RAM,Offset,Data) when is_binary(RAM), is_integer(Offset), is_binary(Data) ->
  Len=size(Data),
  if size(RAM)>=Offset+Len ->
       <<Pre:Offset/binary,_:Len/binary,Post/binary>> = RAM,
       <<Pre/binary,Data/binary,Post/binary>>;
     size(RAM)>Offset ->
       <<Pre:Offset/binary,_/binary>> = RAM,
       <<Pre/binary,Data/binary>>;
     true ->
       <<RAM/binary,0:((Offset-size(RAM))*8)/big,Data/binary>>
  end.


