-module(eevm_ram).
-export([read/3,write/3]).

read(RAM,Offset,Len) when is_binary(RAM), is_integer(Offset), is_integer(Len) ->
  if size(RAM)>=Offset+Len ->
       <<_:Offset/binary,Match:Len/binary,_/binary>> = RAM,
       Match;
     size(RAM)>Offset ->
       <<_:Offset/binary,Match/binary>> = RAM,
       Pad= <<0:((Len-size(Match))*8)/big>>,
       <<Match/binary,Pad/binary>>;
     true ->
       <<0:(Len*8)/big>>
  end.

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


