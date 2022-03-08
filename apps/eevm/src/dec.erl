-module(dec).
-export([decode/1]).

decode(Any) ->
  decode(Any,0).

decode(<<>>,_) ->
  [];
decode(<<16#00, Rest/binary>>,Pos) -> [{Pos,stop} | decode(Rest,Pos+1) ];
decode(<<16#01, Rest/binary>>,Pos) -> [{Pos,add} | decode(Rest,Pos+1) ];
decode(<<16#02, Rest/binary>>,Pos) -> [{Pos,mul} | decode(Rest,Pos+1) ];
decode(<<16#03, Rest/binary>>,Pos) -> [{Pos,sub} | decode(Rest,Pos+1) ];
decode(<<16#04, Rest/binary>>,Pos) -> [{Pos,'div'} | decode(Rest,Pos+1) ];
decode(<<16#05, Rest/binary>>,Pos) -> [{Pos,sdiv} | decode(Rest,Pos+1) ];
decode(<<16#06, Rest/binary>>,Pos) -> [{Pos,'mod'} | decode(Rest,Pos+1) ];
decode(<<16#07, Rest/binary>>,Pos) -> [{Pos,smod} | decode(Rest,Pos+1) ];
decode(<<16#08, Rest/binary>>,Pos) -> [{Pos,addmod} | decode(Rest,Pos+1) ];
decode(<<16#09, Rest/binary>>,Pos) -> [{Pos,mulmod} | decode(Rest,Pos+1) ];
decode(<<16#0a, Rest/binary>>,Pos) -> [{Pos,exp} | decode(Rest,Pos+1) ];
decode(<<16#0b, Rest/binary>>,Pos) -> [{Pos,signextend} | decode(Rest,Pos+1) ];
decode(<<16#0c, Rest/binary>>,Pos) -> [{Pos,16#0c} | decode(Rest,Pos+1) ];

decode(<<16#10, Rest/binary>>,Pos) -> [{Pos,lt} | decode(Rest,Pos+1) ];
decode(<<16#11, Rest/binary>>,Pos) -> [{Pos,gt} | decode(Rest,Pos+1) ];
decode(<<16#12, Rest/binary>>,Pos) -> [{Pos,slt} | decode(Rest,Pos+1) ];
decode(<<16#13, Rest/binary>>,Pos) -> [{Pos,sgt} | decode(Rest,Pos+1) ];
decode(<<16#14, Rest/binary>>,Pos) -> [{Pos,eq} | decode(Rest,Pos+1) ];
decode(<<16#15, Rest/binary>>,Pos) -> [{Pos,iszero} | decode(Rest,Pos+1) ];
decode(<<16#16, Rest/binary>>,Pos) -> [{Pos,'and'} | decode(Rest,Pos+1) ];
decode(<<16#17, Rest/binary>>,Pos) -> [{Pos,'or'} | decode(Rest,Pos+1) ];
decode(<<16#18, Rest/binary>>,Pos) -> [{Pos,'xor'} | decode(Rest,Pos+1) ];
decode(<<16#19, Rest/binary>>,Pos) -> [{Pos,'not'} | decode(Rest,Pos+1) ];
decode(<<16#1a, Rest/binary>>,Pos) -> [{Pos,'byte'} | decode(Rest,Pos+1) ];
decode(<<16#1b, Rest/binary>>,Pos) -> [{Pos,'shl'} | decode(Rest,Pos+1) ];
decode(<<16#1c, Rest/binary>>,Pos) -> [{Pos,shr} | decode(Rest,Pos+1) ];
decode(<<16#1d, Rest/binary>>,Pos) -> [{Pos,sar} | decode(Rest,Pos+1) ];

decode(<<16#20, Rest/binary>>,Pos) -> [{Pos,sha3} | decode(Rest,Pos+1) ];
decode(<<16#22, Rest/binary>>,Pos) -> [{Pos,16#22} | decode(Rest,Pos+1) ];
decode(<<16#32, Rest/binary>>,Pos) -> [{Pos,origin}| decode(Rest,Pos+1) ];
decode(<<16#33, Rest/binary>>,Pos) -> [{Pos,caller}| decode(Rest,Pos+1) ];
decode(<<16#34, Rest/binary>>,Pos) -> [{Pos,callvalue} | decode(Rest,Pos+1) ];
decode(<<16#35, Rest/binary>>,Pos) -> [{Pos,calldataload} | decode(Rest,Pos+1) ];
decode(<<16#36, Rest/binary>>,Pos) -> [{Pos,calldatasize} | decode(Rest,Pos+1) ];
decode(<<16#37, Rest/binary>>,Pos) -> [{Pos,calldatacopy} | decode(Rest,Pos+1) ];
decode(<<16#38, Rest/binary>>,Pos) -> [{Pos,codesize} | decode(Rest,Pos+1) ];
decode(<<16#39, Rest/binary>>,Pos) -> [{Pos,codecopy} | decode(Rest,Pos+1) ];

decode(<<16#40, Rest/binary>>,Pos) -> [{Pos,blockhash} | decode(Rest,Pos+1) ];
decode(<<16#41, Rest/binary>>,Pos) -> [{Pos,coinbase} | decode(Rest,Pos+1) ];
decode(<<16#42, Rest/binary>>,Pos) -> [{Pos,timestamp} | decode(Rest,Pos+1) ];
decode(<<16#43, Rest/binary>>,Pos) -> [{Pos,number} | decode(Rest,Pos+1) ];
decode(<<16#44, Rest/binary>>,Pos) -> [{Pos,difficulty} | decode(Rest,Pos+1) ];
decode(<<16#45, Rest/binary>>,Pos) -> [{Pos,gaslimit} | decode(Rest,Pos+1) ];
decode(<<16#46, Rest/binary>>,Pos) -> [{Pos,chainid} | decode(Rest,Pos+1) ];
decode(<<16#47, Rest/binary>>,Pos) -> [{Pos,selfbalance} | decode(Rest,Pos+1) ];
decode(<<16#48, Rest/binary>>,Pos) -> [{Pos,basefee} | decode(Rest,Pos+1) ];

decode(<<16#50, Rest/binary>>,Pos) -> [{Pos,pop} | decode(Rest,Pos+1) ];
decode(<<16#51, Rest/binary>>,Pos) -> [{Pos,mload} | decode(Rest,Pos+1) ];
decode(<<16#52, Rest/binary>>,Pos) -> [{Pos,mstore} | decode(Rest,Pos+1) ];
decode(<<16#54, Rest/binary>>,Pos) -> [{Pos,sload} | decode(Rest,Pos+1) ];
decode(<<16#55, Rest/binary>>,Pos) -> [{Pos,sstore} | decode(Rest,Pos+1) ];
decode(<<16#56, Rest/binary>>,Pos) -> [{Pos,jump} | decode(Rest,Pos+1) ];
decode(<<16#57, Rest/binary>>,Pos) -> [{Pos,jumpi} | decode(Rest,Pos+1) ];
decode(<<16#58, Rest/binary>>,Pos) -> [{Pos,pc} | decode(Rest,Pos+1) ];
decode(<<16#59, Rest/binary>>,Pos) -> [{Pos,msize} | decode(Rest,Pos+1) ];
decode(<<16#5a, Rest/binary>>,Pos) -> [{Pos,gas} | decode(Rest,Pos+1) ];
decode(<<16#5b, Rest/binary>>,Pos) -> [{Pos,jumpdest} | decode(Rest,Pos+1) ];

%decode(<<16#60, Arg1:8/big,  Rest/binary>>,Pos) -> [{push,1, Arg1} | decode(Rest,Pos+2) ];
%decode(<<16#61, Arg1:16/big, Rest/binary>>,Pos) -> [{push,2, Arg1} | decode(Rest,Pos+3) ];
%decode(<<16#62, Arg1:24/big, Rest/binary>>,Pos) -> [{push,3, Arg1} | decode(Rest,Pos+4) ];
%decode(<<16#63, Arg1:32/big, Rest/binary>>,Pos) -> [{push,4, Arg1} | decode(Rest,Pos+5) ];
%decode(<<16#64, Arg1:40/big, Rest/binary>>,Pos) -> [{push,5, Arg1} | decode(Rest,Pos+6) ];

decode(<<Push, Rest/binary>>,Pos) when Push>=16#60 andalso Push<16#80 ->
  Len=Push-16#5F,
  if(size(Rest)>=Len) ->
      <<Arg:(Len*8)/big,Rest1/binary>> = Rest,
      [{Pos,{push,Len,Arg}} | decode(Rest1,Pos+1+Len) ];
    true ->
      <<Arg:(Len*8)/big>> = <<Rest/binary,0:((Len-size(Rest))*8)/big>>,
      [{Pos,{push,Len,Arg}}]
  end;

%decode(<<16#73, Arg1:160/big, Rest/binary>>,Pos) -> [{push,20, Arg1} | decode(Rest,Pos+21) ];
%decode(<<16#79, Arg1:208/big, Rest/binary>>,Pos) -> [{push,26, Arg1} | decode(Rest,Pos+27) ];
%decode(<<16#7f, Arg1:256/big, Rest/binary>>,Pos) -> [{push,32, Arg1} | decode(Rest,Pos+33) ];

decode(<<Code80, Rest/binary>>,Pos) when Code80>=16#80 andalso Code80<16#90 ->
  [{Pos,{dup,Code80-16#7F}} | decode(Rest,Pos+1) ];
decode(<<Code90, Rest/binary>>,Pos) when Code90>=16#90 andalso Code90<16#A0 ->
  [{Pos,{swap,Code90-16#8F}} | decode(Rest,Pos+1) ];

decode(<<16#a0, Rest/binary>>,Pos) -> [{Pos,{log,0}} | decode(Rest,Pos+1) ];
decode(<<16#a1, Rest/binary>>,Pos) -> [{Pos,{log,1}} | decode(Rest,Pos+1) ];
decode(<<16#a2, Rest/binary>>,Pos) -> [{Pos,{log,2}} | decode(Rest,Pos+1) ];
decode(<<16#a3, Rest/binary>>,Pos) -> [{Pos,{log,3}} | decode(Rest,Pos+1) ];
decode(<<16#a4, Rest/binary>>,Pos) -> [{Pos,{log,4}} | decode(Rest,Pos+1) ];


decode(<<16#f3, Rest/binary>>,Pos) -> [{Pos,return} | decode(Rest,Pos+1) ];
decode(<<16#fd, Rest/binary>>,Pos) -> [{Pos,revert} | decode(Rest,Pos+1) ];
decode(<<16#fe, Rest/binary>>,Pos) -> [{Pos,invalid} | decode(Rest,Pos+1) ];

decode(<<Any:8/integer, Rest/binary>>,Pos) -> [{Pos,Any} | decode(Rest,Pos+1) ].
%decode(<<Opcode:8/integer,_Rest/binary>>,Pos) -> %[Pos,Opcode,_Rest].
%  throw({'bad_opcode',Opcode,Pos}).



