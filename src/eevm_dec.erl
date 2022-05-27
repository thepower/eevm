-module(eevm_dec).
-export([decode/1,decode2list/1]).

decode2list(<<>>) ->
  [];

decode2list(Bin) ->
  case decode(Bin) of
    {Opcode,Rest,_} ->
      [Opcode|decode2list(Rest)];
    {Opcode,Rest} ->
      [Opcode|decode2list(Rest)]
  end.
    

decode(<<16#00, Rest/binary>>) -> {stop,Rest};
decode(<<16#01, Rest/binary>>) -> {add,Rest};
decode(<<16#02, Rest/binary>>) -> {mul,Rest};
decode(<<16#03, Rest/binary>>) -> {sub,Rest};
decode(<<16#04, Rest/binary>>) -> {'div',Rest};
decode(<<16#05, Rest/binary>>) -> {sdiv,Rest};
decode(<<16#06, Rest/binary>>) -> {'mod',Rest};
decode(<<16#07, Rest/binary>>) -> {smod,Rest};
decode(<<16#08, Rest/binary>>) -> {addmod,Rest};
decode(<<16#09, Rest/binary>>) -> {mulmod,Rest};
decode(<<16#0a, Rest/binary>>) -> {exp,Rest};
decode(<<16#0b, Rest/binary>>) -> {signextend,Rest};

decode(<<16#10, Rest/binary>>) -> {lt,Rest};
decode(<<16#11, Rest/binary>>) -> {gt,Rest};
decode(<<16#12, Rest/binary>>) -> {slt,Rest};
decode(<<16#13, Rest/binary>>) -> {sgt,Rest};
decode(<<16#14, Rest/binary>>) -> {eq,Rest};
decode(<<16#15, Rest/binary>>) -> {iszero,Rest};
decode(<<16#16, Rest/binary>>) -> {'and',Rest};
decode(<<16#17, Rest/binary>>) -> {'or',Rest};
decode(<<16#18, Rest/binary>>) -> {'xor',Rest};
decode(<<16#19, Rest/binary>>) -> {'not',Rest};
decode(<<16#1a, Rest/binary>>) -> {'byte',Rest};
decode(<<16#1b, Rest/binary>>) -> {'shl',Rest};
decode(<<16#1c, Rest/binary>>) -> {shr,Rest};
decode(<<16#1d, Rest/binary>>) -> {sar,Rest};

decode(<<16#20, Rest/binary>>) -> {sha3,Rest};

decode(<<16#30, Rest/binary>>) -> {address,Rest};
decode(<<16#31, Rest/binary>>) -> {balance,Rest};
decode(<<16#32, Rest/binary>>) -> {origin,Rest};
decode(<<16#33, Rest/binary>>) -> {caller,Rest};
decode(<<16#34, Rest/binary>>) -> {callvalue,Rest};
decode(<<16#35, Rest/binary>>) -> {calldataload,Rest};
decode(<<16#36, Rest/binary>>) -> {calldatasize,Rest};
decode(<<16#37, Rest/binary>>) -> {calldatacopy,Rest};
decode(<<16#38, Rest/binary>>) -> {codesize,Rest};
decode(<<16#39, Rest/binary>>) -> {codecopy,Rest};
decode(<<16#3a, Rest/binary>>) -> {gasprice,Rest};
decode(<<16#3b, Rest/binary>>) -> {extcodesize,Rest};
decode(<<16#3c, Rest/binary>>) -> {extcodecopy,Rest};
decode(<<16#3d, Rest/binary>>) -> {returndatasize,Rest};
decode(<<16#3e, Rest/binary>>) -> {returndatacopy,Rest};
decode(<<16#3f, Rest/binary>>) -> {extcodehash,Rest};

decode(<<16#40, Rest/binary>>) -> {blockhash,Rest};
decode(<<16#41, Rest/binary>>) -> {coinbase,Rest};
decode(<<16#42, Rest/binary>>) -> {timestamp,Rest};
decode(<<16#43, Rest/binary>>) -> {number,Rest};
decode(<<16#44, Rest/binary>>) -> {difficulty,Rest};
decode(<<16#45, Rest/binary>>) -> {gaslimit,Rest};
decode(<<16#46, Rest/binary>>) -> {chainid,Rest};
decode(<<16#47, Rest/binary>>) -> {selfbalance,Rest};
decode(<<16#48, Rest/binary>>) -> {basefee,Rest};

decode(<<16#50, Rest/binary>>) -> {pop,Rest};
decode(<<16#51, Rest/binary>>) -> {mload,Rest};
decode(<<16#52, Rest/binary>>) -> {mstore,Rest};
decode(<<16#53, Rest/binary>>) -> {mstore8,Rest};
decode(<<16#54, Rest/binary>>) -> {sload,Rest};
decode(<<16#55, Rest/binary>>) -> {sstore,Rest};
decode(<<16#56, Rest/binary>>) -> {jump,Rest};
decode(<<16#57, Rest/binary>>) -> {jumpi,Rest};
decode(<<16#58, Rest/binary>>) -> {pc,Rest};
decode(<<16#59, Rest/binary>>) -> {msize,Rest};
decode(<<16#5a, Rest/binary>>) -> {gas,Rest};
decode(<<16#5b, Rest/binary>>) -> {jumpdest,Rest};

decode(<<Push, Rest/binary>>) when Push>=16#60 andalso Push<16#80 ->
  Len=Push-16#5F,
  Len8=Len*8,
  if(size(Rest)>=Len) ->
      <<Arg:Len8/big,Rest1/binary>> = Rest,
      {{push,Len,Arg},Rest1,1+Len};
    true ->
      <<Arg:Len8/big>> = <<Rest/binary,0:((Len-size(Rest))*8)/big>>,
      {{push,Len,Arg},<<>>,1+Len}
  end;

decode(<<Code80, Rest/binary>>) when Code80>=16#80 andalso Code80<16#90 ->
  {{dup,Code80-16#7F},Rest};
decode(<<Code90, Rest/binary>>) when Code90>=16#90 andalso Code90<16#A0 ->
  {{swap,Code90-16#8F},Rest};

decode(<<16#a0, Rest/binary>>) -> {{log,0},Rest};
decode(<<16#a1, Rest/binary>>) -> {{log,1},Rest};
decode(<<16#a2, Rest/binary>>) -> {{log,2},Rest};
decode(<<16#a3, Rest/binary>>) -> {{log,3},Rest};
decode(<<16#a4, Rest/binary>>) -> {{log,4},Rest};

decode(<<16#f0, Rest/binary>>) -> {create,Rest};
decode(<<16#f1, Rest/binary>>) -> {call,Rest};
decode(<<16#f2, Rest/binary>>) -> {callcode,Rest};
decode(<<16#f3, Rest/binary>>) -> {return,Rest};
decode(<<16#f4, Rest/binary>>) -> {delegatecall,Rest};
decode(<<16#f5, Rest/binary>>) -> {create2,Rest};

decode(<<16#fa, Rest/binary>>) -> {staticcall,Rest};

decode(<<16#fd, Rest/binary>>) -> {revert,Rest};
decode(<<16#fe, Rest/binary>>) -> {invalid,Rest};
decode(<<16#ff, Rest/binary>>) -> {selfdestruct,Rest};

decode(<<Any:8/integer, Rest/binary>>) -> {Any,Rest}.

