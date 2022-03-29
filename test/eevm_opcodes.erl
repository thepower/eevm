-module(eevm_opcodes).

-include_lib("eunit/include/eunit.hrl").

ops_test() ->
  FFBin=binary:encode_unsigned(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),

  [
   ?assertMatch(
      #{stack:=[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFe]},
      eevm_interpret:interp('smod',#{stack=>[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFf8,16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[4]},
      eevm_interpret:interp('addmod',#{stack=>[10,10,8],gas=>100})
     ),
   ?assertMatch(
      #{stack:=[1]},
      eevm_interpret:interp('addmod',#{stack=>[
                                               16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
                                               2,
                                               2],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[4]},
      eevm_interpret:interp('mulmod',#{stack=>[10,10,8],gas=>100})
     ),
   ?assertMatch(
      #{stack:=[9]},
      eevm_interpret:interp('mulmod',#{stack=>
                                       [
                                        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 
                                        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,12],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[100]},
      eevm_interpret:interp('exp',#{stack=>[10,2],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]},
      eevm_interpret:interp(signextend,#{stack=>[0,16#ff],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#7f]},
      eevm_interpret:interp(signextend,#{stack=>[0,16#7f],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[1]},
      eevm_interpret:interp(slt,#{stack=>[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,0],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[0]},
      eevm_interpret:interp(slt,#{stack=>[10,10],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[1]},
      eevm_interpret:interp(sgt,#{stack=>[0,16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[0]},
      eevm_interpret:interp(sgt,#{stack=>[10,10],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]},
      eevm_interpret:interp('not',#{stack=>[0],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#FF]},
      eevm_interpret:interp('byte',#{stack=>[31,255],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#FF]},
      eevm_interpret:interp('byte',#{stack=>[30,16#ff00],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[2]},
      eevm_interpret:interp('shl',#{stack=>[1,1],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#F000000000000000000000000000000000000000000000000000000000000000]},
      eevm_interpret:interp('shl',#{stack=>[4,16#FF00000000000000000000000000000000000000000000000000000000000000],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[1]},
      eevm_interpret:interp('shr',#{stack=>[1,2],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#0f]},
      eevm_interpret:interp('shr',#{stack=>[4,255],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[1]},
      eevm_interpret:interp('sar',#{stack=>[1,2],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]},
      eevm_interpret:interp('sar',#{stack=>[4,16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0],gas=>100})
     ),

   ?assertMatch(
      #{stack:=[16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]},
      eevm_interpret:interp('calldataload',#{stack=>[0],gas=>100,cd=>FFBin})
     ),

   ?assertMatch(
      #{stack:=[16#FF00000000000000000000000000000000000000000000000000000000000000]},
      eevm_interpret:interp('calldataload',#{stack=>[31],gas=>100,cd=>binary:encode_unsigned(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)})
     ),

   ?assertMatch(
      #{stack:=[],memory:=FFBin},
      eevm_interpret:interp('calldatacopy',#{stack=>[0,0,32],gas=>100,cd=>FFBin,memory=><<>>})
     ),

   ?assertMatch(
      #{stack:=[],memory:= << 255,0,0,0,0,0,0,0>>},
      eevm_interpret:interp('calldatacopy',#{stack=>[0,31,8],gas=>100,cd=>FFBin,memory=><<>>})
     ),

   ?assertMatch(
      #{stack:=[16]},
      eevm_interpret:interp('msize',#{stack=>[],gas=>100,memory=><<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>})
     ),


   ?assert(true)
  ].


