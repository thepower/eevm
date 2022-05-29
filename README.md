# eevm is Etherium's EVM inside Erlang VM

It still in alpha status, and it's definitely not production ready yet.

# How to build

```
./rebar3 compile
```

# How to run

```
./rebar3 shell
```

You get in Erlang shell. In shell you can run tests from eevm_scratchpad.erl, look at the code in eevm_scratchpad and play with it. You can start playing with `eevm_scratchpad:tether().`, it's USDT ERC20 contract stolen from ETH.

After doing this you will get something like this

```
> eevm_scratchpad:tether().
St1 #{0 => 49374,1 => 131072,3 => 0,4 => 0,7 => 0,8 => 0,9 => 3,10 => 0,
      67310910001236471336487740600394280602614355732742098307022882807426235821668 =>
          131072}
Bals 131072/0/0
LOG: <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100>>
        args [63486140976153616755203102783360879283472101686154884697241723088393386309925,
              49374,256]
Res stop
Bals 131072/0/0
LOG: <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,94>>
        args [100389287136786176327247604509743168900146139575972864366142685224231313322991,
              49374,512]
Res stop
Bals 130978/0/94
LOG: <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5>>
        args [100389287136786176327247604509743168900146139575972864366142685224231313322991,
              49374,256]
Res stop
Bals 130973/5/94
#{0 => 49374,1 => 131072,3 => 0,4 => 0,7 => 0,8 => 0,9 => 3,
  10 => 0,
  47308557215275847810576800605663265607653752947282041372765295433751199017549 =>
      5,
  67310910001236471336487740600394280602614355732742098307022882807426235821668 =>
      130973,
  75831452412138323880916444173573612722054346159677297449878335876480453112616 =>
      94,
  109967025685673354741881731518503272060083192313621507584907820637209627593305 =>
      1}
```

Here you can see St1 - it's contract's storage after deployment, Bals lines is returning value of balanceOf call for 3 address used in test. LOG lines is emited by smartcontract data. Also you can see final state after 3 transcations finished.

If you like to do automatic code recompilation after editing, you can uncomment sync dep in rebar.config before building and start sync by issuing `sync:go().` in shell.

## TO FIX

- check RAM expansion gas calculation

