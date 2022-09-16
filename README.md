# EEVM

**EEVM** is an EVM implementation for Erlang Virtual Machine. It is developed by [ThePower](https://github.com/thepower) project for embedding into ThePower blockchain, but it is perfectly isolated from ThePower node code and can be easily embedded in any Erlang application.

EEVM runs Solidity code compiled by `solc` without any modifications.

This implementation only requires KECCAK-256 library as dependence. It uses [KSHA3](github.com/onyxrev/ksha3) by default, but there is also slow, but pure-erlang [ESHA3](github.com/cleverfox/esha3) implemantation used by various utilities. This implementation needs to be escriptized (packed into one `escript`-runnable file).

## Interpreter

The Erlang VM interpreter is used.

## Prerequisites

### Hardware

| CPU cores | Memory       | Hard disk                     | Network             |
|-----------|--------------|-------------------------------|---------------------|
| 2         | 2 GB or more | Minimum: 20 GB, SSD preferred | Minimum: 100 Mbit/s |

### Software

| OS             | Erlang version | Eshell version | Docker version                         |
|----------------|----------------|----------------|----------------------------------------|
| Ubuntu v.20.04 | 22 or upper    | 10.4           | latest (20.10.18 as of September 2022) |

## Usage

### How to build EEVM?

To build EEVM, run:

```
./rebar3 compile
```

### How to run EEVM?

Use the following command, to run EEVM:

```
./rebar3 shell
```

After you run the command, you'll get into the Erlang shell. This shell allows you to run tests from `eevm_scratchpad.erl`. You can look through the code and play with it. In the example below, you'll play with `eevm_scratchpad:tether()`.

After running the test, you will get something like this:

```bash
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

where

| Field  | Description                                                                        |
|--------|------------------------------------------------------------------------------------|
| `St1`  | A storage, where the smart contract is stored                                      |
| `Bals` | Returns the value of `balanceOf` call for three addresses used in the example test |
| `LOG`  | Contains data sent to EVM interpreter by the smart-contract                        |

Also, you can see the final state after 3 transactions are finished.