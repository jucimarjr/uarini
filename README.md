ooErlang
========

https://sites.google.com/site/ooerlang1/

What is this?
-------------

ooErlang is an object oriented extension to the Erlang programming language.
Objects are introduced with a syntax close to Java, making easier its adoption
by object-oriented programmers.

How to Install?
---------------
1. get the source
2. build
```shell
$ cd $OOERLANG_SOURCE_DIR
$ make
```
3. configure $PATH
```shell
# in .bashrc add:
export OOE_HOME=<PATH_TO_OOERLANG_DIR>
export PATH=$PATH:$OOE_HOME
```

### Requirements
* Erlang and Make

Getting Start
-------------
```erlang
% simple_calculator.cerl
-class(simple_calculator).
-constructor([new/1]).
-export([add/1, sub/1, mult/1, divi/1, clear/0, result/0]).
-export([usage_example/0]).

attributes.
Result = 0.

methods.
new(Value) -> self::Result = Value.

add(Value) -> self::Result = self::Result + Value.
sub(Value) -> self::Result = self::Result - Value.
mult(Value) -> self::Result = self::Result * Value.
divi(Value) -> self::Result = self::Result / Value.

clear() -> self::Result = 0.
result() -> self::Result.

class_methods.
% example of ((2 + 3 - 1) * 3) / 4 = 3
usage_example() ->
    Calculator = simple_calculator::new(2),
    Calculator::add(3),
    Calculator::sub(1),
    Calculator::mult(3),
    Calculator::divi(4),
    Calculator::result().
```

### Build using Erlang Shell
```shell
$ erl -pa $(OOE_HOME)/ebin
Eshell v6.0 (abort with ^G)
1> ooec:compile(["simple_calculator.cerl"]).

[{ok, simple_calculator}]
2> simple_calculator:usage_example().
3.0
```
