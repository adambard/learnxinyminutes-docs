---
language: erlang
lang: zh-cn
contributors:
    - ["Giovanni Cappellotto", "http://www.focustheweb.com/"]
translators:
    - ["Jakukyo Friel", "http://weakish.github.io"]
filename: erlang-cn.erl
---

```erlang
% 百分比符号标明注释的开始。

%% 两个符号通常用于注释函数。

%%% 三个符号通常用于注释模块。

% Erlang 里使用三种标点符号：
% 逗号 (`,`) 分隔函数调用中的参数、数据构建和模式。
% 句号 (`.`) （后跟空格）分隔函数和 shell 中的表达式。
% 分号 (`;`) 分隔语句。以下环境中使用语句：
% 函数定义和`case`、`if`、`try..catch`、`receive`表达式。

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. 变量和模式匹配
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Num = 42.  % 变量必须以大写字母开头。

% Erlang 的变量只能赋值一次。如果给变量赋不同的值，会导致错误：
Num = 43. % ** exception error: no match of right hand side value 43

% 大多数语言中`=`表示赋值语句，在Erlang中，则表示模式匹配。
% `Lhs = Rhs`实际上意味着：
% 演算右边(Rhs), 将结果与左边的模式匹配。
Num = 7 * 6.

% 浮点数
Pi = 3.14159.

% Atoms 用于表示非数字的常量。
% Atom 以小写字母开始，包含字母、数字、`_`和`@`。
Hello = hello.
OtherNode = example@node.

% Atom 中如果包含特殊字符，可以用单引号括起。
AtomWithSpace = 'some atom with space'.

% Erlang 的元组类似 C 的 struct.
Point = {point, 10, 45}.

% 使用模式匹配操作符`=`获取元组的值。
{point, X, Y} = Point.  % X = 10, Y = 45

% 我们可以使用`_`存放我们不感兴趣的变量。
% `_`被称为匿名变量。和其他变量不同，
% 同一个模式中的多个`_`变量不必绑定到相同的值。
Person = {person, {name, {first, joe}, {last, armstrong}}, {footsize, 42}}.
{_, {_, {_, Who}, _}, _} = Person.  % Who = joe

% 列表使用方括号，元素间使用逗号分隔。
% 列表的元素可以是任意类型。
% 列表的第一个元素称为列表的 head，其余元素称为列表的 tail。
ThingsToBuy = [{apples, 10}, {pears, 6}, {milk, 3}].

% 若`T`是一个列表，那么`[H|T]`同样是一个列表，head为`H`，tail为`T`.
% `|`分隔列表的 head 和 tail.
% `[]`是空列表。
% 我们可以使用模式匹配操作来抽取列表中的元素。
% 如果我们有一个非空的列表`L`，那么`[X|Y] = L`则
% 抽取 L 的 head 至 X，tail 至 Y （X、Y需为未定义的变量）。
[FirstThing|OtherThingsToBuy] = ThingsToBuy.
% FirstThing = {apples, 10}
% OtherThingsToBuy = {pears, 6}, {milk, 3}

% Erlang 中的字符串其实是由整数组成的数组。字符串使用双引号。
Name = "Hello".
[72, 101, 108, 108, 111] = "Hello".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. 循序编程
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Module 是 Erlang 代码的基本单位。我们编写的所有函数都储存在 module 中。
% Module 存储在后缀为 `.erl` 的文件中。
% Module 必须事先编译。编译好的 module 以 `.beam` 结尾。
-module(geometry).
-export([area/1]). % module 对外暴露的函数列表

% `area`函数包含两个分句，分句间以分号相隔。
% 最后一个分句以句号加换行结尾。
% 每个分句由头、体两部门组成。
% 头部包含函数名称和用括号括起的模式，
% 体部包含一系列表达式，如果头部的模式和调用时的参数匹配，这些表达式会被演算。
% 模式匹配依照定义时的顺序依次进行。
area({rectangle, Width, Ht}) -> Width * Ht;
area({circle, R})            -> 3.14159 * R * R.

% 编译文件为 geometry.erl.
c(geometry).  % {ok,geometry}

% 调用函数时必须使用 module 名和函数名。
geometry:area({rectangle, 10, 5}).  % 50
geometry:area({circle, 1.4}).  % 6.15752

% 在 Erlang 中，同一模块中，参数数目不同，名字相同的函数是完全不同的函数。
-module(lib_misc).
-export([sum/1]). % 对外暴露的`sum`函数接受一个参数：由整数组成的列表。
sum(L) -> sum(L, 0).
sum([], N)    -> N;
sum([H|T], N) -> sum(T, H+N).

% fun 是匿名函数。它们没有名字，不过可以赋值给变量。
Double = fun(X) -> 2*X end. % `Double` 指向匿名函数 #Fun<erl_eval.6.17052888>
Double(2).  % 4

% fun 可以作为函数的参数和返回值。
Mult = fun(Times) -> ( fun(X) -> X * Times end ) end.
Triple = Mult(3).
Triple(5).  % 15

% 列表解析是创建列表的表达式（不使用fun、map 或 filter）。
% `[F(X) || X <- L]` 表示 "由 `F(X)` 组成的列表，其中 `X` 取自列表 `L`。
L = [1,2,3,4,5].
[2*X || X <- L].  % [2,4,6,8,10]
% 列表解析可以使用生成器，也可以使用过滤器，过滤器用于筛选列表的一部分。
EvenNumbers = [N || N <- [1, 2, 3, 4], N rem 2 == 0]. % [2, 4]

% Guard 是用于增强模式匹配的结构。
% Guard 可用于简单的测试和比较。
% Guard 可用于函数定义的头部，以`when`关键字开头，或者其他可以使用表达式的地方。
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

% guard 可以由一系列 guard 表达式组成，这些表达式以逗号分隔。
% `GuardExpr1, GuardExpr2, ..., GuardExprN` 为真，当且仅当每个 guard 表达式均为真。
is_cat(A) when is_atom(A), A =:= cat -> true;
is_cat(A) -> false.
is_dog(A) when is_atom(A), A =:= dog -> true;
is_dog(A) -> false.

% guard 序列 `G1; G2; ...; Gn` 为真，当且仅当其中任意一个 guard 表达式为真。
is_pet(A) when is_dog(A); is_cat(A) -> true;
is_pet(A) -> false.

% Record 可以将元组中的元素绑定到特定的名称。
% Record 定义可以包含在 Erlang 源代码中，也可以放在后缀为`.hrl`的文件中（Erlang 源代码中 include 这些文件）。
-record(todo, {
  status = reminder,  % Default value
  who = joe,
  text
}).

% 在定义某个 record 之前，我们需要在 shell 中导入 record 的定义。
% 我们可以使用 shell 函数`rr` (read records 的简称）。
rr("records.hrl").  % [todo]

% 创建和更新 record。
X = #todo{}.
% #todo{status = reminder, who = joe, text = undefined}
X1 = #todo{status = urgent, text = "Fix errata in book"}.
% #todo{status = urgent, who = joe, text = "Fix errata in book"}
X2 = X1#todo{status = done}.
% #todo{status = done,who = joe,text = "Fix errata in book"}

% `case` 表达式。
% `filter` 返回由列表`L`中所有满足`P(x)`为真的元素`X`组成的列表。
filter(P, [H|T]) ->
  case P(H) of
    true -> [H|filter(P, T)];
    false -> filter(P, T)
  end;
filter(P, []) -> [].
filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4]). % [2, 4]

% `if` 表达式。
max(X, Y) ->
  if
    X > Y -> X;
    X < Y -> Y;
    true -> nil;
  end.

% 警告: `if` 表达式里至少有一个 guard 为真，否则会触发异常。


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. 异常
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 当遇到内部错误或显式调用时，会触发异常。
% 显式调用包括 `throw(Exception)`, `exit(Exception)` 和
% `erlang:error(Exception)`.
generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> erlang:error(a).

% Erlang 有两种捕获异常的方法。其一是将调用包裹在`try...catch`表达式中。
catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.

% 另一种方式是将调用包裹在`catch`表达式中。
% 此时异常会被转化为一个描述错误的元组。
catcher(N) -> catch generate_exception(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4. 并发
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Erlang 依赖于 actor并发模型。在 Erlang 编写并发程序的三要素：
% 创建进程，发送消息，接收消息

% 启动一个新的进程使用`spawn`函数，接收一个函数作为参数

F = fun() -> 2 + 2 end. % #Fun<erl_eval.20.67289768>
spawn(F). % <0.44.0>

% `spawn` 函数返回一个pid(进程标识符)，你可以使用pid向进程发送消息。
% 使用 `!` 操作符发送消息。
%  我们需要在进程内接收消息，要用到 `receive` 机制。

-module(caculateGeometry).
-compile(export_all).
caculateAera() ->
    receive
      {rectangle, W, H} ->
        W * H;
      {circle, R} ->
        3.14 * R * R;
      _ ->
        io:format("We can only caculate area of rectangles or circles.")
    end.

% 编译这个模块，在 shell 中创建一个进程，并执行 `caculateArea` 函数。
c(caculateGeometry).
CaculateAera = spawn(caculateGeometry, caculateAera, []).
CaculateAera ! {circle, 2}. % 12.56000000000000049738

% shell也是一个进程(process), 你可以使用`self`获取当前 pid

self(). % <0.41.0>

```

## References

* ["Learn You Some Erlang for great good!"](http://learnyousomeerlang.com/)
* ["Programming Erlang: Software for a Concurrent World" by Joe Armstrong](http://pragprog.com/book/jaerlang/programming-erlang)
* [Erlang/OTP Reference Documentation](http://www.erlang.org/doc/)
* [Erlang - Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)
