---
category: Algorithms & Data Structures
name: Lambda Calculus
lang: zh-cn
contributors:
    - ["Max Sun", "http://github.com/maxsun"]
    - ["Yan Hui Hang", "http://github.com/yanhh0"]
translators:
    - ["Maoyin Sun", "https://github.com/simonmysun"]
---

# Lambda 演算

Lambda 演算(lambda calculus, λ-calculus),
最初由[阿隆佐·邱奇][]([Alonzo Church][])提出,
是世界上最小的编程语言.
尽管没有数字, 字符串, 布尔或者任何非函数的数据类型,
lambda 演算仍可以表示任何图灵机.

[阿隆佐·邱奇]: https://zh.wikipedia.org/wiki/%E9%98%BF%E9%9A%86%E4%BD%90%C2%B7%E9%82%B1%E5%A5%87
[Alonzo Church]: https://en.wikipedia.org/wiki/Alonzo_Church

Lambda 演算由三种元素组成: **变量**(variables)、**函数**(functions)和**应用**(applications)。

| 名称 | 语法                 | 示例      | 解释                                             |
|------|----------------------|-----------|--------------------------------------------------|
| 变量 | `<变量名>`           | `x`       | 一个名为"x"的变量                                |
| 函数 | `λ<参数>.<函数体>`   | `λx.x`    | 一个以"x"(前者)为参数、以"x"(后者)为函数体的函数 |
| 应用 | `<函数><变量或函数>` | `(λx.x)a` | 以"a"为参数调用函数"λx.x"                        |

最基本的函数为恒等函数: `λx.x`, 它等价于`f(x) = x`.
第一个"x"为函数的参数, 第二个为函数体.

## 自由变量和约束变量:

- 在函数`λx.x`中, "x"被称作约束变量因为它同时出现在函数体和函数参数中.
- 在`λx.y`中, "y"被称作自由变量因为它没有被预先声明.

## 求值:

求值操作是通过[β-归约][]([β-Reduction][])完成的,
它本质上是词法层面上的替换.

[β-归约]: https://zh.wikipedia.org/wiki/%CE%9B%E6%BC%94%E7%AE%97#'%22%60UNIQ--postMath-0000006F-QINU%60%22'-%E6%AD%B8%E7%B4%84
[β-Reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction

当对表达式`(λx.x)a`求值时, 我们将函数体中所有出现的"x"替换为"a".

- `(λx.x)a`计算结果为: `a`
- `(λx.y)a`计算结果为: `y`

你甚至可以创建高阶函数:

- `(λx.(λy.x))a`计算结果为: `λy.a`

尽管 lambda 演算传统上仅支持单个参数的函数,
但我们可以通过一种叫作[柯里化][]([Currying][])的技巧创建多个参数的函数.

[柯里化]: https://zh.wikipedia.org/wiki/%E6%9F%AF%E9%87%8C%E5%8C%96
[Currying]: https://en.wikipedia.org/wiki/Currying

- `(λx.λy.λz.xyz)`等价于`f(x, y, z) = ((x y) z)`

有时`λxy.<body>`与`λx.λy.<body>`可以互换使用.

----

认识到传统的 **lambda 演算没有数字, 字符或者任何非函数的数据类型**很重要.

## 布尔逻辑:

在 lambda 演算中没有"真"或"假". 甚至没有 1 或 0.

作为替换:

`T`表示为: `λx.λy.x`

`F`表示为: `λx.λy.y`

首先, 我们可以定义一个"if"函数`λbtf`, 它当`b`为真时返回`t`,
`b`为假时返回`f`

`IF`等价于: `λb.λt.λf.b t f`

通过`IF`, 我们可以定义基本的布尔逻辑运算符:

`a AND b`等价于: `λab.IF a b F`

`a OR b`等价于: `λab.IF a T b`

`NOT a`等价于: `λa.IF a F T`

*注意: `IF a b c`本质上指: `IF((a b) c)`*

## 数字：

尽管 lambda 演算中没有数字,
我们还可以用[邱奇编码][]([Church numerals][])将数字嵌入到 lambda 演算中.

[邱奇编码]: https://zh.wikipedia.org/wiki/%E9%82%B1%E5%A5%87%E7%BC%96%E7%A0%81
[Church numerals]: https://en.wikipedia.org/wiki/Church_encoding

对于任意数字 n: <code>n = λf.f<sup>n</sup></code> 所以:

`0 = λf.λx.x`

`1 = λf.λx.f x`

`2 = λf.λx.f(f x)`

`3 = λf.λx.f(f(f x))`

要增加一个邱奇数, 我们使用后继函数`S(n) = n + 1`:

`S = λn.λf.λx.f((n f) x)`

使用后继函数, 我们可以定义加法:

`ADD = λab.(a S)b`

**挑战**: 试定义乘法函数!

## 变得更小: SKI, SK 和 Iota

### SKI 组合子演算

令 S, K, I 为下列函数:

`I x = x`

`K x y =  x`

`S x y z = x z (y z)`

我们可以将 lambda 演算中的表达式转换为 SKI 组合子演算中的表达式:

1. `λx.x = I`
2. `λx.c = Kc`
3. `λx.(y z) = S (λx.y) (λx.z)`

以邱奇数 2 为例:

`2 = λf.λx.f(f x)`

对于里面的部分 `λx.f(f x)`:

```
  λx.f(f x)
= S (λx.f) (λx.(f x))          (case 3)
= S (K f)  (S (λx.f) (λx.x))   (case 2, 3)
= S (K f)  (S (K f) I)         (case 2, 1)
```

所以:

```
  2
= λf.λx.f(f x)
= λf.(S (K f) (S (K f) I))
= λf.((S (K f)) (S (K f) I))
= S (λf.(S (K f))) (λf.(S (K f) I)) (case 3)
```

对于第一个参数`λf.(S (K f))`有:

```
  λf.(S (K f))
= S (λf.S) (λf.(K f))       (case 3)
= S (K S) (S (λf.K) (λf.f)) (case 2, 3)
= S (K S) (S (K K) I)       (case 2, 3)
```

对于第二个参数`λf.(S (K f) I)`有：

```
  λf.(S (K f) I)
= λf.((S (K f)) I)
= S (λf.(S (K f))) (λf.I)             (case 3)
= S (S (λf.S) (λf.(K f))) (K I)       (case 2, 3)
= S (S (K S) (S (λf.K) (λf.f))) (K I) (case 1, 3)
= S (S (K S) (S (K K) I)) (K I)       (case 1, 2)
```

综上:

```
  2
= S (λf.(S (K f))) (λf.(S (K f) I))
= S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))
```

如果展开这个表达式, 我们最终又会得到邱奇数 2 的相同的表达式.

### SK 组合子演算

SKI 组合子演算还可以进一步简化. 我们可以通过`I = SKK`移除 I 组合子.
我们可以将所有的 `I` 替换为 `SKK`.

### ι 组合子

SK 组合子仍不是最简的. 定义:

```
ι = λf.((f S) K)
```

我们有:

```
I = ιι
K = ι(ιI) = ι(ι(ιι))
S = ι(K) = ι(ι(ι(ιι)))
```

## 更多阅读:

1. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)(英文)
2. [Cornell CS 312 Recitation 26: The Lambda Calculus](https://courses.cs.cornell.edu/cs312/2008sp/recitations/rec26.html)(英文)
3. [Wikipedia - Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)(英文)
4. [Wikipedia - SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)(英文)
5. [Wikipedia - Iota and Jot](https://en.wikipedia.org/wiki/Iota_and_Jot)(英文)
6. [λ演算 - 维基百科，自由的百科全书](https://zh.wikipedia.org/wiki/SKI%E7%BB%84%E5%90%88%E5%AD%90%E6%BC%94%E7%AE%97)
7. [SKI组合子演算 - 维基百科，自由的百科全书](https://zh.wikipedia.org/wiki/SKI%E7%BB%84%E5%90%88%E5%AD%90%E6%BC%94%E7%AE%97)
