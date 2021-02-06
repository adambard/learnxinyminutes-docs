---
name: perl
category: language
language: perl
filename: learnperl-cn.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
translators:
    - ["Yadong Wen", "https://github.com/yadongwen"]
lang: zh-cn
---

Perl 是一个功能强大、特性齐全的编程语言，有25年的历史。

Perl 可以在包括便携式设备和大型机的超过100个平台上运行，既适用于快速原型构建，也适用于大型项目开发。

```perl
# 单行注释以#号开头


#### Perl的变量类型

#  变量以$号开头。
#  合法变量名以英文字母或者下划线起始，
#  后接任意数目的字母、数字或下划线。

### Perl有三种主要的变量类型：标量、数组和哈希。

## 标量
#  标量类型代表单个值：
my $animal = "camel";
my $answer = 42;

# 标量类型值可以是字符串、整型或浮点类型，Perl会根据需要自动进行类型转换。

## 数组
#  数组类型代表一列值：
my @animals = ("camel", "llama", "owl");
my @numbers = (23, 42, 69);
my @mixed   = ("camel", 42, 1.23);



## 哈希
#  哈希类型代表一个键/值对的集合：

my %fruit_color = ("apple", "red", "banana", "yellow");

#  可以使用空格和“=>”操作符更清晰的定义哈希：

my %fruit_color = (
        apple  => "red",
        banana => "yellow",
        );
# perldata中有标量、数组和哈希更详细的介绍。 (perldoc perldata).

# 可以用引用构建更复杂的数据类型，比如嵌套的列表和哈希。

#### 逻辑和循环结构

# Perl有大多数常见的逻辑和循环控制结构

if ( $var ) {
    ...
} elsif ( $var eq 'bar' ) {
    ...
} else {
    ...
}

unless ( condition ) {
                   ...
               }
# 上面这个比"if (!condition)"更可读。

# 有Perl特色的后置逻辑结构
print "Yow!" if $zippy;
print "We have no bananas" unless $bananas;

#  while
  while ( condition ) {
                   ...
               }


# for和foreach
for ($i = 0; $i <= $max; $i++) {
                   ...
               }

foreach (@array) {
                   print "This element is $_\n";
               }


#### 正则表达式

# Perl对正则表达式有深入广泛的支持，perlrequick和perlretut等文档有详细介绍。简单来说：

# 简单匹配
if (/foo/)       { ... }  # 如果 $_ 包含"foo"逻辑为真
if ($a =~ /foo/) { ... }  # 如果 $a 包含"foo"逻辑为真

# 简单替换

$a =~ s/foo/bar/;         # 将$a中的foo替换为bar
$a =~ s/foo/bar/g;        # 将$a中所有的foo替换为bar


#### 文件和输入输出

# 可以使用“open()”函数打开文件用于输入输出。

open(my $in,  "<",  "input.txt")  or die "Can't open input.txt: $!";
open(my $out, ">",  "output.txt") or die "Can't open output.txt: $!";
open(my $log, ">>", "my.log")     or die "Can't open my.log: $!";

# 可以用"<>"操作符读取一个打开的文件句柄。 在标量语境下会读取一行，
# 在列表环境下会将整个文件读入并将每一行赋给列表的一个元素：

my $line  = <$in>;
my @lines = <$in>;

#### 子程序

# 写子程序很简单：

sub logger {
    my $logmessage = shift;
    open my $logfile, ">>", "my.log" or die "Could not open my.log: $!";
    print $logfile $logmessage;
}

# 现在可以像内置函数一样调用子程序：

logger("We have a logger subroutine!");


```

#### 使用Perl模块

Perl模块提供一系列特性来帮助你避免重新发明轮子，CPAN是下载模块的好地方( http://www.cpan.org/ )。Perl发行版本身也包含很多流行的模块。

perlfaq有很多常见问题和相应回答，也经常有对优秀CPAN模块的推荐介绍。

#### 深入阅读

    - [perl-tutorial](http://perl-tutorial.org/)
    - [www.perl.com的learn站点](http://www.perl.org/learn.html)
    - [perldoc](http://perldoc.perl.org/)
    - 以及 perl 内置的： `perldoc perlintro`
