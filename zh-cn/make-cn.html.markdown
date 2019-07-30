---
category: tool
tool: make
contributors:
- ["Robert Steed", "https://github.com/robochat"]
- ["Jichao Ouyang", "https://github.com/jcouyang"]
translators:
- ["Jichao Ouyang", "https://github.com/jcouyang"]
filename: Makefile-cn
lang: zh-cn
---

Makefile 用于定义如何创建目标文件, 比如如何从源码到可执行文件. 创建这一工具的目标是
减少不必要的编译或者任务.是传说中的 Stuart Feldman 在 1976 年花了一个周末写出来的, 
而今仍然使用广泛, 特别是在 Unix 和 Linux 系统上.

虽然每个语言可能都有相应的或多或少提供 make 的功能, 比如 ruby 的 rake, node 的 gulp, broccoli
, scala 的 sbt 等等. 但是 make 的简洁与高效, 和只做一件事并做到极致的风格, 使其至今仍是无可替代的,
甚至与其他构建工具一起使用也并无冲突.

尽管有许多的分支和变体, 这篇文章针对是标准的 GNU make.

```make
# 这行表示注释

# 文件名一定要交 Makefile, 大小写区分, 使用 `make <target>` 生成 target
# 如果想要取别的名字, 可以用 `make -f "filename" <target>`.

# 重要的事情 - 只认识 TAB, 空格是不认的, 但是在 GNU Make 3.82 之后, 可以通过
# 设置参数 .RECIPEPREFIX 进行修改

#-----------------------------------------------------------------------
# 初级
#-----------------------------------------------------------------------

# 创建一个 target 的规则非常简单
# targets : prerequisites
# 	recipe
# 	 …
# prerequisites(依赖) 是可选的, recipe(做法) 也可以多个或者不给.

# 下面这个任务没有给 prerequisites, 只会在目标文件 file0.txt 文件不存在时执行
file0.txt:
	echo "foo" > file0.txt
	# 试试 `make file0.txt`
	# 或者直接 `make`, 因为第一个任务是默认任务.
	# 注意: 即使是这些注释, 如果前面有 TAB, 也会发送给 shell, 注意看 `make file0.txt` 输出

# 如果提供 prerequisites, 则只有 prerequisites 比 target 新时会执行
# 比如下面这个任务只有当 file0.txt 比 file1.txt 新时才会执行.
file1.txt: file0.txt
	cat file0.txt > file1.txt
	# 这里跟shell里的命令式一模一样.
	@cat file0.txt >> file1.txt
	# @ 不会把命令打印到 stdout.
	-@echo 'hello'
	# - 意思是发生错误了也没关系.
	# 试试 `make file1.txt` 吧.

# targets 和 prerequisites 都可以是多个, 以空格分割
file2.txt file3.txt: file0.txt file1.txt
	touch file2.txt
	touch file3.txt

# 如果声明重复的 target, make 会给一个 warning, 后面会覆盖前面的
# 比如重复定义 file2.txt 会得到这样的 warning
# Makefile:46: warning: overriding commands for target `file2.txt'
# Makefile:40: warning: ignoring old commands for target `file2.txt'
file2.txt: file0.txt
	touch file2.txt

# 但是如果不定义任何 recipe, 就不会冲突, 只是多了依赖关系
file2.txt: file0.txt file3.txt

#-----------------------------------------------------------------------
# Phony(假的) Targets
#-----------------------------------------------------------------------

# phony targets 意思是 tagets 并不是文件, 可以想象成一个任务的名字而已.
# 因为不是文件, 无法比对是否有更新, 所以每次make都会执行.
all: maker process

# 依赖于 phony target 的 target 也会每次 make 都执行, 即使 target 是文件
ex0.txt ex1.txt: maker

# target 的声明顺序并不重要, 比如上面的 all 的依赖 maker 现在才声明
maker:
	touch ex0.txt ex1.txt

# 如果定义的 phony target 与文件名重名, 可以用 .PHONY 显示的指明哪些 targets 是 phony
.PHONY: all maker process
# This is a special target. There are several others.

# 常用的 phony target 有: all clean install ...

#-----------------------------------------------------------------------
# 变量与通配符
#-----------------------------------------------------------------------

process: file*.txt | dir/a.foo.b	# 可以用通配符匹配多个文件作为prerequisites
	@echo $^	# $^ 是 prerequisites
	@echo $@	# $@ 代表 target, 如果 target 为多个, $@ 代表当前执行的那个
	@echo $<	# $< prerequisite 中的第一个
	@echo $?	# $? 需要更新的 prerequisite 文件列表
	@echo $+	# $+ 所有依赖, 包括重复的
	@echo $|	# $| 竖线后面的 order-only prerequisites

a.%.b:
	@echo $*  # $* match 的target % 那部分, 包括路径, 比如 `make dir/a.foo.b` 会打出 `dir/foo`

# 即便分开定义依赖, $^ 依然能拿到
process: ex1.txt file0.txt
# 非常智能的, ex1.txt 会被找到, file0.txt 会被去重.

#-----------------------------------------------------------------------
# 模式匹配
#-----------------------------------------------------------------------

# 可以让 make 知道如何转换某些文件到别格式
# 比如 从 svg 到 png
%.png: %.svg
	inkscape --export-png $^

# 一旦有需要 foo.png 这个任务就会运行

# 路径会被忽略, 所以上面的 target 能匹配所有 png
# 但是如果加了路径, make 会找到最接近的匹配, 如果
# make small/foo.png (在这之前要先有 small/foo.svg 这个文件)
# 则会匹配下面这个规则
small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

%.png: %.svg
	@echo 重复定义会覆盖前面的, 现在 inkscape 没用了

# make 已经有一些内置的规则, 比如从 *.c 到 *.o

#-----------------------------------------------------------------------
# 变量
#-----------------------------------------------------------------------
# 其实是宏 macro

# 变量都是字符串类型, 下面这俩是一样一样的

name = Ted
name2="Sarah"

echo:
	@echo $(name)
	@echo ${name2}
	@echo $name    # 这个会被蠢蠢的解析成 $(n)ame.
	@echo \"$(name3)\" # 为声明的变量或扩展成空字符串.
	@echo $(name4)
	@echo $(name5)
# 你可以通过4种方式设置变量.
# 按以下顺序由高到低:
# 1: 命令行参数. 比如试试 `make echo name3=JICHAO`
# 2: Makefile 里面的
# 3: shell 中的环境变量
# 4: make 预设的一些变量

name4 ?= Jean
# 问号意思是如果 name4 被设置过了, 就不设置了.

override name5 = David
# 用 override 可以防止命令行参数设置的覆盖

name4 +=grey
# 用加号可以连接 (中间用空格分割).

# 在依赖的地方设置变量
echo: name2 = Sara2

# 还有一些内置的变量
echo_inbuilt:
	echo $(CC)
	echo ${CXX)}
	echo $(FC)
	echo ${CFLAGS)}
	echo $(CPPFLAGS)
	echo ${CXXFLAGS}
	echo $(LDFLAGS)
	echo ${LDLIBS}

#-----------------------------------------------------------------------
# 变量 2
#-----------------------------------------------------------------------

# 加个冒号可以声明 Simply expanded variables 即时扩展变量, 即只在声明时扩展一次
# 之前的等号声明时 recursively expanded 递归扩展

var := hello
var2 :=  $(var) hello

# 这些变量会在其引用的顺序求值
# 比如 var3 声明时找不到 var4, var3 会扩展成 `and good luck`
var3 := $(var4) and good luck
# 但是一般的变量会在调用时递归扩展, 先扩展 var5, 再扩展 var4, 所以是正常的
var5 = $(var4) and good luck
var4 := good night

echoSEV:
	@echo $(var)
	@echo $(var2)
	@echo $(var3)
	@echo $(var4)
	@echo $(var5)

#-----------------------------------------------------------------------
# 函数
#-----------------------------------------------------------------------

# make 自带了一些函数.
# wildcard 会将后面的通配符变成一串文件路径
all_markdown:
	@echo $(wildcard *.markdown)
# patsubst 可以做替换, 比如下面会把所有 markdown
# 后缀的文件重命名为 md 后缀
substitue: *
	@echo $(patsubst %.markdown,%.md,$* $^)

# 函数调用格式是 $(func arg0,arg1,arg2...)

# 试试
ls:	*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# Directives
#-----------------------------------------------------------------------

# 可以用 include 引入别的 Makefile 文件
# include foo.mk

sport = tennis
# 流程控制语句 (如if else 等等) 顶格写
report:
ifeq ($(sport),tennis)
	@echo 'game, set, match'
else
	@echo "They think it's all over; it is now"
endif

# 还有 ifneq, ifdef, ifndef

foo = true

# 不只是 recipe, 还可以写在外面哟
ifdef $(foo)
bar = 'bar'
endif

hellobar:
	@echo bar
```

### 资源

+ GNU Make 官方文档 [HTML](https://www.gnu.org/software/make/manual/) [PDF](https://www.gnu.org/software/make/manual/make.pdf)
+ [software carpentry tutorial](http://swcarpentry.github.io/make-novice/)
+ learn C the hard way [ex2](http://c.learncodethehardway.org/book/ex2.html) [ex28](http://c.learncodethehardway.org/book/ex28.html)
