---
category: tool
tool: vim
filename: LearnVim-cn.txt
contributors:
   - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
   - ["Jiang Haiyun", "https://github.com/haiiiiiyun"]
lang: zh-cn
---


[Vim](www.vim.org)
(Vi IMproved) 是 Unix 上的流行编辑器 vi 的克隆版本。这个文本编辑器
是为性能和提升效率而设计的，并且在大多数基于 unix 的系统上普遍存在。
它有大量的快捷键可用来快速导航到文件的特定位置，以便进行快速编辑。

## Vim 导航基础

```
    vim <filename>   # 在 Vim 中打开 <filename>
    :q               # 退出 Vim
    :w               # 保存当前文件
    :wq              # 保存文件并退出 Vim 
    :q!              # 退出 Vim 并且不保存文件
                     # ! *强制* 执行 :q， 因此没有保存就退出 Vim
    :x               # 保存文件并且退出 Vim， 是 :wq 的简写版本

    u                # 撤销
    CTRL+R           # 重做

    h                # 左移一个字符
    j                # 下移一行
    k                # 上移一行
    l                # 右移一个字符

    # 在行内移动

    0                # 移到行首
    $                # 移到行尾
    ^                # 移到行内的第一个非空白字符处

    # 在文本中查找

    /word            # 光标之后的所有该词都高亮显示
    ?word            # 光标之前的所有该词都高亮显示
    n                # 查找后将光标移到该词的下一个出现位置
    N                # 光标移到该词的上一个出现位置

    :%s/foo/bar/g    # 将文件每一行上的所有 'foo' 都改成 'bar'
    :s/foo/bar/g     # 将当前行上的所有 'foo' 都改成 'bar'

    # 跳到字符处

    f<字符>         # 向前跳移到 <字符> 上
    t<字符>         # 向前跳移到 <字符> 的左侧

    # 例如，    
    f<               # 向前跣到 < 上
    t<               # 向前跳移到 < 的左侧
    
    # 按词移动

    w                # 前移一个词
    b                # 后移一个词
    e                # 移到当前词的末尾

    # 移动的其它命令

    gg               # 移到文件顶部
    G                # 移到文件末尾
    :NUM             # 移到第 NUM 行 (NUM 是任意数字)
    H                # 移到屏幕顶部
    M                # 移到屏幕中间位置
    L                # 移到屏幕末尾
```

## 模式:

Vim 基于 **模式** 这个概念。

命令模式 - Vim 启动后就处于这个模式，用于导航和操作命令
插入模式 - 用于在你的文件中进行修改
可视模式 - 用于高亮文本并对它们进行操作
Ex 模式  - 用于跳到底部的 ':' 提示行上输入命令

```
    i                # 在光标位置前，将 Vim 切换到插入模式
    a                # 在光标位置后，将 Vim 切换到插入模式
    v                # 将 Vim 切换到可视模式
    :                # 将 Vim 切换到 ex 模式
    <esc>            # 无论你当前处于什么模式，都返回到命令模式

    # 复制和粘贴文本

    y                # 复制所选的内容
    yy               # 复制当前行
    d                # 删除所选的内容
    dd               # 删除当前行
    p                # 在当前光标位置后粘贴复制的文本
    P                # 在当前光标位置前粘贴复制的文本
    x                # 删除当前光标位置处的字符
```

## Vim 的 '语法'

Vim 可以被认为是按 '动词-修饰词-名词' 格式编排的一组命令：

动词     - 你的动作
修饰词   - 你如何执行你的动作
名词     - 你的动作所作用于的对象

关于 '动词'，'修饰词'，和 '名词' 的几个重要例子：

```
    # '动词'
    
    d                # 删除
    c                # 修改
    y                # 复制
    v                # 可视化选择

    # '修饰词'

    i                # 内部的
    a                # 周围的
    NUM              # 数字 (NUM 是任意数字)
    f                # 查找文本并位于其上
    t                # 查找文本并停于其前面
    /                # 从光标处开始查找字符串
    ?                # 在光标前查找字符串

    # '名词'

    w                # 词
    s                # 句子
    p                # 段落
    b                # 块
    
    # 示例 '语句' 或命令

    d2w              # 删除 2 个词
    cis              # 修改段落内的内容
    yip              # 复制段落内的内容 (复制你所在的段落)
    ct<              # 修改直到括号开启处
                     # 对你的当前位置直到下个括号开启处的内容进行修改
    d$               # 删除直到行尾
```

## 一些快捷键和技巧

        <!--TODO: Add more!-->
```
    >                # 将所选内容缩进一级
    <                # 将所选内容取消缩进一级
    :earlier 15m     # 将文档还原到 15 分钟前的状态
    :later 15m       # 逆转上述命令
    ddp              # 相邻行交换位置，先 dd 再 p
    .                # 重复之前动作
```

## 宏

宏基本上来说就是可录制的动作。
当你开始录制宏时，它会记录你使用的 **每个** 动作和命令，
直到你停止录制。当调用宏时，它会将这个完全相同的动作和命令序列
再次应用于所选文本之上。

```
    qa               # 开始录制一个叫 'a' 的宏
    q                # 停止录制
    @a               # 重播宏
```

### 配置 ~/.vimrc

.vimrc 可用于在启动时对 Vim 进行配置。

这里是一个示例 ~/.vimrc 文件：

```
" 示例 ~/.vimrc
" 2015.10 

" 需要 Vim iMproved 版本
set nocompatible

" 根据文件名检测文件类型，以便能进行智能自动缩进等操作。
filetype indent plugin on

" 开启语法高亮
syntax on

" 更好的命令行补全
set wildmenu

" 除了当使用大写字母时使用大小写无关查找
set ignorecase
set smartcase

" 当新开一行时，如果没有开启文件特定的缩进规则，
" 则缩进保持与你当前行一致
set autoindent

" 在左侧显示行号
set number

" 缩进选项，根据个人偏好进行修改

" 每个 TAB 的可视空格数
set tabstop=4

" 编辑时 TAB 对应的空格数
set softtabstop=4

" 当使用缩进操作 (>> 和 <<) 时缩进的空格数
set shiftwidth=4

" 将 TAB 转换成空格
set expandtab

" 为缩进和对齐开启智能化的 TAB 和空格切换功能
set smarttab
```

### 参考

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)
