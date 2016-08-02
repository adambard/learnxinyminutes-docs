---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
    - ["Jonathan Wang", "https://github.com/Jonathansw"]   
    - ["Leo Rudberg", "https://github.com/LOZORD"]
    - ["Betsy Lorton", "https://github.com/schbetsy"]
    - ["John Detter", "https://github.com/jdetter"]
translators:
    - ["Jinchang Ye", "https://github.com/Alwayswithme"]
    - ["Chunyang Xu", "https://github.com/XuChunyang"]
    - ["Weihang Lo", "https://github.com/weihanglo"]
filename: LearnBash-tw.sh
lang: zh-tw
---

Bash 是一個爲 GNU 計劃編寫的 Unix shell，是 Linux 和 Mac OS X 下預設的 shell。
以下大多數例子可以作爲腳本的一部分運行，也可直接在 shell 下互動執行。

[更多資訊](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# 腳本的第一行叫 shebang，用來告知系統如何執行該腳本:
# 參見： http://en.wikipedia.org/wiki/Shebang_(Unix)
# 如你所見，註釋以 # 開頭，shebang 也是註釋。

# 顯示 “Hello world!”
echo Hello world!

# 每一句指令以換行或分號隔開：
echo 'This is the first line'; echo 'This is the second line'

# 宣告一個變數：
Variable="Some string"

# 下面是錯誤的做法：
Variable = "Some string"
# Bash 會把 Variable 當做一個指令，由於找不到該指令，因此這裡會報錯。

# 也不可以這樣：
Variable= 'Some string'
# Bash 會認爲 'Some string' 是一條指令，由於找不到該指令，這裡會再次報錯。
# （這個例子中 'Variable=' 這部分會被當作僅對 'Some string' 起作用的賦值。）

# 使用變數：
echo $Variable
echo "$Variable"
echo '$Variable'
# 當你賦值 (assign) 、匯出 (export)，或者以其他方式使用變數時，變數名前不加 $。
# 如果要使用變數的值， 則要加 $。
# 注意： ' (單引號) 不會展開變數。

# 參數展開式 ${}:
echo ${Variable}
# 這是一個參數展開的簡單用法
# 使用參數展開會得到該變數的值，也就是會「展開」或印出該值。
# 在展開期間，可以修改該值或該參數。
# 以下是修改參數展開式的範例：

# 在變數內部進行字串代換
echo ${Variable/Some/A}
# 會把 Variable 中首次出現的 "some" 替換成 “A”。

# 變數的截取
Length=7
echo ${Variable:0:Length}
# 這樣僅會返回變數值的前7個字元

# 變數的預設值
echo ${Foo:-"DefaultValueIfFooIsMissingOrEmpty"}
# 對 null (Foo=) 和空字串 (Foo="") 起作用； 零（Foo=0）時返回0
# 注意這僅返回預設值而不是改變變數的值

# 括號展開 { }
# 用以產生任意的字串
echo {1..10}
echo {a..z}
# 這將會輸出該範圍內的起始值到最終值。

# 內建變數：
# 下面的內建變數很有用
echo "Last program's return value: $?"
echo "Script's PID: $$"
echo "Number of arguments: $#"
echo "Scripts arguments: $@"
echo "Scripts arguments separated in different variables: $1 $2..."

# 現在，我們知道變數如何使用與印出
# 讓我們開始學習其他Bash基礎吧！

# 使用 `pwd` 指令，可以得知當前工作目錄
# `pwd` 意指 「印出工作目錄」(print working directory)。
# 我們也可使用內建變數 `$PWD`。
# 下列兩行指令等價：
echo "I'm in $(pwd)" # 執行 `pwd` 且將該值內插至輸出中
echo "I'm in $PWD" # 直接內插 `$PWD` 變數

# 如果終端機上有太多輸出，`clear` 指令可以清除螢幕先前的輸出
clear
# Ctrl-L 也有相同的效果

# 讀取輸入：
echo "What's your name?"
read Name # 這裡不需要宣告新變數
echo Hello, $Name!

# 一般 if 結構看起來像這樣：
# 'man test' 可查看更多的信息
if [ $Name != $USER ]
then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
fi

# 注意： 如果 $Name 為空，bash會將該條件式解讀成：
if [ != USER ]
# 這是一個錯誤的語法
# 所以，安全避免空變數的方法如下：
if [ "$Name" != $USER ] ...
# 如果 $Name 為空，該條件式將被視為：
if [ "" != $USER]
# 此條件式可正常運作


# 根據上一個指令執行結果決定是否執行下一個指令
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# 在 if 語句中使用 && 和 || 需要多對方括號
if [ $Name == "Steve" ] && [ $Age -eq 15 ]
then
    echo "This will run if $Name is Steve AND $Age is 15."
fi

if [ $Name == "Daniya" ] || [ $Name == "Zach" ]
then
    echo "This will run if $Name is Daniya OR Zach."
fi

# 表達式的格式如下:
echo $(( 10 + 5 ))

# 與其他程式語言不同的是，bash 運行時依賴上下文。比如，使用 ls 時，列出當前目錄。
ls

# 指令可以帶有選項：
ls -l # 列出文件和目錄的詳細信息
ls -t # 以最後修改時間，對文件與目錄排序
ls -R # 遞迴列出目錄與次目錄的內容

# 前一個指令的輸出可以當作後一個指令的輸入。grep 用來匹配字串。
# 用下面的指令列出當前目錄下所有的 txt 文件：
ls -l | grep "\.txt"

# 使用 `cat` 將檔案印出在標準輸出中：
cat file.txt

# 使用 `cat` 讀取檔案
Contents=$(cat file.txt)
echo "START OF FILE\n$Contents\nEND OF FILE"

# 使用 `cp` 複製檔案或目錄，`cp` 會創建新版本的來源檔案／目錄
# 所以，編輯副本不會影響到初始來源（反之亦然）。
# 注意，如果目的地已存在該檔案／目錄，該檔案／目錄將會被覆寫
cp srcFile.txt clone.txt
cp -r srcDirectory/ dst/ # 遞迴複製

#  `scp` or `sftp` if you plan on exchanging files between computers.
# 如需在兩台電腦間交換檔案，請查看 `scp` 或 `sftp`。
# `scp` 與 `cp` 相似。
# `sftp` 則有更高的互動性（與 `ftp` 相似）。

# 使用 `mv` 來移動目錄與檔案。
# `mv` 與 `cp` 相似，但會刪除來源。
# `mv` 也可以用來重新命名檔案／目錄！
mv s0urc3.txt dst.txt

# 由於 bash 運行時依賴當前目錄的上下文，
# 需要在其他目錄執行指令時，可使用 `cd` 改變當前目錄：
cd ~    # 到家目錄
cd ..   # 到上一層目錄
        # (^^例如, 從 /home/username/Downloads 到 /home/username)
cd /home/username/Documents   # 到指定目錄
cd ~/Documents/..    # 仍位於家目錄，不是嗎？

# 使用子殼程式 (subshells) 在不同目錄間工作
(echo "First, I'm here: $PWD") && (cd someDir; echo "Then, I'm here: $PWD")
pwd # 仍在第一個目錄

# 使用 `mkdir` 來建立新的目錄
mkdir myNewDir
# 使用 `-p` 選項參數，將會自動創建路徑中不存在的目錄
mkdir -p myNewDir/with/intermediate/directories

# 將指令的輸出輸入重新導向（標準輸入、標準輸出、標準錯誤輸出）。
# 從標準輸入讀取資料，直到 ^EOF$ （End-of-file)，且將讀取的資料覆寫至hello.py
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# 重新導向可以到標準輸出（stdout），標準輸入（stdin）和標準錯誤輸出（stderr）。
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# `>` 會覆蓋已存在的文件， `>>` 會以累加的方式輸出文件中。
python hello.py >> "output.out" 2>> "error.err"

# 覆蓋 output.out , 追加 error.err 並統計行數
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# 運行指令並印出文件描述 （比如 /dev/fd/123）
# 具體可查看： man fd
echo <(echo "#helloworld")

# 以 "#helloworld" 覆蓋 output.out:
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# 清理臨時文件並顯示詳情（增加 '-i' 選項啓用互動模式）
# 警告： `rm` 指令無法復原
rm -v output.out error.err output-and-error.log
rm -r tempDir/ # 遞迴刪除

# 一個指令可用 $( ) 嵌套在另一個指令內部：
# 以下的指令會印出當前目錄下的目錄和文件總數
echo "There are $(ls | wc -l) items here."

# 反引號 `` 起相同作用，但不允許嵌套
# 優先使用 $(  ).
echo "There are `ls | wc -l` items here."

# Bash 的 case 語句與 Java 和 C++ 中的 switch 語句類似:
case "$Variable" in
    # 列出需要匹配的字串
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# 循環遍歷給定的參數序列:
# 變數$Variable 的值會被印出 3 次。
for Variable in {1..3}
do
    echo "$Variable"
done

# 或傳統的 “for循環” ：
for ((a=1; a <= 3; a++))
do
    echo $a
done

# 也可以用於文件
# 用 cat 輸出 file1 和 file2 內容
for Variable in file1 file2
do
    cat "$Variable"
done

# 或作用於其他命令的輸出
# 對 ls 輸出的文件執行 cat 指令。
for Output in $(ls)
do
    cat "$Output"
done

# while 循環：
while [ true ]
do
    echo "loop body here..."
    break
done

# 你也可以使用函數
# 定義函數：
function foo ()
{
    echo "Arguments work just like script arguments: $@"
    echo "And: $1 $2..."
    echo "This is a function"
    return 0
}

# 更簡單的方法
bar ()
{
    echo "Another way to declare functions!"
    return 0
}

# 呼叫函數
foo "My name is" $Name

# 有很多有用的指令需要學習:
# 打印 file.txt 的最後 10 行
tail -n 10 file.txt
# 印出 file.txt 的前 10 行
head -n 10 file.txt
# 將 file.txt 按行排序
sort file.txt
# 報告或忽略重複的行，用選項 -d 印出重複的行
uniq -d file.txt
# 打印每行中 ',' 之前內容
cut -d ',' -f 1 file.txt
# 將 file.txt 文件所有 'okay' 替換爲 'great', （兼容正規表達式）
sed -i 's/okay/great/g' file.txt
# 將 file.txt 中匹配正則的行打印到標準輸出
# 這裡印出以 "foo" 開頭, "bar" 結尾的行
grep "^foo.*bar$" file.txt
# 使用選項 "-c" 統計行數
grep -c "^foo.*bar$" file.txt
# 其他實用的選項參數
grep -r "^foo.*bar$" someDir/ # 遞迴的 `grep`
grep -n "^foo.*bar$" file.txt # 顯示行數
grep -rI "^foo.*bar$" someDir/ # 遞迴的 `grep`, 但忽略二進位檔案
# 同樣的搜尋，再過濾包含「baz」的行
grep "^foo.*bar$" file.txt | grep -v "baz"

# 如果要搜尋字面上的字串而不是用正規表達式，使用 `fgrep` 或 `grep -F`
fgrep "foobar" file.txt

# trap command allows you to execute a command when a signal is received by your script.
# `trap` 可以在一個script運行，接收到特定信號時，執行對應的指令
# `trap` 接收到 `SIGHUP`、`SIGINT`、`SIGTERM` 信號時，會移除 $TEMP_FILE
trap "rm $TEMP_FILE; exit" SIGHUP SIGINT SIGTERM

# `sudo` 可用於以superuser的身分執行指令
$NAME1=$(whoami)
$NAME2=$(sudo whoami)
echo "Was $NAME1, then became more powerful $NAME2"

# 以 bash 內建的 'help' 指令閱讀 Bash 內建文件：
help
help help
help for
help return
help source
help .

# 用 man 指令閱讀相關的 Bash 手冊
apropos bash
man 1 bash
man bash

# 用 info 指令查閱命令的 info 文件 （info 中按 ? 顯示幫助信息）
apropos info | grep '^info.*('
man info
info info
info 5 info

# 閱讀 Bash 的 info 文件：
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
