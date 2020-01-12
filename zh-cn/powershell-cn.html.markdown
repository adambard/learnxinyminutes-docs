---
category: tool
tool: powershell
contributors:
    - ["Wouter Van Schandevijl", "https://github.com/laoujin"]
translators:
    - ["谭九鼎", "http://github.com/imba-tjd"]
filename: LearnPowershell-cn.ps1
lang: zh-cn
---

PowerShell 是来自微软、基于 .NET 框架的 Windows 脚本语言和配置管理框架，Windows 7 及之后自带。
下方几乎所有例子都能作为脚本的一部分或者在 shell 中直接执行。

与 Bash 的关键不同是你操纵的基本上都是对象而不是纯文本。

[阅读更多](https://docs.microsoft.com/zh-cn/powershell/scripting/overview)


<!-- Keep the line numbers corresponding with en-us -->


如果你不确定你的环境状况：

```powershell
Get-ExecutionPolicy -List
Set-ExecutionPolicy AllSigned
# 执行策略包括：
# - Restricted: 无法执行脚本。
# - RemoteSigned: Downloaded scripts run only if signed by a trusted publisher.
# - AllSigned: Scripts need to be signed by a trusted publisher.
# - Unrestricted: 可运行所有脚本。
help about_Execution_Policies # 更多信息

# 当前 PowerShell 版本:
$PSVersionTable
```

获得帮助:

```powershell
# 寻找命令
Get-Command about_* # 别名(alias)：gcm
Get-Command -Verb Add
Get-Alias ps
Get-Alias -Definition Get-Process

Get-Help ps | less # 别名：help
ps | Get-Member # 别名：gm

Show-Command Get-EventLog # 显示图形界面填写参数

Update-Help # 需使用管理员权限运行
```

教程在此处开始:

```powershell
# 想必你已经明白了，注释以 # 开始

# 简单的 hello world 例子：
echo Hello world!
# echo 是 Write-Output (=cmdlet) 的 alias
# 大多数 cmdlets 和 functions 遵循 Verb-Noun 命名约定。

# 每个命令以新行开始，或者在分号之后
echo 'This is the first line'; echo 'This is the second line'

# 声明一个变量像这样：
$aString="Some string"
# 或像这样：
$aNumber = 5 -as [double]
$aList = 1,2,3,4,5
$anEmptyList = @()
$aString = $aList -join '--' # yes, -split exists also
$aHashtable = @{name1='val1'; name2='val2'}

# 使用变量：
echo $aString
echo "Interpolation: $aString"
echo "$aString has length of $($aString.Length)"
echo '$aString'
echo @"
This is a Here-String
$aString
"@
# 注意 ' (单引号) 不会扩展变量！
# Here-Strings also work with single quote

# 内置变量：
# 此处有一些有用的内置变量，如：
echo "Booleans: $TRUE and $FALSE"
echo "Empty value: $NULL"
echo "Last program's return value: $?"
echo "Exit code of last run Windows-based program: $LastExitCode"
echo "The last token in the last line received by the session: $$"
echo "The first token: $^"
echo "Script's PID: $PID"
echo "Full path of current script directory: $PSScriptRoot"
echo 'Full path of current script: ' + $MyInvocation.MyCommand.Path
echo "FUll path of current directory: $Pwd"
echo "Bound arguments in a function, script or code block: $PSBoundParameters"
echo "Unbound arguments: $($Args -join ', ')."
# 更多内置：`help about_Automatic_Variables`

# Inline 另一个文件（点运算符）
. .\otherScriptName.ps1


### 控制流
# 我们有通常的 if 结构：
if ($Age -is [string]) {
	echo 'But.. $Age cannot be a string!'
} elseif ($Age -lt 12 -and $Age -gt 0) {
	echo 'Child (Less than 12. Greater than 0)'
} else {
	echo 'Adult'
}

# Switch 语句想比大多数语言更强大
$val = "20"
switch($val) {
  { $_ -eq 42 }           { "结果是 42"; break }
  '20'                    { "Exactly 20"; break }
  { $_ -like 's*' }       { "大小写不敏感"; break }
  { $_ -clike 's*'}       { "clike, ceq, cne 用于大小写敏感"; break }
  { $_ -notmatch '^.*$'}  { "正则匹配。 cnotmatch, cnotlike, ..."; break }
  { 'x' -contains 'x'}    { "FALSE! -contains is for lists!"; break }
  default                 { "其它" }
}

# 经典的 for
for($i = 1; $i -le 10; $i++) {
  "Loop number $i"
}
# 或更短的
1..10 | % { "Loop number $_" }

# PowerShell 还提供
foreach ($var in 'val1','val2','val3') { echo $var }
# while () {}
# do {} while ()
# do {} until ()

# 异常处理
try {} catch {} finally {}
try {} catch [System.NullReferenceException] {
	echo $_.Exception | Format-List -Force
}


### Providers
# 列出当前文件夹中的文件和文件夹
ls # 或 `dir`
cd ~ # goto home

Get-Alias ls # -> Get-ChildItem
# Uh!? These cmdlets have generic names because unlike other scripting
# languages, PowerShell does not only operate in the current directory.
cd HKCU: # go to the HKEY_CURRENT_USER registry hive

# Get all providers in your session
Get-PSProvider


### Pipeline
# Cmdlets 有控制它们执行的参数：
Get-ChildItem -Filter *.txt -Name # 只获取所有 .txt 文件的名
# Only need to type as much of a parameter name until it is no longer ambiguous
ls -fi *.txt -n # -f is not possible because -Force also exists
# 使用 `Get-Help Get-ChildItem -Full` 来全面了解

# 上一个 cmdlet 的结果可以传递给下一个作为输入
# `$_` is the current object in the pipeline object.
ls | Where-Object { $_.Name -match 'c' } | Export-CSV export.txt
ls | ? { $_.Name -match 'c' } | ConvertTo-HTML | Out-File export.html

# If you get confused in the pipeline use `Get-Member` for an overview
# of the available methods and properties of the pipelined objects:
ls | Get-Member
Get-Date | gm

# ` is the line continuation character. Or end the line with a |
Get-Process | Sort-Object ID -Descending | Select-Object -First 10 Name,ID,VM `
	| Stop-Process -WhatIf

Get-EventLog Application -After (Get-Date).AddHours(-2) | Format-List

# Use % as a shorthand for ForEach-Object
(a,b,c) | ForEach-Object `
	-Begin { "Starting"; $counter = 0 } `
	-Process { "Processing $_"; $counter++ } `
	-End { "Finishing: $counter" }

# Get-Process as a table with three columns
# The third column is the value of the VM property in MB and 2 decimal places
# Computed columns can be written more verbose as:
# `@{name='lbl';expression={$_}`
ps | Format-Table ID,Name,@{n='VM(MB)';e={'{0:n2}' -f ($_.VM / 1MB)}} -autoSize


### 函数
# [string] 特性是可选的。
function foo([string]$name) {
	echo "Hey $name, have a function"
}

# 调用你自己的函数
foo "Say my name"

# Functions with named parameters, parameter attributes, parsable documentation
<#
.SYNOPSIS
Setup a new website
.DESCRIPTION
Creates everything your new website needs for much win
.PARAMETER siteName
The name for the new website
.EXAMPLE
New-Website -Name FancySite -Po 5000
New-Website SiteWithDefaultPort
New-Website siteName 2000 # ERROR! Port argument could not be validated
('name1','name2') | New-Website -Verbose
#>
function New-Website() {
	[CmdletBinding()]
	param (
		[Parameter(ValueFromPipeline=$true, Mandatory=$true)]
		[Alias('name')]
		[string]$siteName,
		[ValidateSet(3000,5000,8000)]
		[int]$port = 3000
	)
	BEGIN { Write-Verbose 'Creating new website(s)' }
	PROCESS { echo "name: $siteName, port: $port" }
	END { Write-Verbose 'Website(s) created' }
}


### 全都是 .NET
# 一个 PS 字符串实际上是一个 .NET System.String
# 所有的 .NET 方法和属性也因此可以使用
'string'.ToUpper().Replace('G', 'ggg')
# 或更 powershellish
'string'.ToUpper() -replace 'G', 'ggg'

# Unsure how that .NET method is called again?
'string' | gm

# 调用 .NET 静态方法的语法
[System.Reflection.Assembly]::LoadWithPartialName('Microsoft.VisualBasic')

# 注意 .NET 函数**必须**使用小括号调用
# 而 PS 函数**无法**用小括号调用。
# If you do call a cmdlet/PS function with parentheses,
# it is the same as passing a single parameter list
$writer = New-Object System.IO.StreamWriter($path, $true)
$writer.Write([Environment]::NewLine)
$writer.Dispose()

### IO
# 从输入读取值：
$Name = Read-Host "What's your name?"
echo "Hello, $Name!"
[int]$Age = Read-Host "What's your age?"

# Test-Path, Split-Path, Join-Path, Resolve-Path
# Get-Content filename # returns a string[]
# Set-Content, Add-Content, Clear-Content
Get-Command ConvertTo-*,ConvertFrom-*


### 有用的东西
# 刷新你的 PATH
$env:PATH = [System.Environment]::GetEnvironmentVariable("Path", "Machine") +
	";" + [System.Environment]::GetEnvironmentVariable("Path", "User")

# 在 path 中找 Python
$env:PATH.Split(";") | Where-Object { $_ -like "*python*"}

# 改变工作目录而无需记住之前的路径
Push-Location c:\temp # 将工作目录改为 c:\temp
Pop-Location # 改回之前的工作目录
# 别名为: pushd and popd

# 下载完成后解锁目录
Get-ChildItem -Recurse | Unblock-File

# 在当前目录打开 Windows 资源管理器
ii .

# 按任意键退出
$host.UI.RawUI.ReadKey()
return

# 创建一个 shortcut
$WshShell = New-Object -comObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut($link)
$Shortcut.TargetPath = $file
$Shortcut.WorkingDirectory = Split-Path $file
$Shortcut.Save()
```


Configuring your shell

```powershell
# $Profile 是你的 `Microsoft.PowerShell_profile.ps1` 的完整路径
# 那里所有的代码会在 PS session 开始时执行
if (-not (Test-Path $Profile)) {
	New-Item -Type file -Path $Profile -Force
	notepad $Profile
}
# 更多信息： `help about_profiles`
# 寻求更有用的 shell，一定要看下面的 PSReadLine 项目
```

有趣的项目

* [Channel9](https://channel9.msdn.com/Search?term=powershell%20pipeline#ch9Search&lang-en=en) PowerShell tutorials
* [PSGet](https://github.com/psget/psget) NuGet for PowerShell
* [PSReadLine](https://github.com/lzybkr/PSReadLine/) A bash inspired readline implementation for PowerShell (So good that it now ships with Windows10 by default!)
* [Posh-Git](https://github.com/dahlbyk/posh-git/) Fancy Git Prompt (Recommended!)
* [PSake](https://github.com/psake/psake) Build automation tool
* [Pester](https://github.com/pester/Pester) BDD Testing Framework
* [Jump-Location](https://github.com/tkellogg/Jump-Location) Powershell `cd` that reads your mind
* [PowerShell Community Extensions](https://github.com/Pscx/Pscx)

未提到

* WMI: Windows Management Intrumentation (Get-CimInstance)
* Multitasking: Start-Job -scriptBlock {...},
* Code Signing
* Remoting (Enter-PSSession/Exit-PSSession; Invoke-Command)
