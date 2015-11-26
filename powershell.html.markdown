---
category: tool
tool: powershell
contributors:
    - ["Wouter Van Schandevijl", "https://github.com/laoujin"]
filename: LearnPowershell.ps1
---

PowerShell is the Windows scripting language and configuration management framework from Microsoft built on the .NET Framework. Windows 7 and up ship with PowerShell.  
Nearly all examples below can be a part of a shell script or executed directly in the shell.

A key difference with Bash is that it is mostly objects that you manipulate rather than plain text.

[Read more here.](https://technet.microsoft.com/en-us/library/bb978526.aspx)

```powershell
# As you already figured, comments start with #

# Simple hello world example:
echo Hello world!
# echo is an alias for Write-Output (=cmdlet)
# Most cmdlets and functions follow the Verb-Noun naming convention

# Each command starts on a new line, or after semicolon:
echo 'This is the first line'; echo 'This is the second line'

# Declaring a variable looks like this:
$Variable="Some string"
# Or like this:
$Variable1 = "Another string"

# Using the variable:
echo $Variable
echo "$Variable"
echo '$($Variable + '1')'
echo @"
This is a Here-String
$Variable
"@
# Note that ' (single quote) won't expand the variables!
# Here-Strings also work with single quote

# Builtin variables:
# There are some useful builtin variables, like
echo "Booleans: $TRUE and $FALSE"
echo "Empty value: $NULL"
echo "Last program's return value: $?"
echo "Script's PID: $PID"
echo "Number of arguments passed to script: $#"
echo "All arguments passed to script: $Args"
echo "Script's arguments separated into different variables: $1 $2..."

# Reading a value from input:
$Name = Read-Host "What's your name?"
echo "Hello, $Name!"
[int]$Age = Read-Host "What's your age?"