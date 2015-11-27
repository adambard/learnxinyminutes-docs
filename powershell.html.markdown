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

If you are uncertain about your environment:
```powershell
Get-ExecutionPolicy -List
Set-ExecutionPolicy AllSigned
# Execution policies include:
# - Restricted: Scripts won't run.
# - RemoteSigned: Downloaded scripts run only if signed by a trusted publisher. 
# - AllSigned: Scripts need to be signed by a trusted publisher.
# - Unrestricted: Run all scripts.
help about_Execution_Policies # for more info

# Current PowerShell version:
$PSVersionTable
```

The tutorial starts here:
```powershell
# As you already figured, comments start with #

# Simple hello world example:
echo Hello world!
# echo is an alias for Write-Output (=cmdlet)
# Most cmdlets and functions follow the Verb-Noun naming convention

# Each command starts on a new line, or after semicolon:
echo 'This is the first line'; echo 'This is the second line'

# Declaring a variable looks like this:
$aString="Some string"
# Or like this:
$aNumber = 5

# Using the variable:
echo $aString
echo "Interpolation: $aString"
echo "`$aString has length of $($aString.length)"
echo '$aString'
echo @"
This is a Here-String
$aString
"@
# Note that ' (single quote) won't expand the variables!
# Here-Strings also work with single quote

# Builtin variables:
# There are some useful builtin variables, like
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
echo "Unbound arguments: $($Args -join ', ')." ######################## MOVE THIS TO FUNCTIONS
# More builtins: `help about_Automatic_Variables`

# Reading a value from input:
$Name = Read-Host "What's your name?"
echo "Hello, $Name!"
[int]$Age = Read-Host "What's your age?"




```


Configuring your shell
```powershell
# $Profile is the full path for your `Microsoft.PowerShell_profile.ps1`
# All code there will be executed when the PS session starts
if (-not (Test-Path $Profile)) {
	New-Item -Type file -Path $Profile -Force
	notepad $Profile
}
# More info: `help about_profiles`
```