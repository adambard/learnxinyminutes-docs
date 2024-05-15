---
category: tool
tool: powershell
contributors:
    - ["Wouter Van Schandevijl", "https://github.com/laoujin"]
    - ["Andrew Ryan Davis", "https://github.com/AndrewDavis1191"]
filename: LearnPowershell.ps1
---

PowerShell is the Windows scripting language and configuration management
framework from Microsoft built on the .NET Framework. Windows 7 and up ship
with PowerShell.  
Nearly all examples below can be a part of a shell script or executed directly
in the shell.

A key difference with Bash is that it is mostly objects that you manipulate
rather than plain text. After years of evolving, it resembles Python a bit.

[Read more here.](https://docs.microsoft.com/powershell/scripting/overview)

Powershell as a Language:

```powershell
# Single line comments start with a number symbol.

<#
  Multi-line comments
  like so
#>


####################################################
## 1. Primitive Datatypes and Operators
####################################################

# Numbers
3 # => 3

# Math
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# Powershell uses banker's rounding,
# meaning [int]1.5 would round to 2 but so would [int]2.5
# Division always returns a float. 
# You must cast result to [int] to round.
[int]5 / [int]3       # => 1.66666666666667
[int]-5 / [int]3      # => -1.66666666666667
5.0 / 3.0   # => 1.66666666666667
-5.0 / 3.0  # => -1.66666666666667
[int]$result = 5 / 3 
$result # => 2

# Modulo operation
7 % 3  # => 1

# Exponentiation requires longform or the built-in [Math] class.
[Math]::Pow(2,3)  # => 8

# Enforce order of operations with parentheses.
1 + 3 * 2  # => 7
(1 + 3) * 2  # => 8

# Boolean values are primitives (Note: the $)
$True  # => True
$False  # => False

# negate with !
!$True   # => False
!$False  # => True

# Boolean Operators
# Note "-and" and "-or" usage
$True -and $False  # => False
$False -or $True   # => True

# True and False are actually 1 and 0 but only support limited arithmetic.
# However, casting the bool to int resolves this.
$True + $True # => 2
$True * 8    # => '[System.Boolean] * [System.Int32]' is undefined
[int]$True * 8 # => 8
$False - 5   # => -5

# Comparison operators look at the numerical value of True and False.
0 -eq $False  # => True
1 -eq $True   # => True
2 -eq $True   # => False
-5 -ne $False # => True

# Using boolean logical operators on ints casts to booleans for evaluation.
# but their non-cast value is returned
# Don't mix up with bool(ints) and bitwise -band/-bor
[bool](0)     # => False
[bool](4)     # => True
[bool](-6)    # => True
0 -band 2     # => 0
-5 -bor 0     # => -5

# Equality is -eq (equals)
1 -eq 1  # => True
2 -eq 1  # => False

# Inequality is -ne (notequals)
1 -ne 1  # => False
2 -ne 1  # => True

# More comparisons
1 -lt 10  # => True
1 -gt 10  # => False
2 -le 2  # => True
2 -ge 2  # => True

# Seeing whether a value is in a range
1 -lt 2 -and 2 -lt 3  # => True
2 -lt 3 -and 3 -lt 2  # => False

# (-is vs. -eq) -is checks if two objects are the same type.
# -eq checks if the objects have the same values, but sometimes doesn't work
# as expected.
# Note: we called '[Math]' from .NET previously without the preceeding
# namespaces. We can do the same with [Collections.ArrayList] if preferred.
[System.Collections.ArrayList]$a = @()  # Point a at a new list
$a = (1,2,3,4)
$b = $a                                 # => Point b at what a is pointing to
$b -is $a.GetType()                     # => True, a and b equal same type
$b -eq $a                               # => None! See below
[System.Collections.Hashtable]$b = @{}  # => Point a at a new hash table
$b = @{'one' = 1 
       'two' = 2}
$b -is $a.GetType()                     # => False, a and b types not equal

# Strings are created with " or ' but " is required for string interpolation
"This is a string."
'This is also a string.'

# Strings can be added too! But try not to do this.
"Hello " + "world!"  # => "Hello world!"

# A string can be treated like a list of characters
"Hello world!"[0]  # => 'H'

# You can find the length of a string
("This is a string").Length  # => 16

# You can also format using f-strings or formatted string literals.
$name = "Steve"
$age = 22
"He said his name is $name." 
# => "He said his name is Steve"
"{0} said he is {1} years old." -f $name, $age 
# => "Steve said he is 22 years old"
"$name's name is $($name.Length) characters long." 
# => "Steve's name is 5 characters long."

# Strings can be compared with -eq, but are case insensitive. We can
# force with -ceq or -ieq.
"ab" -eq "ab"  # => True
"ab" -eq "AB"  # => True!
"ab" -ceq "AB"  # => False
"ab" -ieq "AB"  # => True

# Escape Characters in Powershell
# Many languages use the '\', but Windows uses this character for 
# file paths. Powershell thus uses '`' to escape characters
# Take caution when working with files, as '`' is a
# valid character in NTFS filenames.
"Showing`nEscape Chars" # => new line between Showing and Escape
"Making`tTables`tWith`tTabs" # => Format things with tabs

# Negate pound sign to prevent comment
# Note that the function of '#' is removed, but '#' is still present
`#Get-Process # => Fail: not a recognized cmdlet

# $null is not an object
$null  # => None

# $null, 0, and empty strings and arrays all evaluate to False.
# All other values are True
function Test-Value ($value) {
  if ($value) {
    Write-Output 'True'
  }
  else {
    Write-Output 'False'
  }
}

Test-Value ($null) # => False
Test-Value (0)     # => False
Test-Value ("")    # => False
Test-Value []      # => True 
# *[] calls .NET class; creates '[]' string when passed to function
Test-Value ({})    # => True
Test-Value @()     # => False


####################################################
## 2. Variables and Collections
####################################################

# Powershell uses the "Write-Output" function to print
Write-Output "I'm Posh. Nice to meet you!"  # => I'm Posh. Nice to meet you!

# Simple way to get input data from console
$userInput = Read-Host "Enter some data: " # Returns the data as a string

# There are no declarations, only assignments.
# Convention is to use camelCase or PascalCase, whatever your team uses.
$someVariable = 5
$someVariable  # => 5

# Accessing a previously unassigned variable does not throw exception.
# The value is $null by default

# Ternary Operators exist in Powershell 7 and up
0 ? 'yes' : 'no'  # => no


# The default array object in Powershell is an fixed length array.
$defaultArray = "thing","thing2","thing3"
# you can add objects with '+=', but cannot remove objects.
$defaultArray.Add("thing4") # => Exception "Collection was of a fixed size."
# To have a more workable array, you'll want the .NET [ArrayList] class
# It is also worth noting that ArrayLists are significantly faster

# ArrayLists store sequences
[System.Collections.ArrayList]$array = @()
# You can start with a prefilled ArrayList
[System.Collections.ArrayList]$otherArray = @(5, 6, 7, 8)

# Add to the end of a list with 'Add' (Note: produces output, append to $null)
$array.Add(1) > $null    # $array is now [1]
$array.Add(2) > $null    # $array is now [1, 2]
$array.Add(4) > $null    # $array is now [1, 2, 4]
$array.Add(3) > $null    # $array is now [1, 2, 4, 3]
# Remove from end with index of count of objects-1; array index starts at 0
$array.RemoveAt($array.Count-1) # => 3 and array is now [1, 2, 4]
# Let's put it back
$array.Add(3) > $null   # array is now [1, 2, 4, 3] again.

# Access a list like you would any array
$array[0]   # => 1
# Look at the last element
$array[-1]  # => 3
# Looking out of bounds returns nothing
$array[4]  # blank line returned

# Remove elements from a array
$array.Remove($array[3])  # $array is now [1, 2, 4]

# Insert at index an element 
$array.Insert(2, 3)  # $array is now [1, 2, 3, 4]

# Get the index of the first item found matching the argument
$array.IndexOf(2)  # => 1
$array.IndexOf(6)  # Returns -1 as "outside array" 

# You can add arrays
# Note: values for $array and for $otherArray are not modified.
$array + $otherArray  # => [1, 2, 3, 4, 5, 6, 7, 8]

# Concatenate arrays with "AddRange()"
$array.AddRange($otherArray)  # Now $array is [1, 2, 3, 4, 5, 6, 7, 8]

# Check for existence in a array with "in"
1 -in $array  # => True

# Examine length with "Count" (Note: "Length" on arrayList = each items length)
$array.Count  # => 8

# You can look at ranges with slice syntax.
$array[1,3,5]     # Return selected index  => [2, 4, 6]
$array[1..3]      # Return from index 1 to 3 => [2, 3, 4]
$array[-3..-1]    # Return from last 3 to last 1 => [6, 7, 8]
$array[-1..-3]    # Return from last 1 to last 3 => [8, 7, 6]
$array[2..-1]     # Return from index 2 to last (NOT as most expect) => [3, 2, 1, 8]
$array[0,2+4..6]  # Return multiple ranges with the + => [1, 3, 5, 6, 7]

# -eq doesn't compare array but extract the matching elements
$array = 1,2,3,1,1
$array -eq 1          # => 1,1,1
($array -eq 1).Count  # => 3

# Tuples are like arrays but are immutable.
# To use Tuples in powershell, you must use the .NET tuple class.
$tuple = [System.Tuple]::Create(1, 2, 3)
$tuple.Item(0)      # => 1
$tuple.Item(0) = 3  # Raises a TypeError

# You can do some of the array methods on tuples, but they are limited.
$tuple.Length       # => 3
$tuple + (4, 5, 6)  # => Exception
$tuple[0..2]        # => $null (in powershell 5)    => [1, 2, 3] (in powershell 7)
2 -in $tuple        # => False


# Hashtables store mappings from keys to values, similar to (but distinct from) Dictionaries.
# Hashtables do not hold entry order as arrays do. 
$emptyHash = @{}
# Here is a prefilled hashtable
$filledHash = @{"one"= 1 
                "two"= 2 
                "three"= 3}

# Look up values with []
$filledHash["one"]  # => 1

# Get all keys as an iterable with ".Keys".
$filledHash.Keys  # => ["one", "two", "three"]

# Get all values as an iterable with ".Values".
$filledHash.Values  # => [1, 2, 3]

# Check for existence of keys or values in a hash with "-in"
"one" -in $filledHash.Keys  # => True
1 -in $filledHash.Values    # => False (in powershell 5)    => True (in powershell 7)

# Looking up a non-existing key returns $null
$filledHash["four"]  # $null

# Adding to a hashtable
$filledHash.Add("five",5)  # $filledHash["five"] is set to 5
$filledHash.Add("five",6)  # exception "Item with key "five" has already been added"
$filledHash["four"] = 4    # $filledHash["four"] is set to 4, running again does nothing

# Remove keys from a hashtable
$filledHash.Remove("one") # Removes the key "one" from filled hashtable


####################################################
## 3. Control Flow and Iterables
####################################################

# Let's just make a variable
$someVar = 5

# Here is an if statement.
# This prints "$someVar is smaller than 10"
if ($someVar -gt 10) {
    Write-Output "$someVar is bigger than 10."
}
elseif ($someVar -lt 10) {    # This elseif clause is optional.
    Write-Output "$someVar is smaller than 10."
}
else {                        # This is optional too.
    Write-Output "$someVar is indeed 10."
}


<#
Foreach loops iterate over arrays
prints:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
#>
foreach ($animal in ("dog", "cat", "mouse")) {
    # You can use -f to interpolate formatted strings
    "{0} is a mammal" -f $animal
}

<#
For loops iterate over arrays and you can specify indices
prints:
   0 a
   1 b
   2 c
   3 d
   4 e
   5 f
   6 g
   7 h
#>
$letters = ('a','b','c','d','e','f','g','h')
for($i=0; $i -le $letters.Count-1; $i++){
    Write-Host $i, $letters[$i]
}

<#
While loops go until a condition is no longer met.
prints:
    0
    1
    2
    3
#>
$x = 0
while ($x -lt 4) {
    Write-Output $x
    $x += 1  # Shorthand for x = x + 1
}

# Switch statements are more powerful compared to most languages
$val = "20"
switch($val) {
  { $_ -eq 42 }           { "The answer equals 42"; break }
  '20'                    { "Exactly 20"; break }
  { $_ -like 's*' }       { "Case insensitive"; break }
  { $_ -clike 's*'}       { "clike, ceq, cne for case sensitive"; break }
  { $_ -notmatch '^.*$'}  { "Regex matching. cnotmatch, cnotlike, ..."; break }
  default                 { "Others" }
}

# Handle exceptions with a try/catch block
try {
    # Use "throw" to raise an error
    throw "This is an error"
}
catch {
    Write-Output $Error.ExceptionMessage
}
finally {
    Write-Output "We can clean up resources here"
}


# Writing to a file
$contents = @{"aa"= 12 
             "bb"= 21}
$contents | Export-CSV "$env:HOMEDRIVE\file.csv" # writes to a file

$contents = "test string here"
$contents | Out-File "$env:HOMEDRIVE\file.txt" # writes to another file

# Read file contents and convert to json
Get-Content "$env:HOMEDRIVE\file.csv" | ConvertTo-Json


####################################################
## 4. Functions
####################################################

# Use "function" to create new functions
# Keep the Verb-Noun naming convention for functions
function Add-Numbers {
 $args[0] + $args[1]
}

Add-Numbers 1 2 # => 3

# Calling functions with parameters
function Add-ParamNumbers {
 param( [int]$firstNumber, [int]$secondNumber )
 $firstNumber + $secondNumber
}

Add-ParamNumbers -FirstNumber 1 -SecondNumber 2 # => 3 

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
    BEGIN { Write-Output 'Creating new website(s)' }
    PROCESS { Write-Output "name: $siteName, port: $port" }
    END { Write-Output 'Website(s) created' }
}


####################################################
## 5. Modules
####################################################

# You can import modules and install modules
# The Install-Module is similar to pip or npm, pulls from Powershell Gallery
Install-Module dbaTools
Import-Module dbaTools

$query = "SELECT * FROM dbo.sometable"
$queryParams = @{
    SqlInstance = 'testInstance'
    Database    = 'testDatabase'
    Query       = $query
}
Invoke-DbaQuery @queryParams

# You can get specific functions from a module
Import-Module -Function Invoke-DbaQuery


# Powershell modules are just ordinary Posh files. You
# can write your own, and import them. The name of the
# module is the same as the name of the file.

# You can find out which functions and attributes
# are defined in a module.
Get-Command -module dbaTools
Get-Help dbaTools -Full


####################################################
## 6. Classes
####################################################

# We use the "class" statement to create a class
class Instrument {
    [string]$Type
    [string]$Family
}

$instrument = [Instrument]::new()
$instrument.Type = "String Instrument"
$instrument.Family = "Plucked String"

$instrument

<# Output:
Type              Family        
----              ------        
String Instrument Plucked String
#>


####################################################
## 6.1 Inheritance
####################################################

# Inheritance allows new child classes to be defined that inherit 
# methods and variables from their parent class.

class Guitar : Instrument
{
    [string]$Brand
    [string]$SubType
    [string]$ModelType
    [string]$ModelNumber
}

$myGuitar = [Guitar]::new()
$myGuitar.Brand       = "Taylor"
$myGuitar.SubType     = "Acoustic"
$myGuitar.ModelType   = "Presentation"
$myGuitar.ModelNumber = "PS14ce Blackwood"

$myGuitar.GetType()

<#
IsPublic IsSerial Name                                     BaseType                                               
-------- -------- ----                                     --------                                               
True     False    Guitar                                   Instrument  
#>


####################################################
## 7. Advanced
####################################################

# The powershell pipeline allows things like High-Order Functions.

# Group-Object is a handy cmdlet that does incredible things.
# It works much like a GROUP BY in SQL.

<#
 The following will get all the running processes,
 group them by Name,
 and tell us how many instances of each process we have running.
 Tip: Chrome and svcHost are usually big numbers in this regard.
#>
Get-Process | Foreach-Object ProcessName | Group-Object

# Useful pipeline examples are iteration and filtering.
1..10 | ForEach-Object { "Loop number $PSITEM" }
1..10 | Where-Object { $PSITEM -gt 5 } | ConvertTo-Json

# A notable pitfall of the pipeline is its performance when
# compared with other options.
# Additionally, raw bytes are not passed through the pipeline,
# so passing an image causes some issues.
# See more on that in the link at the bottom.

<#
 Asynchronous functions exist in the form of jobs.
 Typically a procedural language,
 Powershell can operate non-blocking functions when invoked as Jobs.
#>

# This function is known to be non-optimized, and therefore slow.
$installedApps = Get-CimInstance -ClassName Win32_Product

# If we had a script, it would hang at this func for a period of time.
$scriptBlock = {Get-CimInstance -ClassName Win32_Product}
Start-Job -ScriptBlock $scriptBlock

# This will start a background job that runs the command.
# You can then obtain the status of jobs and their returned results.
$allJobs = Get-Job
$jobResponse = Get-Job | Receive-Job


# Math is built in to powershell and has many functions.
$r=2
$pi=[math]::pi
$r2=[math]::pow( $r, 2 )
$area = $pi*$r2
$area

# To see all possibilities, check the members.
[System.Math] | Get-Member -Static -MemberType All


<#
 This is a silly one:
 You may one day be asked to create a func that could take $start and $end
 and reverse anything in an array within the given range
 based on an arbitrary array without mutating the original array.
 Let's see one way to do that and introduce another data structure.
#>

$targetArray = 'a','b','c','d','e','f','g','h','i','j','k','l','m'

function Format-Range ($start, $end, $array) {
    [System.Collections.ArrayList]$firstSectionArray = @()
    [System.Collections.ArrayList]$secondSectionArray = @()
    [System.Collections.Stack]$stack = @()
    for ($index = 0; $index -lt $array.Count; $index++) {
        if ($index -lt $start) {
            $firstSectionArray.Add($array[$index]) > $null
        }
        elseif ($index -ge $start -and $index -le $end) {
            $stack.Push($array[$index])
        }
        else {
            $secondSectionArray.Add($array[$index]) > $null
        }
    }
    $finalArray = $firstSectionArray + $stack.ToArray() + $secondSectionArray
    return $finalArray
}

Format-Range 2 6 $targetArray 
# => 'a','b','g','f','e','d','c','h','i','j','k','l','m'

# The previous method works, but uses extra memory by allocating new arrays.
# It's also kind of lengthy.
# Let's see how we can do this without allocating a new array.
# This is slightly faster as well.

function Format-Range ($start, $end) {
  while ($start -lt $end)
  {
      $temp = $targetArray[$start]
      $targetArray[$start] = $targetArray[$end]
      $targetArray[$end] = $temp
      $start++
      $end--
  }
  return $targetArray
}

Format-Range 2 6 # => 'a','b','g','f','e','d','c','h','i','j','k','l','m'
```

Powershell as a Tool:

Getting Help:

```powershell
# Find commands
Get-Command about_* # alias: gcm
Get-Command -Verb Add
Get-Alias ps
Get-Alias -Definition Get-Process

Get-Help ps | less # alias: help
ps | Get-Member # alias: gm

Show-Command Get-WinEvent # Display GUI to fill in the parameters

Update-Help # Run as admin
```

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

```powershell
# Calling external commands, executables, 
# and functions with the call operator.
# Exe paths with arguments passed or containing spaces can create issues.
C:\Program Files\dotnet\dotnet.exe
# The term 'C:\Program' is not recognized as a name of a cmdlet,
# function, script file, or executable program.
# Check the spelling of the name, or if a path was included, 
# verify that the path is correct and try again

"C:\Program Files\dotnet\dotnet.exe"
C:\Program Files\dotnet\dotnet.exe    # returns string rather than execute

&"C:\Program Files\dotnet\dotnet.exe --help"   # fail
&"C:\Program Files\dotnet\dotnet.exe" --help   # success
# Alternatively, you can use dot-sourcing here
."C:\Program Files\dotnet\dotnet.exe" --help   # success

# the call operator (&) is similar to Invoke-Expression, 
# but IEX runs in current scope.
# One usage of '&' would be to invoke a scriptblock inside of your script.
# Notice the variables are scoped
$i = 2
$scriptBlock = { $i=5; Write-Output $i }
& $scriptBlock # => 5
$i # => 2

invoke-expression ' $i=5; Write-Output $i ' # => 5
$i # => 5

# Alternatively, to preserve changes to public variables
# you can use "Dot-Sourcing". This will run in the current scope.
$x=1
&{$x=2};$x # => 1

.{$x=2};$x # => 2


# Remoting into computers is easy.
Enter-PSSession -ComputerName RemoteComputer

# Once remoted in, you can run commands as if you're local.
RemoteComputer\PS> Get-Process powershell

<#
Handles  NPM(K)    PM(K)      WS(K)     CPU(s)     Id  SI ProcessName                                             
-------  ------    -----      -----     ------     --  -- -----------                                             
   1096      44   156324     179068      29.92  11772   1 powershell                                              
    545      25    49512      49852             25348   0 powershell 
#>
RemoteComputer\PS> Exit-PSSession

<#
 Powershell is an incredible tool for Windows management and Automation.
 Let's take the following scenario:
 You have 10 servers.
 You need to check whether a service is running on all of them.
 You can RDP and log in, or PSSession to all of them, but why?
 Check out the following
#>

$serverList = @(
    'server1',
    'server2',
    'server3',
    'server4',
    'server5',
    'server6',
    'server7',
    'server8',
    'server9',
    'server10'
)

[scriptblock]$script = {
    Get-Service -DisplayName 'Task Scheduler'
}

foreach ($server in $serverList) {
    $cmdSplat = @{
        ComputerName  = $server
        JobName       = 'checkService'
        ScriptBlock   = $script
        AsJob         = $true
        ErrorAction   = 'SilentlyContinue'
    }
    Invoke-Command @cmdSplat | Out-Null
}

<#
 Here we've invoked jobs across many servers.
 We can now Receive-Job and see if they're all running.
 Now scale this up 100x as many servers :)
#>
```

Interesting Projects  

* [Channel9](https://channel9.msdn.com/Search?term=powershell%20pipeline#ch9Search&lang-en=en) PowerShell tutorials
* [KevinMarquette's Powershell Blog](https://powershellexplained.com/) Excellent blog that goes into great detail on Powershell
* [PSGet](https://github.com/psget/psget) NuGet for PowerShell
* [PSReadLine](https://github.com/lzybkr/PSReadLine/) A bash inspired readline implementation for PowerShell (So good that it now ships with Windows10 by default!)
* [Posh-Git](https://github.com/dahlbyk/posh-git/) Fancy Git Prompt (Recommended!)
* [Oh-My-Posh](https://github.com/JanDeDobbeleer/oh-my-posh) Shell customization similar to the popular Oh-My-Zsh on Mac
* [PSake](https://github.com/psake/psake) Build automation tool
* [Pester](https://github.com/pester/Pester) BDD Testing Framework
* [ZLocation](https://github.com/vors/ZLocation) Powershell `cd` that reads your mind
* [PowerShell Community Extensions](https://github.com/Pscx/Pscx)
* [More on the Powershell Pipeline Issue](https://github.com/PowerShell/PowerShell/issues/1908)
