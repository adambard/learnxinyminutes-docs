# powershell.md (번역)

---
name: PowerShell
contributors:
    - ["Wouter Van Schandevijl", "https://github.com/laoujin"]
    - ["Andrew Ryan Davis", "https://github.com/AndrewDavis1191"]
filename: LearnPowershell.ps1
---

PowerShell은 .NET Framework를 기반으로 구축된 Microsoft의 Windows 스크립팅 언어 및 구성 관리 프레임워크입니다. Windows 7 이상에는 PowerShell이 함께 제공됩니다.
아래의 거의 모든 예제는 셸 스크립트의 일부가 되거나 셸에서 직접 실행될 수 있습니다.

Bash와의 주요 차이점은 일반 텍스트가 아닌 대부분 객체를 조작한다는 것입니다. 수년간의 발전을 거쳐 Python과 약간 유사해졌습니다.

[여기에서 더 읽어보세요.](https://docs.microsoft.com/powershell/scripting/overview)

언어로서의 PowerShell:

```powershell
# 한 줄 주석은 숫자 기호로 시작합니다.

<#
  여러 줄 주석은
  이와 같습니다
#>


####################################################
## 1. 기본 데이터 타입 및 연산자
####################################################

# 숫자
3 # => 3

# 수학
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# PowerShell은 뱅커스 라운딩을 사용합니다.
# 즉, [int]1.5는 2로 반올림되지만 [int]2.5도 마찬가지입니다.
# 나눗셈은 항상 부동 소수점을 반환합니다.
# 반올림하려면 결과를 [int]로 캐스팅해야 합니다.
[int]5 / [int]3       # => 1.66666666666667
[int]-5 / [int]3      # => -1.66666666666667
5.0 / 3.0   # => 1.66666666666667
-5.0 / 3.0  # => -1.66666666666667
[int]$result = 5 / 3
$result # => 2

# 나머지 연산
7 % 3  # => 1

# 거듭제곱은 긴 형식이나 내장 [Math] 클래스가 필요합니다.
[Math]::Pow(2,3)  # => 8

# 괄호로 연산 순서를 강제합니다.
1 + 3 * 2  # => 7
(1 + 3) * 2  # => 8

# 불리언 값은 기본 타입입니다 (참고: $)
$True  # => True
$False  # => False

# !로 부정
!$True   # => False
!$False  # => True

# 불리언 연산자
# "-and" 및 "-or" 사용법 참고
$True -and $False  # => False
$False -or $True   # => True

# True와 False는 실제로 1과 0이지만 제한된 산술만 지원합니다.
# 그러나 bool을 int로 캐스팅하면 이 문제가 해결됩니다.
$True + $True # => 2
$True * 8    # => '[System.Boolean] * [System.Int32]'는 정의되지 않았습니다
[int]$True * 8 # => 8
$False - 5   # => -5

# 비교 연산자는 True와 False의 숫자 값을 봅니다.
0 -eq $False  # => True
1 -eq $True   # => True
2 -eq $True   # => False
-5 -ne $False # => True

# 불리언 논리 연산자를 int에 사용하면 평가를 위해 불리언으로 캐스팅됩니다.
# 그러나 캐스팅되지 않은 값이 반환됩니다.
# bool(ints)와 비트 -band/-bor를 혼동하지 마십시오.
[bool](0)     # => False
[bool](4)     # => True
[bool](-6)    # => True
0 -band 2     # => 0
-5 -bor 0     # => -5

# 같음은 -eq (equals)입니다.
1 -eq 1  # => True
2 -eq 1  # => False

# 같지 않음은 -ne (notequals)입니다.
1 -ne 1  # => False
2 -ne 1  # => True

# 더 많은 비교
1 -lt 10  # => True
1 -gt 10  # => False
2 -le 2  # => True
2 -ge 2  # => True

# 값이 범위에 있는지 확인
1 -lt 2 -and 2 -lt 3  # => True
2 -lt 3 -and 3 -lt 2  # => False

# (-is vs. -eq) -is는 두 객체가 동일한 타입인지 확인합니다.
# -eq는 객체가 동일한 값을 갖는지 확인하지만 때로는
# 예상대로 작동하지 않습니다.
# 참고: 이전에 네임스페이스 없이 .NET에서 [Math]를 호출했습니다.
# 원하는 경우 [Collections.ArrayList]에서도 동일하게 수행할 수 있습니다.
[System.Collections.ArrayList]$a = @()  # a를 새 목록으로 지정
$a = (1,2,3,4)
$b = $a                                 # => b를 a가 가리키는 것으로 지정
$b -is $a.GetType()                     # => True, a와 b는 동일한 타입
$b -eq $a                               # => 없음! 아래 참조
[System.Collections.Hashtable]$b = @{}  # => b를 새 해시 테이블로 지정
$b = @{'one' = 1
       'two' = 2}
$b -is $a.GetType()                     # => False, a와 b 타입이 같지 않음

# 문자열은 " 또는 '로 생성되지만 문자열 보간에는 "가 필요합니다.
"This is a string."
'This is also a string.'

# 문자열도 추가할 수 있습니다! 하지만 이렇게 하지 마십시오.
"Hello " + "world!"  # => "Hello world!"

# 문자열은 문자 목록처럼 처리할 수 있습니다.
"Hello world!"[0]  # => 'H'

# 문자열의 길이를 찾을 수 있습니다.
("This is a string").Length  # => 16

# f-문자열 또는 서식화된 문자열 리터럴을 사용하여 서식을 지정할 수도 있습니다.
$name = "Steve"
$age = 22
"He said his name is $name."
# => "He said his name is Steve"
"{0} said he is {1} years old." -f $name, $age
# => "Steve said he is 22 years old"
"$name's name is $($name.Length) characters long."
# => "Steve's name is 5 characters long."

# 문자열은 -eq로 비교할 수 있지만 대소문자를 구분하지 않습니다. -ceq 또는 -ieq로
# 강제할 수 있습니다.
"ab" -eq "ab"  # => True
"ab" -eq "AB"  # => True!
"ab" -ceq "AB"  # => False
"ab" -ieq "AB"  # => True

# Powershell의 이스케이프 문자
# 많은 언어에서 '\'를 사용하지만 Windows는 이 문자를
# 파일 경로에 사용합니다. 따라서 Powershell은 '`'를 사용하여 문자를 이스케이프합니다.
# 파일 작업 시 '`'는 NTFS 파일 이름에서 유효한 문자이므로
# 주의하십시오.
"Showing`nEscape Chars" # => Showing과 Escape 사이에 새 줄
"Making`tTables`tWith`tTabs" # => 탭으로 서식 지정

# 주석을 방지하기 위해 파운드 기호 부정
# '#'의 기능은 제거되지만 '#'는 여전히 존재합니다.
`#Get-Process # => 실패: 인식할 수 없는 cmdlet

# $null은 객체가 아닙니다.
$null  # => 없음

# $null, 0, 빈 문자열 및 배열은 모두 False로 평가됩니다.
# 다른 모든 값은 True입니다.
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
# *[]는 .NET 클래스를 호출합니다. 함수에 전달될 때 '[]' 문자열을 생성합니다.
Test-Value ({})    # => True
Test-Value @()     # => False


####################################################
## 2. 변수 및 컬렉션
####################################################

# Powershell은 "Write-Output" 함수를 사용하여 출력합니다.
Write-Output "I'm Posh. Nice to meet you!"  # => I'm Posh. Nice to meet you!

# 콘솔에서 입력 데이터를 간단하게 가져오는 방법
$userInput = Read-Host "Enter some data: " # 데이터를 문자열로 반환

# 선언은 없고 할당만 있습니다.
# 관례는 팀에서 사용하는 camelCase 또는 PascalCase를 사용하는 것입니다.
$someVariable = 5
$someVariable  # => 5

# 이전에 할당되지 않은 변수에 액세스해도 예외가 발생하지 않습니다.
# 기본값은 $null입니다.

# 삼항 연산자는 Powershell 7 이상에 존재합니다.
0 ? 'yes' : 'no'  # => no


# Powershell의 기본 배열 객체는 고정 길이 배열입니다.
$defaultArray = "thing","thing2","thing3"
# '+='로 객체를 추가할 수 있지만 객체를 제거할 수는 없습니다.
$defaultArray.Add("thing4") # => 예외 "컬렉션의 크기가 고정되었습니다."
# 더 작업하기 쉬운 배열을 원한다면 .NET [ArrayList] 클래스를 원할 것입니다.
# ArrayList가 훨씬 빠르다는 점도 주목할 가치가 있습니다.

# ArrayList는 시퀀스를 저장합니다.
[System.Collections.ArrayList]$array = @()
# 미리 채워진 ArrayList로 시작할 수 있습니다.
[System.Collections.ArrayList]$otherArray = @(5, 6, 7, 8)

# 'Add'로 목록 끝에 추가 (참고: 출력을 생성하므로 $null에 추가)
$array.Add(1) > $null    # $array는 이제 [1]입니다.
$array.Add(2) > $null    # $array는 이제 [1, 2]입니다.
$array.Add(4) > $null    # $array는 이제 [1, 2, 4]입니다.
$array.Add(3) > $null    # $array는 이제 [1, 2, 4, 3]입니다.
# 객체 수-1의 인덱스로 끝에서 제거; 배열 인덱스는 0부터 시작
$array.RemoveAt($array.Count-1) # => 3이고 배열은 이제 [1, 2, 4]입니다.
# 다시 넣자
$array.Add(3) > $null   # 배열은 이제 다시 [1, 2, 4, 3]입니다.

# 다른 배열처럼 목록에 액세스
$array[0]   # => 1
# 마지막 요소 보기
$array[-1]  # => 3
# 경계를 벗어나면 아무것도 반환하지 않음
$array[4]  # 빈 줄 반환

# 배열에서 요소 제거
$array.Remove($array[3])  # $array는 이제 [1, 2, 4]입니다.

# 인덱스에 요소 삽입
$array.Insert(2, 3)  # $array는 이제 [1, 2, 3, 4]입니다.

# 인자와 일치하는 첫 번째 항목의 인덱스 가져오기
$array.IndexOf(2)  # => 1
$array.IndexOf(6)  # "배열 외부"로 -1 반환

# 배열을 추가할 수 있습니다.
# 참고: $array 및 $otherArray의 값은 수정되지 않습니다.
$array + $otherArray  # => [1, 2, 3, 4, 5, 6, 7, 8]

# "AddRange()"로 배열 연결
$array.AddRange($otherArray)  # 이제 $array는 [1, 2, 3, 4, 5, 6, 7, 8]입니다.

# "in"으로 배열에 존재 여부 확인
1 -in $array  # => True

# "Count"로 길이 검사 (참고: arrayList의 "Length" = 각 항목의 길이)
$array.Count  # => 8

# 슬라이스 구문으로 범위를 볼 수 있습니다.
$array[1,3,5]     # 선택한 인덱스 반환  => [2, 4, 6]
$array[1..3]      # 인덱스 1에서 3까지 반환 => [2, 3, 4]
$array[-3..-1]    # 마지막 3에서 마지막 1까지 반환 => [6, 7, 8]
$array[-1..-3]    # 마지막 1에서 마지막 3까지 반환 => [8, 7, 6]
$array[2..-1]     # 인덱스 2에서 마지막까지 반환 (대부분이 예상하는 것과 다름) => [3, 2, 1, 8]
$array[0,2+4..6]  # +로 여러 범위 반환 => [1, 3, 5, 6, 7]

# -eq는 배열을 비교하지 않고 일치하는 요소를 추출합니다.
$array = 1,2,3,1,1
$array -eq 1          # => 1,1,1
($array -eq 1).Count  # => 3

# 튜플은 배열과 같지만 불변입니다.
# powershell에서 튜플을 사용하려면 .NET 튜플 클래스를 사용해야 합니다.
$tuple = [System.Tuple]::Create(1, 2, 3)
$tuple.Item(0)      # => 1
$tuple.Item(0) = 3  # TypeError 발생

# 튜플에서 일부 배열 메서드를 수행할 수 있지만 제한적입니다.
$tuple.Length       # => 3
$tuple + (4, 5, 6)  # => 예외
$tuple[0..2]        # => $null (powershell 5에서)    => [1, 2, 3] (powershell 7에서)
2 -in $tuple        # => False


# 해시 테이블은 키에서 값으로의 매핑을 저장하며, 딕셔너리와 유사하지만 다릅니다.
# 해시 테이블은 배열처럼 항목 순서를 유지하지 않습니다.
$emptyHash = @{}
# 미리 채워진 해시 테이블
$filledHash = @{"one"= 1
                "two"= 2
                "three"= 3}

# []로 값 조회
$filledHash["one"]  # => 1

# ".Keys"로 모든 키를 반복 가능한 객체로 가져오기
$filledHash.Keys  # => ["one", "two", "three"]

# ".Values"로 모든 값을 반복 가능한 객체로 가져오기
$filledHash.Values  # => [1, 2, 3]

# "-in"으로 해시에 키 또는 값 존재 여부 확인
"one" -in $filledHash.Keys  # => True
1 -in $filledHash.Values    # => False (powershell 5에서)    => True (powershell 7에서)

# 존재하지 않는 키를 조회하면 $null 반환
$filledHash["four"]  # $null

# 해시 테이블에 추가
$filledHash.Add("five",5)  # $filledHash["five"]가 5로 설정됨
$filledHash.Add("five",6)  # 예외 "키 "five"를 가진 항목이 이미 추가되었습니다."
$filledHash["four"] = 4    # $filledHash["four"]가 4로 설정됨, 다시 실행해도 아무것도 안 함

# 해시 테이블에서 키 제거
$filledHash.Remove("one") # filled 해시 테이블에서 "one" 키 제거


####################################################
## 3. 제어 흐름 및 반복 가능 객체
####################################################

# 변수를 만들어 봅시다
$someVar = 5

# if 문입니다.
# "$someVar is smaller than 10"을 출력합니다.
if ($someVar -gt 10) {
    Write-Output "$someVar is bigger than 10."
}
elseif ($someVar -lt 10) {    # 이 elseif 절은 선택 사항입니다.
    Write-Output "$someVar is smaller than 10."
}
else {                        # 이것도 선택 사항입니다.
    Write-Output "$someVar is indeed 10."
}


<#
Foreach 루프는 배열을 반복합니다.
출력:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
#>
foreach ($animal in ("dog", "cat", "mouse")) {
    # -f를 사용하여 서식화된 문자열을 보간할 수 있습니다.
    "{0} is a mammal" -f $animal
}

<#
For 루프는 배열을 반복하며 인덱스를 지정할 수 있습니다.
출력:
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
While 루프는 조건이 더 이상 충족되지 않을 때까지 계속됩니다.
출력:
    0
    1
    2
    3
#>
$x = 0
while ($x -lt 4) {
    Write-Output $x
    $x += 1  # x = x + 1의 약어
}

# Switch 문은 대부분의 언어에 비해 더 강력합니다.
$val = "20"
switch($val) {
  { $_ -eq 42 }           { "The answer equals 42"; break }
  '20'                    { "Exactly 20"; break }
  { $_ -like 's*' }       { "Case insensitive"; break }
  { $_ -clike 's*'}       { "clike, ceq, cne for case sensitive"; break }
  { $_ -notmatch '^.*$'}  { "Regex matching. cnotmatch, cnotlike, ..."; break }
  default                 { "Others" }
}

# try/catch 블록으로 예외 처리
try {
    # "throw"를 사용하여 오류 발생
    throw "This is an error"
}
catch {
    Write-Output $Error.ExceptionMessage
}
finally {
    Write-Output "We can clean up resources here"
}


# 파일에 쓰기
$contents = @{"aa"= 12
             "bb"= 21}
$contents | Export-CSV "$env:HOMEDRIVE\file.csv" # 파일에 쓰기

$contents = "test string here"
$contents | Out-File "$env:HOMEDRIVE\file.txt" # 다른 파일에 쓰기

# 파일 내용 읽고 json으로 변환
Get-Content "$env:HOMEDRIVE\file.csv" | ConvertTo-Json


####################################################
## 4. 함수
####################################################

# "function"을 사용하여 새 함수 생성
# 함수에 대한 동사-명사 명명 규칙 유지
function Add-Numbers {
 $args[0] + $args[1]
}

Add-Numbers 1 2 # => 3

# 매개변수로 함수 호출
function Add-ParamNumbers {
 param( [int]$firstNumber, [int]$secondNumber )
 $firstNumber + $secondNumber
}

Add-ParamNumbers -FirstNumber 1 -SecondNumber 2 # => 3

# 명명된 매개변수, 매개변수 속성, 구문 분석 가능한 문서가 있는 함수
<#
.SYNOPSIS
새 웹사이트 설정
.DESCRIPTION
새 웹사이트에 필요한 모든 것을 만들어 많은 승리를 거둡니다.
.PARAMETER siteName
새 웹사이트의 이름
.EXAMPLE
New-Website -Name FancySite -Po 5000
New-Website SiteWithDefaultPort
New-Website siteName 2000 # 오류! 포트 인수를 확인할 수 없습니다.
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
## 5. 모듈
####################################################

# 모듈을 가져오고 모듈을 설치할 수 있습니다.
# Install-Module은 pip 또는 npm과 유사하며 Powershell Gallery에서 가져옵니다.
Install-Module dbaTools
Import-Module dbaTools

$query = "SELECT * FROM dbo.sometable"
$queryParams = @{
    SqlInstance = 'testInstance'
    Database    = 'testDatabase'
    Query       = $query
}
Invoke-DbaQuery @queryParams

# 모듈에서 특정 함수를 가져올 수 있습니다.
Import-Module -Function Invoke-DbaQuery


# Powershell 모듈은 일반 Posh 파일일 뿐입니다.
# 직접 작성하고 가져올 수 있습니다. 모듈의 이름은
# 파일 이름과 동일합니다.

# 모듈에 정의된 함수와 속성을
# 찾을 수 있습니다.
Get-Command -module dbaTools
Get-Help dbaTools -Full


####################################################
## 6. 클래스
####################################################

# "class" 문을 사용하여 클래스를 만듭니다.
class Instrument {
    [string]$Type
    [string]$Family
}

$instrument = [Instrument]::new()
$instrument.Type = "String Instrument"
$instrument.Family = "Plucked String"

$instrument

<# 출력:
Type              Family
----              ------
String Instrument Plucked String
#>


####################################################
## 6.1 상속
####################################################

# 상속을 통해 부모 클래스에서 메서드와 변수를
# 상속하는 새로운 자식 클래스를 정의할 수 있습니다.

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
## 7. 고급
####################################################

# powershell 파이프라인은 고차 함수와 같은 것을 허용합니다.

# Group-Object는 놀라운 일을 하는 편리한 cmdlet입니다.
# SQL의 GROUP BY와 매우 유사하게 작동합니다.

<#
 다음은 실행 중인 모든 프로세스를 가져와서
 이름으로 그룹화하고,
 각 프로세스의 실행 중인 인스턴스 수를 알려줍니다.
 팁: Chrome과 svcHost는 일반적으로 이 점에서 큰 숫자입니다.
#>
Get-Process | Foreach-Object ProcessName | Group-Object

# 유용한 파이프라인 예제는 반복 및 필터링입니다.
1..10 | ForEach-Object { "Loop number $PSITEM" }
1..10 | Where-Object { $PSITEM -gt 5 } | ConvertTo-Json

# 파이프라인의 주목할 만한 단점은 다른 옵션에 비해
# 성능이 떨어진다는 것입니다.
# 또한 원시 바이트는 파이프라인을 통과하지 않으므로
# 이미지를 전달하면 몇 가지 문제가 발생합니다.
# 자세한 내용은 하단 링크를 참조하십시오.

<#
 비동기 함수는 작업의 형태로 존재합니다.
 일반적으로 절차적 언어인
 Powershell은 작업으로 호출될 때 비차단 함수를 작동할 수 있습니다.
#>

# 이 함수는 최적화되지 않아 느린 것으로 알려져 있습니다.
$installedApps = Get-CimInstance -ClassName Win32_Product

# 스크립트가 있다면 이 함수에서 일정 시간 동안 멈출 것입니다.
$scriptBlock = {Get-CimInstance -ClassName Win32_Product}
Start-Job -ScriptBlock $scriptBlock

# 이것은 명령을 실행하는 백그라운드 작업을 시작합니다.
# 그런 다음 작업의 상태와 반환된 결과를 얻을 수 있습니다.
$allJobs = Get-Job
$jobResponse = Get-Job | Receive-Job


# 수학은 powershell에 내장되어 있으며 많은 함수가 있습니다.
$r=2
$pi=[math]::pi
$r2=[math]::pow( $r, 2 )
$area = $pi*$r2
$area

# 모든 가능성을 보려면 멤버를 확인하십시오.
[System.Math] | Get-Member -Static -MemberType All


<#
 이것은 어리석은 것입니다:
 언젠가 $start와 $end를 받아
 원래 배열을 변경하지 않고 임의의 배열을 기반으로 주어진 범위 내의
 모든 것을 뒤집을 수 있는 함수를 만들라는 요청을 받을 수 있습니다.
 그것을 하는 한 가지 방법을 보고 다른 데이터 구조를 소개합시다.
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

# 이전 방법은 작동하지만 새 배열을 할당하여 추가 메모리를 사용합니다.
# 또한 다소 깁니다.
# 새 배열을 할당하지 않고 이 작업을 수행하는 방법을 봅시다.
# 이것은 또한 약간 더 빠릅니다.

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

도구로서의 Powershell:

도움말 얻기:

```powershell
# 명령어 찾기
Get-Command about_* # 별칭: gcm
Get-Command -Verb Add
Get-Alias ps
Get-Alias -Definition Get-Process

Get-Help ps | less # 별칭: help
ps | Get-Member # 별칭: gm

Show-Command Get-WinEvent # 매개변수를 채우기 위한 GUI 표시

Update-Help # 관리자로 실행
```

환경이 확실하지 않은 경우:

```powershell
Get-ExecutionPolicy -List
Set-ExecutionPolicy AllSigned
# 실행 정책에는 다음이 포함됩니다:
# - Restricted: 스크립트가 실행되지 않습니다.
# - RemoteSigned: 신뢰할 수 있는 게시자가 서명한 경우에만 다운로드한 스크립트 실행
# - AllSigned: 스크립트는 신뢰할 수 있는 게시자가 서명해야 합니다.
# - Unrestricted: 모든 스크립트 실행
help about_Execution_Policies # 자세한 정보

# 현재 PowerShell 버전:
$PSVersionTable
```

```powershell
# 외부 명령어, 실행 파일,
# 및 호출 연산자가 있는 함수 호출.
# 인수가 전달되거나 공백이 포함된 Exe 경로는 문제를 일으킬 수 있습니다.
C:\Program Files\dotnet\dotnet.exe
# 'C:\Program'이라는 용어는 cmdlet,
# 함수, 스크립트 파일 또는 실행 프로그램의 이름으로 인식되지 않습니다.
# 이름의 철자를 확인하거나 경로가 포함된 경우
# 경로가 올바른지 확인하고 다시 시도하십시오.

"C:\Program Files\dotnet\dotnet.exe"
C:\Program Files\dotnet\dotnet.exe    # 실행 대신 문자열 반환

&"C:\Program Files\dotnet\dotnet.exe --help"   # 실패
&"C:\Program Files\dotnet\dotnet.exe" --help   # 성공
# 또는 여기서 점 소싱을 사용할 수 있습니다.
."C:\Program Files\dotnet\dotnet.exe" --help   # 성공

# 호출 연산자(&)는 Invoke-Expression과 유사하지만,
# IEX는 현재 범위에서 실행됩니다.
# '&'의 한 가지 용도는 스크립트 블록을 스크립트 내에서 호출하는 것입니다.
# 변수가 범위 지정되었는지 확인하십시오.
$i = 2
$scriptBlock = { $i=5; Write-Output $i }
& $scriptBlock # => 5
$i # => 2

invoke-expression ' $i=5; Write-Output $i ' # => 5
$i # => 5

# 또는 공용 변수에 대한 변경 사항을 유지하려면
# "점 소싱"을 사용할 수 있습니다. 이것은 현재 범위에서 실행됩니다.
$x=1
&{$x=2};$x # => 1

.{$x=2};$x # => 2


# 컴퓨터에 원격으로 접속하는 것은 쉽습니다.
Enter-PSSession -ComputerName RemoteComputer

# 원격으로 접속하면 로컬인 것처럼 명령을 실행할 수 있습니다.
RemoteComputer\PS> Get-Process powershell

<#
Handles  NPM(K)    PM(K)      WS(K)     CPU(s)     Id  SI ProcessName
-------  ------    -----      -----     ------     --  -- -----------
   1096      44   156324     179068      29.92  11772   1 powershell
    545      25    49512      49852             25348   0 powershell
#>
RemoteComputer\PS> Exit-PSSession

<#
 PowerShell은 Windows 관리 및 자동화를 위한 놀라운 도구입니다.
 다음 시나리오를 살펴보겠습니다.
 10개의 서버가 있습니다.
 모든 서버에서 서비스가 실행 중인지 확인해야 합니다.
 RDP로 로그인하거나 모든 서버에 PSSession으로 접속할 수 있지만 왜 그래야 할까요?
 다음을 확인하십시오.
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
 여기서는 여러 서버에서 작업을 호출했습니다.
 이제 Receive-Job을 사용하여 모두 실행 중인지 확인할 수 있습니다.
 이제 서버 수를 100배로 확장해 봅시다 :)
#>
```

흥미로운 프로젝트

* [Channel9](https://channel9.msdn.com/Search?term=powershell%20pipeline#ch9Search&lang-en=en) PowerShell 튜토리얼
* [KevinMarquette의 Powershell 블로그](https://powershellexplained.com/) PowerShell에 대해 자세히 설명하는 훌륭한 블로그
* [PSGet](https://github.com/psget/psget) PowerShell용 NuGet
* [PSReadLine](https://github.com/lzybkr/PSReadLine/) PowerShell용 bash에서 영감을 받은 readline 구현 (Windows10에 기본적으로 제공될 정도로 좋음!)
* [Posh-Git](https://github.com/dahlbyk/posh-git/) 멋진 Git 프롬프트 (권장!)
* [Oh-My-Posh](https://github.com/JanDeDobbeleer/oh-my-posh) Mac의 인기 있는 Oh-My-Zsh와 유사한 셸 사용자 지정
* [PSake](https://github.com/psake/psake) 빌드 자동화 도구
* [Pester](https://github.com/pester/Pester) BDD 테스트 프레임워크
* [ZLocation](https://github.com/vors/ZLocation) 마음을 읽는 Powershell `cd`
* [PowerShell 커뮤니티 확장](https://github.com/Pscx/Pscx)
* [Powershell 파이프라인 문제에 대한 추가 정보](https://github.com/PowerShell/PowerShell/issues/1908)
