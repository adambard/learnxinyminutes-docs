---
name: ColdFusion
filename: learncoldfusion.cfm
contributors:
    - ["Wayne Boka", "http://wboka.github.io"]
    - ["Kevin Morris", "https://twitter.com/kevinmorris"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

ColdFusion은 웹 개발을 위한 스크립팅 언어입니다.
[여기에서 더 읽어보십시오.](http://www.adobe.com/products/coldfusion-family.html)

### CFML
_**C**old**F**usion **M**arkup **L**anguage_
ColdFusion은 태그 기반 언어로 시작했습니다. 거의 모든 기능은 태그를 사용하여 사용할 수 있습니다.

```cfm
<em>HTML 태그는 출력 가독성을 위해 제공되었습니다.</em>

<!--- 주석은 "<!---"로 시작하고 "--->"로 끝납니다. --->
<!--- 
    주석은
    여러 줄에
    걸쳐
    작성할 수 있습니다.
--->

<!--- CFML 태그는 HTML 태그와 유사한 형식을 가집니다. --->
<h1>단순 변수</h1>
<!--- 변수 선언: 변수는 JavaScript와 유사하게 느슨하게 타입이 지정됩니다. --->
<p><b>myVariable</b>을 "myValue"로 설정</p>
<cfset myVariable = "myValue" />
<p><b>myNumber</b>를 3.14로 설정</p>
<cfset myNumber = 3.14 />

<!--- 단순 데이터 표시 --->
<!--- 문자열, 숫자 및 표현식과 같은 단순 값에는 <cfoutput>을 사용합니다. --->
<p><b>myVariable</b> 표시: <cfoutput>#myVariable#</cfoutput></p><!--- myValue --->
<p><b>myNumber</b> 표시: <cfoutput>#myNumber#</cfoutput></p><!--- 3.14 --->

<hr />

<h1>복합 변수</h1>
<!--- 복합 변수 선언 --->
<!--- 1차원 배열 선언: 리터럴 또는 대괄호 표기법 --->
<p>리터럴 또는 대괄호 표기법을 사용하여 <b>myArray1</b>을 1차원 배열로 설정</p>
<cfset myArray1 = [] />
<!--- 1차원 배열 선언: 함수 표기법 --->
<p>함수 표기법을 사용하여 <b>myArray2</b>를 1차원 배열로 설정</p>
<cfset myArray2 = ArrayNew(1) />

<!--- 복합 변수 출력 --->
<p><b>myArray1</b> 내용</p>
<cfdump var="#myArray1#" /> <!--- 빈 배열 객체 --->
<p><b>myArray2</b> 내용</p>
<cfdump var="#myArray2#" /> <!--- 빈 배열 객체 --->

<!--- 연산자 --->
<!--- 산술 --->
<h1>연산자</h1>
<h2>산술</h2>
<p>1 + 1 = <cfoutput>#1 + 1#</cfoutput></p>
<p>10 - 7 = <cfoutput>#10 - 7#<br /></cfoutput></p>
<p>15 * 10 = <cfoutput>#15 * 10#<br /></cfoutput></p>
<p>100 / 5 = <cfoutput>#100 / 5#<br /></cfoutput></p>
<p>120 % 5 = <cfoutput>#120 % 5#<br /></cfoutput></p>
<p>120 mod 5 = <cfoutput>#120 mod 5#<br /></cfoutput></p>

<hr />

<!--- 비교 --->
<h2>비교</h2>
<h3>표준 표기법</h3>
<p>1은 1과 같습니까? <cfoutput>#1 eq 1#</cfoutput></p>
<p>15는 1과 같지 않습니까? <cfoutput>#15 neq 1#</cfoutput></p>
<p>10은 8보다 큽니까? <cfoutput>#10 gt 8#</cfoutput></p>
<p>1은 2보다 작습니까? <cfoutput>#1 lt 2#</cfoutput></p>
<p>10은 5보다 크거나 같습니까? <cfoutput>#10 gte 5#</cfoutput></p>
<p>1은 5보다 작거나 같습니까? <cfoutput>#1 lte 5#</cfoutput></p>

<h3>대체 표기법</h3>
<p>1은 1과 같습니까? <cfoutput>#1 eq 1#</cfoutput></p>
<p>15는 1과 같지 않습니까? <cfoutput>#15 neq 1#</cfoutput></p>
<p>10은 8보다 큽니까? <cfoutput>#10 gt 8#</cfoutput></p>
<p>1은 2보다 작습니까? <cfoutput>#1 lt 2#</cfoutput></p>
<p>10은 5보다 크거나 같습니까? <cfoutput>#10 gte 5#</cfoutput></p>
<p>1은 5보다 작거나 같습니까? <cfoutput>#1 lte 5#</cfoutput></p>

<hr />

<!--- 제어 구조 --->
<h1>제어 구조</h1>

<cfset myCondition = "Test" />

<p>테스트할 조건: "<cfoutput>#myCondition#</cfoutput>"</p>

<cfif myCondition eq "Test">
    <cfoutput>#myCondition#. 테스트 중입니다.</cfoutput>
<cfelseif myCondition eq "Production">
    <cfoutput>#myCondition#. 조심해서 진행하십시오!!!</cfoutput>
<cfelse>
    myCondition을 알 수 없습니다.
</cfif>

<hr />

<!--- 루프 --->
<h1>루프</h1>
<h2>For 루프</h2>
<cfloop from="0" to="10" index="i">
	<p>인덱스는 <cfoutput>#i#</cfoutput></p>
</cfloop>

<h2>For Each 루프 (복합 변수)</h2>

<p><b>myArray5</b>를 [5, 15, 99, 45, 100]으로 설정</p>

<cfset myArray5 = [5, 15, 99, 45, 100] />

<cfloop array="#myArray5#" index="i">
	<p>인덱스는 <cfoutput>#i#</cfoutput></p>
</cfloop>

<p><b>myArray4</b>를 ["Alpha", "Bravo", "Charlie", "Delta", "Echo"]로 설정</p>

<cfset myArray4 = ["Alpha", "Bravo", "Charlie", "Delta", "Echo"] />

<cfloop array="#myArray4#" index="s">
	<p>인덱스는 <cfoutput>#s#</cfoutput></p>
</cfloop>

<h2>Switch 문</h2>

<p><b>myArray5</b>를 [5, 15, 99, 45, 100]으로 설정</p>

<cfset myArray5 = [5, 15, 99, 45, 100] />

<cfloop array="#myArray5#" index="i">
	<cfswitch expression="#i#">
		<cfcase value="5,15,45" delimiters=",">
			<p><cfoutput>#i#</cfoutput>는 5의 배수입니다.</p>
		</cfcase>
		<cfcase value="99">
			<p><cfoutput>#i#</cfoutput>는 99입니다.</p>
		</cfcase>
		<cfdefaultcase>
			<p><cfoutput>#i#</cfoutput>는 5, 15, 45 또는 99가 아닙니다.</p>
		</cfdefaultcase>
	</cfswitch>
</cfloop>

<hr />

<h1>유형 변환</h1>

<style>
	table.table th, table.table td {
		border: 1px solid #000000;
		padding: 2px;
	}

	table.table th {
		background-color: #CCCCCC;
	}
</style>

<table class="table" cellspacing="0">
	<thead>
		<tr>
			<th>값</th>
			<th>부울로</th>
			<th>숫자로</th>
			<th>날짜-시간으로</th>
			<th>문자열로</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<th>"Yes"</th>
			<td>TRUE</td>
			<td>1</td>
			<td>오류</td>
			<td>"Yes"</td>
		</tr>
		<tr>
			<th>"No"</th>
			<td>FALSE</td>
			<td>0</td>
			<td>오류</td>
			<td>"No"</td>
		</tr>
		<tr>
			<th>TRUE</th>
			<td>TRUE</td>
			<td>1</td>
			<td>오류</td>
			<td>"Yes"</td>
		</tr>
		<tr>
			<th>FALSE</th>
			<td>FALSE</td>
			<td>0</td>
			<td>오류</td>
			<td>"No"</td>
		</tr>
		<tr>
			<th>숫자</th>
			<td>숫자가 0이 아니면 True; 그렇지 않으면 False.</td>
			<td>숫자</td>
			<td>이 장의 앞부분에 있는 "날짜-시간 값"을 참조하십시오.</td>
			<td>숫자의 문자열 표현(예: "8").</td>
		</tr>
		<tr>
			<th>문자열</th>
			<td>"Yes"이면 True <br>"No"이면 False <br>0으로 변환할 수 있으면 False <br>다른 숫자로 변환할 수 있으면 True</td>
			<td>숫자를 나타내는 경우(예: "1,000" 또는 "12.36E-12") 해당 숫자로 변환됩니다.</td>
			<td>날짜-시간을 나타내는 경우(다음 열 참조) 해당 날짜-시간 객체의 숫자 값으로 변환됩니다. <br>ODBC 날짜, 시간 또는 타임스탬프(예: "{ts '2001-06-14 11:30:13'}")이거나 전체 또는 약어 월 이름을 포함한 표준 미국 날짜 또는 시간 형식으로 표현된 경우 해당 날짜-시간 값으로 변환됩니다. <br>요일 또는 비정상적인 구두점은 오류를 발생시킵니다. <br>대시, 슬래시 및 공백은 일반적으로 허용됩니다.</td>
			<td>문자열</td>
		</tr>
		<tr>
			<th>날짜</th>
			<td>오류</td>
			<td>날짜-시간 객체의 숫자 값.</td>
			<td>날짜</td>
			<td>ODBC 타임스탬프.</td>
		</tr>
	</tbody>
</table>

<hr />

<h1>구성 요소</h1>

<em>참조용 코드 (함수는 IE를 지원하기 위해 무언가를 반환해야 합니다)</em>
```

```cfs
<cfcomponent>
	<cfset this.hello = "Hello" />
	<cfset this.world = "world" />

	<cffunction name="sayHello">
		<cfreturn this.hello & ", " & this.world & "!" />
	</cffunction>

	<cffunction name="setHello">
		<cfargument name="newHello" type="string" required="true" />

		<cfset this.hello = arguments.newHello />

		<cfreturn true />
	</cffunction>

	<cffunction name="setWorld">
		<cfargument name="newWorld" type="string" required="true" />

		<cfset this.world = arguments.newWorld />

		<cfreturn true />
	</cffunction>

	<cffunction name="getHello">
		<cfreturn this.hello />
	</cffunction>

	<cffunction name="getWorld">
		<cfreturn this.world />
	</cffunction>
</cfcomponent>

<cfset this.hello = "Hello" />
<cfset this.world = "world" />

<cffunction name="sayHello">
	<cfreturn this.hello & ", " & this.world & "!" />
</cffunction>

<cffunction name="setHello">
	<cfargument name="newHello" type="string" required="true" />

	<cfset this.hello = arguments.newHello />

	<cfreturn true />
</cffunction>

<cffunction name="setWorld">
	<cfargument name="newWorld" type="string" required="true" />

	<cfset this.world = arguments.newWorld />

	<cfreturn true />
</cffunction>

<cffunction name="getHello">
	<cfreturn this.hello />
</cffunction>

<cffunction name="getWorld">
	<cfreturn this.world />
</cffunction>


<b>sayHello()</b>
<cfoutput><p>#sayHello()#</p></cfoutput>
<b>getHello()</b>
<cfoutput><p>#getHello()#</p></cfoutput>
<b>getWorld()</b>
<cfoutput><p>#getWorld()#</p></cfoutput>
<b>setHello("Hola")</b>
<cfoutput><p>#setHello("Hola")#</p></cfoutput>
<b>setWorld("mundo")</b>
<cfoutput><p>#setWorld("mundo")#</p></cfoutput>
<b>sayHello()</b>
<cfoutput><p>#sayHello()#</p></cfoutput>
<b>getHello()</b>
<cfoutput><p>#getHello()#</p></cfoutput>
<b>getWorld()</b>
<cfoutput><p>#getWorld()#</p></cfoutput>
```

### CFScript
_**C**old**F**usion **S**cript_
최근 몇 년 동안 ColdFusion 언어는 태그 기능을 반영하는 스크립트 구문을 추가했습니다. 최신 CF 서버를 사용하는 경우 거의 모든 기능을 스크립트 구문을 사용하여 사용할 수 있습니다.

## 더 읽을거리

아래 제공된 링크는 주제를 이해하기 위한 것이므로, Google에서 특정 예제를 자유롭게 검색하십시오.

1. [Adobe의 Coldfusion 참조](https://helpx.adobe.com/coldfusion/cfml-reference/topics.html)
2. [오픈 소스 문서](http://cfdocs.org/)
