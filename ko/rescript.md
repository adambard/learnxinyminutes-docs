---
name: ReScript
filename: rescript.res
contributors:
  - ["Seth Corker", "https://sethcorker.com"]
  - ["Danny Yang", "https://yangdanny97.github.io/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

ReScript는 효율적이고 사람이 읽을 수 있는 JavaScript로 컴파일되는 강력한 타입 언어입니다. 어떤 코드베이스 크기에도 확장 가능한 번개처럼 빠른 컴파일러 도구 체인이 함께 제공됩니다. ReScript는 OCaml과 Reason에서 파생되었으며, 타입 추론 및 패턴 매칭과 같은 멋진 기능과 초보자에게 친숙한 구문 및 JavaScript 생태계에 중점을 둡니다.

```javascript
/* 주석은 슬래시-별표로 시작하고 별표-슬래시로 끝납니다. */
// 한 줄 주석은 이중 슬래시로 시작합니다.

/*----------------------------------------------
 * 변수 및 함수 선언
 *----------------------------------------------
 * 변수 및 함수는 let 키워드를 사용하고 세미콜론으로 끝납니다.
 * `let` 바인딩은 불변입니다.
 */

let x = 5
/* - 타입을 추가하지 않았습니다. ReScript는 x가 int임을 추론합니다. */

/* 이와 같은 함수는 두 개의 인수를 받아 더합니다. */
let add = (a, b) => a + b
/* - 이것도 타입 주석이 필요 없습니다! */

/*----------------------------------------------
 * 타입 주석
 *----------------------------------------------
 * 대부분의 경우 타입을 명시적으로 주석을 달 필요는 없지만,
 * 필요한 경우 이름 뒤에 타입을 추가할 수 있습니다.
 */

/* 타입은 다음과 같이 명시적으로 작성할 수 있습니다. */
let x: int = 5

/* 이전의 add 함수도 명시적으로 주석을 달 수 있습니다. */
let add2 = (a: int, b: int): int => a + b

/* type 키워드를 사용하여 타입을 별칭으로 지정할 수 있습니다. */
type companyId = int
let myId: companyId = 101

/* Mutation은 ReScript에서 권장되지 않지만 필요한 경우 사용할 수 있습니다.
   let 바인딩을 변형해야 하는 경우 값은 `ref()`로 래핑되어야 합니다. */
let myMutableNumber = ref(120)

/* 값(ref 컨테이너가 아님)에 액세스하려면 `.contents`를 사용하십시오. */
let copyOfMyMutableNumber = myMutableNumber.contents

/* 새 값을 할당하려면 `:=` 연산자를 사용하십시오. */
myMutableNumber := 240

/*----------------------------------------------
 * 기본 타입 및 연산자
 *----------------------------------------------
 */

/* > 문자열 */

/* 문자열에는 큰따옴표를 사용하십시오. */
let greeting = "Hello world!"

/* 문자열은 여러 줄에 걸쳐 있을 수 있습니다. */
let aLongerGreeting = "저를 보세요,
저는 여러 줄 문자열입니다.
"

/* 유니코드에는 `를 사용하십시오. */
let world = `🌍`

/* ` 주석은 문자열 보간에도 사용됩니다. */
let helloWorld = `hello, ${world}`
/* 바인딩은 문자열로 변환되어야 합니다. */
let age = 10
let ageMsg = `저는 ${Int.toString(age)}살입니다.`


/* ++로 문자열 연결 */
let name = "John " ++ "Wayne"
let emailSubject = "Hi " ++ name ++ ", you're a valued customer"

/* > 문자 */

/* 문자 유형에는 단일 문자를 사용하십시오. */
let lastLetter = 'z'
/* - 문자는 유니코드 또는 UTF-8을 지원하지 않습니다. */

/* > 부울 */

/* 부울은 true 또는 false일 수 있습니다. */
let isLearning = true

true && false  /* - : bool = false  논리곱 */
true || true   /* - : bool = true   논리합  */
!true          /* - : bool = false  논리 부정 */

/* 보다 큼 `>`, 또는 크거나 같음 `>=` */
'a' > 'b' /* - bool : false */

/* 보다 작음 `<`, 또는 작거나 같음 `<=` */
1 < 5 /* - : bool = true */

/* 구조적 같음 */
"hello" == "hello" /* - : bool = true */

/* 참조적 같음 */
"hello" === "hello" /* - : bool = false */
/* - 두 개의 다른 "hello" 문자열 리터럴이기 때문에 false입니다. */

/* 구조적 같지 않음 */
lastLetter != 'a' /* -: bool = true */

/* 참조적 같지 않음 */
lastLetter !== lastLetter /* - : bool = false */

/* > 정수 */
/* 정수에 대한 수학 연산 수행 */

1 + 1          /* - : int = 2  */
25 - 11        /* - : int = 11 */
5 * 2 * 3      /* - : int = 30 */
8 / 2          /* - : int = 4  */

/* > 부동 소수점 */
/* 부동 소수점 연산자 뒤에는 점이 붙습니다. */

1.1 +. 1.5     /* - : float = 2.6  */
18.0 -. 24.5   /* - : float = -6.5 */
2.5 *. 2.0     /* - : float = 5.   */
16.0 /. 4.0    /* - : float = 4.   */

/* > 튜플
 * 튜플은 다음 속성을 가집니다.
  - 불변
  - 순서 지정
  - 생성 시 고정 크기
  - 이기종 (다른 유형의 값을 포함할 수 있음)
 튜플은 2개 이상의 값입니다. */

let teamMember = ("John", 25)

/* 값과 일치하는 타입 주석 */
let position2d: (float, float) = (9.0, 12.0)

/* 패턴 매칭은 관심 있는 값만 검색하는 훌륭한 도구입니다.
   y 값만 원한다면 `_`를 사용하여 값을 무시합니다. */
let (_, y) = position2d
y +. 1.0 /* - : float = 13. */

/* > 레코드 */

/* 레코드는 명시적인 유형을 가져야 합니다. */
type trainJourney = {
  destination: string,
  capacity: int,
  averageSpeed: float,
}

/* 유형이 선언되면 ReScript는 필요할 때마다 유형을 추론할 수 있습니다. */
let firstTrip = {destination: "London", capacity: 45, averageSpeed: 120.0}

/* 점 표기법을 사용하여 속성에 액세스합니다. */
let maxPassengers = firstTrip.capacity

/* 레코드 유형을 다른 파일에 정의하는 경우 Trips.res라는 파일에 trainJourney가 있었다면 파일 이름을 참조해야 합니다. */
let secondTrip: Trips.trainJourney = {
  destination: "Paris",
  capacity: 50,
  averageSpeed: 150.0,
}

/* 레코드는 기본적으로 불변입니다. */
/* 그러나 레코드의 내용은 스프레드 연산자를 사용하여 복사할 수 있습니다. */
let newTrip = {...secondTrip, averageSpeed: 120.0}

/* 레코드 속성은 `mutable` 키워드로 명시적으로 변경할 수 있습니다. */
type breakfastCereal = {
  name: string,
  mutable amount: int,
}

let tastyMuesli = {name: "Tasty Muesli TM", amount: 500}

tastyMuesli.amount = 200
/* - tastyMuesli의 양은 이제 200입니다. */

/* Punning은 중복 타이핑을 피하는 데 사용됩니다. */
let name = "Just As Good Muesli"
let justAsGoodMuesli = {name, amount: 500}
/* - justAsGoodMuesli.name은 이제 "Just As Good Muesli"이며, 이는
   { name: name, amount: 500 }과 동일합니다. */

/* > 변형
   상호 배타적인 상태는 변형으로 표현할 수 있습니다. */

type authType =
  | GitHub
  | Facebook
  | Google
  | Password
/* - 생성자는 대문자로 시작해야 합니다. */
/* - 레코드와 마찬가지로 변형은 다른 파일에 선언된 경우 이름을 지정해야 합니다. */

let userPreferredAuth = GitHub

/* 변형은 switch 문과 함께 사용하기에 좋습니다. */
let loginMessage =
  switch (userPreferredAuth) {
  | GitHub => "GitHub 자격 증명으로 로그인."
  | Facebook => "Facebook 계정으로 로그인."
  | Google => "Google 계정으로 로그인"
  | Password => "이메일과 비밀번호로 로그인."
  }

/* > 옵션
   옵션은 None 또는 Some('a)일 수 있으며, 여기서 'a는 유형입니다. */

let userId = Some(23)

/* switch는 두 가지 경우를 처리합니다. */
let alertMessage =
  switch (userId) {
  | Some(id) => "환영합니다. 귀하의 ID는" ++ string_of_int(id)
  | None => "계정이 없습니다!"
  }
/* - `None` 또는 `Some` 케이스가 누락되면 오류가 발생합니다. */

/* > 목록
  * 목록은 다음 속성을 가집니다.
   - 불변
   - 순서 지정
   - 항목을 앞에 추가하는 속도
   - 분할 속도

  * ReScript의 목록은 연결 목록입니다.
 */

/* 목록은 `list` 키워드로 선언되고 중괄호로 묶인 값으로 초기화됩니다. */
let userIds = list{1, 4, 8}

/* 유형은 list<'a>로 명시적으로 설정할 수 있으며, 여기서 'a는 유형입니다. */
type idList = list<int>
type attendanceList = list<string>

/* 목록은 불변입니다. */
/* 그러나 기존 목록에 추가된 요소를 사용하여 새 목록을 만들 수 있습니다. */
let newUserIds = list{101, 102, ...userIds}

/* > 배열
 * 배열은 다음 속성을 가집니다.
  - 변경 가능
  - 임의 접근 및 업데이트 속도 */

/* 배열은 `[`로 선언되고 `]`로 끝납니다. */
let languages = ["ReScript", "JavaScript", "OCaml"]

/*----------------------------------------------
 * 함수
 *----------------------------------------------
 */

/* ReScript 함수는 화살표 구문을 사용하며, 표현식이 반환됩니다. */
let signUpToNewsletter = email => "Thanks for signing up " ++ email

/* 다음과 같이 함수를 호출합니다. */
signUpToNewsletter("hello@ReScript.org")

/* 더 긴 함수에는 블록을 사용하십시오. */
let getEmailPrefs = email => {
  let message = "Update settings for " ++ email
  let prefs = ["Weekly News", "Daily Notifications"]

  (message, prefs)
}
/* - 최종 튜플은 암시적으로 반환됩니다. */

/* > 레이블이 지정된 인수 */

/* 인수는 ~ 기호로 레이블을 지정할 수 있습니다. */
let moveTo = (~x, ~y) => {
  /* x,y로 이동 */
  ()
}

moveTo(~x=7.0, ~y=3.5)

/* 레이블이 지정된 인수는 함수 내에서 사용되는 이름을 가질 수도 있습니다. */
let getMessage = (~message as msg) => "==" ++ msg ++ "=="

getMessage(~message="You have a message!")
/* - 호출자는 ~message를 지정하지만 내부적으로 함수는 이를 사용할 수 있습니다. */

/* 다음 함수도 명시적으로 유형이 선언되어 있습니다. */
let showDialog = (~message: string): unit => {
  () /* 대화 상자 표시 */
}

/* > 커링
   함수는 커링될 수 있으며 부분적으로 호출되어 쉽게 재사용할 수 있습니다.
   나머지 인수는 ...로 표시됩니다. */

let div = (denom, numr) => numr / denom
let divBySix = div(6, ...)
let divByTwo = div(2, ...)

div(3, 24)     /* - : int = 8  */
divBySix(128)  /* - : int = 21 */
divByTwo(10)   /* - : int = 5  */

/* > 선택적 레이블 인수 */

/* 선택적 레이블 인수에 `=?` 구문을 사용하십시오. */
let greetPerson = (~name, ~greeting=?) => {
  switch (greeting) {
  | Some(greet) => greet ++ " " ++ name
  | None => "Hi " ++ name
  }
}
/* - 세 번째 인수 `unit` 또는 `()`는 필수입니다. 이를 생략하면
   함수가 커링되어 greetPerson(~name="Kate")가 부분 함수를 생성합니다.
   이를 해결하려면 선언 및 호출 시 `unit`을 추가하십시오. */

/* 선택적 레이블 인수 없이 greetPerson 호출 */
greetPerson(~name="Kate")

/* 모든 인수로 greetPerson 호출 */
greetPerson(~name="Marco", ~greeting="How are you today,")

/* > 파이프 */
/* 함수는 파이프라인 연산자로 호출할 수 있습니다. */

/* 첫 번째 인수를 전달하려면 `->`를 사용하십시오(파이프-첫 번째). */
3->div(24)     /* - : int = 8 */
/* - 이것은 div(3, 24)와 동일합니다. */

36->divBySix   /* - : int = 6 */
/* - 이것은 divBySix(36)와 동일합니다. */

/* 파이프는 코드를 함께 연결하기 쉽게 만듭니다. */
let addOne = a => a + 1
let divByTwo = a => a / 2
let multByThree = a => a * 3

let pipedValue = 3->addOne->divByTwo->multByThree /* - : int = 6 */

/*----------------------------------------------
 * 제어 흐름 및 패턴 매칭
 *----------------------------------------------
 */

/* > If-else */
/* ReScript에서 `If`는 결과를 반환하는 표현식입니다. */

/* greeting은 "Good morning!"이 됩니다. */
let greeting = if (true) {"Good morning!"} else {"Hello!"}

/* else 분기가 없으면 표현식은 `unit` 또는 `()`를 반환합니다. */
if (false) {
  showDialog(~message="정말 떠나시겠습니까?")
}
/* - 결과가 `unit` 유형이므로 결과를 할당하려면 두 반환 유형이 동일해야 합니다. */

/* > 구조 분해 */
/* 데이터 구조에서 속성을 쉽게 추출합니다. */

let aTuple = ("Teacher", 101)

/* 튜플의 값을 추출할 수 있습니다. */
let (name, classNum) = aTuple

/* 레코드의 속성도 추출할 수 있습니다. */
type person = {
  firstName: string,
  age: int,
}
let bjorn = {firstName: "Bjorn", age: 28}

/* 변수 이름은 레코드 속성 이름과 일치해야 합니다. */
let {firstName, age} = bjorn

/* 그러나 다음과 같이 이름을 바꿀 수 있습니다. */
let {firstName: bName, age: bAge} = bjorn

let {firstName: cName, age: _} = bjorn

/* > Switch
   스위치를 사용한 패턴 매칭은 ReScript의 중요한 도구입니다.
   표현적이고 간결한 도구를 위해 구조 분해와 함께 사용할 수 있습니다. */

/* 간단한 목록을 가져옵니다. */
let firstNames = ["James", "Jean", "Geoff"]

/* 처리하려는 각 경우에 대해 이름을 패턴 일치시킬 수 있습니다. */
switch (firstNames) {
| [] => "이름 없음"
| [first] => "오직 " ++ first
| [first, second] => "두 개의 이름 " ++ first ++ "," ++ second
| [first, second, third] =>
  "세 개의 이름, " ++ first ++ ", " ++ second ++ ", " ++ third
| _ => "많은 이름"
}
/* - `_`는 끝에 있는 모든 것을 포괄하는 것으로, 값에 상관없이 모든 다른 경우와 일치합니다. */

/* > When 절 */

let isJohn = a => a == "John"
let maybeName = Some("John")

/* When은 간단한 스위치에 더 복잡한 논리를 추가할 수 있습니다. */
let aGreeting =
  switch (maybeName) {
  | Some(name) when isJohn(name) => "안녕하세요 존! 잘 지내세요?"
  | Some(name) => "안녕하세요 " ++ name ++ ", 환영합니다."
  | None => "인사할 사람이 없습니다."
  }

/* > 예외 */

/* 사용자 정의 예외 */
exception Under_Age

/* 함수 내에서 예외 발생 */
let driveToTown = (driver: person) =>
  if (driver.age >= 15) {
    "우리는 마을에 있습니다."
  } else {
    raise(Under_Age)
  }

let evan = {firstName: "Evan", age: 14}

/* 패턴 매치 on the exception Under_Age */
switch (driveToTown(evan)) {
| status => print_endline(status)
| exception Under_Age =>
  print_endline(evan.firstName ++ "은 운전하기에 너무 어립니다!")
}

/* 또는 try 블록을 사용할 수 있습니다. */
/* - ReScript 예외는 옵션으로 피할 수 있으며 거의 사용되지 않습니다. */
let messageToEvan =
  try {
    driveToTown(evan)
  } catch {
  | Under_Age => evan.firstName ++ "은 운전하기에 너무 어립니다!"
  }

/*----------------------------------------------
 * 객체
 *----------------------------------------------
 * 객체는 레코드 유형과 유사하지만 덜 엄격합니다.
 * 객체는 클래스와 유사합니다.
 */

/* 객체는 레코드처럼 유형이 지정될 수 있지만 속성 이름은 인용됩니다. */
type surfaceComputer = {
  "color": string,
  "capacity": int,
}
let surfaceBook: surfaceComputer = { "color": "blue", "capacity": 512 }

/* 객체는 유형이 필요하지 않습니다. */
let hamster = { "color": "brown", "age": 2 }

/* 객체 타이핑은 구조적이므로 필요한 필드가 있는 모든 객체를 허용하는 함수를 가질 수 있습니다. */
let getAge = animal => animal["age"]
getAge(hamster)
getAge({ "name": "Fido", "color": "silver", "age": 3 })
getAge({ "age": 5 })

/*----------------------------------------------
 * 모듈
 *----------------------------------------------
 * 모듈은 코드를 구성하고 네임스페이스를 제공하는 데 사용됩니다.
 * 각 파일은 기본적으로 모듈입니다.
 */

/* 모듈 생성 */
module Staff = {
  type role =
    | Delivery
    | Sales
    | Other
  type member = {
    name: string,
    role,
  }

  let getRoleDirectionMessage = staff =>
    switch (staff.role) {
    | Delivery => "진심으로 배달하십시오!"
    | Sales => "당신만이 할 수 있는 것처럼 판매하십시오!"
    | Other => "당신은 팀의 중요한 부분입니다!"
    }
}

/* 모듈은 점 표기법으로 액세스할 수 있습니다. */
let newEmployee: Staff.member = {name: "Laura", role: Staff.Delivery}

/* 모듈 이름을 사용하는 것이 번거로울 수 있으므로 `open`을 사용하여 모듈의 내용을 현재 범위로 열 수 있습니다. */
open Staff

let otherNewEmployee: member = {name: "Fred", role: Other}

/* 모듈은 `include` 키워드를 사용하여 확장할 수 있습니다. include는 새 모듈의 범위에 모듈의 내용을 복사합니다. */
module SpecializedStaff = {
  include Staff

  /* `member`가 포함되어 있으므로 명시적으로 참조할 필요가 없습니다. */
  let ceo: member = {name: "Reggie", role: Other}

  let getMeetingTime = staff =>
    switch (staff) {
    | Other => 11_15 /* - : int = 1115 밑줄은 서식 지정용입니다. */
    | _ => 9_30
    }
}
```

## 더 읽을거리

- [공식 ReScript 문서](https://rescript-lang.org/)
- [ReScript 사용해 보기 - 온라인 플레이그라운드](https://rescript-lang.org/try)