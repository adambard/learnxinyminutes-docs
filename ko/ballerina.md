---
name: Ballerina
contributors:
    - ["Anjana Fernando", "https://github.com/lafernando"]
filename: learn_ballerina.bal
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Ballerina](https://ballerina.io/)는 클라우드 개발을 즐거운 경험으로 만들기 위한 정적 타입 프로그래밍 언어입니다.

```java
// 한 줄 주석

// 현재 소스 파일로 모듈 가져오기
import ballerina/io;
import ballerina/time;
import ballerina/http;
import ballerinax/java.jdbc;
import ballerina/lang.'int as ints;
import ballerinax/awslambda;
// 모듈 별칭 "af"는 전체 모듈 이름 대신 코드에서 사용됩니다.
import ballerinax/azure.functions as af;

http:Client clientEP = new ("https://freegeoip.app/");
jdbc:Client accountsDB = new ({url: "jdbc:mysql://localhost:3306/AccountsDB",
                               username: "test", password: "test"});

// 서비스는 Ballerina에서 일급 개념이며 Ballerina 프로그램의 진입점 중 하나입니다.
// Ballerina 플랫폼은 또한 Kubernetes와 같은 환경에 쉽게 배포할 수 있도록 지원합니다(https://ballerina.io/learn/deployment/kubernetes/).
service geoservice on new http:Listener(8080) {

    @http:ResourceConfig {
        path: "/geoip/{ip}"
    }
    resource function geoip(http:Caller caller, http:Request request,
                            string ip) returns @tainted error? {
        http:Response resp = check clientEP->get("/json/" + <@untainted>ip);
        check caller->respond(<@untainted> check resp.getTextPayload());
    }

}

// AWS Lambda를 사용한 서버리스 함수형 서비스 지원.
// Ballerina 컴파일러는 배포할 최종 배포 아티팩트를 자동으로 생성합니다.
@awslambda:Function
public function echo(awslambda:Context ctx, json input) returns json {
    return input;
}

@awslambda:Function
public function notifyS3(awslambda:Context ctx,
                         awslambda:S3Event event) returns json {
    return event.Records[0].s3.'object.key;
}

// Azure Functions를 사용한 서버리스 함수형 서비스 지원.
// AWS Lambda와 유사하게 컴파일러는 배포 아티팩트를 생성합니다.
@af:Function
public function fromQueueToQueue(af:Context ctx,
        @af:QueueTrigger { queueName: "queue1" } string inMsg,
        @af:QueueOutput { queueName: "queue2" } af:StringOutputBinding outMsg) {
    outMsg.value = inMsg;
}

// 사용자 정의 레코드 유형
public type Person record {
    string id;              // 필수 필드
    string name;
    int age?;               // 선택적 필드
    string country = "N/A"; // 기본값
};

@af:Function
public function fromHttpTriggerCosmosDBInput(
        @af:HTTPTrigger { route: "c1/{country}" } af:HTTPRequest httpReq,
        @af:CosmosDBInput { connectionStringSetting: "CosmosDBConnection",
        databaseName: "db1", collectionName: "c1",
        sqlQuery: "select * from c1 where c1.country = {country}" }
        Person[] dbReq)
        returns @af:HTTPOutput string|error {
    return dbReq.toString();
}

public function main() returns @tainted error? {
    int a = 10;               // 64비트 부호 있는 정수
    float b = 1.56;           // 64비트 IEEE 754-2008 이진 부동 소수점 수
    string c = "hello";       // 유니코드 문자열
    boolean d = true;         // true, false
    decimal e = 15.335;       // 십진 부동 소수점 수

    var f = 20;               // 'var'를 사용한 유형 추론 - 'f'는 int입니다.

    int[] intArray = [1, 2, 3, 4, 5, 6];
    int x = intArray.shift(); // 디큐 작업과 유사
    x = intArray.pop();       // 마지막 요소 제거
    intArray.push(10);        // 끝에 추가

    // 튜플 - 각 슬롯에 대해 고유한 유형을 가진 고정 길이 배열과 유사
    [string, int] p1 = ["Jack", 1990];
    [string, int] p2 = ["Tom", 1986];
    io:println("Name: ", p1[0], " Birth Year: ", p1[1]);

    string name1;
    int birthYear1;
    [name1, birthYear1] = p1;     // 튜플 구조 분해

    var [name2, birthYear2] = p2; // 동일한 문에서 값 선언 및 할당

    // If 문
    int ix = 10;
    if ix < 10 {
        io:println("value is less than 10");
    } else if ix == 10 {
        io:println("value equals to 10");
    } else {
        io:println("value is greater than 10");
    }

    // 루프
    int count = 10;
    int i = 0;
    while i < 10 {
        io:println(i);
    }
    // 0에서 count까지 루프(포함)
    foreach var j in 0...count {
        io:println(j);
    }
    // 0에서 count까지 루프(미포함)
    foreach var j in 0..<count {
        io:println(j);
    }
    // 목록 루프
    foreach var j in intArray {
        io:println(j);
    }

    json j1 = { "name" : name1, "birthYear" : birthYear1, "zipcode" : 90210 };
    io:println(j1.name, " - ", j1.zipcode);
    // "mergeJson"을 통해 JSON 값에 새 필드가 추가됩니다.
    var j2 = j1.mergeJson({ "id" : "90400593053"});

    // XML 네임스페이스 선언
    xmlns "http://example.com/ns1" as ns1;
    xmlns "http://example.com/default";

    // 리터럴 값의 XML 변수
    xml x1 = xml `<ns1:entry><name>{{name1}}</name><birthYear>{{birthYear1}}</birthYear></ns1:entry>`;
    io:println(x1);
    // XML 값의 특정 요소에 액세스
    io:println(x1/<name>);
    // XML 값의 모든 자식 항목 나열
    io:println(x1/*);

    // 함수 호출
    x = add(1, 2);
    io:println(multiply(2, 4));
    // 기본 매개변수에 대한 값을 제공하는 호출
    io:println(multiply(3, 4, true));
    // 나머지 매개변수(다중 값)에 대한 값을 사용한 호출
    io:println(addAll(1, 2, 3));
    io:println(addAll(1, 2, 3, 4, 5));

    // 함수 포인터
    (function (int, int) returns int) op1 = getOperation("add");
    (function (int, int) returns int) op2 = getOperation("mod");
    io:println(op1(5, 10));
    io:println(op2(13, 10));

    // 클로저
    (function (int x) returns int) add5 = getAdder(5);
    (function (int x) returns int) add10 = getAdder(10);
    io:println(add5(10));
    io:println(add10(10));

    int[] numbers = [1, 2, 3, 4, 5, 6, 7, 8];
    // 함수형 반복
    int[] evenNumbers = numbers.filter(function (int x) returns boolean { return x % 2 == 0; });

    // 유니온 유형 - "input"은 문자열 또는 byte[] 유형입니다.
    string|byte[] uval = "XXX";

    // 유형 테스트 표현식("uval is string")을 사용하여 변수의 런타임 유형을 확인할 수 있습니다.
    if uval is string {
        // 현재 범위에서 "uval"은 문자열 값입니다.
        string data = "data:" + uval;
    } else {
        // "if" 문의 표현식에서 문자열이 아님을 배제했으므로 남은 유일한 유형은 "byte[]"입니다. 따라서 현재 범위에서 "uval"은 항상 "byte[]"입니다.
        int inputLength = uval.length();
    }

    // 오류 처리
    string input = io:readln("Enter number: ");
    int|error result = ints:fromString(input);
    if result is int {
        io:println("Number: ", result);
    } else {
        io:println("Invalid number: ", input);
    }

    // check 표현식을 사용하여 하위 표현식이 런타임에 오류 값으로 평가된 경우 현재 함수에서 오류를 직접 반환할 수 있습니다.
    int number = check ints:fromString(input);

    // 함수에서 워커를 사용한 동시 실행
    doWorkers();

    // 미래를 사용한 비동기 실행
    future<int> f10 = start fib(10);
    var webresult = clientEP->get("/");
    int fresult = wait f10;
    if webresult is http:Response {
        io:println(webresult.getTextPayload());
        io:println(fresult);
    }

    // 매핑 유형
    map<int> ageMap = {};
    ageMap["Peter"] = 25;
    ageMap["John"] = 30;

    int? agePeter = ageMap["Peter"]; // int?는 유니온 유형 int|() - int 또는 nill입니다.
    if agePeter is int {
        io:println("Peter's age is ", agePeter);
    } else {
        io:println("Peter's age is not found");
    }

    Person person1 = { id: "p1", name : "Anne", age: 28, country: "Sri Lanka" };
    Scores score1 = { physics : 80, mathematics: 95 };
    score1["chemistry"] = 75;
    io:println(score1["chemistry"]);

    Student student1 = { id: "s1", name: "Jack", age: 25, country: "Japan" };
    student1.college = "Stanford";
    string? jacksCollege = student1?.college; // 선택적 필드 액세스
    if jacksCollege is string {
        io:println("Jack's college is ", jacksCollege);
    }

    // 구조적 유형 시스템으로 인해 "student1"은 "person2"에 할당될 수 있습니다.
    // student1의 구조가 person2의 구조와 호환되기 때문입니다.
    // 여기서 "Student"는 "Person"이라고도 할 수 있습니다.
    Person person2 = student1;

    map<int> grades = {"Jack": 95, "Anne": 90, "John": 80, "Bill": 55};
    Person px1 = {id: "px1", name: "Jack", age: 30, country: "Canada"};
    Person px2 = {id: "px2", name: "John", age: 25};
    Person px3 = {id: "px3", name: "Anne", age: 17, country: "UK"};
    Person px4 = {id: "px4", name: "Bill", age: 15, country: "USA"};
    Person[] persons = [];
    persons.push(px1);
    persons.push(px2);
    persons.push(px3);
    persons.push(px4);

    // 목록 데이터에 대한 복잡한 쿼리를 실행하는 데 사용되는 쿼리 표현식
    Result[] results = from var person in persons
                       let int lgrade = (grades[person.name] ?: 0)
                       where lgrade > 75
                           let string targetCollege = "Stanford"
                           select {
                               name: person.name,
                               college: targetCollege,
                               grade: lgrade
                           };

    // 신뢰할 수 없는 데이터를 처리하기 위한 컴파일 타임 오염 검사
    string s1 = "abc";
    mySecureFunction(s1);
    // "s2"를 명시적으로 오염된 값으로 만듭니다. Ballerina 프로그램에 대한 외부 입력(예: 명령줄 인수 및 네트워크 입력)은 기본적으로 오염된 데이터로 표시됩니다.
    string s2 = <@tainted> s1;
    // "s2x"는 이제 오염된 값입니다. 왜냐하면 그 값은 오염된 값(s1)을 사용하여 파생되었기 때문입니다.
    string s2x = s2 + "abc";
    // 다음 줄의 주석을 해제하면 컴파일 오류가 발생합니다. 왜냐하면 오염되지 않은 값을 예상하는 함수에 오염된 값(s2x)을 전달하기 때문입니다.
    // mySecureFunction(s2x);

    // 객체 인스턴스화
    Employee emp1 = new("E0001", "Jack Smith", "Sales", 2009);
    io:println("The company service duration of ", emp1.name,
               " is ", emp1.serviceDuration());

    // 지원되는 작업은 "transaction" 블록에 작업을 묶어 트랜잭션에서 실행할 수 있습니다.
    transaction {
        // 아래 데이터베이스 작업을 단일 로컬 트랜잭션에서 실행합니다.
        var r1 = accountsDB->update("UPDATE Employee SET balance = balance + ? WHERE id = ?", 5500.0, "ID001");
        var r2 = accountsDB->update("UPDATE Employee SET balance = balance + ? WHERE id = ?", 5500.0, "ID001");
    }
}

// 객체는 데이터와 기능을 모두 캡슐화하는 행동 유형입니다.
type Employee object {

    // 개인 필드는 객체 및 해당 메서드 내에서만 볼 수 있습니다.
    private string empId;
    // 공용 필드는 누구나 액세스할 수 있습니다.
    public string name;
    public string department;
    // 기본 한정자는 "보호된" 필드이며, 모듈 내에서만 액세스할 수 있습니다.
    int yearJoined;

    // 객체 초기화 함수; 객체가 인스턴스화될 때 자동으로 호출됩니다.
    public function __init(string empId, string name, string department, int yearJoined) {
        self.empId = empId;
        self.name = name;
        self.department = department;
        self.yearJoined = yearJoined;
    }

    // 객체 메서드
    public function serviceDuration() returns int {
        time:Time ct = time:currentTime();
        return time:getYear(ct) - self.yearJoined;
    }

};

// 학생은 사람의 하위 유형입니다.
type Student record {
    string id;
    string name;
    int age;
    string college?;
    string country;
};

type Scores record {
    int physics;
    int mathematics;
};

type Result record {
    string name;
    string college;
    int grade;
};

public function getOperation(string op) returns (function (int, int) returns int) {
    if op == "add" {
        return add;
    } else if op == "mod" {
        return function (int a, int b) returns int { // 익명 함수
            return a % b;
        };
    } else {
        return (x, y) => 0; // 단일 표현식 익명 no-op 함수
    }
}

// 두 개의 필수 매개변수
public function add(int a, int b) returns int {
    return a + b;
}

// 'log'는 기본 매개변수입니다.
public function multiply(int a, int b, boolean log = false) returns int {
    if log {
        io:println("Multiplying ", a, " with ", b);
    }
    return a * b;
}

// 'numbers'는 나머지 매개변수입니다 - 배열과 유사하게 여러 값을 가질 수 있습니다.
public function addAll(int... numbers) returns int {
    int result = 0;
    foreach int number in numbers {
        result += number;
    }
    return result;
}

public function getAdder(int n) returns (function (int x) returns int) {
    return function (int x) returns int { // 클로저 반환
        return x + n;
    };
}

function fib(int n) returns int {
    if n <= 2 {
        return 1;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

// 워커 블록 "w1" 및 "w2"의 코드는 이 함수가 호출될 때 동시에 실행됩니다. "wait" 표현식은 주어진 워커가 결과를 검색하기 위해 완료될 때까지 기다립니다.
public function doWorkers() {
    worker w1 returns int {
        int j = 10;
        j -> w2;
        int b;
        b = <- w2;
        return b * b;
    }
    worker w2 returns int {
        int a;
        a = <- w1;
        a * 2 -> w1;
        return a + 2;
    }
    record {int w1; int w2;} x = wait {w1, w2};
    io:println(x);
}

// 오염되지 않은 문자열 값만 사용하는 함수입니다.
public function mySecureFunction(@untainted string input) {
    io:println(input);
}
```

### 더 읽을거리

* [예제로 배우는 Ballerina](https://ballerina.io/learn/by-example/)
* [사용자 가이드](https://ballerina.io/learn/installing-ballerina/)
* [API 문서](https://ballerina.io/learn/api-docs/ballerina/)
* [언어 사양](https://ballerina.io/spec/)