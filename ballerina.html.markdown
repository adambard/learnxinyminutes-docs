---
language: Ballerina
contributors:
    - ["Anjana Fernando", "https://github.com/lafernando"]
filename: learn_ballerina.bal
---

[Ballerina](https://ballerina.io/) is a statically-typed programming language for making development for the cloud an enjoyable experience. 

```java
// Single-line comment

// Import modules into the current source file 
import ballerina/io;
import ballerina/time;
import ballerina/http;
import ballerinax/java.jdbc;
import ballerina/lang.'int as ints;
import ballerinax/awslambda;
// Module alias "af" used in code in place of the full module name
import ballerinax/azure.functions as af;

http:Client clientEP = new ("https://freegeoip.app/");
jdbc:Client accountsDB = new ({url: "jdbc:mysql://localhost:3306/AccountsDB", 
                               username: "test", password: "test"});

// A service is a first-class concept in Ballerina, and is one of the 
// entrypoints to a Ballerina program. 
// The Ballerina platform also provides support for easy deployment to 
// environments such as Kubernetes (https://ballerina.io/learn/deployment/kubernetes/).
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

// Serverless Function-as-a-Service support with AWS Lambda.
// The Ballerina compiler automatically generates the final deployment 
// artifact to be deployed.
@awslambda:Function
public function echo(awslambda:Context ctx, json input) returns json {
    return input;
}

@awslambda:Function
public function notifyS3(awslambda:Context ctx, 
                         awslambda:S3Event event) returns json {
    return event.Records[0].s3.'object.key;
}

// Serverless Function-as-a-Service support with Azure Functions.
// Similar to AWS Lambda, the compiler generates the deployment artifacts.
@af:Function
public function fromQueueToQueue(af:Context ctx, 
        @af:QueueTrigger { queueName: "queue1" } string inMsg,
        @af:QueueOutput { queueName: "queue2" } af:StringOutputBinding outMsg) {
    outMsg.value = inMsg;
}

// A custom record type
public type Person record {
    string id;              // required field
    string name;            
    int age?;               // optional field
    string country = "N/A"; // default value
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
    int a = 10;               // 64-bit signed integer
    float b = 1.56;           // 64-bit IEEE 754-2008 binary floating point number
    string c = "hello";       // a unicode string
    boolean d = true;         // true, false
    decimal e = 15.335;       // decimal floating point number

    var f = 20;               // type inference with 'var' - 'f' is an int
    
    int[] intArray = [1, 2, 3, 4, 5, 6];
    int x = intArray.shift(); // similar to a dequeue operation
    x = intArray.pop();       // removes the last element
    intArray.push(10);        // add to the end

    // Tuples - similar to a fixed length array with a distinct type for each slot
    [string, int] p1 = ["Jack", 1990]; 
    [string, int] p2 = ["Tom", 1986];
    io:println("Name: ", p1[0], " Birth Year: ", p1[1]);

    string name1;
    int birthYear1;
    [name1, birthYear1] = p1;     // tuple destructuring

    var [name2, birthYear2] = p2; // declare and assign values in the same statement

    // If statements
    int ix = 10;
    if ix < 10 {
        io:println("value is less than 10");
    } else if ix == 10 {
        io:println("value equals to 10");
    } else {
        io:println("value is greater than 10");
    }

    // Loops
    int count = 10;
    int i = 0;
    while i < 10 {
        io:println(i);
    }
    // Loop from 0 to count (inclusive)
    foreach var j in 0...count {
        io:println(j);
    }
    // Loop from 0 to count (non-inclusive)
    foreach var j in 0..<count {
        io:println(j);
    }
    // Loop a list
    foreach var j in intArray {
        io:println(j);
    }

    json j1 = { "name" : name1, "birthYear" : birthYear1, "zipcode" : 90210 };
    io:println(j1.name, " - ", j1.zipcode);
    // New fields are added to a JSON value through "mergeJson"
    var j2 = j1.mergeJson({ "id" : "90400593053"});

    // XML namespace declaration
    xmlns "http://example.com/ns1" as ns1;
    xmlns "http://example.com/default";
    
    // XML variable from a literal value
    xml x1 = xml `<ns1:entry><name>{{name1}}</name><birthYear>{{birthYear1}}</birthYear></ns1:entry>`;
    io:println(x1);
    // Access specific elements in the XML value
    io:println(x1/<name>);
    // List all child items in the XML value
    io:println(x1/*);

    // Function invocations
    x = add(1, 2);
    io:println(multiply(2, 4));
    // Invocation providing value for the defaultable parameter
    io:println(multiply(3, 4, true));
    // Invocation with values to a rest parameter (multi-valued)
    io:println(addAll(1, 2, 3));
    io:println(addAll(1, 2, 3, 4, 5));

    // Function pointers
    (function (int, int) returns int) op1 = getOperation("add");
    (function (int, int) returns int) op2 = getOperation("mod");
    io:println(op1(5, 10));
    io:println(op2(13, 10));

    // Closures
    (function (int x) returns int) add5 = getAdder(5);
    (function (int x) returns int) add10 = getAdder(10);
    io:println(add5(10));
    io:println(add10(10));

    int[] numbers = [1, 2, 3, 4, 5, 6, 7, 8];
    // Functional iteration
    int[] evenNumbers = numbers.filter(function (int x) returns boolean { return x % 2 == 0; });

    // Union types - "input" is of type either string or byte[]
    string|byte[] uval = "XXX";

    // A type test expression ("uval is string") can be used to check the 
    // runtime type of a variable.
    if uval is string {
        // In the current scope, "uval" is a string value
        string data = "data:" + uval;
    } else {
        // Since the expression in the "if" statement ruled out that it's not a string,
        // the only type left is "byte[]"; so in the current scope, "uval" will always
        // be a "byte[]".
        int inputLength = uval.length();
    }

    // Error handling
    string input = io:readln("Enter number: ");
    int|error result = ints:fromString(input);
    if result is int {
        io:println("Number: ", result);
    } else {
        io:println("Invalid number: ", input);
    }

    // A check expression can be used to directly return the error from
    // the current function if its subexpression evaluated to an error
    // value in the runtime. 
    int number = check ints:fromString(input); 

    // Concurrent execution using workers in a function
    doWorkers();

    // Asynchronous execution with futures
    future<int> f10 = start fib(10);
    var webresult = clientEP->get("/");
    int fresult = wait f10;
    if webresult is http:Response {
        io:println(webresult.getTextPayload());
        io:println(fresult);
    }

    // Mapping types
    map<int> ageMap = {};
    ageMap["Peter"] = 25;
    ageMap["John"] = 30;

    int? agePeter = ageMap["Peter"]; // int? is the union type int|() - int or nill
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
    string? jacksCollege = student1?.college; // optional field access
    if jacksCollege is string {
        io:println("Jack's college is ", jacksCollege);
    }

    // Due to the structural type system, "student1" can be assigned to "person2",
    // since the student1's structure is compatible with person2's,
    // where we can say, a "Student" is a "Person" as well. 
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

    // Query expressions used to execute complex queries for list data
    Result[] results = from var person in persons
                       let int lgrade = (grades[person.name] ?: 0)
                       where lgrade > 75 
                           let string targetCollege = "Stanford"
                           select { 
                               name: person.name, 
                               college: targetCollege, 
                               grade: lgrade 
                           };

    // Compile-time taint checking for handling untrusted data
    string s1 = "abc";
    mySecureFunction(s1);
    // Explicitely make "s2" a tainted value. External input to a Ballerina
    // program such as command-line arguments and network input are by-default
    // marked as tainted data.
    string s2 = <@tainted> s1;
    // "s2x" is now a tainted value, since its value is derived using a 
    // tainted value (s1).
    string s2x = s2 + "abc";
    // The following line uncommented will result in a compilation error,
    // since we are passing a tainted value (s2x) to a function which 
    // exepects an untainted value.
    // mySecureFunction(s2x);

    // Instantiating objects
    Employee emp1 = new("E0001", "Jack Smith", "Sales", 2009);
    io:println("The company service duration of ", emp1.name, 
               " is ", emp1.serviceDuration());

    // Supported operations can be executed in a transaction by enclosing the actions
    // in a "transaction" block. 
    transaction {
        // Executes the below database operations in a single local transactions
        var r1 = accountsDB->update("UPDATE Employee SET balance = balance + ? WHERE id = ?", 5500.0, "ID001");
        var r2 = accountsDB->update("UPDATE Employee SET balance = balance + ? WHERE id = ?", 5500.0, "ID001");
    }
}

// An object is a behavioural type, which encapsulates both data and functionality.
type Employee object {
    
    // Private fields are only visible within the object and its methods
    private string empId;
    // Public fields can be accessed by anyone
    public string name;
    public string department;
    // The default qualifier is a "protected" field, 
    // which are accessible only within the module.
    int yearJoined;           
    
    // The object initialization function; automatically called when an object is instantiated.
    public function __init(string empId, string name, string department, int yearJoined) {
        self.empId = empId;
        self.name = name;
        self.department = department;
        self.yearJoined = yearJoined;        
    }

    // An object method
    public function serviceDuration() returns int {
        time:Time ct = time:currentTime();
        return time:getYear(ct) - self.yearJoined;
    }

};

// Student is a subtype of Person
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
        return function (int a, int b) returns int { // anonymous function
            return a % b;
        };
    } else {
        return (x, y) => 0; // single expression anonymous no-op function 
    }
}

// Two required parameters
public function add(int a, int b) returns int {
    return a + b;
}

// 'log' is a defaultable parameter
public function multiply(int a, int b, boolean log = false) returns int {
    if log {
        io:println("Multiplying ", a, " with ", b);
    }
    return a * b;
}

// 'numbers' is a rest parameter - it can have multiple values, 
// similar to an array.
public function addAll(int... numbers) returns int {
    int result = 0;
    foreach int number in numbers {
        result += number;
    }
    return result;
}

public function getAdder(int n) returns (function (int x) returns int) {
    return function (int x) returns int { // returns closure
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

// The code in worker blocks "w1" and "w2" are executed concurrency 
// when this function is invoked. The "wait" expressions waits for
// the given workers to finish to retrieve their results.
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

// A function which takes in only an untainted string value.
public function mySecureFunction(@untainted string input) {
    io:println(input);
}
```

### Further Reading

* [Ballerina by Example](https://ballerina.io/learn/by-example/)
* [User Guide](https://ballerina.io/learn/installing-ballerina/)
* [API Documentation](https://ballerina.io/learn/api-docs/ballerina/)
* [Language Specification](https://ballerina.io/spec/)
