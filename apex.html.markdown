---
language: apex
contributors:
    - ["Christian Coleman", "https://github.com/christiancoleman"]
filename: apex.html.markdown
---

Apex is a strongly typed, object-oriented programming language that allows developers to execute flow and transaction control statements on Salesforce servers in conjunction with calls to the API.
[Read more here.](https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_intro_what_is_apex.htm)

```java
public class ApexControllerExample {

	// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/langCon_apex_data_types.htm

	// Single-line comments start with //

	/*
	Multi-line comments look like this.
	*/

	// Java and Apex are extremely similar in syntax
	// one difference that I like to point out is the following..
	// unlike Java where an integer would be assigned like this:
	// int i = 3;
	// in Apex you need to do:
	Integer i = 3;
	String helloWorld = 'This will be printed to the debug console!';

	// use System.debug() to print lines in the developer console
	System.debug(i); // 3
	System.debug(helloWorld); // This will be printed to the debug console!

	// Apex is case insensitive so the following atrocities not only compile but work perfectly
	SySteM.dEbUg('hI!');
	system.DEBUG('this');
	SYSTEM.debug('is');
	sYSTEM.dEBUG('gross');
	system.debug('!');
	// but don't be that guy...

	/* 
		All of the reserved namespaces in the Apex reference guide: 
		https://developer.salesforce.com/docs/atlas.en-us.apexref.meta/apexref/apex_ref_guide.htm
		can be used without doing any explicit importing
	*/

	// PRIMITIVE TYPES /////////////////////////////////////////////////////////////////////////////
	// Blob
	// Boolean
	// Date
	// Datetime
	// Decimal
	// Double
	// Id
	// Long
	// Object
	// String
	// Integer

	Blob blobby = Blob.valueOf(helloWorld);
	System.debug(blobby); // Blob[42]
	
	Boolean booly = true;

	Date odate = Date.today();
	System.debug(odate); // 2022-09-15 00:00:00

	Datetime odt = Datetime.now();
	System.debug(odt); // 2022-09-15 16:31:11

	Decimal deci = 3.14159;
	System.debug(deci); // 3.14159

	Double doubly = 3.14159;
	System.debug(doubly); // 3.14159

	// there's an algorithm behind the scenes that verifies ids are correct
	// the first assignment is an actual case id from one of my orgs, but 
	// if you change the id slightly it will no longer save as a valid id
	// works
	Id myPretendId1 = '5004C00000CU4sQQAT';
	// doesn't work
	//Id myPretendId2 = '1234567890';
	System.debug(myPretendId1); // 5004C00000CU4sQQAT

	Long long1 = 21415151L;
	Long long2 = 21415151;
	System.debug(long1); // 21415151
	System.debug(long2); // 21415151
	
	Object objString = 'This is a string';
	Object objNum = 42;
	
	// casting the objects to their appropriate types
	String actualString = (String) objString;
	Integer actualNum = (Integer) objNum;

	System.debug(objString); // This is a string
	System.debug(objNum); // 42
	//System.debug(objString + objNum); // does not work
	System.debug(actualString); // This is a string
	System.debug(actualNum); // 42
	System.debug(actualString + actualNum); // This is a string42

	// newInstance(hour, minutes, seconds, milliseconds)
	Time t = Time.newInstance(1, 30, 0, 0);
	System.debug(t); // 01:30:00.000Z

	// SALESFORCE TYPES ////////////////////////////////////////////////////////////////////////////
	// Case
	// Account
	// Opportunity
	// ...
	// System
	// Time
	// Database

	// Standard Salesforce objects have API names that can be referenced in code; the API name is usually the word(s) without any spaces
	Case c = new Case();
	Account a = new Account();
	Opportunity o = new Opportunity();

	// Custom objects can also be accessed in code via their API names, but custom objects
	// have a __c appended to their names. This helps differentiating what's been custom
	// and what's remain standard.
	// For example, if we had a custom object named: 'Newspaper', then to instantiate one of them do:
	// Newspaper__c news = new Newspaper__c();

	// Populate a standard field on the Case object
	c.Comments = 'These are case comments in a standard field';

	// If we were to add a custom field on the case object called Additional Details then it's population might look like:
	//c.Additional_Details__c = 'These are additional details';
	//^ Just like custom objects, custom fields have a __c appendage 

	// NON-PRIMITIVE TYPES /////////////////////////////////////////////////////////////////////////
	// sObject
	// List
	// Set
	// Map
	// enum
	
	// sObject is to Salesforce types as Object is to primitives
	sObject genericSObject = new Case();

	// casting the sObject to an actual Case
	Case actualCase = (Case) genericSObject;

	// creating lists
	List<Account> accountList = new List<Account>();
	List<Case> caseList = new List<Case>();

	// creating sets
	Set<Account> accountSet = new Set<Account>();
	Set<Case> caseSet = new Set<Case>();

	// creating maps
	Map<Id, Account> accountMap = new Map<Id, Account>();
	Map<Id, Case> caseMap = new Map<Id, Case>();
	Map<Case, Account> caseAccountMap = new Map<Case, Account>();
	Map<Account, Case> accountCaseMap = new Map<Account, Case>();

	// ADDING TYPES OF YOUR OWN ////////////////////////////////////////////////////////////////////////////////////////////////////
	// Every class that you add (and make either public or global) can be referenced and either used as a static helper or
	// instantiated depending 

	// CONTROL STRUCTURES //////////////////////////////////////////////////////////////////////////////////////////////////////////
	//for
	//while
	//if
	for(Integer i = 0; i < 50; i++){
		System.debug('For loops!' + i);
	}

	final Integer LOOP_X_TIMES = 5; // final signifies a constant and this variable can not be reassigned or changed
	Integer currentCount = 0;
	while(currentCount <= LOOP_X_TIMES){
		System.debug('While loops!');
		currentCount++; // ++ syntax means add 1 to the value and save it, just like C++
	}

	if(LOOP_X_TIMES == currentCount){
		System.debug('We have finished our loops!');
	} else {
		System.debug('I might have miscounted! Either way, the loops are done!');
	}

	// DMLs
	Case newCase = new Case();
	insert newCase;
	newCase.Comments = 'Hello here are some comments.';
	update newCase;

	// Queries
	List<Case> actualQueryForCases = [SELECT Id FROM Case];

}
```

## Further Reading

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.

**Official Oracle Guides**:

* [Java Tutorial Trail from Sun / Oracle](https://docs.oracle.com/javase/tutorial/index.html)

* [Java Access level modifiers](https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Object-Oriented Programming Concepts](https://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](https://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](https://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](https://www.oracle.com/technetwork/java/codeconvtoc-136057.html)

* New features in Java 8:
    * [Lambda expressions (functional programming)](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)
    * [Date and time API (java.time package)](http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)

**Online Practice and Tutorials**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)

* [Codewars - Java Katas](https://www.codewars.com/?language=java)

* [University of Helsinki - Object-Oriented programming with Java](http://moocfi.github.io/courses/2013/programming-part-1/)

**Books**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](https://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](https://www.amazon.com/gp/product/0071606300)
