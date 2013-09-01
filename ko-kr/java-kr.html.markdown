---
language: java
filename: java-kr.java
category: language
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["wikibook", "http://wikibook.co.kr"]
lang: ko-kr
---

자바는 일반 목적으로 사용할 수 있고 동시성을 지원하며, 클래스 기반의 객체지향 컴퓨터 프로그래밍 언어입니다.
[더 자세한 사항](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// 한 줄짜리 주석은 //로 시작합니다.
/*
여러 줄 주석은 다음과 같은 형태입니다.
*/
/**
자바독(JavaDoc) 주석은 이렇게 생겼습니다. 자바독 주석은 클래스나 클래스의
다양한 속성을 기술하는 데 사용됩니다.
*/

// java.util 패키지 안에 있는 ArrayList 클래스를 임포트합니다.
import java.util.ArrayList;
// java.security 패키지 안에 있는 모든 클래스를 임포트합니다.
import java.security.*;

// 각 .java 파일에는 공용(public) 클래스가 들어 있으며, 클래스의 이름은
// 파일명과 동일합니다.
public class LearnJava {

    // 프로그램에는 반드시 진입점 역할을 하는 main 메서드가 하나 있어야 합니다.
    public static void main (String[] args) {

        // System.out.println을 이용해 한 줄을 출력합니다.
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // 줄바꿈 없이 뭔가를 출력하려면 System.out.print를 사용합니다.
        System.out.print("Hello ");
        System.out.print("World");


        ///////////////////////////////////////
        // 타입 & 변수
        ///////////////////////////////////////

        // <타입> <이름>과 같은 형태로 변수를 선언합니다.
        // Byte - 부호가 있는 8비트 2의 보수 정수
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - 부호가 있는 16비트 2의 보수 정수
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - 부호가 있는 32비트 2의 보수 정수
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - 부호가 있는 64비트 2의 보수 정수
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L은 이 변수의 값이 Long 타입임을 나타내는 데 사용됩니다.
        // L이 없는 것들은 기본적으로 정수로 간주됩니다.

        // 참고: 자바에는 부호 없는(unsigned) 타입이 없습니다.

        // Float - 단정도 32비트 IEEE 754 부동 소수점 수
        float fooFloat = 234.5f;
        // f는 이 변수의 값이 float 타입임을 나타내는 데 사용됩니다.
        // f를 지정하지 않으면 double로 간주됩니다.

        // Double - 배정도 64비트 IEEE 754 부동 소수점 수
        double fooDouble = 123.4;

        // Boolean - 참(true) & 거짓(false)
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - 단일 16비트 유니코드 문자
        char fooChar = 'A';

        // 변수를 변경할 수 없게 만들려면 final을 지정합니다.
        final int HOURS_I_WORK_PER_WEEK = 9001;

        // 문자열
        String fooString = "My String Is Here!";

        // \n은 새로운 줄을 시작하는 이스케이프 문자입니다.
        String barString = "Printing on a new line?\nNo Problem!";
        // \t는 탭 문자를 추가하는 이스케이프 문자입니다.
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // 배열
        // 배열의 크기는 반드시 선언할 때 결정해야 합니다.
        // 배열을 선언하는 형식은 다음과 같습니다.
        //<자료형> [] <변수명> = new <자료형>[<배열 크기>];
        int [] intArray = new int[10];
        String [] stringArray = new String[1];
        boolean [] booleanArray = new boolean[100];

        // 배열을 선언하고 초기화하는 또 다른 방법
        int [] y = {9000, 1000, 1337};

        // 배열 인덱스 - 요소에 접근
        System.out.println("intArray @ 0: " + intArray[0]);

        // 배열의 인덱스는 0에서부터 시작하며 변경 가능합니다.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // 기타 참고할 만한 자료구조
        // ArrayLists - 좀 더 많은 기능을 제공하고 크기를 변경 가능하다는 점을
        //              제외하면 배열과 비슷합니다.
        // LinkedLists
        // Maps
        // HashMaps

        ///////////////////////////////////////
        // 연산자
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2; // 다중 선언의 축약형

        // 산술 연산은 이해하기 어렵지 않습니다.
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (0.5를 잘라 버립니다)

        // 나눗셈
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // 비교 연산자
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // 비트 연산자!
        /*
        ~       단항 보수 연산
        <<      산술적 왼쪽 시프트
        >>      산술적 오른쪽 시프트
        >>>     논리적 오른쪽 시프트
        &       비트 단위 논리곱(AND)
        ^       비트 단위 배타적 논리합(OR)
        |       비트 단위 논리합(OR)
        */

        // 증감 연산자
        int i = 0;
        System.out.println("\n->Inc/Dec-rementation");
        System.out.println(i++); //i = 1. 후치 증가 연산
        System.out.println(++i); //i = 2. 전치 증가 연산
        System.out.println(i--); //i = 1. 후치 감소 연산
        System.out.println(--i); //i = 0. 전치 감소 연산

        ///////////////////////////////////////
        // 제어 구조
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // if 문은 C 언어와 비슷합니다.
        int j = 10;
        if (j == 10){
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }

        // while 루프
        int fooWhile = 0;
        while(fooWhile < 100)
        {
            // System.out.println(fooWhile);
            // 카운터를 증가
            // 99번 반복, fooWhile 0->99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // do-while 루프
        int fooDoWhile = 0;
        do
        {
            // System.out.println(fooDoWhile);
            // 카운터를 증가
            // 99번 반복, fooDoWhile 0->99
            fooDoWhile++;
        }while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // for 루프
        int fooFor;
        // for 루프 구조 => for(<초기식>; <조건식>; <증감식>)
        for(fooFor=0; fooFor<10; fooFor++){
            // System.out.println(fooFor);
            // 10번 반복, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);

        // switch-case 문
        // switch는 byte, short, char, int 자료형을 대상으로 동작합니다.
        // 아울러 열거형을 비롯해 String 클래스 및 원시 타입을 감싼 Character, 
        // Byte, Short, Integer와 같은 몇 가지 특별한 클래스에 대해서도 동작합니다.
        int month = 3;
        String monthString;
        switch (month){
            case 1:
                    monthString = "January";
                    break;
            case 2:
                    monthString = "February";
                    break;
            case 3:
                    monthString = "March";
                    break;
            default:
                    monthString = "Some other month";
                    break;
        }
        System.out.println("Switch Case Result: " + monthString);


        ///////////////////////////////////////
        // 자료형 변환과 형변환
        ///////////////////////////////////////

        // 데이터 변환

        // 문자열에서 정수로 변환
        Integer.parseInt("123");// 정수 버전의 "123"을 반환

        // 정수를 문자열로 변환
        Integer.toString(123);// 문자열 버전의 123을 반환

        // 다른 변환에 대해서는 아래 클래스를 확인해 보세요.
        // Double
        // Long
        // String

        // 형변환
        // 자바 객체 또한 형변환할 수 있으며, 이와 관련해서 알아야 할 세부사항이
        // 많을뿐더러 다소 중급 수준에 해당하는 개념들도 다뤄야 합니다.
        // 이와 관련된 사항은 아래 링크를 참고하세요.
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // 클래스와 함수
        ///////////////////////////////////////

        System.out.println("\n->Classes & Functions");

        // (Bicycle 클래스의 정의)

        // 클래스를 인스턴스화하려면 new를 사용합니다.
        Bicycle trek = new Bicycle();

        // 객체의 메서드를 호출합니다.
        trek.speedUp(3); // 항상 설정자 메서드와 접근자 메서드를 사용해야 합니다.
        trek.setCadence(100);

        // 현재 객체의 값을 표시할 때는 관례적으로 toString을 사용합니다.
        System.out.println("trek info: " + trek.toString());

    } // main 메서드 끝
} // LearnJava 클래스 끝


// .java 파일 안에 다른 비공개 클래스를 포함할 수 있습니다.


// 클래스 선언 문법:
// <public/private/protected> class <클래스명>{
//    // 데이터 필드, 생성자, 함수가 모두 이곳에 들어갑니다.
//    // 자바에서는 함수를 메서드라고 부릅니다.
// }

class Bicycle {

    // Bicycle의 필드와 변수
    public int cadence; // Public: 어느 곳에서도 접근할 수 있습니다.
    private int speed;  // Private: 클래스 안에서만 접근할 수 있습니다.
    protected int gear; // Protected: 현재 클래스와 하위 클래스에서 접근할 수 있습니다.
    String name; // default: 현재 패키지 안에서만 접근할 수 있습니다.

    // 생성자는 클래스를 생성하는 방법 중 하나입니다.
    // 다음은 기본 생성자입니다.
    public Bicycle() {
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }

    // 다음은 구체화된 생성자입니다(인자를 담고 있습니다)
    public Bicycle(int startCadence, int startSpeed, int startGear, String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // 함수 문법:
    // <public/private/protected> <반환형> <함수명>(<인자>)

    // 자바 클래스는 필드에 대해 접근자 메서드와 설정자 메서드를 구현할 때가 많습니다.

    // 메서드 선언 문법:
    // <유효범위> <반환형> <메서드명>(<인자>)
    public int getCadence() {
        return cadence;
    }

    // void 메서드는 반환형이 필요하지 않습니다.
    public void setCadence(int newValue) {
        cadence = newValue;
    }

    public void setGear(int newValue) {
        gear = newValue;
    }

    public void speedUp(int increment) {
        speed += increment;
    }

    public void slowDown(int decrement) {
        speed -= decrement;
    }

    public void setName(String newName) {
        name = newName;
    }

    public String getName() {
        return name;
    }

    // 현재 객체의 속성값을 표시하는 메서드
    @Override
    public String toString() {
        return "gear: " + gear +
                " cadence: " + cadence +
                " speed: " + speed +
                " name: " + name;
    }
} // Bicycle 클래스의 끝

// PennyFarthing은 Bicycle의 하위 클래스입니다.
class PennyFarthing extends Bicycle {
    // (페니 파딩은 앞바퀴가 굉장히 큰 자전거입니다. 기어가 없죠.)

    public PennyFarthing(int startCadence, int startSpeed){
        // super를 이용해 부모 생성자를 호출합니다.
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // @annotation을 이용해 재정의하는 메서드를 표시해야 합니다.
    // 애노테이션과 애노테이션의 용도에 관한 자세한 내용은 아래 링크를 참고하세요.
    // 애노테이션: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
    }

}

```

## 기타 참고자료

다음 링크를 통해 다양한 주제를 이해하고 구글을 통해 구체적인 예제들을 찾아보세요.

공부할 만한 기타 주제:

* [썬/오라클의 자바 자습서](http://docs.oracle.com/javase/tutorial/index.html)

* [자바 접근 제한자](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [객체 지향 프로그래밍 개념](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [상속(Inheritance)](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [다형성(Polymorphism)](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [추상화(Abstraction)](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [예외(Exceptions)](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [인터페이스(Interfaces)](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [제네릭(Generics)](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [자바 코딩 관례(Java Code Conventions)](http://www.oracle.com/technetwork/java/codeconv-138413.html)
