---
name: COBOL
contributors:
    - ["Hyphz", "http://github.com/hyphz/"]
filename: learn.cob
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

COBOL은 1960년 원래 설계 이후 여러 번 개정된 비즈니스 지향 언어입니다.

```cobol
      *COBOL. 1985년처럼 코딩하기.
      *OpenCobolIDE 4.7.6에서 GnuCOBOL로 컴파일됩니다.

      *COBOL은 레거시(COBOL-85)와
      *현대(COBOL-2002 및 COBOL-2014) 버전 간에 상당한 차이가 있습니다.
      *레거시 버전은 1-6열을 비워 두어야 합니다(천공 카드의 인덱스 번호를
      *저장하는 데 사용됨).
      *7열의 '*'는 주석을 의미합니다.
      *레거시 COBOL에서는 주석이 전체 줄만 될 수 있습니다.
      *현대 COBOL은 고정 열을 요구하지 않으며 주석에 *>를 사용하며,
      *이는 줄 중간에 나타날 수 있습니다.
      *레거시 COBOL은 또한 최대 줄 길이에 제한을 둡니다.
      *키워드는 레거시 COBOL에서 대문자여야 하지만,
      *현대에서는 대소문자를 구분하지 않습니다.
      *현대 COBOL은 혼합 대소문자를 사용할 수 있지만
      *COBOL 코드를 작성할 때 여전히 모두 대문자를 사용하는 것이 일반적입니다.
      *이것이 대부분의 전문 COBOL 개발자가 하는 일입니다.
      *COBOL 문은 마침표로 끝납니다.

      *COBOL 코드는 4개의 부서로 나뉩니다.
      *이 부서들은 순서대로 다음과 같습니다:
      *IDENTIFICATION DIVISION.
      *ENVIRONMENT DIVISION.
      *DATA DIVISION.
      *PROCEDURE DIVISION.

      *먼저 프로그램에 ID를 부여해야 합니다.
      *IDENTIFICATION DIVISION에는 다른 값도 포함될 수 있지만,
      *주석일 뿐입니다. PROGRAM-ID는 유일하게
      *필수입니다.
       IDENTIFICATION DIVISION.
           PROGRAM-ID.    LEARN.
           AUTHOR.        JOHN DOE.
           DATE-WRITTEN.  05/02/2020.

      *변수를 선언해 보겠습니다.
      *DATA DIVISION 내의 WORKING-STORAGE 섹션에서 이 작업을 수행합니다.
      *각 데이터 항목(일명 변수)은 레벨 번호로 시작하고,
      *항목 이름, 그 뒤에 변수가 포함할 데이터 유형을
      *설명하는 PICTURE 절이 옵니다.
      *거의 모든 COBOL 프로그래머는 PICTURE를 PIC로 축약합니다.
      *A는 알파벳, X는 영숫자, 9는 숫자입니다.

      *예:
       01  MYNAME PIC XXXXXXXXXX.    *> 10자 문자열.

      *하지만 모든 X를 세는 것은 오류를 유발할 수 있으므로,
      *위 코드는 다음과 같이 다시 작성할 수 있습니다.
       01  MYNAME PIC X(10).

      *다음은 몇 가지 추가 예입니다:
       01  AGE             PICTURE  9(3).   *> 최대 3자리 숫자.
       01  BIRTH_YEAR      PIC      S9(7).  *> 최대 7자리 부호 있는 숫자.
       01  LAST_NAME       PIC      X(10).  *> 최대 10자 문자열.

      *COBOL에서는 여러 공백이 단일 공백과 동일하므로
      *다른 코더가 읽기 쉽도록 코드를 정렬하기 위해 여러 공백을 사용하는 것이 일반적입니다.


      *이제 코드를 작성해 보겠습니다. 다음은 간단한 Hello World 프로그램입니다.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 THE-MESSAGE      PIC X(20).
       PROCEDURE DIVISION.
           DISPLAY "STARTING PROGRAM".
           MOVE "HELLO WORLD" TO THE-MESSAGE.
           DISPLAY THE-MESSAGE.
           STOP RUN.

      *위 코드는 다음을 출력합니다:
      *STARTING PROGRAM
      *HELLO WORLD



      ********COBOL은 수학을 수행할 수 있습니다***************
       ADD 1 TO AGE GIVING NEW-AGE.
       SUBTRACT 1 FROM COUNT.
       DIVIDE VAR-1 INTO VAR-2 GIVING VAR-3.
       COMPUTE TOTAL-COUNT = COUNT1 PLUS COUNT2.


      *********PERFORM********************
      *PERFORM 키워드를 사용하면 코드의 다른 지정된 섹션으로 점프한 다음
      *지정된 코드 섹션이 완료되면 다음 실행 가능한 문으로 돌아갈 수 있습니다.
      *PERFORM이라는 전체 단어를 작성해야 하며 축약할 수 없습니다.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOCOBOL.

       PROCEDURE DIVISION.
           FIRST-PARA.
               DISPLAY 'THIS IS IN FIRST-PARA'.
      *SECOND-PARA를 건너뛰고 3번째 및 4번째를 수행합니다.
      *그런 다음 THIRD-PARA 및 FOURTH-PARA를 수행한 후
      *여기로 돌아와 STOP RUN까지 프로그램을 계속합니다.
           PERFORM THIRD-PARA THRU FOURTH-PARA.

           SECOND-PARA.
               DISPLAY 'THIS IS IN SECOND-PARA'.

           STOP RUN.

           THIRD-PARA.
               DISPLAY 'THIS IS IN THIRD-PARA'.

           FOURTH-PARA.
               DISPLAY 'THIS IS IN FOURTH-PARA'.


      *위 프로그램을 컴파일하고 실행하면
      *다음 결과가 생성됩니다(순서 참고):
      *THIS IS IN FIRST-PARA
      *THIS IS IN THIRD-PARA
      *THIS IS IN FOURTH-PARA
      *THIS IS IN SECOND-PARA


      **********STRING을 사용하여 변수 결합 ***********

      *이제 두 가지 관련 COBOL 동사인 STRING과
      *UNSTRING에 대해 알아볼 시간입니다.

      *STRING 동사는 두 개 이상의 문자열을 연결하거나 합치는 데 사용됩니다.
      *UNSTRING은 놀랍게도 문자열을 두 개 이상의 작은 문자열로 분리하는 데 사용됩니다.
      *프로그램에서 STRING 또는 UNSTRING을 사용할 때 DELIMITED BY를 사용하는 것을
      *기억하는 것이 중요합니다.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEARNING.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FULL-NAME PIC X(20).
       01 FIRST-NAME PIC X(13) VALUE "BOB GIBBERISH".
       01 LAST-NAME PIC X(5) VALUE "COBB".
       PROCEDURE DIVISION.
           STRING FIRST-NAME DELIMITED BY SPACE
             " "
             LAST-NAME DELIMITED BY SIZE
             INTO FULL-NAME
           END-STRING.
           DISPLAY "THE FULL NAME IS: "FULL-NAME.
       STOP RUN.


      *위 코드는 다음을 출력합니다:
      *THE FULL NAME IS: BOB COBB


      *그 이유를 알아보기 위해 살펴보겠습니다.

      *먼저, 문자열 명령으로 생성하는 변수를 포함하여 모든 변수를
      *DATA DIVISION에 선언했습니다.

      *작업은 PROCEDURE DIVISION에서 발생합니다.
      *STRING 키워드로 시작하여 END-STRING으로 끝납니다. 그 사이에
      *더 큰 마스터 변수로 결합하려는 것을 나열합니다. 여기서는
      *FIRST-NAME, 공백 및 LAST-NAME을 결합하고 있습니다.

      *FIRST-NAME 및 LAST-NAME 뒤에 오는 DELIMITED BY 구문은
      *프로그램에 각 변수를 얼마나 캡처할지 알려줍니다.
      *DELIMITED BY SPACE는 프로그램에 처음부터 시작하여
      *공백을 만날 때까지 변수를 캡처하도록 지시합니다.
      *DELIMITED BY SIZE는 프로그램에 변수의 전체 크기를 캡처하도록 지시합니다.
      *FIRST-NAME 뒤에 DELIMITED BY SPACE가 있으므로 GIBBERISH
      *부분은 무시됩니다.

      *이것을 더 명확하게 하려면 위 코드의 10행을 다음과 같이 변경하십시오.
           STRING FIRST-NAME DELIMITED BY SIZE
      *그런 다음 프로그램을 다시 실행하십시오. 이번에는 출력이 다음과 같습니다:
      *THE FULL NAME IS: BOB GIBBERISH COBB
```

## 더 읽을거리

* [GnuCOBOL](https://gnucobol.sourceforge.io/)