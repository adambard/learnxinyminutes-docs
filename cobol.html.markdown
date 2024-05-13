---
language: COBOL
contributors:
    - ["Hyphz", "http://github.com/hyphz/"]
filename: learn.COB
---
COBOL is a business-oriented language revised multiple times since its original design in 1960. It is claimed to still be used in over 80% of 
organizations.

```cobol
      *COBOL. Coding like it's 1985. 
      *Compiles with GnuCOBOL in OpenCobolIDE 4.7.6.
       
      *COBOL has significant differences between legacy (COBOL-85)
      *and modern (COBOL-2002 and COBOL-2014) versions.
      *Legacy versions require columns 1-6 to be blank (they are used
      *to store the index number of the punched card).
      *A '*' in column 7 means a comment.
      *In legacy COBOL, a comment can only be a full line.
      *Modern COBOL doesn't require fixed columns and uses *> for
      *a comment, which can appear in the middle of a line.
      *Legacy COBOL also imposes a limit on maximum line length.
      *Keywords have to be in capitals in legacy COBOL,
      *but are case insensitive in modern.
      *Although modern COBOL allows you to use mixed-case characters
      *it is still common to use all caps when writing COBOL code.
      *This is what most professional COBOL developers do.
      *COBOL statements end with a period.
      
      *COBOL code is broken up into 4 divisions.
      *Those divisions, in order, are:
      *IDENTIFICATION DIVISION.
      *ENVIRONMENT DIVISION.
      *DATA DIVISION.
      *PROCEDURE DIVISION.

      *First, we must give our program an ID.
      *Identification division can include other values too,
      *but they are comments only. Program-id is the only one that is mandatory.
       IDENTIFICATION DIVISION.
           PROGRAM-ID.    LEARN.
           AUTHOR.        JOHN DOE.
           DATE-WRITTEN.  05/02/2020.

      *Let's declare some variables.
      *We do this in the WORKING-STORAGE section within the DATA DIVISION.
      *Each data item (aka variable) starts with a level number, 
      *then the name of the item, followed by a picture clause 
      *describing the type of data that the variable will contain.
      *Almost every COBOL programmer will abbreviate PICTURE as PIC.
      *A is for alphabetic, X is for alphanumeric, and 9 is for numeric.
       
      *example:
      01  MYNAME PIC xxxxxxxxxx.    *> A 10 character string.
       
      *But counting all those x's can lead to errors, 
      *so the above code can, and should
      *be re-written as:
      01 MYNAME PIC X(10).
       
      *Here are some more examples:
      01  AGE             PIC      9(3).   *> A number up to 3 digits.
      01  LAST_NAME       PIC      X(10).  *> A string up to 10 characters.
       
      *In COBOL, multiple spaces are the same as a single space, so it is common
      *to use multiple spaces to line up your code so that it is easier for other
      *coders to read.
      01  inyear picture s9(7). *> S makes number signed.
                                 *> Brackets indicate 7 repeats of 9,
                                 *> ie a 6 digit number (not an array).

      *Now let's write some code. Here is a simple, Hello World program.
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
      
      *The above code will output:
      *STARTING PROGRAM
      *HELLO WORLD
      

      
      ********COBOL can perform math***************
      ADD 1 TO AGE GIVING NEW-AGE.
      SUBTRACT 1 FROM COUNT.
      DIVIDE VAR-1 INTO VAR-2 GIVING VAR-3.
      COMPUTE TOTAL-COUNT = COUNT1 PLUS COUNT2.
      
      
      *********PERFORM********************
      *The PERFORM keyword allows you to jump to another specified section of the code,
      *and then to return to the next executable
      *statement once the specified section of code is completed. 
      *You must write the full word, PERFORM, you cannot abbreviate it.

      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLOCOBOL.

      PROCEDURE DIVISION.
         FIRST-PARA.
             DISPLAY 'THIS IS IN FIRST-PARA'.
         PERFORM THIRD-PARA THRU FOURTH-PARA. *>skip second-para and perform 3rd & 4th
         *> then after performing third and fourth,
         *> return here and continue the program until STOP RUN.
   
         SECOND-PARA.
             DISPLAY 'THIS IS IN SECOND-PARA'.
         STOP RUN.
   
         THIRD-PARA.
             DISPLAY 'THIS IS IN THIRD-PARA'.
   
         FOURTH-PARA.
             DISPLAY 'THIS IS IN FOURTH-PARA'.
   
   
      *When you compile and execute the above program, it produces the following result: 
          THIS IS IN FIRST-PARA
          THIS IS IN THIRD-PARA
          THIS IS IN FOURTH-PARA
          THIS IS IN SECOND-PARA
          
          
      **********Combining variables together using STRING ***********
      
      *Now it is time to learn about two related COBOL verbs: string and unstring.

      *The string verb is used to concatenate, or put together, two or more strings.
      *Unstring is used, not surprisingly, to separate a         
      *string into two or more smaller strings. 
      *It is important that you remember to use ‘delimited by’ when you
      *are using string or unstring in your program. 

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


      *The above code will output:
      THE FULL NAME IS: BOB COBB


      *Let’s examine it to see why.

      *First, we declared all of our variables, including the one that we are creating
      *by the string command, in the DATA DIVISION.

      *The action takes place down in the PROCEDURE DIVISION. 
      *We start with the STRING keyword and end with END-STRING. In between we         
      *list what we want to combine together into the larger, master variable. 
      *Here, we are combining FIRST-NAME, a space, and LAST-NAME. 

      *The DELIMITED BY phrase that follows FIRST-NAME and 
      *LAST-NAME tells the program how much of each variable we want to capture. 
      *DELIMITED BY SPACE tells the program to start at the beginning, 
      *and capture the variable until it runs into a space. 
      *DELIMITED BY SIZE tells the program to capture the full size of the variable. 
      *Since we have DELIMITED BY SPACE after FIRST-NAME, the GIBBERISH part is ignored. 

      *To make this clearer, change line 10 in the above code to:

      STRING FIRST-NAME DELIMITED BY SIZE

      *and then re-run the program. This time the output is:

      THE FULL NAME IS: BOB GIBBERISH COBB
```

##Ready For More?

* [GnuCOBOL](https://sourceforge.net/projects/open-cobol/)

