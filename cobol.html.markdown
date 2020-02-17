---
language: COBOL
filename: learncbl.cbl
contributors:
    - ["Mikael Kemstedt", "https://github.com/MikaelKemstedt"]
---

COBOL stands for "**Co**mmon **B**usiness **O**riented **Language**.
It is a high-level programming language first developed in 1959 
(standardised in 1968) and as the name suggests it is designed for developing 
business with mostly file-oriented applications.

It's still in use today, mainly because there are so many applications 
implemented in COBOL and the cost of maintenance is often lower than migrating 
to another language. 
Another part is the "If it ain't broke don't fix it" mentality. 

The syntax is pretty easy, it's almost like talking to the computer. But 
actually using what you've learned is a bit harder since if you're going to be 
working with COBOL you'll need to learn a bunch of other things surrounding it. 
Like:
    JCL (Job Control Language), 
    PL/1 (Programming Language One), 
    CICS (Customer Information Control System), 
    DB2 (Database 2, basically SQL in mainframe),
    as well as different file types and the actual mainframe interface. 

If you want a more in-depth information on those there will be links in the end
to help you get started. 

This guide will use:

    COBOL v6.3.0 as well as the standard IBM z/OS compiler. 



Before we go into the code block.
The first thing you have to think about is the coloumns (80 in total),

The first 6 are reserved for line numbers looking something like this:

00100

00200

00300

...

If you're working on a mainframe you can type in line commands on those numbers.
But if you're on any other platform you can ignore these numbers. 

The 7th columnis reserved for comments, continuation, printer stopper and 
debug indication marks:

<<<<<<< HEAD
\*

\-
=======
*

-
>>>>>>> Update cobol.html.markdown

/

D

Column 8-11 is called "Column A" and column 12-72 is "Column B". 

Column A is where you write the first level of your code, indention matters. 

Column B is where you write everything else, indention does not matter.

Column 73-80 is used for identification purpose. So the program will not read
anything that is written here, except for the system generated numbers. Just 
don't write anything here and you're good.

```cobol
           *> Inline comments look like this.
      *****************************************************************
      * In COBOL you usually write an annotation of when the program was
      * written as well as changed, who wrote / changed it, and what did
      * they do. 
      * Keep in mind everyone got different standards so it might not 
      * look the same for you as it does here. 
      *
      * Program name:    LEARNCBL                               
      * Original author: MIKAEL KEMSTEDT                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 16/02/2020 MIKAEL KEMSTEDT  Started writing the guide.         
      *                                                               
      *****************************************************************
      * There are 4 kinds of levels in COBOL:
      * DIVISION
      * SECTION
      * PARAGRAPH
      * CODE
      
      * As you see below the IDENTIFICATION DIVISION is the highest 
      * level So everything below it will run until it hits another
      * division.
      * Inside this division there are sections. The section will run
      * until it hits another section or the division ends.
      * Inside that are paragraphs, which will run until it hits
      * another paragraph, section, or division.
      * Lastly is the code, which runs until it hits the end of itself,
      * paragraph, section, or division.
      
      * Visual higharchy would be something like this:
      * PROGRAM runs everything below
      *    DIVISION runs everything below
      *       SECTION runs everything below
      *          PARAGRAPH runs everything below
      *             CODE 
      
      * In COBOL you always need 4 divisions, the 1st one is this:
       IDENTIFICATION DIVISION.
      * This is where you write a some information about the program
      * itself. Though only PROGRAM-ID. is the mandatory field.
      * Filenames should always be max 8 characters. Some characters are
      * reserved, these are:
      * AFB
      * AFH
      * CBC
      * CEE
      * CEH
      * CEL
      * CEQ
      * CEU
      * DFH
      * DSN
      * EDC
      * FOR
      * IBM
      * IFY
      * IGY
      * IGZ
      * ILB
      
      * Having one of those 3 characters at the start of your program
      * name might confuse the compiler. Which you do not want. 
      * It's recommended to keep the ID the same as the file name for
      * easier maintanence. 
       PROGRAM-ID.  LEARNCBL.
       AUTHOR. MIKAEL KEMSTEDT. 
       DATE-WRITTEN. 16/02/2020. 
       DATE-COMPILED. 16/02/2020. 
       SECURITY. NON-CONFIDENTIAL.
      *****************************************************************
      *****************************************************************
      * The 2nd division is this one:
       ENVIRONMENT DIVISION.
      * Enviroment division has two sections,
      * One for configuration (all optional):
       CONFIGURATION SECTION.
      * Source computer is where the program will be compiled on.
       SOURCE-COMPUTER. IBM-370.
      * Object computer is where the program will run. 
       OBJECT-COMPUTER. IBM-370.
      * Special names changes the reading of the data that you get. 
       SPECIAL-NAMES. 
      * The most used ones are:
      *    DECIMAL-POINT IS COMMA. 
      * Makes COBOL read 1,000.99 as 1.000,99

           CURRENCY SIGN IS 'USD' WITH PICTURE SYMBOL '$'. 
      * Currency sign is characters while the symbol is well, a symbol.
      * $ is the default currency symbol. 
      * If $ is written as currency sign then it is ignored.

      * And One for files (all optional): 
       INPUT-OUTPUT SECTION.
      * FILE-CONTROL is where you connect external files with your own
      * program as well as define a few extra variables if you want.
      * Everything except the declarations and the 
      * "SELECT <file_var> ASSIGN TO <file_name.ext>." bits are
      * usually handled by like a JCL so it's not something you need to
      * deal with as a COBOL programmer. 

       FILE-CONTROL.
      * Read the following as:
      * Select <file_var> to assign to the file "file.txt"
           SELECT FILEIN ASSIGN TO 'FILEIN.TXT'
      * In mainframe you'll write another vairable name instead of the 
      * file because it is sent by the JCL instead.
      *
      * The one varoable that you almost always want is this one:
              FILE STATUS IS FILEIN-STATUS
      * The ones you want to remember are "00" for success and "10" for 
      * end-of-file. You can find the rest of the codes here:
      * http://ibmmainframes.com/references/a27.html
      
      * You also got the following.
      * You got 3 types of files, sequential, relative, indexed. 
      * The syntax for those are the following:
      * Sequential (most used):
              ACCESS MODE SEQUENTIAL.  
      * Relative:
      *       ACCESS MODE {SEQUENTIAL/DYNAMIC/RANDOM} 
      *       [RELATIVE KEY IS <var>]
      * Indexed:
      *       ACCESS MODE {SEQUENTIAL/DYNAMIC/RANDOM}
          
      *       ALTERNATE RECORD KEY IS <var> [WITH DUPLICATES]
             
      * Following is only needed if you use relative or indexed file.
      * This describes the logical structore of the file.
      * Default is sequential
      *       ORGANIZATION IS {SEQUENTIAL/RELATIVE/INDEXED}

      * Optional in the following make the program create the file if it
      * does not exist. 
      *    SELECT OPTIONAL FILEOUT ASSIGN TO 'FILEOUT.TXT'.
           SELECT FILEOUT ASSIGN TO 'FILEOUT.TXT'
              FILE STATUS IS FILEOUT-STATUS
              ACCESS MODE SEQUENTIAL.
      * Note that the '.' is after the last row to connect all rows 
      * about the file together.
      
      
      * SortfileS will be explained later on.
           SELECT SORTFILE ASSIGN TO 'SORT.TXT'

      * I-O-Control specifies when checkpoints are to be taken and the 
      * storage areas to be shared by different files. Read more about
      * it here:
      * https://www.ibm.com/support/knowledgecenter/SSQ2R2_9.1.1/com.ibm.ent.cbl.zos.doc/PGandLR/ref/rliosioc.html
       I-O-CONTROL. 
      
      * DATA DIVISION is the 3rd division, it contains all the variables
      * and their information that you're using in your program. 
      * ALL the variables that you use have to be defined here. There
      * is no such thing as creating a variable inside the code. 
       DATA DIVISION.
      * There are 3 sections under DATA DIVISiON
      * File section defines how the files you declared above, look.
       FILE SECTION.
      * FD <file_var>.
       FD  FILEIN.
      * Let's say the file looks like this:
      * TRANSACTIONID  AMMOUNT  BALANCEBEFORE  BALANCEAFTER
      * The file declaration can represent that like this:
       01  FILEIN-POST. *> The "group variable"
           05 FILEIN-TRANSID             PIC 9(5)X(5),99.
           05 FILEIN-AMMOUNT             PIC ZZZ,ZZZ,ZZ9.99.
           05 FILEIN-BALANCEBEFORE       PIC ZZZ,ZZZ,ZZ9.99.
           05 FILEIN-BALANCEAFTER        PIC ZZZ,ZZZ,ZZ9.99.
      * What exacly the PIC 9(5)... means will be covered after the file 
      * section.

      * But you might not know exactly what the file looks like, in that
      * case. Just declare it as a single long variable
       FD  FILEOUT.
       01  FILEOUT-POST                  PIC X(80).
      * You can also change other things, similar to the FILE-CONTROL
      * declarations. It would look something like this:
      * FD FILEIN
      *    RECORD CONTAINS 80 CHARACTERS
      *    BLOCK CONTAINS 10 RECORDS
      *    DATA RECORD IS FILEIN-POST

      *    RECORDING MODE IS {F/D/V} 
      * Using this changes recod and block size.
      * F = FIXED LENGTH syntax is like above.

      * D = DYNAMIC LENGTH syntax is:
      *    RECORD IS VARYING IN SIZE 1 TO 80 [DEPENDING ON <var>]
      *    BLOCK CONTRAINS IN SIZE 1 TO 10 [DEPENDING ON <var>]

      * V = VARIABLE LENGTH syntax is:
      *    RECORD CONTAINS 1 TO 80 CHARACTERS
      *    BLOCK CONTRAINS 1 TO 10 CHARACTERS
      * More in-depth here:
      * https://www.mainframestechhelp.com/tutorials/cobol/cobol-file-section.htm
       
      * SortfileS will be explained later on.
       SD SORTFILE.
       01 SORT-POST                      PIC X(80).

      *****************************************************************
      * In WORKING-STORAGE we declare every variable that has its origin
      * in the program. 
       WORKING-STORAGE SECTION.
      * A variable is declared in 3 steps:
      * level-number data-name           picture clause
      * The level-number is two numbers that say what type of variable 
      * it is (var, const, boolean) and at what level it is at.
      * The level-numbers that you use are:
      * 01-49, 66, 77, and 88.
      * 01-49 are general level-numbers that you use to declare normal
      * variables.
      * 66 is not really used anymore, it has to do with groups which 
      * will be explained in a little bit.
      * 77 is not a parent nor child variable. It is generally not used.
      * 88 are booleans. Either true or false.

      * Every variable has to start at a 01 or 77 level in column A.
      * Then you got the data-name or the actual variable name you'll
      * reference when writing your code. 
      * Different places has different standards on how these are named.
      
      * Last part is the declaration of what the variable contains.
      * PIC is short for PICTURE. After PIC You write what type of data
      * it is. Here in the first we have 99 which means two numbers.
      * A = ALPHABETIC (OR SPACE)
      * B = SPACE
      * P = DECIMAL SCALING POSITION (doesn't count in size of the item)
      * S = OPERATIONAL SIGN (doesn't count in size of the item)
      * V = ASSUMED DECIMAL POINT (doesn't count in size of the item)
      * X = ALPHANUMERIC CHARACTER (any from the EBCDIC set)
      * Z = ZERO SUPRESSION CAHRACTER (001.00 -> 1.00)
      * There are some more special ones, you can find them here:
      * https://www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_73/rzasb/picsym.htm#picsym

      * The ones you're going to use 99% of the time are: X, A, 9, V, Z,
      * and S
      
      * When defining a file-status variable, it's two numbers.
      * So we just put two 9s together when declaring it.
       01 FILEIN-STATUS                  PIC 99. 
           88 FILEIN-EOF                     VALUE '10'.
      * 88 levels don't use pic, they use VALUE instead. 
      * if FILEIN-STATUS = 10 then FILEIN-EOF = TRUE 
      * if it's something else     FILEIN-EOF = FALSE.
      * If you have one 88 level it's generally called a switch.
       01 FILEOUT-STATUS                 PIC 99.
           88 FILEOUT-EOF                    VALUE '10'.  
           88 FILEOUT-NOTOPEN                VALUE '48'.
      * If you have multiple, it's called flags.
      
      * You have groups as well. This way you can easily split up a
      * variable into smaller pieces in case you only want to change
      * a small part of it
       01 CURRENT-DATE. *> In this case, this group is declared as:
      * PIC X(8).
      * Groups are always declared as X.
           05 C-YEAR                     PIC 9(4). *> same as PIC 9999.
      * We use 05 in case we want to add something later on and we don't
      * want to change every single level-number. So adding a 03 item
      * above this and below the 01 will make the 03 item the parent
      * and the 01 item the grand parent. 
           05 C-MONTH                    PIC 99.
      * Note the '.' are after every declaration.
           05 C-DAY                      PIC 99.

      * We can also declare tables. (avoid if you're not sure they
      * will hold for ~50 years, also avoid big ones since they take up
      * a lot of memory)
       01 NOTABLE.
      * This will make all the variables below have 10 instances.
           05 YESTABLE OCCURS 10
               INDEXED BY YES-INDEX. *> Dot after this since it all goes 
                                    *> together
      * You don't have to declare the index combined with the table,
      * defining it alone is also ok. But linking it to the table 
      * prevents you from using it somewhere else.
               10 YESTABLE-1              PIC A(3).
               *> You can make it variable only.
               10 YESTABLE-2              PIC X OCCURS 2.
               *> YESTABLE-2 has 10 * 2 instances.

      * You can also nest them until you run out of level-numbers.
      * It gets confusing after a couple of levels though.
                   15 YESNEST1                          OCCURS 10.
                       20 YESNEST2                      OCCURS 10.
                           25 YESNEST3                  OCCURS 10.
                               30 YESNEST4              OCCURS 10.
      * They don't have to be indented by 4 every time in column B. But
      * it has to be more than the one above.
                                   35 YESNEST5          OCCURS 10.
                                    45 YESNEST6         OCCURS 10000000.
                          *> It also doesn't have to be on the same row.
                                                                      49 
                                                                    YEET
                                                             PIC ZZ9.99.
      * Just because you can doesn't mean you should. 
      * Try to keep the declarations tidy.

      * There are a few data typs that we can add to after the 
      * pic clause. One of them is COMP.
       01 NO-INDEX                       PIC 99 USAGE IS COMP.
                                 *> Just PIC 99 COMP. Also works.
      * The data types you have for numbers (only S and 9) are:
      * COMP / COMP-1 / COMP-4 / BINARY = 2 or 4 bytes int
      * COMP-2 = 8 bytes int
      * COMP-3 / PACKED DECIAMAL = (n + 1) / 2 bytes float
      * The ones you're going to use the most are COMP and COMP-3

      * Now we can talk about the number-level 66.
      * First we make a normal group.
       01 TIMESTAMP.
           05 T-YEAR                     PIC X(4) VALUE '2020'. 
           *> Since it's x we have to use '' for the value.
           05 T-MONTH                    PIC X(2). 
           *> Don't use VALUE if you change the value later on.
           05 T-DAY                      PIC X(2). 
           05 T-HOUR                     PIC 9(2) VALUE 00.
           *> With 9 you get an error if you use ''. 
           05 T-MIN                      PIC 9(2). 
           05 T-SEC                      PIC 9(2). 
      
      * Now the 66 renames the variables above. 
       66 T-DATE RENAMES T-YEAR THRU T-DAY.
      * So now if you only want the date instead of the full timestamp,
      * you only get the date when you reference T-DATE.
      * Note that this only references the variables that you enter
      * it doesn't change them.

      * If you have something like a date and you want to make it look
      * like this instead: 2020-01-01
      * You can use filler like this:
       01 FIXED-TIMESTAMP.
           05 FT-YEAR                    PIC X(4).
           05 FILLER                     PIC X VALUE '-'.
           05 FT-MONTH                   PIC X(2). 
           05 FILLER                     PIC X VALUE '-'.
           05 FT-DAY                     PIC X(2). 
           05 FILLER                     PIC X VALUE 'T'.
           05 FT-HOUR                    PIC 9(2). 
           05 FILLER                     PIC X VALUE ':'.
           05 FT-MIN                     PIC 9(2). 
           05 FILLER                     PIC X VALUE ':'.
           05 FT-SEC                     PIC 9(2).
      * The good thing about this is that you can't change the value
      * directly, you have to change the whole FIXED-TIMESTAMP. Which 
      * you shouldn't do anyway, so using fillers prevents small 
      * mistakes like that. 

      * Some variables we will use in the code below.
       01 A                              PIC X.
       01 B                              PIC X.
       01 C                              PIC 9  COMP. 
       01 D                              PIC 9  COMP.
       01 R                              PIC 9  COMP. 
       01 TEN                            PIC 99 COMP.     
       01 ELEVEN                         PIC 99 COMP.      
       01 HAMBURGER                      PIC X(3).
       01 HAM-C                          PIC 9 COMP.
       01 TOP-BUN                        PIC X.
       01 BOTTOM-BUN                     PIC X.
       01 MRINSPECT                      PIC X(30).
       01 INSP1                          PIC 99 COMP.     
       01 INSP2                          PIC 99 COMP. 
       01 INSP3                          PIC 99 COMP.     
       01 INSP4                          PIC 99 COMP. 
       01 THEGOODSTUFF                   PIC X.
       01 WS-FILEIN-POST.
           05 WS-FILEIN-TRANSID          PIC 9(5)X(5),99.
           05 WS-FILEIN-AMMOUNT          PIC ZZZ,ZZZ,ZZ9.99.
           05 WS-FILEIN-BALANCEBEFORE    PIC ZZZ,ZZZ,ZZ9.99.
           05 WS-FILEIN-BALANCEAFTER     PIC ZZZ,ZZZ,ZZ9.99.
      * Last thing is the linkage-section.
       LINKAGE SECTION.
      * This is where variables that are used from a parent program 
      * are declared.
       01 LINKAGE-VARIABLE1              PIC X(20).
      * Only 01 level variables can be linked.
       01 LINKAGE-VARIABLE2              PIC X(20).
      * Linking groups is also OK.
       01 LINKAGE-VARIABLE3. *> But this one has to be the one that is
      * declared after the "USING" part below. 
           05 LINKAGE-CHILD              PIC X(10).
       01 LINKAGE-VARIABLE4              PIC X(20).

      *****************************************************************
      * The 4th and last division. Under here we write all the code.
      * If you have a parent program that calls this program with
      * variables you have to add USING <var>... 
      * and then add the <var>... to the linage section.
      *PROCEDURE DIVISION.
       PROCEDURE DIVISION USING LINKAGE-VARIABLE1 
                                LINKAGE-VARIABLE2
                                LINKAGE-VARIABLE3
                                LINKAGE-VARIABLE4.

      * The code starts right after the PRECEDURE DIVISION so you can 
      * write whatever you want after that and it will run. In general 
      * you want to start with a section or paragraph.
       A100-PERFORM-CALL SECTION.
           
      * To start another section / paragraph you use perform.
           PERFORM B100-PARAGRAPHS
      * This makes the code jump to that section / paragraph and then
      * come back once it's done. 
           PERFORM C100-INITIALIZE  
      * If you want to call another program you do this:
           CALL 'EASYCBL' USING A
      * Starts the cobol program with the PROGRAM-ID of EASYCBL

           . *> A100-PERFORM-CALL SECTION ends here with the .

       B100-PARAGRAPHS SECTION.
      

      * In here you can have paragraphs
       B110-FILL-DATE. *> Note that pragraphs are written exactly the
      * same as a section except you remove the word "SECTION" from the
      * end. 
           
           DISPLAY 'HELLO WORLD'

           . *> B110-FILL-DATE ends here
      * But because B110-FILL-DATE is a paragraph the section hasn't 
      * ended and we keep going.
      *> Rewrite that line
       B120-DISPLAY-DATE.
           DISPLAY 'HELLO WORLD 2'

           . *> Paragraph ends here section keeps going
      * I wouldn't recommend letting it bleed over like this. Instead 
      * just call the paragraph in the end of the paragraph above for 
      * easier maintanence. 
       
      * You don't need to end the section in some way. 
      * It ends as the next section starts
      * But for clarity you can do something like this:
       SECTION-END.
           EXIT SECTION
           .
      

       C100-INITIALIZE SECTION.
      * Statements
      * Before you start coding you want to initialize the variables
           INITIALIZE CURRENT-DATE 
      * You can initialize groups, that initializes every variable under
      * it as well.
                      NOTABLE
                      NO-INDEX
                      YES-INDEX
                      TIMESTAMP 
                      T-DATE 
                      FIXED-TIMESTAMP
      * This resets the variables to either space (or . ) or 0
           
           PERFORM C110-MOVE-AND-MATH 
           PERFORM C120-DISPLAY-ACCEPT 
           PERFORM C130-IF-EVALUATE  
           PERFORM C140-STRING-UNSTRING-INSPECT 
           PERFORM C150-INDEX-AND-TABLES 
           PERFORM C160-FILE-STATEMENTS 
           PERFORM C170-LOOPS
      *    PERFORM C200-NO-GO-ZONE
           .
           
       C110-MOVE-AND-MATH SECTION.
      
      * What you will use the most is this:
           MOVE 'B' TO A
           MOVE A TO B
      * With move you just copy one content into the other. 
      * In other languages this would be the same as A = B

           SET YES-INDEX TO 1
      * YES-INDEX = 1
           SET YES-INDEX UP BY 1
           SET YES-INDEX DOWN BY 1
      
      * You have all the mathamatical statements like 
      * (can only use variables declared at least as 9/9V9/S9/S9V9):
           ADD 1.2 2 3 4 TO TEN *> TEN = 10, the .2 is ignored
           ADD 1.25 2.25 3.25 4.25 TO ELEVEN *> ELEVEN = 11
           
           MULTIPLY 1 BY 2 GIVING C *> C = 4

           SUBTRACT 1 FROM 2 GIVING D *> D = 1
           INITIALIZE C 
      * Since we used it and want to use it again,
      * it's a good idea to intialize it again
           DIVIDE 10 BY 3 GIVING C REMAINDER R  *> C = 3, R = 1
      
      * But compute is better if you need to do math
           COMPUTE C = (1 + 2) / 2 * (3 ** 2) ** 0.5
      * + = Addition
      * - = Subtraction
      * * = Multiplication
      * / = Division
      * ** = Exponentiation
           .
            
       C120-DISPLAY-ACCEPT SECTION.
      * To write to the console (or log if you use mainframe)
           DISPLAY 'HELLO WORLD'
      * Avoid these unless you're writing an error message, that way
      * you don't forget and clog up a log file in production.
      * It's recommended to write an error message something like this 
      * so it stands out:
           DISPLAY '**************************************************'
           DISPLAY 'GOT AN ERROR: *ERROR*                             '
           DISPLAY 'MOREINFO                                          ' 
           DISPLAY '**************************************************'

      * Getting a value from the user can be done with:
           ACCEPT A *> Waits for user input
      * It can also accept values from JCL but that's not very common.
           .

       C130-IF-EVALUATE SECTION.
      * If statements
           IF C NOT = 2
               CONTINUE *> Don't do anything
           ELSE
               COMPUTE C = 2 
           END-IF *> Every IF you do has to have a END-IF
      * You can end it with a . but that should be avoided.
      * While you can nest if statements, 
      * it's ugly and should be avoided as well.
           IF A = 'A'
               CONTINUE
           ELSE *> ELSE IFs aren't a thing
               IF A = 'A'
                   CONTINUE
               ELSE 
                   IF A = 'A'
                       CONTINUE
                   ELSE 
                       IF A = 'A'
                           CONTINUE
                       ELSE 
                           IF A = 'A'
                               CONTINUE
                           ELSE 
                               IF A = 'A'
                                   CONTINUE
                               ELSE 
                                   CONTINUE
           . *> Don't do this.
                          
      * Instead use Evaluate:
           EVALUATE TRUE *> true if you want to check an expression
           WHEN 1 = 2
               CONTINUE
           
           WHEN 2 - 2 = 5
           WHEN 2 + 2 = 5
               CONTINUE *> When one of the two above are true
           
           WHEN FILEIN-EOF *> Can also use switches / flags
               CONTINUE 
           WHEN OTHER
              CONTINUE

           END-EVALUATE *> Like if, end every evaluate

           EVALUATE A *> Basically a switch statement
           WHEN 'A' *> If variable A = 'A'
               CONTINUE

           WHEN 'B' *> If variable A = 'B'
               CONTINUE

           END-EVALUATE

      * You can have multiple contitions if you want as well:
           EVALUATE TRUE ALSO A
           WHEN 1 = 1 ALSO 'A'
               CONTINUE 

           WHEN 1 = 1 ALSO 'B' 
               CONTINUE 

           WHEN 1 GREATER THAN 2 ALSO 'C' *> Can also write it out
               CONTINUE

           WHEN 1 < 2 ALSO 'A' THRU 'D' *> ... ALSO (A OR B OR C OR D)
               CONTINUE

           END-EVALUATE
           .     
           
       C140-STRING-UNSTRING-INSPECT SECTION.
      * String handeling
      * To add two string together you use STRING.
           STRING '(S' DELIMITED BY SIZE
      * DELIMITED BY is mandatory, by what is where the string ends.
      * SIZE is the whole string.
                  '|)' DELIMITED BY SIZE 
             INTO HAMBURGER 
             WITH POINTER HAM-C *> Counts how big the new string is.
      * HAMBURGER is only declared as PIC X(3). So you'll get an error
             ON OVERFLOW DISPLAY 'HAMBURGER IS TOO BIG'
             NOT ON OVERFLOW DISPLAY 'HAMBURGER WAS GREAT'
           END-STRING
      * HAMBURGER = '(S|'

           INITIALIZE HAMBURGER 
      * Lets redo it so it fits
           STRING '(' DELIMITED BY SIZE
                  '|)' DELIMITED BY SIZE 
             INTO HAMBURGER 
           END-STRING                                        
      
      * Now you might want to split a string into small ones so you can
      * see what is what, then you use UNSTRING
           UNSTRING HAMBURGER DELIMITED BY ''
               INTO TOP-BUN
                    THEGOODSTUFF
                    BOTTOM-BUN      
      * POINTER, OVERFLOW CAN ALSO BE WRITTEN HERE         
           END-UNSTRING 
      * If one of the variables is too small it will truncate it.
      
      * What if we want to check if a variable has a specific character
      * in it? Then INSPECT is what you're looking for
           
           MOVE 'ABCDEF.GHIJKLM/NOPQRST?UVWXYZ/123' TO MRINSPECT 

           INSPECT MRINSPECT TALLYING INSP1
               FOR CHARACTERS BEFORE INITIAL '/'
               *> INSPT1 = 14
               *> if it's not found then it is the same as the length
               *> of all the characters in MRINSPECT
      
      * Replace something? INSPECT
           INSPECT MRINSPECT TALLYING INSP2 
               FOR ALL '/'
               REPLACING CHARACTERS BY '1'
               AFTER INITIAL '/'
               BEFORE '3'
               *> INSP2 = 2
               *> MRINSPECT = ABCDEF.GHIJKLM/111111111111111113
      
      * Convert? INSPECT
           INSPECT MRINSPECT 
               CONVERTING 'GHIJKLM' 
               TO 'ABCDEF.' 
               *> MRINSPECT = ABCDEF.ABCJKLM/111111111111111113
           .

       C150-INDEX-AND-TABLES SECTION.
      * Table handling
      * To get a specific entry in a table you do table_name(index)
      * Index starts at 1.
      * So getitng the first entry in a table looks like this:
           DISPLAY YESTABLE(1)
      * You can think of variables as a small table of characters
      * You have to define the start from where on the variable you 
      * want to start getting the characters you want. Looks like this:
           DISPLAY MRINSPECT(1:10) *> Start at the first character
      * and get the next 10. -> 'ABCDEF.GHI'

      * You don't have to define the end.
           DISPLAY MRINSPECT(10:) *> Starts at character 10 and gets all
      * the following characters
           
      * Get the 2nd character in the 3rd table entry
           DISPLAY YESTABLE(3)(2:1)

      * Put something into the variable YEET
           MOVE 'YOTE' TO YEET(45678)

      * When you got a table and you want to know if you have a certain 
      * variable inside it, then you can use SEARCH
           SEARCH YESTABLE
           AT END
               DISPLAY 'NOT FOUND' *> It's empty so this will show.
           WHEN YESTABLE(YES-INDEX) = 'FOUND :)' *> Since we linked the 
      * index to the table it automatically increments.
               DISPLAY YESTABLE(YES-INDEX)
           END-SEARCH
           .

       C160-FILE-STATEMENTS SECTION.
      * File handling
      * First you have to open the file that you want to work with.
      * There are 4 ways of opening a file.
           OPEN INPUT FILEIN      *> For reading
           OPEN OUTPUT FILEOUT    *> Write, clears the file
      *    OPEN EXTEND            *> Appent writing
      *    OPEN I-O               *> Both read and write
      * Opening a file that is already open will give an error
      *    OPEN INPUT FILEIN      *> FAILS

      * To read a file you do:
           READ FILEIN 
           END-READ *> IMPORTANT, don't forget the END-READ

      * Read it again to get the next row.
           READ FILEIN AT END SET FILEIN-EOF TO TRUE
      * If it reached the end of the file, set FILEIN-EOF to true.
           END-READ
      * The file status is changed at the end of the file, so putting a 
      * flag on the status variable instead, reduces the clutter. 

      * The content of the file is put into the file declaration:
      * 01  FILEIN-POST. <--- this
      *    05 FILEIN-TRANSID             PIC 9(5)X(5),99.
      *    05 FILEIN-AMMOUNT             PIC ZZZ,ZZZ,ZZ9.99.
      *    05 FILEIN-BALANCEBEFORE       PIC ZZZ,ZZZ,ZZ9.99.
      *    05 FILEIN-BALANCEAFTER        PIC ZZZ,ZZZ,ZZ9.99."
      
      * The first 12 characters are thus in transid
      * The next 11 are in ammount
      * 11 in balancebefore
      * 11 in balanceafter
      
      * If you want to reset row that it is on, just close and open the
      * file agian.
           CLOSE FILEIN
           OPEN INPUT FILEIN
           
           READ FILEIN
           END-READ
      * And you're back to the first row again.
       
      * If you want the content to be written to another variable as 
      * well as the declaration:
           READ FILEIN INTO WS-FILEIN-POST
           END-READ

      * Then we have write.
           WRITE FILEOUT 
           END-WRITE
      * Writes from the FILEOUT file declaration.
      * You can do the same as READ, use another variable to write.
           WRITE FILEOUT FROM WS-FILEIN-POST

      * Keep the files open as short as possible to minimize memory 
      * usage as well as run time. 
           CLOSE FILEOUT FILEIN  *> You can close as well as open
      * multiple files at once.
           
      * I-O Files  
           OPEN I-O FILEOUT 

      * Read the post until you find what you're looking for
           READ FILEOUT                                                 
           END-READ
           
           MOVE 'REWRITTEN' TO FILEOUT-POST

           REWRITE FILEOUT *> Rewrite that line
           END-REWRITE 

      * And at last we close the files that we opened at the start
           CLOSE FILEIN FILEOUT

           PERFORM C161-SORT-FILE 
           PERFORM C162-SORT-TABLE
           PERFORM C169-MERGE-FILE
           .

       C161-SORT-FILE SECTION.
      * Now's the time for sort files. 
           SORT SORTFILE ON ASCENDING KEY SORT-POST 
               USING FILEIN GIVING FILEOUT
      * Here is how it goes:
      * Opens SORTFILE in I-O, FILEIN as INPUT, and FILEOUT as OUTPUT
      * FILEIN records goes into SORTFILE
      * SORTFILE sorts them depending on SORT-POST
      * SORTFILE records goes to FILEOUT
      * Closes all the files also deletes the SORTFILE file

      * Some other variations:
           SORT SORTFILE ON DESCENDING KEY SORT-POST 
               USING FILEIN GIVING FILEOUT

      * Sort into the same file
           SORT SORTFILE ON DESCENDING KEY SORT-POST 
               USING FILEIN GIVING FILEIN

      * Duplicates can be handled seperatlly, to do that use:
           SORT SORTFILE ON DESCENDING KEY SORT-POST 
               WITH DUPLICATES IN ORDER  
               *> Just DUPLICATES is enough as well
               USING FILEIN GIVING FILEOUT
      
      * If you want to handle the inpit and output yourself you can do 
      * that with input and output procedures
           SORT SORTFILE ON ASCENDING KEY SORT-POST
               INPUT PROCEDURE C163-INPUT-PROCEDURE 
      * Is called before it gets sent to be sorted
               OUTPUT PROCEDURE C164-OUTPUT-PROCEDURE
      * Is called after it got sorted
      * You can put a lot of logic in those procedures,
      * These ones are just enough to sent one whole file. 

      * Rarely used:
      * If you defined ALPHABET under SPECIANL-NAMES you can also use
           SORT SORTFILE ON DESCENDING KEY SORT-POST 
               *> SEQUENCE <ALPHABET_NAME>
               USING FILEIN GIVING FILEOUT
      * To be used in alphanumeric comparisons. 
      
      *    MERGE
      
      * More info on SORT here:
      * https://www.ibm.com/support/knowledgecenter/en/SS6SG3_6.3.0/lr/ref/rlpssort.html
           .
       
      * These sections / paragraphs are special in a way that allows 
      * them to use some statements that you can't use elsewhere
       C163-INPUT-PROCEDURE SECTION.
      * Since this is the input procedure we need to open and read
      * form the input file.
           OPEN INPUT FILEIN

           READ FILEIN
           END-READ
           
      * Loops will be explained later, this one goes until it reaches
      * the end of the file.
           PERFORM UNTIL FILEIN-EOF
               *> Check if the row is '123'
               IF FILEIN-POST NOT = '123'
                   *> if it's not, release / send the row to SORT
                   RELEASE SORT-POST FROM FILEIN-POST
               END-IF
               
               *> Read another row
               READ FILEIN
               END-READ
           END-PERFORM
           
           *> Close the file.
           CLOSE FILEIN
           .

       C164-OUTPUT-PROCEDURE SECTION.
      * Open it for output since we are in the output procedure.
           OPEN OUTPUT FILEOUT
      
           *> Return instead of release. Return the row from SORT to
           *> FIELOUT-POST
           RETURN SORT-POST INTO FILEOUT-POST 
               AT END MOVE 'A' TO A
           END-RETURN
           
           *> Another loop that does the same thing as above.
           PERFORM UNTIL A = 'A'
               RETURN SORT-POST INTO FILEOUT-POST 
                   AT END MOVE 'A' TO A
               END-RETURN
           END-PERFORM

           CLOSE FILEOUT
           .
       
       C162-SORT-TABLE SECTION.
      * Basically the same thing as with the files, but without the
      * procedures nor files. 
           SORT SORTFILE ON ASCENDING KEY SORT-POST 

           SORT SORTFILE ON DESCENDING KEY SORT-POST 

           SORT SORTFILE ON ASCENDING KEY SORT-POST 
               DUPLICATES
               *>SEQUENCE

           SORT SORTFILE ON ASCENDING KEY SORT-POST 
           .
       
       C169-MERGE-FILE SECTION.
      * You can also merge two files if they are sorted by the same key
           MERGE SORTFILE ASCENDING FILEIN-TRANSID 
               USING FILEIN FILEOUT  *> 1 or more files here
               *> SEQUENCE can be used
               GIVING FILEIN *> Output procedure can be used here
           .

       C170-LOOPS SECTION.
      * Loops are really easy to do in COBOL.

      * Going back a bit, when you read or write a file, you want to do 
      * that a few times, so putting that in a loop is what you have to
      * do. 
      * BUT, since the file declarations are empty, you want to read it
      * once before you start. and then put a read / write at the end 
      * of the loop as well.
           OPEN INPUT FILEIN
           PERFORM C171-READ-LOOP UNTIL FILEIN-EOF *> Basically a  
                                                   *> while loop
           CLOSE FILEIN

      * A for loop would look like this
           PERFORM VARYING C FROM 1 BY 1 UNTIL C = 9
               DISPLAY C
           END-PERFORM *> Inline perform need to be ended.

      * Counting down:
           PERFORM VARYING C FROM 9 BY -1 UNTIL C = 0
               DISPLAY C
           END-PERFORM

      * You can have multiple conditions
           PERFORM VARYING C FROM 1 BY 2 UNTIL (C = 9) AND (D = 0)
      * But only one counter inside the perform statement
               SET D TO 0           
           END-PERFORM

      * Only do it a couple of times
           PERFORM 2 TIMES
               DISPLAY '2 TIMER'
           END-PERFORM
           .
      
       C171-READ-LOOP SECTION.
           DISPLAY FILEIN-POST

           READ FILEIN
           END-READ
           .

       C200-NO-GO-ZONE SECTION.
      * Two statements you want to avoid:
           ALTER Z999-TERMINATE TO A100-PERFORM-CALL  
      * This just makes it incredibly hard to maintain. NEVER use this.
      * It's also deprecated but still works for backwards compatability 
           GO TO Z999-TERMINATE *> We altered it so we just start over.
      * Only time you may use go to is when you have to exit the code 
      * because you go an error or something like that. 
           .

       Z999-TERMINATE.
           GOBACK
           .
```


## Further Reading

 * [IBM Documentation](https://www.ibm.com/support/knowledgecenter/en/SS6SG3_6.3.0/welcome.html)
 * [IBM Programming Guide](https://www.ibm.com/support/knowledgecenter/en/SS6SG3_6.3.0/pg/abouthst.html)
