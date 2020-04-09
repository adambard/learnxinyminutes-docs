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
      *to store the index number of the punched card..)
      *A * in column 7 means a comment.
      *In legacy COBOL, a comment can only be a full line.
      *Modern COBOL doesn't require fixed columns and uses *> for
      *a comment, which can appear in the middle of a line.
      *Legacy COBOL also imposes a limit on maximum line length.
      *Keywords have to be in capitals in legacy COBOL,
      *but are case insensitive in modern.

      *First, we must give our program ID.
      *Identification division can include other values too,
      *but they are comments only. Program-id is mandatory.
       identification division.
       program-id. learn.

      *Let's declare some variables.
       data division.
       working-storage section.

      *Variables are specified by a "picture" - how they should be
      *displayed, and variable type is inferred from this.
      *The "01" value is the level number which is used for building
      *data structures.
       01  myname picture xxxxxxxxxx.    *> A 10 character string.
       01  age picture 999.   *> A number up to 3 digits.
       01  valx picture 999.  *> Another number up to 3 digits.
       01  inyear picture s9(7). *> S makes number signed.
                                 *> Brackets indicate 7 repeats of 9,
                                 *> ie a 6 digit number (not an array).

      *Now let's write some code.
       procedure division.

       main-procedure.
            *> COBOL is the language that uses DISPLAY instead of PRINT.
            *> Note: no full stops after commands. Only after the LAST
            *> command.
            display "Hello. What's your name?"

            *> Let's input a string.
            *> If input too long, later characters are trimmed.
            accept myname
            display "Hello " myname    *> We can display several things.
            display "How old are you?"

            *> Let's input a number.
            *> If input too long, EARLIER characters are trimmed.
            accept age

            display age *> Left-padded to three chracaters with zeroes,
                        *> because of the defined PICTURE for age.

            *> We have two ways of doing a FOR loop.
            *> Old style way: doesn't give an index.
            perform age times
                display "*" with no advancing *> Ie, no newline at end
            end-perform
            display "." *> Output buffer isn't flushed until newline.

            *> New style way: with an index.
            perform varying valx from 1 by 1 until valx > age
                display valx "-" with no advancing
            end-perform
            display "."

            *> If tests are still good old if tests.
            if myname = "Bob" then
                display "I don't like Bob."
            else
                display "I don't know you."
            end-if

            *> There are two ways of doing subprograms and calling
            *> them.
            *> The simplest way: a paragraph.
            perform subparagraph

            *> The complex way, with parameters and stuff.
            call "eratosthenes" using age returning valx

            display "There were " valx " primes."

            stop run.

       subparagraph. *> Marks the top of an internal subprogram.
           *> Shares variable score with its caller.
           
           *> Read year from system timer.
           *> Remember the whole "year 2000 crisis"? The yyyyddd 
           *> option was added in response to that.
           accept inyear from day yyyyddd.

           *> We can do math step-by-step like this...
           divide 1000 into inyear.
           subtract age from inyear.

           display "You were born in " inyear "."

           *> Or we can just use expressions.
           compute inyear = 1970 - inyear.

           if inyear >= 0 then
               display "When you were " inyear ", " with no advancing
           else
               display inyear " years before you were born, " with no
                 advancing
           end-if

           display "COBOL was the most popular language in the world."
           . *> You can put the final . on a new line if it's clearer.


      *If we want to use a subprogram, we use literally a subprogram.
      *This is the entire program layout, repeated for the 
      *eratosthenes subroutine.
       identification division.
       program-id. eratosthenes.

       data division.
       working-storage section.
      *Declare an array.
      *We can declare a variable to use as an index for it at the
      *same time.
       01  sieve pic 9 occurs 999 times indexed by sa, sb.
           *> Standard cobol doesn't have a boolean type.
       01  pstart pic 999.
       01  counter pic 999.

      *Our parameters have to be declared in the linkage section.
      *Their pictures must match the values they're called with.
       linkage section.
       01  maxvalue picture 999.

      *"using" declares our actual parameter variables.
      *"returning" declares the variable value returned at end.
       procedure division using maxvalue returning counter.
       main-procedure.

           display "Here are all the primes up to " maxvalue "."

           perform varying sa from 1 by 1 until sa > maxvalue
               move 1 to sieve (sa)
           end-perform

           perform varying sa from 2 by 1 until sa > maxvalue
               if sieve(sa) = 1 then
                   compute pstart = sa + sa
                   perform varying sb from pstart by sa until sb >
                   maxvalue
                       move 0 to sieve(sb)
                   end-perform
               end-if
           end-perform

           initialise counter  *> To zero by default for a number.

           perform varying sa from 2 by 1 until sa > maxvalue
               if sieve(sa) = 1 THEN
                   display sa
                   add 1 to counter
               end-if
           end-perform.

       end program eratosthenes.

       end program learn.

```

##Ready For More?

* [GnuCOBOL](https://sourceforge.net/projects/open-cobol/)

