---
language: prolog
filename: learnprolog.pl
contributors:
    - ["hyphz", "http://github.com/hyphz/"]
---

Prolog is a logic programming language first specified in 1972, and refined into multiple modern implementations.

```
% This is a comment.

% Prolog treats code entered in interactive mode differently
% to code entered in a file and loaded ("consulted").
% This code must be loaded from a file to work as intended.
% Lines that begin with ?- can be typed in interactive mode.
% A bunch of errors and warnings will trigger when you load this file
% due to the examples which are supposed to fail - they can be safely
% ignored.

% Output is based on SWI-prolog 7.2.3. Different Prologs may behave
% differently.

% Prolog is based on the ideal of logic programming.
% A subprogram (called a predicate) represents a state of the world.
% A command (called a goal) tells Prolog to make that state of the world
%   come true, if possible.

% As an example, here is a definition of the simplest kind of predicate:
% a fact.

magicNumber(7).
magicNumber(9).
magicNumber(42).

% This introduces magicNumber as a predicate and says that it is true
% with parameter 7, 9, or 42, but no other parameter. Note that
% predicate names must start with lower case letters. We can now use
% interactive mode to ask if it is true for different values:

?- magicNumber(7).                   % True
?- magicNumber(8).                   % False
?- magicNumber(9).                   % True

% Some older Prologs may display "Yes" and "No" instead of True and
% False.

% What makes Prolog unusual is that we can also tell Prolog to _make_
% magicNumber true, by passing it an undefined variable. Any name
% starting with a capital letter is a variable in Prolog.

?- magicNumber(Presto).              % Presto = 7 ;
                                     % Presto = 9 ;
                                     % Presto = 42.

% Prolog makes magicNumber true by assigning one of the valid numbers to
% the undefined variable Presto. By default it assigns the first one, 7.
% By pressing ; in interactive mode you can reject that solution and
% force it to assign the next one, 9. Pressing ; again forces it to try
% the last one, 42, after which it no longer accepts input because this
% is the last solution. You can accept an earlier solution by pressing .
% instead of ;.

% This is Prolog's central operation: unification. Unification is
% essentially a combination of assignment and equality! It works as
% follows:
%  If both sides are bound (ie, defined), check equality.
%  If one side is free (ie, undefined), assign to match the other side.
%  If both sides are free, the assignment is remembered. With some luck,
%    one of the two sides will eventually be bound, but this isn't
%    necessary.
%
% The = sign in Prolog represents unification, so:

?- 2 = 3.                            % False - equality test
?- X = 3.                            % X = 3 - assignment
?- X = 2, X = Y.                     % X = Y = 2 - two assignments
                                     % Note Y is assigned too, even though it is
                                     % on the right hand side, because it is free
?- X = 3, X = 2.                     % False
                                     % First acts as assignment and binds X=3
                                     % Second acts as equality because X is bound
                                     % Since 3 does not equal 2, gives False
                                     % Thus in Prolog variables are immutable
?- X = 3+2.                          % X = 3+2 - unification can't do arithmetic
?- X is 3+2.                         % X = 5 - "is" does arithmetic.
?- 5 = X+2.                          % This is why = can't do arithmetic -
                                     % because Prolog can't solve equations
?- 5 is X+2.                         % Error. Unlike =, the right hand side of IS
                                     % must always be bound, thus guaranteeing
                                     % no attempt to solve an equation.
?- X = Y, X = 2, Z is Y + 3.         % X = Y, Y = 2, Z = 5.
                                     % X = Y are both free, so Prolog remembers
                                     % it. Therefore assigning X will also
                                     % assign Y.

% Any unification, and thus any predicate in Prolog, can either:
% Succeed (return True) without changing anything,
%   because an equality-style unification was true
% Succeed (return True) and bind one or more variables in the process,
%   because an assignment-style unification was made true
% or Fail (return False)
%   because an equality-style unification was false
% (Failure can never bind variables)

% The ideal of being able to give any predicate as a goal and have it
% made true is not always possible, but can be worked toward. For
% example, Prolog has a built in predicate plus which represents
% arithmetic addition but can reverse simple additions.

?- plus(1, 2, 3).                    % True
?- plus(1, 2, X).                    % X = 3 because 1+2 = X.
?- plus(1, X, 3).                    % X = 2 because 1+X = 3.
?- plus(X, 2, 3).                    % X = 1 because X+2 = 3.
?- plus(X, 5, Y).                    % Error - although this could be solved,
                                     % the number of solutions is infinite,
                                     % which most predicates try to avoid.

% When a predicate such as magicNumber can give several solutions, the
% overall compound goal including it may have several solutions too.

?- magicNumber(X), plus(X,Y,100).    % X = 7, Y = 93 ;
                                     % X = 9, Y = 91 ;
                                     % X = 42, Y = 58 .
% Note: on this occasion it works to pass two variables to plus because
% only Y is free (X is bound by magicNumber).

% However, if one of the goals is fully bound and thus acts as a test,
% then solutions which fail the test are rejected.
?- magicNumber(X), X > 40.           % X = 42
?- magicNumber(X), X > 100.          % False

% To see how Prolog actually handles this, let's introduce the print
% predicate. Print always succeeds, never binds any variables, and
% prints out its parameter as a side effect.

?- print("Hello").                   % "Hello" true.
?- X = 2, print(X).                  % 2 true.
?- X = 2, print(X), X = 3.           % 2 false - print happens immediately when
                                     % it is encountered, even though the overall
                                     % compound goal fails (because 2 != 3,
                                     % see the example above).

% By using Print we can see what actually happens when we give a
% compound goal including a test that sometimes fails.
?- magicNumber(X), print(X), X > 40. % 7 9 42 X = 42 .

% MagicNumber(X) unifies X with its first possibility, 7.
% Print(X) prints out 7.
% X > 40 tests if 7 > 40. It is not, so it fails.
% However, Prolog remembers that magicNumber(X) offered multiple
% solutions. So it _backtracks_ to that point in the code to try
% the next solution, X = 9.
% Having backtracked it must work through the compound goal
% again from that point including the Print(X). So Print(X) prints out
% 9.
% X > 40 tests if 9 > 40 and fails again.
% Prolog remembers that magicNumber(X) still has solutions and
% backtracks. Now X = 42.
% It works through the Print(X) again and prints 42.
% X > 40 tests if 42 > 40 and succeeds so the result bound to X
% The same backtracking process is used when you reject a result at
% the interactive prompt by pressing ;, for example:

?- magicNumber(X), print(X), X > 8.  % 7 9 X = 9 ;
                                     % 42 X = 42.

% As you saw above we can define our own simple predicates as facts.
% More complex predicates are defined as rules, like this:

nearby(X,Y) :- X = Y.
nearby(X,Y) :- Y is X+1.
nearby(X,Y) :- Y is X-1.

% nearby(X,Y) is true if Y is X plus or minus 1.
% However this predicate could be improved. Here's why:

?- nearby(2,3).                      % True ; False.
% Because we have three possible definitions, Prolog sees this as 3
% possibilities. X = Y fails, so Y is X+1 is then tried and succeeds,
% giving the True answer. But Prolog still remembers there are more
% possibilities for nearby() (in Prolog terminology, "it has a
% choice point") even though "Y is X-1" is doomed to fail, and gives us
% the option of rejecting the True answer, which doesn't make a whole
% lot of sense.

?- nearby(4, X).                     % X = 4 ;
                                     % X = 5 ;
                                     % X = 3. Great, this works
?- nearby(X, 4).                     % X = 4 ;
                                     % error
% After rejecting X = 4 prolog backtracks and tries "Y is X+1" which is
% "4 is X+1" after substitution of parameters. But as we know from above
% "is" requires its argument to be fully instantiated and it is not, so
% an error occurs.

% One way to solve the first problem is to use a construct called the
% cut, !, which does nothing but which cannot be backtracked past.

nearbychk(X,Y) :- X = Y, !.
nearbychk(X,Y) :- Y is X+1, !.
nearbychk(X,Y) :- Y is X-1.

% This solves the first problem:
?- nearbychk(2,3).                   % True.

% But unfortunately it has consequences:
?- nearbychk(2,X).                   % X = 2.
% Because Prolog cannot backtrack past the cut after X = Y, it cannot
% try the possibilities "Y is X+1" and "Y is X-1", so it only generates
% one solution when there should be 3.
% However if our only interest is in checking if numbers are nearby,
% this may be all we need, thus the name nearbychk.
% This structure is used in Prolog itself from time to time (for example
% in list membership).

% To solve the second problem we can use built-in predicates in Prolog
% to verify if a parameter is bound or free and adjust our calculations
% appropriately.
nearby2(X,Y) :- nonvar(X), X = Y.
nearby2(X,Y) :- nonvar(X), Y is X+1.
nearby2(X,Y) :- nonvar(X), Y is X-1.
nearby2(X,Y) :- var(X), nonvar(Y), nearby2(Y,X).

% We can combine this with a cut in the case where both variables are
% bound, to solve both problems.
nearby3(X,Y) :- nonvar(X), nonvar(Y), nearby2(X,Y), !.
nearby3(X,Y) :- nearby2(X,Y).

% However when writing a predicate it is not normally necessary to go to
% these lengths to perfectly support every possible parameter
% combination. It suffices to support parameter combinations we need to
% use in the program. It is a good idea to document which combinations
% are supported. In regular Prolog this is informally in structured
% comments, but in some Prolog variants like Visual Prolog and Mercury
% this is mandatory and checked by the compiler.

% Here is the structured comment declaration for nearby3:

%!    nearby3(+X:Int, +Y:Int) is semideterministic.
%!    nearby3(+X:Int, -Y:Int) is multi.
%!    nearby3(-X:Int, +Y:Int) is multi.

% For each variable we list a type. The + or - before the variable name
% indicates if the parameter is bound (+) or free (-). The word after
% "is" describes the behaviour of the predicate:
%   semideterministic - can succeed once or fail
%     ( Two specific numbers are either nearby or not )
%   multi - can succeed multiple times but cannot fail
%     ( One number surely has at least 3 nearby numbers )
%  Other possibilities are:
%    det - always succeeds exactly once (eg, print)
%    nondet - can succeed multiple times or fail.
% In Prolog these are just structured comments and strictly informal but
% extremely useful.

% An unusual feature of Prolog is its support for atoms. Atoms are
% essentially members of an enumerated type that are created on demand
% whenever an unquoted non variable value is used. For example:
character(batman).            % Creates atom value batman
character(robin).             % Creates atom value robin
character(joker).             % Creates atom value joker
character(darthVader).        % Creates atom value darthVader
?- batman = batman.           % True - Once created value is reused
?- batman = batMan.           % False - atoms are case sensitive
?- batman = darthVader.       % False - atoms are distinct

% Atoms are popular in examples but were created on the assumption that
% Prolog would be used interactively by end users - they are less
% useful for modern applications and some Prolog variants abolish them
% completely. However they can be very useful internally.

% Loops in Prolog are classically written using recursion.
% Note that below, writeln is used instead of print because print is
% intended for debugging.

%!    countTo(+X:Int) is deterministic.
%!    countUpTo(+Value:Int, +Limit:Int) is deterministic.
countTo(X) :- countUpTo(1,X).
countUpTo(Value, Limit) :- Value = Limit, writeln(Value), !.
countUpTo(Value, Limit) :- Value \= Limit, writeln(Value),
    NextValue is Value+1,
    countUpTo(NextValue, Limit).

?- countTo(10).                      % Outputs 1 to 10

% Note the use of multiple declarations in countUpTo to create an
% IF test. If Value = Limit fails the second declaration is run.
% There is also a more elegant syntax.

%!    countUpTo2(+Value:Int, +Limit:Int) is deterministic.
countUpTo2(Value, Limit) :- writeln(Value),
    Value = Limit -> true ; (
        NextValue is Value+1,
        countUpTo2(NextValue, Limit)).

?- countUpTo2(1,10).                 % Outputs 1 to 10

% If a predicate returns multiple times it is often useful to loop
% through all the values it returns. Older Prologs used a hideous syntax
% called a "failure-driven loop" to do this, but newer ones use a higher
% order function.

%!    countTo2(+X:Int) is deterministic.
countTo2(X) :- forall(between(1,X,Y),writeln(Y)).

?- countTo2(10).                     % Outputs 1 to 10

% Lists are given in square brackets. Use memberchk to check membership.
% A group is safe if it doesn't include Joker or does include Batman.

%!     safe(Group:list(atom)) is deterministic.
safe(Group) :- memberchk(joker, Group) -> memberchk(batman, Group) ; true.

?- safe([robin]).                    % True
?- safe([joker]).                    % False
?- safe([joker, batman]).            % True

% The member predicate works like memberchk if both arguments are bound,
% but can accept free variables and thus can be used to loop through
% lists.

?- member(X, [1,2,3]).               % X = 1 ; X = 2 ; X = 3 .
?- forall(member(X,[1,2,3]),
       (Y is X+1, writeln(Y))).      % 2 3 4

% The maplist function can be used to generate lists based on other
% lists. Note that the output list is a free variable, causing an
% undefined value to be passed to plus, which is then bound by
% unification. Also notice the use of currying on the plus predicate -
% it's a 3 argument predicate, but we specify only the first, because
% the second and third are filled in by maplist.

?- maplist(plus(1), [2,3,4], Output).   % Output = [3, 4, 5].
```

##Ready For More?

* [SWI-Prolog](http://www.swi-prolog.org/)
