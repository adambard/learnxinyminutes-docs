---
name: Picat
filename: learnpicat.pi
contributors:
    - ["Claude", "https://github.com/r33drichards"]
---

Picat is a logic-based, multi-paradigm programming language. The name stands
for **P**attern-matching, **I**ntuitive, **C**onstraints, **A**ctors, and
**T**abling. It blends Prolog-style logic programming with functions,
imperative loops and assignment, dynamic programming via tabling, and a
powerful constraint-solving toolbox.

A Picat program is a set of *predicates* and *functions* made of rules. The
entry point of a runnable program is the `main` predicate.

```picat
% This is a single-line comment.

/* This is a
   block comment. */

% A program runs by executing the `main` predicate.
main =>
    println("Hello, Picat!").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Numbers and arithmetic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_numbers =>
    println(2 + 3 * 4),   % 14
    println(7 // 2),      % 3   integer division
    println(7 / 2),       % 3.5 float division
    println(7 mod 3),     % 1
    println(2 ** 10),     % 1024  power
    println(2 ** 0.5),    % 1.41421
    println(abs(-5)),     % 5
    println(max(3, 9)),   % 9
    println(gcd(12, 18)). % 6

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Atoms, strings and characters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Atoms start with a lowercase letter (or are 'quoted').
% Strings are written with double quotes and are lists of characters.

go_text =>
    Name = picat,                  % an atom
    println(Name),
    S = "a string",                % a string = list of chars
    println(S),
    println(to_uppercase("picat")),% PICAT
    println(len("picat")),         % 5  (strings are lists)
    println("foo" ++ "bar"),       % foobar  (++ concatenates)
    println(ord('a')),             % 97
    println(chr(98)).              % b

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. Variables, unification and assignment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Variables start with an uppercase letter or underscore.
% `=` is unification (as in Prolog), not assignment.
% `:=` reassigns a variable imperatively.

go_vars =>
    X = 41,            % bind X by unification
    println(X),
    Y = 0,
    Y := Y + 1,        % reassign with :=
    Y := Y + 1,
    println(Y),        % 2
    [A, B, C] = [1, 2, 3],   % unification destructures a list
    println(A + B + C).      % 6

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. Lists, ranges, arrays and maps
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_collections =>
    L = [1, 2, 3, 4, 5],
    println(L[1]),          % 1   indexing is 1-based
    println(L[2..4]),       % [2,3,4]  slicing
    println(first(L)),      % 1
    println(L ++ [6, 7]),   % [1,2,3,4,5,6,7]
    println(sum(L)),        % 15
    println(sort([3, 1, 2])),    % [1,2,3]
    [H | T] = L,                 % head/tail unification
    printf("head=%w tail=%w\n", H, T),  % head=1 tail=[2,3,4,5]

    % Ranges
    println(1..5),          % [1,2,3,4,5]
    println(1..2..9),       % [1,3,5,7,9]  (From..Step..To)

    % Arrays use curly braces and are fixed-size
    Arr = {10, 20, 30},
    println(Arr[2]),        % 20
    Arr[2] := 99,           % arrays are mutable
    println(Arr),           % {10,99,30}
    println(to_list(Arr)),  % [10,99,30]

    % Maps are hash tables
    M = new_map([apple = 3, pear = 5]),
    M.put(banana, 2),
    println(M.get(apple)),       % 3
    println(M.keys().sort()).    % [apple,banana,pear]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5. List comprehensions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_comprehensions =>
    println([X * X : X in 1..5]),               % [1,4,9,16,25]
    println([X : X in 1..20, X mod 2 == 0]),    % even numbers
    % Multiple generators
    println([(X, Y) : X in 1..2, Y in 1..2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 6. Control flow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_control =>
    % if / elseif / else
    N = 7,
    if N mod 2 == 0 then
        println(even)
    elseif N == 7 then
        println(lucky)
    else
        println(odd)
    end,

    % foreach loop
    foreach(I in 1..3)
        printf("i = %w\n", I)
    end,

    % foreach with several generators acts like nested loops
    foreach(I in 1..2, J in 1..2)
        printf("%w-%w ", I, J)
    end,
    nl,

    % while loop with reassignment
    K = 3, Fact = 1,
    while (K > 0)
        Fact := Fact * K,
        K := K - 1
    end,
    println(Fact).   % 6

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7. Functions vs. predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A *function* is defined with `=` and returns a value.
double(X) = X * 2.

% Functions can have several clauses (pattern matching on arguments).
fib(0) = 0.
fib(1) = 1.
fib(N) = fib(N - 1) + fib(N - 2).

% A *predicate* is defined with `=>` and succeeds or fails (no return value).
% Pattern-matching rules use `=>` (deterministic) or `?=>` (backtrackable).
greet(Name) =>
    printf("Hello, %w!\n", Name).

go_funcs =>
    println(double(21)),   % 42
    println(fib(10)),      % 55
    greet(world).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 8. Recursion and a pattern-matching gotcha
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recursion over a list. `[_|T]` matches head (ignored) and tail T.
my_len([]) = 0.
my_len([_|T]) = 1 + my_len(T).

% IMPORTANT: unlike Prolog, a pattern-matching rule head does NOT bind
% output variables. Repeated/output variables must be unified in the body.
% Wrong:   pick([H|_], H) ?=> true.      % H won't unify with the caller
% Right:
pick([H|_], X) ?=> X = H.
pick([_|T], X) ?=> pick(T, X).

go_recursion =>
    println(my_len([a, b, c, d])).   % 4

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 9. Nondeterminism and collecting solutions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_nondet =>
    % `member/2` enumerates elements; findall collects every solution.
    println(findall(X, member(X, [10, 20, 30]))),   % [10,20,30]
    println(findall(E, $pick([a, b, c], E))),       % [a,b,c]
    % `$` quotes a term so it is built, not evaluated as a function call.
    % Committed choice / first solution with -> (if-then-else):
    ( member(Y, [3, 6, 9]), Y > 5 ->
        printf("first > 5 is %w\n", Y)
    ;   println(none)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 10. Higher-order functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_higher_order =>
    println(map(double, [1, 2, 3])),    % [2,4,6]
    println(fold(+, 0, [1, 2, 3, 4])),  % 10
    println([X : X in 1..10, X mod 3 == 0]). % [3,6,9]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 11. Tabling (memoization / dynamic programming)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The `table` directive memoizes results, turning exponential recursion
% into linear time. Great for dynamic programming.
table
tfib(0) = 0.
tfib(1) = 1.
tfib(N) = tfib(N - 1) + tfib(N - 2).

go_tabling =>
    println(tfib(50)).   % 12586269025  (instant, thanks to tabling)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 12. Structures (compound terms)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_structs =>
    P = $point(3, 4),    % build a compound term with `$`
    println(P),          % point(3,4)
    println(P[1]),       % 3   access argument by index
    println(arity(P)),   % 2
    $point(A, B) = P,    % unify to deconstruct
    printf("A=%w B=%w\n", A, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 13. Constraint programming
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Picat ships with constraint solvers. Import `cp` (or `sat`, `mip`, `smt`).
% Constraint operators are prefixed with `#`: #= #!= #> #< #=< #>=
import cp.

% Solve the cryptarithmetic puzzle SEND + MORE = MONEY.
send_more_money(Digits) =>
    Digits = [S, E, N, D, M, O, R, Y],
    Digits :: 0..9,              % domain of each variable
    all_different(Digits),
    S #!= 0,
    M #!= 0,
                1000*S + 100*E + 10*N + D
              + 1000*M + 100*O + 10*R + E
    #= 10000*M + 1000*O + 100*N + 10*E + Y,
    solve(Digits).               % search for an assignment

% The classic N-queens model, using an array of constrained variables.
queens(N, Q) =>
    Q = new_array(N),
    Q :: 1..N,
    foreach(I in 1..N-1, J in I+1..N)
        Q[I] #!= Q[J],               % not on the same row
        abs(Q[I] - Q[J]) #!= J - I   % not on the same diagonal
    end,
    solve(Q).

go_constraints =>
    send_more_money(Ds),
    println(Ds),               % [9,5,6,7,1,0,8,2]
    queens(8, Q),
    println(Q.to_list()).      % a valid 8-queens placement

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 14. Input and output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_io =>
    print("no newline "),      % print without newline
    println("with newline"),   % print with newline
    printf("%w and %d and %s\n", foo, 42, "bar"),  % formatted
    % read_int/0, read_line/0 and read_term/0 read from standard input.
    nl.                         % print a newline
```

## Running Picat

Save your code in a file like `program.pi` and run it:

```
picat program.pi
```

Picat also has an interactive shell. Start it with `picat`, then load files
and query goals:

```
$ picat
Picat> X = 2 + 3, println(X).
5
Picat> cl("program.pi").   % compile/load a file
Picat> main.               % run a goal
```

## Further reading

* [Official Picat website](http://picat-lang.org/)
* [Picat User's Guide](https://picat-lang.org/download/picat_guide_html/picat_guide.html)
* [Picat book: "Constraint Solving and Planning with Picat"](https://picat-lang.org/picatbook2015.html)
* [Programming examples](https://picat-lang.org/projects.html)
