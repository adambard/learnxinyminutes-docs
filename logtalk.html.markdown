---
language: Logtalk
contributors:
    - ["Paulo Moura", "http://github.com/pmoura"]
filename: learnlogtalk.lgt
---

Logtalk is an object-oriented logic programming language that extends and leverages Prolog with modern code encapsulation and code reuse mechanisms without compromising its declarative programming features. Logtalk is implemented in highly portable code and can use most modern and standards compliant Prolog implementations as a back-end compiler.

To keep its size reasonable, this tutorial necessarily assumes that the reader have a working knowledge of Prolog and is biased towards describing Logtalk object-oriented features.

# Syntax

Logtalk uses standard Prolog syntax with the addition of a few operators and directives for a smooth learning curve and wide portability. One important consequence is that Prolog code can be easily encapsulated in objects with little or no changes. Moreover, Logtalk can transparently interpret most Prolog modules as Logtalk objects.

Some of the most important operators are:

* `::/2` - sending a message to an object
* `::/1` - sending a message to _self_ (i.e. to the object that received the message being processed)
* `^^/1` - _super_ call (of an inherited or imported predicate)

* `<</2` - debugging context switch call (allows calling a predicate from within an object)

Some of the most important entity and predicate directives will be introduced in the next sections.

# Entities and roles

Logtalk provides _objects_, _protocols_, and _categories_ as first-class entities. Relations between entities define _patterns of code reuse_ and the _roles_ played by the entities. For example, when an object _instantiates_ another object, the first object plays the role of an instance and the second object plays the role of a class. An _extends_ relation between two objects implies that both objects play the role of prototypes, with one of them extending the other, its parent prototype.

# Defining an object

An object encapsulates predicate declarations and definitions. Objects can be created dynamically but are usually static and defined in source files. A single source file can contain any number of entity definitions. A  simple object, defining a list member public predicate:

```logtalk
:- object(list).

	:- public(member/2).
	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
```

# Compiling source files

Assuming that the code above for the `list` object is saved in a `list.lgt` file, it can be compiled and loaded using the `logtalk_load/1` built-in predicates or its abbreviation, `{}/1`, with the file path as argument (the extension can be omitted):

```logtalk
?- {list}.
yes
```

# Sending a message to an object

The `::/2` infix operator is used to send a message to an object. As in Prolog, we can backtrack for alternative solutions:

```logtalk
?- list::member(X, [1,2,3]).
X = 1 ;
X = 2 ;
X = 3
yes
```

Encapsulation is enforced. A predicate can be declared _public_, _protected_, or _private_. It can also be _local_ when there is no scope directive for it. For example:

```logtalk
:- object(scopes).

	:- private(bar/0).
	bar.

	local.

:- end_object.
```

Assuming the object is saved in a `scopes.lgt` file:

```logtalk
?- {scopes}.
yes

?- scopes::bar.
error(permission_error(access,private_predicate,bar/0),logtalk(scopes::bar,user))

?- scopes::local.
error(existence_error(predicate_declaration,local/0),logtalk(scopes::local,user))
```

When the predicate in a message is unknown for the object (the role it plays determines the lookup procedures), we also get an error. For example:

```logtalk
?- scopes::unknown.
error(existence_error(predicate_declaration,unknown/0),logtalk(scopes::unknown,user))
```

# Defining and implementing a protocol

Protocols contain predicate declarations that can be implemented by objects and categories:

```logtalk
:- protocol(listp).

	:- public(member/2).

:- end_protocol.

:- object(list,
	implements(listp)).

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
```

The scope of the protocol predicates can be restricted using protected or private implementation. For example:

```logtalk
:- object(stack,
	implements(private::listp)).

:- end_object.
```

In fact, all entity relations can be qualified as public (the default), protected, or private.

# Prototypes

An object without an  _instantiation_ or _specialization_ relation with another object plays the role of a prototype. A prototype can _extend_ another object, its parent prototype.

```logtalk
% clyde, our prototypical elephant
:- object(clyde).

	:- public(color/1).
	color(grey).

	:- public(number_of_legs/1).
	number_of_legs(4).

:- end_object.

% fred, another elephant, is like clyde, except that he's white
:- object(fred,
	extends(clyde)).

	color(white).

:- end_object.
```

When answering a message sent to an object playing the role of a prototype, we validate the message and look for an answer first in the prototype itself and, if not found, we delegate to the prototype parents if any:

```logtalk
?- fred::number_of_legs(N).
N = 4
yes

?- fred::color(C).
C = white
yes
```

A message is valid if the corresponding predicate is declared (and the sender is within scope) but it will fail, rather then throwing an error, if the predicate is not defined. This is called the _closed-world assumption_. For example, consider the following object, saved in a `foo.lgt` file:

```logtalk
:- object(foo).

	:- public(bar).

:- end_object.
```

Loading the file and trying to call the `bar/0` predicate fails as expected. Note that this is different from calling an _unknown_ predicate, which results in an error:

```logtalk
?- {foo}.
yes

?- foo::bar.
no

?- catch(foo::baz, Error, true).
Error = error(
	existence_error(predicate_declaration,baz/0), logtalk(foo::baz,user)
).
```

# Classes and instances

In order to define objects playing the role of classes and/or instances, an object must have at least an instantiation or a specialization relation with another object. Objects playing the role of meta-classes can be used when we need to see a class also as an instance. We use the following example to also illustrate how to dynamically create new objects at runtime:

```logtalk
% a simple, generic, meta-class defining a new/2 predicate for its instances
:- object(class,
	instantiates(class)).

	:- public(new/2).
	new(Instance, Clauses) :-
		self(Class),
		create_object(Instance, [instantiates(Class)], [], Clauses).

:- end_object.

% a simple class defining age/1 and name/1 predicate for its instances
:- object(person,
	instantiates(class)).

	:- public([
		age/1, name/1
	]).

	% a default value for age/1
	age(42).

:- end_object.

% a static instance of the class person
:- object(john,
	instantiates(person)).

	name(john).
	age(12).

:- end_object.
```

When answering a message sent to an object playing the role of an instance, we validate the message by starting in its class and going up to its class superclasses if necessary. Assuming that the message is valid, then we look for an answer starting in the instance itself:

```logtalk
?- person::new(Instance, [name(paulo)]).
Instance = o1
yes

?- o1::name(Name).
Name = paulo
yes

?- o1::age(Age).
Age = 42
yes

?- john::age(Age).
Age = 12
yes
```

# Categories

A category is a fine grained unit of code reuse, used to encapsulate a _cohesive_ set of predicate declarations and definitions, implementing a _single_ functionality, that can be imported into any object. A category can thus be seen as the dual concept of a  protocol. In the following example, we define categories representing car engines and then import them into car objects:

```logtalk
% a protocol describing engine characteristics
:- protocol(carenginep).

	:- public([
		reference/1,
		capacity/1,
		cylinders/1,
		horsepower_rpm/2,
		bore_stroke/2,
		fuel/1
	]).

:- end_protocol.

% a typical engine defined as a category
:- category(classic,
	implements(carenginep)).

	reference('M180.940').
	capacity(2195).
	cylinders(6).
	horsepower_rpm(94, 4800).
	bore_stroke(80, 72.8).
	fuel(gasoline).

:- end_category.

% a souped up version of the previous engine
:- category(sport,
	extends(classic)).

	reference('M180.941').
	horsepower_rpm(HP, RPM) :-
		^^horsepower_rpm(ClassicHP, ClassicRPM),	% "super" call
		HP is truncate(ClassicHP*1.23),
		RPM is truncate(ClassicRPM*0.762).

:- end_category.

% with engines (and other components), we may start "assembling" some cars
:- object(sedan,
	imports(classic)).

:- end_object.

:- object(coupe,
	imports(sport)).

:- end_object.
```

Categories are independently compiled and thus allow importing objects to be updated by simple updating the imported categories without requiring object recompilation. Categories also provide _runtime transparency_:

```logtalk
?- sedan::current_predicate(Predicate).
Predicate = reference/1 ;
Predicate = capacity/1 ;
Predicate = cylinders/1 ;
Predicate = horsepower_rpm/2 ;
Predicate = bore_stroke/2 ;
Predicate = fuel/1
yes
```

# Hot patching

Categories can be also be used for hot-patching objects. A category can add new predicates to an object and/or replace object predicate definitions. For example, consider the following object:

```logtalk
:- object(buggy).

	:- public(p/0).
	p :- write(foo).

:- end_object.
```

Assume that the object prints the wrong string when sent the message `p/0`:

```logtalk
?- {buggy}.
yes

?- buggy::p.
foo
yes
```

If the object source code is not available and we need to fix an application running the object code, we can simply define a category that fixes the buggy predicate:

```logtalk
:- category(patch,
	complements(buggy)).

	% fixed p/0 def
	p :- write(bar).

:- end_category.
```

After compiling and loading the category into the running application we will now get:

```logtalk
?- {patch}.
yes

?- buggy::p.
bar
yes
```

# Parametric objects and categories

Objects and categories can be parameterized by using as identifier a compound term instead of an atom. Object and category parameters are _logical variables_ shared with all encapsulated predicates. An example with geometric circles:

```logtalk
:- object(circle(_Radius, _Color)).

	:- public([
		area/1, perimeter/1
	]).

	area(Area) :-
		parameter(1, Radius),
		Area is pi*Radius*Radius.

	perimeter(Perimeter) :-
		parameter(1, Radius),
		Perimeter is 2*pi*Radius.

:- end_object.
```

Parametric objects are used just as any other object, usually providing values for the parameters when sending a message:

```logtalk
?- circle(1.23, blue)::area(Area).
Area = 4.75291
yes
```

Parametric objects also provide a simple way of associating a set of predicates with a Prolog predicate. Prolog facts can be interpreted as _parametric object proxies_ when they have the same functor and arity as the identifiers of parametric objects. Handy syntax is provided to for working with proxies. For example, assuming the following clauses for a `circle/2` predicate:

```logtalk
circle(1.23, blue).
circle(3.71, yellow).
circle(0.39, green).
circle(5.74, black).
circle(8.32, cyan).
```

With these clauses loaded, we can easily compute for example a list with the areas of all the circles:

```logtalk
?- findall(Area, {circle(_, _)}::area(Area), Areas).
Areas = [4.75291, 43.2412, 0.477836, 103.508, 217.468]
yes
```

# Events and monitors

Logtalk supports _event-driven programming_ by allowing defining events and monitors for those events. An event is simply the sending of a message to an object. Interpreting message sending as an atomic activity, a _before_ event and an _after_ event are recognized. Event monitors define event handler predicates, `before/3` and `after/3`, and can query, register, and delete a system-wide event registry that associates events with monitors. For example, a simple tracer for any message being sent using the `::/2` control construct can be defined as:

```logtalk
:- object(tracer,
	implements(monitoring)).    % built-in protocol for event handlers

	:- initialization(define_events(_, _, _, _, tracer)).

	before(Object, Message, Sender) :-
		write('call: '), writeq(Object), write(' <-- '), writeq(Message),
		write(' from '), writeq(Sender), nl.

	after(Object, Message, Sender) :-
		write('exit: '), writeq(Object), write(' <-- '), writeq(Message),
		write(' from '), writeq(Sender), nl.

:- end_object.
```

Assuming that the `tracer` object and the `list` object defined earlier are compiled and loaded, we can observe the event handlers in action by sending a message:

```logtalk
?- list::member(X, [1,2,3]).

call: list <-- member(X, [1,2,3]) from user
exit: list <-- member(1, [1,2,3]) from user
X = 1 ;
exit: list <-- member(2, [1,2,3]) from user
X = 2 ;
exit: list <-- member(3, [1,2,3]) from user
X = 3
yes
```

# Lambda expressions

Logtalk supports lambda expressions. Lambda parameters are represented using a list with the `(>>)/2` infix operator connecting them to the lambda. Some simple examples using library meta-predicates:

```logtalk
?- {library(metapredicates_loader)}.
yes

?- meta::map([X,Y]>>(Y is 2*X), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

Currying is also supported:

```logtalk
?- meta::map([X]>>([Y]>>(Y is 2*X)), [1,2,3], Ys).
Ys = [2,4,6]
yes
```


# Macros

Terms and goals in source files can be _expanded_ at compile time by specifying a _hook object_ that defines term-expansion and goal-expansion rules. For example, consider the following simple object, saved in a `source.lgt` file:

```logtalk
:- object(source).

	:- public(bar/1).
	bar(X) :- foo(X).

	foo(a). foo(b). foo(c).

:- end_object.
```

Let's define an hook object, saved in a `my_macros.lgt` file, that changes clauses and calls to the `foo/1` local predicate:

```logtalk
:- object(my_macros,
	implements(expanding)).		% built-in protocol for expanding predicates

	term_expansion(foo(Char), baz(Code)) :-
		char_code(Char, Code).	% standard built-in predicate

	goal_expansion(foo(X), baz(X)).

:- end_object.
```

After loading the macros file, we can then expand our source file with it:

```logtalk
?- logtalk_load(my_macros), logtalk_load(source, [hook(my_macros)]).
yes

?- source::bar(X).
X = 97 ;
X = 98 ;
X = 99
true
```

The Logtalk library provides support for combining hook objects using different workflows (for example, defining a pipeline of expansions).

# Further information

Visit the [Logtalk website](http://logtalk.org) for more information.
