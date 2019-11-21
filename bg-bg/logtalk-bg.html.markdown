---
language: Logtalk
filename: llearnlogtalk-bg.lgt
contributors:
    - ["Paulo Moura", "http://github.com/pmoura"]
translators:
    - ["vsraptor", "https://github.com/vsraptor"]
lang: bg-bg

---

Logtalk е обектно-ориентиран (ОО) модерен логически език за програмиране, които разширява Prolog с възможности за капсулиране (еncapsulation) и многократно използване на кода без да компрометира декларативните възможности на езика. Logtalk е имплементиран така че да може да бъде адапртиран към всеки стандартен Prolog като back-end компилатор, тоест е напълно прозрачен за нормална Prolog програма.
Допълнително, Logtalk също може да интерпретира Prolog модули, като Logtalk обекти.

Основната структурна единица за изграждане на програмни със Logtalk е чрез използване на обекти.
Logtalk поддържа както стандартния начин за изграждане на иерархий от класове познати ни от езици като Java, същто така и prototype-OOP познат ни от езици като JavaScript.
Запомнете че всичко стартира с дефинирането и създаването на обект.


## Syntax (Синтакс)


Logtalk използва стандартен Prolog синтакс, с минимум допълнителни оператори и директиви.
Важно последствие от това е че кода лесно се капсулира с много малко промени спрямо оригинален код.

Операторите които Logtalk добавя към Prolog са :

    ::/2 - изпраща саобщение до обект (аналогично на метод в стандартните ООП езици)
    ::/1 - изпраща саобщение до себе си (self) (тоест до обекта който е получил съобщението което обработваме в момента)
    ^^/1 - super call (изпраща саобщение до наследен или импортиран предикат(predicate))


## Entities and roles (Субекти и роли)


Logtalk предоставя обекти, портоколи и категории като първокласни-субекти (first-class entities). Връзката между тях описва ролята която субектите изпалняват. 
Обектите могат да играят различни роли в зависимост от как ги дефинираме тоест какви директиви използваме при дефиницията.

Например когато използваме обект А за да създадем нов обект Б, обект Б играе ролята на "инстанция", а обект А играе ролята на клас.
Ако използваме "extends"-дефиниция единия от обектите играе ролята на протоип(prototype) за другия.


## Defining an object (Дефиниране на обект)


Чрез дефинирането на обект ние капсулираме дефиницията на "предикатите".
Обекти могат да се създадат динамично или дефинират статично във код-файла.
Ето как дефинираме примерен обект :

```logtalk
:- object(list).

    :- public(member/2).
    member(Head, [Head| _]).
    member(Head, [_| Tail]) :-
        member(Head, Tail).

:- end_object.
```

## Compiling source files (Компилиран)


Ако предположим че кода е записан във файл с име list.lgt, можем да го компилираме чрез logtalk_load/1 предиката или съкратения вариант {}/1.


```logtalk
?- {list}.
yes
```

## Sending a message to an object (Изпращане на събщение до обект) 


Както казахме ::/2 infix оператор се използва за изпращане на съобщение до обекта. Както в Prolog, ние можем да backtrack-нем за алтернативни решения, понеже метода е просто стандартен предикат :

```logtalk
?- list::member(X, [1,2,3]).
X = 1 ;
X = 2 ;
X = 3
yes

?- write_canonical(list::member(X, [1,2,3])).
::(list,member(_,[1,2,3]))
```

Кагато декларирме обект автоматично предикатите са капсулирани (еncapsulation), тоест извън обекта те са невидими за останалата част от програмата. Естествено има опции да променим това поведение чрез public, protected, или private предикати. 

```logtalk
:- object(scopes).

    :- private(bar/0).
    bar.

    local.

:- end_object.
```

Ако кода е записан в scopes.lgt фаил и се опитаме да изпратим саобщтение до частен(private) или локален предикат ще получим грешка:

```logtalk
?- {scopes}.
yes

?- catch(scopes::bar, Error, true).
Error = error(
    permission_error(access, private_predicate, bar/0),
    logtalk(scopes::bar, user)
)
yes

?- catch(scopes::local, Error, true).
Error = error(
    existence_error(predicate_declaration, local/0),
    logtalk(scopes::local, user)
)
yes
```

Когато предиката е непознат за обекта това също генерира грешка. Например :

```logtalk
?- catch(scopes::unknown, Error, true).
Error = error(
    existence_error(predicate_declaration, unknown/0),
    logtalk(scopes::unknown, user)
)
yes
```

## Протоколи (Defining and implementing a protocol)


За тези от вас свикнали със стандартно ООП, Protocols наподобяват Interfaces в Java.
Протоколите съдържат предикати които могат да бъдат в последствие имплементирани в обекти и категории :

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

Обхвата(scope) на предикатите в протокола могат да бъде ограничени чрез protected или private клаузи. 
Например:

```logtalk
:- object(stack,
    implements(private::listp)).

:- end_object.
```

Всички субекти(entity) релации могат да бъдат пре-дефинирани с public, protected или private, подбно на начина показан по горе.


## Прототипи (Prototypes)


Всеки обект без instantiation или specialization спецификация с друг обект, играе ролята на прототип. 
Прототип-обект може да предефинира и разщири протоипа-родител.

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

Когато системата отговаря на съобщение изпратено до обект който играе ролята на прототип, тя търси отговор първо в прототипа и ако не намери предикат делегира отговора на прототипа-родител-обект :

```logtalk
?- fred::number_of_legs(N).
N = 4
yes

?- fred::color(C).
C = white
yes
```

Съобщението е валидно но няма да генерира грещка, ако предиката е дефиниран но не е деклариран/имплементиран. Това е така наречения closed-world assumption. 
Например :

```logtalk
:- object(foo).

    :- public(bar/0).

:- end_object.
```

Ако заредим файла и се опитаме да извикаме bar/0, няма да получим отговор, както може да очакваме. Ако обаче предиката не е дори дефиниран, ще получим гращка :

```logtalk
?- {foo}.
yes

?- foo::bar.
no

?- catch(foo::baz, Error, true).
Error = error(
    existence_error(predicate_declaration, baz/0),
    logtalk(foo::baz, user)
)
yes
```

## Класове и инстанции (Classes and instances)


За да саздадем обекти които играят ролята на класове и/или инстанции, трябва да използваме поне instantiation или specialization дефиниция с друг обект. Обектите които играят роля на мета-класове могат да се използват ако е нужно още за саздаване на инстанции на класа.
Следващия пример ще илюстрира как можем динамично да саздадаваме обекти :

```logtalk
% a simple, generic, metaclass defining a new/2 predicate for its instances
:- object(metaclass,
    instantiates(metaclass)).

    :- public(new/2).
    new(Instance, Clauses) :-
        self(Class),
        create_object(Instance, [instantiates(Class)], [], Clauses).

:- end_object.

% a simple class defining age/1 and name/1 predicate for its instances
:- object(person,
    instantiates(metaclass)).

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

Когато отговаряме на съобщение изпратено до обект който играе ролята на инстанция, системата валидира съобщението първо в текущия клас, след това класа-родител ако е необходимо. Ако съобщението е валидно тогава проверяваме инстанцията :

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

## Категории (Categories)


Категорията е капсулран код който може да се рециклира (reuse) в различни обекти. Докато Протокола е само дефиниции Категорията е сащо и декларация/имплементация на предикатите които сме дефинирали.
В следващия пример ще дефинираме категории представящи автомобилни двигатели след което ще ги импортираме в автомобил-обекти :
   

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
        ^^horsepower_rpm(ClassicHP, ClassicRPM),    % "super" call
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

Категориите се компилират отделно и разрешават импортираните обекти да бъдат обновени като просто обновим категориите без да е необходимо да прекомпилираме обекта:

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

## Hot patching


Категориите още могат да се използват за промяна на обекти "в движение", след като вече са били инстанциирани. 
Например :

```logtalk
:- object(buggy).

    :- public(p/0).
    p :- write(foo).

:- end_object.
```

Да предположим че обекта изпечатва грешното съобщение p/0 :

```logtalk
?- {buggy}.
yes

?- buggy::p.
foo
yes
```

Ако кода който описва този обект не е наличен и трябва да коригираме приложението, ние можем просто да създадем категория която да коригира необходимия предикат :

```logtalk
:- category(patch,
    complements(buggy)).

    % fixed p/0 def
    p :- write(bar).

:- end_category.
```

След компилиране и зареждане на категорията ще получим :

```logtalk
?- {patch}.
yes

?- buggy::p.
bar
yes
```


## Parametric objects and categories


Обектите и категориите могат да се параметризират ако използваме за индентификатор комплексен-термин вместо атом.
Параметрите са логически променливи достъпни за всички капсулирани предикати. 
Пример с геометрични кръгове :

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

Параметричните-обекти се използват като всеки друг обект, обикновенно осигуряваики стойности за параметрите когато изпращаме съобщение.

```logtalk
?- circle(1.23, blue)::area(Area).
Area = 4.75291
yes
```

Параметричните-обекти още осигуряват лесен начин за ассоцииране на различни предикати със нормални Prolog предикати.
Prolog факти могат да бъдат интерпретирани като посредници (proxies).
Например следните клаузи на circle/2 предикат :


```logtalk
circle(1.23, blue).
circle(3.71, yellow).
circle(0.39, green).
circle(5.74, black).
circle(8.32, cyan).
```

можем лесно да изчислим площа на всички кръгове :

```logtalk
?- findall(Area, {circle(_, _)}::area(Area), Areas).
Areas = [4.75291, 43.2412, 0.477836, 103.508, 217.468]
yes
```

{Goal}::Message формата доказва(proves) Goal и изпраща съобщение до генерирания термин.


## Събития и мониторинг (Events and monitors)


Logtalk поддържа event-driven програмиране чрез дефинирането на събития и монитори за тези събития.
Събитие е просто изпращане на съобщение към обект. При обработването на съобщение системата разпознава before-събитие и after-събитие.
Мониторите дефинират предикати които ще прихаванат тези събития (before/3 и after/3).
Нампример следния монитор ще прихаване съобщенията изпратени чрез ::/2  :

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

Ето как можем да проследим реакцията на изпращане на съобщение :

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

Събития могат да се изтрият динамично чрез define_events/5 и abolish_events/5 предикати.


## Lambda expressions


Logtalk поддържа lambda expressions. Lambda параметрите се предават чрез списък към (>>)/2 infix оператор свързвайки ги с lambda.
Ето няколко примера :

```logtalk
?- {library(metapredicates_loader)}.
yes

?- meta::map([X,Y]>>(Y is 2*X), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

Currying се поддържа :

```logtalk
?- meta::map([X]>>([Y]>>(Y is 2*X)), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

Lambda free variables can be expressed using the extended syntax {Free1, ...}/[Parameter1, ...]>>Lambda.


## Макроси (Macros)


Термини и Цели могат да бъдат пре-интерпретирани (expanded) по време на компилация ако специфицираме hook-обект който дефинира прецедурите на пре-интерпретиране.
Нека следният обект е записан във фаил source.lgt :

```logtalk
:- object(source).

    :- public(bar/1).
    bar(X) :- foo(X).

    foo(a). foo(b). foo(c).

:- end_object.
```

и следния hooк-обект е записан в my_macros.lgt, който пре-интерпретира foo/1 предиката :

```logtalk
:- object(my_macros,
    implements(expanding)).    % built-in protocol for expanding predicates

    term_expansion(foo(Char), baz(Code)) :-
        char_code(Char, Code). % standard built-in predicate

    goal_expansion(foo(X), baz(X)).

:- end_object.
```

След зареждането на файла с макроси ние можем да пре-интерпретираме ползайки hook-флаг за компилатора :

```logtalk
?- logtalk_load(my_macros), logtalk_load(source, [hook(my_macros)]).
yes

?- source::bar(X).
X = 97 ;
X = 98 ;
X = 99
true
```

## Допълнителна информация (Further information)


Посетете сайта на [Logtalk website](http://logtalk.org) за повече информация.

