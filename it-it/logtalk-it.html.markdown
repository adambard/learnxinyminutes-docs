---
language: Logtalk
filename: learnlogtalk-it.lgt
contributors:
    - ["Paulo Moura", "http://github.com/pmoura"]
translators:
    - ["Ugo Chirico", "https://github.com/ugochirico"]
lang: it-it
---

Logtalk è un linguaggio di programmazione logica orientata agli oggetti che estende il linguaggio Prolog con le moderne tecniche di Object-Oriented Programming quali incapsulamento, ereditarietà e riutilizzo del codice, senza compromettere le caratteristiche di programmazione dichiarativa del Prolog. Logtalk è implementato in codice altamente portabile e utilizza i più moderni standard di conformità del Prolog rispetto al compilatore backend.

Per mantenere una dimensione ragionevole, questo tutorial presuppone necessariamente che il lettore abbia una conoscenza del linguaggio Prolog ed è inoltre focalizzato esclusivamente sulla descrizione delle caratteristiche object-oriented di Logtalk.

# Sintassi

Logtalk utilizza la sintassi standard del linguaggio Prolog con l'aggiunta di un paio di operatori e di alcune direttive per una curva di apprendimento morbida e per assicurare ampia portabilità. Una conseguenza importante è che il codice Prolog può essere facilmente incapsulato in oggetti con poche o nessuna modifica. Inoltre, Logtalk può interpretare come oggetti Logtalk, in modo trasparente, la maggior parte dei moduli Prolog già esistenti.

I principali operatori sono:

* `::/2` - per inviare un messaggio ad un oggetto
* `::/1` - per inviare un messaggio a se stesso _self_ (cioè all'oggetto che riceverà il messaggio)
* `^^/1` - _super_ per chiamare un predicato ereditato o importato

Alcune delle più importanti entità e direttive saranno introdotte nelle sezioni successive.

# Entità e Ruoli

Logtalk tratta gli oggetti, i protocolli e le categorie come entità di prima classe. I rapporti tra le entità definiscono i _patterns of code reuse_ ossia i modelli di riutilizzo del codice e i  _roles_ ossia i ruoli svolti da tali entità. Ad esempio, quando un oggetto istanzia un altro oggetto, il primo oggetto assume il ruolo di istanza e il secondo oggetto assume il ruolo di classe. Una relazione di tipo _extends_ tra due oggetti implica che entrambi gli oggetti svolgano il ruolo di prototipi, in cui uno di loro estende l'altro, che diventa quindi suo prototipo padre.

# Definizione di un oggetto

Un oggetto incapsula le dichiarazioni e le definizioni dei predicati. Gli oggetti possono essere creati in modo dinamico, ma di solito sono dichiarati come statici e definiti nel codice sorgente. Un singolo file sorgente può contenere un qualsiasi numero di definizioni di entità. Ecco un semplice oggetto `list` che definisce un membro pubblico `member/2`:

```logtalk
:- object(list).

	:- public(member/2).
	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
```

# Compilazione e il caricamento dei file sorgenti

Supponendo che il codice di cui sopra per l'oggetto `list` venga salvato in un file` list.lgt`, esso può essere compilato e caricato utilizzando il predicato predefiniti `logtalk_load/1` o la sua abbreviazione `{}/1`, con il percorso del file come argomento (l'estensione può essere omessa):

```logtalk
?- {list}.
yes
```

In generale, le entità potrebbero avere dipendenze sulle entità definite in altri file di origine (ad esempio le entità di biblioteca). Per caricare un file e tutte le sue dipendenze, la soluzione consigliata consiste nel definire un file _loader_ che carica tutti i file necessari per un'applicazione. Un file loader è semplicemente un file di origine, in genere denominato `loader.lgt`, che effettua chiamate ai predicati built-in `logtalk_load/1-2`, di solito
da una direttiva `initialization/1` per la portabilità e conformità agli standard. Caricatore file vengono forniti per tutte le librerie, strumenti ed esempi. 

# Inviare un messaggio ad un oggetto

L'operatore infisso `::/2` è usato per inviare messaggi ad un oggetto. Analogamente al Prolog, è possibile fare backtracking per le soluzioni alternative:

```logtalk
?- list::member(X, [1,2,3]).
X = 1 ;
X = 2 ;
X = 3
yes
```

Analogamente alla programmazione object-oriented, logtalk consente anche l'Incapsulamento. 
Un predicato può essere dichiarata pubblico, protetto o privato. Può anche essere _local_ quando non esiste una direttiva specifica per esso all'interno dello scope. Per esempio:

```logtalk
:- object(scopes).

	:- private(bar/0).
	bar.

	local.

:- end_object.
```

Assumendo che l'oggetto è salvato nel file `scopes.lgt`:

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

Quando il predicato in un messaggio non è noto per l'oggetto (il ruolo dell'oggetto determina le procedure di ricerca), si ha un errore.
Per esempio:

```logtalk
?- catch(scopes::unknown, Error, true).
Error = error(
	existence_error(predicate_declaration, unknown/0),
	logtalk(scopes::unknown, user)
)
yes
```

Un punto fondamentale da capire è che le direttive che specificano il predicato nello scope specificano la semantica di chiamata (_calling_) del predicato, e non la semantica di definizione (_definition_). Ad esempio, se un oggetto ha il ruolo di una classe e dichiara un predicato privato, tale predicato può essere definito nelle sue sottoclassi e nelle istanze * ma * può essere chiamato solo nelle sue istanza (_from_) dalla classe.

# Definizione e implementazione di un protocollo

Un Protocollo contiene le dichiarazioni dei predicati che possono essere implementati da un qualsivoglia numero di oggetti e categorie:

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

Lo scope dei predicati di un protocollo può essere ristretto usando implementazioni protected e private. Ad esempio:

```logtalk
:- object(stack,
	implements(private::listp)).

:- end_object.
```

Difatti, tutte le relazioni tra entità (nella direttiva di apertura di un entità) possono essere definite come public (default), protected, o private.

# Prototipi

Un oggetto senza una istanza o senza una relazione di specializzazione con un altro oggetto interpreta il ruolo di prototipo. Un prototipo può estendere un altro oggetto, il suo prototipo genitore.

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

Per rispondere ad un messaggio inviato ad un oggetto che ha il ruolo di prototipo, si cerca prima una risposta nel prototipo stesso e se il prototipo non sa rispondere si passa all'eventuale prototipo genitore (se esiste):

```logtalk
?- fred::number_of_legs(N).
N = 4
yes

?- fred::color(C).
C = white
yes
```

Un messaggio è valido se il relativo predicato è dichiarato in un oggetto (e se il mittente è nel campo di applicazione), ma fallirà, piuttosto che lanciare un errore, se il predicato non è definito. Questa è chiamata la _closed-world assumption_. Ad esempio, si consideri il seguente oggetto, salvato in un file `foo.lgt`:

```logtalk
:- object(foo).

	:- public(bar/0).

:- end_object.
```

Caricando il file e cercando di chiamare il predicato `bar/0` questo fallisce come previsto. Si noti che ciò è diverso dal chiamare un predicato sconosciuto _unknown_, che invece genera un errore:

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

# Classi e istanze

Per definire gli oggetti nei ruoli di classi e/o istanze, un oggetto deve avere almeno un istanziazione o una relazione di specializzazione con un altro oggetto. Gli oggetti che hanno il ruolo di meta-classi possono essere utilizzati quando abbiamo bisogno di usare una classe come se fosse un'istanza. Il seguente esempio mostra come creare dinamicamente nuovi oggetti in fase di esecuzione:

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

Nel rispondere ad un messaggio inviato ad un oggetto ha assunto il ruolo di istanza, tal messaggio viene convalidato partendo dalla sua classe e andando a ritroso nella gerarchia, se necessario, fino alle sue superclassi. Supponendo che il messaggio sia valido, allora si cerca una risposta a partire dall'istanza stessa:

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

# Categorie

Una categoria è un'unità atomica di codice riutilizzabile. Una categoria è usata per incapsulare una insieme coesivo (_cohesive_) di dichiarazioni e di definizioni di predicato ed è atta ad implementare una singola (_single_) funzionalità che può essere importata in qualsiasi oggetto. Una categoria può quindi essere concepita come il concetto duale di protocollo. Nel seguente esempio, si definiscono prima le categorie che rappresentano i motori di auto e poi si importano tali categorie negli oggetti auto:

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

Le Categorie sono compilate in modo indipendente e, quindi, consentono l'importazione di oggetti da aggiornare mediante il semplice aggiornamento delle categorie importate, senza richiedere pertanto la ricompilazione dell'oggetto. Le Categorie forniscono anche la _runtime transparency_,  cioè il protocollo della categoria si aggiunge al protocollo degli oggetti che importano tale categoria:

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

Le categorie possono essere anche usate per modificare gli oggetti al volo (_hot-patch_). Una categoria può aggiungere nuovi predicati ad un oggetto e/o sostituire le definizioni dei predicati dell'oggetto. Ad esempio, si consideri il seguente oggetto:

```logtalk
:- object(buggy).

	:- public(p/0).
	p :- write(foo).

:- end_object.
```

Si supponga che l'oggetto stampi la stringa sbagliata quando riceve il messaggio `p/0`:

```logtalk
?- {buggy}.
yes

?- buggy::p.
foo
yes
```

Se il codice sorgente dell'oggetto non è disponibile e bisogna correggere l'applicazione che sta eseguendo il codice dell'oggetto, si può semplicemente definire una categoria che corregge il predicato non corretto:

```logtalk
:- category(patch,
	complements(buggy)).

	% fixed p/0 def
	p :- write(bar).

:- end_category.
```

Dopo la compilazione e il caricamento della categoria nell'applicazione in esecuzione si ottiene:

```logtalk
?- {patch}.
yes

?- buggy::p.
bar
yes
```

Poiché l'hot-patching pregiudica forzatamente l'incapsulamento, un apposito flag di compilazione `complementary` può essere impostato (a livello globale o per un singolo oggetto) per consentire, limitare o prevenire l'hot-patching.

# Oggetti Parametrici e Categorie

Gli oggetti e le categorie possono essere parametrizzati utilizzando come identificativo un compound-term al posto di un atomo. Oggetti e parametri di una categoria sono variabili logiche _logical variables_ condivise con tutti i predicati incapsulati. Ecco un esempio con cerchi geometrici:

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

Oggetti parametrici possono essere utilizzati come qualsiasi altro oggetto e di solito forniscono i valori da assegnare ai parametri quando si invia un messaggio:

```logtalk
?- circle(1.23, blue)::area(Area).
Area = 4.75291
yes
```

Gli oggetti parametrici forniscono anche un modo semplice per associare un insieme di predicati con un semplice predicato Prolog. Fatti Prolog possono essere interpretati come oggetti proxy parametrici ( _parametric object proxies_) quando hanno lo stesso funtore e arietà degli identificatori di oggetti parametrici. Per lavorare con i proxy viene fornita una sintassi maneggevole. Per esempio, si prendano le seguenti clausole per il predicato `circle/2`:

```logtalk
circle(1.23, blue).
circle(3.71, yellow).
circle(0.39, green).
circle(5.74, black).
circle(8.32, cyan).
```

Con queste clausole, si può facilmente calcolare, ad esempio, un elenco con le aree di tutti i cerchi:

```logtalk
?- findall(Area, {circle(_, _)}::area(Area), Areas).
Areas = [4.75291, 43.2412, 0.477836, 103.508, 217.468]
yes
```

In pratica, il costrutto `{Goal}::Message` prova il goal `Goal`, instanziando le variabili interne e inviando un messaggio `Message` al termine risultante.

# Eventi and monitor

Logtalk supporta l'_event-driven programming_ mediante la definizione di eventi e di monitor. Un evento è semplicemente l'invio di un messaggio ad un oggetto. Un monitor è un gestore di un evento. L'evento (con l'invio di un messaggio) è un'attività atomica, ed è preceduta da un evento _before_ e da un evento _after_. Il monitor gestisce tali eventi mediante i predicati, `before/3` e `after/3`, che sono chiamati rispettivamente prima e dopo il verificarsi dell'evento. Un monitor può inoltre interrogare, registrare e cancellare un evento nel registro eventi a livello di sistema il quale che associa gli eventi con i monitor. Ad esempio, un semplice tracer per ogni messaggio inviato utilizzando il costrutto `::/2` può essere definito come:

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

Supponendo che l'oggetto `tracer` e l'oggetto `list` definito in precedenza siano stati già compilati e caricati, si possono osservare i gestori di eventi in azione durante l'invio di un messaggio:

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

Gli eventi possono essere impostati e cancellati dinamicamente in fase di esecuzione chiamando i predicati predefiniti `define_events/5` e` abolish_events/5` .

La programmazione event-driven può essere vista come una forma di _computational reflection_. Si noti però che gli eventi sono generati solo quando si utilizza il costrutto di controllo per l'invio di messaggi `::/2`.

# Espressioni lambda

Logtalk supporta anche le espressioni lambda. I parametri della espressioni lambda sono rappresentati mediante una lista con l'operatore infisso `(>>)/2` che collega i parametri alla relativa lambda espressione. Ecco alcuni semplici esempi di che usano i meta-predicati.


```logtalk
?- {library(metapredicates_loader)}.
yes

?- meta::map([X,Y]>>(Y is 2*X), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

Logtalk supporta anche il _currying_:

```logtalk
?- meta::map([X]>>([Y]>>(Y is 2*X)), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

Infine, le variabili libere Lambda possono essere espresso usando la sintassi estesa `{Free1, ...}/[Parameter1, ...]>>Lambda`.

# Macro

I Termini e goal nel file sorgente possono essere _estesi_ al momento della compilazione specificando una hook ad un oggetto (_hook object_) che definisce le regole di riscrittura dei termini e riscrittura dei quesiti. Ad esempio, si consideri il seguente oggetto semplice, salvato nel file `source.lgt`:

```logtalk
:- object(source).

	:- public(bar/1).
	bar(X) :- foo(X).

	foo(a). foo(b). foo(c).

:- end_object.
```

Si supponga il seguente hook all'oggetto, salvato nel file `my_macros.lgt`, che estende le clausole e chiama il predicato locale  `foo/1`:

```logtalk
:- object(my_macros,
	implements(expanding)).    % built-in protocol for expanding predicates

	term_expansion(foo(Char), baz(Code)) :-
		char_code(Char, Code). % standard built-in predicate

	goal_expansion(foo(X), baz(X)).

:- end_object.
```

Dopo aver caricato il file contenente la macro, si può espandere il nostro file sorgente usando il flag del compilatore `hook`:

```logtalk
?- logtalk_load(my_macros), logtalk_load(source, [hook(my_macros)]).
yes

?- source::bar(X).
X = 97 ;
X = 98 ;
X = 99
true
```

La libreria Logtalk fornisce infine il supporto per combinare hook agli oggetti utilizzando diversi modi (ad esempio, definendo una pipeline di espansioni).


# Maggiori informazioni

Visita il [Sito web di Logtalk (en)](http://logtalk.org) per maggiori informazioni.
