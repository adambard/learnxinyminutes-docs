---
language: Logtalk
filename: learnlogtalk.lgt
contributors:
    - ["Paulo Moura", "http://github.com/pmoura"]
    - ["Adrián Arroyo Calle", "https://adrianistan.eu"]
lang: es-es
---

Logtalk nació en 1998 debido a los problemas de Prolog al escribir aplicaciones grandes. Diseñado por Paulo Moura, se trata de acercar Smalltalk y Prolog. De este modo el elemento fundamental de Logtalk serán los objetos, no las clases. A través de objetos podremos hacer clases si es lo que deseamos, o podemos usarlos como prototipos. Logtalk se ejecuta por encima de un sistema Prolog, sobre todo aquellos que implementen el estándar ISO Prolog. Actualmente Logtalk soporta: B-Prolog, Ciao Prolog, CxProlog, ECLiPSe, GNU Prolog, JIProlog, LVM, Quintus Prolog, Scryer Prolog, SICStus Prolog, SWI Prolog, Tau Prolog, Trealla Prolog, XSB y YAP. El código 100% Logtalk debería ser portable entre estos sistemas.

Podemos instalar Logtalk desde [logtalk.org](https://logtalk.org/).

En Logtalk existen tres tipos de entidades: objetos, protocolos y categorías.

# Objetos

El elemento fundamental de Logtalk es el objeto. Veremos que casi todo en Logtalk son objetos. Estos objetos no tienen estado ya que son puramente declarativos. Con los objetos podemos construir jerarquías tipo prototipo (ideal para objetos únicos) y tipo clase (ideal para objetos de los que se harán muchos).

```logtalk
:- object(python).

    :- public(say_creator/0).
    say_creator :-
        write('Created by: Guido van Rossum'),nl.

:- end_object.
```

Este código crea un objeto llamado _python_ con un predicado o método público, llamado _say\_creator_ con aridad 0. El predicado en sí lo que hace es imprimir por pantalla un texto. Lo vamos a guardar en un fichero llamado langs.lgt (lgt es la extensión de Logtalk).

Vamos a ejecutarlo. Para ello, lanzamos el comando de Logtalk de nuestro Prolog, por ejemplo, en Scryer Prolog, es _scryerlgt_. Si usas SWI Prolog, es _swilgt_. Si todo va bien, verás una terminal similar a la de Prolog. Una vez allí, cargamos el fichero con _logtalk\_load_.

```logtalk
?- logtalk_load('langs.lgt').
```

Si todo va bien, ahora podremos mandar un mensaje al objeto python.

```logtalk
?- python::say_creator.
Creator: Guido van Rossum
   true.
```

Si no existe predicado en el objeto o si la visibilidad del método no lo permite, el envío falla. Se podría decir mucho de los mensajes como que emiten eventos, se puede hacer broadcasting, etc pero esto es más avanzado.

Los objetos pueden: _implementar_ protocolos, _importar_ categorías, _extender_ otros objetos, _instanciar_ un objeto (clase) y _especializar_ otro objeto (clase). Las jerarquías de objetos se pueden realizar mediante extensiones (prototipos) o mediante instancias y especializaciones (clases).

Por ejemplo, un objeto que _extienda_ otro se haría así:

```logtalk
:- object(animal).

    :- public(run/0).
    run :-
        write('Animal is running'),nl.

:- end_object.

:- object(dog, extends(animal)).

    :- public(sleep/0).
    sleep :-
        write('Dog is sleeping'),nl.
:- end_object.
```

Si cargamos el fichero, veremos que el objeto animal tiene solo el método run, mientras que dog tiene el método run y sleep.

Aquí podemos introducir también las otras dos formas de mandar un mensaje. Con _::_ podemos mandar un mensaje al mismo objeto en el que estamos. Y con _^^_ podemos mandar un mensaje al superobjeto (es decir, del que estamos extendiendo).

```logtalk
:- object(animal).

    :- public(run/0).
    run :-
        write('Animal is running'),nl.

:- end_object.

:- object(dog, extends(animal)).

    :- public(run/0).
    run :-
        write('Dog is running'),nl,
        ^^run.

    :- public(sleep/0).
    sleep :-
        write('Dog is sleeping'),nl.

    :- public(run_and_sleep/0).
    run_and_sleep :-
        ::run,
        ::sleep.
:- end_object.
```

Al ejecutar _dog::run_and_sleep/0_ obtendremos:

```
Dog is running
Animal is running
Dog is sleeping
```

Adicionalmente podemos usar _self/1_ para obtener una referencia al objeto actual. _run\_and\_sleep/0_ puede escribirse también así:

```logtalk
    run_and_sleep :-
        self(Self),
        Self::run,
        Self::sleep.
```

## Crear objetos de forma dinámica

Si has programado en otros lenguaje OOP quizá te esté extrañando que todos los objetos existan desde el principio en Logtalk. Afortunadamente, esto era porque no habíamos llegado todavía a la parte donde se crean de forma dinámica. Esto se hace mediante el predicado _create\_object/4_.

El primer argumento será la variable o el nombre del objeto. Seguidamente irán las relaciones. En tercer lugar las directivas (ahora las vemos pero básicamente public, private, info,...) y en cuarto lugar las cláusulas.

Por ejemplo, estos dos códigos son equivalentes:

```logtalk
?- create_object(foo, [extends(bar)], [public(foo/1)], [foo(1), foo(2)]).

%%%%

:- object(foo, extends(bar)).

    :- dynamic.

    :- public(foo/1).
    foo(1).
    foo(2).

:- end_object.
```

Lo normal es dejar la creación de objetos dinámicos en un objeto prototito o en una metaclase.

Los objetos dinámicos se pueden borrar con _abolish\_object/1_.

## Objetos paramétricos

Existen unos objetos algo diferentes, los objetos paramétricos. Son objetos cuyo identificador no es un átomo sino un término compuesto de Prolog. De este modo los propios objetos cargan con información en su definición de nombre.

```logtalk
:- object(ruby(_Creator)).

    :- public(say_creator/0).
    say_creator :-
        write('Created by: '),
        parameter(1, Creator),
        write(Creator),nl.

:- end_object.
```

Y lo tendremos que ejecutar de esta forma: _ruby('Matz')::say\_creator._. Los objetos paramétricos también pueden usar _this/1_ y la sintaxis de doble underscore (\_Creator\_). Pero como siempre, para estas cosas extra, lo recomendable es mirar el Logtalk Handbook.

# Protocolos

Lo siguiente dentro de Logtalk son los protocolos. Los protocolos permiten separar la interfaz de la implementación. Los protocolos solo incluyen las declaraciones de los predicados, pero no su lógica.

```logtalk
:- protocol(langp).

    :- public(say_creator/0).

:- end_protocol.

:- object(python, implements(langp)).

    say_creator :-
        write('Created by: Guido van Rossum'),nl.

:- end_object.
```

Ahora definimos un protocolo llamado langp, que el objeto python implementa. Ahora ya no necesitamos incluir la directiva de visibilidad en el objeto ya que esa forma parte de la interfaz al exterior del objeto.

# Categorías

Las categorías son unidades de código reutilizables. Podemos pensar en ellos como partes de un objeto que se pueden añadir a otros objetos.

```logtalk
:- category(reverse_creator(_Creator_)).

    :- uses(list, [reverse/2]).

    :- public(reverse_creator/1).
    reverse_creator(Reversed) :-
        reverse(_Creator_, Reversed).

:- end_category.

:- object(ruby(CreatorParam), implements(langp), imports(reverse_creator(CreatorParam))).

    say_creator :-
        write('Created by: '),
        parameter(1, Creator),
        write(Creator),nl.

:- end_object.
```

En este ejemplo, vamos a tener una categoría (paramétrica) y vamos a importarlo desde nuestro objeto paramétrico. Así podemos ver como los objetos paramétricos se pueden pasar los parámetros entre sí. Además, en el ejemplo se usa la directiva use, que nos permite "importar" ciertos predicados de algunos objetos (en este caso del objeto list) para no tener que escribir la sintaxis larga de mandar mensaje.

Para que esto funcione, vamos a tener que cargar la librería con el objeto list en Logtalk primero.

```logtalk
?- logtalk_load(types(loader)).
    ....
?- logtalk_load('langs.lgt').
    ....
?- ruby("Matz")::reverse_creator(X).
    X = "ztaM".
```

Esto nos lleva a la pregunta de, ¿hay alguna forma de organizar los proyectos en Logtalk?

# Estructura proyecto Logtalk

Por norma general un proyecto Logtalk consta de archivos lgt. Hay dos especiales, loader.lgt y tester.lgt.

loader.lgt deberá contener todo lo necesario para cargar la aplicación, normalmente cargar todos los ficheros y librerías en el orden correcto.

```logtalk
:- initialization((
    logtalk_load(sets(loader)),
    logtalk_load(meta(loader)),
    logtalk_load(app)
)).
```

Este es un ejemplo de fichero loader, que carga las librerías sets, meta y el fichero app.lgt.

El fichero tester.lgt es igual, pero deberá incluir una llamada a ejecutar los tests de lgtUnit. lgtUnit es el framework de testing de Logtalk, muy interesante pero del que hablaré en otro artículo.

# Lambdas y más

Logtalk incluye predicados lambda, es decir, predicados definidos sobre un metapredicado.

```logtalk
?- meta::map([X, Y]>>(Y is 2*X), [1,2,3], Ys).
   Ys = [2,4,6].
```

En este caso, el predicado lambda es equivalente al predicado:

```logtalk
times2(X, Y) :- Y is 2 * X.
```

Pero sin tener que haber definido un nombre ni ubicarlo en otro sitio.

Logtalk incluye más funcionalidad aparte de objetos, protocolos y categorías. Eventos, multithreading, type checking, debugger, documentation, tests, etc. Definitivamente, escribir sobre todo ello llevaría un buen rato.

Esto es solo una introducción a Logtalk. Aparte del [handbook](https://logtalk.org/manuals/index.html) también puedes ver los [ejemplos del repo de GitHub](https://github.com/LogtalkDotOrg/logtalk3/).
