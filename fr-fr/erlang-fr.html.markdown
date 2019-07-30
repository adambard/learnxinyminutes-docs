---
language: erlang
contributors:
    - ["Giovanni Cappellotto", "http://www.focustheweb.com/"]
translators:
    - ["Julien Cretel", "https://github.com/Jubobs"]
filename: learnerlang-fr.erl
lang: fr-fr
---

```erlang
% Un signe pour cent marque le début d'un commentaire de fin de ligne.

%% Deux signes pour cent sont utilisés pour commenter les fonctions.

%%% Trois signes pour cent sont utilisés pour commenter les modules.

% Trois symboles de ponctuation sont utilisés en Erlang.
% Les virgules (`,`) servent à séparer les paramètres dans les appels de
% fonctions, les contructeurs, et les motifs.
% Les points (`.`) (suivis par des blancs) servent à séparer les fonctions et
% les expressions dans l'interpréteur.
% Les points-virgules (`;`) servent à séparer les clauses. Ces dernières
% apparaissent dans différent cas de figure : définitions de fonctions et
% expressions `case`, `if`, `try..catch`, `receive`.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Variables et filtrage par motif
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(L'équivalent anglais de *filtrage par motif* est *pattern matching*.)

Nb = 42.  % Chaque nom de variable doit commencer par une lettre majuscule.

% Les variables Erlang ne peuvent être affectées qu'une seule fois ; si vous
% essayez d'affecter une autre valeur à la variable `Nb`, vous obtiendrez
% une erreur.
Nb = 43. % ** exception error: no match of right hand side value 43

% Dans la plupart des languages, `=` indique une affectation. En Erlang,
% cependant, `=` indique un filtrage par motif. En fait, `Gauche = Droit`
% signifie ce qui suit : évalue le côté droit (`Droit`), et ensuite filtre le
% résultat à l'aide du motif du côté gauche (`Gauche`).
Nb = 7 * 6.

% Nombre en virgule flottante.
Pi = 3.14159.

% Les atomes représentent des valeurs constantes non-numériques. Un atome
% commence par une lettre minuscule, suivie d'une séquence composée de
% caractères alphanumériques, de tirets bas (`_`), ou d'arobases (`@`).
Bonjour = bonjour.
AutreNoeud = exemple@noeud.

% Les atomes de valeur autre qu'alphanumérique peuvent être délimités par
% des guillemets droits simples.
AtomeAvecEspace = 'un atome contenant des espaces'.

% Les tuples sont similaires aux enregistrements du language C.
Point = {point, 10, 45}.

% Pour extraire des valeurs d'un tuple, on filtre par motif avec
% l'opérateur `=`.
{point, X, Y} = Point.  % X = 10, Y = 45

% On peut utiliser `_` comme caractère joker pour les variables qui ne nous
% intéressent pas. Le symbole `_` est appelé variable muette. Contrairement
% aux variables normales, de multiples apparitions de `_` dans un même motif
% ne lient pas nécessairement à la même valeur.
Personne = {personne, {nom, {prenom, joe}, {famille, armstrong}},
                      {pointure, 42}}.
{_, {_, {_, Qui}, _}, _} = Personne.  % Qui = joe

% Pour créer une liste, on écrit les éléments de la liste entre crochets, en
% les séparant par des virgules.
% Les éléments d'une liste peuvent avoir n'importe quel type.
% Le premier élément d'une liste est appelé la tête de la liste. Si on retire
% la tête d'une liste, ce qui reste est appelée la queue de la liste.
Articles = [{pommes, 10}, {poires, 6}, {lait, 3}].

% Si `Q` est une liste, alors `[T|Q]` est aussi une liste dont la tête est `T`
% et dont la queue est `Q`. La barre verticale (`|`) sépare la tête d'une
% liste de sa queue.
% `[]` est la liste vide.
% On peut extraire des éléments d'une liste par filtrage de motif. Si `L` est
% une liste non vide, alors l'expression `[X|Y] = L`, où `X` et `Y` sont des
% variables non affectées, va extraire la tête de la liste dans `X` et la
% queue de la liste dans `Y`.
[PremierArticle|AutresArticles] = Articles.
% PremierArticle = {pommmes, 10}
% AutresArticles = [{poires, 6}, {lait, 3}]

% Il n'y a pas de chaînes de caractères en Erlang. Les chaînes de caractères
% ne sont rien de plus que des listes d'entiers.
% Les chaînes de caractères sont délimitées par des guillemets droits doubles
% (`"`).
Nom = "Bonjour".
[66, 111, 110, 106, 111, 117, 114] = "Bonjour".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. Programmation séquentielle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Les modules constituent l'unité de base d'un programme Erlang. Toutes les
% fonctions que l'on écrit sont enregistrées dans des modules. Les modules sont
% enregistrés dans des fichiers avec une extension `.erl`.
% Les modules doivent être compilés afin d'éxecuter le programme.
% Un module compilé a une extension `.beam`.
-module(geometrie).
-export([aire/1]). % la liste des fonctions exportées par le module.

% La fonction `aire` est composée de deux clauses. Les clauses sont séparées
% par un point-virgule, et la dernière clause est suivie d'un point et un
% espace blanc. Chaque clause a une en-tête et un corps ; l'en-tête consiste
% en un nom de fonction suivi d'un motif (entre parenthèses), et le corps
% consiste en une séquence d'expressions, qui sont évaluées si le motif de
% l'en-tête est cohérent par rapport à la valeur des paramètres d'appel.
% L'expression est filtrée séquentiellement par les différents motifs, dans
% l'ordre dans lequel ils apparaissent dans la définition de la fonction.
aire({rectangle, Largeur, Hauteur}) -> Largeur * Hauteur;
aire({cercle, R})                   -> 3.14159 * R * R.

% Compilation du code du fichier geometrie.erl.
c(geometrie).  % {ok,geometrie}

% Le nom du module doit être inclus avec le nom de la fonction afin
% d'identifier précisément quelle fonction on souhaite appeler.
geometrie:aire({rectangle, 10, 5}).  % 50
geometrie:area({cercle, 1.4}).  % 6.15752

% En Erlang, deux fonctions portant le même nom mais ayant des arités
% différentes (c'est à dire ne prenant pas le même nombre de paramètres)
% au sein d'un même module représentent des fonctions complètement
% différentes.
-module(lib_divers).
-export([somme/1]). % exporte la fonction `somme` d'arité 1
                    % acceptant un paramètre : une liste d'entiers.
somme(L) -> somme(L, 0).
somme([], N)    -> N;
somme([T|Q], N) -> somme(Q, T+N).

% Les `fun`s sont des fonctions "anonymes" ; elles sont appelées ainsi parce
% qu'elles n'ont pas de nom. Cependant, elles peuvent être affectées à des
% variables.
Doubler = fun(X) -> 2 * X end. % `Doubler` pointe vers une fonction anonyme
                               % dont le handle est : #Fun<erl_eval.6.17052888>
Doubler(2).  % 4

% Les fonctions peuvent prendre des `fun`s comme paramètres et peuvent renvoyer
% des `fun`s.
Mult = fun(Fois) -> ( fun(X) -> X * Fois end ) end.
Tripler = Mult(3).
Tripler(5).  % 15

% Les listes en compréhension sont des expressions qui créent des listes sans
% requérir ni `fun`s, ni maps, ni filters.
% La notation `[F(X) || X <- L]` signifie "la liste des `F(X)` où `X` est
% extrait de la liste `L`."
L = [1,2,3,4,5].
[2 * X || X <- L].  % [2,4,6,8,10]
% Une liste en compréhension peut être constituée de générateurs, ainsi que de
% gardes, qui sélectionnent un sous-ensemble des valeurs générées.
NombresPairs = [N || N <- [1, 2, 3, 4], N rem 2 == 0]. % [2, 4]

% La garde est un élément syntaxique qui rend le filtrage par motif encore
% plus puissant. Les gardes permettent de d'effectuer de simple tests et
% comparaisons sur les variables d'un motif. Les gardes peuvent être
% utilisées dans les en-têtes de fonctions, au sein desquelles elles sont
% introduites par le mot-clé `when`, ou encore à n'importe quel endroit où
% une expression est autorisée.
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

% Une garde est une série d'expressions gardes, séparées par des virgules (`,`).
% La garde `ExprGarde1, ExprGarde2, ..., ExprGardeN` est vraie si toutes les
% expressions gardes `ExprGarde1`, `ExprGarde2, ..., `ExprGardeN` ont pour
% valeur `true`.
est_chat(A) when is_atom(A), A =:= chat -> true;
est_chat(A) -> false.
est_chien(A) when is_atom(A), A =:= chien -> true;
est_chien(A) -> false.

% Une séquence de gardes est composée soit d'une seule garde ou bien d'une
% série de gardes, séparées par des points-virgules (`;`). La séquence de
% gardes `G1; G2; ...; Gn` est vraie si au moins l'une des gardes `G1`, `G2`,
% ..., `Gn` a pour valeur `true`.
est_animal(A) when is_atom(A), (A =:= chien) or (A =:= chat) -> true;
est_animal(A)                                                -> false.

% Attention : toutes les expressions Erlang valides ne peuvent pas être
% utilisées comme expressions gardes ; en particulier, nos fonctions
% `est_chat` et `est_chien` ne sont pas autorisées au sein de la séquence de
% gardes dans la définition de `est_animal`. Pour plus de détails sur les
% expressions autorisées ands les séquences de gardes, voir cette
% [section](http://erlang.org/doc/reference_manual/expressions.html#id81912)
% du manuel Erlang.

% Les enregistrements permettent d'associer un nom à un certain élément dans
% un tuple.
% Les enregistrements peuvent être définis dans des fichiers sources Erlang
% ou bien dans des fichiers avec une extension `.hrl`, qui sont ensuite inclus
% dans des fichiers sources Erlang.
-record(afaire, {
  statut = rappel,  % Valeur par défaut
  qui = joe,
  texte
}).

% Les définitions d'enregistrements doivent être lues dans l'interpreteur
% pour qu'on puisse définir un enregistrement. On utilise la fonction `rr`
% (abbréviation de *read records* en anglais, ou *lire enregistrements* en
% français) pour ça.
rr("enregistrements.hrl").  % [afaire]

% Création et mise à jour d'enregistrements :
X = #afaire{}.
% #afaire{statut = rappel, qui = joe, texte = undefined}
X1 = #afaire{statut = urgent, texte = "Corriger erreurs dans livre"}.
% #afaire{statut = urgent, qui = joe, texte = "Corriger erreurs dans livre"}
X2 = X1#afaire{statut = fini}.
% #afaire{statut = fini, qui = joe, texte = "Corriger erreurs dans livre"}

% Expressions `case`.
% `filter` renvoie une liste de tous les éléments `X` d'une liste `L` pour
% lesquels `P(X)` est vrai.
filter(P, [H|T]) ->
  case P(H) of
    true -> [H|filter(P, T)];
    false -> filter(P, T)
  end;
filter(P, []) -> [].
filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4]). % [2, 4]

% Expressions `if`.
max(X, Y) ->
  if
    X > Y -> X;
    X < Y -> Y;
    true -> nil
  end.

% Attention : au moins l'une des gardes dans l'expression `if` doit avoir pour
% valeur `true` ; autrement, une exception sera lancée.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. Exceptions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Des exceptions sont lancées par le système quand des erreurs internes
% surviennent, ou de manière explicite dans le programme en appelant
% `throw(Exception)`, `exit(Exception)`, ou `erlang:error(Exception)`.
generer_exception(1) -> a;
generer_exception(2) -> throw(a);
generer_exception(3) -> exit(a);
generer_exception(4) -> {'EXIT', a};
generer_exception(5) -> erlang:error(a).

% Erlang dispose de deux méthodes pour capturer une exception. La première
% consiste à inclure l'appel de de la fonction qui lance l'exception dans une
% expression `try...catch`.
catcher(N) ->
  try generer_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.

% L'autre méthode consiste à inclure l'appel dans une expression `catch`.
% Quand une exception est capturée, elle est convertie en un tuple qui décrit
% l'erreur.
catcher(N) -> catch generer_exception(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4. Concurrence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Erlang est basé sur le modèle d'acteur pour la concurrence. Seulement trois
% opérations sont requises pour écrire des programmes concurrents en Erlang :
% la création de processus, l'envoi de messages, et la réception de messages.

% Pour démarrer un nouveau processus, on utilise la fonction `spawn`, qui
% prend une fonction comme paramètre.

F = fun() -> 2 + 2 end. % #Fun<erl_eval.20.67289768>
spawn(F). % <0.44.0>

% `spawn` renvoie un pid (*process identifier* en anglais, ou *identifiant de
% processus* en français), qui peut être utilisé pour envoyer des messages au
% processus en question. Pour passer des messages, on utilise l'opérateur `!`.
% Pour que cela soit utile, on doit aussi être en mesure de recevoir des
% messages, ce qui est accompli grâce à une clause `receive` :

-module(calculerGeometrie).
-compile(export_all).
calculerAire() ->
    receive
      {rectangle, W, H} ->
        W * H;
      {cercle, R} ->
        3.14 * R * R;
      _ ->
        io:format("Seule l'aire d'un rectangle / cercle peut etre calculee.")
    end.

% Compilation du module and création d'un processus qui évalue `calculerAire`
% dans l'interpréteur.
c(calculerGeometrie).
CalculerAire = spawn(calculerGeometrie, calculerAire, []).
CalculerAire ! {cercle, 2}. % 12.56000000000000049738

% L'interpréteur est lui-même un processus ; on peut utiliser `self` pour
% obtenir le pid actuel.
self(). % <0.41.0>

```

## Ressources (en anglais)

* ["Learn You Some Erlang for great good!"](http://learnyousomeerlang.com/)
* ["Programming Erlang: Software for a Concurrent World" by Joe Armstrong](http://pragprog.com/book/jaerlang/programming-erlang)
* [Erlang/OTP Reference Documentation](http://www.erlang.org/doc/)
* [Erlang - Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)
