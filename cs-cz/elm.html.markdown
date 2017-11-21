---
language: Elm
contributors:
    - ["Max Goldstein", "http://maxgoldste.in/"]
translators:
    - ["Robin Pokorný", "http://robinpokorny.com/"]
filename: learnelm-cz.elm
lang: cs-cz
---

Elm je funkcionální reaktivní jazyk, který se kompiluje do (klientského) JavaScriptu.
Elm je silně typovaný, díky tomu je překladač schopen zachytit většinu chyb okamžitě a
vypsat snadno srozumitelná chybová hlášení.
Elm se hodí k tvorbě webových uživatelských rozhraní a her.


```haskell
-- Jednořádkové komentáře začínají dvěma pomlčkami.
{- Víceřádkové komentáře mohou být takto uzavřeny do bloku.
{- Mohou být i zanořeny. -}
-}

{-- Základy --}

-- Aritmetika
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20

-- Každé číslo bez desetinné tečky je typu Int nebo Float.
33 / 2 -- 16.5 s reálným dělením
33 // 2 -- 16 s celočíselným dělením

-- Umocňování
5 ^ 2 -- 25

-- Pravdivostní proměnné
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- Řetězce a znaky
"Toto je textový řetězec, protože používá dvojité uvozovky."
'a' -- znak v jednoduchých uvozovkách

-- Řetězce lze spojovat.
"Ahoj " ++ "světe!" -- "Ahoj světe!"

{-- Seznamy (List), n-tice (Tuple) a Záznamy (Record) --}

-- Každá položka seznamu musí být stejného typu.
["příliš", "žluťoučký", "kůň", "úpěl"]
[1, 2, 3, 4, 5]
-- Druhý příklad lze zapsat také pomocí dvou teček.
List.range 1 5

-- Spojovat seznamy lze stejně jako řetězce.
List.range 1 5 ++ List.range 6 10 == List.range 1 10 -- True

-- K přidání položky do seznamu použijte funkci "cons".
0 :: List.range 1 5 -- [0, 1, 2, 3, 4, 5]

-- Funkce "head" pro získání první položky seznamu i funkce "tail" pro získání následujích položek
-- vrací typ Maybe. Místo zjišťování, jestli nějaká položka není null,
-- se s chybějcími hodnotami vypořádáme explicitně.
List.head (List.range 1 5) -- Just 1
List.tail (List.range 1 5) -- Just [2, 3, 4, 5]
List.head [] -- Nothing
-- List.nazevFunkce odkazuje na funkci, která žije v modulu List.

-- Každý prvek v n-tici může být jiného typu, ale n-tice má pevný počet prvků.
("elm", 42)

-- K získání hodnot z dvojice použijte funkce first a second.
-- (Toto je pouze zkratka. Brzy si ukážeme, jak na to "správně".)
fst ("elm", 42) -- "elm"
snd ("elm", 42) -- 42

-- Prázná n-tice, neboli "unit", se občas používá jako zástupný symbol.
-- Je to jediná hodnota svého typu, který se také nazývá "Unit".
()

-- Záznamy jsou podobné n-ticím, ale prvky jsou pojmenovány. Na pořadí nezáleží.
-- Povšimněte si, že hodnoty vlastností se přiřazují rovnítky, ne dvojtečkami.
{ x = 3, y = 7 }

-- K hodnotám se přistupuje pomocí tečky a názvu vlastnosti.
{ x = 3, y = 7 }.x -- 3

-- Nebo využitím přístupové funkce, což je jen tečka a název vlastnosti.
.y { x = 3, y = 7 } -- 7

-- Změna hodnoty vlastnosti v záznamu. (Záznam tuto vlastnost už musí mít.)
{ osoba |
  jmeno = "Jiří" }

-- Změna více vlastností s využitím aktuálních hodnot.
{ hmotnyBod |
  poloha = hmotnyBod.poloha + hmotnyBod.rychlost,
  rychlost = hmotnyBod.rychlost + hmotnyBod.zrychleni }

{-- Řídicí struktury --}

-- Podmínky vždy musí mít větev "else" a obě větve musí být stejného typu.
if powerLevel > 9000 then
  "PÁNI!"
else
  "hmm"

-- Podmínky lze skládat za sebe.
if n < 0 then
  "n je záporné"
else if n > 0 then
  "n je kladné"
else
  "n je nula"

-- Použíjte příkaz "case" k nalezení shody vzoru a různých možností.
case seznam of
  [] -> "odpovídá práznému seznamu"
  [x]-> "odpovídá seznamu o právě jedné položce, " ++ toString x
  x::xs -> "odpovídá seznamu o alespoň jedné položce, jehož prvním prvkem je " ++ toString x
-- Shody se vyhodnocují v zapsaném pořadí. Kdybychom umístili [x] poslední, nikdy by nenastala shoda,
-- protože x::xs také odpovídá  (xs by byl prázdný seznam). Shody "nepropadají".
-- Překladač vždy upozorní na chybějící nebo přebývající větve.

-- Větvení typu Maybe.
case List.head seznam of
  Just x -> "První položka je " ++ toString x
  Nothing -> "Seznam byl prázdný."

{-- Funkce --}

-- Syntaxe funkcí je v Elmu velmi úsporná, založená spíše na mezerách
-- než na závorkách. Neexistuje tu klíčové slovo "return".

-- Funkci definujeme jejím jménem, parametry, rovnítkem a tělem.
vynasob a b =
  a * b

-- Funkci voláme předáním parametrů (bez oddělujících čárek).
vynasob 7 6 -- 42

-- Částečně aplikované funkci předáme pouze některé parametry.
-- Poté zvolíme nové jméno.
zdvoj =
  vynasob 2

-- Konstanty jsou podobné, ale nepřijímají žádné parametry.
odpoved =
  42

-- Předejte funkci jako parametr jiným funkcím.
List.map zdvoj (List.range 1 4) -- [2, 4, 6, 8]

-- Nebo použijte anonymní funkci.
List.map (\a -> a * 2) (List.range 1 4) -- [2, 4, 6, 8]

-- V definici funkce lze zapsat vzor, může-li nastat pouze jeden případ.
-- Tato funkce přijímá jednu dvojici místo dvou parametrů.
obsah (sirka, delka) =
  sirka * delka

obsah (6, 7) -- 42

-- Složenými závorkami vytvořte vzor pro názvy vlastností v záznamu.
-- Použijte "let" k definici lokálních proměnných.
objem {sirka, delka, hloubka} =
  let
    obsah = sirka * delka
  in
    obsah * hloubka

objem { sirka = 3, delka = 2, hloubka = 7 } -- 42

-- Funkce mohou být rekurzivní.
fib n =
  if n < 2 then
    1
  else
    fib (n - 1) + fib (n - 2)

List.map fib (List.range 0 8) -- [1, 1, 2, 3, 5, 8, 13, 21, 34]

-- Jiná rekurzivní funkce (v praxi použijte List.length).
delkaSeznamu seznam =
  case seznam of
    [] -> 0
    x::xs -> 1 + delkaSeznamu xs

-- Funkce se volají před jakýmkoli infixovým operátorem. Závorky určují prioritu.
cos (degrees 30) ^ 2 + sin (degrees 30) ^ 2 -- 1
-- Nejprve se aplikuje "degrees" na číslo 30, výsledek je pak předán trigonometrickým
-- funkcím, které jsou následně umocněny na druhou, na závěr proběhne sčítání.

{-- Typy a typové anotace --}

-- Překladač odvodí typ každé hodnoty ve vašem programu.
-- Typy vždy začínají velkým písmenem. Čtete x : T jako "x je typu T".
-- Některé běžné typy, které můžete videt v Elmovém REPLu.
5 : Int
6.7 : Float
"ahoj" : String
True : Bool

-- Funkce mají také typy. Čtěte "->" jako "vrací".
-- O typu na konci uvažujte jako návratovém typu, o ostatních jako typech argumentů.
not : Bool -> Bool
round : Float -> Int

-- Když definujete hodnotu, je dobrým zvykem zapsat nad ni její typ.
-- Anotace je formou dokumentace, která je ověřována překladačem.
zdvoj : Int -> Int
zdvoj x = x * 2

-- Funkce jako parametr je uzavřena v závorkách.
-- Typy s malým počátečním písmenem jsou typové proměnné:
-- mohou být libovolného typu, ale v každém volání musí být stejné.
List.map : (a -> b) -> List a -> List b
-- "List tečka map je typu a-vrací-b, vrací seznam-položek-typu-a, vrací seznam-položek-typu-b."

-- Existují tři speciální typové proměnné:
-- číslo (number), porovnatelné (comparable), and spojitelné (appendable).
-- Čísla dovolují použít aritmetiku na Int a Float.
-- Porovnatelné dovolují uspořádat čísla a řetězce, např. a < b.
-- Spojitelné lze zřetězit pomocí a ++ b.

{-- Typové aliasy a výčtové typy --}

-- Pro záznamy a n-tice již typy automaticky existují.
-- (Povšimněte si, že typ vlatnosti záznamu přiřazujeme dvojtečkou a hodnotu rovnítkem.)
pocatek : { x : Float, y : Float, z : Float }
pocatek =
  { x = 0, y = 0, z = 0 }

-- Stávajícím typům lze dávat jména využitím aliasů.
type alias Bod3D =
  { x : Float, y : Float, z : Float }

-- Alias pro záznam funguje také jako jeho konstruktor.
jinyPocatek : Bod3D
jinyPocatek =
  Bod3D 0 0 0

-- Jedná se stále o stejný typ, lze je tedy porovnat.
pocatek == jinyPocatek -- True

-- Oproti tomu výčtový (union) typ definuje zcela nový typ.
-- Výčtový typ se takto jmenuje, protože může být jedním z několika vybraných možností.
-- Každá možnost je reprezentována jako "tag".
type Smer =
  Sever | Jih | Vychod | Zapad

-- Tagy mohou obsahovat další hodnoty známých typů. Lze využít i rekurze.
type IntStrom =
  Vrchol | Uzel Int IntStrom IntStrom
-- "Vrchol" i "Uzel" jsou tagy. Vše, co následuje za tagem, je typ.

-- Tagy lze použít jako hodnoty funkcí.
koren : IntStrom
koren =
  Vrchol 7 List List

-- Výčtové typy (a typové aliasy) mohou obsahovat typové proměnné.
type Strom a =
  Vrchol | Uzel a (Strom a) (Strom a)
-- "Typ strom-prvků-a je vrchol, nebo uzel obsahující a, strom-prvků-a a strom-prvků-a."

-- Vzory se shodují s tagy. Tagy s velkým počátečním písmenem odpovídají přesně.
-- Proměnné malým písmem odpovídají čemukoli. Podtržítko také odpovídá čemukoli,
-- ale určuje, že tuto hodnotu dále nechceme používat.
nejviceVlevo : Strom a -> Maybe a
nejviceVlevo strom =
  case strom of
    Vrchol -> Nothing
    Uzel x Vrchol _ -> Just x
    Uzel _ podstrom _ -> nejviceVlevo podstrom

-- To je víceméně vše o jazyku samotném.
-- Podívejme se nyní, jak organizovat a spouštět náš kód.

{-- Moduly a importování --}

-- Standardní knihovny jsou organizovány do modulů, stejně jako knihovny třetích stran,
-- které můžete využívat. Ve větších projektech můžete definovat vlastní moduly.

-- Vložte toto na začátek souboru. Pokud nic neuvedete, předpokládá se "Main".
module Jmeno where

-- Výchozím chováním je, že se exportuje vše.
-- Případně můžete definovat exportované vlastnosti explicitně.
module Jmeno (MujTyp, mojeHodnota) where

-- Běžný návrhový vzor je expotovat pouze výčtový typ bez jeho tagů.
-- Tento vzor je znám jako krycí typ a často se využívá v knihovnách.

-- Z jiných modulů lze importovat kód a použít jej v aktuálním modulu.
-- Nasledující umístí Dict do aktuálního scope, takže lze volat Dict.insert.
import Dict

-- Importuje modul Dict a typ Dict, takže v anotacích není nutné psát Dict.Dict.
-- Stále lze volat Dict.insert.
import Dict exposing (Dict)

-- Přejmenování importu.
import Graphics.Collage as C

{-- Porty --}

-- Port oznamuje, že budete komunikovat s vnějším světem.
-- Porty jsou dovoleny pouze v modulu Main.

-- Příchozí port je jen typová anotace.
port idKlienta : Int

-- Odchozí port má definici.
port objednavkaKlienta : List String
port objednavkaKlienta = ["Knihy", "Potraviny", "Nábytek"]

-- Nebudeme zacházet do detailů, ale v JavaScriptu se dají nastavit
-- callbacky pro zasílání na příchozí porty a čtení z odchozích portů.

{-- Nástroje pro příkazovou řádku --}

-- Kompilace souboru.
$ elm make MujSoubor.elm

-- Při prvním spuštění nainstaluje Elm standardní knihovny a vytvoří soubor
-- elm-package.json, kde jsou uloženy informace o vašem projektu.

-- Elm reactor je server, který překládá a spouští vaše soubory.
-- Kliknutím na klíč vedle názvu souboru spustíte debugger s cestovám v čase!
$ elm reactor

-- Zkoušejte si jednoduché příkazy v Read-Eval-Print Loop.
$ elm repl

-- Balíčky jsou určeny uživatelským jménem na GitHubu a názvem repozitáře.
-- Nainstalujte nový balíček a uložte jej v souboru elm-package.json.
$ elm package install evancz/elm-lang/html

-- Porovnejte změny mezi verzemi jednoho balíčku.
$ elm package diff elm-lang/html 1.1.0 2.0.0
-- Správce balíčků v Elmu vyžaduje sémantické verzování,
-- takže minor verze nikdy nerozbije váš build.
```

Jazyk Elm je překvapivě malý. Nyní se můžete podívat do skoro jakéhokoli zdrojového kódu
v Elmu a budete mít zběžnou představu o jeho fungování.
Ovšem možnosti, jak psát kód, který je odolný vůči chybám a snadno se refaktoruje, jsou neomezené!

Zde jsou některé užitečné zdroje (v angličtině).

* [Hlavní stránka Elmu](http://elm-lang.org/). Obsahuje:
  * Odkazy na [instalátory](http://elm-lang.org/install)
  * [Documentaci](http://elm-lang.org/docs), včetně [popisu syntaxe](http://elm-lang.org/docs/syntax)
  * Spoustu nápomocných [příkladů](http://elm-lang.org/examples)

* Documentace pro [standardní knihovny Elmu](http://package.elm-lang.org/packages/elm-lang/core/latest/). Povšimněte si:
  * [Základy](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics), které jsou automaticky importovány
  * Typ [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) a jeho bratranec typ [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result), které se běžně používají pro chybějící hodnoty a ošetření chyb.
  * Datové struktury jako [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List), [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array), [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) a [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set)
  * JSON [enkódování](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode) a [dekódování](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode)

* [Architektura Elmu](https://github.com/evancz/elm-architecture-tutorial#the-elm-architecture). Esej od tvůrce Elmu s příklady, jak organizovat kód do komponent.

* [Elm mailing list](https://groups.google.com/forum/#!forum/elm-discuss). Všichni jsou přátelští a nápomocní.

* [Scope v Elmu](https://github.com/elm-guides/elm-for-js/blob/master/Scope.md#scope-in-elm) a [Jak číst typové anotace](https://github.com/elm-guides/elm-for-js/blob/master/How%20to%20Read%20a%20Type%20Annotation.md#how-to-read-a-type-annotation). Další komunitní návody o základech Elmu, psáno pro JavaScriptové vývojáře.

Běžte si zkusit něco napsat v Elmu!
