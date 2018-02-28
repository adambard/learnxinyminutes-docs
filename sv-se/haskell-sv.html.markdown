---
language: Haskell
filename: learnhaskell-sv.hs
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Edward Tjörnhammar", "http://edwtjo.me"]
lang: sv-se
---

Haskell skapades för att vara ett praktiskt, rent, funktionellt
programmeringsspråk. Det är känt för sin använding av monader och dess
härledande typsystem men anledningen till att jag ständigt återbesöker språket
är på grund av dess elegans. Haskell gör programmering till ett rent nöje.

```haskell
-- Radkommenterar börjar med två bindestreck.
{- Flerradskommentarer innesluts av vänster/höger måsvinge bindestreck
block på detta vis.
-}

----------------------------------------------------
-- 1. Fördefinierade datatyper och operatorer
----------------------------------------------------

-- Du har siffror
3 -- 3

-- Matte fungerar som förväntat
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Division är normalt inte heltalsdivision
35 / 4 -- 8.75

-- Heltalsdivision, här infix div
35 `div` 4 -- 8

-- Boolar (Sant och Falskt) är fördefinierade
True
False

-- Samt dess operationer
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- I ovanstående exempel är `not` en funktion vilken bara tar ett argument.
-- Haskell behöver inte paranteser för sina funktionsanrop... alla argument
-- ges mellanslagsseparerade direkt efter funktionen. Det övergripande mönstret
-- är:
-- func arg1 arg2 arg3...
-- Se sektionen om funktioner för information om hur du skriver dina egna.

-- Strängar och bokstäver
"Detta är en sträng"
'a' -- bokstav
'Du kan inte använda enkelfnutt för strängar.' -- fel!

-- Strängar kan konkateneras
"Hej " ++ "världen!" -- "Hej världen!"

-- En sträng är en lista av bokstäver
['H', 'e', 'j', 's', 'a', 'n'] -- "Hejsan"
"Detta är en sträng" !! 0 -- 'D'


----------------------------------------------------
-- 2. Listor och Tupler
----------------------------------------------------

-- Varje element i en lista måste ha samma typ.
-- Dessa listor är ekvivalenta:
[1, 2, 3, 4, 5]
[1..5]

-- Intervall är mångsidiga.
['A'..'F'] -- "ABCDEF"

-- Man kan stega intervall.
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- [] (Haskell förutsätter normalt inkrement)
[5,4..1] -- [5, 4, 3, 2, 1]

-- Indexering in i en lista
[1..10] !! 3 -- 4 (nollindexerat)

-- Man kan ha oändliga listor i Haskell!
[1..] -- listan över alla naturliga tal

-- Oändliga listor fungerar enbart för att Haskell har "lat evaluering".
-- Det betyder att Haskell bara evaluerar de uttryck den måste. Du kan alltså
-- fråga efter det 1000:e elementet i en oändlig lista och Haskell kommer då ge
-- dig det:

[1..] !! 999 -- 1000

-- Nu har Haskell evaluerat element 1 till 1000 i denna lista... men resten
-- av medlemmarna i denna oändliga lista existerar inte ännu! Haskell kommer
-- faktiskt inte utvärdera element den inte måste.

-- Sammanslagning av två listor
[1..5] ++ [6..10]

-- Lägg till 0 vid listhuvudet
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- fler listoperationer som huvud, svans, initiella samt sista
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- listomfattningar
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- med bivilkor
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Varje element i en tupel kan ha olika typ men en tupel kan bara ha en
-- fixerad, eller statisk, längd.
-- En tupel:
("haskell", 1)

-- För att komma åt element i ett par, alltså en 2-tupel, finns
-- de fördefinierade funktionerna:
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Funktioner
----------------------------------------------------
-- En enkel funktion med två parametrar
add a b = a + b

-- Notera även att om du använder ghci (Haskellinterpretatorn) kommer du behöva
-- använda `let` namnbindning för att synliggöra din funktionsdeklaration,
-- alltså
let add a b = a + b

-- För att använda funktionen
add 1 2 -- 3

-- Man kan även göra funktionsanropet infix, alltså mellan parametersättningen,
-- med hjälp av bakåtfnuttar:
1 `add` 2 -- 3

-- Du kan även definiera funktioner vars funktionsnamn avsaknar bokstäver!
-- Med hjälp av parenteser kan du därmed definiera operatorer (normalt infix)!
-- Följande är en operator för heltalsdivision, vilken förlitar sig på div:
(//) a b = a `div` b
35 // 4 -- 8

-- Funktionsvakter: ett enkelt sätt att grena ut dina funktioner
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- Mönstermatchning fungerar på liknande vis. Här ger vi tre olika
-- parametermatchningar för vårat fib-resulat. Haskell kommer automatiskt följa
-- första bästa träff, uppifrån ned, vars vänstra sida om likhetstecknet matchar
-- anroparens parametervärde.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- Mönstermatchning på tupler:
foo (x, y) = (x + 1, y + 2)

-- Mönstermatchning på listor. Här är `x` det första elementet i listan och `xs`
-- är resten av listan. Nu kan vi skriva våran egen map-funktion
minMap func [] = []
minMap func (x:xs) = func x:(minMap func xs)

-- Anonyma funktioner, eller lambdauttryck, skapas med hjälp av omvänt
-- snedstreck, följt av parametrarna
minMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- Användning av fold (även kallad `inject`, `reduce`, osv.) tillsammans med en
-- anonym funktion. `fold1` är en vänstervikande funktion och använder första
-- värdet i listan som det initiella värdet för ackumulatorn.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Mer funktioner
----------------------------------------------------

-- Partiell applikation:
-- Om du inte anropar funktionen med alla sina argument
-- blir den partiellt applicerad. Det betyder att du erhåller en funktion där en
-- delmängd av parametrarna blivit värdesatta men några är fortfarande fria.
add a b = a + b
foo = add 10 -- foo är nu en funktion som tar ett nummer och lägger till 10 till
             -- det
foo 5 -- 15

-- Ett annat sätt att skriva samma sak
foo = (10+)
foo 5 -- 15

-- Funktionskomposition:
-- Operatorn `.` kedjar ihop funktioner
-- Till exempel, nedan är `foo` en funktion som tar ett värde, den adderar 10
-- till det, multiplicerar det resultatet med 4 och sen ersätts med det värdet.
foo = (4*) . (10+)

-- 4*(10+5) = 60
foo 5 -- 60

-- Precedensordning:
-- Haskell har en operator `$`. Denna operator applicerar en funktion till en
-- given parameter med dess precedens. I kontrast mot vanlig
-- funktionsapplikation, vilket har den högsta utvärderingsprioriteten 10 och
-- associerar till vänster, har denna prioritetsordning 0 och är
-- högerassociativ. Denna låga prioritet medför att parameteruttrycket till
-- höger om operatorn får det reducerat innan det appliceras till sin vänster.

-- före
even (fib 7) -- falskt

-- ekvivalent
even $ fib 7 -- falskt

-- med funktionskomposition
even . fib $ 7 -- falskt


----------------------------------------------------
-- 5. Typsignaturer
----------------------------------------------------

-- Haskell har ett väldigt starkt typsystem, alla giltiga uttryck har en typ.

-- Några grundläggande typer:
5 :: Integer
"hello" :: String
True :: Bool

-- Funktioner har också typer,
-- `not` tar en bool och returnerar en bool:
-- not :: Bool -> Bool

-- Här är ett exempel på en funktionssignatur vilken beskriver en funktion som
-- reducerar två heltal till ett:
-- add :: Integer -> Integer -> Integer

-- Trots att Haskell härleder typen på icke typsatta uttryck är det bra form att
-- explicit ange dessa för ens deklarerade funktioner:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Kontrollflöde och Ifsatser
----------------------------------------------------

-- if-sats
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if-statser kan spridas över rader men indentering har betydelse
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case uttryck: följande är ett exempel på kommandoradsparsning
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell har inte loopar istället används recursion.
-- map applicerar en funktion över varje element i en lista

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- man kan deklarera en for funktion genom att använda map
for array func = map func array

-- och därefter använda den tillsammans med en anonym funktion för att
-- efterlikna en loop
for [0..5] $ \i -> show i

-- men vi kunde även ha skrivit på följande vis:
for [0..5] show

-- Du kan använda foldl eller foldr för att reducera en lista
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- Vilket är samma sak som
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl viker från vänster, foldr från höger
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- Vilket alltså är samma sak som
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. Datatyper
----------------------------------------------------

-- Såhär definierar du din egen datatyp i Haskell
data Color = Red | Blue | Green

-- När du gjort det kan du använda den i funktionssignaturer och uttryck
say :: Color -> String
say Red   = "Du är Rö!"
say Blue  = "Du är Blå!"
say Green = "Du är Grön!"

-- Dina datatyper kan även ta parametrar
data Maybe a = Nothing | Just a

-- Följande uttryck är alla specialiseringar av typen Maybe
Just "hello"    -- har typen `Maybe String`
Just 1          -- har typen `Maybe Int`
Nothing         -- har typen `Maybe a` för alla `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- Även om IO inte kan förstås fullt ut utan att först förklara monader är det
-- inte svårt att lära sig tillräckligt för att komma igång

-- När ett Haskellprogram körs är det topnivåns main som körs. Main måste
-- returnerna ett värde av typen `IO a`, för någon typ `a`. Till exempel:

main :: IO ()
main = putStrLn $ "Hej, himmelen! " ++ (say Blue)
-- putStrLn har typen type String -> IO ()

-- Det är enkelt att göra IO om du kan implementera ditt program som en funktion
-- från String till String. Funktionen
--    interact :: (String -> String) -> IO ()
-- tar denna funktion och matar den med strängdata från stdin och skriver ut
-- resultatet som en sträng på stdout

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- Du kan tänka på värden av typen `IO ()` som att representera
-- händelsesekvenser du vill att din dator skall utföra, likt imperativa språk.
-- För att kedja ihop händelsesekvenser använder man ett syntaktiskt socker
-- kallat do-notation. Som exempel:

sägHej :: IO ()
sägHej = do
   putStrLn "Vad heter du?"
   namn <- getLine -- denna raden läser en rad från stdin och vi binder den till
                  -- funktionsnamnet `namn`
   putStrLn $ "Hejsan, " ++ namn

-- Övning: Skriv din egen version av interageringsfunktionen `interact` som bara
-- läser en rad från stdin, vanliga `interact` läser till EOF.

-- Koden i sägHej kommer dock aldrig exekveras. Den enda handlingen som blir det
-- är som bekant utvärderingen av `main`.
-- För att köra `sägHej` kommentera ut definition av `main` ovan och
-- avkommentera nedanstående version:
--   main = sayHello

-- Låt oss bättre förstå hur funktionen `getLine` vi just använde fungerar. Dess
-- typsignatur är:
--    getLine :: IO String
-- Du kan tänka på typen `IO a` som att representeras av ett datorprogram vilken
-- kommer generera ett värde av typen `a` när det exekveras (utöver allt annat
-- det kan tänkas göra). Vi kan därtill binda detta värde till ett namn för
-- återanvändning genom att använda `<-`. Vi kan även skapa våran egen handling
-- av typen `IO String`:

handling :: IO String
handling = do
   putStrLn "Detta är en rad, tihi"
   input1 <- getLine
   input2 <- getLine
   -- Typen av hela `do` blocket är vad som står på sista raden. Här är även
   -- `return` inte ett nyckelord i språket utan en funktion med en typsignatur
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- Vi kan använda `return` på samma sätt som vi använde `getLine`:

main'' = do
    putStrLn "Jag kommer eka två rader!"
    result <- handling
    putStrLn result
    putStrLn "Tack och hej leverpastej!"

-- Typen `IO` är ett exempel på en monad. Sättet Haskell utnyttjar monader på är
-- anledningen till hur språket kan bibehålla sin renhet. En funktion vilken
-- interagerar med omvärlden (alltså gör IO) blir markerad med `IO` i sin
-- typsignatur. Detta låter oss enkelt upptäcka vilka funktioner som är "rena"
-- (inte interagerar med omvärlden eller är tillståndsoberoende) and vilka
-- funktioner som inte är det.

-- Detta är ett mäktigt särdrag eftersom det är enkelt att köra rena funktioner
-- sammanlöpande; Samtidig programmering är enkel att göra i Haskell.

----------------------------------------------------
-- 9. Haskell REPL (kodtolk)
----------------------------------------------------

-- Efter installation av GHC kan vi starta tolken genom att skriva `ghci`.
-- Nu kan du mata in Haskellkod direkt i den. Nya värden måste introduceras med
-- `let` bindning:

let foo = 5

-- Du kan även se typen av namnbindningen med `:t`

> :t foo
foo :: Integer

-- Operatorer, som `+`, `:` och `$` är funktioner. Deras typ kan inspekteras
-- genom att skriva operatorn mellan parenteser:

> :t (:)
(:) :: a -> [a] -> [a]

-- Du kan få ytterliggare information om något namn genom att använda `:i`

> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
    -- Defined in ‘GHC.Num’
infixl 6 +

-- Du kan även köra alla handlingar av typen `IO ()` direkt i tolken

> sägHej
Vad är ditt namn?
Kompis!
Hello, Kompis!

```

Det finns mycket mer att upptäcka med Haskell, inklusive typklasser och monader.
Vilka är de stora idéerna som gör Haskell till det roliga programmeringsspråket
det är. Jag lämar dig med ett sista exempel; En implementation av quicksort:

```haskell
qsort [] = []
qsort (p:xs) = qsort mindre ++ [p] ++ qsort större
    where mindre  = filter (< p) xs
          större = filter (>= p) xs
```

Det finns två populära sätt att installera Haskell på: Den traditionella [Cabal sättet](http://www.haskell.org/platform/), eller det nyare [Stack sättet](https://www.stackage.org/install).

Du kan finna vänligare och/eller djupare introduktioner till Haskell på engelska
från:
[Learn you a Haskell](http://learnyouahaskell.com/),
[Happy Learn Haskell Tutorial](http://www.happylearnhaskelltutorial.com/) eller
[Real World Haskell](http://book.realworldhaskell.org/).
