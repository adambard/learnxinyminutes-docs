---
language: yaml
filename: learnyaml-hu.yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Tamás Diószegi", "https://github.com/ditam"]
lang: hu-hu
---

A YAML egy adat sorosító nyelv, amit úgy terveztek, hogy közvetlenül is 
olvasható és írható legyen emberi szemmel.

A JSON formátum egy szigorú befoglaló halmazát alkotja, kiegészítve azt
szintaktikai jelentéssel bíró sortörésekkel és indentációval,
a Pythonhoz hasonlóan. A Pythonnal ellentétben azonban a YAML nem engedélyezi
a közvetlen tab karakterek jelenlétét.

Megjegyzés: UTF-8 ékezetes betűk használhatóak, ha a fájl kódlása megfelelő,
a kódolást a tartalomban explicit nem kell (és nem is lehet) feltüntetni.

```yaml
# A kommentek YAML-ban így néznek ki.

##################
# Skalár típusok #
##################

# A gyökér objektumunk (az egész dokumentumra értve) egy map,
# ami a más nyelvekből ismert dictionary, hash vagy object típusokkal egyenértékű.
kulcs: érték
masik_kulcs: Másik érték jön ide.
egy_szam: 100
tudomanyos_jelolessel: 1e+12
boolean: true
null_value: null
kulcs benne szóközökkel: érték
# Látható, hogy a sztringeket nem szükséges idézőjelek közé zárni, bár szabad.
Továbbá: "Idézőjelekkel megadott sztring."
"A kulcs is lehet idézőjeles.": "Hasznos lehet, ha ':'-ot akarsz a kulcsban."

# Többsoros sztringek írhatóak 'literal block'-ként ('|' jelet használva)
# vagy 'folded block'-ként is ('>' jelet használva).
literal_block: |
    Ez az egész szöveg-blokk lesz az értéke a literal_block kulcsnak,
    a sortöréseket megtartva.

    Az ilyen sztringet az indentáció visszahúzása zárja le, a behúzás pedig
    eltávolításra kerül.

        A 'még jobban' behúzott részek megtartják a behúzásukat - 
        ezeknek a soroknak 4 szóköz behúzása lesz.
folded_style: >
    Az az egész szöveg-blokk lesz az értéke a 'folded_style' kulcsnak, de
    ezúttal minden sortörés egy szóközre lesz cserélve.

    Az üres sorok, mint a fenti, új sor karakterre cserélődnek.

        A 'még jobban' behúzott sorok megtartják a sortöréseiket, -
        ez a szöveg két sorban jelenik meg.

######################
# Gyűjtemény típusok #
######################

# Egymásba ágyazás a behúzás változtatásával érhető el.
beagyazott_map:
    key: value
    another_key: Another Value
    masik_beagyazott_map:
        hello: hello

# A mapeknek nem csak sztring kulcsaik lehetnek.
0.25: lebegőpontos kulcs

# A kulcsok lehetnek többsoros objektumok is, ? jellel jelezve a kulcs kezdetét
? |
    Ez itt egy
    többsoros kulcs
: és ez az értéke

# Szintén engedélyezett a kollekció típusok használata kulcsként, de egyéb
# nyelvekben ez gyakran problémákat fog okozni.

# Szekvenciák (listákkal vagy tömbökkel egyenértékűek) így néznek ki:
egy_szekvencia:
    - Item 1
    - Item 2
    - 0.5 # Többféle típust is tartalmazhat
    - Item 4
    - key: value
      another_key: another_value
    -
        - Ez egy szekvencia
        - egy másik szekvenciába ágyazva

# Mivel a YAML a JSON befoglaló halmazát alkotja, JSON szintaxisú
# mapek és szekvenciák is használhatóak:
json_map: {"key": "value"}
json_seq: [3, 2, 1, "takeoff"]

#########################
# EXTRA YAML KÉPESSÉGEK #
#########################

# A YAML-ben ún. 'anchor'-ök segítségével könnyen lehet duplikálni
# tartalmakat a dokumentumon belül. A következő kulcsok azonos értékkel bírnak:
anchored_tartalom: &anchor_neve Ez a sztring két kulcs értéke is lesz.
másik_anchor: *anchor_neve

# Vannak a YAML-ben tagek is, amivel explicit lehet típusokat jelölni.
explicit_string: !!str 0.5
# Bizonyos implementációk nyelv-specifikus tageket tartalmaznak, mint
# például ez a Python komplex szám típusának jelölésére:
python_complex_number: !!python/complex 1+2j

######################
# EXTRA YAML TÍPUSOK #
######################

# Nem a sztringek és a számok az egyedüli skalár típusok YAML-ben.
# ISO-formátumú dátumok és dátumot jelölő literal kifejezések is értelmezettek.
datetime: 2001-12-15T02:59:43.1Z
datetime_with_spaces: 2001-12-14 21:59:43.10 -5
date: 2002-12-14

# A !!binary tag jelöli, hogy egy sztring valójában base64-kódolású
# reprezentációja egy bináris blob-nak
gif_file: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# Létezik a YAML-ban egy halmaz típus (set) is, ami így néz ki:
set:
    ? elem1
    ? elem2
    ? elem3

# Mint Pythonban, a halmazok null értékekkel feltöltött mapek, vagyis a fenti
# halmaz egyenértékű a következővel:
set2:
    elem1: null
    elem2: null
    elem3: null
```