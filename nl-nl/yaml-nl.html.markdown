---
language: yaml
filename: learnyaml-nl.yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Niels van Velzen", "https://nielsvanvelzen.me"]
  - ["Sam van Kampen", "http://tehsvk.net"]
lang: nl-nl
---

YAML is een dataserialisatietaal ontworpen om snel te kunnen worden begrepen door mensen.

Het is een strikte superset van JSON en bevat nieuwe regels en een strikte manier van inspringen die lijkt op de manier van Python. In tegenstelling tot Python kan je alleen geen tabtekens gebruiken.

```yaml
# Commentaar in YAML ziet er zo uit

##################
# SCALAIRE TYPES #
##################

# Ons hoofdobject (Wat in het hele document gebruikt wordt) is een map;
# dit staat gelijk aan een dictionary, hash of object in andere talen.
sleutel: waarde
nog_een_sleutel: Een andere waarde
nummer_waarde: 100
wetenschappelijke_waarde: 1e+12
boolean_waarde: true
null_waarde: null
sleutel met spaties: waarde
# Merk op dat strings niet verplicht in quotes moeten, maar dit kan wel.
quote_waarde: "Een string in quotes"
"Ook sleutels kunnen in quotes": "Dit is bijvoorbeeld handig als je een dubbelepunt wilt gebruiken in je key"

# Tekst over meerdere lijnen kan je schrijven als een 'letterlijk blok' (met |)
# Of een 'gevouwen blok' (met >)
letterlijk_blok: |
    Dit hele blok met tekst is de waarde van de 'letterlijk_blok'-sleutel,
    met nieuwe lijnen behouden.

    Het blok blijft door gaan tot het geeindigd wordt door korter in te springen.

        Lijnen die groter zijn ingesprongen behouden dit.
gevouwen_stijl: >
    Dit blok met tekst zal de waarde zijn van 'gevouwen_stijl',
    maar deze keer zullen alle nieuwe lijnen worden vervangen met een spatie.

    Lege lijnen, zoals hierboven, zullen worden vertaald naar een nieuwe lijn.

        Meer ingesprongen lijnen zullen hun nieuwe lijnen ook behouden,
        deze tekst zal over 2 lijnen te zien zijn.

##################
# COLLECTIETYPES #
##################

# Nesten wordt bereikt met inspringen.
geneste_map:
    sleutel: waarde
    andere_sleutel: andere waarde
    andere_geneste_map:
        hallo: wereld

# In een map hoeft de sleutel geen string te zijn.
0.25: een float als sleutel

# Sleutels kunnen ook meerdere lijnen gebruiken met behulp van het vraagteken
? |
    Dit is een sleutel
    met meerdere lijnen
: en dit is de waarde

# YAML staat ook collectietypes toe in sleutels, maar veel programmeertalen
# zullen hierover klagen.

# Sequences (gelijk aan lijsten of arrays) zien er zo uit:
een_sequence:
    - Item 1
    - Item 2
    - 0.5 # sequences kunnen meerdere typen waardes bevatten.
    - Item 4
    - sleutel: waarde
      andere_sleutel: andere waarde
    -
        - Dit is een sequence
        - in een andere sequence

# Doordat YAML een superset van JSON is kan je ook mappen en
# sequences volgens de JSON-stijl maken:
json_map: {"sleutel": "waarde"}
json_seq: [3, 2, 1, "takeoff"]

#######################
# EXTRA YAML-FUNCTIES #
#######################

# YAML heeft ook een handige functie genaamd 'anchors' (ankers), deze laten je
# makkelijk de waarde van ergens anders in je document kopieëren. Beide sleutels
# krijgen dezelfde waarde:
geankert_content: &anker_naam Deze string zal verschijnen als waarde voor de twee sleutels
andere_anker: *anker_naam

# YAML heeft ook tags, deze gebruik je om een expliciet type te verklaren
expliciete_string: !!str 0.5
# Sommige parsers gebruiken taalspecifieke tags, zoals deze voor Python's
# complexe nummertype:
python_complex_nummer: !!python/complex 1+2j

#######################
# EXTRA TYPES IN YAML #
#######################

# Strings en nummers zijn niet de enige types die YAML begrijpt.
# ISO opgemaakte datum- en datumtijdnotaties werken ook:
datumtijd: 2001-12-15T02:59:43.1Z
datumtijd_met_spaties: 2001-12-14 21:59:43.10 -5
datum: 2002-12-14

# De !!binary tag geeft aan dat de string een base64-gecodeerde
# binary blob is.
gif_bestand: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML heeft ook een settype, dat ziet er zo uit:
set:
    ? item1
    ? item2
    ? item3

# Zoals in Python zijn sets gewoon mappen met nulwaarden;
# bovenstaand is gelijk aan:
set2:
    item1: null
    item2: null
    item3: null
```
