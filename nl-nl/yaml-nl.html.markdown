---
language: yaml
filename: learnyaml.yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Jeroen Deviaene", "https://github.com/Jerodev"]
---

YAML is een data serializatie taal ontworpen om gemakkelijk te schrijven en
leesbaar te zijn.

yaml is gebaseerd op JSON, met enkele toevoegingen en een syntax met spaties
en nieuwe lijnen, zoals Python. Hou er echter rekening mee dat YAML geen
tabulaties toe staat.


```yaml
# commentaar in yaml wordt op deze manier gedaan

################
# SCALAR TYPES #
################

# Het root element (dat door gaat voor het volledige document) is een map,
# dit is gelijk aan een dictionary, hash of object in andere talen.
sleuten: waarde
nog_een_sleutel: Hier is nog een waarde.
een_nummer: 100
wetenschappelijke_notatie: 1e+12
boolean: true
null_waarde: null
sleutel met spaties: value
# Merk op dat strings geen aanhalingstekens nodig hebben. Het is echter wel toegestaan
merk_op: "Een string in aanhalingstekens."
"Sleutels kunnen ook tussen aanhalingstekens.": "Handig als je een ':' wil in je sleutel"

# Strings uit meerdere lijnen kunnen geschreven worden als 'literal block' (met een '|'),
# of 'folded block' (met een '>').
literal_block: |
    Deze volledige blok tekst zal de waarde zijn van de sleutel 'literal block'
    met de nieuwe lijnen behouden

    Dit blok gaat door tot een nieuwe sleutel gemaakt wordt of het einde van het 
    document. spaties aan het begin van een lijn worden verwijderd.

        Lijnen met meer spaties aan het begin behouden deze extra spaties.
folded_style: >
    Deze volledige blok tekst zal de waarde zijn van de sleutel 'folded_style'
    maar deze keer worden alle nieuwe lijnen vervangen door een enkele spatie.

    Lege lijnen, zoals hierboven worden vervangen door een newline karakter.

####################
# COLLECTIE TYPES #
####################

# Sleutels kunnen genest worden met extra spaties
genest:
    sleutel: waarde
    tweede_sleutel: Nog een waarde
    nog_eens_nesten:
        hallo: hoi

# Sleutels moeten niet altijd strings zijn
0.25: een numerieke sleutel

# Sleutels kunnen ook uit meerder lijnen bestaan, hiervoor gebruik je ? om 
# het begin van een sleutel aan te geven.
? |
    Dit is een sleutel
    uit meerdere lijnen
: en dit is de waarde

# YAML staat ook collecties toe in sleutels, maar verschillende programmeer
# talen kunnen hier niet mee overweg.

# Sequenties (equivalent van lijsten en arrays) worden opgemaakt op deze manier:
een_sequentie:
    - Item 1
    - Item 2
    - 0.5 # sequenties kunnen verschillende types bevatten.
    - Item 4
    - sleutel: waarde
      nogeensleutel: nogeenwaarde
    -
        - Dit is een sequentie
        - In een andere sequentie

# Sinds YAML gebaseerd is op JSON kunnen json maps ook gebruikt worden in YAML
json_map: {"sleutel": "waarde"}
json_seq: [3, 2, 1, "takeoff"]

#######################
# EXTRA YAML FUNCTIES #
#######################

# YAML heeft ook een handige functie genaamd 'ankers', die je gemakkelijk waardes
# laat dupliceren doorheen het document. beide sleutels zullen de zelfde waarde
# hebben.
anker_waarde: &anker_naam Deze string wordt de waarde van beide sleutels
ander_anker: *anker_naam

# YAML heeft ook tags, deze worden gebruikt om types te defigneren.
explicite_string: !!str 0.5
# Somige parsers gebruiken tags voor bepaalde talen, zoals deze van Python
# complex nummer type.
python_complex_nummer: !!python/complex 1+2j

####################
# EXTRA YAML TYPES #
####################

# Strings en nummers zijn niet de enige types in YAML.
# datums en tijden in ISO-formaten kunnen ook geparsed worden.
datetime: 2001-12-15T02:59:43.1Z
datetime_met_spaties: 2001-12-14 21:59:43.10 -5
datum: 2002-12-14

# De !!binary tag heeft aan dat de string eigenlijk een base64 geëncodeerde
# binaire blob is.
gif_afbeelding: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML heeft ook een set type, dit ziet er als volgt uit:
set:
    ? item1
    ? item2
    ? item3

# Net zoals in Python zijn sets eigenlijk maps met null waardes
# Het bovenstaande is gelijk aan dit:
set2:
    item1: null
    item2: null
    item3: null
```
