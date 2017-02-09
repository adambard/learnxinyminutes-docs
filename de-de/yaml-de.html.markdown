---
language: yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Ruben M.", "https://github.com/switchhax"]
filename: learnyaml-de.yaml
lang: de-de
---

YAML ist eine Sprache zur Datenserialisierung, die sofort von Menschenhand geschrieben und gelesen werden kann.

YAML ist ein Superset von JSON mit der Erweiterung um syntaktisch wichtige Zeilenumbrüche und Einrückungen, ähnlich wie auch in Python. Anders als in Python allerdings erlaubt YAML keine Tabulator-Zeichen.

```yaml
# Kommentare in YAML schauen so aus.

#################
# SKALARE TYPEN #
#################

# Unser Kernobjekt (für das ganze Dokument) wird das Assoziative Datenfeld (Map) sein,
# welches equivalent zu einem Hash oder einem Objekt einer anderen Sprache ist.
Schlüssel: Wert
nochn_Schlüssel: Hier kommt noch ein Wert hin.
eine_Zahl: 100
wissenschaftliche_Notation: 1e+12
boolean: true
null_Wert: null
Schlüssel mit Leerzeichen: value
# Strings müssen nicht immer mit Anführungszeichen umgeben sein, können aber:
jedoch: "Ein String in Anführungzeichen"
"Ein Schlüssel in Anführungszeichen": "Nützlich, wenn du einen Doppelpunkt im Schlüssel haben willst."

# Mehrzeilige Strings schreibst du am besten als 'literal block' (| gefolgt vom Text)
# oder ein 'folded block' (> gefolgt vom text).
literal_block: |
    Dieser ganze Block an Text ist der Wert vom Schlüssel literal_block,
    mit Erhaltung der Zeilenumbrüche.

    Das Literal fährt solange fort bis dieses unverbeult ist und die vorherschende Einrückung wird 
    gekürzt.

        Zeilen, die weiter eingerückt sind, behalten den Rest ihrer Einrückung -
        diese Zeilen sind mit 4 Leerzeichen eingerückt.
folded_style: >
    Dieser ganze Block an Text ist der Wert vom Schlüssel folded_style, aber diesmal
    werden alle Zeilenumbrüche durch ein Leerzeichen ersetzt.

    Freie Zeilen, wie obendrüber, werden in einen Zeilenumbruch verwandelt.

        Weiter eingerückte Zeilen behalten ihre Zeilenumbrüche -
        diese Textpassage wird auf zwei Zeilen sichtbar sein.

####################
# COLLECTION TYPEN #
####################

# Verschachtelung wird duch Einrückung erzielt.
eine_verschachtelte_map:
    schlüssel: wert
    nochn_Schlüssel: Noch ein Wert.
    noch_eine_verschachtelte_map:
        hallo: hallo

# Schlüssel müssen nicht immer String sein.
0.25: ein Float-Wert als Schlüssel

# Schlüssel können auch mehrzeilig sein, ? symbolisiert den Anfang des Schlüssels
? |
    Dies ist ein Schlüssel,
    der mehrzeilig ist.
: und dies ist sein Wert

# YAML erlaubt auch Collections als Schlüssel, doch viele Programmiersprachen
# werden sich beklagen.

# Folgen (equivalent zu Listen oder Arrays) schauen so aus:
eine_Folge:
    - Artikel 1
    - Artikel 2
    - 0.5 # Folgen können verschiedene Typen enthalten.
    - Artikel 4
    - schlüssel: wert
      nochn_schlüssel: nochn_wert
    -
        - Dies ist eine Folge
        - innerhalb einer Folge

# Weil YAML eine Erweiterung von JSON ist, können JSON-ähnliche Maps und Folgen
# geschrieben werden:
json_map: {"schlüssel": "wert"}
json_seq: [3, 2, 1, "Start"]

############################
# EXTRA YAML EIGENSCHAFTEN #
############################

# YAML stellt zusätzlich Verankerung zu Verfügung, welche es einfach machen 
# Inhalte im Dokument zu vervielfältigen. Beide Schlüssel werden den selben Wert haben.
verankerter_inhalt: &anker_name Dieser String wird als Wert beider Schlüssel erscheinen.
anderer_anker: *anker_name

# YAML hat auch Tags, mit denen man explizit Typangaben angibt.
explicit_string: !!str 0.5
# Manche Parser implementieren sprachspezifische Tags wie dieser hier für Pythons
# komplexe Zahlen.
python_komplexe_Zahlen: !!python/komplex 1+2j

####################
# EXTRA YAML TYPEN #
####################

# Strings and Zahlen sind nicht die einzigen Skalare, welche YAML versteht.
# ISO-formatierte Datumsangaben and Zeiangaben können ebenso geparsed werden.
DatumZeit: 2001-12-15T02:59:43.1Z
DatumZeit_mit_Leerzeichen: 2001-12-14 21:59:43.10 -5
Datum: 2002-12-14

# Der !!binary Tag zeigt das ein String base64 verschlüsselt ist.
# Representation des Binären Haufens
gif_datei: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML bietet auch Mengen (Sets), welche so ausschauen
menge:
    ? artikel1
    ? artikel2
    ? artikel3

# Wie in Python sind Mengen nicht anderes als Maps nur mit null als Wert; das Beispiel oben drüber ist equivalent zu:
menge:
    artikel1: null
    artikel2: null
    artikel3: null
```
