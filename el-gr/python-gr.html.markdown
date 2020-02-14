---
language: Python
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Roberto Fernandez Diaz", "https://github.com/robertofd1995"]
filename: learnpython-gr.py
lang: el-gr
---

Η Python δημιουργήθηκε από τον Guido van Rossum στις αρχές των 90s. Πλέον είναι μία από τις πιο
δημοφιλείς γλώσσες. Ερωτευεται κανείς την python για τη συντακτική της απλότητα.
Βασικά είναι εκτελέσιμος ψευδοκώδικας.

Το Feedback είναι πάντα δεκτό! Μπορείτε να με βρείτε στο [@haritonaras](http://twitter.com/haritonaras)
ή τον αρχικό συγγραφέα στο [@louiedinh](http://twitter.com/louiedinh) ή στο
louiedinh [at] [google's email service]

Σημείωση: Το παρόν άρθρο ασχολείται μόνο με την Python 3. Δείτε [εδώ](http://learnxinyminutes.com/docs/pythonlegacy/) αν θέλετε να μάθετε την παλιά Python 2.7

```python

# Τα σχόλια μίας γραμμής ξεκινούν με #

""" Τα σχόλια πολλαπλών γραμμών μπορούν
    να γραφούν με τρία ", και συχνά χρησιμοποιούνται
    ως documentation.
"""

####################################################
## 1. Primitive (πρωταρχικοί) Τύποι Δεδομένων και Τελεστές
####################################################

# Αφού έχει αριθμούς
3  # => 3

# Λογικά θα έχει και Μαθηματικά...
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# Η διαίρεση ακεραίων κάνει στρογγυλοποίηση προς τα κάτω για θετικούς και αρνητικούς αριθμούς
5 // 3       # => 1
-5 // 3      # => -2
5.0 // 3.0   # => 1.0 # works on floats too
-5.0 // 3.0  # => -2.0

# Το αποτέλεσμα της διαίρεσης είναι πάντα float
10.0 / 3  # => 3.3333333333333335

# Modulo τελεστής
7 % 3  # => 1

# Ύψωση σε δύναμη (x**y, x στην y-οστή δύναμη)
2**3  # => 8

# Ελέγχουμε την προτεραιότητα πράξεων με παρενθέσεις
(1 + 3) * 2  # => 8

# Οι Boolean τιμές είναι primitives (Σημ.: τα κεφαλαία)
True
False

# άρνηση με το not
not True   # => False
not False  # => True

# Boolean τελεστές
# Σημ. ότι τα "and" και "or" είναι case-sensitive
True and False  # => False
False or True   # => True

# Τα True και False είναι 1 και 0 αλλά με διαφορετικά keywords
True + True # => 2
True * 8    # => 8
False - 5   # => -5

# Μπορούμε να δούμε τις αριθμητικές τιμές των True και False μέσω των τελεστών σύγκρισης
0 == False  # => True
1 == True   # => True
2 == True   # => False
-5 != False # => True

# Χρησιμοποιώντας τελεστές boolean σε ακεραίους, οι ακέραιοι γίνονται cast σε
# boolean ώστε να γίνει η αποτίμηση της έκφρασης.
# Το αποτέλεσμα όμως είναι non-cast, δηλαδή ίδιου τύπου με τα αρχικά ορίσματα
# Μην μπερδεύετε τις bool(ints) και bitwise and/or (&,|)
bool(0)     # => False
bool(4)     # => True
bool(-6)    # => True
0 and 2     # => 0
-5 or 0     # => -5

# Ισότητα ==
1 == 1  # => True
2 == 1  # => False

# Διάφορο !=
1 != 1  # => False
2 != 1  # => True

# Περισσότερες συγκρίσεις
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Κοιτάζουμε αν μία τιμή ανήκει σε ένα εύρος
1 < 2 and 2 < 3  # => True
2 < 3 and 3 < 2  # => False
# Το Chaining (αλυσίδωση? :P) κάνει το παραπάνω πιο όμορφα
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is vs. ==) το is ελέγχει αν δύο μεταβλητές αναφέρονται στο ίδιο αντικείμενο,
# αλλά το == ελέγχει αν τα αντικείμενα στα οποία αναφέρονται οι μεταβλητές έχουν τις ίδιες τιμές
a = [1, 2, 3, 4]  # το a δείχνει σε μία νέα λίστα, [1,2,3,4]
b = a             # το b δείχνει στο αντικείμενο που δείχνει το a
b is a            # => True, a και b αναφέρονται στο ίδιο αντικείμενο
b == a            # => True, τα αντικείμενα των a κι b είναι ίσα
b = [1, 2, 3, 4]  # Το b δείχνει σε μία νέα λίστα, [1, 2, 3, 4]
b is a            # => False, a και b δεν αναφέρονται στο ίδιο αντικείμενο
b == a            # => True, τα αντικείμενα των a και b είναι ίσα

# Τα Strings (συμβολοσειρές) δημιουργούνται με " ή '
"This is a string."
'This is also a string.'

# Μπορούμε και να προσθέτουμε Strings, αλλά προσπαθήστε να μην το κάνετε
"Hello " + "world!"  # => "Hello world!"
# Τα String literals (αλλά όχι οι μεταβλητές) μπορούν να συντμιθούν και χωρίς το '+'
"Hello " "world!"    # => "Hello world!"

# Μπορούμε να φερθούμε σε string σαν να είναι λίστα από χαρακτήρες
"This is a string"[0]  # => 'T'

# Μπορούμε να βρούμε το μήκος ενός string
len("This is a string")  # => 16

# Το .format μπορεί να χρησιμοποιηθεί για να μορφοποιήσουμε strings, όπως εδώ:
"{} can be {}".format("Strings", "interpolated")  # => "Strings can be interpolated"

# Μπορείς να επαναλάβεις τα ορίσματα του formatting για να γλιτώσεις λίγο χρονο
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
# => "Jack be nimble, Jack be quick, Jack jump over the candle stick"

# Μπορείς να χρησιμοποιήσεις keywords αν βαριέσαι το μέτρημα.
"{name} wants to eat {food}".format(name="Bob", food="lasagna")  # => "Bob wants to eat lasagna"

# Αν ο κώδικας Python 3 που γράφεις πρόκειται να τρέξει και με python 2.5 ή παλιότερη
# μπορείς επίσης να χρησιμοποιήσεις το παλιό τρόπο για formatting:
"%s can be %s the %s way" % ("Strings", "interpolated", "old")  # => "Strings can be interpolated the old way"

# Μπορείς επίσης να μορφοποιήσεις χρησιμοποιώντας τα f-strings / formatted string literals (σε Python 3.6+)
name = "Reiko"
f"She said her name is {name}." # => "She said her name is Reiko"
# Μπορείς βασικά να βάλεις οποιαδήποτε έκφραση Python στα άγκιστρα και θα εμφανιστεί στο string.
f"{name} is {len(name)} characters long."


# το None είναι ένα αντικείμενο (object)
None  # => None

# Μη χρησιμοποιείτε το σύμβολο ισότητας "==" για να συγκρίνετε αντικείμενα με το None
# Χρησιμοποιείτε το "is". Αυτό ελέγχει για ισότητα της ταυτότητας του αντικειμένου.
"etc" is None  # => False
None is None   # => True

# Τα None, 0, και τα κενά strings/lists/dicts/tuples αποτιμούνται στην τιμή False
# All other values are True
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False
bool(())  # => False

####################################################
## 2. Μεταβλητές (variables) και Συλλογές (collections)
####################################################

# Η Python έχει μία συνάρτηση print()
print("I'm Python. Nice to meet you!")  # => I'm Python. Nice to meet you!

# By default, η συνάρτηση print() τυπώνει και ένα χαρακτήρα αλλαγής γραμμμής στο τέλος
# Χρησιμοποιείτε το προαιρετικό όρισμο end για να τυπώνει οτιδήποτε άλλο
print("Hello, World", end="!")  # => Hello, World!

# Απλός τρόπος για να πάρουμε δεδομένα εισόδου από το console
input_string_var = input("Enter some data: ") # επιστρέφει τα δεδομένα ως string
# Σημ.: Στις προηγούμενες εκδόσεις της Python, η μέθοδος input() ονομαζόταν raw_input()

# Δεν υπάρχουν δηλώσεις, μόνο αναθέσεις τιμών.
# Η σύμβαση είναι να χρησιμοποιούμε μικρά γράμματα με κάτω παύλες
some_var = 5
some_var  # => 5

# Η πρόσβαση σε μεταβλητή που δεν έχει λάβει τιμή είναι εξαίρεση
# Δες τον Έλεγχο Ροής για να μάθεις περισσότερα για το χειρισμό εξαιρέσεων
some_unknown_var  # Προκαλέι ένα NameError

# Η παρακάτω έκφραση μπορεί να χρησιμποιηθεί ισοδύναμα με τον τελεστή '?' της C
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# Οι λίστες κρατούν ακολουθίς
li = []
# Μπορείς να αρχίσεις με μία προ-γεμισμένη λίστα
other_li = [4, 5, 6]

# Και να βάλεις πράγματα στο τέλος με την μέθοδο append
li.append(1)    # η li τώρα είναι [1]
li.append(2)    # η li τώρα είναι [1, 2]
li.append(4)    # η li τώρα είναι [1, 2, 4]
li.append(3)    # η li τώρα είναι [1, 2, 4, 3]
# Αφαιρούμε από το τέλος με την μέθοδο pop
li.pop()        # => 3 και η li γίνεται [1, 2, 4]
# Ας βάλουμε το 3 πίσω στη θέση του
li.append(3)    # η li γίνεται πάλι [1, 2, 4, 3].

# Προσπελαύνουμε τις λίστες όπως τους πίνακες σε άλλες γλώσσες
li[0]   # => 1
# Το τελευταίο στοιχείο...
li[-1]  # => 3

# Όταν βγαίνουμε εκτός ορίων της λίστας προκαλείται IndexError
li[4]  # προκαλεί IndexError

# Μπορείς να δεις ranges μιας λίστας με το slice syntax ':'
# Ο δείκτης εκίνησης περιλαμβάνεται στο διάστημα, ο δείκτης τερματισμού όχι
# (είναι ανοικτό/κλειστό διάστημα για τους φίλους των μαθηματικών)
li[1:3]   # => [2, 4]
# Αγνόησε την αρχή και επίστρεψε τη λίστα
li[2:]    # => [4, 3]
# Αγνόησε το τέλος και επίστρεψε τη λίστα
li[:3]    # => [1, 2, 4]
# Διάλεξε κάθε δεύτερο στοιχείο
li[::2]   # =>[1, 4]
# Επίστρεψε ένα reversed αντίγραφο της λίστας
li[::-1]  # => [3, 4, 2, 1]
# Χρησιμοποιείστε οποιαδήποτε συνδυασμό αυτών για να φτιάξετε πιο προχωρημένα slices
# li[start:end:step]

# Φτιάξε ένα αντίγραφο της λίστας χρησιμοποιώντας slices
li2 = li[:]  # => li2 = [1, 2, 4, 3] αλλά το (li2 is li) επιστρέφει False

# Αφαίρεσε οποιοδήποτε στοιχείο από λίστα με την εντολή "del"
del li[2]  # η li γίνεται [1, 2, 3]

#  Αφαιρούμε το πρώτο στιγμυότυπο μιας τιμής
li.remove(2)  # η li γίνεται [1, 3]
li.remove(2)  # Προκαλεί ένα ValueError καθώς το 2 δεν βρίσκεται στη λίστα.

# Εισαγωγή ενός στοιχείου σε συγκεκριμένη θέση
li.insert(1, 2)  # η li γίνεται πάλι [1, 2, 3]

# Βρες το index (δείκτη) του πρώτου στοιχείου με τιμή ίση με το όρισμα
li.index(2)  # => 1
li.index(4)  # Προκαλεί ValueError καθώς το 4 δεν βρίσκεται στη λίστα

# Μπορείς να προσθέτεις λίστες
# Σημ.: οι τιμές των li, other_li δεν αλλάζουν.
li + other_li  # => [1, 2, 3, 4, 5, 6]

# Σύντμιση λιστών με τη μέθοδο "extend()"
li.extend(other_li)  # Τώρα η  li είναι [1, 2, 3, 4, 5, 6]

# Ελεγχος της ύπαρξης στοιχείου σε λίστα με το "in"
1 in li  # => True

# Εξατάζουμε το μήκος με "len()"
len(li)  # => 6


# Τα Tuples είναι σαν τις λίστες αλλά είναι αμετάβλητα (immutable).
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # Προκαλεί TypeError

# Σημειώστε ότι ένα tuple μήκους 1 πρέπει να έχει ένα κόμμα μετά το τελευταίο στοιχείο
# αλλά τα tuples άλλων μηκών, ακόμα και μηδενικού μήκους, δεν χρειάζονται κόμμα.
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# Μπορείς να εφαρμόσεις τις περισσότερες μεθόδους των λιστών και στα tuples
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# Μπορείς να κάνεις unpack/"ξεπακετάρεις" tuples σε μεταβλητές
a, b, c = (1, 2, 3)  # a == 1, b == 2 και c == 3
#  Μπορείς επίσης να επεκτείνεις το unpacking
a, *b, c = (1, 2, 3, 4)  # a == 1, b == [2, 3] και c == 4
# Τα Tuples δημιουργούνται by deafult αν δεν βάλεις παρενθέσεις
d, e, f = 4, 5, 6  # το tuple 4, 5, 6 "ξεπακετάρεται" στις μεταβλητές d, e και f
# αντίστοιχα έτσι ώστε να γίνεται d = 4, e = 5 and f = 6
# Δείτε πόσο εύκολα μπορούμε να εναλλάσουμε δύο τιμές
e, d = d, e  # το d παίρνει την τιμή 5 και το e παίρνει την τιμή 4


# Τα λεξικά (Dictionaries) αποθηκεύουν απεικονίσεις από κλειδιά σε τιμές
empty_dict = {}
# Εδώ έχουμε ένα προ-γεμισμένο dictionary
filled_dict = {"one": 1, "two": 2, "three": 3}

# Σημ. ότι τα κλειδιά για τα dictionaries πρέπει να είναι αμετάβλητοι τύποι
# (immutable) αυτό γίνετια για να διασφαλίσουμε ότι τα κλειδιά μπορούν να
# μετατρέπονται σε σταθερές τιμές κατακερματισμού (hash values) για γρήγορη εύρεση.
# Μερικοί αμετάβλητοι τύποι είναι τα ints, floats, strings, tuples.
invalid_dict = {[1,2,3]: "123"}  # => Προκαλεί TypeError: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # Οι τιμές όμως μπορούν να έχουν οποιοδήποτε τύπο.

# Βρίσκουμε τιμές με []
filled_dict["one"]  # => 1

# Μπορείς να πάρεις όλα τα κλειδιά με τη μέθοδο "keys()".
# Πρέπει να "τυλίξουμε" την κλήση με list() για να το μετατρέψουμε σε λίστα
# Θα μιλήσουμε για αυτά αργότερα. Σημ. - σε εκδόσεις Python < 3.7, η σειρά που
# εμφανίζονται τα κλειδιά δεν είναι εγγυημένη. Τα αποτελέσματά σας ίσως να μην
# είναι ακριβώς ίδια με τα παρακάτω. Στην έκδοση 3.7 πάντως, τα αντικείμενα του
# λεξικού διατηρούν τη σειρά με την οποία εισήχθησαν στο dictionary
list(filled_dict.keys())  # => ["three", "two", "one"] σε Python <3.7
list(filled_dict.keys())  # => ["one", "two", "three"] σε Python 3.7+

# Παίρνουμε όλες τις τιμές ενός iterable με τη μέθοδο "values()". Και πάλι
# χρειάζεται να το περιτυλίξουμε σε list()
# Σημ. -  όπως παραπάνω σχετικά με τη σειρά των keys
list(filled_dict.values())  # => [3, 2, 1]  in Python <3.7
list(filled_dict.values())  # => [1, 2, 3] in Python 3.7+

# Έλεγχος της ύπαρξης κλειδιών σε ένα dictionary με το "in"
"one" in filled_dict  # => True
1 in filled_dict      # => False

# Αν ψάξεις την τιμή ανύπαρκτου κλειδιού προκαλείται KeyError
filled_dict["four"]  # KeyError

# Χρησιμοποιούμε τη μέθοδο "get()" για να αποφύγουμε το KeyError
filled_dict.get("one")      # => 1
filled_dict.get("four")     # => None
# στο δεύτερο argument της get() μπορούμε να βάλουμε μία τιμή που πρέπει να
# επιστρέψει αν δεν υπάρχει το key που ψάχνουμε
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)  # => 4

# το "setdefault()" εισάγει στο dictionary μόνο αν δεν υπάρχει το κλειδί
filled_dict.setdefault("five", 5)  # filled_dict["five"] γίνεται 5
filled_dict.setdefault("five", 6)  # filled_dict["five"] μένει 5 (υπαρκτό κλειδί)

# Προσθήκη σε dictionary
filled_dict.update({"four":4})  # => {"one": 1, "two": 2, "three": 3, "four": 4}
filled_dict["four"] = 4         # β' τρόπος

# Αφαίρεση κλειδιών από dictionary με del
del filled_dict["one"]  # Αφαιρεί το κλειδί "one" από το filled_dict

# Από την Python 3.5 μπορείς να χρησιμοποιήσεις και πρόσθετες επιλογές για unpacking
{'a': 1, **{'b': 2}}  # => {'a': 1, 'b': 2}
{'a': 1, **{'a': 2}}  # => {'a': 2}



# τα Sets -όπως όλοι περιμένουμε- αποθηκεύουν σύνολα
empty_set = set()
# Αρχικοποιούμε ένα set με μερικές τιμές. Ναι, μοιάζει λίγο με dictionary, Sorry.
some_set = {1, 1, 2, 2, 3, 4}  # some_set is now {1, 2, 3, 4}

# Παρομοίως με τα κλειδιά του dictionary, τα στοιχεία ενός συνόλου πρέπει να είναι
# αμετάβλητα (immutable)
invalid_set = {[1], 1}  # => Προκαλεί TypeError: unhashable type: 'list'
valid_set = {(1,), 1}

# Προσθέτουμε άλλο ένα στοιχείο στο σύνολο
filled_set = some_set
filled_set.add(5)  # το filled_set είναι τώρα {1, 2, 3, 4, 5}
# Τα σύνολα δεν έχουν διπλοτυπα αντικείμενα
filled_set.add(5)  # το σύνολο παραμένει ίδιο {1, 2, 3, 4, 5}

# το & κάνει την τομή δύο συνόλων.
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# και το | την ένωση
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# Η διαφορά συνόλων με το -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Το ^ επιστρέφει τη συμμετρική διαφορά
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Ελεγχος για το αν το δεξιά σύνολο είναι υπερσύνολο του δεξιού
{1, 2} >= {1, 2, 3} # => False

# Ελεγχος για το αν το δεξιά σύνολο είναι υποσύνολο του δεξιού
{1, 2} <= {1, 2, 3} # => True

# με το in κάνουμε έλεγχο ύπαρξης στοιχείο σε σετ
2 in filled_set   # => True
10 in filled_set  # => False



####################################################
## 3. Έλεγχος Ροής και Iterables
####################################################

# Φτιάχνουμε μία μεταβλητή
some_var = 5

# Εδώ έχουμε ένα if statement. Η στοίχιση είναι σημαντική στην Python!
# Η σύμβαση είναι να χρησιμοποιούμε 4 κενά, όχι tabs.
# Το παρακάτω τυπώνει "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # το (else if) -> elif μέρος είναι προαιρετικό.
    print("some_var is smaller than 10.")
else:                  # και το else είναι προαιρετικό.
    print("some_var is indeed 10.")


"""
τα for loops τρέχουν πάνω σε lists
το παρακάτω τυπώνει:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # You can use format() to interpolate formatted strings
    print("{} is a mammal".format(animal))

"""
το "range(number)" επιστρέφει ένα iterable με αριθμούς
από το μηδέν μέχρι τον δωσμένο αριθμό number (κλειστό/ανοικτό διάστημα)
Το παρακάτω τυπώνει:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
το "range(lower, upper)" επιστρέφει ένα iterable με αριθμούς
από το lower εώς το upper (κλειστό/ανοικτό διάστημα)
το παρακάτω τυπώνει:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
το "range(lower, upper, step)" επιστρέφει ένα iterable με αριθμούς
από το lower μέχρι το upper, με βήμα step
αν δεν δώσουμε τιμή βήματος, το default βήμα είναι 1.
το παρακάτω τυπώνει:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)
"""

τα While loops τρέχουν μέχρι μία συνθήκη να γίνει ψευδής.
το παρακάτω τυπώνει:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Shorthand for x = x + 1

# Χειριζόμαστε εξαιρέσεις με ένα try/except block
try:
    # Χρησιμοποιούμε το "raise" για να πετάξουμε ένα error
    raise IndexError("This is an index error")
except IndexError as e:
    pass                 # το Pass δεν κάνει τίποτα. Συνήθως κάνουμε ανάκτηση.
except (TypeError, NameError):
    pass                 # Μπορούμε να χειριζόμαστε πολλές εξαιρέσεις μαζί, αν χρειαστεί
else:                    # Προαιρετικό στο try/except block. Πρέπει να ακολουθεί όλα τα except blocks
    print("All good!")   # τρέχει μόνο αν ο κώδικας στο try δεν προκαλεί εξαιρέσεις
finally:                 #  Εκτελείται ό,τι και να γίνει
    print("We can clean up resources here")

# Αντί για try/finally για να καθαρίσουμε τους πόρους, μπορούμε να χρησιμοποιούμε το
# with expression as target:
    pass to cleanup resources you can use a with statement
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Η Python προσφέρει μία θεμελιώδη αφαίρεση (abstraction) που λέγεται Iterable.
# iterable είναι ένα αντικείμενο που μπορεί να χρησιμοποιηθεί ως ακολουθία.
# Το αντικείμενο που επιστρέφει η συνάρτηση range, είναι ένα iterable.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['one', 'two', 'three']).
# Αυτό είναι ένα αντικείμενο που υλοποιεί την iterable διεπαφή μας.

# μπορούμε να τρέχουμε loops πάνω του.
for i in our_iterable:
    print(i)  # Prints one, two, three

# Ωστόσο δεν μπορούμε να προσπελάσουμε τα στοιχεία του με index.
our_iterable[1]  # προκαλεί a TypeError

# Ένα iterable είναι ένα αντικείμενο που ξέρει πώς να δημιουργήσει έναν iterator.
our_iterator = iter(our_iterable)

# Ο iterator μας είναι ένα αντικείμενο που μπορεί να θυμάται την κατάσταση όπως το διατρέχουμε.
# Παίρνουμε το επόμενο αντικείμενο με το "next()"
next(our_iterator)  # => "one"

# Διατηρεί την κατάσταση καθώς επαναλαμβάνουμε.
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# Όταν ο iterator έχει επιστρέψει όλα τα δεδομένα του, προκαλεί ένα μια εξαίρεση StopIteration.
next(our_iterator)  # προκαλεί StopIteration

# Μπορείς να πάρεις όλα τα αντικείμενα ενός iteratior καλώντας list() πάνω του.
list(filled_dict.keys())  # => Επιστρέφει ["one", "two", "three"]


####################################################
## 4. Συναρτήσεις
####################################################

# Χρησιμποιούμε το "def" για να ορίσουμε νέες συναρτήσεις
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y  # επιστρέφει τιμές με την εντολή return

# Καλούμε συναρτήσεις με παραμέτρους
add(5, 6)  # => τυπώνει "x is 5 and y is 6" και επιστρέφει 11

# Ένας άλλος τρόπος να καλέσεις συνάρτησει είναι με keyword arguments (ορίσματα λέξεις-κλειδιά)
add(y=6, x=5)  # τα Keyword arguments μπορούν να δωθούν με οποιαδήποτε σειρά.

# Μπορείς να ορίσεις συναρτήσεις που δέχονται μεταβλητό πλήθος ορισμάτων
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# Μπορούμε να ορίσουμε και συναρτήσεις που δέχονται μεταβλητό πλήθος keyword arguments
def keyword_args(**kwargs):
    return kwargs

# Για να δούμε τι γίνεται αν την καλέσουμε
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# Μπορείς να κάνεις και τα δύο ταυτόχρονα αν θες
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) τυπώνει:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Όταν καλείς συναρτήσεις μπορείς να κάνεις και το αντίστροφο από args/kwargs!
# Χρησιμοποίησε το * για να επεκτείνεις tuples και χρησιμοποίησε το ** για να επεκτείλεις kwargs
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # ισοδύναμο με all_the_args(1, 2, 3, 4)
all_the_args(**kwargs)         # ισοδύναμο με all_the_args(a=3, b=4)
all_the_args(*args, **kwargs)  # ισοδύναμο με all_the_args(1, 2, 3, 4, a=3, b=4)

# Επιστρέφουμε πλειάδα τιμών (με tuple assignments)
def swap(x, y):
    return y, x  # Επιστρέφει πολλές τιμές ως tuple χωρίς την παρένθεση
                 # (Σημ.: οι παρενθέσεις έχουν παραλειφθεί αλλά μπορούν να γραφούν)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # Ξανά, οι παρενθέσεις έχουν παραληφθεί αλλά μπορούν να γραφούν

# Εμβέλεια συναρτήσεων
x = 5

def set_x(num):
    # Η τοπική μεταβλητή x δεν είναι η ίδια με την global μεταβλητή x
    x = num    # => 43
    print(x)   # => 43

def set_global_x(num):
    global x
    print(x)   # => 5
    x = num    # η global μεταβλητή x τώρα είναι 6
    print(x)   # => 6

set_x(43)
set_global_x(6)


# Η Python έχει πρώτης τάξης συναρτήσεις
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# Αλλά έχει και anonymous συναρτήσεις.
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# Υπάρχουν ενσωματωμένες συναρτήσεις μεγαλύτερης τάξης
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# Μπορούμε να χρησιμοποιήσουμε list comprehensions για ωραία maps και filters
# το List comprehension αποθηκεύει την έξοδο ως μία λίστα που μπορεί και η ίδια
# να είναι μια εμφωλευμένη λίστα
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# Μπορείς επίσης να κατασκευάσεις set και dict comprehensions.
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. Modules
####################################################

# Μπορείς να κάνεις import modules
import math
print(math.sqrt(16))  # => 4.0

# Μπορείς να πάρεις συγκεκριμένες συναρτήσεις από ένα module
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# Μπορείς να κάνεις import όλες τις συναρτήσεις από ένα module.
# Προσοχή:  δεν προτείνεται
from math import *

# Μπορείς να δημιουργείς συντομογραφίες για τα ονόματα των modules
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Τα Python modules είναι απλά αρχεία Python. Μπορείς να δημιουργήσεις τα δικά σου
# και να τα κάνεις import το όνομα του module είναι ίδιο με το όνομα του αρχείου

# μπορείς να βρεις ποιες συναρτήσεις και γνωρίσματα ορίζονται στο module
import math
dir(math)

# Αν έχεις ένα Python script με όνομα math.py στον ίδιο φάκελο με το τρέχον script
# το αρχείο math.py θα φορτωθεί και όχι το built-in Python module
# Αυτό συμβαίνει επειδή τα τοπικά αρχεία έχουν προτεραιότητα έναντι των built-in
# βιβλιοθηκών της Python


####################################################
## 6. Κλάσεις - Classes
####################################################

# χρησιμοποιούμε το "class" statement για να δημιουργήσουμε μια κλάση
class Human:

    # Ένα γνώρισμα της κλάσης. Είναι κοινό για όλα τα στιγμιότυπα αυτής.
    species = "H. sapiens"

    # Βασικός initializer, καλείται όταν δημιουργείται στιγμιότυπο της κλάσης.
    # Σημ. οι διπλές κάτω παύλες πριν και μετά υποδηλώνουν αντικείμενα
    # ή γνωρίσματα που χρησιμοποιούνται από την Python αλλά ζουν σε ελεγχόμενα από
    # το χρήση namespaces.
    # Μέθοδοι (ή αντικείμενα ή γνωρίσματα) σαν τα __init__, __str__, __repr__ κλπ
    # είναι ειδικές μέθοδοι (λέγονται και dunder (double underscore) μέθοδοι)
    # Δεν πρέπει να δηλώνεις δικές σου τέτοιες συναρτήσεις
    def __init__(self, name):
        # Εκχώρησε στο attribute name του object το όρισμα
        self.name = name

        # Αρχικοποίησε την ιδιότητα
        self._age = 0

    # Μία μέθοδος στιγμιότυπου (instance method). Όλες οι μέθοδοι παίρνουν το
    # "self" ως πρώτο όρισμα
    def say(self, msg):
        print("{name}: {message}".format(name=self.name, message=msg))

    # Ακόμα μία instance method
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # Μία μέθοδος κλάσεις είναι κοινή ανάμεσα σε όλα τα instances.
    # Καλούνται με  calling class ώς πρώτο όρισμα
    @classmethod
    def get_species(cls):
        return cls.species

    # Μία στατική μέθοδος καλείται χωρίς αναφορά σε κλάση ή στιγμιότυπο
    @staticmethod
    def grunt():
        return "*grunt*"

    # Ένα property είναι ακριβώς σαν ένα getter.
    # Μετατρέπει τη μέθοδο age σε ένα γνώρισμα (attribute) μόνο-για-ανάγνωση
    # με το ίδιο όνομα.
    # Δεν χρειάζεται να γράφουμε τετριμένους getters και setters στην Python όμως.
    @property
    def age(self):
        return self._age

    # Αυτό επιτρέπει στο property να γίνει set
    @age.setter
    def age(self, age):
        self._age = age

    # Αυτό επιτρέπει σε ένα property να διαγραφεί
    @age.deleter
    def age(self):
        del self._age


# Όταν ο διερμηνέας της Python διαβάζει αρχείο πηγαίου κώδικα τον εκτελεί όλο.
# Αυτός ο έλεγχος του __name__ σιγουρεύει ότι αυτό το block κώδικα τρέχει μόνο
# αυτό το module είναι το κύριο πρόγραμμα (και όχι imported)
if __name__ == '__main__':
    # Δημιουργούμε στιγμιότυπο κλάσης
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # τα i και j είναι στιγμιότυπα του τύπου Human

    # Καλούμε τη μέθοδο της κλάσης
    i.say(i.get_species())          # "Ian: H. sapiens"
    # Αλλάζουμε το κοινό attribute των αντικειμένων της κλάσης
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # Καλούμε τη static μέθοδο
    print(Human.grunt())            # => "*grunt*"

    # Δεν μπορούμε να καλέσουμε τη στατική μέθοδο με ένα στιγμιότυπο
    # επειδή το i.grunt() θα βάλει αυτόματα το self (δηλαδή το αντικείμενο i) ως όρισμα
    print(i.grunt())                # => TypeError: grunt() takes 0 positional arguments but 1 was given

    # Ενημερώνουμε το property για αυτό το στγμιότυπο
    i.age = 42
    # Παίρνουμε το property
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # Διαγράφουμε το property
    del i.age
    # i.age                         # => αυτό θα προκαλούσε AttributeError


####################################################
## 6.1 Κληρονομικότητα - Inheritance
####################################################

# Η κληρονομικότητα επιτρέπει σε νέες κλάσεις-παιδιά να οριστούν και να υιοθετήσουν
# μεθόδους και μεταβλητές από την κλάση-γονέα.

# Χρησιμοποιώντας την κλάση Human που ορίστηκε πριν ως τη βασική κλάση (ή κλάση-γονέα)
# μπορούμε να ορίσουμε τις κλάσεις-παιδιά Superhero, που κληρονομεί μεταβλητές όπως
# "species", "name", και "age", καθώς και μεθόδους όπως "sing" και "grunt"
# από την κλάση Human, αλλά επίσης έχει τις δικές του ξεχωριστές ιδιότητες

# Για να εκμεταλλευτείς το modularization κατά αρχείο, μπορείς να βάλεις την παραπάνω κλάση
# σε δικό της αρχείο, ας πούμε human.py

# Για να κάνουμε import συναρτήσεις από άλλα αρχεία χρησιμοποιούμε το παρακάτω format
# from "filename-without-extension" import "function-or-class"

from human import Human


# Προσδιόρισε την/τις parent class(es) ως παραμέτρους της κλάσης που ορίζεται
class Superhero(Human):

    # Αν η κλάση-παιδί πρέπει να κληρονομήσει όλους τους οεισμούς της κλάσης-γονέα
    # χωρίς καμία αλλαγή, μπορείς απλά να γράψεις pass (και τίποτα άλλο)
    # αλλά σε αυτή την περίπτωση είναι σχολιασμένο για να επιτρέψει τη δημιουργία
    # ξεχωριστής κλάσης-παιδιού:
    # pass

    # Η κλάση παιδί μπορεί να υπερφορτώσει (override) τα attributes της κλάσης από την οποία κληρονομεί
    species = 'Superhuman'

    # Τα παιδιά αυτόματα, κληρονομούν τον constructo της κλάσης-γονέα
    # συμπεριλαμβανομένων των ορισμάτων, αλλά μπορείς και να ορίσεις πρόσθετα ορίσματα
    # ή ορισμούς και να κάνεις override τις μεθόδους, όπως τον constructor.
    # Αυτός ο constructor κληρονομεί το όρισμα "name" από την κλάση Human και
    # προσθέτει τα ορίσματα "superpower" και "movie":
    def __init__(self, name, movie=False,
                 superpowers=["super strength", "bulletproofing"]):

        # πρόσθήκη επιπλέον attributes της κλάσης:
        self.fictional = True
        self.movie = movie
        # έχετε το νου σας τις μεταβλητές (mutable)  default τιμές, καθώς είναι κοινές
        self.superpowers = superpowers

        # Η συνάρτηση "super" επιτρέπει την πρόσβαση στις μεθόδους της κλάσης-γονέα
        # που είναι υπερφορτωμένες από το παιδί. Σε αυτή την περίπτωση τη μέθοδο __init__
        # Το παρακάτω καλεί τον constructor της κλάσης-γονέα:
        super().__init__(name)

    # υπερφόρτωση της μεθόδου sing
    def sing(self):
        return 'Dun, dun, DUN!'

    # προσθήκη νέας μεθόδου που εφαρμόζεται σε στιγμιότυπα
    def boast(self):
        for power in self.superpowers:
            print("I wield the power of {pow}!".format(pow=power))


if __name__ == '__main__':
    sup = Superhero(name="Tick")

    # Έλεγχος για το αν το στιγμιότυπο sup ανήκει στην κλάση Human
    if isinstance(sup, Human):
        print('I am human')
    if type(sup) is Superhero:
        print('I am a superhero')
# TODO:
    # Παίρνουμε το  Method Resolution search Order που χρησιμοποιούν οι getattr() και super()
    # Αυτό το attribute είναι δυναμικό και μπορεί να ανανεωθεί
    print(Superhero.__mro__)    # => (<class '__main__.Superhero'>,
                                # => <class 'human.Human'>, <class 'object'>)

    # Καλούμε μέθοδο της κλάσης-γονέα, αλλά χρησιμοποιεί το δικό της attribute
    print(sup.get_species())    # => Superhuman

    # Καλεί την υπερφορτωμένη μέθοδο
    print(sup.sing())           # => Dun, dun, DUN!

    # Καλεί μέθοδο από την κλάση Human
    sup.say('Spoon')            # => Tick: Spoon

    # Καλεί μέθοδο που υπάρχει μόνο στην κλάση Superhero
    sup.boast()                 # => I wield the power of super strength!
                                # => I wield the power of bulletproofing!

    # Κληρονομημένο class attribute
    sup.age = 31
    print(sup.age)              # => 31

    # Attribute που υπάρχει μόνο στην μέσα στην κλάση Superhero
    print('Am I Oscar eligible? ' + str(sup.movie))

####################################################
## 6.2 Πολλαπλή Κληρονομικότητα - Multiple Inheritance
####################################################

# Ένας ακόμη ορισμός κλάσης
# bat.py
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # Αυτή η κλάση έχει επίσης μία μέθοδο say
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # Και τη δική της μέθοδο sonar
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)


# Και ορίζουμε μία ακόμα κλάση που κληρονομεί από τις κλάσεις Superhero και Bat
# superhero.py
from superhero import Superhero
from bat import Bat

# Ας πούμε αυτή την κλάση Batman
class Batman(Superhero, Bat):

    def __init__(self, *args, **kwargs):
        # Τυπικά γα να κληρονομήουμε attributes πρέπει να καλέσουμε τη super:
        # super(Batman, self).__init__(*args, **kwargs)      
        # Ωστόσο έχουμε να κάνουμε με πολλαπλή κληρονομικότητα εδώ, και το super()
        # δουλεύει μόνο με την αμέσως ανώτερη κλάση στην ιεραρχία.
        # Οπότε, καλούμε ρητά την __init__ για όλους τους πρόγονους
        # Η χρήση των *args και **kwargs επιτρέπει έναν καθαρό τρόπο για να περνάμε ορίσματα
        # με κάθε κλάση-γονέα να "βγάζει μία φλούδα από το κρεμμύδι".
        Superhero.__init__(self, 'anonymous', movie=True,
                           superpowers=['Wealthy'], *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # υπερφορτώνουμε την τιμή του γνωρίσματος name
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    #
    # Λάβε το Method Resolution search Order που χρησιμοποιείται από το getattr() και το super().
    # Αυτό το attribute είναι δυναμικό και μπορεί να ενημερωθεί
    print(Batman.__mro__)       # => (<class '__main__.Batman'>,
                                # => <class 'superhero.Superhero'>,
                                # => <class 'human.Human'>,
                                # => <class 'bat.Bat'>, <class 'object'>)

    # Καλεί την μέθοδο της κλάσης-πατέρα αλλά χρησιμοποιεί το attribute της δικής του κλάσης
    print(sup.get_species())    # => Superhuman

    # Καλεί την υπερφορτωμένη μέθοδο
    print(sup.sing())           # => nan nan nan nan nan batman!

    # Καλεί μέθοδο από την κλάση Human, επειδή μετράει η σειρά της κληρονομιάς
    sup.say('I agree')          # => Sad Affleck: I agree

    # Καλεί μέθοδο που ανήκει μόνο στον δεύτερο πρόγονο
    print(sup.sonar())          # => ))) ... (((

    # Attribute της κληρονομημένης κλάσης
    sup.age = 100
    print(sup.age)              # => 100

    # Κληρονομούμενο attribute από τον δεύτερο πρόγονο του οποίου η default τιμή
    # έχει υπερφορτωθεί.
    print('Can I fly? ' + str(sup.fly)) # => Can I fly? False



####################################################
## 7. Προχωρημένα
####################################################

# Με τους Generators μπορείς να γράψεις τεμπέλικο κώδικα.
def double_numbers(iterable):
    for i in iterable:
        yield i + i
# Οι Generators είναι αποδοτικοί από άποψη μνήμης επειδή φορτώνουν μόνο τα δεδομένα
# που είναι αναγκαία για να επεξεργαστούμε την επόμενη τιμή του iterable.
# Αυτό μας επιτρέπει να κάνουμε πράξεις σε τιμές που υπό άλλες συνθήκες θα ήταν
# απαγορευτικά μεγάλες.
for i in double_numbers(range(1, 900000000)):  # το `range` είναι ένας generator.
    print(i)
    if i >= 30:
        break

# Όπως μπορείς να δημιουργήσεις list comprehension, έτσι μπορείς να δημιουργήσεις και
# generator comprehensions
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # τυπώνει -1 -2 -3 -4 -5 στο console/terminal

# Μπορείς επίσης να μετατρέψεις ένα generator comprehension απευθείας σε λίστα.
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# Decorators
# σε αυτό το παράδειγμα το `beg` τυλίγει το `say`. Αν το say_please είναι True τότε
# θα αλλάξει το μήνυμα που επιστρέφεται.
from functools import wraps


def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Please! I am poor :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Can you buy me a beer?"
    return msg, say_please


print(say())                 # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(
```

## Έτοιμοι για περισσότερα?

### Δωρεάν Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)
* [Python 3 Computer Science Circles](http://cscircles.cemc.uwaterloo.ca/)
* [Dive Into Python 3](http://www.diveintopython3.net/index.html)
* [A Crash Course in Python for Scientists](http://nbviewer.jupyter.org/gist/anonymous/5924718)
