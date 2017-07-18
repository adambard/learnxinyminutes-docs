---
language: ruby
filename: learnruby-fr.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]

translators:
  - ["Geoffrey Roguelon", "https://github.com/GRoguelon"]
  - ["Nami-Doc", "https://github.com/Nami-Doc"]
  - ["Sylvain Abélard", "http://github.com/abelards"]
lang: fr-fr
---

```ruby
# Ceci est un commentaire

=begin
Ceci est un commentaire multiligne
Personne ne les utilise
Vous devriez en faire de même
=end

# Tout d'abord : tout est un objet.

# Les nombres sont des objets

3.class #=> Fixnum # on voit que c'est une classe Ruby et non un "type spécial"

3.to_s #=> "3" # on peut appeler des méthodes sur ces objets, comme `to_s` (transforme en texte)

# Les opérateurs de base
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
22 / 7 #=> 3 # si les deux éléments sont entiers, c'est une division entière
22.0 / 7 #=> 3.142857142857143
22 / 7.0 #=> 3.142857142857143
2**10 #=> 1024 # exposant
1024 % 10 #=> 4 # modulo (reste de la division euclidienne)

# Les opérateurs sont juste des raccourcis
# pour appeler une méthode sur un objet
1.+(3) #=> 4
10.* 5 #=> 50

# Les valeurs spéciales sont des objets
nil # nul, vide ou indéfini
true # vrai
false # faux

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Égalité
1 == 1 #=> true
2 == 1 #=> false

# Inégalité
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# à part false lui-même, nil est la seule autre valeur "considérée comme fausse"
!nil   #=> true
!false #=> true
!0     #=> false

# Plus de comparaisons
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Les chaînes de caractères sont des objets
'Je suis une chaîne de caractères'.class #=> String
"Je suis également une chaîne de caractères".class #=> String

placeholder = "utiliser l'interpolation de chaîne de caractères"
"Je peux #{placeholder} quand j'utilise les guillemets"
#=> "Je peux utiliser l'interpolation de chaîne de caractères quand j'utilise les guillemets"

# Affichez un message
puts "J'affiche à l'écran!"

# Il y a quelques autres raccourcis pour les chaînes de caractères
rb = "Ruby"
interpolation = "Bonjour Ruby" # s'écrit aussi %Q[Bonjour #{rb}] %Q{Bonjour #{rb}} avec l'interpolation
literal = "Bonjour \#{rb}" # avec le backslash, le dièse est un "vrai dièse" (le slash ne s'affiche que pour le debug)
literal == %q[Bonjour #{rb}] # le Q majuscule fait l'interpolation, le q minuscule ne la fait pas
multi = %Q[Cette chaîne
est sur plusieurs
lignes] # => "Cette chaîne\nest sur plusieurs\nlignes" # le caractère \n signifie retour à la ligne

# Variables
x = 25 #=> 25
x #=> 25

# Notez que l'affectation retourne la valeur affectée.
# Cela signifie que vous pouvez affecter plusieurs fois de suite :

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Par convention, utilisez la notation underscore
# pour nommer les variables
snake_case = true

# Utilisez des noms de variable explicites
path_to_project_root = '/nom/correct/'
path = '/mauvais/nom/'

# Symboles (aussi des objets)
# Les symboles sont immuables, constants,
# réutilisables et représentés en interne
# par une valeur entière. Ils sont souvent
# utilisés à la place des chaînes de caractères
# pour transmettre efficacement des valeurs
# spécifiques ou significatives

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Tableaux

# Ceci est un tableau
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Les tableaux contiennent différents types d'élément.

[1, "hello", false] #=> [1, "hello", false]

# Les tableaux peuvent être indexés
# Trouver la valeur en donnant la position en partant du début (le premier élément est à l'index 0)
array[0] #=> 1
array[12] #=> nil

# Comme les opérateurs, la syntaxe [var] est juste un raccourci
# pour appeler la méthode [] d'un objet
array.[] 0 #=> 1
array.[] 12 #=> nil

# On peut compter en partant de la fin avec un index négatif (le dernier élément est à l'index -1)
array[-1] #=> 5

# Avec un index de début et un nombre d'éléments [position, nombre]
array[1, 3] #=> [2, 3, 4]

# Ou avec un intervalle [index de début .. index de fin]
array[1..3] #=> [2, 3, 4]

# Ajoutez un élément au tableau comme ceci
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Les Hash sont des dictionnaires Ruby contenant des paires de clé/valeur.
# Les Hash sont délimitées par des accolades :
hash = {'color' => 'green', 'number' => 5}

hash.keys #=> ['color', 'number']

# Les Hash retournent la valeur
# en utilisant la clé associée à la valeur :
hash['color'] #=> 'green'
hash['number'] #=> 5

# Recherchez une clé inexistante dans une Hash retourne nil :
hash['nothing here'] #=> nil

# Depuis Ruby 1.9, Une syntaxe spécifique est apparue
# en utilisant les symboles comme clés :

new_hash = { defcon: 3, action: true}

new_hash.keys #=> [:defcon, :action]

# Astuce : Les tableaux et les Hash sont dénombrables
# Ils partagent un certain nombre de méthodes pratiques
# telle que each, map, count, etc...

# Structures de contrôle

if true
  "si l'instruction est vraie"
elsif false
  "si l'instruction de départ n'était pas vraie, et que cette nouvelle condition est vraie (facultatif)"
else
  "tous les autres cas (il est également facultatif de faire une clause else)"
end

for compteur in 1..5
  puts "itération #{compteur}"
end
#=> itération 1
#=> itération 2
#=> itération 3
#=> itération 4
#=> itération 5

# CEPENDANT, l'usage de la boucle for est très rare.
# À la place, utilisez la méthode "each"
# et passez lui un bloc de code.
# Un bloc de code est un ensemble d'instructions
# que vous pouvez passer à une methode comme "each".
# Les blocs sont similaires aux lambdas, aux fonctions anonymes
# ou encore aux closures dans d'autres langages.
#
# La méthode "each" exécute le bloc de code
# pour chaque élément de l'intervalle d'éléments.
# Le bloc de code passe un paramètre compteur.
# Appelez la méthode "each" avec un bloc de code comme ceci :

(1..5).each do |compteur|
  puts "itération #{compteur}"
end
#=> itération 1
#=> itération 2
#=> itération 3
#=> itération 4
#=> itération 5

# Vous pouvez également mettre un bloc de code entre accolades :
(1..5).each {|compteur| puts "itération #{compteur}"}

# Le contenu des structures de données peut être parcouru
# en utilisant la méthode each.
array.each do |element|
  puts "#{element} est une partie du tableau"
end
hash.each do |cle, valeur|
  puts "#{cle} est #{valeur}"
end

compteur = 1
while compteur <= 5 do
  puts "itération #{compteur}"
  compteur += 1
end
#=> itération 1
#=> itération 2
#=> itération 3
#=> itération 4
#=> itération 5

grade = 'B'

case grade
when 'A'
  puts "Excellent"
when 'B'
  puts "Plus de chance la prochaine fois"
when 'C'
  puts "Vous pouvez faire mieux"
when 'D'
  puts "C'est pas terrible"
when 'F'
  puts "Vous avez échoué!"
else
  puts "Sytème de notation alternatif"
end

# Fonctions

def double(x)
  x * 2
end

# Les fonctions (et tous les blocs de code) retournent
# implicitement la valeur de la dernière instruction évaluée
double(2) #=> 4

# Les parenthèses sont facultatives
# lorsqu'il n'y a pas d'ambiguïté sur le résultat
double 3 #=> 6

double double 3 #=> 12

def sum(x,y)
  x + y
end

# Les paramètres de méthode sont séparés par des virgules
sum 3, 4 #=> 7

sum sum(3,4), 5 #=> 12

# yield
# Toutes les méthodes ont un argument facultatif et implicite
# de type bloc de code
# il peut être appelé avec le mot clé 'yield'

def surround
  puts "{"
  yield
  puts "}"
end

surround { puts 'Bonjour tout le monde' }

# {
# Bonjour tout le monde
# }


# Définissez une classe avec le mot clé 'class'
class Humain

  # Une variable de classe
  # est partagée par toutes les instances de cette classe.
  @@espece = "H. sapiens"

  # Constructeur de classe
  def initialize(nom, age = 0)
    # Affectez l'argument à la variable d'instance 'nom'
    # pour la durée de vie de l'instance de cette classe
    @nom = nom
    # Si le paramètre 'age' est absent,
    # la valeur par défaut définie dans la liste des arguments sera utilisée.
    @age = age
  end

  # Une simple méthode setter
  def nom=(nom)
    @nom = nom
  end

  # Une simple méthode getter
  def nom
    @nom
  end

  # Une méthode de classe utilise le mot clé 'self'
  # pour se distinguer de la méthode d'instance.
  # La méthode sera alors accessible à partir de la classe
  # et non pas de l'instance.
  def self.say(msg)
    puts "#{msg}"
  end

  def espece
    @@espece
  end

end


# Instanciez une classe
jim = Humain.new("Jim Halpert")

dwight = Humain.new("Dwight K. Schrute")

# Appelez quelques méthodes
jim.espece #=> "H. sapiens"
jim.nom #=> "Jim Halpert"
jim.nom = "Jim Halpert II" #=> "Jim Halpert II"
jim.nom #=> "Jim Halpert II"
dwight.espece #=> "H. sapiens"
dwight.nom #=> "Dwight K. Schrute"

# Appelez la méthode de classe
Humain.say("Hi") #=> "Hi"

# Les classes sont également des objets en Ruby.
# Par conséquent, les classes peuvent avoir des variables d'instance.
# Les variables de classe sont partagées parmi
# la classe et ses descendants.

# Classe parente
class Humain
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(valeur)
    @@foo = valeur
  end
end

# Classe fille
class Travailleur < Humain
end

Humain.foo # 0
Travailleur.foo # 0

Humain.foo = 2 # 2
Travailleur.foo # 2

# Les variables d'instance de classe ne sont pas partagées
# avec les classes héritées.

class Humain
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(valeur)
    @bar = valeur
  end
end

class Docteur < Humain
end

Humain.bar # 0
Docteur.bar # nil

```
