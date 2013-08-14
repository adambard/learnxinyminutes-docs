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
lang: fr-fr
---

```ruby
# Ceci est un commentaire

=begin
Ceci est un commentaire multiligne
Personne ne les utilise
Vous devriez en faire de même
=end

# Tout d'abord : Tout est un objet.

# Les nombres sont des objets

3.class #=> Fixnum

3.to_s #=> "3"

# Arithmétique de base
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# L'arithmétique est juste un raccourci
# pour appeler la méthode d'un objet
1.+(3) #=> 4
10.* 5 #=> 50

# Les valeurs spéciales sont des objets
nil # Nothing to see here
true # truth
false # falsehood

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

# à part false lui-même, nil est la seule autre valeur 'false'

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

# Afficher sur la sortie standard
puts "J'affiche à l'écran!"

# Variables
x = 25 #=> 25
x #=> 25

# Notez que l'affectation retourne la valeur affectée.
# Cela signifie que vous pouvez affecter plusieurs fois de suite :

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Par convention, utiliser snake_case (à base d'underscore)
# comme nom de variable
snake_case = true

# Utiliser des noms de variable explicites
path_to_project_root = '/nom/correct/'
path = '/mauvais/nom/'

# Symboles (sont des objets)
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

# Les tableaux peuvent être indéxés
# Du début
array[0] #=> 1
array[12] #=> nil

# Comme l'arithmétique, la syntaxe [var] est juste un raccourci
# pour appeler la méthode [] d'un objet
array.[] 0 #=> 1
array.[] 12 #=> nil

# À la fin
array[-1] #=> 5

# Avec un index de début et de fin
array[2, 4] #=> [3, 4, 5]

# Ou avec un interval
array[1..3] #=> [2, 3, 4]

# Ajouter un élément au tableau comme ceci
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Les Hashes sont des dictionnaires Ruby contenant des paires de clé/valeur.
# Les Hashes sont délimitées par des accolades :
hash = {'color' => 'green', 'number' => 5}

hash.keys #=> ['color', 'number']

# Les Hashes peuvent retourner rapidement
# la valeur en utilisant la clé :
hash['color'] #=> 'green'
hash['number'] #=> 5

# Rechercher une clé inexistante dans une Hash retourne nil:
hash['nothing here'] #=> nil

# Depuis Ruby 1.9, Une syntaxe spécifique est apparue en utilisant les symboles comme clés :

new_hash = { defcon: 3, action: true}

new_hash.keys #=> [:defcon, :action]

# Astuce : Les tableaux et les Hashes sont dénombrables
# Ils partagent un certain nombre de méthodes pratiques
# telle que each, map, count, etc...

# Structures de contrôle

if true
  "si instruction"
elsif false
  "autrement si, facultatif"
else
  "autrement, également facultatif"
end

for compteur in 1..5
  puts "itération #{compteur}"
end
#=> itération 1
#=> itération 2
#=> itération 3
#=> itération 4
#=> itération 5

# CEPENDANT, Personne n'utilise la boucle for.
# À la place, vous devrez utiliser la méthode "each"
# et lui passer un bloc de code.
# Un bloc de code est un ensemble d'instructions que vous pouvez passer à une methode comme "each".
# Les blocs sont similaires aux lambdas, les fonctions anonymes ou les closures dans d'autres langages.
#
# La méthode "each" exécute le bloc de code pour chaque élément de l'interval d'éléments.
# Le bloc de code passe un paramètre compteur.
# Appeler la méthode "each" avec un bloc de code comme ceci :

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
  puts "Excellence"
when 'B'
  puts "Plus de chance la première fois"
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

# Les paranthèses sont facultative
# où il n'y a pas d'ambiguïté sur le résultat
double 3 #=> 6

double double 3 #=> 12

def sum(x,y)
  x + y
end

# Les paramètres de méthode sont séparés par des virgules
sum 3, 4 #=> 7

sum sum(3,4), 5 #=> 12

# yield
# Toutes les méthodes ont un argument de type bloc de code
# facultatif et implicite
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


# Définir une classe avec le mot clé 'class'
class Humain

  # Une variable de classe,
  # elle est partagée par toutes les instances de cette classe.
  @@espece = "H. sapiens"

  # Constructeur de classe
  def initialize(nom, age = 0)
    # Affecter l'argument à la variable d'instance 'nom'
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

  def species
    @@species
  end

end


# Instancier une classe
jim = Humain.new("Jim Halpert")

dwight = Humain.new("Dwight K. Schrute")

# Appelons quelques méthodes
jim.espece #=> "H. sapiens"
jim.nom #=> "Jim Halpert"
jim.nom = "Jim Halpert II" #=> "Jim Halpert II"
jim.nom #=> "Jim Halpert II"
dwight.espece #=> "H. sapiens"
dwight.nom #=> "Dwight K. Schrute"

# Appelons la méthode de classe
Humain.say("Hi") #=> "Hi"

# Les classes sont également des objets en Ruby.
# Par conséquent, les classes peuvent avoir des variables d'instance.
# Les variables de classe sont partagées parmi
# la classe et ses descendants.

# Classe de base
class Humain
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(valeur)
    @@foo = valeur
  end
end

# Héritage de classe
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
