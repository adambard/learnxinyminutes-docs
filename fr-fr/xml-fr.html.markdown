---
language: xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Geoffrey Liu", "https://github.com/g-liu"]
filename: learnxml-fr.xml
lang: fr-fr
---

XML est un langage de balisage conçu pour stocker et transporter les informations.

Contrairement à HTML, XML ne spécifie pas comment afficher ou formater les informations, juste comment les porter.

* La syntaxe XML

```xml
<!-- Les commentaires en XML ressemblent ceci -->

<?xml version="1.0" encoding="UTF-8"?>
<librairie>
  <livre categorie="CUISINE">
    <titre lang="en">Everyday Italian</titre>
    <auteur>Giada De Laurentiis</auteur>
    <an>2005</an>
    <prix>30.00</prix>
  </livre>
  <livre categorie="ENFANTS">
    <titre lang="en">Harry Potter</titre>
    <auteur>J. K. Rowling</auteur>
    <an>2005</an>
    <prix>29.99</prix>
  </livre>
  <livre categorie="WEB">
    <titre lang="en">Learning XML</titre>
    <auteur>Erik T. Ray</auteur>
    <an>2003</an>
    <prix>39.95</prix>
  </livre>
</librairie>

<!-- Ce qui suit est un fichier XML typique.
  Il commence par une déclaration, qui informe certaines métadonnées (en option).

XML utilise une structure arborescente. Ci-dessus, le nœud racine est «librairie», qui a 
   trois nœuds enfants, qui sont appelés «livres». Ces nœuds ont plus de nœuds enfants, et ainsi de suite ...

On crée les nœuds avec des balises d'ouverture / fermeture, et les enfants sont les nœuds juste entre 
   les balises d'ouverture et de fermeture. -->


<!-- XML porte deux types d'informations:
  1 - Les attributs -> les métadonnées sur un nœud.
      Habituellement, l'analyseur XML utilise cette information pour bien stocker les données.
  2 - Les éléments -> les informations pures.
      C'est ce que l'analyseur retrouvera du fichier XML.
      Les éléments apparaissent entre les balises d'ouverture et de fermeture, sans parenthèses. -->
      
  
<!-- Ci-dessous, un élément avec deux attributs -->
<fichier type="gif" id="4293">ordinateur.gif</fichier>


```

* Un document bien formaté & le validation

Un document XML est bien formaté s'il est syntaxiquement correct. 
Cependant, il est possible d'injecter plus de contraintes dans le document, 
en utilisant les définitions de documents, tels que les schémas DTD et XML.

Un document XML qui suit une définition de document est dit valide, 
en ce qui concerne ce document.

Avec cet outil, vous pouvez vérifier les données XML en dehors de la logique de l'application.

```xml

<!-- Ci-dessous, vous pouvez voir une version simplifiée du document de librairie, 
   avec l'addition de définition DTD. -->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Librairie.dtd">
<librairie>
  <livre categorie="CUISINE">
    <titre>Everyday Italian</titre>
    <prix>30.00</prix>
  </livre>
</librairie>

<!-- Cette DTD pourrait être quelque chose comme: -->

<!DOCTYPE note
[
<!ELEMENT librairie (livre+)>
<!ELEMENT livre (titre,prix)>
<!ATTLIST livre categorie CDATA "Littérature">
<!ELEMENT titre (#PCDATA)>
<!ELEMENT prix (#PCDATA)>
]>

<!-- La DTD commence par une déclaration.
   Après, le nœud racine est déclaré, qui exige un ou plusieurs nœuds enfants. 
   Chaque «livre» doit contenir exactement un «titre» et «prix» et un attribut 
   appelé «catégorie», avec «littérature» comme valeur par défaut. 
   Les nœuds de «titre» et «prix» contiennent des informations de caractère analysés
   (Anglais: «parsed character data») -->

<!-- La DTD pourrait être déclarée dans le fichier XML lui-même -->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT librairie (livre+)>
<!ELEMENT livre (titre,prix)>
<!ATTLIST livre categorie CDATA "Littérature">
<!ELEMENT titre (#PCDATA)>
<!ELEMENT prix (#PCDATA)>
]>

<librairie>
  <livre categorie="CUISINE">
    <titre>Everyday Italian</titre>
    <prix>30.00</prix>
  </livre>
</librairie>
```
