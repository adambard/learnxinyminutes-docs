---
language: xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
  - ["Geoffrey Liu", "https://github.com/g-liu"]
filename: learnxml.xml
---

XML est un langage de balisage conçu pour stocker et transporter des informations.

Contrairement à HTML, XML ne spécifie pas comment afficher ou de formater les informations, juste comment le porter.

* La syntaxe XML

```xml
<!-- Les commentaires en XML ressemblent ceci -->

<?xml version="1.0" encoding="UTF-8"?>
<librarie>
  <livre categorie="CUISINE">
    <titre lang="en">Everyday Italian</titre>
    <auteur>Giada De Laurentiis</auteur>
    <an>2005</an>
    <prix>30.00</prix>
  </livre>
  <livre category="CHILDREN">
    <titre lang="en">Harry Potter</titre>
    <auteur>J K. Rowling</auteur>
    <an>2005</an>
    <prix>29.99</prix>
  </livre>
  <livre category="WEB">
    <titre lang="en">Learning XML</titre>
    <auteur>Erik T. Ray</auteur>
    <an>2003</an>
    <prix>39.95</prix>
  </livre>
</librarie>

<!-- Ce qui précède est un fichier XML typique.
  Il commence par une déclaration, qui informe certaines métadonées (en option).

XML utilise une structure arborescente. Ci-dessus, le nœud racine est «librairie», qui a 
   trois nœuds enfants, qui sont appelés «livres». Ces nœuds a plus de nœuds enfants, et ainsi de suite ...

Les noeuds sont créés avec des balises d'ouverture / fermeture, et enfants sont les nœuds juste entre 
   les balises d'ouverture et de fermeture. -->


<!-- XML porte deux types d'informations:
  1 - Les attributs -> les metadonnées sur un nœud.
      Habituellement, l'analyseur XML utilise cette information pour stocker les informations
  2 - Les éléments -> l'informations pures.
      C'est ce que l'analyseur de récupérer le fichier XML.
      Éléments apparaissent entre les balises d'ouverture et de fermeture, sans parenthèses. -->
      
  
<!-- Ci-dessous, un élément avec deux attributs -->
<fichier type="gif" id="4293">ordinateur.gif</fichier>


```

* Un document bien-formaté x le validation

Un document XML est bien formaté s'il est syntaxiquement correcte. 
Cependant, il est possible d'injecter plus de contraintes dans le document, 
en utilisant les définitions de documents, tels que les schémas DTD et XML.

Un document XML qui suit une définition de document est dite valide, 
en ce qui concerne ce document.

Avec cet outil, vous pouvez vérifier les données XML en dehors de la logique de l'application.

```xml

<!-- Ci-dessous, vous pouvez voir une version simplifiée du document de librairie, 
   avec l'addition de définition DTD. -->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Librarie.dtd">
<librarie>
  <livre categorie="CUISINE">
    <titre>Everyday Italian</titre>
    <prix>30.00</prix>
  </livre>
</librarie>

<!-- Cette DTD pourrait être quelque chose comme: -->

<!DOCTYPE note
[
<!ELEMENT librarie (livre+)>
<!ELEMENT livre (titre,prix)>
<!ATTLIST livre categorie CDATA "Littérature">
<!ELEMENT titre (#PCDATA)>
<!ELEMENT prix (#PCDATA)>
]>

<!-- La DTD commence par une déclaration. 
   Après, le nœud racine est déclarée, qui exige un ou plusieurs nœuds enfants. 
   Chaque «livre» doit contenir exactement un «titre» et «prix» et un attribut 
   appelée «catégorie», de «littérature» comme sa valeur par défaut. 
   Le «titre» et nœuds de «prix» contiennent d'informations de caractère analysées
   (anglais: «parsed character data») -->

<!-- La DTD pourrait être déclaré dans le fichier XML lui-même -->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT librarie (livre+)>
<!ELEMENT livre (titre,prix)>
<!ATTLIST livre categorie CDATA "Littérature">
<!ELEMENT titre (#PCDATA)>
<!ELEMENT prix (#PCDATA)>
]>

<librarie>
  <livre categorie="CUISINE">
    <titre>Everyday Italian</titre>
    <prix>30.00</prix>
  </livre>
</librarie>
```
