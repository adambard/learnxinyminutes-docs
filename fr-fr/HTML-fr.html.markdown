---
language: html
filename: learnhtml-fr.html
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
lang: fr-fr
---

HTML signifie HyperText Markup Language. 
C'est un langage (format de fichiers) qui permet d'écrire des pages internet.
C’est un langage de balisage, il nous permet d'écrire des pages HTML au moyen de balises (Markup, en anglais).
Les fichiers HTML sont en réalité de simple fichier texte.
Qu'est-ce que le balisage ? C'est une façon de hiérarchiser ses données en les entourant par une balise ouvrante et une balise fermante. 
Ce balisage sert à donner une signification au texte ainsi entouré.
Comme tous les autres langages, HTML a plusieurs versions. Ici, nous allons parlons de HTML5.

**NOTE :** Vous pouvez tester les différentes balises que nous allons voir au fur et à mesure du tutoriel sur des sites comme [codepen](http://codepen.io/pen/) afin de voir les résultats, comprendre, et vous familiariser avec le langage.
Cet article porte principalement sur la syntaxe et quelques astuces.


```html
<!-- Les commentaires sont entouré comme cette ligne! -->

<!-- #################### Les balises #################### -->
   
<!-- Voici un exemple de fichier HTML que nous allons analyser -->
<!-- Venez voir ce que ça donne  --> 

<!doctype html>
	<html>
		<head>
			<title>Mon Site</title>
		</head>
		<body>
			<h1>Hello, world!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">Venez voir ce que ça donne</a>
			<p>Ceci est un paragraphe</p>
			<p>Ceci est un autre paragraphe</p>
			<ul>
				<li>Ceci est un item d'une liste non ordonnée (liste à puces)</li>
				<li>Ceci est un autre item</li>
				<li>Et ceci est le dernier item de la liste</li>
			</ul>
		</body>
	</html>

<!-- Un fichier HTML débute toujours par indiquer au navigateur que notre page est faite en HTML -->

<!doctype html>

<!-- Après ça on commence par ouvrir une balise <html> -->
<html>
</html>
<!-- Et puis on la referme à la fin du fichier avec </html> -->
<!-- après cette balise de fin, plus rien ne doit apparaître. -->

<!-- À l'intérieur (entre la balise ouvrant et fermante <html></html>), on trouve : -->

<!-- Un entête (<head> en anglais ; il faut le refermer avec </head>) -->
<!-- L'entête contient des descriptions et informations annexes qui ne sont pas affichées : se sont les métadonnées -->

<head>
	<title>Mon Site</title><!-- La balise <title> permet d'indiquer au navigateur le titre à afficher dans la barre de l'onglet de la fenêtre -->
</head>

<!-- Après la balise <head>, on trouve la balise <body> -->
<!-- Pour le moment, rien n'est encore affiché dans la fenêtre du navigateur. -->
<!-- Il faut ensuite remplir le corps (balise <body>) avec du contenu -->

<body>
	<h1>Hello, world!</h1> <!-- La balise h1 permet de structurer le texte, c'est  un titre -->
	<!-- Il exite différents sous-titres à <h1> qui sont hiérarchisés du plus important (h2) au plus précis (h6) -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">Venez voir ce que ça donne</a> <!-- Lien vers la source cible indiqué dans href="" -->
	<p>Ceci est un paragraphe </p> <!-- La balise <p> permet d'inclure du texte à la page html -->
	<p>Ceci est un autre paragraphe</p>
	<ul> <!-- La balise <ul> permet d'introduire une liste à puces -->
	<!-- Si on souhaite une liste ordonnée : <ol> liste numérotée, 1. pour le premier élément, 2. pour le second, etc -->
		<li>Ceci est un item d'une liste non ordonnée (liste à puces)</li>
		<li>Ceci est un autre item</li>
		<li>Et ceci est le dernier item de la liste</li>
	</ul>
</body>

<!-- Voilà comment créer un fichier HTML simple -->

<!-- Mais il est possible d'ajouter encore des balises plus spécifiques -->

<!-- Pour insérer une image -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- On indique la source de l'image dans src="" -->
<!-- La source peut-être un URL ou encore la destination d'un fichier de votre ordinateur -->

<!-- Il est possible de réaliser des tableaux également -->

<table> <!-- On ouvre la balise <table> -->
	<tr> <!-- <tr> permet de créer une ligne -->
		<th>First Header</th> <!-- <th> permet de créer un titre au tableau -->
		<th>Second Header</th>
	</tr>
	<tr>
		<td>Première ligne, première cellule</td> <!-- <td> permet de créer une cellule -->
		<td>Première ligne, deuxième cellule</td>
	</tr>
	<tr>
		<td>Deuxième ligne, première cellule</td>
		<td>Deuxième ligne, deuxième cellule</td>
	</tr>
</table>

```

## Utilisation

Le HTML s'écrit dans des fichiers `.html`.

## En savoir plus 

* [Tutoriel HTML](http://slaout.linux62.org/html_css/html.html)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
