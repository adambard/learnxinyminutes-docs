---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Geoffrey Liu", "http://github.com/g-liu"]
filename: markdown.md
---

John Gruber a créé la langue Markdown en 2004. Markdown est destiné à être facile à lire et à écrire, et facile à convertir à HTML (et actuellement, de nombreux autres formats ainsi).

Donnez-moi autant de réactions que vous voulez! / N'hésitez pas à débourser et tirez-demander!


```markdown
<!--
	Le markdown (MD) est un surensemble d'HTML, donc n'importe quel fichier HTML est valide MD.
	Ce signifie que nous pouvons utiliser les éléments HTML dans le MD, comme l'élément de commentaire, et
	un analyseur de MD ne sera pas les affecter. Néanmoins, si vous créez un élément HTML
	dans votre fichier MD, vous ne pouvez pas utilizer la syntaxe MD dans le contenu de l'élément.
-->

<!--
	Markdown varie aussi dans leur implementation, à partier d'analyseur aux autre. Ce
	guide va tenter de clarifier lorsque les traits sont universels ou lorsqu'ils sont
	spécifiques à un certain analyseur
-->


<!-- Boutisses/Headers -->
<!-- Vous pouvez çréer facilement les éléments HTML <h1> à <h6>, en faisant précéder le
	texte avec le nombre correspondant de hachages -->
# C'est un <h1>
## C'est un <h2>
### C'est un <h3>
#### C'est un <h4>
##### C'est un <h5>
###### C'est un <h6>

<!-- Markdown nous fournit également avec deux chemins alternatifs d'indiquer h1 et h2 -->
C'est un h1
=============

C'est un h2
-------------

<!-- Simples styles de texte -->
<!-- Text can be easily styled as italic or bold using markdown -->

*This text is in italics.*
_And so is this text._

**This text is in bold.**
__And so is this text.__

***This text is in both.***
**_As is this!_**
*__And this!__*

<!-- In Github Flavored Markdown, which is used to render markdown files on
Github, we also have strikethrough: -->

~~This text is rendered with strikethrough.~~

<!-- Paragraphs are a one or multiple adjacent lines of text separated by one or
multiple blank lines. -->

This is a paragraph. I'm typing in a paragraph isn't this fun?

Now I'm in paragraph 2.
I'm still in paragraph 2 too!


I'm in paragraph three!

<!-- Should you ever want to insert an HTML <br /> tag, you can end a paragraph
with two or more spaces and then begin a new paragraph. -->

I end with two spaces (highlight me to see them).  

There's a <br /> above me!

<!-- Block quotes are easy and done with the > character. -->

> This is a block quote. You can either
> manually wrap your lines and put a `>` before every line or you can let your lines get really long and wrap on their own.
> It doesn't make a difference so long as they start with a `>`.

> You can also use more than one level
>> of indentation?
> How neat is that?

<!-- Lists -->
<!-- Unordered lists can be made using asterisks, pluses, or hyphens -->

* Item
* Item
* Another item

or

+ Item
+ Item
+ One more item

or 

- Item
- Item
- One last item

<!-- Ordered lists are done with a number followed by a period -->

1. Item one
2. Item two
3. Item three

<!-- You don't even have to label the items correctly and markdown will still
render the numbers in order, but this may not be a good idea -->

1. Item one
1. Item two
1. Item three
<!-- (This renders the same as the above example) -->

<!-- You can also use sublists -->

1. Item one
2. Item two
3. Item three
    * Sub-item
    * Sub-item
4. Item four

<!-- Code blocks -->
<!-- You can indicate a code block (which uses the <code> element) by indenting
a line with four spaces or a tab -->

    This is code
    So is this

<!-- You can also re-tab (or add an additional four spaces) for indentation
inside your code -->

    my_array.each do |item|
        puts item
    end

<!-- Inline code can be created using the backtick character ` -->

John didn't even know what the `go_to()` function did!

<!-- In Github Flavored Markdown, you can use a special syntax for code -->

\`\`\`ruby <!-- except remove those backslashes when you do this, just ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- here too, no backslashes, just ``` -->

<-- The above text doesn't require indenting, plus Github will use syntax
highlighting of the language you specify after the ``` -->

<!-- Horizontal rule (<hr />) -->
<!-- Horizontal rules are easily added with three or more asterisks or hyphens,
with or without spaces. -->

***
---
- - - 
****************

<!-- Links -->
<!-- One of the best things about markdown is how easy it is to make links. Put
the text to display in hard brackets [] followed by the url in parentheses () -->

[Click me!](http://test.com/)

<!-- You can also add a link title using quotes inside the parentheses -->

[Click me!](http://test.com/ "Link to Test.com")

<!-- Relative paths work too. -->

[Go to music](/music/).

<!-- Markdown also supports reference style links -->

[Click this link][link1] for more info about it!
[Also check out this link][foobar] if you want to.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"

<!-- The title can also be in single quotes or in parentheses, or omitted
entirely. The references can be anywhere in your document and the reference IDs
can be anything so long as they are unique. -->

<!-- There is also "implicit naming" which lets you use the link text as the id -->

[This][] is a link.

[this]: http://thisisalink.com/

<!-- But it's not that commonly used. -->

<!-- Images -->
<!-- Images are done the same way as links but with an exclamation point in front! -->

![This is the alt-attribute for my image](http://imgur.com/myimage.jpg "An optional title")

<!-- And reference style works as expected -->

![This is the alt-attribute.][myimage]

[myimage]: relative/urls/cool/image.jpg "if you need a title, it's here"

<!-- Miscellany -->
<!-- Auto-links -->

<http://testwebsite.com/> is equivalent to
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-links for emails -->

<foo@bar.com>

<!-- Escaping characters -->

I want to type *this text surrounded by asterisks* but I don't want it to be
in italics, so I do this: \*this text surrounded by asterisks\*.

<!-- Tables -->
<!-- Tables are only available in Github Flavored Markdown and are slightly
cumbersome, but if you really want it: -->

| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| Left-aligned | Centered | Right-aligned |
| blah         | blah     | blah          |

<!-- or, for the same results -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop

<!-- The end! -->

```

For more info, check out John Gruber's official post of syntax [here](http://daringfireball.net/projects/markdown/syntax) and Adam Pritchard's great cheatsheet [here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
