---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
    - ["Gabriele Luz", "https://github.com/gabrieleluz"]
    - ["Monique Baptista", "https://github.com/bfmonique"]
    - ["Marcel Ribeiro-Dantas", "https://github.com/mribeirodantas"]

lang: pt-br
filename: learnmarkdown-pt.md
---

O Markdown foi lançado por John Gruber em 2004. Criado para ser uma sintaxe
fácil de ler e escrever e que é facilmente convertida em HTML (hoje, suporta
outros formatos também).

O Markdown varia em termos de implementação de um parser para outro. Esse guia
irá tentar deixar explícito quando os recursos são universais ou quando são
específicos para um parser em específico.

## Elementos HTML
O Markdown é um superconjunto do HTML, de modo que qualquer arvquivo HTML é
um arquivo Markdown válido.

```md
<!--Isso significa que nós podemos usar elementos HTML em Markdown, como o
elemento de comentário, e eles não serão afetados pelo analisador de
remarcação. No entanto, se você criar um elemento HTML em seu arquivo Markdown,
você não pode usar sintaxe de remarcação dentro do conteúdo desse elemento.-->
```

## Cabeçalhos

Você pode criar elementos HTML `<h1>` até `<h6>` facilmente antecedendo o texto
que deseja estar nesse elemento por um número de cerquilhas (#).

```md
# Isto é um cabeçalho <h1>
## Isto é um cabeçalho <h2>
### Isto é um cabeçalho <h3>
#### Isto é um cabeçalho <h4>
##### Isto é um cabeçalho <h5>
###### Isto é um cabeçalho <h6>
```

Markdown também nos fornece duas maneiras alternativas de indicar h1 e h2.

```md
Isto é um cabeçalho h1
======================

Isto é um cabeçalho h2
----------------------
```

## Estilos de texto simples

O texto pode ser facilmente estilizado como itálico ou negrito usando Markdown.

```md
*Este texto está em itálico*
_E este também está._

**Este texto está em negrito**
__E este também está.__

***Este texto está em negrito e itálico.***
**_E este também está_**
*__Danou-se! Este também__*
```

No GitHub Flavored Markdown, que é usado para processar arquivos Markdown
GitHub, nós também temos:

```md
~~Este texto é processado com tachado.~~
```

## Parágrafos
Os parágrafos estão em uma ou várias linhas adjacentes de texto separadas por
uma ou múltiplas linhas em branco.

```md
Este é um parágrafo. Eu estou digitando em um parágrafo, não é legal?

Agora, eu estou no parágrafo 2.
Ainda continuo no parágrafo 2!


Eu estou no parágrafo três!
```

Se você quiser inserir uma tag HTML `<br />`, você pode acabar com um parágrafo
com dois ou mais espaços e, em seguida, começar um novo parágrafo.

```md
Termino com dois espaços (selecione essa linha para vê-los).  

Há um <br /> acima de mim!
```

Blocos de citações são fáceis e feitos com o caractere >.

```md
> Este é um bloco de citação. Você pode
> Quebrar manualmente suas linhas e colocar um `>` antes de cada linha ou você
> pode deixar suas linhas ficarem muito longas e quebrarem por conta própria.
> Não faz diferença, desde que elas comecem com um `>`.

> Você também pode usar mais de um nível
>> De recuo?
> O quão legal é isso?

```

## Listas

As listas não ordenadas podem ser feitas usando asteriscos, sinais de positivo
ou hífens.

```md
* Item
* Item
* Outro item

ou

+ Item
+ Item
+ Outro item

ou

- Item
- Item
- Um último item
```

Listas ordenadas são feitas com um número seguido por um ponto.

```md
1. Item um
2. Item dois
3. Item três
```

Você não precisa sequer rotular os itens corretamente e o Markdown ainda
assim deixará os itens em ordem, mas isso pode não ser uma boa idéia.

```md
1. Item um
1. Item dois
1. Item três
```

(Isto é processado da mesma forma que o exemplo acima)

Você também pode usar sublistas.

```md
1. Item um
2. Item dois
3. Item três
    * Sub-item
    * Sub-item
4. Item quatro
```

Existem também listas de tarefas. Isso cria checkboxes (caixas de seleção) do
HTML.

```md
As caixas abaixo sem o 'x' são checkboxes HTML desmarcadas.
- [ ] Primeira tarefa a completar.
- [ ] Segunda tarefa a completar
A caixa de seleção abaixo será exibida como uma checkbox HTML marcada.
- [x] Essa tarefa foi completa
```

## Blocos de código

Você pode indicar um bloco de código (que utiliza o elemento `<code>`) através
de indentação com quatro espaços ou uma tabulação.

```md
    Isto é código
    É assim, sacou?
```

Você pode também adicionar mais tabulações (ou adicionar mais quatro espaços
adicionais) para indentação no seu código.

```md
    my_array.each do |item|
      puts item
    end
```

Código embutido pode ser criado usando o caractere de crase `` ` ``.

```md
John não sabia nem o que a função `go_to()` fazia!
```

No GitHub Flavored Markdown, você pode usar uma sintaxe especial para código.

````md
```ruby
def foobar
  puts "Olá mundo!"
end
```
````

O texto acima não requer indentação, além disso o GitHub vai usar o destaque
de sintaxe da linguagem qeu você especificar após a tag <code>```</code>.

## Linha Horizontal

Linhas horizontais são facilmente adicionadas com três ou mais asteriscos ou
hífens, com ou sem espaços.

```md
***
---
- - -
****************
```

## Links

Uma das melhores coisas sobre o Mardkwon é o quão fácil é criar links.
Coloque o texto a ser exibido entre colchetes [] seguido pela url entre
parênteses ()

```md
[Clique aqui!](http://test.com/)
```

Você também pode adicionar um título ao link usando aspas dentro dos parênteses.

```md
[Cliqueaqui!](http://test.com/ "Link para Test.com")
```

Caminhos relativos funcionam também.

```md
[Ir para música](/música/).
```

O Markdown também suporta links para referências no texto.

```md
[Clique nesse link][link1] para mais informações!
[Também cheque esse link][foobar] se você quiser.

[link1]: http://test.com/ "Legal!"
[link2r]: http://foobar.biz/ "Certo!"
```

O título também pode estar entre aspas simples ou entre parênteses, ou omitido
inteiramente. As referências podem estar em qualquer lugar no documento e os
IDs de referência podem ser qualquer um, desde que eles sejam únicos.

Existe também a "nomeação implícita", que permite que você use o texto do link
como o id:

```md
[Isso][] é um link.

[Isso]: http://thisisalink.com/
```

Mas geralmente não são usados.

### Tabela de conteúdo

Alguns sabores de Markdown fazem inclusive uso de combinação de listas, links e
cabeçalhos de modo a criar uma tabela de conteúdo. Nesse caso, títulos em
cabeçalhos são precedidos de (`#`) e são usados como ids para links. Se o
cabeçalho consistir de várias palavras, elas serão conectadas por hífens (`-`),
que também substitui alguns caracteres especiais. (Embora alguns outros
carácteres especiais sejam omitidos.)

```md
- [Cabeçalho](#cabecalho)
- [Um outro cabeçalho](#outro-cabecalho)
- [Capítulo](#capitulo)
  - [Subcapítulo <h3 />](#subcapitulo-h3-)
```

De qualquer modo, esse é um recurso que talvez não funcione em todas as
implementações do Markdown da mesma forma.

## Imagens

As imagens são feitas da mesma forma que os links, mas com um ponto de
exclamação na frente!

```md
![Este é alt-text (texto alternativo) para minha imagem](http://imgur.com/myimage.jpg "Um título opcional")
```

E estilo de referência funciona como esperado

```md
![Esse é o alt-attribute.][myimage]

[Minha imagem]: relative/urls/cool/image.jpg "se precisar de um título, está aqui"
```

## Miscelânea

### Auto-links

```md
<http://testwebsite.com/> é equivalente a
[http://testwebsite.com/](http://testwebsite.com/)
```

### Auto-links para e-mails

```md
<foo@bar.com>
```

### Escapando caracteres

Quero digitar *Este texto entre asteriscos*, mas eu não quero que ele seja
em itálico, então eu faço o seguinte: \*Este texto entre asteriscos \*.

### Teclas do teclado

No GitHub Flavored Markdown, você pode usar a tag `<kbd>` para representar uma
tecla do teclado.

```md
Seu computador travou? Tente apertar
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```

### Tabelas

Tabelas estão disponíveis apenas no GitHub Flavored Markdown e são ligeiramente
complicadas, mas se você realmente quer:

```md
| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| esquerda-alin| Centrado | direita-alinh |
| blah         | blah     | blah          |
```

Ou, para os mesmos resultados

```md
Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh isso é tão feio | faça isto | parar
```

## Markdownlint

De modo a simplificar o trabalho com Markdown e padronizar estilo de código, o
`Markdownlint` foi criado. Essa ferramenta está disponível como plugin para
algumas interfaces de desenvolvimento (IDEs) e pode ser utilizada como um
utilitário para garantir validade e legibilidade do Markdown.

---

## Leitura complementar

Para mais informações, confira o post oficial de John Gruber sobre sintaxe [aqui](http://daringfireball.net/projects/markdown/syntax)
e o do cheatsheet do Adam Pritchard [aqui](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).

Se você quiser aprender mais sobre recursos de alguns sabores específicos de
Markdown, veja:

- [GitHub Flavored Markdown](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
- [GitLab Flavored Markdown](https://docs.gitlab.com/ee/user/markdown.html)
