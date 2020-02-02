---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
    - ["Gabriele Luz", "https://github.com/gabrieleluz"]
    - ["Monique Baptista", "https://github.com/bfmonique"]

lang: pt-br    
filename: learnmarkdown-pt.md
---

Markdown foi criado por John Gruber in 2004. Originado para ser fácil de ler e 
escrever sintaxe que converte facilmente em HTML (hoje, suporta outros formatos também).

Dê-me feedback tanto quanto você quiser! / Sinta-se livre para fazer uma bifurcação (fork) e 
puxar o projeto (pull request)

## Elementos HTML
Markdown é um superconjunto do HTML, de modo que qualquer arvquivo HTML é 
um arquivo Markdown válido.
```md
<!-- Markdown é um superconjunto do HTML, de modo que qualquer arquivo HTML é 
um arquivo Markdown válido. Isso significa que nós podemos usar elementos HTML 
em Markdown, como o elemento de comentário, e eles não serão afetados pelo analisador
de remarcação. No entanto, se você criar um elemento HTML em seu arquivo Markdown, você
não pode usar sintaxe de remarcação dentro desse conteúdo do elemento.-->

<!--A maneira como o Markdown é analisado varia de software para software.
Este guia vai tentar esclarecer quando as características são universais, ou quando eles são específicos para um determinado interpretador -->


## Cabeçalhos

Você pode criar elementos HTML `<h1>` até `<h6>` facilmente antecedendo o texto
que deseja estar nesse elemento por um número de hashes (#)

# Isto é um cabeçalho <h1>
## Isto é um cabeçalho <h2>
### Isto é um cabeçalho <h3>
#### Isto é um cabeçalho <h4>
##### Isto é um cabeçalho <h5>
###### Isto é um cabeçalho <h6>
```

Markdown também nos fornece duas maneiras alternativas de indicar h1 e h2

```md
Isto é um cabeçalho h1
======================

Isto é um cabeçalho h2
----------------------
```

## Estilos de texto simples
O texto pode ser facilmente denominado como marcação itálico, negrito ou tachado usando:

```md
*Este texto está em itálico*
_E este também está._

**Este texto está em negrito**
__E este também está._

***Este texto está em negrito e itálico.***
**_E este também está_**
*--Danouse! Este também__*
```

Em GitHub Flavored Markdown, que é usado para processar arquivos Markdown 
GitHub, nós também temos:

```md
~~Este texto é processado com tachado.~~
```

## Parágrafos
Os parágrafos estão uma ou várias linhas adjacentes de texto separadas por 
uma ou múltiplas linhas em branco.

```md
Este é um parágrafo. Eu estou digitando em um parágrafo, não é legal?

Agora, eu estou no parágrafo 2.
... Ainda continuo no parágrafo 2! :)

Eu estou no parágrafo três.
```

Se você quiser inserir uma tag HTML `<br />`, você pode acabar com um parágrafo 
com dois ou mais espaços e, em seguida, começar um novo parágrafo

```md
Termino com dois espaços (destacar-me para vê-los). 

Há um <br /> acima de mim!
```


Bloco de citações são fáceis e feito com o caractere >.

```md
> Este é um bloco de citação. Você pode 
> Quebrar manualmente suas linhas e colocar um `>` antes de cada linha ou você pode
> deixar suas linhas ficarem muito longas e quebrarem por conta própria. Não faz diferença, 
> desde que eles começam com um `>`.
	

> Você também pode usar mais de um nível 
>> De recuo?
```

## Listas
As listas não ordenadas podem ser feitas usando asteriscos, positivos ou hífens

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

<!-- Você não tem poder para rotular os itens corretamente e a remarcação ainda deixará os 
itens em ordem, mas isso pode não ser uma boa idéia -->


```md
1. Item um
1. Item dois
1. Item três
```
(Isto é processado da mesma forma que o exemplo acima)

Você também pode usar sublistas

```md
1. Item um
2. Item dois
3. Item três
    * Sub-item
    * Sub-item
4. Item quatro
```

Existem também listas de tarefas. Isso cria checkboxes (caixas de seleção) de HTML

```md
As caixas abaixo sem o 'x' são checkboxes HTML desmarcadas
- [ ] Primeira tarefa a completar
- [ ] Segunda tarefa a completar
A caixa de seleção abaixo será exibida como uma checkbox HTML marcada
- [x] Essa tarefa foi completa

```


## Blocos de código
Você pode indicar um bloco de código (que utiliza o elemento `<code>`) pelo recuo
uma linha com quatro espaços ou uma guia
```md
	Isto é código
	É assim, sacou?
	
```

Você pode também re-guia (ou adicionar mais quatro espaços adicionais) para o recuo 
dentro do seu código

```md
	my_array.each do |item|
        puts item
    end	
 ```

Código embutido pode ser criada usando o caractere de crase `` ` ``

```md
John não sabia nem o que o função `goto()` fazia!
```

Em GitHub Flavored Markdown, você pode usar uma sintaxe especial para o código
```md
	``` ruby
	def foobar
    	puts "Hello world!"
	end
	```
```
=======
\`\`\`ruby <!-- exceto remover essas barras invertidas quando você faz isso, apenas ```
ruby! --> 
def foobar
    puts "Hello world!"
end
\`\`\` <!-- Aqui também, não use barras invertidas, apenas ``` -->

O texto acima não requer recuo, além disso o GitHub vai usar a sintaxe highlight da linguagem que você especificar após a \`\`\`.

## Linha Horizontal
Linhas horizontais são facilmente adicionados com três ou mais asteriscos ou hífens,
com ou sem espaços.

```md
***
---
- - - 
****************
```

## Links
Uma das melhores coisas sobre a marcação é o quão fácil é fazer ligações. Colocar 
o texto a ser exibido entre parênteses rígidos [] seguido pela url em parênteses ()

```md
[Click aqui!](http://test.com/)
```

Você também pode adicionar um título link usando aspas dentro dos parênteses

```md
[Click aqui!](http://test.com/ "Link para Test.com")
```

Caminhos relativos funcionam também.

```md
[Ir para música](/música/).
```

Markdown também suporta ligações de estilo de referência

```md
[Clique neste link] [link1] para mais informações sobre isso!
[Além disso, verifique este link] [foobar] se você quiser.

[link1]: http://test.com/ "Legal!"
[foobar]: http://foobar.biz/ "OK!"
```

O título também pode estar entre aspas simples ou entre parênteses, ou omitido 
inteiramente. As referências podem estar em qualquer lugar no documento e os IDs de referência 
pode ser qualquer um, desde que eles são únicos.

Existe também a "nomeação implicita", que permite que você use o texto do link como o id

```md
[Este] [] é um link. 

[este]: http://thisisalink.com/ 
```

Mas não são usados normalmente

## Imagens
As imagens são feitas da mesma forma que as ligações, mas com um ponto de exclamação na frente!

```md
![Este é pairar-texto (texto alternativo) para minha imagem](http://imgur.com/myimage.jpg "Um título opcional") 
```

E estilo de referência funciona como esperado

```md
![Este é o pairar-texto.][Myimage] 

[myimage]: relative/urls/legal/image.jpg "se você precisa de um título, é aqui" 
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

Quero digitar * Este texto entre asteriscos *, mas eu não quero que ele seja 
em itálico, então eu faço o seguinte: \*Este texto entre asteriscos \*.

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

Fim!

---
Para mais informações, confira o post oficial de John Gruber de sintaxe [aqui](http://daringfireball.net/projects/markdown/syntax)
e de Adam Pritchard grande cheatsheet [aqui](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
