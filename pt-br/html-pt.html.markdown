---
language: html
filename: learnhtml-br.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Robert Steed", "https://github.com/robochat"]
lang: pt-br
---

HTML é um acrônimo de HyperText Markup Language(Linguagem de Marcação de HiperTexto).
É uma linguagem que nos permite escrever páginas para a "world wide web".
É uma linguagem de marcação, nos permite escrever páginas na web usando código
para indicar como o texto e os dados serão ser exibidos.
De fato, arquivos HTML são simples arquivos de texto.
O que seria marcação? É um método de organização dos dados da página envolvidos
por abertura e fechamento de tags.
Essa marcação serve para dar significado ao texto que envolve.
Assim como outras linguagens, o HTML tem diversas versões. Aqui falaremos sobre o HTML5.

**NOTA :**  Você pode testar diferentes tags e elementos conforme progride os
tutoriais em sites como [codepen](http://codepen.io/pen/) podendo ver seus efeitos,
entendendo como funcionam e se familiarizando com a linguagem.
Esse artigo tem seu foco principal na sintaxe do HTML e algumas dicas úteis.


```html
<!-- Comentários são envolvidos conforme essa linha! -->

<!-- #################### As Tags #################### -->

<!-- Aqui está um exemplo de arquivo HTML que iremos analisar. -->

<!doctype html>
	<html>
		<head>
			<title>Meu Site</title>
		</head>
		<body>
			<h1>Olá, mundo!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">Venha ver como isso aparece</a>
			<p>Esse é um parágrafo.</p>
			<p>Esse é um outro parágrafo.</p>
			<ul>
				<li>Esse é um item de uma lista não enumerada (bullet list)</li>
				<li>Esse é um outro item</li>
				<li>E esse é o último item da lista</li>
			</ul>
		</body>
	</html>

<!-- Um arquivo HTML sempre inicia indicando ao navegador que é uma página HTML. -->
<!doctype html>

<!-- Após isso, inicia abrindo a tag <html>. -->
<html>

<!-- Essa tag deverá ser fechada ao final do arquivo com </html>. -->
</html>

<!-- Não deverá haver nada após o fechamento desta tag. -->

<!-- Entre a abertura e o fechamento das tags <html></html>, nós encontramos: -->

<!-- Um cabeçalho definido por <head> (deverá ser fechado com </head>). -->
<!-- O cabeçalho contém uma descrição e algumas informações adicionais que não serão exibidas; chamam-se metadados. -->

<head>
	<title>Meu Site</title><!-- Essa tag <title> indica ao navegador o título a ser exibido na barra de títulos e no nome da aba. -->
</head>

<!-- Após a seção <head>, nós encontramos a tag - <body> -->
<!-- Até esse ponto, nada descrito irá aparecer na janela do browser. -->
<!-- Nós deveremos preencher o body(corpo) com o conteúdo a ser exibido. -->

<body>
	<h1>Olá, mundo!</h1> <!-- A tag h1 cria um título. -->
  <!-- Há também subtítulos do <h1>, o mais importante, aos mais precisos (h6). -->
  <a href = "http://codepen.io/anon/pen/xwjLbZ">Venha ver o que isso exibe</a> <!-- Um hiperlink ao endereço preenchido no atributo href="" -->
	<p>Esse é um parágrafo.</p> <!-- A tag <p> permite incluir um texto na página. -->
	<p>Esse é um outro parágrafo.</p>
	<ul> <!-- A tag <ul> cria uma lista de marcação. -->
	<!-- Para criar uma lista ordenada, devemos usar <ol>, exibindo 1. para o primeiro elemento, 2. para o segundo, etc. -->
		<li>Esse é um item de uma lista não-enumerada.</li>
		<li>Esse é um outro item</li>
		<li>E esse é o último item da lista</li>
	</ul>
</body>

<!-- E é isso, criar um arquivo HTML pode ser bem simples. -->

<!-- Também é possível adicionar alguns outros tipos de tags HTML. -->

<!-- Para inserir uma imagem. -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- O caminho da imagem deve ser indicado usando o atributo src="" -->
<!-- O caminho da imagem pode ser uma URL ou até mesmo o caminho do arquivo no seu computador. -->

<!-- Também é possível criar uma tabela. -->

<table> <!-- Iniciamos a tabela com a tag <table>. -->
	<tr> <!-- <tr> nos permite criar uma linha. -->
		<th>Primeiro cabeçalho</th> <!-- <th> nos permite criar o título de uma coluna. -->
		<th>Segundo cabeçalho</th>
	</tr>
	<tr>
		<td>Primeira linha, primeira coluna</td> <!-- <td> nos permite criar uma célula da tabela. -->
		<td>Primeira linha, segunda coluna</td>
	</tr>
	<tr>
		<td>Segunda linha, primeira coluna</td>
		<td>Segunda linha, segunda coluna</td>
	</tr>
</table>

```

## Uso

HTML é escrito em arquivos com a extensão `.html` ou `.htm`. Seu mime type é `text/html`.

## Para aprender mais

* [wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
