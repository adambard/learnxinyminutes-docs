---
language: xml
filename: learnxml-pt.xml
contributors:
    - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
---

XML é uma linguagem de marcação projetada para armazenar e transportar dados.

Ao contrário de HTML, XML não especifica como exibir ou formatar os dados, 
basta carregá-lo.

* Sintaxe XML

```xml
<!-- Comentários em XML são feitos desta forma -->

<?xml version="1.0" encoding="UTF-8"?>
<livraria>
	<livro category="COZINHA">
		<titulo lang="en">Everyday Italian</titulo>
		<autor>Giada De Laurentiis</autor>
		<year>2005</year>
		<preco>30.00</preco>
	</livro>
	<livro category="CRIANÇAS">
		<titulo lang="en">Harry Potter</titulo>
		<autor>J K. Rowling</autor>
		<year>2005</year>
		<preco>29.99</preco>
	</livro>
	<livro category="WEB">
		<titulo lang="en">Learning XML</titulo>
		<autor>Erik T. Ray</autor>
		<year>2003</year>
		<preco>39.95</preco>
	</livro>
</livraria>

<!-- Um típico arquivo XML é mostrado acima.
	Ele começa com uma declaração, informando alguns metadados (opcional).
	
	XML usa uma estrutura de árvore. Acima, o nó raiz é "Livraria", que tem
	três nós filhos, todos os 'Livros'. Esses nós tem mais nós filhos, 
	e assim por diante...
	
	Nós são criados usando tags abre/fecha, filhos são justamente os nós que 
	estão entre estes nós. -->


<!-- XML traz dois tipos de dados:
	1 - Atributos -> Isso é metadados sobre um nó.
			Normalmente, o parser XML usa esta informação para armazenar os dados
			corretamente. Caracteriza-se por aparecer em parênteses dentro da tag 
			de abertura.
	2 - Elementos -> É dados puros.
			Isso é o que o analisador irá recuperar a partir do arquivo XML. 
			Elementos aparecem entre as tags de abertura e fechamento, 
			sem parênteses. -->
			
	
<!-- Abaixo, um elemento com dois atributos -->
<arquivo type="gif" id="4293">computer.gif</arquivo>


```

* Documento bem formatado x Validação

Um documento XML é bem formatado se estiver sintaticamente correto.No entanto,
é possível injetar mais restrições no documento, utilizando definições de 
documentos, tais como DTD e XML Schema.

Um documento XML que segue uma definição de documento é chamado válido, sobre 
esse documento.

Com esta ferramenta, você pode verificar os dados XML fora da lógica da aplicação.

```xml

<!-- Abaixo, você pode ver uma versão simplificada do documento livraria,
com a adição de definição DTD.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "livraria.dtd">
<livraria>
	<livro category="COOKING">
		<titulo >Everyday Italian</titulo>
		<preco>30.00</preco>
	</livro>
</livraria>

<!-- Este DTD poderia ser algo como:-->

<!DOCTYPE note
[
<!ELEMENT livraria (livro+)>
<!ELEMENT livro (titulo,preco)>
<!ATTLIST livro category CDATA "Literature">
<!ELEMENT titulo (#PCDATA)>
<!ELEMENT preco (#PCDATA)>
]>


<!-- O DTD começa com uma declaração.
	Na sequência, o nó raiz é declarado, o que requer uma ou mais crianças nós 
	'Livro'. Cada 'Livro' deve conter exatamente um 'titulo' e um 'preco' e um 
	atributo chamado "categoria", com "Literatura", como o valor padrão.
	Os nós "título" e "preço" contêm um conjunto de dados de caráter analisados.-->

<!-- O DTD poderia ser declarado dentro do próprio arquivo XML .-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT livraria (livro+)>
<!ELEMENT livro (titulo,preco)>
<!ATTLIST livro category CDATA "Literature">
<!ELEMENT titulo (#PCDATA)>
<!ELEMENT preco (#PCDATA)>
]>

<livraria>
	<livro category="COOKING">
		<titulo >Everyday Italian</titulo>
		<preco>30.00</preco>
	</livro>
</livraria>
```
