---
language: Pug
contributors:
  - ["Michael Warner", "https://github.com/MichaelJGW"]
filename: index-pt.pug
translators:
  - ["Adaías Magdiel", "https://adaiasmagdiel.com/"]
lang: pt-br
---

Pug é uma pequena linguagem que compila para HTML. Possui uma sintaxe limpa
com algumas funcionalidades adicionais, como declarações if e loops. Também pode ser utilizada
como uma linguagem de templates no lado do servidor para tecnologias como o Node.js.

```pug
//- Comentário de uma linha

//- Comentário de
    várias linhas

//- ---TAGS---
//- Básico
div
//- <div></div>
h1
//- <h1></h1>
minha-propriaTag
//- <minha-propriaTag></minha-propriaTag>

//- Tags irmãs
div
div
//- <div></div>
    <div></div>

//- Tags aninhadas
div
  div
//- <div>
      <div></div>
    </div>

//- Textos
h1 Olá, pessoas
//- <h1>Olá, pessoas</h1>

//- Texto de várias linhas
div.
  Oi,
  tudo bem?
//- <div>
      Oi,
      tudo bem?
    </div>

//- ---ATRIBUTOS---
div(class="minha-class" id="meu-id" meu-proprio-atributo="data" enabled)
//- <div class="minha-class" id="meu-id" meu-proprio-atributo="data" enabled></div>

//- Abreviações
span.minha-class
//- <span class="minha-class"></span>
.minha-class
//- <div class="minha-class"></div>
div#meu-id
//- <div id="meu-id"></div>
div#meu-id.minha-class
//- <div class="minha-class" id="meu-id"></div>


//- ---JAVASCRIPT---
- const lang = "pug";

//- Javascript em várias linhas
-
  const lang = "pug";
  const awesome = true;

//- Classes com Javascript
- const myClass = ['class1', 'class2', 'class3']
div(class=myClass)
//- <div class="class1 class2 class3"></div>

//- Estilos com Javascript
- const myStyles = {'color':'white', 'background-color':'blue'}
div(styles=myStyles)
//- <div styles="{&quot;color&quot;:&quot;white&quot;,&quot;background-color&quot;:&quot;blue&quot;}"></div>

//- Atributos com Javascript
- const myAttributes = {"src": "photo.png", "alt": "My Photo"}
img&attributes(myAttributes)
//- <img src="photo.png" alt="My Photo">
- let disabled = false
input(type="text" disabled=disabled)
//- <input type="text">
- disabled = true
input(type="text" disabled=disabled)
//- <input type="text" disabled>

//- Templates com Javascript
- const name = "Bob";
h1 Olá, #{name}
h1= name
//- <h1>Olá, Bob</h1>
//- <h1>Bob</h1>

//- ---LOOPS---

//- 'each' e 'for' tem a mesma função, aqui nós usaremos apenas 'each'.

each value, i in [1,2,3]
  p=value
//-
  <p>1</p>
  <p>2</p>
  <p>3</p>

each value, index in [1,2,3]
  p=value + '-' + index
//-
  <p>1-0</p>
  <p>2-1</p>
  <p>3-2</p>

each value in []
  p=value
//-

each value in []
  p=value
else
  p Sem valores

//- <p>Sem valores</p>

//- ---CONDICIONAIS---

- const number = 5
if number < 5
  p o número é menor do que 5
else if number > 5
  p o número é maior do que 5
else
  p o número é 5
//- <p>o número é 5</p>

- const orderStatus = "Aguardando";
case orderStatus
  when "Aguardando"
    p.warn Seu pedido está em espera
  when "Completado"
    p.success Seu pedido foi completado.
  when -1
    p.error Ocorreu algum erro
  default
    p Nenhum registro de pedido encontrado
//- <p class="warn">Seu pedido está em espera</p>

//- --INCLUINDO CONTEÚDOS--
//- Caminho do arquivo -> "includes/nav.pug"
h1 Indústrias ACME
nav
  a(href="index.html") Início
  a(href="about.html") Sobre Nós

//- Caminho do arquivo -> "index.pug"
html
  body
    include includes/nav.pug
//-
  <html>
    <body>
      <h1>Indústrias ACME</h1>
      <nav><a href="index.html">Início</a><a href="about.html">Sobre Nós</a></nav>
    </body>
  </html>

//- Importando Javascript e CSS
script
  include scripts/index.js
style
  include styles/theme.css

//- ---MIXIN---
mixin basic
  div Olá
+basic
//- <div>Olá</div>

mixin comment(nome, comentario)
  div
    span.comment-name= nome
    div.comment-text= comentario
+comment("Gil", "Tudo é divino, tudo é maravilhoso")
//-
  <div>
    <span class="comment-name">Gil</span>
    <div class="comment-text">Tudo é divino, tudo é maravilhoso</div>
  </div>
```

### Saiba Mais

- [Site Oficial](https://pugjs.org/)
- [Documentação](https://pugjs.org/api/getting-started.html)
- [Repositório no GitHub](https://github.com/pugjs/pug)
