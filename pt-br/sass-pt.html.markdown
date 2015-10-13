---
language: sass
filename: learnsass.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
translators:
  - ["Lucas Pugliesi Ferreira", "https://github.com/fplucas"]
lang: pt-br
---

Sass é uma extensão da linguagem CSS, que adiciona funcionalidades como variáveis, aninhamentos, misturas e mais.
Sass (e outros pré-processadores como o [Less](http://lesscss.org/) ajudam desenvolvedores a escrever um código conciso e sustentável.

No Sass há dois tipos diferentes de sintaxe para escolher. SCSS, o qual é a mesma sintaxe do CSS, porém com algumas funcionalidades do Sass. Ou Sass (sintaxe original), a qual usa identação ao invés de chaves e pontos e vírgulas.
Esse tutorial é escrito em SCSS.


```scss

	
//Comentários de uma linha são removidos quando o Sass é compilado para CSS.

/*Comentários de múltiplas linhas são preservados. */	
	
	
	
/*Variáveis
==============================*/
	
	

/* Você pode armazenar um valor de CSS (como uma cor) em uma variável.
Use  o símbolo '$' para criar uma variável. */
	
$cor-primaria: #A3A4FF;
$cor-secundaria: #51527F;
$fonte-do-corpo: 'Roboto', sans-serif;	

/* Você pode usar as variáveis ao longo da sua folha de estilo. 
Agora, se você quiser mudar uma cor, deve fazer a mudança apenas uma vez.*/	
	
body {
	background-color: $cor-primaria;
	color: $cor-secundaria;
	font-family: $fonte-do-corpo;
}

/* Isso será compilado em: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Isso é muito mais sustentável do que trocar a cor toda vez que aparecer em sua folha de estilo. */
	


/*Misturas
==============================*/



/* Se você acha que está escrevendo o mesmo código para mais de um elemento, você poderá querer salvá-lo em uma mistura (mixin).

Usando o comando '@mixin', aidcione um nome à sua mistura.*/

@mixin centro {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Você pode usar a mistura usando o comando '@include' e o nome da mistura. */

div {
	@include centro;
	background-color: $cor-primaria;
}

/*O qual compilará em: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}


/* Você também pode usar misturas para criar uma propriedade de rápida escrita. */

@mixin tamanho($largura, $altura) {
	width: $largura;
	height: $altura;
}
	
/*A qual você pode chamá-la passando seus argumentos de largura e altura. */

.retangulo {
	@include tamanho(100px, 60px);
}

.quadrado {
	@include tamanho(40px, 40px);
}

/* Isso será compilado em: */
.retangulo {
  width: 100px;
  height: 60px; 
}

.quadrado {
  width: 40px;
  height: 40px; 
}




/*Extensão (Herança)
==============================*/



/*Extensão é outra forma de compartilhas as propriedades de um seletor com outros. */

.mensagem {
	@include tamanho(5em, 5em);
	border: 5px solid $cor-secundaria;
}

.mensagem-sucesso {
	@extend .mensagem;
	border-color: #22df56;
}

/* Compilado para: */
.mensagem, .mensagem-successo {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F; 
}

.mensagem-successo {
  border-color: #22df56; 
}


	

/*Aninhamento
==============================*/



/*Sass permite você a aninhar alguns seletores com outros */

ul {
	list-style-type: none;
	margin-top: 2em;
	
	li {
		background-color: #FF0000;		
	}	
}

/* '&' será substituído pelo seletor pai. */
/* Você pode também aninhar pseudo-classes. */
/* Tenha em mente que utilizando muito aninhamento, seu código ficará menos sustentável.
Por exemplo: */

ul {
	list-style-type: none;
	margin-top: 2em;
	
	li {
		background-color: red;
		
		&:hover {
		  background-color: blue;
		}
		
		a {
		  color: white;
		}
	}	
}

/* Compilará para: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



	
```	



## SASS or Sass?
Você alguma vez já imaginou se Sass é um acrônimo ou não? Você provavelmente não pensou, mas eu te contarei mesmo assim. O nome da linguagem é uma palavra, "Sass", e não um acrônimo.
Pelo motivo das pessoas constantemente escrevem como "SASS", o criador da linguagem brincou chamando de "Syntactically Awesome StyleSheets", que significa "Folhas de estilos sintaticamente fantásticas". 


## Pratique Sass
Se você deseja brincar com Sass no seu navegador, dê uma olhada no [SassMeister](http://sassmeister.com/).
Você pode usar também a sintaxe, apenas indo nas configurações e selecionando Sass ou SCSS.

	
## Further reading
* [Official Documentation](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) fornece tutoriais (de iniciante a avançado) e artigos.
