---
category: tool
tool: amd
contributors:
    - ["Frederik Ring", "https://github.com/m90"]
translators:
    - ["Felipe Tarijon", "http://nanoincub.com/"]
lang: pt-br
filename: learnamd-pt.js
---

## Começando com AMD

A API de Definição de Módulos Assíncrona **Asynchronous Module Definition** 
especifica um mecanismo para definição de módulos em JavaScript para os quais o
módulo e suas dependências podem ser carregados de forma assíncrona. Isso é
particularmente bem adequado para o ambiente do browser onde o carregamento de
módulos de forma síncrona fica sujeito a problemas de performance, usabilidade,
debugging e problemas de acesso em requisições cross-domain.

### Conceito básico
```javascript
// O básico da API de AMD consiste de nada mais que dois métodos: `define` e `require`
// e isso é tudo sobre a definição de módulo e consumo:
// `define(id?, dependências?, factory)` define um módulo
// `require(dependências, callback)` importa uma série de dependências e
// consome elas no callback passado como parâmetro.

// Vamos começar usando o define para definir um novo módulo
// que não tem dependências. Nós vamos fazer isso passando um nome
// e uma função factory para definir:
define('awesomeAMD', function(){
  var isAMDAwesome = function(){
    return true;
  };
  // O valor retornado da função de factory do módulo é
  // o que os outros módulos ou chamadas de require irão
  // receber quando requisitarem nosso módulo `awesomeAMD`.
  // O valor exportado pode ser qualquer coisa, (construtor) funções,
  // objetos, primitives, até mesmo undefined (apesar de que não irão ajudar muito).
  return isAMDAwesome;
});

// Agora, vamos definir outro módulo que depende do nosso módulo `awesomeAMD`.
// Perceba que existe um argumento adicional definindo nossas dependências do
// módulo agora:
define('loudmouth', ['awesomeAMD'], function(awesomeAMD){
  // dependências serão passadas como argumentos da factory
  // na ordem que elas forem especificadas
  var tellEveryone = function(){
    if (awesomeAMD()){
      alert('Isso é tãaaao loko!');
    } else {
      alert('Bem estúpido, né não?');
    }
  };
  return tellEveryone;
});

// Agora que nós sabemos como usar o define, vamos usar o `require` para
// começar nosso programa. A assinatura do `require` é `(arrayDedependências, callback)`.
require(['loudmouth'], function(loudmouth){
  loudmouth();
});

// Para fazer esse tutorial executável, vamos implementar uma versão muito básica
// (não-assíncrona) de AMD bem aqui nesse lugar:
function define(nome, deps, factory){
  // perceba como os módulos sem dependências são manipulados
  define[nome] = require(factory ? deps : [], factory || deps);
}

function require(deps, callback){
  var args = [];
  // primeiro vamos recuperar todas as dependências necessárias
  // pela chamada requerida
  for (var i = 0; i < deps.length; i++){
    args[i] = define[deps[i]];
  }
  // corresponder todas as dependências da função de callback
  return callback.apply(null, args);
}
// você pode ver esse código em ação aqui: http://jsfiddle.net/qap949pd/
```

### Uso na vida real com require.js

Em contraste com o exemplo introdutório, `require.js` (a biblioteca mais popular de AMD) na verdade implementa o **A** do **AMD**, permitindo que você carregue os módulos e suas
dependências via XHR:

```javascript
/* file: app/main.js */
require(['modules/algumaClasse'], function(AlgumaClasse){
  // o callback é deferido até que a dependencia seja carregada
  var coisa = new AlgumaClasse();
});
console.log('Então aqui estamos nós, esperando!'); // isso vai rodar primeiro
```

Por convenção, você geralmente guarda um módulo em um arquivo. `require.js` pode resolver nome de módulos baseado no caminho das pastas, então você não precisa nomear os seus módulos, mas sim simplesmente referenciar eles usando sua origem. No exemplo `algumaClasse` é adotado a pasta `modules`, relativa a configuração da sua `baseUrl`:

* app/
  * main.js
  * modules/
    * algumaClasse.js
    * algunsHelpers.js
    * ...
  * daos/
    * coisas.js
    * ...

Isso significa que nós podemos definir `algumaClasse` sem especificar o id de um módulo:

```javascript
/* arquivo: app/modules/algumaClasse.js */
define(['daos/coisas', 'modules/algunsHelpers'], function(coisasDao, helpers){
  // definição de módulo, claro, irá acontecer também de forma assíncrona
  function AlgumaClasse(){
    this.metodo = function(){/**/};
    // ...
  }
  return AlgumaClasse;
});
```
Para alterar o comportamento padrão de mapeamento de caminho de pastas utilize
`requirejs.config(configObj)` em seu `main.js`:

```javascript
/* arquivo: main.js */
requirejs.config({
  baseUrl : 'app',
  paths : {
    // você pode também carregar módulos de outros locais
    jquery : '//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
});
require(['jquery', 'coolLibFromBower', 'modules/algunsHelpers'], function($, coolLib, helpers){
  // um arquivo `main` precisa chamar o require pelo menos uma vez,
  // caso contrário, o código jamais rodará
  coolLib.facaAlgoDoidoCom(helpers.transform($('#foo')));
});
```
Apps baseados em `require.js` geralmente terão u´m único ponto de acesso (`main.js`) que é passado à tag script do `require.js` como um data-attribute. Ele vai ser automaticamente carregado e executado com o carregamento da página:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Umas 100 tags de script? Nunca mais!</title>
</head>
<body>
  <script src="require.js" data-main="app/main"></script>
</body>
</html>
```

### Otimizando um projeto inteiro utilizando r.js

Muitas pessoas preferem usar AMD para sanar a organização do código durante o desenvolvimento, mas continuam querendo colocar um único arquivo de script em produção ao invés de realizarem centenas de requisições XHRs no carregamento da página.

`require.js` vem com um script chamado `r.js` (que você vai provavelmente rodar em node.js, embora Rhino suporte também) que você pode analisar o gráfico de dependências de seu projeto, e fazer em um único arquivo contendo todos os seus módulos (corretamente nomeados), minificados e prontos para serem consumidos.

Instale-o utilizando `npm`:
```shell
$ npm install requirejs -g
```

Agora você pode alimentá-lo com um arquivo de configuração:
```shell
$ r.js -o app.build.js
```

Para o nosso exemplo acima a configuração pode ser essa:
```javascript
/* file : app.build.js */
({
  name : 'main', // nome do ponto de acesso
  out : 'main-built.js', // nome o arquivo para gravar a saída
  baseUrl : 'app',
  paths : {
    // `empty:` fala para o r.js que isso ainda deve ser baixado da CDN, usando
    // o local especificado no `main.js`
    jquery : 'empty:',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
})
```

Para usar o arquivo gerado, em produção, simplesmente troque o `data-main`:
```html
<script src="require.js" data-main="app/main-built"></script>
```

Uma incrível e detalhada visão geral [de build options](https://github.com/jrburke/r.js/blob/master/build/example.build.js) está disponível no repositório do GitHub.

### Tópicos não abordados nesse tutorial
* [Plugins de carregamento / transforms](http://requirejs.org/docs/plugins.html)
* [CommonJS style carregamento e exportação](http://requirejs.org/docs/commonjs.html)
* [Configuração avançada](http://requirejs.org/docs/api.html#config)
* [Shim configuration (carregando módulos sem AMD)](http://requirejs.org/docs/api.html#config-shim)
* [Carregando e otimizando CSS com require.js](http://requirejs.org/docs/optimization.html#onecss)
* [Usando almond.js para builds](https://github.com/jrburke/almond)

### Outras leituras:

* [Especificação oficial](https://github.com/amdjs/amdjs-api/wiki/AMD)
* [Por quê AMD?](http://requirejs.org/docs/whyamd.html)
* [Universal Module Definition](https://github.com/umdjs/umd)

### Implementações:

* [require.js](http://requirejs.org)
* [dojo toolkit](http://dojotoolkit.org/documentation/tutorials/1.9/modules/)
* [cujo.js](http://cujojs.com/)
* [curl.js](https://github.com/cujojs/curl)
* [lsjs](https://github.com/zazl/lsjs)
* [mmd](https://github.com/alexlawrence/mmd)
