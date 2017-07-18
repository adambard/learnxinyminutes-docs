---

category: tool
tool: amd
contributors:
    - ["Frederik Ring", "https://github.com/m90"]

translators:
    - ["Damaso Sanoja", "https://github.com/damasosanoja"]
filename: learnamd-es.js
lang: es-es
---

## Iniciando con AMD

El API del **Módulo de Definición Asíncrono** especifica un mecanismo para definir módulos JavaScript de manera tal que tanto el módulo como sus dependencias puedan ser cargadas de manera asíncrona. Esto es particularmente adecuado para el entorno del navegador donde la carga sincronizada de los módulos genera problemas de rendimiento, usabilidad, depuración y acceso de multi-dominios.

### Conceptos básicos
```javascript
// El API básico de AMD consiste en tan solo dos métodos: `define` y `require`
// y se basa en la definición y consumo de los módulos:
// `define(id?, dependencias?, fábrica)` define un módulo
// `require(dependencias, callback)` importa un conjunto de dependencias y
// las consume al invocar el callback

// Comencemos usando define para definir un nuevo módulo
// que no posee dependencias. Lo haremos enviando un nombre
// y una función fábrica para definirla:
define('awesomeAMD', function(){
  var isAMDAwesome = function(){
    return true;
  };
  // El valor que regresa la función fábrica del módulo será
  // lo que los otros módulos o llamados require recibirán cuando
  // soliciten nuestro módulo `awesomeAMD`.
  // El valor exportado puede ser cualquier cosa, funciones (constructores),
  // objetos, primitivos, incluso indefinidos (aunque eso no ayuda mucho).
  return isAMDAwesome;
});

// Ahora definamos otro módulo que dependa de nuestro módulo `awesomeAMD`.
// Observe que ahora hay un argumento adicional que define 
// las dependencias de nuestro módulo:
define('loudmouth', ['awesomeAMD'], function(awesomeAMD){
  // las dependencias serán enviadas a los argumentos de la fábrica
  // en el orden que sean especificadas
  var tellEveryone = function(){
    if (awesomeAMD()){
      alert('This is sOoOo rad!');
    } else {
      alert('Pretty dull, isn\'t it?');
    }
  };
  return tellEveryone;
});

// Como ya sabemos utilizar define usemos ahora `require` para poner en marcha
//  nuestro programa. La firma de `require` es `(arrayOfDependencies, callback)`.
require(['loudmouth'], function(loudmouth){
  loudmouth();
});

// Para hacer que este tutorial corra código, vamos a implementar una
// versión muy básica (no-asíncrona) de AMD justo aquí:
function define(name, deps, factory){
  // observa como son manejados los módulos sin dependencias
  define[name] = require(factory ? deps : [], factory || deps);
}

function require(deps, callback){
  var args = [];
  // primero recuperemos todas las dependencias que necesita
  // el llamado require
  for (var i = 0; i < deps.length; i++){
    args[i] = define[deps[i]];
  }
  // satisfacer todas las dependencias del callback
  return callback.apply(null, args);
}
// puedes ver este código en acción aquí: http://jsfiddle.net/qap949pd/
```

### Uso en el mundo real con require.js

En contraste con el ejemplo introductorio, `require.js` (la librería AMD más popular) implementa la **A** de **AMD**, permitiéndote cargar los módulos y sus dependencias asincrónicamente via XHR:

```javascript
/* file: app/main.js */
require(['modules/someClass'], function(SomeClass){
  // el callback es diferido hasta que la dependencia sea cargada
  var thing = new SomeClass();
});
console.log('So here we are, waiting!'); // esto correrá primero
```

Por convención, usualmente guardas un módulo en un fichero. `require.js` puede resolver los nombres de los módulos basados en rutas de archivo, de forma que no tienes que nombrar tus módulos, simplemente referenciarlos usando su ubicación. En el ejemplo `someClass` asumimos que se ubica en la carpeta `modules`, relativa a tu `baseUrl` configurada:

* app/
  * main.js
  * modules/
    * someClass.js
    * someHelpers.js
    * ...
  * daos/
    * things.js
    * ...

Esto significa que podemos definir `someClass` sin especificar su id de módulo:

```javascript
/* file: app/modules/someClass.js */
define(['daos/things', 'modules/someHelpers'], function(thingsDao, helpers){
  // definición de módulo, por supuesto, ocurrirá también asincrónicamente
  function SomeClass(){
    this.method = function(){/**/};
    // ...
  }
  return SomeClass;
});
```

Para alterar el comportamiento del mapeo de ruta usa `requirejs.config(configObj)` en tu `main.js`:

```javascript
/* file: main.js */
requirejs.config({
  baseUrl : 'app',
  paths : {
    // también puedes cargar módulos desde otras ubicaciones
    jquery : '//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
});
require(['jquery', 'coolLibFromBower', 'modules/someHelpers'], function($, coolLib, helpers){
  // un fichero `main` necesita llamar a require al menos una vez,
  // de otra forma jamás correrá el código
  coolLib.doFancyStuffWith(helpers.transform($('#foo')));
});
```
Las aplicaciones basadas en `require.js` usualmente tendrán un solo punto de entrada (`main.js`) que se pasa a la etiqueta del script `require.js` como un atributo de datos. Será cargado y ejecutado automáticamente al cargar la página:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Cien etiquetas de script? Nunca más!</title>
</head>
<body>
  <script src="require.js" data-main="app/main"></script>
</body>
</html>
```

### Optimizar todo un proyecto usando r.js

Muchas personas prefieren usar AMD para la organización del código durante el desarrollo, pero quieren enviar para producción un solo fichero en vez de ejecutar cientos de XHRs en las cargas de página.

`require.js` incluye un script llamado `r.js` (el que probablemente correrás en node.js, aunque Rhino también es soportado) que puede analizar el gráfico de dependencias de tu proyecto, y armar un solo fichero que contenga todos tus módulos (adecuadamente nombrados), minificado y listo para consumo.

Instálalo usando `npm`:
```shell
$ npm install requirejs -g
```

Ahora puedes alimentarlo con un fichero de configuración:
```shell
$ r.js -o app.build.js
```

Para nuestro ejemplo anterior el archivo de configuración luciría así:
```javascript
/* file : app.build.js */
({
  name : 'main', // nombre del punto de entrada
  out : 'main-built.js', // nombre del fichero donde se escribirá la salida
  baseUrl : 'app',
  paths : {
    // `empty:` le dice a r.js que esto aún debe ser cargado desde el CDN, usando
    // la ubicación especificada en `main.js`
    jquery : 'empty:',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
})
```

Para usar el fichero creado en producción, simplemente intercambia `data-main`:
```html
<script src="require.js" data-main="app/main-built"></script>
```

Un increíblemente detallado [resumen de opciones de generación](https://github.com/jrburke/r.js/blob/master/build/example.build.js) está disponible en el repositorio de GitHub.

### Temas no cubiertos en este tutorial
* [Cargador de plugins / transformaciones](http://requirejs.org/docs/plugins.html)
* [Cargando y exportando estilos CommonJS](http://requirejs.org/docs/commonjs.html)
* [Configuración avanzada](http://requirejs.org/docs/api.html#config)
* [Configuración de Shim (cargando módulos no-AMD)](http://requirejs.org/docs/api.html#config-shim)
* [Cargando y optimizando CSS con require.js](http://requirejs.org/docs/optimization.html#onecss)
* [Usando almond.js para construcciones](https://github.com/jrburke/almond)

### Otras lecturas:

* [Especificaciones oficiales](https://github.com/amdjs/amdjs-api/wiki/AMD)
* [¿Por qué AMD?](http://requirejs.org/docs/whyamd.html)
* [Definición Universal de Módulos](https://github.com/umdjs/umd)

### Implementaciones:

* [require.js](http://requirejs.org)
* [dojo toolkit](http://dojotoolkit.org/documentation/tutorials/1.9/modules/)
* [cujo.js](http://cujojs.com/)
* [curl.js](https://github.com/cujojs/curl)
* [lsjs](https://github.com/zazl/lsjs)
* [mmd](https://github.com/alexlawrence/mmd)
