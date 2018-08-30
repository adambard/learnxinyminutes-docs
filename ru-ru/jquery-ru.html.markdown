---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
translators:
    - ["Ev Bogdanov", "https://github.com/evbogdanov"]
lang: ru-ru
filename: jquery-ru.js
---

jQuery — это библиотека JavaScript, которая помогает "делать больше, писать меньше". Она выполняет множество типичных JavaScript-задач, упрощая написание кода. jQuery используется крупными компаниями и разработчиками со всего мира. Она упрощает и ускоряет работу с AJAX, с событиями, с DOM и со многим другим.

Поскольку jQuery является библиотекой JavaScript, сначала вам следует [изучить JavaScript](https://learnxinyminutes.com/docs/ru-ru/javascript-ru/).

```js


///////////////////////////////////
// 1. Селекторы

// Для получения элемента в jQuery используются селекторы
var page = $(window); // Получить страницу целиком

// В качестве селектора может выступать CSS-селектор
var paragraph = $('p'); // Получить все <p> элементы
var table1 = $('#table1'); // Получить элемент с идентификатором 'table1'
var squares = $('.square'); // Получить все элементы с классом 'square'
var square_p = $('p.square') // Получить <p> элементы с классом 'square'

```
