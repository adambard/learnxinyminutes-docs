---
category: Algorithms & Data Structures
name: Asymptotic Notation
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["Gerson Lázaro", "https://gersonlazaro.com"]
lang: es-es
---

# Notaciones asintóticas

## ¿Qué son?

Las notaciones asintóticas son lenguajes que nos permitan analizar el tiempo de 
ejecución de un algoritmo identificando su comportamiento si el tamaño de 
entrada para el algoritmo aumenta. Esto también se conoce como la tasa de 
crecimiento de un algoritmo. ¿El algoritmo de repente se vuelve increíblemente 
lento cuando el tamaño de entrada crece? ¿Tiende a mantener un rápido tiempo de 
ejecución a medida que el tamaño de entrada aumenta? La notación asintótica nos 
da la capacidad para responder a estas preguntas.

## ¿Hay alternativas que respondan a estas preguntas?

Una manera sería contar el número de operaciones primitivas en diferentes 
tamaños de entrada. Aunque esta es una solución válida, la cantidad de trabajo 
que esto conlleva, incluso para los algoritmos simples, no justifica su uso.

Otra manera es medir físicamente la cantidad de tiempo que un algoritmo toma 
para completar su ejecución dados diferentes tamaños de entrada. Sin embargo, 
la exactitud y la relatividad (los tiempos obtenidos sólo serían relativos a la 
máquina sobre la cual se calcularon) de este método está ligado a variables 
ambientales tales como especificaciones de hardware, capacidad de procesamiento,
etc.

## Tipos de Notación Asintótica

En la primera sección de este documento hemos descrito cómo una notación 
asintótica identifica el comportamiento de un algoritmo ante los cambios en el 
tamaño de la entrada. Imaginemos un algoritmo como una función f, con tamaño de 
entrada n, y f(n) siendo el tiempo de ejecución. Así que para un algoritmo f 
dado, con el tamaño de entrada n obtenemos algún tiempo de ejecución resultante 
f(n). Esto resulta en un gráfico donde el eje Y es el tiempo de ejecución, el 
eje X es el tamaño de la entrada y los puntos en el gráfico son los resultantes 
de la cantidad de tiempo para un tamaño de entrada dado.

Puedes etiquetar una función, o un algoritmo, con una notación asintótica de 
muchas maneras diferentes. Algunos ejemplos son describir un algoritmo por su 
mejor caso, su peor caso, o el caso promedio. Lo más común es analizar un 
algoritmo por su peor caso. Por lo general, no se evalúa el mejor caso, porque 
no planeas el algoritmo para estas condiciones. Un muy buen ejemplo de esto son
los algoritmos de ordenamiento; específicamente, añadir elementos a un árbol. 
El mejor caso para la mayoría de los algoritmos podría ser tan bajo como una 
sola operación. Sin embargo, en la mayoría de los casos, el elemento que está 
añadiendo tendrá que ser ordenado adecuadamente a través del árbol, lo que 
podría significar examinar toda una rama. Este es el peor de los casos, y 
para estos casos es que planeamos el algoritmo.


### Tipos de funciones, límites, y simplificación

```
Función logarítmica - log n
Función lineal - an + b
Función cuadrática - an^2 + bn + c
Función polinomicas - an^z + . . . + an^2 + a*n^1 + a*n^0, donde z es constante
Función exponencial - a^n, donde a es constante
```

Estas son algunas clasificaciones de funciones de crecimiento básicos utilizados
en varias notaciones. La lista comienza en la función de crecimiento menor
(logarítmica, el tiempo de ejecución mas rápido) y pasa a la de mayor 
crecimiento  (exponencial, el tiempo de ejecución mas lento). Observe como al 
crecer 'n', o la entrada, en cada una de estas funciones, el resultado aumenta 
claramente mucho más rápido en las cuadráticas, polinómicas y exponenciales, 
en comparación con las logarítmicas y lineales.

Una anotación muy importante es que en las notaciones que se discutirán debes 
hacer tu mejor esfuerzo por utilizar los términos más simples. Esto significa 
hacer caso omiso de las constantes y terminos de orden inferior, porque a medida
que el tamaño de entrada (o n en f(n)) aumenta hacia el infinito (límites 
matemáticos), los términos y constantes de orden inferior se vuelven de poca o 
ninguna importancia. Dicho esto, si tienes constantes que son 2^9001, 
o alguna otra cantidad ridícula, inimaginable, te daras cuenta de que la 
simplificación sesgará la exactitud de la notación.

Como queremos algo simplificado, vamos a modificarlo un poco...

```
Logarítmico - log n
Lineal - n
Cuandrático - n^2
Polinómico - n^z, donde z es constante
Exponencial - a^n, donde a es constante
```

### O-grande (Big-O)
O-grande (Big-O), comúnmente escrito como O, es una notación asintótica para el 
peor caso, o el techo de crecimiento para una función determinada. Si `f (n)` 
es el tiempo de ejecución del algoritmo, y `g (n)` es un tiempo de complejidad 
arbitraria que relacionas con el algoritmo, entonces `f (n)` es O(g(n)), si por 
cualquier constante real c (c > 0), `f (n)` <= `c g(n)` para cada tamaño de 
entrada n (n > 0 ).


*Ejemplo 1*  

```
f(n) = 3log n + 100  
g(n) = log n
```

`f(n)` es O(g(n))?  
`3 log n + 100` es O(log n)?  
Echemos un vistazo a la definición de O-grande.

```
3log n + 100 <= c * log n  
```
¿Hay alguna constante c que satisface esto para todo n? 

```
3log n + 100 <= 150 * log n, n > 2 (indefinido en n = 1)  
```

¡Sí! La definición de O-grande se cumple, por lo tanto `f (n)` es O(g(n)).

*Ejemplo 2*  

```
f(n) = 3*n^2  
g(n) = n
```

`f(n)` es O(g(n))?  
`3 * n^2` es O(n)?  
Echemos un vistazo a la definición de O-grande.

```
3 * n^2 <= c * n  
```

¿Hay alguna constante c que satisface esto para todo n? 
No, no la hay. `f(n)` no es O(g(n)).

### Big-Omega
Big-Omega, comunmente escrito como Ω, es una notación asintótica para el mejor
caso, o el piso en el crecimiento para una función dada.

`f(n)` es Ω(g(n)), si para cualquier constante real c (c > 0), 
`f(n)` es >= `c g(n)` para cualquier tamaño de entrada n (n > 0).

No dudes en dirigirte a los recursos adicionales para ejemplos sobre esto. 
O-grande es la notación principal utilizada para la complejidad general de
tiempo algoritmico.

### Notas finales
Es difícil mantener este tipo de tema corto, y sin duda deberias revisar los 
libros y recursos en línea en la lista. Entran en mucha mayor profundidad con 
definiciones y ejemplos. 

## Libros

* [Algoritmos (Algorithms)](http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X)
* [Diseño de algoritmos (Algorithm Design)](http://www.amazon.com/Algorithm-Design-Foundations-Analysis-Internet/dp/0471383651)

## Recursos Online

* [MIT](http://web.mit.edu/16.070/www/lecture/big_o.pdf)
* [KhanAcademy](https://www.khanacademy.org/computing/computer-science/algorithms/asymptotic-notation/a/asymptotic-notation)
* [Apuntes Facultad de Ingeniería](https://www.scribd.com/document/317979564/Apuntes-Sobre-Analisis-de-Algoritmos)
