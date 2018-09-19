---
language: Erlang
lang: es-es
contributors:
    ["Giovanni Cappellotto", "http://www.focustheweb.com/"]
translators:
  - ["Ernesto Pelayo", "http://github.com/ErnestoPelayo"]
filename: learnerlang.erl
---
# Erlang
%  Signo de porcentaje inicia un comentario de una línea.

%% Se usarán dos por ciento de caracteres para comentar funciones.

%%% Se usarán tres por ciento de caracteres para comentar los módulos.

### Utilizamos tres tipos de puntuación en Erlang.

+ **Comas (`,`)**  argumentos separados en llamadas a funciones, constructores de
datos y patrones.

+ **Periodos (`.`)** (seguido de espacios en blanco) separa funciones completas y
expresiones en el shell.

+  **Semicolons (`;`)** cláusulas separadas. Encontramos cláusulas en varios contextos: de definiciones de funciones y en **`case`**,**` if`**, **`try..catch`**, y **` receive`** de expresiones.

 ## 1.-Variables y coincidencia de patrones.


- En Erlang, las nuevas variables están vinculadas con una instrucción **`=`**.
>**Num = 42.**

- Todos los nombres de variables deben comenzar con una letra mayúscula.

- Erlang tiene variables de asignación única; si intentas asignar un diferente de valor a la variable **`Num`**, obtendrá un error.
Num = 43. **error de excepción**: no coincide con el valor del lado derecho 43

- En la mayoría de los idiomas, **`=`** denota una declaración de asignación. En Erlang, sin embargo,**`=`** denota una operación de coincidencia de patrones.

- Cuando se usa una variable vacía en el del lado izquierdo del operador `=` to está vinculado (asignado), pero cuando está atado variable se usa en el lado izquierdo, se observa el siguiente comportamiento.
>**`Lhs = Rhs`** realmente significa esto: evaluar el lado derecho (**` Rhs`**),  y luego coincide con el resultado contra el patrón en el lado izquierdo (**`Lhs`**).
>**Num = 7 * 6.**

- Número de punto flotante.
Pi = 3.14159.

- Los átomos se usan para representar diferentes valores constantes no numéricos.

- Átomos comienza con letras minúsculas, seguido de una secuencia de caracteres

- alfanuméricos de caracteres o el signo de subrayado (**`_`**) o en (**` @ `**).
>**Hola = hola.**
  **OtherNode = ejemplo @ nodo.**

- Los átomos con valores no alfanuméricos se pueden escribir al encerrar los átomos con apóstrofes.
>**AtomWithSpace = 'algún átomo con espacio'.**

+ Tuples son similares a las estructuras en C.
>**Point = {point, 10, 45}.**

- Si queremos extraer algunos valores de una tupla, usamos el patrón de coincidencia
 operador **`=`**.
> **{punto, X, Y} = Punto. % X = 10, Y = 45**

- Podemos usar **`_`** como marcador de posición para variables que no nos interesan.

- El símbolo **`_`** se llama una variable anónima. A diferencia de las variables  regulares,varias apariciones de `_` en el mismo patrón no tienen que vincularse a mismo valor.
>**Person = {person, {name, {first, joe}, {last, armstrong}}, {footsize, 42}}.**
**{_, {_, {_, who }, _}, _} = Persona. % Who = joe**

+ Creamos una lista al encerrar los elementos de la lista entre corchetes y separándolos con comas.

+ Los elementos individuales de una lista pueden ser de cualquier tipo.

- El primer elemento de una lista es el encabezado de la lista. Si te imaginas eliminar del encabezado de la lista, lo que queda se llama cola de la lista.
>**ThingsToBuy = [{manzanas, 10}, {peras, 6}, {leche, 3}].**

- Si `T` es una lista, entonces **` [H | T] `** también es una lista, con la cabeza **` H`** y la cola **`T`**.

+ La barra vertical (**`|`**) separa el encabezado de una lista de su cola.
  **`[]`** es la lista vacía.

+ Podemos extraer elementos de una lista con una operación de coincidencia de
  patrones. Si nosotros tiene una lista no vacía **`L`**, luego la expresión **` [X | Y] = L`**, donde **`X`** y **` Y`** son variables independientes, extraerán el encabezado de la lista en **`X`** y la cola de la lista en **`Y`**.
>**[FirstThing | OtherThingsToBuy] = ThingsToBuy.**
**FirstThing = {manzanas, 10}**
**OtherThingsToBuy = [{peras, 6}, {leche, 3}]**

+ No hay cadenas en Erlang. Las cadenas son realmente solo listas de enteros.

+ Las cadenas están entre comillas dobles (**`" `**).
>**Nombre = "Hola".
[72, 101, 108, 108, 111] = "Hola".**
