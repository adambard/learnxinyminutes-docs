---
language: wolfram
lang: es-es
contributors:
    - ["Daniel Caballero", "http://github.com/danielcaballero796/"]
filename: learnwolfram-es.md
---

Wolfram es un lenguaje subyacente originalmente utilizado en Mathematica, pero ahora esta disponible para su uso en múltiples contextos.

El lenguaje de Wolfram tiene varias interfaces:
* La interfaz de línea de comandos kernel de Raspberry Pi (recién llamada _The Wolfram Language_) que se ejecuta interactivamente y no puede producir entrada gráfica.
* _Mathematica_ que es un texto rico / editor de matemáticas con Wolfram interactivo construido: presionando shift + Return en una "celda de código" crea una celda de salida con el resultado, que no es dinámica
* _Wolfram Workbench_ la cual es la interfaz de Eclipse del lenguaje Wolfram

El código de este ejemplo se puede escribir en cualquier interfaz y editarlo con Wolfram Workbench. Cargar directamente en Matematica puede resultar incómodo porque el archivo no contiene información de formato de celda (lo que haría que el archivo sea un desastre enorme para ser leído como texto) - puede ser visto / editado pero tal vez requerira algún ajuste.

```
(* Esto es un comentario *)

(* En Mathematica en lugar de utilizar estos comentarios, puede crear una celda de texto
   Y anotar su código con texto e imágenes bien escritas *)

(* Escribe una expresión devuelve el resultado *)
2*2              (* 4 *)
5+8              (* 13 *)

(* Llamada de función *)
(* Nota, los nombres de funciones (y todo lo demás) distingue entre mayúsculas y minúsculas *)
Sin[Pi/2]        (* 1 *)

(* Sintaxis alternativa para la Llamada de una función con un parámetro *)
Sin@(Pi/2)       (* 1 *)
(Pi/2) // Sin    (* 1 *)

(* Cada sintaxis en WL tiene algún equivalente como una llamada de función *)
Veces[2, 2]      (* 4 *)
Mas[5, 8]       (* 13 *)

(* El uso de una variable por primera vez lo define y lo hace global *)
x = 5             (* 5 *)
x == 5            (* verdadero, Estilo-C asignación y pruebas de igualdad *)
x                 (* 5 *)
x = x + 5         (* 10 *)
x                 (* 10 *)
Modifique[x, 20]  (* No estaba bromeando cuando dije que TODO tiene una función equivalente *)
x                 (* 20 *)

(* Debido a que WL se basa en un sistema de álgebra computacional, *)
(* El uso de variables indefinidas está bien, simplemente obstruyen la evaluación *)
cow + 5          (* 5 + cow, cow es indefinido por lo que no puede evaluar más *)
cow + 5 + 10     (* 15 + cow, Va a evaluar lo que puede *)
%                (* 15 + cow, % Busca el último retorno *)
% - cow          (* 15, cow variable indefinida cancelada *)
moo = cow + 5    (* Cuidado, moo ahora tiene una expresión, no un número! *)

(* Definición de una función *)
Double[x_] := x * 2    (* Nota: = para evitar la evaluación inmediata del RHS
                          y después de x para indicar que no hay restricciones de concordancia de patrones *)
Double[10]             (* 20 *)
Double[Sin[Pi/2]]      (* 2 *)
Double @ Sin @ (Pi/2)  (* 2, @-Sintaxis evita colas de paréntesis *)
(Pi/2) // Sin // Double(* 2, //-Sintaxis lista las funciones en orden de ejecución *)

(* Para un uso de programación de estilo imperativo; Para separar declaraciones *)
(* Descarta cualquier salida de LHS y ejecuta RHS *)
miPrimero[] := (Print@"Hola"; Print@"Mundo")  (* Tenga en cuenta que los padres externos son críticos
                                                ; La precedencia es menor que := *)
miPrimero[]                                    (* Hola Mundo *)

(* Estilo-C para bucle *)
PrintTo[x_] := For[y=0, y<x, y++, (Print[y])]  (* Start, test, incr, body *)
PrintTo[5]                                     (* 0 1 2 3 4 *)

(* bucle *)
x = 0; While[x < 2, (Print@x; x++)]     (* Ciclo con prueba y cuerpo *)

(* Si y condicionales *)
x = 8; If[x==8, Print@"Yes", Print@"No"]   (* Condición, Caso verdadero, Caso distinto*)
Switch[x, 2, Print@"Two", 8, Print@"Yes"]  (* Interruptor de estilo de coincidencia de valor *)
Which[x==2, Print@"No", x==8, Print@"Yes"] (* estilo de caso distinto *)

(* Las variables distintas de los parámetros son globales por defecto, incluso dentro de las funciones *)
y = 10             (* 10, Variable global y *)
PrintTo[5]         (* 0 1 2 3 4 *)
y                  (* 5, Global por contador de bucle dentro de PrintTo *)
x = 20             (* 20, Variable global x *)
PrintTo[5]         (* 0 1 2 3 4 *)
x                  (* 20, x en PrintTo Es un parámetro y automáticamente local *)

(* Las variables locales se declaran utilizando la metafunción del módulo *)
(* Version con variable local*)
BetterPrintTo[x_] := Module[{y}, (For[y=0, y<x, y++, (Print@y)])]
y = 20             (* Variable global y *)
BetterPrintTo[5]   (* 0 1 2 3 4 *)
y                  (* 20, eso es mejor *)

(* El módulo realmente nos permite declarar cualquier ámbito que nos guste *)
Module[{count}, count=0;        (* Declare el alcance de este recuento de variables *)
  (IncCount[] := ++count);      (* Estas funciones están dentro de ese ámbito *)
  (DecCount[] := --count)]
count              (* count - Variable global count no esta definida *)
IncCount[]         (* 1, usando count variable dentro del alcance *)
IncCount[]         (* 2, incCount lo actualiza *)
DecCount[]         (* 1, decCount tambien lo hace *)
count              (* count - Aún ninguna variable global con ese nombre*)

(* listas *)
miLista = {1, 2, 3, 4}     (* {1, 2, 3, 4} *)
miLista[[1]]               (* 1 - Los índices de la lista de notas comienzan en 1, no 0 *)
Map[Double, miLista]       (* {2, 4, 6, 8} - Lista de estilo funcional mapa función *)
Double /@ miLista          (* {2, 4, 6, 8} - Sintaxis abreviada para arriba *)
Scan[Print, miLista]       (* 1 2 3 4 - Lista de bucle sobre estilo imperativo *)
Fold[Plus, 0, miLista]     (* 10 (0+1+2+3+4) *)
FoldList[Plus, 0, miLista] (* {0, 1, 3, 6, 10} - Guardar los resultados intermedios *)
Append[miLista, 5]         (* {1, 2, 3, 4, 5} - Note que miLista no está actualizada *)
Prepend[miLista, 5]        (* {5, 1, 2, 3, 4} - añada "miLista = " Si quieres que lo sea *)
Join[miLista, {3, 4}]      (* {1, 2, 3, 4, 3, 4} *)
miLista[[2]] = 5          (* {1, 5, 3, 4} - Esto actualiza miLista *)

(* Asociaciones, aka Diccionarios /Hashes *)
miHash = <|"Green" -> 2, "Red" -> 1|>   (* crea una asociación *)
miHash[["Green"]]                       (* 2, uselo *)
miHash[["Green"]] := 5                  (* 5, actualizalo *)
miHash[["Puce"]] := 3.5                 (* 3.5, extiendalo *)
KeyDropFrom[miHash, "Verde"]            (* Limpia la llave Verde *)
Claves[miHash]                            (* {Rojo} *)
Valores[miHash]                          (* {1} *)

(* Y no puedes hacer ninguna demo de Wolfram sin mostrar esto *)
Manipular[y^2, {y, 0, 20}] (* Devuelve una interfaz de usuario reactiva que muestra y ^ 2
                               Y permite ajustar y entre 0-20 con un deslizador.
                               Sólo funciona en interfaces gráficas *)
```

##Listo para mas?

* [Centro de Documentación](http://reference.wolfram.com/language/)
