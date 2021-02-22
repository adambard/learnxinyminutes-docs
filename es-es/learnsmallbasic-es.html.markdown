---
language: SmallBASIC
filename: learnsmallbasic-es.bas
contributors:
    - ["Chris Warren-Smith", "http://smallbasic.sourceforge.net"]
translators:
    - ["José Juan Hernández García", "http://jjuanhdez.es"]
lang: es-es
---

## Acerca de

SmallBASIC es un intérprete del lenguaje BASIC rápido y fácil de aprender, ideal para cálculos cotidianos, scripts y prototipos. SmallBASIC incluye funciones trigonométricas, matrices y álgebra, un IDE integrado, una potente librería de cadenas de texto, comandos de sistema, sonido y gráficos, junto con una sintaxis de programación estructurada.

## Desarrollo

SmallBASIC fue desarrollado originalmente por Nicholas Christopoulos a finales de 1999 para el Palm Pilot. El desarrollo del proyecto ha sido continuado por Chris Warren-Smith desde el año 2005.
Versiones de SmallBASIC se han hecho para una serie dispositivos de mano antiguos, incluyendo Franklin eBookman y el Nokia 770. También se han publicado varias versiones de escritorio basadas en una variedad de kits de herramientas GUI, algunas de las cuales han desaparecido. Las plataformas actualmente soportadas son Linux y Windows basadas en SDL2 y Android basadas en NDK. También está disponible una versión de línea de comandos de escritorio, aunque no suele publicarse en formato binario.
Alrededor de 2008 una gran corporación lanzó un entorno de programación BASIC con un nombre de similar. SmallBASIC no está relacionado con este otro proyecto.

```
REM Esto es un comentario
' y esto tambien es un comentario

REM Imprimir texto
PRINT "hola"
? "? es la abreviatura de PRINT"

REM Estructuras de control
FOR index = 0 TO 10 STEP 2
  ? "Este es el numero de linea "; index
NEXT
J = 0
REPEAT
 J++
UNTIL J = 10
WHILE J > 0
 J--
WEND

REM Estructura Select Case
SELECT CASE "Cool"
 CASE "null", 1, 2, 3, 4, 5, 6, 7, 8, "Cool", "blah"
 CASE "No Cool"
   PRINT "Fallo epico"
 CASE ELSE
   PRINT "Fallo"
END SELECT

REM Captura de errores con TRY/CATCH
TRY
  fn = Freefile
  OPEN filename FOR INPUT As #fn
CATCH err
  PRINT "No se pudo abrir"
END TRY

REM Procedimientos y funciones definidas por el usuario
FUNC add2(x, y)
  ' variables pueden declararse como locales en el ambito de una SUB o FUNC
  LOCAL k
  k = "k dejara de existir cuando retorne FUNC"
  add2 = x + y
END
PRINT add2(5, 5)

SUB print_it(it)
  PRINT it
END
print_it "IT...."

REM Visualizacion de lineas y pixeles
At 0, ymax / 2 + txth ("Q")
COLOR 1: ? "sin(x)":
COLOR 8: ? "cos(x)":
COLOR 12: ? "tan(x)"
LINE 0, ymax / 2, xmax, ymax / 2
FOR i = 0 TO xmax
  PSET i, ymax / 2 - SIN(i * 2 * pi / ymax) * ymax / 4 COLOR 1
  PSET i, ymax / 2 - COS(i * 2 * pi / ymax) * ymax / 4 COLOR 8
  PSET i, ymax / 2 - TAN(i * 2 * pi / ymax) * ymax / 4 COLOR 12
NEXT
SHOWPAGE

REM SmallBASIC es ideal para experimentar con fractales y otros efectos interesantes
DELAY 3000
RANDOMIZE
ff = 440.03
FOR j = 0 TO 20
  r = RND * 1000 % 255
  b = RND * 1000 % 255
  g = RND * 1000 % 255
  c = RGB(r, b, g)
  ff += 9.444
  FOR i = 0 TO 25000
    ff += ff
    x = MIN(xmax, -x + COS(f * i))
    y = MIN(ymax, -y + SIN(f * i))
    PSET x, y COLOR c
    IF (i % 1000 == 0) THEN
      SHOWPAGE
    fi
  NEXT
NEXT j

REM Para historiadores de computadoras, SmallBASIC puede ejecutar programas
REM encontrados en los primeros libros de computacion y revistas, por ejemplo:
10 LET A = 9
20 LET B = 7
30 PRINT A * B
40 PRINT A / B

REM SmallBASIC también tiene soporte para algunos conceptos modernos como JSON
aa = ARRAY("{\"cat\":{\"name\":\"harry\"},\"pet\":\"true\"}")
IF (ismap(aa) == false) THEN
  THROW "no es un mapa"
END IF
PRINT aa

PAUSE

```
## Artículos

* [Primeros pasos](http://smallbasic.sourceforge.net/?q=node/1573)
* [Bienvenido a SmallBASIC](http://smallbasic.sourceforge.net/?q=node/838)

## GitHub

* [Código fuente](https://github.com/smallbasic/SmallBASIC)
* [Reference snapshot](http://smallbasic.github.io/)
 
