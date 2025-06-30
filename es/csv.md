---
name: CSV
contributors:
- [Timon Erhart, 'https://github.com/turbotimon/']
translators:
- [Luis Morales, 'https://github.com/LewisPons']
---

CSV (valores separados por comas) es un formato de archivo ligero que se utiliza para almacenar datos tabulares
en texto plano, diseñado para facilitar el intercambio de datos entre programas,
especialmente hojas de cálculo y bases de datos. Su simplicidad y legibilidad humana
lo han convertido en una piedra angular de la interoperabilidad de datos. A menudo se utiliza para
mover datos entre programas con formatos incompatibles o propietarios.

Aunque el RFC 4180 proporciona un estándar para el formato, en la práctica, el término "CSV"
se utiliza a menudo de forma más amplia para referirse a cualquier archivo de texto que:

- Se pueda interpretar como datos tabulares
- Utilice un delimitador para separar los campos (columnas)
- Utilice saltos de línea para separar los registros (filas)
- Opcionalmente incluya una cabecera en la primera fila

```csv
Nombre, Edad, FechaDeNacimiento
Alice, 30, 1993-05-14
Bob, 25, 1998-11-02
Charlie, 35, 1988-03-21
```

## Delimitadores para filas y columnas

Las filas se separan normalmente con saltos de línea (`\n` o `\r\n`), mientras que las columnas
(campos) se separan con un delimitador específico. Aunque las comas son el delimitador más
común para los campos, otros caracteres, como el punto y coma (`;`), se utilizan
comúnmente en regiones donde las comas son separadores decimales (por ejemplo, Alemania).
Los tabuladores (`\t`) también se utilizan como delimitadores en algunos casos, y estos archivos a menudo
se denominan "TSV" (valores separados por tabuladores).

Ejemplo usando punto y coma como delimitador y coma para el separador decimal:

```csv
Nombre; Edad; Nota
Alice; 30; 50,50
Bob; 25; 45,75
Charlie; 35; 60,00
```

## Tipos de datos

Los archivos CSV no definen inherentemente los tipos de datos. Los números y las fechas se almacenan como
texto plano, y su interpretación depende del software que importa el
archivo. Normalmente, los datos se interpretan de la siguiente manera:

```csv
Dato, Comentario
100, Interpretado como un número (entero)
100.00, Interpretado como un número (punto flotante)
2024-12-03, Interpretado como una fecha o una cadena (dependiendo del analizador)
Hola Mundo, Interpretado como texto (cadena)
"1234", Interpretado como texto en lugar de un número
```

## Entrecomillado de cadenas y caracteres especiales

El entrecomillado de cadenas solo es necesario si la cadena contiene el delimitador, caracteres
especiales, o si de otra manera podría ser interpretada como un número. Sin embargo, a
menudo se considera una buena práctica entrecomillar todas las cadenas para mejorar la legibilidad y
robustez.

```csv
Ejemplos de entrecomillado de cadenas,
Cadena sin comillas,
"Cadena opcionalmente entrecomillada (buena práctica)",
"Si contiene el delimitador, necesita ser entrecomillada",
"También, si contiene caracteres especiales como \n saltos de línea o \t tabuladores",
"El propio carácter de entrecomillado "" normalmente se escapa duplicando la comilla ("""")",
"o en algunos sistemas con una barra invertida \" (como otros escapes)",
```

Sin embargo, asegúrese de que para un documento, el método de entrecomillado sea consistente.
Por ejemplo, los dos últimos ejemplos de entrecomillado con "" o \" no serían
consistentes y podrían causar problemas.

## Codificación

Se utilizan diferentes codificaciones. La mayoría de los archivos CSV modernos utilizan la codificación UTF-8, pero
los sistemas más antiguos podrían utilizar otras como ASCII o ISO-8859.

Si el archivo se transfiere o comparte entre diferentes sistemas, es una buena
práctica definir explícitamente la codificación utilizada, para evitar problemas con la
malinterpretación de caracteres.

## Más recursos

- [Wikipedia](https://es.wikipedia.org/wiki/Valores_separados_por_comas)
- [RFC 4180](https://datatracker.ietf.org/doc/html/rfc4180)
