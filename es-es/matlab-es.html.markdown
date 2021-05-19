---
language: Matlab
filename: learnmatlab-es.mat
contributors:
    - ["mendozao", "http://github.com/mendozao"]
    - ["jamesscottbrown", "http://jamesscottbrown.com"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Claudson Martins", "http://github.com/claudsonm"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
---

MATLAB significa 'MATrix LABoratory'. Es un poderoso lenguaje de computación numérica comúnmente usado en ingeniería y matemáticas.

Si tiene algún comentario, no dude en ponerse en contacto el autor en
[@the_ozzinator](https://twitter.com/the_ozzinator), o
[osvaldo.t.mendoza@gmail.com](mailto:osvaldo.t.mendoza@gmail.com).

```matlab
%% Una sección de código comienza con dos símbolos de porcentaje. Los títulos de la sección van en la misma líneas.
% Los comentarios comienzan con un símbolo de porcentaje.

%{
Los Comentarios de multiples líneas se
ven
como 
esto
%}

% Dos símbolos de porcentaje denotan el comienzo de una nueva sección de código.
% Secciones de código individuales pueden ser ejecutadas moviendo el cursor hacia la sección,
% seguida por un clic en el botón de “Ejecutar Sección”
% o usando Ctrl+Shift+Enter (Windows) o Cmd+Shift+Return (macOS)

%% Este es el comienzo de una sección de código
% Una forma de usar las secciones es separar un código de inicio costoso que no cambia, como cargar datos
load learnmatlab.mat y

%% Esta es otra sección de código
% Esta sección puede ser editada y ejecutada de manera repetida por sí misma, 
% y es útil para la programación exploratoria y demostraciones.
A = A * 2;
plot(A);

%% Las secciones de código también son conocidas como celdas de código o modo celda (no ha de ser confundido con arreglo de celdas)


% Los comandos pueden abarcar varias líneas, usando '...'
 a = 1 + 2 + ...
 + 4

% Los comandos se pueden pasar al sistema operativo
!ping google.com

who % Muestra todas las variables en la memoria
whos % Muestra todas las variables en la memoria con sus tipos
clear % Borra todas tus variables de la memoria
clear('A') % Borra una variable en particular
openvar('A') % Variable abierta en editor de variables

clc % Borra la escritura en la ventana de Comando
diary % Alterna la escritura del texto de la ventana de comandos al archivo
ctrl-c % Aborta el cálculo actual

edit('myfunction.m') % Abrir función/script en el editor
type('myfunction.m') % Imprime la fuente de la función/script en la ventana de comandos

profile on 	% Enciende el generador de perfilador de código
profile off 	% Apaga el generador de perfilador de código
profile viewer 	% Abre el perfilador de código

help command 	% Muestra la documentación del comando en la ventana de comandos
doc command 	% Muestra la documentación del comando en la ventana de Ayuda
lookfor command % Busca el comando en la primera línea comentada de todas las funciones
lookfor command -all % busca el comando en todas las funciones


% Formato de salida
format short 	% 4 decimales en un número flotante
format long 	% 15 decimales
format bank 	% solo dos dígitos después del punto decimal - para cálculos financieros
fprintf('texto') % imprime "texto" en la pantalla
disp('texto') 	% imprime "texto" en la pantalla

% Variables y expresiones
myVariable = 4 	% Espacio de trabajo de aviso muestra la variable recién creada
myVariable = 4; % Punto y coma suprime la salida a la Ventana de Comando
4 + 6  		% ans = 10
8 * myVariable 	% ans = 32
2 ^ 3 		% ans = 8
a = 2; b = 3;
c = exp(a)*sin(pi/2) % c = 7.3891

% Llamar funciones se pueden realizar de dos maneras:
% Sintaxis de función estándar:
load('myFile.mat', 'y') % argumentos entre paréntesis, separados por comas
% Sintaxis del comando:
load myFile.mat y 	% sin paréntesis, y espacios en lugar de comas
% Tenga en cuenta la falta de comillas en el formulario de comandos: 
% las entradas siempre se pasan como texto literal; no pueden pasar valores de variables.
% Además, no puede recibir salida:
[V,D] = eig(A);  % esto no tiene equivalente en forma de comando
[~,D] = eig(A);  % si solo se quiere D y no V



% Operadores lógicos
1 > 5 % ans = 0
10 >= 10 % ans = 1
3 ~= 4 % No es igual a -> ans = 1
3 == 3 % Es igual a -> ans = 1
3 > 1 && 4 > 1 % AND -> ans = 1
3 > 1 || 4 > 1 % OR -> ans = 1
~1 % NOT -> ans = 0

% Los operadores lógicos se pueden aplicar a matrices:
A > 5
% para cada elemento, si la condición es verdadera, ese elemento es 1 en la matriz devuelta
A( A > 5 )
% devuelve un vector que contiene los elementos en A para los que la condición es verdadera

% Cadenas
a = 'MiCadena'
length(a) % ans = 8
a(2) % ans = y
[a,a] % ans = MiCadenaMiCadena


% Celdas
a = {'uno', 'dos', 'tres'}
a(1) % ans = 'uno' - retorna una celda
char(a(1)) % ans = uno - retorna una cadena

% Estructuras
A.b = {'uno','dos'};
A.c = [1 2];
A.d.e = false;

% Vectores
x = [4 32 53 7 1]
x(2) % ans = 32, los índices en Matlab comienzan 1, no 0
x(2:3) % ans = 32 53
x(2:end) % ans = 32 53 7 1

x = [4; 32; 53; 7; 1] % Vector de columna

x = [1:10] % x = 1 2 3 4 5 6 7 8 9 10
x = [1:2:10] % Incrementa por 2, i.e. x = 1 3 5 7 9

% Matrices
A = [1 2 3; 4 5 6; 7 8 9]
% Las filas están separadas por un punto y coma; los elementos se separan con espacio o coma
% A =

%     1     2     3
%     4     5     6
%     7     8     9

A(2,3) % ans = 6, A(fila, columna)
A(6) % ans = 8
% (concatena implícitamente columnas en el vector, luego indexa en base a esto)


A(2,3) = 42 % Actualiza la fila 2 col 3 con 42
% A =

%     1     2     3
%     4     5     42
%     7     8     9

A(2:3,2:3) % Crea una nueva matriz a partir de la anterior
%ans =

%     5     42
%     8     9

A(:,1) % Todas las filas en la columna 1
%ans =

%     1
%     4
%     7

A(1,:) % Todas las columnas en la fila 1
%ans =

%     1     2     3

[A ; A] % Concatenación de matrices (verticalmente)
%ans =

%     1     2     3
%     4     5    42
%     7     8     9
%     1     2     3
%     4     5    42
%     7     8     9

% esto es lo mismo que
vertcat(A,A);


[A , A] % Concatenación de matrices (horizontalmente)

%ans =

%     1     2     3     1     2     3
%     4     5    42     4     5    42
%     7     8     9     7     8     9

% esto es lo mismo que
horzcat(A,A);


A(:, [3 1 2]) % Reorganiza las columnas de la matriz original
%ans =

%     3     1     2
%    42     4     5
%     9     7     8

size(A) % ans = 3 3

A(1, :) =[] % Elimina la primera fila de la matriz
A(:, 1) =[] % Elimina la primera columna de la matriz

transpose(A) % Transponer la matriz, que es lo mismo que:
A one
ctranspose(A) % Hermitian transpone la matriz
% (la transposición, seguida de la toma del conjugado complejo de cada elemento)
A' % Versión concisa de transposición compleja
A.' % Versión concisa de transposición (sin tomar complejo conjugado)




% Elemento por elemento Aritmética vs. Matriz Aritmética
% Por sí solos, los operadores aritméticos actúan sobre matrices completas. Cuando preceden
% por un punto, actúan en cada elemento en su lugar. Por ejemplo:
A * B % Multiplicación de matrices
A .* B % Multiplica cada elemento en A por su elemento correspondiente en B

% Hay varios pares de funciones, donde una actúa sobre cada elemento y 
% la otra (cuyo nombre termina en m) actúa sobre la matriz completa.
exp(A) % exponencializar cada elemento
expm(A) % calcular la matriz exponencial
sqrt(A) % tomar la raíz cuadrada de cada elemento
sqrtm(A) % encuentra la matriz cuyo cuadrado es A


% Trazando
x = 0:.10:2*pi; % Crea un vector que comienza en 0 y termina en 2 * pi con incrementos de .1
y = sin(x);
plot(x,y)
xlabel('x axis')
ylabel('y axis')
title('Plot of y = sin(x)')
axis([0 2*pi -1 1]) % x rango de 0 a 2 * pi, y rango de -1 a 1

plot(x,y1,'-',x,y2,'--',x,y3,':') % Para múltiples funciones en una parcela.
legend('Line 1 label', 'Line 2 label') % Etiquetar curvas con una leyenda.

% Método alternativo para trazar múltiples funciones en una parcela.
% mientras 'hold' está activado, los comandos se agregan al gráfico existente en lugar de reemplazarlo.
plot(x, y)
hold on
plot(x, z)
hold off

loglog(x, y) % Un diagrama de log-log.
semilogx(x, y) % Un diagrama con el eje x logarítmico.
semilogy(x, y) % Un diagrama con el eje y logarítmico.

fplot (@(x) x^2, [2,5]) % Un diagrama con el eje y logarítmico...

grid on % Muestra la cuadrícula; apague con 'grid off'.
axis square % Hace que la región actual de los ejes sea cuadrada.
axis equal % Establece la relación de aspecto para que las unidades de datos sean las mismas en todas las direcciones.

scatter(x, y); % Gráfico de dispersión
hist(x); % Histograma
stem(x); % Traza los valores como tallos, útiles para mostrar datos discretos.
bar(x); % Diagrama de barras

z = sin(x);
plot3(x,y,z); % Trazado de línea 3D.

pcolor(A) % Trazado de línea 3D...
contour(A) % Diagrama de contorno de la matriz.
mesh(A) % Traza una superficie de malla.

h = figure % Crea nuevo objeto figura, con el mango h.
figure(h) % Hace que la figura correspondiente al mango h la figura actual.
close(h) % Cierra la figura con mango h.
close all % Cierra todas las ventanas con figura abierta.
close % Cierra ventana de figura actual.

shg % Trae una ventana gráfica existente hacia adelante, o crea una nueva si es necesario.
clf clear % Borra la ventana de la figura actual y restablece la mayoría de las propiedades de la figura.

% Las propiedades se pueden establecer y cambiar a través de un identificador de figura.
% Puede guardar un identificador de una figura cuando la crea.
% La función get devuelve un handle a la figura actual
h = plot(x, y); % Puedes guardar un control de una figura cuando la creas
set(h, 'Color', 'r')
% 'y' yellow; 'm' magenta, 'c' cyan, 'r' red, 'g' green, 'b' blue, 'w' white, 'k' black
set(h, 'LineStyle', '--')
% '--' es línea continua, '---' discontinua, ':' punteada, '-.' dash-dot, 'none' es sin línea
get (h, 'LineStyle')


% La función gca devuelve un mango a los ejes para la figura actual
set(gca, 'XDir', 'reverse'); % invierte la dirección del eje x

% Para crear una figura que contenga varios ejes en posiciones de mosaico, use 'subplot'
subplot(2,3,1); % seleccione la primera posición en una grilla de subtramas de 2 por 3
plot(x1); title('First Plot') % traza algo en esta posición
subplot(2,3,2); % selecciona la segunda posición en la grilla
plot(x2); title('Second Plot') % trazar algo allí


% Para usar funciones o scripts, deben estar en su ruta o directorio actual
path % muestra la ruta actual
addpath /path/to/dir % agrega a la ruta
rmpath /path/to/dir % elimina de la ruta
cd /path/to/move/into % cambia de directorio


% Las variables se pueden guardar en archivos .mat
save('myFileName.mat') % Guarda las variables en su espacio de trabajo
load('myFileName.mat') % Carga las variables guardadas en espacio de trabajo

% M-file Scripts
% Un archivo de script es un archivo externo que contiene una secuencia de instrucciones.
% Permiten evitar escribir repetidamente el mismo código en la ventana de comandos
% Tienen extensiones .m

% M-file Functions
% Al igual que los scripts, y tienen la misma extensión .m
% Pero pueden aceptar argumentos de entrada y devolver una salida
% Además, tienen su propio espacio de trabajo (es decir, diferente alcance variable).
% El nombre de la función debe coincidir con el nombre del archivo (por lo tanto, guarde este ejemplo como double_input.m).
% 'help double_input.m' devuelve los comentarios en la línea que comienza la función
function output = double_input(x)
	% double_input(x) devuelve el doble del valor de x
	output = 2*x;
end
double_input(6) % ans = 12


% También puede tener subfunciones y funciones anidadas.
% Las subfunciones están en el mismo archivo que la función primaria, y solo pueden ser
% llamadas por funciones en el archivo. Las funciones anidadas se definen dentro de otra
% otras funciones y tienen acceso tanto a su área de trabajo como a su propio espacio de trabajo.

% Si desea crear una función sin crear un nuevo archivo, puede usar una
% función anónima. Útil cuando se define rápidamente una función para pasar a
% otra función (por ejemplo, trazar con fplot, evaluar una integral indefinida
% con quad, encuentra roots con fzero, o encuentra mínimo con fminsearch).
% Ejemplo que devuelve el cuadrado de su entrada, asignado al identificador sqr:
sqr = @(x) x.^2;
sqr(10) % ans = 100
doc function_handle % averiguar más

% User input
a = input('Ingrese el valor:')

% Detiene la ejecución del archivo y le da control al teclado: el usuario puede examinar
% o cambiar las variables. Escriba 'return' para continuar la ejecución, o 'dbquit' para salir del teclado

% Lectura de datos (también xlsread / importdata / imread para archivos de excel / CSV / image)
fopen(filename)

% Salida
disp(a) % Imprime el valor de la variable a
disp('Hola Mundo') % Imprime una cadena
fprintf % Imprime en la ventana de comandos con más control

% Declaraciones condicionales (los paréntesis son opcionales, pero buen estilo)
if (a > 15)
	disp('Mayor que 15')
elseif (a == 23)
	disp('a es 23')
else
	disp('Ninguna condicion se ha cumplido')
end

% Bucles
% NB. haciendo un bucle sobre los elementos de un vector / matriz es lento!
% Siempre que sea posible, use funciones que actúen en todo el vector / matriz a la vez
for k = 1:5
	disp(k)
end

k = 0;
while (k < 5)
	k = k + 1;
end

% Ejecución del código de tiempo: 'toc' imprime el tiempo desde que se llamó 'tic'
tic
A = rand(1000);
A*A*A*A*A*A*A;
toc

% Conectarse a una base de datos MySQL
dbname = 'database_name';
username = 'root';
password = 'root';
driver = 'com.mysql.jdbc.Driver';
dburl = ['jdbc:mysql://localhost:8889/' dbname];
javaclasspath('mysql-connector-java-5.1.xx-bin.jar'); %xx depende de la versión, descarga disponible en http://dev.mysql.com/downloads/connector/j/
conn = database(dbname, username, password, driver, dburl);
sql = ['SELECT * from table_name where id = 22'] % Ejemplo de instrucción sql
a = fetch(conn, sql) %a contendrá sus datos


% Funciones matemáticas comunes
sin(x)
cos(x)
tan(x)
asin(x)
acos(x)
atan(x)
exp(x)
sqrt(x)
log(x)
log10(x)
abs(x) % Si x es complejo, devuelve la magnitud
min(x)
max(x)
ceil(x)
floor(x)
round(x)
rem(x)
rand % Números pseudoaleatorios distribuidos uniformemente
randi % Enteros pseudoaleatorios distribuidos uniformemente
randn % Números pseudoaleatorios distribuidos normalmente

% Operaciones matemáticas complejas
abs(x) 	 % Magnitud de la variable compleja x
phase(x) % Fase (o ángulo) de la variable compleja x
real(x)  % Retorna la parte real de x (es decir, devuelve a si x = a + jb)
imag(x)  % Retorna la parte imaginaria de x (es decir, devuelve b si x = a + jb)
conj(x)  % Retorna el complejo conjugado 


% Constantes comunes
pi
NaN
inf

% Resolviendo ecuaciones matriciales (si no hay solución, devuelve una solución de mínimos cuadrados)
%Los operadores \ y / son equivalentes a las funciones mldivide y mrdivide
x=A\b % Resuelve Ax = b. Más rápido y más numéricamente preciso que usar inv (A) * b.
x=b/A % Resuelve xA = b

inv(A) % calcular la matriz inversa
pinv(A) % calcular el pseudo-inverso

% Funciones de matriz comunes
zeros(m,n) % m x n matriz de 0
ones(m,n) % m x n matriz de 1
diag(A) % Extrae los elementos diagonales de una matriz A
diag(x) % Construya una matriz con elementos diagonales enumerados en x, y ceros en otra parte
eye(m,n) % Matriz de identidad
linspace(x1, x2, n) % Devuelve n puntos equiespaciados, con min x1 y max x2
inv(A) % Inverso de la matriz A
det(A) % Determinante de A
eig(A) % Valores propios y vectores propios de A
trace(A) % Traza de la matriz: equivalente a sum(diag(A))
isempty(A) % Determina si la matriz está vacía
all(A) % Determina si todos los elementos son distintos de cero o verdaderos
any(A) % Determina si alguno de los elementos es distinto de cero o verdadero
isequal(A, B) % Determina la igualdad de dos matrices
numel(A) % Cantidad de elementos en matriz
triu(x) % Devuelve la parte triangular superior de x
tril(x) % Devuelve la parte triangular inferior de x
cross(A,B) % Devuelve el producto cruzado de los vectores A y B
dot(A,B) % Devuelve un producto escalar de dos vectores (debe tener la misma longitud)
transpose(A) % Devuelve la transposición de A
fliplr(A) % Voltea la matriz de izquierda a derecha
flipud(A) % Voltea la matriz de arriba hacia abajo

% Factorizaciones de matrices
[L, U, P] = lu(A) % Descomposición LU: PA = LU, L es triangular inferior, U es triangular superior, P es matriz de permutación
[P, D] = eig(A) % eigen-decomposition: AP = PD, las columnas de P son autovectores y las diagonales de D'son valores propios
[U,S,V] = svd(X) % SVD: XV = US, U y V son matrices unitarias, S tiene elementos diagonales no negativos en orden decreciente

% Funciones comunes de vectores
max     % componente más grande
min     % componente más pequeño
length  % longitud de un vector
sort    % ordenar en orden ascendente
sum     % suma de elementos
prod    % producto de elementos
mode    % valor modal
median  % valor mediano
mean    % valor medio
std     % desviación estándar
perms(x) % enumera todas las permutaciones de elementos de x
find(x) % Encuentra todos los elementos distintos de cero de x y devuelve sus índices, puede usar operadores de comparación,
        % i.e. find( x == 3 ) devuelve índices de elementos que son iguales a 3
        % i.e. find( x >= 3 ) devuelve índices de elementos mayores o iguales a 3


% Clases
% Matlab puede soportar programación orientada a objetos.
% Las clases deben colocarse en un archivo del nombre de la clase con la extensión .m. 
% Para comenzar, creamos una clase simple para almacenar puntos de referencia de GPS.
% Comience WaypointClass.m
classdef WaypointClass % El nombre de la clase.
  properties % Las propiedades de la clase se comportan como Estructuras
    latitude 
    longitude 
  end
  methods 
    % Este método que tiene el mismo nombre de la clase es el constructor.
    function obj = WaypointClass(lat, lon)
      obj.latitude = lat;
      obj.longitude = lon;
    end

    % Otras funciones que usan el objeto Waypoint
    function r = multiplyLatBy(obj, n)
      r = n*[obj.latitude];
    end

    % Si queremos agregar dos objetos Waypoint juntos sin llamar
    % a una función especial, podemos sobrecargar la aritmética de Matlab así:
    function r = plus(o1,o2)
      r = WaypointClass([o1.latitude] +[o2.latitude], ...
                        [o1.longitude]+[o2.longitude]);
    end
  end
end
% Fin WaypointClass.m

% Podemos crear un objeto de la clase usando el constructor
a = WaypointClass(45.0, 45.0)

% Las propiedades de clase se comportan exactamente como estructuras de Matlab.
a.latitude = 70.0
a.longitude = 25.0

% Los métodos se pueden llamar de la misma manera que las funciones
ans = multiplyLatBy(a,3)

% El método también se puede llamar usando notación de puntos. En este caso, el objeto
% no necesita ser pasado al método.
ans = a.multiplyLatBy(a,1/3)

% Las funciones de Matlab pueden sobrecargarse para manejar objetos.
% En el método anterior, hemos sobrecargado cómo maneja Matlab
% la adición de dos objetos Waypoint.
b = WaypointClass(15.0, 32.0)
c = a + b

```

## Más sobre Matlab

* [The official website (EN)](http://www.mathworks.com/products/matlab/)
* [The official MATLAB Answers forum (EN)](http://www.mathworks.com/matlabcentral/answers/)
* [Loren on the Art of MATLAB (EN)](http://blogs.mathworks.com/loren/)
* [Cleve's Corner (EN)](http://blogs.mathworks.com/cleve/)

