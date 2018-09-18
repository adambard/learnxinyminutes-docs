---
lenguage: Matlab
nombre del archivo: learnmatlab.mat
contribuciones:
    - ["mendozao", "http://github.com/mendozao"]
    - ["jamesscottbrown", "http://jamesscottbrown.com"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Claudson Martins", "http://github.com/claudsonm"]
---
  
MATLAB significa MATrix LABoratory. Es un poderoso lenguaje de cálculo numérico comúnmente utilizado en ingeniería y matemáticas. Si tiene algún comentario, no dude en ponerse en contacto conmigo en [@the_ozzinator] (https://twitter.com/the_ozzinator), o [osvaldo.t.mendoza@gmail.com] (mailto: osvaldo.t.mendoza@gmail.com).

```matlab
%% secciones de código comienzan con dos signos de porcentaje. Los títulos de las secciones van en la misma línea.

%% Los comentarios comienzan con un signo de porcentaje.
% {
Comentarios de líneas múltiples
alguna cosa
me gusta
esta

% Dos signos de porcentaje indican el comienzo de una nueva sección de código
% Se pueden ejecutar secciones de códigos individuales moviendo el cursor a la sección seguida de
% haciendo clic en el botón "Ejecutar sección
% o usando Ctrl + Shift + Enter (Windows) o Cmd + Shift + Retorno (OS X)

%% Este es el comienzo de una sección de código
% Una forma de usar secciones es separar el código de inicio costoso pero inmutable como cargar datos
cargar myFile.mat y
A = A * 2;
plot(A);

  
Las secciones de código %% también se conocen como celdas de código o modo de celda (no debe confundirse con las matrices de celdas)

% de comandos pueden abarcar varias líneas, usando '...': a = 1 + 2 + ...
 + 4

% de comandos se pueden pasar al sistema operativo
!ping google.com

  
who% Muestra todas las variables en la memoria 
whos% Muestra todas las variables en la memoria, con sus tipos 
clear% Borra todas las variables de la memoria 
clear ('A')% Borra una variable particular openvar ('A')% Variable abierta en el editor de variables

  
clc% borra la escritura en su ventana de comando diario% Alternar la escritura del texto de la ventana de comando en un archivo ctrl-c% Anula el cálculo actual

edit('myfunction.m') % Abrir función / script en editor
type('myfunction.m') %   Imprima la fuente de la función / secuencia de comandos en la ventana de comandos


