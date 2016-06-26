---
language: Matlab
contributors:
    - ["mendozao", "http://github.com/mendozao"]
    - ["jamesscottbrown", "http://jamesscottbrown.com"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
translators:
    - ["Samuele Gallerani", "http://github.com/fontealpina"]
lang: it-it
filename: matlab-it.md
---

MATLAB sta per MATrix LABoratory ed è un potente linguaggio per il calcolo numerico comunemente usato in ingegneria e matematica.

```matlab
% I commenti iniziano con il segno percentuale.

%{
I commenti multilinea
assomigliano a
qualcosa
del genere
%}

% i comandi possono essere spezzati su più linee, usando '...':
 a = 1 + 2 + ...
 + 4

% i comandi possono essere passati al sistema operativo
!ping google.com

who % Mostra tutte le variabili in memoria
whos % Mostra tutte le variabili in memoria, con i loro tipi
clear % Cancella tutte le tue variabili dalla memoria
clear('A') % Cancella una particolare variabile
openvar('A') % Apre la variabile in un editor di variabile

clc % Cancella il contenuto della Command Window
diary % Attiva il log della Command Window su file
ctrl-c % Interrompe il calcolo corrente

edit('myfunction.m') % Apre la funzione/script nell'editor
type('myfunction.m') % Stampa il codice della funzione/script sulla Command Window

profile on 	% Attiva la profilazione del codice
profile off 	% Disattiva la profilazione del codice
profile viewer 	% Apre il profilatore

help comando 	% Mostra la documentazione di comando sulla Command Window
doc comando 	% Mostra la documentazione di comando sulla Help Window
lookfor comando % Cerca comando nella prima linea di commento di tutte le funzioni
lookfor comando -all % Cerca comando in tutte le funzioni


% Formattazione dell'output
format short 	% 4 decimali in un numero float
format long 	% 15 decimali
format bank 	% Solo due cifre decimali - per calcoli finaziari
fprintf('text') % Stampa "text" a terminale
disp('text') 	% Stampa "text" a terminale

% Variabili ed espressioni
miaVariabile = 4 % Il pannello Workspace mostra la nuova variabile creata
miaVariabile = 4; % Il punto e virgola evita che l'output venga stampato sulla Command Window
4 + 6  		% ans = 10
8 * myVariable 	% ans = 32
2 ^ 3 		% ans = 8
a = 2; b = 3;
c = exp(a)*sin(pi/2) % c = 7.3891

% La chiamata di funzioni può essere fatta in due modi differenti:
% Sintassi standard di una funzione:
load('myFile.mat', 'y') % argomenti tra parentesi, separati da virgole
% Sintassi di tipo comando:
load myFile.mat y 	% Non ci sono parentesi e gli argometi sono separati da spazi
% Notare la mancanza di apici nella sintassi di tipo comando: gli input sono sempre passati come
% testo letterale - non è possibile passare valori di variabili. Inoltre non può ricevere output:
[V,D] = eig(A);  % Questa non ha una forma equivalente con una sintassi di tipo comando
[~,D] = eig(A);  % Se si vuole solo D e non V



% Operatori logici
1 > 5 % ans = 0
10 >= 10 % ans = 1
3 ~= 4 % Not equal to -> ans = 1
3 == 3 % equal to -> ans = 1
3 > 1 && 4 > 1 % AND -> ans = 1
3 > 1 || 4 > 1 % OR -> ans = 1
~1 % NOT -> ans = 0

% Gli operatori logici possono essere applicati alle matrici:
A > 5
% Per ogni elemento, se la condizione è vera, quell'elemento vale 1 nella matrice risultante
A( A > 5 )
% Restituisce un vettore contenente gli elementi in A per cui la condizione è vera

% Stringhe
a = 'MyString'
length(a) % ans = 8
a(2) % ans = y
[a,a] % ans = MyStringMyString


% Celle
a = {'one', 'two', 'three'}
a(1) % ans = 'one' - ritorna una cella
char(a(1)) % ans = one - ritorna una stringa

% Strutture
A.b = {'one','two'};
A.c = [1 2];
A.d.e = false;

% Vettori
x = [4 32 53 7 1]
x(2) % ans = 32, gli indici in Matlab iniziano da 1, non da 0
x(2:3) % ans = 32 53
x(2:end) % ans = 32 53 7 1

x = [4; 32; 53; 7; 1] % Vettore colonna

x = [1:10] % x = 1 2 3 4 5 6 7 8 9 10

% Matrici
A = [1 2 3; 4 5 6; 7 8 9]
% Le righe sono separate da punto e virgola, mentre gli elementi sono separati da spazi
% A =

%     1     2     3
%     4     5     6
%     7     8     9

A(2,3) % ans = 6, A(row, column)
A(6) % ans = 8
% (implicitamente concatena le colonne in un vettore, e quindi gli indici sono riferiti al vettore)


A(2,3) = 42 % Aggiorna riga 2 colonna 3 con 42
% A =

%     1     2     3
%     4     5     42
%     7     8     9

A(2:3,2:3) % Crea una nuova matrice a partire da quella precedente
%ans =

%     5     42
%     8     9

A(:,1) % Tutte le righe nella colonna 1
%ans =

%     1
%     4
%     7

A(1,:) % Tutte le colonne in riga 1
%ans =

%     1     2     3

[A ; A] % Concatenazione di matrici (verticalmente)
%ans =

%     1     2     3
%     4     5    42
%     7     8     9
%     1     2     3
%     4     5    42
%     7     8     9

% è equivalente a
vertcat(A,A);


[A , A] % Concatenazione di matrici (orrizontalmente)

%ans =

%     1     2     3     1     2     3
%     4     5    42     4     5    42
%     7     8     9     7     8     9

% è equivalente a
horzcat(A,A);


A(:, [3 1 2]) % Ripristina le colonne della matrice originale
%ans =

%     3     1     2
%    42     4     5
%     9     7     8

size(A) % ans = 3 3

A(1, :) =[] % Rimuove la prima riga della matrice
A(:, 1) =[] % Rimuove la prima colonna della matrice

transpose(A) % Traspone la matrice, equivale a:
A one
ctranspose(A) % Trasposizione hermitiana della matrice
% (ovvero il complesso coniugato di ogni elemento della matrice trasposta)




% Aritmetica Elemento per Elemento vs. Artimetica Matriciale
% Gli operatori aritmetici da soli agliscono sull'intera matrice. Quando sono preceduti
% da un punto, allora agiscono su ogni elemento. Per esempio:
A * B % Moltiplicazione matriciale
A .* B % Moltiplica ogni elemento di A per il corrispondente elemento di B

% Ci sono diverse coppie di funzioni, in cui una agisce su ogni elemento, e
% l'altra (il cui nome termina con m) agisce sull'intera matrice.
exp(A) % Calcola l'esponenziale di ogni elemento
expm(A) % Calcola la matrice esponenziale
sqrt(A) % Calcola la radice quadrata di ogni elementotake the square root of each element
sqrtm(A) % Trova la matrice di cui A nè è la matrice quadrata


% Plot di grafici
x = 0:.10:2*pi; % Crea un vettore che inizia a 0 e termina 2*pi con incrementi di .1
y = sin(x);
plot(x,y)
xlabel('x axis')
ylabel('y axis')
title('Plot of y = sin(x)')
axis([0 2*pi -1 1]) % x range da 0 a 2*pi, y range da -1 a 1

plot(x,y1,'-',x,y2,'--',x,y3,':') % Per stampare più funzioni in unico plot
legend('Line 1 label', 'Line 2 label') % Aggiunge un etichetta con il nome delle curve

% Metodo alternativo per stampare funzioni multiple in un unico plot.
% mentre 'hold' è on, i comandi sono aggiunti al grafico esistene invece di sostituirlo
plot(x, y)
hold on
plot(x, z)
hold off

loglog(x, y) % Un plot di tipo log-log
semilogx(x, y) % Un plot con asse x logaritmico
semilogy(x, y) % Un plot con asse y logaritmico

fplot (@(x) x^2, [2,5]) % Stampa la funzione x^2 da x=2 a x=5

grid on % Mostra la griglia, disattivare con 'grid off'
axis square % Rende quadrata la regione individuata dagli assi
axis equal % Iposta l'aspetto del grafico in modo che le unità degli assi siano le stesse

scatter(x, y); % Scatter-plot
hist(x); % Istogramma

z = sin(x);
plot3(x,y,z); % Stampa una linea 3D

pcolor(A) % Heat-map di una matrice: stampa una griglia di rettangoli, colorati in base al valore
contour(A) % Contour plot di una matrice
mesh(A) % Stampa come una superfice di mesh

h = figure	% Crea un nuovo oggetto figura, con handle f
figure(h) % Rende la figura corrispondente al handle h la figura corrente
close(h) % Chiude la figura con handle h
close all % Chiude tutte le figure
close % Chiude la figura corrente

shg % Riutilizza una finestra grafica già esistente, o se necessario ne crea una nuova
clf clear % Pulisce la figura corrente, e resetta le proprietà della figura

% Le proprietà possono essere impostate e modificate attraverso l'handle della figura.
% Si può salvare l'handle della figura quando viene creata.
% La funzione gcf restituisce un handle alla figura attuale.
h = plot(x, y); % Si può salvare un handle della figura quando viene creata
set(h, 'Color', 'r')
% 'y' yellow; 'm' magenta, 'c' cyan, 'r' red, 'g' green, 'b' blue, 'w' white, 'k' black
set(h, 'LineStyle', '--')
 % '--' linea continua, '---' tratteggiata, ':' puntini, '-.' trattino-punto, 'none' nessuna linea
get(h, 'LineStyle')


% La funzione gca restituisce un handle degli assi della figura corrente
set(gca, 'XDir', 'reverse'); % Inverte la direzione dell'asse x

% Per creare una figura che contiene diverse sottofigure, usare subplot
subplot(2,3,1); % Seleziona la prima posizione in una griglia 2 per 3 di sottofigure
plot(x1); title('First Plot') % Stampa qualcosa in questa posizione
subplot(2,3,2); % Seleziona la seconda posizione nella griglia
plot(x2); title('Second Plot') % Stampa qualcosa in questa posizione


% Per usare funzioni o script, devono essere nel tuo path o nella directory corrente
path % Mostra il path corrente
addpath /path/to/dir % Aggiunge al path
rmpath /path/to/dir % Rimuove dal path
cd /path/to/move/into % Cambia directory


% Le variabili possono essere salvate in file .mat
save('myFileName.mat') % Salva le variabili nel tuo Workspace
load('myFileName.mat') % Carica variabili salvate nel tuo Workspace

% M-file Scripts
% I file di script sono file esterni che contengono una sequenza di istruzioni.
% Permettono di evitare di scrivere ripetutamente lo stesso codice nella Command Window
% Hanno estensione .m

% M-file Functions
% Come gli script, hanno la stessa estensione .m
% Ma possono accettare argomenti di input e restituire un output.
% Inoltre, hanno un proprio workspace (differente scope delle variabili).
% Il nome della funzione dovrebbe coincidere con il nome del file (quindi salva questo esempio come double_input.m).
% 'help double_input.m' restituisce i commenti sotto alla linea iniziale della funzione
function output = double_input(x)
	%double_input(x) restituisce il doppio del valore di x
	output = 2*x;
end
double_input(6) % ans = 12


% Si possono anche avere sottofunzioni e funzioni annidate.
% Le sottofunzioni sono nello stesso file della funzione primaria, e possono solo essere
% chiamate da funzioni nello stesso file. Le funzioni annidate sono definite dentro ad altre
% funzioni, e hanno accesso ad entrambi i workspace.

% Se si vuole creare una funzione senza creare un nuovo file si può usare una
% funzione anonima. Utile quando si vuole definire rapidamente una funzione da passare ad
% un'altra funzione (es. stampa con fplot, valutare un integrale indefinito
% con quad, trovare le radici con fzenzro, o trovare il minimo con fminsearch).
% Esempio che restituisce il quadrato del proprio input, assegnato all'handle sqr:
sqr = @(x) x.^2;
sqr(10) % ans = 100
doc function_handle % scopri di più

% Input dell'utente
a = input('Enter the value: ')

% Ferma l'esecuzione del file e cede il controllo alla tastiera: l'utente può esaminare
% o cambiare variabili. Digita 'return' per continuare l'esecuzione, o 'dbquit' per uscire
keyboard

% Importarare dati (anche xlsread/importdata/imread per excel/CSV/image file)
fopen(filename)

% Output
disp(a) % Stampa il valore della variabile a
disp('Hello World') % Stampa una stringa
fprintf % Stampa sulla Command Window con più controllo

% Istruzioni condizionali (le parentesi sono opzionali, ma un buon stile)
if (a > 15)
	disp('Maggiore di 15')
elseif (a == 23)
	disp('a è 23')
else
	disp('nessuna condizione verificata')
end

% Cicli
% NB. Ciclare su elementi di vettori/matrici è lento!
% Dove possibile, usa funzioni che agiscono sull'intero vettore/matrice
for k = 1:5
	disp(k)
end

k = 0;
while (k < 5)
	k = k + 1;
end

% Misurare la durata dell'esecuzione del codice: 'toc' stampa il tempo trascorso da quando 'tic' è stato chiamato
tic
A = rand(1000);
A*A*A*A*A*A*A;
toc

% Connessione a un Database MySQL
dbname = 'database_name';
username = 'root';
password = 'root';
driver = 'com.mysql.jdbc.Driver';
dburl = ['jdbc:mysql://localhost:8889/' dbname];
javaclasspath('mysql-connector-java-5.1.xx-bin.jar');
% xx dipende dalla versione, download disponibile all'indirizzo http://dev.mysql.com/downloads/connector/j/
conn = database(dbname, username, password, driver, dburl);
sql = ['SELECT * from table_name where id = 22'] % Esempio istruzione sql
a = fetch(conn, sql) % conterra i tuoi dati


% Funzioni matematiche comuni
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
abs(x)
min(x)
max(x)
ceil(x)
floor(x)
round(x)
rem(x)
rand % Numeri pseudocasuali uniformemente distribuiti
randi % Numeri interi pseudocasuali uniformemente distrubuiti
randn % Numeri pseudocasuali distrbuiti normalmente

% Costanti comuni
pi
NaN
inf

% Risolvere equazioni matriciali
% Gli operatori \ e / sono equivalenti alle funzioni mldivide e mrdivide
x=A\b % Risolve Ax=b. Più veloce e più accurato numericamente rispetto ad usare inv(A)*b.
x=b/A % Risolve xA=b

inv(A) % Calcola la matrice inversa
pinv(A) % Calcola la matrice pseudo-inversa

% Funzioni comuni su matrici
zeros(m,n) % Matrice m x n di zeri
ones(m,n) % Matrice m x n di uni
diag(A) % Estrae gli elementi della diagonale della matrice A
diag(x) % Costruisce una matrice con elementi diagonali uguali agli elementi di x, e zero negli altri elementi
eye(m,n) % Matrice identità
linspace(x1, x2, n) % Ritorna n punti equamente distanziati, con minimo x1 e massimo x2
inv(A) % Matrice inversa di A
det(A) % Determinante di A
eig(A) % Autovalori e autovettori di A
trace(A) % Traccia della matrice -  equivalente a sum(diag(A))
isempty(A) % Verifica se l'array è vuoto
all(A) % Verifica se tutti gli elementi sono nonzero o veri
any(A) % Verifica se almento un elemento è nonzero o vero
isequal(A, B) % Verifica l'uguaglianza di due array
numel(A) % Numero di elementi nella matrice
triu(x) % Ritorna la parte triangolare superiore di x
tril(x) % Ritorna la parte triangolare inferiore di x
cross(A,B) %  Ritorna il prodotto vettoriale dei vettori A e B
dot(A,B) % Ritorna il prodotto scalare di due vettori (devono avere la stessa lunghezza)
transpose(A) % Ritorna la trasposta di A
fliplr(A) % Capovolge la matrice da sinistra a destra
flipud(A) % Capovolge la matrice da sopra a sotto

% Fattorizzazione delle matrici
[L, U, P] = lu(A) % Decomposizione LU: PA = LU, L è il triangolo inferiore, U è il triangolo superiore, P è la matrice di permutazione
[P, D] = eig(A) % Auto-decomposizione: AP = PD, le colonne di P sono autovettori e gli elementi sulle diagonali di D sono autovalori
[U,S,V] = svd(X) % SVD: XV = US, U e V sono matrici unitarie, S ha gli elementi della diagonale non negativi in ordine decrescente

% Funzioni comuni su vettori
max     % elemento più grande
min     % elemento più piccolo
length  % lunghezza del vettore
sort    % ordina in modo crescente
sum     % somma degli elementi
prod    % prodotto degli elementi
mode	% valore moda
median  % valore mediano
mean    % valore medio
std     % deviazione standard
perms(x) % lista tutte le permutazioni di elementi di x


% Classi
% Matlab supporta la programmazione orientata agli oggetti.
% La classe deve essere messa in un file con lo stesso nome della classe e estensione .m
% Per iniziare, creiamo una semplice classe per memorizzare waypoint GPS
% Inizio WaypointClass.m
classdef WaypointClass % Il nome della classe.
  properties % Le proprietà della classe funzionano come Strutture
    latitude
    longitude
  end
  methods
    % Questo metodo che ha lo stesso nome della classe è il costruttore
    function obj = WaypointClass(lat, lon)
      obj.latitude = lat;
      obj.longitude = lon;
    end

    % Altre funzioni che usano l'oggetto Waypoint
    function r = multiplyLatBy(obj, n)
      r = n*[obj.latitude];
    end

    % Se si vuole aggiungere due oggetti Waypoint insieme senza chiamare
    % una funzione speciale si può sovradefinire una funzione aritmetica di Matlab come questa:
    function r = plus(o1,o2)
      r = WaypointClass([o1.latitude] +[o2.latitude], ...
                        [o1.longitude]+[o2.longitude]);
    end
  end
end
% End WaypointClass.m

% Si può creare un oggetto della classe usando un costruttore
a = WaypointClass(45.0, 45.0)

% Le proprietà della classe si comportano esattamente come una Struttura Matlab.
a.latitude = 70.0
a.longitude = 25.0

% I metodi possono essere chiamati allo stesso modo delle funzioni
ans = multiplyLatBy(a,3)

% Il metodo può anche essere chiamato usando una notazione con punto. In questo caso, l'oggetto
% non necessita di essere passato al metodo.
ans = a.multiplyLatBy(a,1/3)

% Le funzioni Matlab possono essere sovradefinite per gestire oggetti.
% Nel metodo sopra, è stato sovradefinito come Matlab gestisce
% l'addizione di due oggetti Waypoint.
b = WaypointClass(15.0, 32.0)
c = a + b

```

## Di più su Matlab

* Sito ufficiale [http://http://www.mathworks.com/products/matlab/](http://www.mathworks.com/products/matlab/)
* Forum ufficiale di MATLAB: [http://www.mathworks.com/matlabcentral/answers/](http://www.mathworks.com/matlabcentral/answers/)
