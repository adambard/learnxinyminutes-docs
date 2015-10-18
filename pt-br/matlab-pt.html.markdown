---
language: Matlab
contributors:
    - ["mendozao", "http://github.com/mendozao"]
    - ["jamesscottbrown", "http://jamesscottbrown.com"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
translators:
    - ["Claudson Martins", "https://github.com/claudsonm"]
lang: pt-br
filename: learnmatlab-pt.mat

---

MATLAB significa MATrix LABoratory. É uma poderosa linguagem de computação numérica geralmente utilizada em engenharia e matemática.

Se você tem algum feedback, por favor fique a vontade para me contactar via
[@the_ozzinator](https://twitter.com/the_ozzinator), ou
[osvaldo.t.mendoza@gmail.com](mailto:osvaldo.t.mendoza@gmail.com).

```matlab
% Comentários iniciam com um sinal de porcentagem

%{
Comentários de múltiplas linhas
parecem
com
algo assim
%}

% Comandos podem ocupar várinhas linhas, usando '...':
 a = 1 + 2 + ...
 + 4

% Comandos podem ser passados para o sistema operacional
!ping google.com

who % Exibe todas as variáveis na memória
whos % Exibe todas as variáveis na memória, com seus tipos
clear % Apaga todas as suas variáveis da memória
clear('A') % Apaga uma variável em particular
openvar('A') % Abre a variável no editor de variável

clc % Apaga o conteúdo escrito na sua janela de comando
diary % Alterna o conteúdo escrito na janela de comando para um arquivo de texto
ctrl-c % Aborta a computação atual

edit('minhafuncao.m') % Abre a função/script no editor
type('minhafuncao.m') % Imprime o código-fonte da função/script na janela de comando

profile on    % Ativa o perfil de código
profile off 	% Desativa o perfil de código
profile viewer 	% Visualiza os resultados na janela de Profiler

help comando 	% Exibe a documentação do comando na janela de comando
doc comando 	% Exibe a documentação do comando na janela de ajuda
lookfor comando % Procura por comando na primeira linha comentada de todas as funções
lookfor comando -all % Procura por comando em todas as funções


% Formatação de saída
format short 	% 4 casas decimais em um número flutuante
format long 	% 15 casas decimais
format bank 	% 2 dígitos após o ponto decimal - para cálculos financeiros
fprintf('texto') % Imprime na tela "texto"
disp('texto') 	% Imprime na tela "texto"

% Variáveis & Expressões
minhaVariavel = 4  % O painel Workspace mostra a variável recém-criada
minhaVariavel = 4; % Ponto e vírgula suprime a saída para a janela de comando
4 + 6  		% Resposta = 10
8 * minhaVariavel 	% Resposta = 32
2 ^ 3 		% Resposta = 8
a = 2; b = 3;
c = exp(a)*sin(pi/2) % c = 7.3891

% A chamada de funções pode ser feita por uma das duas maneiras:
% Sintaxe de função padrão:
load('arquivo.mat', 'y') % Argumentos entre parênteses, separados por vírgula
% Sintaxe de comando:
load arquivo.mat y 	% Sem parênteses, e espaços ao invés de vírgulas
% Observe a falta de aspas na forma de comando: entradas são sempre passadas
% como texto literal - não pode passar valores de variáveis.
% Além disso, não pode receber saída:
[V,D] = eig(A);  % Isto não tem um equivalente na forma de comando
[~,D] = eig(A);  % Se você só deseja D e não V



% Operadores Lógicos e Relacionais
1 > 5 % Resposta = 0
10 >= 10 % Resposta = 1
3 ~= 4 % Diferente de -> Resposta = 1
3 == 3 % Igual a -> Resposta = 1
3 > 1 && 4 > 1 % E -> Resposta = 1
3 > 1 || 4 > 1 % OU -> Resposta = 1
~1 % NOT -> Resposta = 0

% Operadores Lógicos e Relacionais podem ser aplicados a matrizes
A > 5
% Para cada elemento, caso seja verdade, esse elemento será 1 na matriz retornada
A( A > 5 )
% Retorna um vetor com os elementos de A para os quais a condição é verdadeira

% Cadeias de caracteres (Strings)
a = 'MinhaString'
length(a) % Resposta = 11
a(2) % Resposta = i
[a,a] % Resposta = MinhaStringMinhaString


% Vetores de células
a = {'um', 'dois', 'três'}
a(1) % Resposta = 'um' - retorna uma célula
char(a(1)) % Resposta = um - retorna uma string

% Estruturas
A.b = {'um','dois'};
A.c = [1 2];
A.d.e = false;

% Vetores
x = [4 32 53 7 1]
x(2) % Resposta = 32, índices no Matlab começam por 1, não 0
x(2:3) % Resposta = 32 53
x(2:end) % Resposta = 32 53 7 1

x = [4; 32; 53; 7; 1] % Vetor coluna

x = [1:10] % x = 1 2 3 4 5 6 7 8 9 10

% Matrizes
A = [1 2 3; 4 5 6; 7 8 9]
% Linhas são separadas por um ponto e vírgula;
% Elementos são separados com espaço ou vírgula
% A =

%     1     2     3
%     4     5     6
%     7     8     9

A(2,3) % Resposta = 6, A(linha, coluna)
A(6) % Resposta = 8
% (implicitamente encadeia as colunas do vetor, e então as indexa)


A(2,3) = 42 % Atualiza a linha 2 coluna 3 com o valor 42
% A =

%     1     2     3
%     4     5     42
%     7     8     9

A(2:3,2:3) % Cria uma nova matriz a partir da antiga
%Resposta =

%     5     42
%     8     9

A(:,1) % Todas as linhas na coluna 1
%Resposta =

%     1
%     4
%     7

A(1,:) % Todas as colunas na linha 1
%Resposta =

%     1     2     3

[A ; A] % Concatenação de matrizes (verticalmente)
%Resposta =

%     1     2     3
%     4     5    42
%     7     8     9
%     1     2     3
%     4     5    42
%     7     8     9

% Isto é o mesmo de
vertcat(A,A);


[A , A] % Concatenação de matrizes (horizontalmente)

%Resposta =

%     1     2     3     1     2     3
%     4     5    42     4     5    42
%     7     8     9     7     8     9

% Isto é o mesmo de
horzcat(A,A);


A(:, [3 1 2]) % Reorganiza as colunas da matriz original
%Resposta =

%     3     1     2
%    42     4     5
%     9     7     8

size(A) % Resposta = 3 3

A(1, :) =[] % Remove a primeira linha da matriz
A(:, 1) =[] % Remove a primeira coluna da matriz

transpose(A) % Transposta a matriz, que é o mesmo de:
A one
ctranspose(A) % Transposta a matriz
% (a transposta, seguida pelo conjugado complexo de cada elemento)




% Aritmética Elemento por Elemento vs. Aritmética com Matriz
% Naturalmente, os operadores aritméticos agem em matrizes inteiras. Quando
% precedidos por um ponto, eles atuam em cada elemento. Por exemplo:
A * B % Multiplicação de matrizes
A .* B % Multiplica cada elemento em A por seu correspondente em B

% Existem vários pares de funções nas quais uma atua sob cada elemento, e a
% outra (cujo nome termina com m) age na matriz por completo.
exp(A) % Exponencia cada elemento
expm(A) % Calcula o exponencial da matriz
sqrt(A) % Tira a raiz quadrada de cada elemento
sqrtm(A) %  Procura a matriz cujo quadrado é A


% Gráficos
x = 0:.10:2*pi; % Vetor que começa em 0 e termina em 2*pi com incrementos de 0,1
y = sin(x);
plot(x,y)
xlabel('eixo x')
ylabel('eixo y')
title('Gráfico de y = sin(x)')
axis([0 2*pi -1 1]) % x vai de 0 a 2*pi, y vai de -1 a 1

plot(x,y1,'-',x,y2,'--',x,y3,':') % Para várias funções em um só gráfico
legend('Descrição linha 1', 'Descrição linha 2') % Curvas com uma legenda

% Método alternativo para traçar várias funções em um só gráfico:
% Enquanto 'hold' estiver ativo, os comandos serão adicionados ao gráfico
% existente ao invés de o substituirem.
plot(x, y)
hold on
plot(x, z)
hold off

loglog(x, y) % Plotar em escala loglog
semilogx(x, y) % Um gráfico com eixo x logarítmico
semilogy(x, y) % Um gráfico com eixo y logarítmico

fplot (@(x) x^2, [2,5]) % Plotar a função x^2 para x=2 até x=5

grid on % Exibe as linhas de grade; Oculta com 'grid off'
axis square % Torna quadrada a região dos eixos atuais
axis equal % Taxa de proporção onde as unidades serão as mesmas em todas direções

scatter(x, y); % Gráfico de dispersão ou bolha
hist(x); % Histograma

z = sin(x);
plot3(x,y,z); % Plotar em espaço em 3D

pcolor(A) % Mapa de calor da matriz: traça uma grade de retângulos, coloridos pelo valor
contour(A) % Plotar de contorno da matriz
mesh(A) % Plotar malha 3D

h = figure	% Cria uma nova figura objeto, com identificador h
figure(h) % Cria uma nova janela de figura com h
close(h) % Fecha a figura h
close all % Fecha todas as janelas de figuras abertas
close % Fecha a janela de figura atual

shg % Traz uma janela gráfica existente para frente, ou cria uma nova se necessário
clf clear % Limpa a janela de figura atual e redefine a maioria das propriedades da figura

% Propriedades podem ser definidas e alteradas através de um identificador.
% Você pode salvar um identificador para uma figura ao criá-la.
% A função gcf retorna o identificador da figura atual
h = plot(x, y); % Você pode salvar um identificador para a figura ao criá-la
set(h, 'Color', 'r')
% 'y' amarelo; 'm' magenta, 'c' ciano, 'r' vermelho, 'g' verde, 'b' azul, 'w' branco, 'k' preto
set(h, 'LineStyle', '--')
 % '--' linha sólida, '---' tracejada, ':' pontilhada, '-.' traço-ponto, 'none' sem linha
get(h, 'LineStyle')


% A função gca retorna o identificador para os eixos da figura atual
set(gca, 'XDir', 'reverse'); % Inverte a direção do eixo x

% Para criar uma figura que contém vários gráficos use subplot, o qual divide
% a janela de gráficos em m linhas e n colunas.
subplot(2,3,1); % Seleciona a primeira posição em uma grade de 2-por-3
plot(x1); title('Primeiro Plot') % Plota algo nesta posição
subplot(2,3,2); % Seleciona a segunda posição na grade
plot(x2); title('Segundo Plot') % Plota algo ali


% Para usar funções ou scripts, eles devem estar no caminho ou na pasta atual
path % Exibe o caminho atual
addpath /caminho/para/pasta % Adiciona o diretório ao caminho
rmpath /caminho/para/pasta % Remove o diretório do caminho
cd /caminho/para/mudar % Muda o diretório


% Variáveis podem ser salvas em arquivos *.mat
save('meuArquivo.mat') % Salva as variáveis do seu Workspace
load('meuArquivo.mat') % Carrega as variáveis em seu Workspace

% Arquivos M (M-files)
% Um arquivo de script é um arquivo externo contendo uma sequência de instruções.
% Eles evitam que você digite os mesmos códigos repetidamente na janela de comandos.
% Possuem a extensão *.m

% Arquivos M de Funções (M-file Functions)
% Assim como scripts e têm a mesma extensão *.m
% Mas podem aceitar argumentos de entrada e retornar uma saída.
% Além disso, possuem seu próprio workspace (ex. diferente escopo de variáveis).
% O nome da função deve coincidir com o nome do arquivo (salve o exemplo como dobra_entrada.m)
% 'help dobra_entrada.m' retorna os comentários abaixo da linha de início da função
function output = dobra_entrada(x)
	%dobra_entrada(x) retorna duas vezes o valor de x
	output = 2*x;
end
dobra_entrada(6) % Resposta = 12


% Você também pode ter subfunções e funções aninhadas.
% Subfunções estão no mesmo arquivo da função primária, e só podem ser chamados
% por funções dentro do arquivo. Funções aninhadas são definidas dentro de
% outras funções, e têm acesso a ambos workspaces.

% Se você quer criar uma função sem criar um novo arquivo, você pode usar uma
% função anônima. Úteis para definir rapidamente uma função para passar a outra
% função (ex. plotar com fplot, avaliar uma integral indefinida com quad,
% procurar raízes com fzero, ou procurar mínimo com fminsearch).
% Exemplo que retorna o quadrado de sua entrada, atribuído ao identificador sqr:
sqr = @(x) x.^2;
sqr(10) % Resposta = 100
doc function_handle % Saiba mais

% Entrada do usuário
a = input('Digite o valor: ')

% Para a execução do arquivo e passa o controle para o teclado: o usuário pode
% examinar ou alterar variáveis. Digite 'return' para continuar a execução, ou 'dbquit' para sair
keyboard

% Leitura de dados (ou xlsread/importdata/imread para arquivos excel/CSV/imagem)
fopen(nomedoarquivo)

% Saída
disp(a) % Imprime o valor da variável a
disp('Olá Mundo') % Imprime a string
fprintf % Imprime na janela de comandos com mais controle

% Estruturas Condicionais (os parênteses são opicionais, porém uma boa prática)
if (a > 15)
	disp('Maior que 15')
elseif (a == 23)
	disp('a é 23')
else
	disp('Nenhuma condição reconheceu')
end

% Estruturas de Repetição
% Nota: fazer o loop sobre elementos de um vetor/matriz é lento!
% Sempre que possível, use funções que atuem em todo o vetor/matriz de uma só vez.
for k = 1:5
	disp(k)
end

k = 0;
while (k < 5)
	k = k + 1;
end

% Tempo de Execução de Código (Timing Code Execution): 'toc' imprime o tempo
% passado desde que 'tic' foi chamado.
tic
A = rand(1000);
A*A*A*A*A*A*A;
toc

% Conectando a uma base de dados MySQL
dbname = 'nome_base_de_dados';
username = 'root';
password = 'root';
driver = 'com.mysql.jdbc.Driver';
dburl = ['jdbc:mysql://localhost:8889/' dbname];
%Abaixo, o xx depende da versão, download disponível em http://dev.mysql.com/downloads/connector/j/
javaclasspath('mysql-connector-java-5.1.xx-bin.jar');
conn = database(dbname, username, password, driver, dburl);
sql = ['SELECT * FROM nome_tabela WHERE id = 22'] % Exemplo de uma consulta SQL
a = fetch(conn, sql) %a will contain your data


% Funções Matemáticas Comuns
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
rand % Números pseudo-aleatórios uniformemente distribuídos
randi % Inteiros pseudo-aleatórios uniformemente distribuídos
randn % Números pseudo-aleatórios normalmente distribuídos

% Constantes Comuns
pi
NaN
inf

% Resolvendo equações matriciais (se não houver solução, retorna uma solução de mínimos quadrados)
% Os operadores \ e / são equivalentes às funções mldivide e mrdivide
x=A\b % Resolve Ax=b. Mais rápido e numericamente mais preciso do que inv(A)*b.
x=b/A % Resolve xA=b

inv(A) % Calcula a matriz inversa
pinv(A) % Calcula a pseudo-inversa

% Funções Matriciais Comuns
zeros(m,n) % Matriz de zeros m x n
ones(m,n) % Matriz de 1's m x n
diag(A) % Extrai os elementos diagonais da matriz A
diag(x) % Constrói uma matriz com os elementos diagonais listados em x, e zero nas outras posições
eye(m,n) % Matriz identidade
linspace(x1, x2, n) % Retorna n pontos igualmente espaçados, com min x1 e max x2
inv(A) % Inverso da matriz A
det(A) % Determinante da matriz A
eig(A) % Valores e vetores próprios de A
trace(A) % Traço da matriz - equivalente a sum(diag(A))
isempty(A) % Testa se a matriz está vazia
all(A) % Testa se todos os elementos são diferentes de zero ou verdadeiro
any(A) % Testa se algum elemento é diferente de zero ou verdadeiro
isequal(A, B) % Testa a igualdade de duas matrizes
numel(A) % Número de elementos na matriz
triu(x) % Retorna a parte triangular superior de x
tril(x) % Retorna a parte triangular inferior de x
cross(A,B) %  Retorna o produto cruzado das matrizes A e B
dot(A,B) % Retorna o produto escalar de duas matrizes (devem possuir mesmo tamanho)
transpose(A) % Retorna a matriz transposta de A
fliplr(A) % Inverte a matriz da esquerda para a direita
flipud(A) % Inverte a matriz de cima para baixo

% Fatorações de Matrizes
% Decomposição LU: PA = LU,L é triangular inferior, U é triangular superior, P é a matriz de permutação
[L, U, P] = lu(A)
% Decomposição em Autovalores: AP = PD, colunas de P são autovetores e as diagonais de D são autovalores
[P, D] = eig(A)
% SVD: XV = US, U e V são matrizes unitárias, S possui elementos não negativos na diagonal em ordem decrescente
[U,S,V] = svd(X)

% Funções Vetoriais Comuns
max     % Maior componente
min     % Menor componente
length  % Tamanho do vetor
sort    % Ordena em orcer ascendente
sum     % Soma de elementos
prod    % Produto de elementos
mode	% Valor modal
median  % Valor mediano
mean    % Valor médio
std     % Desvio padrão
perms(x) % Lista todas as permutações de elementos de x


% Classes
% Matlab pode suportar programação orientada a objetos.
% Classes devem ser colocadas em um arquivo de mesmo nome com a extensão *.m
% Para começar, criamos uma simples classe que armazena posições de GPS
% Início ClassePosicoesGPS.m
classdef ClassePosicoesGPS % O nome da classe.
  properties % As propriedades da classe comportam-se como estruturas
    latitude 
    longitude 
  end
  methods 
    % Este método que tem o mesmo nome da classe é o construtor.
    function obj = ClassePosicoesGPS(lat, lon)
      obj.latitude = lat;
      obj.longitude = lon;
    end

    % Outras funções que usam os objetos de PosicoesGPS
    function r = multiplicarLatPor(obj, n)
      r = n*[obj.latitude];
    end

    % Se quisermos somar dois objetos de PosicoesGPS juntos sem chamar
    % uma função especial nós podemos sobrepor a aritmética do Matlab, desta maneira:
    function r = plus(o1,o2)
      r = ClassePosicoesGPS([o1.latitude] +[o2.latitude], ...
                        [o1.longitude]+[o2.longitude]);
    end
  end
end
% End ClassePosicoesGPS.m

% Podemos criar um objeto da classe usando o construtor
a = ClassePosicoesGPS(45.0, 45.0)

% Propriedades da classe se comportam exatamente como estruturas Matlab
a.latitude = 70.0
a.longitude = 25.0

% Métodos podem ser chamados da mesma forma que funções
ans = multiplicarLatPor(a,3)

% O método também pode ser chamado usando a notação de ponto. Neste caso,
% o objeto não precisa ser passado para o método.
ans = a.multiplicarLatPor(a,1/3)

% Funções do Matlab podem ser sobrepostas para lidar com objetos.
% No método abaixo, nós sobrepomos a forma como o Matlab lida com a soma de
% dois objetos PosicoesGPS.
b = ClassePosicoesGPS(15.0, 32.0)
c = a + b

```

## Mais sobre Matlab

* O site oficial [http://http://www.mathworks.com/products/matlab/](http://www.mathworks.com/products/matlab/)
* O fórum oficial de respostas: [http://www.mathworks.com/matlabcentral/answers/](http://www.mathworks.com/matlabcentral/answers/)

