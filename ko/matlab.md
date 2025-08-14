---
name: MATLAB
filename: learnmatlab.m
contributors:
    - ["mendozao", "http://github.com/mendozao"]
    - ["jamesscottbrown", "http://jamesscottbrown.com"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Claudson Martins", "http://github.com/claudsonm"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

MATLAB은 MATrix LABoratory의 약자입니다. 공학 및 수학에서 일반적으로 사용되는 강력한 수치 계산 언어입니다.

```matlab
%% 코드 섹션은 두 개의 퍼센트 기호로 시작합니다. 섹션 제목은 같은 줄에 씁니다.
% 주석은 퍼센트 기호로 시작합니다.

%{
여러 줄 주석은
다음과
같습니다
%}

% 두 개의 퍼센트 기호는 새 코드 섹션의 시작을 나타냅니다
% 개별 코드 섹션은 커서를 섹션으로 이동한 다음
% "섹션 실행" 버튼을 클릭하거나
% 또는 Ctrl+Shift+Enter(Windows) 또는 Cmd+Shift+Return(macOS)을 사용하여 실행할 수 있습니다.

%% 이것은 코드 섹션의 시작입니다
%  섹션을 사용하는 한 가지 방법은 데이터 로드와 같이 비용이 많이 들지만 변경되지 않는 시작 코드를 분리하는 것입니다.
load myFile.mat y

%% 이것은 또 다른 코드 섹션입니다
%  이 섹션은 자체적으로 반복적으로 편집하고 실행할 수 있으며, 탐색적 프로그래밍 및 데모에 유용합니다.
A = A * 2;
plot(A);

%% 코드 섹션은 코드 셀 또는 셀 모드라고도 합니다(셀 배열과 혼동하지 마십시오).


% 명령어는 '...'을 사용하여 여러 줄에 걸쳐 작성할 수 있습니다:
 a = 1 + 2 + ...
 + 4

% 명령어를 운영 체제에 전달할 수 있습니다
!ping google.com

who          % 메모리의 모든 변수를 표시합니다
whos         % 메모리의 모든 변수를 유형과 함께 표시합니다
clear        % 메모리에서 모든 변수를 지웁니다
clear('A')   % 특정 변수를 지웁니다
openvar('A') % 변수 편집기에서 변수를 엽니다

clc    % 명령 창의 내용을 지웁니다
diary  % 명령 창 텍스트를 파일에 쓰는 것을 토글합니다
ctrl-c % 현재 계산을 중단합니다

edit('myfunction.m') % 편집기에서 함수/스크립트를 엽니다
type('myfunction.m') % 함수/스크립트의 소스를 명령 창에 인쇄합니다

profile on     % 코드 프로파일러를 켭니다
profile off    % 코드 프로파일러를 끕니다
profile viewer % 프로파일러를 엽니다

help command         % 명령 창에 명령어에 대한 문서를 표시합니다
doc command          % 도움말 창에 명령어에 대한 문서를 표시합니다
lookfor command      % 모든 함수의 첫 번째 주석 줄에서 명령어를 검색합니다
lookfor command -all % 모든 함수에서 명령어를 검색합니다


% 출력 서식
format short    % 부동 소수점 수에서 소수점 4자리
format long     % 소수점 15자리
format bank     % 소수점 뒤 두 자리만 - 금융 계산용
fprintf('text') % 화면에 "text"를 인쇄합니다
disp('text')    % 화면에 "text"를 인쇄합니다

% 변수 및 표현식
myVariable = 4  % 작업 공간 창에 새로 생성된 변수가 표시됩니다
myVariable = 4; % 세미콜론은 명령 창에 출력을 억제합니다
4 + 6           % ans = 10
8 * myVariable  % ans = 32
2 ^ 3           % ans = 8
a = 2; b = 3;
c = exp(a)*sin(pi/2) % c = 7.3891

% 함수 호출은 두 가지 방법 중 하나로 수행할 수 있습니다:
% 표준 함수 구문:
load('myFile.mat', 'y') % 괄호 안에 인수를 쉼표로 구분하여 전달합니다
% 명령어 구문:
load myFile.mat y       % 괄호 없음, 쉼표 대신 공백 사용
% 명령어 형식에서는 따옴표가 없음에 유의하십시오: 입력은 항상
% 리터럴 텍스트로 전달됩니다 - 변수 값을 전달할 수 없습니다. 또한 출력을 받을 수 없습니다:
[V,D] = eig(A);  % 이것은 명령어 형식에 해당하는 것이 없습니다
[~,D] = eig(A);  % V가 아닌 D만 원하는 경우



% 논리
1 > 5 % ans = 0
10 >= 10 % ans = 1
3 ~= 4 % 같지 않음 -> ans = 1
3 == 3 % 같음 -> ans = 1
3 > 1 && 4 > 1 % AND -> ans = 1
3 > 1 || 4 > 1 % OR -> ans = 1
~1 % NOT -> ans = 0

% 논리는 행렬에 적용될 수 있습니다:
A > 5
% 각 요소에 대해 조건이 참이면 반환된 행렬에서 해당 요소는 1입니다
A( A > 5 )
% 조건이 참인 A의 요소를 포함하는 벡터를 반환합니다

% 문자열
a = 'MyString'
length(a) % ans = 8
a(2) % ans = y
[a,a] % ans = MyStringMyString


% 셀
a = {'one', 'two', 'three'}
a(1) % ans = 'one' - 셀을 반환합니다
char(a(1)) % ans = one - 문자열을 반환합니다

% 구조체
A.b = {'one','two'};
A.c = [1 2];
A.d.e = false;

% 벡터
x = [4 32 53 7 1]
x(2) % ans = 32, MATLAB의 인덱스는 0이 아닌 1부터 시작합니다
x(2:3) % ans = 32 53
x(2:end) % ans = 32 53 7 1

x = [4; 32; 53; 7; 1] % 열 벡터

x = [1:10] % x = 1 2 3 4 5 6 7 8 9 10
x = [1:2:10] % 2씩 증가, 즉 x = 1 3 5 7 9

% 행렬
A = [1 2 3; 4 5 6; 7 8 9]
% 행은 세미콜론으로 구분됩니다. 요소는 공백이나 쉼표로 구분됩니다.
% A =

%     1     2     3
%     4     5     6
%     7     8     9

A(2,3) % ans = 6, A(행, 열)
A(6) % ans = 8
% (암시적으로 열을 벡터로 연결한 다음 해당 벡터를 인덱싱합니다)


A(2,3) = 42 % 2행 3열을 42로 업데이트합니다
% A =

%     1     2     3
%     4     5     42
%     7     8     9

A(2:3,2:3) % 이전 행렬에서 새 행렬을 만듭니다
%ans =

%     5     42
%     8     9

A(:,1) % 1열의 모든 행
%ans =

%     1
%     4
%     7

A(1,:) % 1행의 모든 열
%ans =

%     1     2     3

[A ; A] % 행렬 연결 (수직)
%ans =

%     1     2     3
%     4     5    42
%     7     8     9
%     1     2     3
%     4     5    42
%     7     8     9

% 이것은 다음과 같습니다
vertcat(A,A);


[A , A] % 행렬 연결 (수평)

%ans =

%     1     2     3     1     2     3
%     4     5    42     4     5    42
%     7     8     9     7     8     9

% 이것은 다음과 같습니다
horzcat(A,A);


A(:, [3 1 2]) % 원본 행렬의 열을 재정렬합니다
%ans =

%     3     1     2
%    42     4     5
%     9     7     8

size(A) % ans = 3 3

A(1, :) =[] % 행렬의 첫 번째 행을 삭제합니다
A(:, 1) =[] % 행렬의 첫 번째 열을 삭제합니다

transpose(A) % 행렬을 전치합니다. 다음과 같습니다:
A.' % 전치의 간결한 버전 (복소 공액을 취하지 않음)
ctranspose(A) % 행렬을 에르미트 전치합니다. 다음과 같습니다:
A'  % 복소 전치의 간결한 버전
    % (전치 후 각 요소의 복소 공액을 취함)





% 요소별 산술 대 행렬 산술
% 산술 연산자는 자체적으로 전체 행렬에 작용합니다. 앞에
% 마침표가 있으면 대신 각 요소에 작용합니다. 예:
A * B % 행렬 곱셈
A .* B % A의 각 요소를 B의 해당 요소와 곱합니다

% 여러 쌍의 함수가 있으며, 하나는 각 요소에 작용하고
% 다른 하나(이름이 m으로 끝남)는 전체 행렬에 작용합니다.
exp(A) % 각 요소를 지수화합니다
expm(A) % 행렬 지수를 계산합니다
sqrt(A) % 각 요소의 제곱근을 취합니다
sqrtm(A) %  제곱이 A인 행렬을 찾습니다


% 플로팅
x = 0:.10:2*pi; % 0에서 시작하여 2*pi에서 끝나고 .1씩 증가하는 벡터를 만듭니다
y = sin(x);
plot(x,y)
xlabel('x 축')
ylabel('y 축')
title('y = sin(x)의 플롯')
axis([0 2*pi -1 1]) % x 범위는 0에서 2*pi, y 범위는 -1에서 1

plot(x,y1,'-',x,y2,'--',x,y3,':') % 한 플롯에 여러 함수를 표시합니다
legend('선 1 레이블', '선 2 레이블') % 범례로 곡선에 레이블을 지정합니다

% 한 플롯에 여러 함수를 표시하는 다른 방법.
% 'hold'가 켜져 있는 동안 명령어는 기존 그래프를 대체하는 대신 추가됩니다.
plot(x, y)
hold on
plot(x, z)
hold off

loglog(x, y) % 로그-로그 플롯
semilogx(x, y) % x축이 로그 스케일인 플롯
semilogy(x, y) % y축이 로그 스케일인 플롯

fplot (@(x) x^2, [2,5]) % x=2에서 x=5까지 함수 x^2를 플로팅합니다

grid on % 그리드 표시; 'grid off'로 끕니다
axis square % 현재 축 영역을 정사각형으로 만듭니다
axis equal % 데이터 단위가 모든 방향에서 동일하도록 가로세로 비율을 설정합니다

scatter(x, y); % 산점도
hist(x); % 히스토그램
stem(x); % 값을 줄기로 플로팅하여 이산 데이터를 표시하는 데 유용합니다
bar(x); % 막대 그래프

z = sin(x);
plot3(x,y,z); % 3D 선 플롯

pcolor(A) % 행렬의 히트맵: 값에 따라 색상이 지정된 사각형 그리드로 플로팅합니다
contour(A) % 행렬의 등고선 플롯
mesh(A) % 메시 표면으로 플로팅합니다

h = figure % 핸들 h를 사용하여 새 그림 객체를 만듭니다
figure(h) % 핸들 h에 해당하는 그림을 현재 그림으로 만듭니다
close(h) % 핸들 h를 사용하여 그림을 닫습니다
close all % 모든 열린 그림 창을 닫습니다
close % 현재 그림 창을 닫습니다

shg % 기존 그래픽 창을 앞으로 가져오거나 필요한 경우 새 창을 만듭니다
clf clear % 현재 그림 창을 지우고 대부분의 그림 속성을 재설정합니다

% 속성은 그림 핸들을 통해 설정하고 변경할 수 있습니다.
% 그림을 만들 때 핸들을 저장할 수 있습니다.
% get 함수는 현재 그림에 대한 핸들을 반환합니다
h = plot(x, y); % 그림을 만들 때 핸들을 저장할 수 있습니다
set(h, 'Color', 'r')
% 'y' 노란색; 'm' 자홍색, 'c' 청록색, 'r' 빨간색, 'g' 녹색, 'b' 파란색, 'w' 흰색, 'k' 검은색
set(h, 'LineStyle', '--')
 % '--'는 실선, '---' 파선, ':' 점선, '-.' 파선-점선, 'none'은 선 없음
get(h, 'LineStyle')


% gca 함수는 현재 그림의 축에 대한 핸들을 반환합니다
set(gca, 'XDir', 'reverse'); % x축의 방향을 반전시킵니다

% 타일 위치에 여러 축을 포함하는 그림을 만들려면 subplot을 사용하십시오
subplot(2,3,1); % 2x3 서브플롯 그리드의 첫 번째 위치를 선택합니다
plot(x1); title('첫 번째 플롯') % 이 위치에 무언가를 플로팅합니다
subplot(2,3,2); % 그리드의 두 번째 위치를 선택합니다
plot(x2); title('두 번째 플롯') % 거기에 무언가를 플로팅합니다


% 함수나 스크립트를 사용하려면 경로 또는 현재 디렉토리에 있어야 합니다
path % 현재 경로 표시
addpath /path/to/dir % 경로에 추가
rmpath /path/to/dir % 경로에서 제거
cd /path/to/move/into % 디렉토리 변경


% 변수는 .mat 파일에 저장할 수 있습니다
save('myFileName.mat') % 작업 공간의 변수를 저장합니다
load('myFileName.mat') % 저장된 변수를 작업 공간으로 로드합니다

% M-파일 스크립트
% 스크립트 파일은 일련의 문장을 포함하는 외부 파일입니다.
% 명령 창에서 동일한 코드를 반복적으로 입력하는 것을 피할 수 있습니다
% .m 확장자를 가집니다

% M-파일 함수
% 스크립트와 같고 동일한 .m 확장자를 가집니다
% 그러나 입력 인수를 받고 출력을 반환할 수 있습니다
% 또한 자체 작업 공간(즉, 다른 변수 범위)을 가집니다.
% 함수 이름은 파일 이름과 일치해야 합니다(따라서 이 예제를 double_input.m으로 저장하십시오).
% 'help double_input.m'은 함수 시작 줄 아래의 주석을 반환합니다
function output = double_input(x)
  %double_input(x)은 x 값의 두 배를 반환합니다
  output = 2*x;
end
double_input(6) % ans = 12


% 하위 함수 및 중첩 함수도 가질 수 있습니다.
% 하위 함수는 기본 함수와 동일한 파일에 있으며 파일의 함수에서만
% 호출할 수 있습니다. 중첩 함수는 다른 함수 내에 정의되며
% 해당 작업 공간과 자체 작업 공간 모두에 액세스할 수 있습니다.

% 새 파일을 만들지 않고 함수를 만들고 싶다면
% 익명 함수를 사용할 수 있습니다. 다른 함수에 전달할 함수를
% 빠르게 정의할 때 유용합니다(예: fplot으로 플로팅, quad로 부정적분 평가,
% fzero로 근 찾기 또는 fminsearch로 최소값 찾기).
% 입력의 제곱을 반환하는 예제, 핸들 sqr에 할당됨:
sqr = @(x) x.^2;
sqr(10) % ans = 100
doc function_handle % 자세히 알아보기

% 사용자 입력
a = input('값을 입력하십시오: ')

% 파일 실행을 중지하고 키보드에 제어권을 부여합니다: 사용자는
% 변수를 검사하거나 변경할 수 있습니다. 실행을 계속하려면 'return'을 입력하고
% 종료하려면 'dbquit'를 입력하십시오
keyboard

% 데이터 읽기 (excel/CSV/이미지 파일의 경우 xlsread/importdata/imread도 사용)
fopen(filename)

% 출력
disp(a) % 변수 a의 값을 인쇄합니다
disp('Hello World') % 문자열을 인쇄합니다
fprintf % 더 많은 제어로 명령 창에 인쇄합니다

% 조건문 (괄호는 선택 사항이지만 좋은 스타일입니다)
if (a > 23)
  disp('23보다 큼')
elseif (a == 23)
  disp('a는 23입니다')
else
  disp('두 조건 모두 충족되지 않음')
end

% 반복
% NB. 벡터/행렬의 요소를 반복하는 것은 느립니다!
% 가능한 경우 한 번에 전체 벡터/행렬에 작용하는 함수를 사용하십시오
for k = 1:5
  disp(k)
end

k = 0;
while (k < 5)
  k = k + 1;
end

% 코드 실행 시간 측정: 'toc'는 'tic'이 호출된 이후의 시간을 인쇄합니다
tic
A = rand(1000);
A*A*A*A*A*A*A;
toc

% MySQL 데이터베이스에 연결
dbname = 'database_name';
username = 'root';
password = 'root';
driver = 'com.mysql.jdbc.Driver';
dburl = ['jdbc:mysql://localhost:8889/' dbname];
javaclasspath('mysql-connector-java-5.1.xx-bin.jar'); %xx는 버전에 따라 다름, http://dev.mysql.com/downloads/connector/j/에서 다운로드 가능
conn = database(dbname, username, password, driver, dburl);
sql = ['SELECT * from table_name where id = 22'] % 예제 sql 문
a = fetch(conn, sql) %a에 데이터가 포함됩니다


% 일반적인 수학 함수
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
abs(x) %x가 복소수이면 크기를 반환합니다
min(x)
max(x)
ceil(x)
floor(x)
round(x)
rem(x)
rand % 균일하게 분포된 의사 난수
randi % 균일하게 분포된 의사 난수 정수
randn % 정규 분포된 의사 난수

%복소수 수학 연산
abs(x)   % 복소 변수 x의 크기
phase(x) % 복소 변수 x의 위상(또는 각도)
real(x)  % x의 실수부를 반환합니다 (즉, x = a +jb이면 a를 반환)
imag(x)  % x의 허수부를 반환합니다 (즉, x = a+jb이면 b를 반환)
conj(x)  % 복소 공액을 반환합니다


% 일반적인 상수
pi
NaN
inf

% 행렬 방정식 풀기 (해가 없으면 최소 제곱 해를 반환)
% \ 및 / 연산자는 mldivide 및 mrdivide 함수와 동일합니다
x=A\b % Ax=b를 풉니다. inv(A)*b를 사용하는 것보다 빠르고 수치적으로 더 정확합니다.
x=b/A % xA=b를 풉니다

inv(A) % 역행렬 계산
pinv(A) % 유사 역행렬 계산

% 일반적인 행렬 함수
zeros(m,n) % 0으로 채워진 m x n 행렬
ones(m,n) % 1로 채워진 m x n 행렬
diag(A) % 행렬 A의 대각선 요소를 추출합니다
diag(x) % 대각선 요소가 x에 나열되고 다른 곳은 0인 행렬을 구성합니다
eye(m,n) % 단위 행렬
linspace(x1, x2, n) % 최소 x1, 최대 x2로 n개의 등간격 점을 반환합니다
inv(A) % 행렬 A의 역행렬
det(A) % A의 행렬식
eig(A) % A의 고유값 및 고유 벡터
trace(A) % 행렬의 트레이스 - sum(diag(A))와 동일
isempty(A) % 배열이 비어 있는지 테스트합니다
all(A) % 모든 요소가 0이 아니거나 참인지 테스트합니다
any(A) % 요소 중 0이 아니거나 참인 것이 있는지 테스트합니다
isequal(A, B) % 두 배열의 동등성을 테스트합니다
numel(A) % 행렬의 요소 수
triu(x) % x의 상삼각 부분을 반환합니다
tril(x) % x의 하삼각 부분을 반환합니다
cross(A,B) %  벡터 A와 B의 외적을 반환합니다
dot(A,B) % 두 벡터의 스칼라 곱을 반환합니다 (길이가 같아야 함)
transpose(A) % A의 전치를 반환합니다
fliplr(A) % 행렬을 왼쪽에서 오른쪽으로 뒤집습니다
flipud(A) % 행렬을 위에서 아래로 뒤집습니다

% 행렬 분해
[L, U, P] = lu(A) % LU 분해: PA = LU, L은 하삼각, U는 상삼각, P는 순열 행렬
[P, D] = eig(A) % 고유값 분해: AP = PD, P의 열은 고유 벡터, D의 대각선은 고유값
[U,S,V] = svd(X) % SVD: XV = US, U와 V는 유니터리 행렬, S는 감소 순서로 음이 아닌 대각선 요소를 가짐

% 일반적인 벡터 함수
max     % 가장 큰 구성 요소
min     % 가장 작은 구성 요소
length  % 벡터의 길이
sort    % 오름차순 정렬
sum     % 요소의 합
prod    % 요소의 곱
mode    % 최빈값
median  % 중앙값
mean    % 평균값
std     % 표준 편차
perms(x) % x 요소의 모든 순열 나열
find(x) % x의 모든 0이 아닌 요소를 찾아 인덱스를 반환하며, 비교 연산자를 사용할 수 있습니다.
        % 즉, find( x == 3 )은 3과 같은 요소의 인덱스를 반환합니다
        % 즉, find( x >= 3 )은 3보다 크거나 같은 요소의 인덱스를 반환합니다


% 클래스
% MATLAB은 객체 지향 프로그래밍을 지원할 수 있습니다.
% 클래스는 .m 확장자를 가진 클래스 이름의 파일에 넣어야 합니다.
% 시작하려면 GPS 웨이포인트를 저장하는 간단한 클래스를 만듭니다.
% WaypointClass.m 시작
classdef WaypointClass % 클래스 이름.
  properties % 클래스의 속성은 구조체처럼 작동합니다
    latitude
    longitude
  end
  methods
    % 클래스와 이름이 같은 이 메서드는 생성자입니다.
    function obj = WaypointClass(lat, lon)
      obj.latitude = lat;
      obj.longitude = lon;
    end

    % Waypoint 객체를 사용하는 다른 함수
    function r = multiplyLatBy(obj, n)
      r = n*[obj.latitude];
    end

    % 특수 함수를 호출하지 않고 두 Waypoint 객체를 더하고 싶다면
    % 다음과 같이 MATLAB의 산술을 오버로드할 수 있습니다:
    function r = plus(o1,o2)
      r = WaypointClass([o1.latitude] +[o2.latitude], ...
                        [o1.longitude]+[o2.longitude]);
    end
  end
end
% WaypointClass.m 끝

% 생성자를 사용하여 클래스의 객체를 만들 수 있습니다
a = WaypointClass(45.0, 45.0)

% 클래스 속성은 MATLAB 구조체와 똑같이 작동합니다.
a.latitude = 70.0
a.longitude = 25.0

% 메서드는 함수와 같은 방식으로 호출할 수 있습니다
ans = multiplyLatBy(a,3)

% 메서드는 점 표기법을 사용하여 호출할 수도 있습니다. 이 경우 객체를
% 메서드에 전달할 필요가 없습니다.
ans = a.multiplyLatBy(1/3)

% MATLAB 함수는 객체를 처리하도록 오버로드될 수 있습니다.
% 위 메서드에서 MATLAB이 두 Waypoint 객체의
% 덧셈을 처리하는 방법을 오버로드했습니다.
b = WaypointClass(15.0, 32.0)
c = a + b
```

## MATLAB에 대해 더 알아보기

* [공식 웹사이트](http://www.mathworks.com/products/matlab/)
* [공식 MATLAB 답변 포럼](http://www.mathworks.com/matlabcentral/answers/)
* [Loren on the Art of MATLAB](http://blogs.mathworks.com/loren/)
* [Cleve's Corner](http://blogs.mathworks.com/cleve/)
