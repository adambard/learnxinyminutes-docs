---
language: Matlab
contributors:
    - ["mendozao", "http://github.com/mendozao"]
---

Matlab stands for Matrix Laboratory. It is a powerful numerical computing language commonly used in engineering and mathematics.  

If you have any feedback please feel free to reach me at
[@the_ozzinator](https://twitter.com/the_ozzinator), or
[osvaldo.t.mendoza@gmail.com](mailto:osvaldo.t.mendoza@gmail.com).

```Matlab



% Comments start with a percent sign.

%{ Multi line comments look 
something
like
this %}

clear % Erases all your variables from memory
clc % Erases the writing on your Command Window
who % Displays all variables in memory
diary % History of session
ctrl-c % Abort current computation

help command % Displays documentation for command in Command Window
lookfor command % Searches for a given command


% Output formatting
format short % 4 decimals in a floating number
format long % 15 decimals
fprintf 

% Variables & Expressions
myVariable = 4 % Notice Workspace pane shows newly created variable
myVariable = 4; % Semi colon suppresses output to the Command Window
4 + 6  % ans = 10 
8 * myVariable % ans = 32 
a = 2; b = 3; 
c = exp(a)*sin(pi/2) % c = 7.3891

% Logicals
1 > 5 % ans = 0
10 >= 10 % ans = 1
3 ~= 4 % Not equal to -> ans = 1
3 == 3 % equal to -> ans = 1
3 > 1 && 4 > 1 % AND -> ans = 1
3 > 1 || 4 > 1 % OR -> ans = 1
~1 % NOT -> ans = 0

% Strings
a = 'MyString'
length(a) % ans = 8
a(2) % ans = y
[a,a] % ans = MyStringMyString


% Cells
a = {'one', 'two', 'three'} 
a(1) % ans = 'one' - returns a cell
char(a(1)) % ans = one - returns a string


% Vectors
x = [4 32 53 7 1] 
x(2) % ans = 32, indices in Matlab start 1, not 0
x(2:3) % ans = 32 53
x(2:end) % ans = 32 53 7 1

x = [4; 32; 53; 7; 1] % Column vector

x = [1:10] % x = 1 2 3 4 5 6 7 8 9 10

% Matrices
A = [1 2 3; 4 5 6; 7 8 9] 
% Rows are seperated with a semi colon, each element is seperated with space or comma
% A =

%     1     2     3
%     4     5     6
%     7     8     9

A(2,3) % ans = 6, A(row, column)
A(2,3) = 42 % Update row 2 col 3 with 42
% A =

%     1     2     3
%     4     5     42
%     7     8     9

A(2:3,2:3) % Creates a new matrix from the old one
%ans =

%     5     42
%     8     9

A(:,1) % All rows in column 1
%ans =

%     1
%     4
%     7

A(1,:) % All columns in row 1
%ans =

%     1     2     3

A(:, [3 1 2]) %Rearrange the columns of original matrix
%ans =

%     3     1     2
%    42     4     5
%     9     7     8

A(1, :) =[] %Delete the first row of the matrix

size(A) % ans = 3 3

A' % Transpose the matrix

[A ; A] % Concatenation of matrices
%ans =

%     1     2     3
%     4     5    42
%     7     8     9
%     1     2     3
%     4     5    42
%     7     8     9


%Element by Element Arithmetic VS Matrix Arithmetic 
A * B % Matrix multiplication
A .* B % Multiple each element in A by its corresponding element in B


%Plotting
x = 0:.10:2*pi % Creates a vector that starts at 0 and ends at 2*pi with increments of .1
y = sin(x)
plot(x,y)
xlabel('x axis')
ylabel('y axis')
title('Plot of y = sin(x)')
axis([0 2*pi -1 1]) % x range from 0 to 2*pi, y range from -1 to 1
plot(x,y1,’-’,x,y2,’--’,x,y3,’:’) % For multiple functions on one plot


% .mat files
% Save the variables in your Workspace 

%M-file Scripts
%A script file is an external file that contains a sequence of statements.
%Better than typing your code in the Command Window
%Have .m extensions


%M-file Functions
%Programs that accept inputs and return an output
%Have .m extensions
% double_input.m - naming your ,m file the same as you call it in the file is required
function output = double_input(x) 
	%double_input(x) returns twice the value of x
	output = 2*x;
end
double_input(6) % ans = 12 

%User input
a = input('Enter the value: ')

%Reading in data
fopen(filename) 

%Output
disp(a) % Print out the value of variable a
disp('Hello World') % Print out a string
fprintf % More control display to Command Window 

%Conditional statements
if a > 15
	disp('Greater than 15')
elseif a == 23
	disp('a is 23')
else
	disp('neither condition met')
end

%Looping
for k = 1:5
	disp(k)
end
	
k = 0;	
while (k < 5)
	k = k + 1;
end


%Connecting to a MySQL Database
dbname = 'database_name';
username = 'root';
password = 'root';
driver = 'com.mysql.jdbc.Driver';
dburl = ['jdbc:mysql://localhost:8889/' dbname];
javaclasspath('mysql-connector-java-5.1.xx-bin.jar'); %xx depends on version, download available at http://dev.mysql.com/downloads/connector/j/
conn = database(dbname, username, password, driver, dburl); 
sql = ['SELECT * from table_name where id = 22'] %Example sql statement
a = fetch(conn, sql) %a will contain your data


% Common math functions
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
rand
randi

% Common constants
pi
NaN
inf

% Common matrix functions
zeros(m,n) % m x n matrix of 0's
ones(m,n) % m x n matrix of 1's
diag(A) % Extracts the diagonal elements of a matrix 
eye(m,n) % Indentity matrix
inv(A) % Inverse of matrix A
det(A) % Determinant of A
eig(A) %Eigenvalues and eigenvectors of A
isempty(A) % Tests if array is empty
isequal(A, B) %Tests equality of two arrays
numel(A) %Number of elements in matrix
triu(x) % Returns the upper triangular part of x
tril(x) % Returns the lower triangular part of x
cross(A,B) %  Returns the cross product of the vectors A and B
dot(A,B) % Returns the scalar product of the vectors A and B.  A and B must be vectors of the same length.
transpose(A) % Returns the transpose of A


```

## More on Matlab

* The official website [http://http://www.mathworks.com/products/matlab/](http://www.mathworks.com/products/matlab/)
