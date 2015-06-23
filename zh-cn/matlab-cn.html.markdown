---
language: Matlab
contributors:
    - ["mendozao", "http://github.com/mendozao"]
    - ["jamesscottbrown", "http://jamesscottbrown.com"]
translators:
    - ["sunxb10", "https://github.com/sunxb10"]
lang: zh-cn
---

MATLAB 是 MATrix LABoratory （矩阵实验室）的缩写，它是一种功能强大的数值计算语言，在工程和数学领域中应用广泛。

如果您有任何需要反馈或交流的内容，请联系本教程作者[@the_ozzinator](https://twitter.com/the_ozzinator)、[osvaldo.t.mendoza@gmail.com](mailto:osvaldo.t.mendoza@gmail.com)。

```matlab
% 以百分号作为注释符

%{
多行注释
可以
这样
表示
%}

% 指令可以随意跨行，但需要在跨行处用 '...' 标明：
 a = 1 + 2 + ...
 + 4

% 可以在MATLAB中直接向操作系统发出指令
!ping google.com

who  % 显示内存中的所有变量
whos  % 显示内存中的所有变量以及它们的类型
clear  % 清除内存中的所有变量
clear('A')  % 清除指定的变量
openvar('A')  % 在变量编辑器中编辑指定变量

clc  % 清除命令窗口中显示的所有指令
diary  % 将命令窗口中的内容写入本地文件
ctrl-c  % 终止当前计算

edit('myfunction.m')  % 在编辑器中打开指定函数或脚本
type('myfunction.m')  % 在命令窗口中打印指定函数或脚本的源码

profile on  % 打开 profile 代码分析工具
profile of  % 关闭 profile 代码分析工具
profile viewer  % 查看 profile 代码分析工具的分析结果

help command    % 在命令窗口中显示指定命令的帮助文档
doc command     % 在帮助窗口中显示指定命令的帮助文档
lookfor command  % 在所有 MATLAB 内置函数的头部注释块的第一行中搜索指定命令
lookfor command -all  % 在所有 MATLAB 内置函数的整个头部注释块中搜索指定命令


% 输出格式
format short    % 浮点数保留 4 位小数
format long     % 浮点数保留 15 位小数
format bank     % 金融格式，浮点数只保留 2 位小数
fprintf('text') % 在命令窗口中显示 "text"
disp('text')    % 在命令窗口中显示 "text"


% 变量与表达式
myVariable = 4  % 命令窗口中将新创建的变量
myVariable = 4; % 加上分号可使命令窗口中不显示当前语句执行结果
4 + 6       % ans = 10 
8 * myVariable  % ans = 32 
2 ^ 3       % ans = 8 
a = 2; b = 3; 
c = exp(a)*sin(pi/2) % c = 7.3891


% 调用函数有两种方式：
% 标准函数语法：
load('myFile.mat', 'y') % 参数放在括号内，以英文逗号分隔
% 指令语法:
load myFile.mat y   % 不加括号，以空格分隔参数
% 注意在指令语法中参数不需要加引号：在这种语法下，所有输入参数都只能是文本文字，
% 不能是变量的具体值，同样也不能是输出变量
[V,D] = eig(A);  % 这条函数调用无法转换成等价的指令语法
[~,D] = eig(A);  % 如果结果中只需要 D 而不需要 V 则可以这样写



% 逻辑运算
1 > 5  % 假，ans = 0
10 >= 10  % 真，ans = 1
3 ~= 4  % 不等于 -> ans = 1
3 == 3  % 等于 -> ans = 1
3 > 1 && 4 > 1  % 与 -> ans = 1
3 > 1 || 4 > 1  % 或 -> ans = 1
~1  % 非 -> ans = 0

% 逻辑运算可直接应用于矩阵，运算结果也是矩阵
A > 5
% 对矩阵中每个元素做逻辑运算，若为真，则在运算结果的矩阵中对应位置的元素就是 1
A( A > 5 )
% 如此返回的向量，其元素就是 A 矩阵中所有逻辑运算为真的元素

% 字符串
a = 'MyString'
length(a)  % ans = 8
a(2)  % ans = y
[a,a]  % ans = MyStringMyString
b = '字符串'  % MATLAB目前已经可以支持包括中文在内的多种文字
length(b)  % ans = 3
b(2)  % ans = 符
[b,b]  % ans = 字符串字符串


% 元组（cell 数组）
a = {'one', 'two', 'three'} 
a(1)  % ans = 'one' - 返回一个元组
char(a(1))  % ans = one - 返回一个字符串


% 结构体
A.b = {'one','two'};
A.c = [1 2];
A.d.e = false;


% 向量
x = [4 32 53 7 1] 
x(2)  % ans = 32，MATLAB中向量的下标索引从1开始，不是0
x(2:3)  % ans = 32 53
x(2:end)  % ans = 32 53 7 1

x = [4; 32; 53; 7; 1]  % 列向量

x = [1:10]  % x = 1 2 3 4 5 6 7 8 9 10


% 矩阵
A = [1 2 3; 4 5 6; 7 8 9] 
% 以分号分隔不同的行，以空格或逗号分隔同一行中的不同元素
% A =

%     1     2     3
%     4     5     6
%     7     8     9

A(2,3) % ans = 6，A(row, column)
A(6) % ans = 8 
% （隐式地将 A 的三列首尾相接组成一个列向量，然后取其下标为 6 的元素）


A(2,3) = 42  % 将第 2 行第 3 列的元素设为 42
% A =

%     1     2     3
%     4     5     42
%     7     8     9

A(2:3,2:3)  % 取原矩阵中的一块作为新矩阵
%ans =

%     5     42
%     8     9

A(:,1)  % 第 1 列的所有元素
%ans =

%     1
%     4
%     7

A(1,:)  % 第 1 行的所有元素
%ans =

%     1     2     3

[A ; A]  % 将两个矩阵上下相接构成新矩阵
%ans =

%     1     2     3
%     4     5    42
%     7     8     9
%     1     2     3
%     4     5    42
%     7     8     9

% 等价于 
vertcat(A, A);


[A , A]  % 将两个矩阵左右相接构成新矩阵

%ans =

%     1     2     3     1     2     3
%     4     5    42     4     5    42
%     7     8     9     7     8     9

% 等价于
horzcat(A, A);


A(:, [3 1 2])  % 重新排布原矩阵的各列
%ans =

%     3     1     2
%    42     4     5
%     9     7     8

size(A)  % 返回矩阵的行数和列数，ans = 3 3

A(1, :) =[]  % 删除矩阵的第 1 行
A(:, 1) =[]  % 删除矩阵的第 1 列

transpose(A)  % 矩阵转置，等价于 A'
ctranspose(A)  % 矩阵的共轭转置（对矩阵中的每个元素取共轭复数）


% 元素运算 vs. 矩阵运算
% 单独运算符就是对矩阵整体进行矩阵运算
% 在运算符加上英文句点就是对矩阵中的元素进行元素计算
% 示例如下：
A * B  % 矩阵乘法，要求 A 的列数等于 B 的行数
A .* B  % 元素乘法，要求 A 和 B 形状一致（A 的行数等于 B 的行数， A 的列数等于 B 的列数）
% 元素乘法的结果是与 A 和 B 形状一致的矩阵，其每个元素等于 A 对应位置的元素乘 B 对应位置的元素

% 以下函数中，函数名以 m 结尾的执行矩阵运算，其余执行元素运算： 
exp(A)  % 对矩阵中每个元素做指数运算
expm(A)  % 对矩阵整体做指数运算
sqrt(A)  % 对矩阵中每个元素做开方运算
sqrtm(A)  % 对矩阵整体做开放运算（即试图求出一个矩阵，该矩阵与自身的乘积等于 A 矩阵）


% 绘图
x = 0:.10:2*pi;  % 生成一向量，其元素从 0 开始，以 0.1 的间隔一直递增到 2*pi（pi 就是圆周率）
y = sin(x);
plot(x,y)
xlabel('x axis')
ylabel('y axis')
title('Plot of y = sin(x)')
axis([0 2*pi -1 1])  % x 轴范围是从 0 到 2*pi，y 轴范围是从 -1 到 1

plot(x,y1,'-',x,y2,'--',x,y3,':')  % 在同一张图中绘制多条曲线
legend('Line 1 label', 'Line 2 label')  % 为图片加注图例
% 图例数量应当小于或等于实际绘制的曲线数目，从 plot 绘制的第一条曲线开始对应

% 在同一张图上绘制多条曲线的另一种方法：
% 使用 hold on，令系统保留前次绘图结果并在其上直接叠加新的曲线，
% 如果没有 hold on，则每个 plot 都会首先清除之前的绘图结果再进行绘制。
% 在 hold on 和 hold off 中可以放置任意多的 plot 指令，
% 它们和 hold on 前最后一个 plot 指令的结果都将显示在同一张图中。
plot(x, y1)
hold on
plot(x, y2)
plot(x, y3)
plot(x, y4)
hold off

loglog(x, y)  % 对数—对数绘图
semilogx(x, y)  % 半对数（x 轴对数）绘图
semilogy(x, y)  % 半对数（y 轴对数）绘图

fplot (@(x) x^2, [2,5])  % 绘制函数 x^2 在 [2, 5] 区间的曲线

grid on  % 在绘制的图中显示网格，使用 grid off 可取消网格显示
axis square  % 将当前坐标系设定为正方形（保证在图形显示上各轴等长）
axis equal  % 将当前坐标系设定为相等（保证在实际数值上各轴等长）

scatter(x, y);  % 散点图
hist(x);  % 直方图

z = sin(x);
plot3(x,y,z);  % 绘制三维曲线

pcolor(A)  % 伪彩色图（热图）
contour(A)  % 等高线图
mesh(A)  % 网格曲面图

h = figure  % 创建新的图片对象并返回其句柄 h
figure(h)  % 将句柄 h 对应的图片作为当前图片
close(h)  % 关闭句柄 h 对应的图片
close all  % 关闭 MATLAB 中所用打开的图片
close  % 关闭当前图片

shg  % 显示图形窗口
clf clear  % 清除图形窗口中的图像，并重置图像属性

% 图像属性可以通过图像句柄进行设定
% 在创建图像时可以保存图像句柄以便于设置
% 也可以用 gcf 函数返回当前图像的句柄 
h = plot(x, y);  % 在创建图像时显式地保存图像句柄
set(h, 'Color', 'r')
% 颜色代码：'y' 黄色，'m' 洋红色，'c' 青色，'r' 红色，'g' 绿色，'b' 蓝色，'w' 白色，'k' 黑色
set(h, 'Color', [0.5, 0.5, 0.4])
% 也可以使用 RGB 值指定颜色
set(h, 'LineStyle', '--')
% 线型代码：'--' 实线，'---' 虚线，':' 点线，'-.' 点划线，'none' 不划线
get(h, 'LineStyle')
% 获取当前句柄的线型


% 用 gca 函数返回当前图像的坐标轴句柄
set(gca, 'XDir', 'reverse');  % 令 x 轴反向

% 用 subplot 指令创建平铺排列的多张子图
subplot(2,3,1);  % 选择 2 x 3 排列的子图中的第 1 张图
plot(x1); title('First Plot')  % 在选中的图中绘图
subplot(2,3,2);  % 选择 2 x 3 排列的子图中的第 2 张图
plot(x2); title('Second Plot')  % 在选中的图中绘图


% 要调用函数或脚本，必须保证它们在你的当前工作目录中
path  % 显示当前工作目录
addpath /path/to/dir  % 将指定路径加入到当前工作目录中
rmpath /path/to/dir  % 将指定路径从当前工作目录中删除
cd /path/to/move/into  % 以制定路径作为当前工作目录


% 变量可保存到 .mat 格式的本地文件
save('myFileName.mat')  % 保存当前工作空间中的所有变量 
load('myFileName.mat')  % 将指定文件中的变量载入到当前工作空间 


% .m 脚本文件
% 脚本文件是一个包含多条 MATLAB 指令的外部文件，以 .m 为后缀名
% 使用脚本文件可以避免在命令窗口中重复输入冗长的指令


% .m 函数文件
% 与脚本文件类似，同样以 .m 作为后缀名
% 但函数文件可以接受用户输入的参数并返回运算结果
% 并且函数拥有自己的工作空间（变量域），不必担心变量名称冲突
% 函数文件的名称应当与其所定义的函数的名称一致（比如下面例子中函数文件就应命名为 double_input.m）
% 使用 'help double_input.m' 可返回函数定义中第一行注释信息
function output = double_input(x) 
    % double_input(x) 返回 x 的 2 倍
    output = 2*x;
end
double_input(6)  % ans = 12 


% 同样还可以定义子函数和内嵌函数
% 子函数与主函数放在同一个函数文件中，且只能被这个主函数调用
% 内嵌函数放在另一个函数体内，可以直接访问被嵌套函数的各个变量


% 使用匿名函数可以不必创建 .m 函数文件
% 匿名函数适用于快速定义某函数以便传递给另一指令或函数（如绘图、积分、求根、求极值等）
% 下面示例的匿名函数返回输入参数的平方根，可以使用句柄 sqr 进行调用：
sqr = @(x) x.^2;
sqr(10) % ans = 100
doc function_handle % find out more


% 接受用户输入
a = input('Enter the value: ')


% 从文件中读取数据
fopen(filename)
% 类似函数还有 xlsread（excel 文件）、importdata（CSV 文件）、imread（图像文件）


% 输出
disp(a)  % 在命令窗口中打印变量 a 的值
disp('Hello World')  % 在命令窗口中打印字符串
fprintf  % 按照指定格式在命令窗口中打印内容

% 条件语句（if 和 elseif 语句中的括号并非必需，但推荐加括号避免混淆）
if (a > 15)
    disp('Greater than 15')
elseif (a == 23)
    disp('a is 23')
else
    disp('neither condition met')
end

% 循环语句
% 注意：对向量或矩阵使用循环语句进行元素遍历的效率很低！！
% 注意：只要有可能，就尽量使用向量或矩阵的整体运算取代逐元素循环遍历！！
% MATLAB 在开发时对向量和矩阵运算做了专门优化，做向量和矩阵整体运算的效率高于循环语句
for k = 1:5
    disp(k)
end
    
k = 0;  
while (k < 5)
    k = k + 1;
end


% 程序运行计时：'tic' 是计时开始，'toc' 是计时结束并打印结果
tic
A = rand(1000);
A*A*A*A*A*A*A;
toc


% 链接 MySQL 数据库
dbname = 'database_name';
username = 'root';
password = 'root';
driver = 'com.mysql.jdbc.Driver';
dburl = ['jdbc:mysql://localhost:8889/' dbname];
javaclasspath('mysql-connector-java-5.1.xx-bin.jar');  % 此处 xx 代表具体版本号
% 这里的 mysql-connector-java-5.1.xx-bin.jar 可从 http://dev.mysql.com/downloads/connector/j/ 下载
conn = database(dbname, username, password, driver, dburl); 
sql = ['SELECT * from table_name where id = 22']  % SQL 语句
a = fetch(conn, sql)  % a 即包含所需数据


% 常用数学函数
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
rand  % 均匀分布的伪随机浮点数
randi  % 均匀分布的伪随机整数
randn  % 正态分布的伪随机浮点数

% 常用常数
pi
NaN
inf

% 求解矩阵方程（如果方程无解，则返回最小二乘近似解）
% \ 操作符等价于 mldivide 函数，/ 操作符等价于 mrdivide 函数
x=A\b  % 求解 Ax=b，比先求逆再左乘 inv(A)*b 更加高效、准确
x=b/A  % 求解 xA=b

inv(A)  % 逆矩阵
pinv(A)  % 伪逆矩阵


% 常用矩阵函数
zeros(m, n)  % m x n 阶矩阵，元素全为 0
ones(m, n)  % m x n 阶矩阵，元素全为 1
diag(A)  % 返回矩阵 A 的对角线元素
diag(x)  % 构造一个对角阵，对角线元素就是向量 x 的各元素 
eye(m, n)  % m x n 阶单位矩阵
linspace(x1, x2, n)  % 返回介于 x1 和 x2 之间的 n 个等距节点
inv(A)  % 矩阵 A 的逆矩阵
det(A)  % 矩阵 A 的行列式
eig(A)  % 矩阵 A 的特征值和特征向量
trace(A)  % 矩阵 A 的迹（即对角线元素之和），等价于 sum(diag(A))
isempty(A)  % 测试 A 是否为空
all(A)  % 测试 A 中所有元素是否都非 0 或都为真（逻辑值）
any(A)  % 测试 A 中是否有元素非 0 或为真（逻辑值）
isequal(A, B)  % 测试 A 和 B是否相等
numel(A)  % 矩阵 A 的元素个数
triu(x)  % 返回 x 的上三角这部分
tril(x)  % 返回 x 的下三角这部分
cross(A, B)  % 返回 A 和 B 的叉积（矢量积、外积）
dot(A, B)  % 返回 A 和 B 的点积（数量积、内积），要求 A 和 B 必须等长
transpose(A)  % A 的转置，等价于 A'
fliplr(A)  % 将一个矩阵左右翻转
flipud(A)  % 将一个矩阵上下翻转

% 矩阵分解
[L, U, P] = lu(A)  % LU 分解：PA = LU，L 是下三角阵，U 是上三角阵，P 是置换阵
[P, D] = eig(A)  % 特征值分解：AP = PD，D 是由特征值构成的对角阵，P 的各列就是对应的特征向量
[U, S, V] = svd(X)  % 奇异值分解：XV = US，U 和 V 是酉矩阵，S 是由奇异值构成的半正定实数对角阵

% 常用向量函数
max     % 最大值
min     % 最小值 
length  % 元素个数
sort    % 按升序排列 
sum     % 各元素之和 
prod    % 各元素之积
mode    % 众数
median  % 中位数 
mean    % 平均值 
std     % 标准差
perms(x) % x 元素的全排列

```

## 相关资料

* 官方网页：[http://http://www.mathworks.com/products/matlab/](http://www.mathworks.com/products/matlab/)
* 官方论坛：[http://www.mathworks.com/matlabcentral/answers/](http://www.mathworks.com/matlabcentral/answers/)
