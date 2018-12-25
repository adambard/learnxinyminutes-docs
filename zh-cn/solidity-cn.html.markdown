---
language: Solidity
filename: learnSolidity-cn.sol
lang: zh-cn
contributors:
  - ["Nemil Dalal", "https://www.nemil.com"]
  - ["Joseph Chow", ""]
  - ["Bhoomtawath Plinsut", "https://github.com/varshard"]
  - ["Shooter", "https://github.com/liushooter"]
translators:
  - ["Bob Jiang", "https://github.com/bobjiang"]
---

Solidity 使你在[以太坊](https://www.ethereum.org/)上编程，一个基于区块链的虚拟机，
允许创建和执行智能合约，无需中心化的或可信的一方。

Solidity 是一种与 Javascript 和 C 的相似的、静态类型的合约编程语言。与OOP（面向对象）中
的对象一样，每个合约都包含状态变量、函数和公共数据类型。合约特定功能包括修饰符（guard）子句，
事件通知的侦听器及自定义的全局变量。

以太坊合约的例子包括众筹、投票以及盲拍（私密拍卖）。

Solidity 代码中存在高风险和高成本的错误，因此你必须非常小心地进行测试并慢慢地发布。**随着
以太坊的快速变化，本文档不可能是最新的，所以你应该关注最新的的 solidity 聊天室和以太网博客。
照搬这里的代码，会存在重大错误或弃用代码模式的风险。（说人话--别照抄例子中的代码）**

与其他代码不同，可能还需要添加如暂停、弃用和限制使用的设计模式，来降低风险。本文档主要讨论语法，
因此排除了许多流行的设计模式。

由于 Solidity 和以太坊正在积极开发，通常会标记为实验或 beta 特性，并很可能会更改。因此欢迎
提交更改请求。

```javascript
// 首先，一个简单的银行合约
// 允许存款、取款、以及检查余额

// simple_bank.sol (注意 .sol 后缀)
/* **** 例子开始 **** */

// 声明源文件的编译器版本
pragma solidity ^0.4.19;

// 开始 Natspec 注释（三个斜杠）
// 用作文档 - 及UI元素、动作的描述性数据

/// @title SimpleBank
/// @author nemild

/* 'contract' 和其他语言的 'class' 类似 (类变量、继承等) */
contract SimpleBank { // 单词首字母大写
    // 声明函数外的状态变量，合约生命周期内可用

    // 地址映射到余额的字典，总是要小心数字的溢出攻击
    mapping (address => uint) private balances;

    // "private" 的意思是其他合约不能直接查询余额，但对于区块链上的其他方来说，数据仍然是可见的。

    address public owner;
    // 'public' 使用户或合约可以从外部读取（不可写）

    // Events（事件） - 向外部监听器发布动作
    event LogDepositMade(address accountAddress, uint amount);

    // Constructor（构造函数）（译者注：solidity 从0.4.22开始使用 constructor() 作为构造函数）
    function SimpleBank() public {
        // msg 提供了发送给合约的消息详情
        // msg.sender 是合约的调用者（这里是合约创建者的地址）
        owner = msg.sender;
    }

    /// @notice 存款 ether (以太币)
    /// @return 存款后用户的余额
    function deposit() public payable returns (uint) {
        // 使用 'require' 来检测用户的输入，'assert' 是内部常量
        // 我们要确保不会发生溢出问题（上溢）
        require((balances[msg.sender] + msg.value) >= balances[msg.sender]);

        balances[msg.sender] += msg.value;
        // 状态变量不需要 "this." 或 "self."
        // 默认情况下，所有值都设置为数据类型的初始值

        LogDepositMade(msg.sender, msg.value); // 触发事件

        return balances[msg.sender];
    }

    /// @notice 从银行取款以太币 （ether）
    /// @dev 不会返回任何多余的以太币（ether）
    /// @param withdrawAmount 取款的数量
    /// @return 用户还剩下的余额
    function withdraw(uint withdrawAmount) public returns (uint remainingBal) {
        require(withdrawAmount <= balances[msg.sender]);

        // 注意在发送任何交易，即通过 .transfer .send 调用外部函数之前，马上减掉取款数量
        // 这可以允许调用者使用递归请求大于其余额的金额。目标是在调用外部函数之前提交状态，
        // 包括.transfer / .send
        balances[msg.sender] -= withdrawAmount;

        // 这会自动引发失败，也就是说还原了更新的余额
        msg.sender.transfer(withdrawAmount);

        return balances[msg.sender];
    }

    /// @notice 获取余额
    /// @return 用户的余额
    // 'view' 防止函数编辑状态变量；允许函数本地运行或链下运行
    function balance() view public returns (uint) {
        return balances[msg.sender];
    }
}
// ** 例子结束 **


// 下面， solidity 基础

// 1. 数据类型与关联的方法
// uint 类型用作现金数量（没有双浮点型或单浮点型）及日期（用 unix 时间）
uint x;

// 256字节的 int， 实例化后不能改变
int constant a = 8;
int256 constant a = 8; // 和上一行一样，这里256字节显性化了
uint constant VERSION_ID = 0x123A1; // 16进制常量
// 'constant' 关键字, 编译器在每个出现的地方替换为实际的值

// 所有的状态变量（函数之外的那些），默认是 'internal' 的，只能在合约及所有继承的合约内
// 可以访问。需要显性的设置为 'public' 才能允许外部合约访问。
int256 public a = 8;

// 对于 int 和 uint，可以显性的设置位数（从8位到256位，8位跳跃），如int8, int16, int24
uint8 b;
int64 c;
uint248 e;

// 当心不要溢出以及免收此类攻击，例如，对于加法最好这么做：
uint256 c = a + b;
assert(c >= a); // assert 测试内部不变的值；require 用来测试用户输入
// 更多通用算法问题的例子，参考 Zeppelin's SafeMath library
// https://github.com/OpenZeppelin/zeppelin-solidity/blob/master/contracts/math/SafeMath.sol


// 没有内建的随机函数，使用其他合约获得随机数

// 类型转换
int x = int(b);

bool b = true; // 或 'var b = true;' 隐含的类型

// 地址 - 20个字节或160位以太坊地址（16进制数字），不允许进行运算
address public owner;

// 账户类型：
// 合约账户：在创建时设置地址（创建者地址函数，交易发送）
// 外部账户：（个人账户）从公钥创建的地址

// 'public' 的含义是自动创建的 getter 方法，而不是 setter 方法可以公开的、外部访问。

// 所有地址都可以进行转账
owner.transfer(SOME_BALANCE); // 失败后还原

// 还可以调用较低级别的 .send ， 转账失败会返回 false
if (owner.send) {}
// 记住：用 'if' 包着 send 函数，因为合约地址执行这些函数转账时，可能会失败
// 另外，确保转账前先减掉余额，因为存在递归调用的风险。

// 检查余额
owner.balance; // 所有者的余额（用户或合约）


// 字符类型，从1到32位可用
byte a; // byte 等同于 byte1
bytes2 b;
bytes32 c;

// 动态大小的字符
bytes m; // 特殊的数组，等同于 byte[]，比 byte1 到 byte32 更贵
// 尽可能不用 bytes

// 等同于 bytes，但不允许长度或索引的访问
string n = "hello"; // UTF8存储，注意双引号而不是单引号
// 字符功能未来会增加，推荐使用 bytes32 或 bytes

// 推断类型
// var 会根据第一次赋值决定类型，不能用来作为函数的参数
var a = true;
// 小心使用，推断可能带来错误的类型，例如，int8，而计数器需要的是 int16

// 函数可以用 var 类型赋值给变量
function a(uint x) returns (uint) {
    return x * 2;
}
var f = a;
f(22); // 调用

// 默认的，所有值实例化后都设为 0

// 大多数类型上可以调用删除（不会销毁值，而是设置为0，初始值）
uint x = 5;


// 集合
(x, y) = (2, 7); // 多值的赋值


// 2. 数据结构
// 数组
bytes32[5] nicknames; // 静态数组
bytes32[] names; // 动态数组
uint newLength = names.push("John"); // 添加返回数组的新长度
// 长度
names.length; // 获得数组长度
names.length = 1; // 可以设定长度（仅针对 storage 中的动态数组）

// 多维数组
uint x[][5]; // 5个动态数组元素的数组(和多数语言的顺序相反)

// 字典类型 (任一类型到其他类型的映射)
mapping (string => uint) public balances;
balances["charles"] = 1;
// balances["ada"]得到 0, 所有没有设定key值的，返回0
// 'public' 允许跟着（调用）另一份合约
contractName.balances("charles"); // returns 1
// 'public' 创建 getter （而不是 setter ）如下：
function balances(string _account) returns (uint balance) {
    return balances[_account];
}

// 内嵌的 mapping
mapping (address => mapping (address => uint)) public custodians;

// 删除
delete balances["John"];
delete balances; // 所有元素设为 0

// 不像其他语言，不知道 keys 的话不能列出 mapping 中的所有元素 - 可以在这之上构建数据结构

// 结构
struct Bank {
    address owner;
    uint balance;
}
Bank b = Bank({
    owner: msg.sender,
    balance: 5
});
// 或
Bank c = Bank(msg.sender, 5);

c.balance = 5; // 设为新值
delete b;
// 设为初始值，结构内所有变量设为0，除了 mapping

// 枚举
enum State { Created, Locked, Inactive }; // 常常作为状态机
State public state; // 声明枚举变量
state = State.Created;
// 枚举类型可以显性化的转换为 ints
uint createdState = uint(State.Created); //  0

// 数据位置：内存（Memory） vs. 存储（storage） vs. 调用数据（calldata）
// 所有复杂类型（数据、结构）都有一个数据位置，内存数据不持久，而存储的数据是持久的。
// 本地变量和状态变量默认是存储，函数参数默认是内存。堆栈存放较小的本地变量

// 多数类型，可以显性化的设定使用的数据位置


// 3. 简单操作符
// solidity 提供了比较、位运算及数学运算的功能
// 指数运算: **
// 异或运算: ^
// 按位取反: ~


// 4. 值得注意的全局变量
// ** this **
this; // 合约的地址
// 常常用在合约生命周期结束前，转走剩下的余额
this.balance;
this.someFunction(); // 通过 call 的方式而不是内部跳转的方式，从外部调用函数

// ** msg - 合约收到的当前消息 ** **
msg.sender; // 发送者的地址
msg.value; // 该合约内的以太币数量（单位 wei），该函数应该标记为 "payable"
msg.data; // 字符，完整的调用数据
msg.gas; // 剩余 gas

// ** tx - 交易信息 **
tx.origin; // 本次交易的发送者地址
tx.gasprice; // 本次交易的 gas price

// ** block - 当前区块信息 **
now; // 当前时间（大概）block.timestamp的别名 (采用的 Unix 时间)
// 注意这个可能被矿工操纵，因此请小心使用

block.number; // 当前区块号
block.difficulty; // 当前区块难度
block.blockhash(1); // 返回 bytes32，只对最近 256 个区块有效
block.gasLimit();

// ** 存储 - 持久化存储哈希 **
storage['abc'] = 'def'; // 256 位单词 到 256 位单词的映射


// 4. 函数及更多
// A. 函数
// 简单函数
function increment(uint x) returns (uint) {
    x += 1;
    return x;
}

// 函数可以通过指定返回的参数名，来返回多个参数
function increment(uint x, uint y) returns (uint x, uint y) {
    x += 1;
    y += 1;
}
// 调用前一个函数
uint (a,b) = increment(1,1);

// 'view' ('constant'的别名)
// 表明函数不会改变持久化的变量，View函数会本地执行，而不是链上运行。
// 注意：constant 关键字很快会废弃。
uint y = 1;

function increment(uint x) view returns (uint x) {
    x += 1;
    y += 1; // 这一行会失败
    // y 是一个状态变量，不能在 view 的函数里改变 y
}

// 'pure' 比 'view' 或 'constant' 更加严格，甚至不允许读取状态变量
// 具体的规则很复杂，请参考
// view/pure:
// http://solidity.readthedocs.io/en/develop/contracts.html#view-functions

// '函数可见性指示器'
// 'view'可以有以下修饰符，包括：
// public - 内部及外部可见（函数的默认值）
// external - 仅外部可见(包括 this 发起的调用)
// private - 仅当前合约可见
// internal - 仅当前合约及继承的合约可见

// 通常，显性的标记每个函数是个好主意

// 函数的挂起 - 可以将函数赋值给变量
function a() {
    var z = b;
    b();
}

function b() {

}

// 所有接收 ether 的函数必须标记为 'payable'
function depositEther() public payable {
    balances[msg.sender] += msg.value;
}


// 首选循环来递归（最大的调用堆栈深度是 1024），另外不要设置没有限制的循环，
// 因为这可能会达到 gas limit

// B. 事件
// 事件通知外部各方; 易于搜索和访问来自外部区块链（使用轻客户端）的事件
// 通常在合约参数之后声明

// 通常，首字母大写并在前面加上 Log ，防止与函数混淆

// 声明
event LogSent(address indexed from, address indexed to, uint amount); // 注意 capital first letter

// 调用
LogSent(from, to, amount);

/*
    // 对于外部方（合约或外部实体），使用 Web3 Javascript 库来监听
    // 以下是javascript代码,不是solidity代码
    Coin.LogSent().watch({}, '', function(error, result) {
        if (!error) {
            console.log("Coin transfer: " + result.args.amount +
                " coins were sent from " + result.args.from +
                " to " + result.args.to + ".");
            console.log("Balances now:\n" +
                "Sender: " + Coin.balances.call(result.args.from) +
                "Receiver: " + Coin.balances.call(result.args.to));
        }
    }

*/

// 一个合约依赖另一个合约的共同范例（例如，合约取决于另一个合约提供的当前汇率）

// C. 修饰器
// 修饰器验证函数的输入，例如最小余额或用户身份验证; 类似于其他语言的保护子句

// '_' （下划线）经常用在代码的最后一行，表明被调用的函数放在那里
modifier onlyAfter(uint _time) { require (now >= _time); _; }
modifier onlyOwner { require(msg.sender == owner) _; }
// 常用于状态机
modifier onlyIfStateA (State currState) { require(currState == State.A) _; }

// 修饰器紧跟在函数声明之后
function changeOwner(newOwner)
onlyAfter(someTime)
onlyOwner()
onlyIfState(State.A)
{
    owner = newOwner;
}

// 下划线可以包含在代码结束之前，但明显地返回将跳过后面的代码，因此谨慎使用
modifier checkValue(uint amount) {
    _;
    if (msg.value > amount) {
        uint amountToRefund = amount - msg.value;
        msg.sender.transfer(amountToRefund);
    }
}


// 6. 判断和循环

// 所有基本的逻辑判断都有效 - 包括 if else, for, while, break, continue
// return - 但不跳转

// 语法同 javascript, 但没有从非布尔值到布尔值的类型转换
// (必须使用比较操作符获得布尔变量值)

// 请注意由用户行为决定的循环 - 因为合约对于代码块具有最大量的 gas 限制 -
// 如果超过限制该代码则将失败
// 例如：
for(uint x = 0; x < refundAddressList.length; x++) {
    refundAddressList[x].transfer(SOME_AMOUNT);
}

// 上述两个错误:
// 1. 转账失败会阻塞循环完成，钱被占用
// 2. 该循环可能会很长（根据需要赔偿的用户数量而定），并且也可能由于超过一个区块最大 gas 限制
// 而总是失败。你应该让人们自己从他们的子账户取款并标记取款完成
// 例如，首选拉动式的付款，而不是推动式的付款


// 7. 对象与合约

// A. 调用外部合约
contract InfoFeed {
    function info() returns (uint ret) { return 42; }
}

contract Consumer {
    InfoFeed feed; // 指向区块链上的一个合约

    // 设置 feed 为已存在的合约实例
    function setFeed(address addr) {
        // 当心类型自动转换；不会调用构造函数
        feed = InfoFeed(addr);
    }

    // 设置 feed 为一个合约的新实例
    function createNewFeed() {
        feed = new InfoFeed(); // 创建新实例，调用构造函数
    }

    function callFeed() {
        // 最后的括号调用合约，可选择的增加自定义的 ether 或 gas 价格
        feed.info.value(10).gas(800)();
    }
}

// B. 继承

// 和顺序有关，最后继承的合约（如 'def'）可以覆盖之前已继承合约的部分
contract MyContract is abc, def("a custom argument to def") {

// 覆盖函数
    function z() {
        if (msg.sender == owner) {
            def.z(); // 调用覆盖的函数
            super.z(); // 调用继承的上层合约的函数
        }
    }
}

// 抽象函数
function someAbstractFunction(uint x);
// 不可以编译，因此用在基础或抽象合约中，等待实现

// C. 导入

import "filename";
import "github.com/ethereum/dapp-bin/library/iterable_mapping.sol";


// 8. 其他关键字

// A. 自毁
// 自毁当前的合约，转账资金到一个地址（常常是创建者的地址）
selfdestruct(SOME_ADDRESS);

// 从当前或以后的区块中移除存储或代码，会帮助客户端瘦身，但之前的数据会永久在区块链中

// 常见模式，让所有者结束合约并收回剩余的资金
function remove() {
    if(msg.sender == creator) { // 只有合约的创建者可以这么做
        selfdestruct(creator); // 自毁合约，返还资金
    }
}

// 可能希望手动停用合约，而不是自毁
// (发送到自毁合约的 ether 会丢失掉)


// 9. 注意合约的设计

// A. 困惑
// 区块链上所有变量都是公开可见的，因此任何私有的需求变得很困惑。(好比哈希的秘密)

// 步骤: 1. 承诺某事, 2. 揭示承诺
keccak256("some_bid_amount", "some secret"); // commit

// 以后调用合约的 reveal 函数，展示出用 SHA3 哈希的 bid 加 secret
reveal(100, "mySecret");

// B. 存储优化
// 写入区块链可能很昂贵，因为数据是永久存储的；鼓励用巧妙的方法使用内存
//（最终，编译会更好，但现在有利于规划数据结构 - 并将最小数量存储在区块链中）

// 多维数组这样的变量可能会成本很高
// (成本用于存储数据 - 而不是声明未填充的变量)

// C. 区块链中的数据访问
// 不能限制人或计算机读取交易或交易状态的内容

// 然而 'private' 可以防止其他*合约*直接读取数据 - 任意其他方仍然可以从区块链读取数据

// 从开始的所有数据都存在区块链上，因此任何人都可以查看之前的所有数据和变化

// D. 定时任务
// 必须手动调用合约来处理时间相关的调度；也可以创建外部代码来定期的ping，
// 或为其他人提供激励（以太）

// E. 观察者模式
//观察者模式允许您注册为订阅者，然后注册一个由oracle调用的函数
//（注意，oracle 需要付费来运行此操作）。与 Pub / sub 中的订阅有些相似之处

// 这是一个抽象合约，包括客户端和服务器端的类的导入，客户端应该要实现
contract SomeOracleCallback {
    function oracleCallback(int _value, uint _time, bytes32 info) external;
}

contract SomeOracle {
    SomeOracleCallback[] callbacks; // 所有订阅者的数组

    // 注册订阅者
    function addSubscriber(SomeOracleCallback a) {
        callbacks.push(a);
    }

    function notify(value, time, info) private {
        for(uint i = 0;i < callbacks.length; i++) {
            // 所有调用的订阅者必须实现 oracleCallback
            callbacks[i].oracleCallback(value, time, info);
        }
    }

    function doSomething() public {
        // 实现的代码

        // 通知所有的订阅者
        notify(_value, _time, _info);
    }
}

// 现在你的客户端合约可以通过 importing SomeOracleCallback 和注册某些 Oracle 来
// addSubscriber 添加订阅者

// F. 状态机
// 参见如下的例子，枚举类型的 State 和 修饰器 inState


// *** 例子: 众筹的例子（与 Kickstarter 大致相似）***
// ** 开始例子 **

// CrowdFunder.sol
pragma solidity ^0.4.19;

/// @title CrowdFunder
/// @author nemild
/// @translator bobjiang
contract CrowdFunder {
    // 由创建者创建的变量
    address public creator;
    address public fundRecipient; // 创建者可能和收件人不同
    uint public minimumToRaise; // 需要提示，否则每个人都会得到退款
    string campaignUrl;
    byte constant version = 1;

    // 数据结构
    enum State {
        Fundraising,
        ExpiredRefund,
        Successful
    }
    struct Contribution {
        uint amount;
        address contributor;
    }

    // 状态变量State variables
    State public state = State.Fundraising; // 创建时实例化
    uint public totalRaised;
    uint public raiseBy;
    uint public completeAt;
    Contribution[] contributions;

    event LogFundingReceived(address addr, uint amount, uint currentTotal);
    event LogWinnerPaid(address winnerAddress);

    modifier inState(State _state) {
        require(state == _state);
        _;
    }

    modifier isCreator() {
        require(msg.sender == creator);
        _;
    }

    // 允许合约销毁之前，最终合约状态后要等待24周
    modifier atEndOfLifecycle() {
    require(((state == State.ExpiredRefund || state == State.Successful) &&
        completeAt + 24 weeks < now));
        _;
    }

    function CrowdFunder(
        uint timeInHoursForFundraising,
        string _campaignUrl,
        address _fundRecipient,
        uint _minimumToRaise)
        public
    {
        creator = msg.sender;
        fundRecipient = _fundRecipient;
        campaignUrl = _campaignUrl;
        minimumToRaise = _minimumToRaise;
        raiseBy = now + (timeInHoursForFundraising * 1 hours);
    }

    function contribute()
    public
    payable
    inState(State.Fundraising)
    returns(uint256 id)
    {
        contributions.push(
            Contribution({
                amount: msg.value,
                contributor: msg.sender
            }) // 采用数组，因此可以遍历
        );
        totalRaised += msg.value;

        LogFundingReceived(msg.sender, msg.value, totalRaised);

        checkIfFundingCompleteOrExpired();
        return contributions.length - 1; // 返回 id
    }

    function checkIfFundingCompleteOrExpired()
    public
    {
        if (totalRaised > minimumToRaise) {
            state = State.Successful;
            payOut();

            // 可以激励在这里发起状态改变的人
        } else if ( now > raiseBy )  {
            state = State.ExpiredRefund; // 支持者可以通过调用 getRefund(id) 收取退款
        }
        completeAt = now;
    }

    function payOut()
    public
    inState(State.Successful)
    {
        fundRecipient.transfer(this.balance);
        LogWinnerPaid(fundRecipient);
    }

    function getRefund(uint256 id)
    inState(State.ExpiredRefund)
    public
    returns(bool)
    {
        require(contributions.length > id && id >= 0 && contributions[id].amount != 0 );

        uint256 amountToRefund = contributions[id].amount;
        contributions[id].amount = 0;

        contributions[id].contributor.transfer(amountToRefund);

        return true;
    }

    function removeContract()
    public
    isCreator()
    atEndOfLifecycle()
    {
        selfdestruct(msg.sender);
        // 创建者获得所有未被声明的钱
    }
}
// ** 结束例子 **

// 10. 其他原生的函数

// 货币单位
// 货币使用 wei 来定义，以太币的最小单位 = 1 wei;
uint minAmount = 1 wei;
uint a = 1 finney; // 1 ether == 1000 finney
// 其他单位，请参阅: http://ether.fund/tool/converter

// 时间单位
1 == 1 second
1 minutes == 60 seconds

// 可以乘以带时间单位的变量，因为单位不会存储在变量中
uint x = 5;
(x * 1 days); // 5 天

// 小心闰秒闰年与平等声明的时间
// (相反，首选大于或小于)

// 加密算法
// 传递的所有字符串在哈希操作之前需要连接在一起
sha3("ab", "cd");
ripemd160("abc");
sha256("def");

// 11. 安全

// 以太坊的合约中，错误可能是灾难性的 - 即使在 solidity 中是流行的模式，也可能发现是反模式的

// 参见文档底部的安全链接

// 12. 较低层次的函数
// call - 较低层次，不会经常使用，不提供类型安全性
successBoolean = someContractAddress.call('function_name', 'arg1', 'arg2');

// callcode - 在调用合约的*上下文*中执行的目标地址上的代码
// 提供库功能
someContractAddress.callcode('function_name');


// 13. 注意风格
// 基于 Python 的 PEP8 风格指南
// 全部风格指南: http://solidity.readthedocs.io/en/develop/style-guide.html

// 快速总结:
// 4个空格缩进
// 两行隔开合约声明（和其他高级别的声明）
// 避免括号内留出多余的空格
// 可以省略一行语句的花括号 (if, for, 等)
// else 应该单独一行


// 14. NATSPEC 注释
// 用于文档、注释和外部UI

// 合约的 natspec - 总是在合约定义的上面
/// @title 合约标题
/// @author 作者名字

// 函数的 natspec
/// @notice 函数做什么的相关信息；展示什么时候执行该函数、
/// @dev 开发者使用的函数文档

// 函数参数、返回值的 natspec
/// @param 有关参数用途的描述
/// @return 返回值的描述
```

## 更多资源
- [Solidity Docs](https://solidity.readthedocs.org/en/latest/)
- [Smart Contract Best Practices](https://github.com/ConsenSys/smart-contract-best-practices)
- [EthFiddle - The JsFiddle for Solidity](https://ethfiddle.com/)
- [Browser-based Solidity Editor](https://remix.ethereum.org/)
- [Gitter Solidity Chat room](https://gitter.im/ethereum/solidity)
- [Modular design strategies for Ethereum Contracts](https://docs.erisindustries.com/tutorials/solidity/)

## 重要的库文件
- [Zeppelin](https://github.com/OpenZeppelin/zeppelin-solidity/): Libraries that provide common contract patterns (crowdfuding, safemath, etc)

## 示例合约
- [Dapp Bin](https://github.com/ethereum/dapp-bin)
- [Solidity Baby Step Contracts](https://github.com/fivedogit/solidity-baby-steps/tree/master/contracts)
- [ConsenSys Contracts](https://github.com/ConsenSys/dapp-store-contracts)
- [State of Dapps](http://dapps.ethercasts.com/)

## 安全
- [Thinking About Smart Contract Security](https://blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [Smart Contract Security](https://blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Hacking Distributed Blog](http://hackingdistributed.com/)

## 风格
- [Solidity Style Guide](http://solidity.readthedocs.io/en/latest/style-guide.html): Ethereum's style guide is heavily derived from Python's [PEP 8](https://www.python.org/dev/peps/pep-0008/) style guide.

## 编辑器
- [Emacs Solidity Mode](https://github.com/ethereum/emacs-solidity)
- [Vim Solidity](https://github.com/tomlion/vim-solidity)
- Editor Snippets ([Ultisnips format](https://gist.github.com/nemild/98343ce6b16b747788bc))

## Future to Dos
- 新关键字: protected, inheritable
- 常见设计模式列表 (throttling, RNG, version upgrade)
- 常见的安全反模式

请随意发送 pull request 或者发邮件给作者  nemild -/at-/ gmail

或者发邮件给译者 jiangxb -/at-/ gmail.com
