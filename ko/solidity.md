---
name: Solidity
filename: learnSolidity.sol
contributors:
  - ["Nemil Dalal", "https://www.nemil.com"]
  - ["Joseph Chow", ""]
  - ["Bhoomtawath Plinsut", "https://github.com/varshard"]
  - ["Shooter", "https://github.com/liushooter"]
  - ["Patrick Collins", "https://gist.github.com/PatrickAlphaC"]
---

Solidity를 사용하면 중앙 집중식 또는 신뢰할 수 있는 당사자 없이 스마트 계약을 생성하고 실행할 수 있는 블록체인 기반 가상 머신인 [Ethereum](https://www.ethereum.org/)에서 프로그래밍할 수 있습니다.

Solidity는 JavaScript 및 C와 유사한 정적으로 유형이 지정된 계약 프로그래밍 언어입니다. OOP의 객체와 마찬가지로 각 계약에는 상태 변수, 함수 및 공통 데이터 유형이 포함됩니다. 계약별 기능에는 수정자(가드) 절, 리스너용 이벤트 알리미 및 사용자 지정 전역 변수가 포함됩니다.

일부 이더리움 계약 예로는 크라우드펀딩, 투표, [탈중앙화 금융](https://defipulse.com/) 및 블라인드 경매가 있습니다.

Solidity 코드에는 오류에 대한 높은 위험과 높은 비용이 있으므로 테스트하고 천천히 출시하는 데 매우 신중해야 합니다. 이더리움의 급격한 변화로 인해 이 문서는 최신 상태를 유지하기 어려울 수 있으므로 최신 정보는 SOLIDITY 채팅방과 이더리움 블로그를 따르는 것이 좋습니다. 여기의 모든 코드는 오류나 오래된 코드 패턴의 상당한 위험과 함께 있는 그대로 제공됩니다.

다른 코드와 달리 위험을 줄이기 위해 일시 중지, 사용 중단 및 사용량 조절과 같은 디자인 패턴을 추가해야 할 수도 있습니다. 이 문서는 주로 구문을 다루므로 많은 인기 있는 디자인 패턴은 제외합니다.

Solidity와 이더리움은 활발하게 개발 중이므로 실험적이거나 베타 기능은 일반적으로 표시되며 변경될 수 있습니다. 풀 리퀘스트를 환영합니다.

# Remix 및 Metamask 작업

솔리디티 코드를 빌드, 배포 및 테스트하는 가장 쉬운 방법 중 하나는 다음을 사용하는 것입니다.

1. [Remix 웹 IDE](https://remix.ethereum.org/)
2. [메타마스크 지갑](https://metamask.io/).

시작하려면 [메타마스크 브라우저 확장 프로그램 다운로드](https://metamask.io/)를 받으십시오.

설치가 완료되면 Remix로 작업합니다. 아래 코드가 미리 로드되지만, 그곳으로 가기 전에 Remix를 시작하기 위한 몇 가지 팁을 살펴보겠습니다. [이 링크를 눌러](https://remix.ethereum.org/#version=soljson-v0.8.19+commit.7dd6d404.js&optimize=false&evmVersion=null&gist=2acb47a0286fe45d2464fa937f00fef3&runs=200) 모두 로드하십시오.

1. 솔리디티 컴파일러 선택

![Solidity-in-remix](/images/solidity/remix-solidity.png)

2. 해당 링크로 로드된 파일 열기

![Solidity-choose-file](/images/solidity/remix-choose-file.png)

3. 파일 컴파일

![Solidity-compile](/images/solidity/remix-compile.png)

4. 배포

![Solidity-deploy](/images/solidity/remix-deploy.png)

5. 계약과 놀기

![Solidity-deploy](/images/solidity/remix-interact.png)

첫 번째 계약을 배포했습니다! 축하합니다!

정의된 함수를 테스트하고 놀 수 있습니다. 각 함수가 무엇을 하는지 알아보려면 주석을 확인하십시오.

지금은 달리 지시하지 않는 한 `Remix VM`을 계속 사용하십시오.


```solidity
// 먼저, 간단한 은행 계약
// 입금, 출금 및 잔액 조회를 허용합니다.

// simple_bank.sol (.sol 확장자 참고)
/* **** 예제 시작 **** */
// 파일 상단에 특수 주석을 사용하여
// 코드 라이선스와 사용된 솔리디티 버전을 나타냅니다.
// SPDX-License-Identifier: MIT

// 소스 파일 컴파일러 버전 선언
pragma solidity ^0.8.19;

// Natspec 주석(세 개의 슬래시)으로 시작
// 문서화 및 UI 요소/작업에 대한 설명 데이터로 사용됩니다.

/// @title SimpleBank
/// @author nemild

/* 'contract'는 다른 언어의 'class'와 유사합니다(클래스 변수,
상속 등). */
contract SimpleBank { // 대문자 단어
    // 함수 외부에서 상태 변수를 선언하면 계약 수명 동안 지속됩니다.

    // 주소를 잔액에 매핑하는 사전
    mapping (address => uint) private balances;

    // "private"은 다른 계약이 잔액을 직접 쿼리할 수 없음을 의미합니다.
    // 하지만 데이터는 블록체인의 다른 당사자에게 여전히 볼 수 있습니다.

    address public owner;
    // 'public'은 사용자나 계약이 외부에서 읽을 수 있도록 합니다(쓰기 불가).

    // 이벤트 - 외부 리스너에게 작업을 알립니다.
    event LogDepositMade(address accountAddress, uint amount);

    // 생성자, 여기에서 하나 또는 여러 변수를 받을 수 있습니다. 하나만 허용됩니다.
    constructor() {
        // msg는 계약으로 전송된 메시지에 대한 세부 정보를 제공합니다.
        // msg.sender는 계약 호출자입니다(계약 생성자의 주소).
        owner = msg.sender;
    }

    /// @notice 은행에 이더 입금
    /// @return 입금 후 사용자 잔액
    function deposit() public payable returns (uint) {
        // 'require'를 사용하여 사용자 입력을 테스트하고, 'assert'를 사용하여 내부 불변량을 테스트합니다.
        // 여기서는 오버플로 문제가 없는지 확인합니다.
        // 최신 버전의 솔리디티에서는 자동으로 확인됩니다.
        require((balances[msg.sender] + msg.value) >= balances[msg.sender]);

        balances[msg.sender] += msg.value;
        // 상태 변수에는 "this." 또는 "self."가 필요하지 않습니다.
        // 모든 값은 기본적으로 데이터 유형의 초기 값으로 설정됩니다.

        emit LogDepositMade(msg.sender, msg.value); // 이벤트 발생

        return balances[msg.sender];
    }

    /// @notice 은행에서 이더 인출
    /// @dev 전송된 초과 이더는 반환하지 않습니다.
    /// @param withdrawAmount 인출하려는 금액
    /// @return remainingBal
    function withdraw(uint withdrawAmount) public returns (uint remainingBal) {
        require(withdrawAmount <= balances[msg.sender]);

        // 보내기 전에 즉시 잔액을 공제하는 방식에 유의하십시오.
        // 이 계약에서 모든 .transfer/.send는 외부 함수를 호출할 수 있습니다.
        // 이로 인해 호출자가 재귀 호출을 사용하여 잔액보다
        // 많은 금액을 요청할 수 있습니다.
        // .transfer/.send를 포함한 외부 함수를 호출하기 전에 상태를 커밋하는 것을 목표로 하십시오.
        balances[msg.sender] -= withdrawAmount;

        // 이것은 실패 시 자동으로 예외를 발생시키므로 업데이트된 잔액이 되돌려집니다.
        payable(msg.sender).transfer(withdrawAmount);

        return balances[msg.sender];
    }

    /// @notice 잔액 조회
    /// @return 사용자 잔액
    // 'view'(예: 상수)는 함수가 상태 변수를 편집하는 것을 방지합니다.
    // 함수가 로컬/블록체인 외부에서 실행되도록 허용합니다.
    function balance() view public returns (uint) {
        return balances[msg.sender];
    }
}
// ** 예제 끝 **


// 이제 솔리디티의 기본 사항

// 1. 데이터 유형 및 관련 메서드
// uint는 통화 금액(더블 또는 플로트 없음)과
// 날짜(유닉스 시간)에 사용됩니다.
uint x;

// 256비트 int, 인스턴스화 후 변경할 수 없음
int constant a = 8;
int256 constant a = 8; // 위 줄과 동일한 효과, 여기서는 256이 명시적임
uint constant VERSION_ID = 0x123A1; // 16진수 상수
// 'constant'를 사용하면 컴파일러가 각 발생을 실제 값으로 바꿉니다.

// 모든 상태 변수(함수 외부의 변수)는
// 기본적으로 'internal'이며 계약 내부 및
// 상속하는 모든 계약에서만 액세스할 수 있습니다.
// 외부 계약이 액세스할 수 있도록 하려면 명시적으로 'public'으로 설정해야 합니다.
int256 public a = 8;

// int 및 uint의 경우 256까지 8단계로 공간을 명시적으로 설정할 수 있습니다.
// 예: int8, int16, int24
uint8 b;
int64 c;
uint248 e;

// 이전 버전의 솔리디티에서는 덧셈으로 인해 "오버플로"가 발생할 수 있었습니다.
// 예를 들어, 덧셈의 경우 다음과 같이 수행합니다.
uint256 c = a + b;
assert(c >= a);
// 하지만 최신 버전의 솔리디티는 정수 수학의 오버플로/언더플로를 자동으로 확인합니다.


// 내장된 임의 함수 없음, 현재 블록 해시를 해시하여 의사 난수를 얻거나 체인링크 VRF와 같은 것을 사용하여 진정한 난수를 얻을 수 있습니다.
// https://docs.chain.link/docs/get-a-random-number

// 유형 캐스팅
int x = int(b);

bool b = true;

// 주소 - 20바이트/160비트 이더리움 주소 보유
// 산술 허용 안 됨
address public owner;

// 계정 유형:
// 계약 계정: 생성 시 설정된 주소(생성자 주소의 함수, 전송된 트랜잭션 수)
// 외부 계정: (개인/외부 엔티티): 공개 키에서 생성된 주소

// 공개/외부에서 액세스할 수 있음을 나타내기 위해 'public' 필드 추가
// getter는 자동으로 생성되지만 setter는 생성되지 않음

// 모든 주소로 이더를 보낼 수 있습니다.
owner.transfer(SOME_BALANCE); // 실패 시 실패하고 되돌립니다.

// 더 낮은 수준의 .send 호출을 수행할 수도 있으며, 실패하면 false를 반환합니다.
if (owner.send(amount)) {} // 'if'로 send를 래핑하는 것을 기억하십시오. 계약 주소에는
// send 시 실행되는 함수가 있으며 실패할 수 있습니다.
// 또한 재귀 호출 위험이 있으므로 send를 시도하기 전에
// 잔액을 공제해야 합니다. 계약을 고갈시킬 수 있습니다.

// 잔액 확인 가능
owner.balance; // 소유자(사용자 또는 계약)의 잔액


// 1에서 32까지 사용 가능한 바이트
bytes1 a; // bytes1은 명시적 형식입니다.
bytes2 b;
bytes32 c;

// 동적 크기 바이트
bytes m; // 특수 배열, byte[] 배열과 동일(단, 꽉 채워짐)
// byte1-byte32보다 비싸므로 가능하면 그것들을 사용하십시오.

// bytes와 동일하지만 길이 또는 인덱스 액세스를 허용하지 않음(현재로서는)
string n = "hello"; // UTF8로 저장됨, 작은따옴표가 아닌 큰따옴표 참고
// 향후 추가될 문자열 유틸리티 함수
// UTF8은 더 많은 저장 공간을 사용하므로 bytes32/bytes를 선호하십시오.

// 기본적으로 모든 값은 인스턴스화 시 0으로 설정됩니다.

// 대부분의 유형에서 Delete를 호출할 수 있습니다.
// (값을 파괴하지 않고 값을 0, 즉 초기 값으로 설정합니다)
delete x;


// 구조 분해/튜플
(x, y) = (2, 7); // 여러 값 할당/교환


// 2. 자료 구조
// 배열
bytes32[5] nicknames; // 정적 배열
bytes32[] names; // 동적 배열
names.push("John"); // 요소 추가(더 이상 길이를 반환하지 않음)
// 길이
names.length; // 길이 가져오기
// 참고: 직접 길이 할당은 최신 솔리디티 버전에서 제거되었습니다.

// 다차원 배열
uint[][5] x; // 동적 배열 요소 5개를 가진 배열(대부분의 언어와 반대 순서)

// 사전(모든 유형에서 다른 유형으로)
mapping (string => uint) public balances;
balances["charles"] = 1;
// balances["ada"] 결과는 0, 설정되지 않은 모든 키 값은 0을 반환합니다.
// 'public'은 다른 계약에서 다음을 허용합니다.
contractName.balances("charles"); // 1 반환
// 'public'은 다음과 같은 getter를 생성했습니다(setter는 아님).
function balances(string memory _account) public view returns (uint balance) {
    return balances[_account];
}

// 중첩 매핑
mapping (address => mapping (address => uint)) public custodians;

// 삭제하려면
delete balances["John"];
delete balances; // 모든 요소를 0으로 설정

// 다른 언어와 달리 소스 키를 알지 못하면
// 매핑의 모든 요소를 반복할 수 없습니다.
// 이를 위해 상위 데이터 구조를 빌드할 수 있습니다.

// 구조체
struct Bank {
    address owner;
    uint balance;
}
Bank b = Bank({
    owner: msg.sender,
    balance: 5
});
// 또는
Bank c = Bank(msg.sender, 5);

c.balance = 5; // 새 값으로 설정
delete b;
// 초기 값으로 설정, 매핑을 제외한 구조체의 모든 변수를 0으로 설정

// 열거형
enum State { Created, Locked, Inactive }; // 상태 머신에 자주 사용됨
State public state; // 열거형에서 변수 선언
state = State.Created;
// 열거형은 명시적으로 int로 변환할 수 있습니다.
uint createdState = uint(State.Created); //  0

// 데이터 위치: 메모리 대 스토리지 대 콜데이터 - 모든 복합 유형(배열,
// 구조체)에는 데이터 위치가 있습니다.
// '메모리'는 지속되지 않고 '스토리지'는 지속됩니다.
// '콜데이터'도 지속되지 않으며 "읽기 전용"이므로 수정할 수 없습니다.
// 기본값은 로컬 및 상태 변수의 경우 '스토리지'이고 함수 매개변수의 경우 '메모리'입니다.
// 스택은 작은 로컬 변수를 보유합니다.

// 대부분의 유형에 대해 사용할 데이터 위치를 명시적으로 설정할 수 있습니다.


// 3. 간단한 연산자
// 비교, 비트 연산자 및 산술 연산자가 제공됩니다.
// 거듭제곱: **
// 배타적 논리합: ^
// 비트 부정: ~


// 4. 주목할 만한 전역 변수
// ** this **
this; // 계약 주소
// 종종 계약 수명 마지막에 남은 잔액을 당사자에게 이체하는 데 사용됩니다.
this.balance;
this.someFunction(); // 내부 점프가 아닌 호출을 통해 외부에서 함수 호출

// ** msg - 계약이 받은 현재 메시지 ** **
msg.sender; // 보낸 사람 주소
msg.value; // wei 단위로 이 계약에 제공된 이더 금액, 함수는 "payable"로 표시해야 합니다.
msg.data; // 바이트, 완전한 호출 데이터

// ** tx - 이 트랜잭션 **
tx.origin; // 트랜잭션 보낸 사람 주소
tx.gasprice; // 트랜잭션의 가스 가격

// ** block - 현재 블록에 대한 정보 **
block.timestamp; // 현재 시간(대략)(유닉스 시간 사용)
// 이것은 채굴자에 의해 조작될 수 있으므로 주의해서 사용하십시오.

block.number; // 현재 블록 번호
block.difficulty; // 현재 블록 난이도
block.blockhash(1); // bytes32 반환, 가장 최근 256개 블록에 대해서만 작동
block.gasLimit();

// ** storage - 영구 저장소 해시 **
storage['abc'] = 'def'; // 256비트 단어를 256비트 단어에 매핑


// 5. 함수 등
// A. 함수
// 간단한 함수
function increment(uint x) returns (uint) {
    x += 1;
    return x;
}

// 함수는 많은 인수를 반환할 수 있으며,
// 반환된 인수 이름을 지정하면 명시적 반환이 필요하지 않습니다.
function increment(uint x, uint y) returns (uint x, uint y) {
    x += 1;
    y += 1;
}
// 이전 함수 호출
(uint a, uint b) = increment(1,1);

// 'view'(상수의 별칭)
// 함수가 영구 변수를 변경하지 않거나 변경할 수 없음을 나타냅니다.
// View 함수는 블록체인이 아닌 로컬에서 실행됩니다.
// 참고: 상수 키워드는 곧 사용되지 않을 예정입니다.
uint y = 1;

function increment(uint x) view returns (uint x) {
    x += 1;
    y += 1; // 이 줄은 실패합니다.
    // y는 상태 변수이며 view 함수에서 변경할 수 없습니다.
}

// 'pure'는 'view' 또는 'constant'보다 더 엄격하며,
// 상태 변수 읽기도 허용하지 않습니다.
// 정확한 규칙은 더 복잡하므로
// view/pure에 대해 자세히 알아보십시오:
// http://solidity.readthedocs.io/en/develop/contracts.html#view-functions

// '함수 가시성 지정자'
// 이것들은 'view'가 있는 곳에 배치할 수 있으며 다음을 포함합니다.
// public - 외부 및 내부에서 볼 수 있음(함수의 기본값)
// external - 외부에서만 볼 수 있음(this.로 호출한 경우 포함)
// private - 현재 계약에서만 볼 수 있음
// internal - 현재 계약 및 상속받는 계약에서만 볼 수 있음

// 일반적으로 각 함수를 명시적으로 표시하는 것이 좋습니다.

// 함수 호이스팅 - 변수에 함수를 할당할 수 있습니다.
function a() {
    function() internal z = b;
    z();
}

function b() {

}

// 이더를 받는 모든 함수는 'payable'로 표시해야 합니다.
function depositEther() public payable {
    balances[msg.sender] += msg.value;
}


// 재귀보다 루프를 선호하십시오(최대 호출 스택 깊이는 1024).
// 또한 경계가 없는 루프를 설정하지 마십시오.
// 가스 한도를 초과할 수 있습니다.

// B. 이벤트
// 이벤트는 외부 당사자에게 알립니다. 블록체인 외부에서 이벤트를
// 검색하고 액세스하기 쉽습니다(경량 클라이언트 사용).
// 일반적으로 계약 매개변수 뒤에 선언합니다.

// 일반적으로 대문자로 표시하고 명시성을 위해 앞에 Log를 추가하여
// 함수 호출과 혼동을 방지합니다.

// 선언
event LogSent(address indexed from, address indexed to, uint amount); // 대문자 첫 글자 참고

// 호출
emit LogSent(from, to, amount);

/**

외부 당사자(계약 또는 외부 엔티티)가 Web3 JavaScript 라이브러리를 사용하여
감시하려면:

// 다음은 솔리디티 코드가 아닌 JavaScript 코드입니다.
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
**/

// 한 계약이 다른 계약에 의존하는 일반적인 패러다임(예:
// 다른 계약에서 제공하는 현재 환율에 의존하는 계약)

// C. 수정자
// 수정자는 최소 잔액 또는 사용자 인증과 같은 함수에 대한
// 입력을 확인합니다. 다른 언어의 가드 절과 유사합니다.

// '_' (밑줄)은 종종 본문 마지막 줄에 포함되며,
// 호출되는 함수가 거기에 배치되어야 함을 나타냅니다.
modifier onlyAfter(uint _time) { require (block.timestamp >= _time); _; }
modifier onlyOwner { require(msg.sender == owner); _; }
// 상태 머신과 함께 자주 사용됨
modifier onlyIfStateA (State currState) { require(currState == State.A); _; }

// 함수 선언 바로 뒤에 추가
function changeOwner(newOwner)
onlyAfter(someTime)
onlyOwner()
onlyIfState(State.A)
{
    owner = newOwner;
}

// 밑줄은 본문 끝 전에 포함될 수 있지만,
// 명시적으로 반환하면 건너뛰므로 주의해서 사용하십시오.
modifier checkValue(uint amount) {
    _;
    if (msg.value > amount) {
        uint amountToRefund = amount - msg.value;
        msg.sender.transfer(amountToRefund);
    }
}


// 6. 분기 및 루프

// if/else, for, while, break, continue를 포함한
// 모든 기본 논리 블록이 작동합니다.
// return - 하지만 switch는 없음

// 구문은 JavaScript와 동일하지만 부울이 아닌
// 값에서 부울로의 유형 변환은 없습니다(부울 값을 얻으려면
// 비교 연산자를 사용해야 함).

// 사용자 행동에 의해 결정되는 For 루프의 경우, 계약에는
// 코드 블록에 대한 최대 가스 양이 있으므로 주의하십시오.
// 초과하면 실패합니다.
// 예:
for(uint x = 0; x < refundAddressList.length; x++) {
    refundAddressList[x].transfer(SOME_AMOUNT);
}

// 위 두 가지 오류:
// 1. 전송 실패는 루프가 완료되는 것을 막고 돈을 묶습니다.
// 2. 이 루프는 임의로 길 수 있으므로(환불이 필요한 사용자 수에 따라),
// 블록의 최대 가스를 초과하므로 항상 실패할 수 있습니다.
// 대신, 사람들이 하위 계정에서 개별적으로 인출하고 인출된 것으로 표시하도록 해야 합니다.
// 예: 푸시 결제보다 풀 결제를 선호


// 7. 객체/계약

// A. 외부 계약 호출
contract InfoFeed {
    function info() payable returns (uint ret)  { return 42; }
}

contract Consumer {
    InfoFeed feed; // 블록체인의 계약을 가리킴

    // 기존 계약 인스턴스에 피드 설정
    function setFeed(address addr) {
        // 자동으로 캐스팅되므로 주의하십시오. 생성자는 호출되지 않습니다.
        feed = InfoFeed(addr);
    }

    // 계약의 새 인스턴스에 피드 설정
    function createNewFeed() {
        feed = new InfoFeed(); // 새 인스턴스 생성, 생성자 호출
    }

    function callFeed() {
        // 마지막 괄호는 계약을 호출하며, 선택적으로
        // 사용자 지정 이더 값 또는 가스를 추가할 수 있습니다.
        feed.info{value: 10, gas: 800}();
    }
}

// B. 상속

// 순서가 중요하며, 마지막으로 상속된 계약(즉, 'def')은
// 이전에 상속된 계약의 일부를 재정의할 수 있습니다.
contract MyContract is abc, def("a custom argument to def") {

// 함수 재정의
    function z() {
        if (msg.sender == owner) {
            def.z(); // def에서 재정의된 함수 호출
            super.z(); // 즉각적인 부모 재정의된 함수 호출
        }
    }
}

// 추상 함수
function someAbstractFunction(uint x);
// 컴파일할 수 없으므로 구현되는
// 기본/추상 계약에서 사용됩니다.

// C. 가져오기

import "filename";
import "github.com/ethereum/dapp-bin/library/iterable_mapping.sol";


// 8. 기타 키워드

// A. Selfdestruct
// 현재 계약을 자체 파괴하고 주소(종종 생성자)로 자금 보내기
selfdestruct(SOME_ADDRESS);

// 현재/미래 블록에서 스토리지/코드 제거
// 씬 클라이언트에 도움이 되지만 이전 데이터는 블록체인에 지속됨

// 일반적인 패턴, 소유자가 계약을 종료하고 남은 자금을 받도록 함
function remove() {
    if(msg.sender == creator) { // 계약 생성자만 이 작업을 수행하도록 허용
        selfdestruct(creator); // 계약을 비활성화하고 자금 반환
    }
}

// 자체 파괴 대신 수동으로 계약을 비활성화할 수 있음
// (자체 파괴된 계약으로 전송된 이더는 손실됨)


// 9. 계약 디자인 노트

// A. 난독화
// 모든 변수는 블록체인에서 공개적으로 볼 수 있으므로
// 비공개인 모든 것은 난독화해야 합니다(예: 비밀로 해시).

// 단계: 1. 무언가에 커밋, 2. 커밋 공개
keccak256("some_bid_amount", "some secret"); // 커밋

// 나중에 계약의 공개 함수 호출
// SHA3으로 해시되는 입찰가와 비밀 표시
reveal(100, "mySecret");

// B. 스토리지 최적화
// 데이터가 영원히 저장되므로 블록체인에 쓰는 것은 비용이 많이 들 수 있습니다.
// 메모리를 현명하게 사용하는 방법을 장려합니다(결국 컴파일이 더 나아지겠지만,
// 지금은 데이터 구조를 계획하고 블록체인에 최소한의 양을 저장하는 것이
// 이점입니다).

// 다차원 배열과 같은 항목은 비용이 많이 들 수 있습니다.
// (비용은 채워지지 않은 변수를 선언하는 것이 아니라 데이터를 저장하는 데 따름)

// C. 블록체인의 데이터 액세스
// 인간이나 컴퓨터가 트랜잭션 또는 트랜잭션 상태의
// 내용을 읽는 것을 제한할 수 없습니다.

// 'private'은 다른 *계약*이 데이터를 직접 읽는 것을
// 방지하지만 다른 모든 당사자는 여전히 블록체인의 데이터를 읽을 수 있습니다.

// 시간 시작까지의 모든 데이터는 블록체인에 저장되므로
// 누구나 모든 이전 데이터와 변경 사항을 관찰할 수 있습니다.

// D. 오라클 및 외부 데이터
// 오라클은 블록체인 외부에서 스마트 계약과 상호 작용하는 방법입니다.
// 실제 세계에서 데이터를 가져오거나, 실제 세계에 post 요청을 보내거나
// 그 반대로 하는 데 사용됩니다.

// 계약은 직접 호출해야 하고 시간에 "구독"할 수 없으므로
// 시간 기반 계약 구현도 오라클을 통해 수행됩니다.
// 스마트 계약이 분산되어 있기 때문에 데이터를
// 분산된 방식으로 가져오고 싶을 것입니다. 그렇지 않으면
// 스마트 계약 설계 문제가 방지하는 중앙 집중식 위험에 직면하게 됩니다.

// 미리 상자에 담긴 분산 데이터를 얻고 사용하는 가장 쉬운 방법은 체인링크 데이터 피드입니다.
// https://docs.chain.link/docs/get-the-latest-price
// 여러 소스에서 이미 집계되어 온체인으로 전달된 온체인 참조 지점을
// 참조할 수 있으며, 이를 "데이터 뱅크"로 사용할 수 있습니다.
// 소스의.

// 여기에서 API를 호출하는 다른 예를 볼 수 있습니다:
// https://docs.chain.link/docs/make-a-http-get-request

// 그리고 물론 자신만의 오라클 네트워크를 구축할 수 있습니다.
// 애플리케이션이 얼마나 중앙 집중식인지 분산되어 있는지 확인하십시오.

// 직접 오라클 네트워크 설정

// E. 크론 작업
// 계약은 시간 기반 스케줄링을 처리하기 위해 수동으로 호출해야 합니다.
// 정기적으로 핑을 보내거나 다른 사람에게 인센티브(이더)를 제공하는
// 외부 코드를 만들 수 있습니다.

// F. 옵저버 패턴
// 옵저버 패턴을 사용하면 구독자로 등록하고
// 오라클에서 호출하는 함수를 등록할 수 있습니다(참고, 오라클은
// 이 작업 실행 비용을 지불합니다).
// Pub/sub의 구독과 일부 유사점

// 이것은 추상 계약이며, 클라이언트와 서버 클래스 모두
// 클라이언트가 구현해야 하는 것을 가져옵니다.
contract SomeOracleCallback {
    function oracleCallback(int _value, uint _time, bytes32 info) external;
}

contract SomeOracle {
    SomeOracleCallback[] callbacks; // 모든 구독자 배열

    // 구독자 등록
    function addSubscriber(SomeOracleCallback a) {
        callbacks.push(a);
    }

    function notify(value, time, info) private {
        for(uint i = 0;i < callbacks.length; i++) {
            // 호출된 모든 구독자는 oracleCallback을 구현해야 합니다.
            callbacks[i].oracleCallback(value, time, info);
        }
    }

    function doSomething() public {
        // 무언가를 하는 코드

        // 모든 구독자에게 알림
        notify(_value, _time, _info);
    }
}

// 이제 클라이언트 계약은 SomeOracleCallback을 가져와
// Some Oracle에 등록하여 addSubscriber를 추가할 수 있습니다.

// G. 상태 머신
// State 열거형 및 inState 수정자에 대한 아래 예 참조
```

아래 전체 예제를 [`Remix의 Remix VM`을 사용하여 여기서 작업하십시오.](https://remix.ethereum.org/#version=soljson-v0.8.19+commit7dd6d404.js&optimize=false&evmVersion=null&gist=2acb47a0286fe45d2464fa937f00fef3&runs=200)

```solidity
// *** 예제: 크라우드펀딩 예제(킥스타터와 대체로 유사) ***
// ** 예제 시작 **

// CrowdFunder.sol
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/// @title CrowdFunder
/// @author nemild
contract CrowdFunder {
    // 생성자가 생성 시 설정하는 변수
    address public creator;
    address payable public fundRecipient; // 생성자는 수신자와 다를 수 있으며 payable이어야 함
    uint public minimumToRaise; // 팁을 주기 위해 필요, 그렇지 않으면 모두 환불받음
    string campaignUrl;
    uint256 version = 1;

    // 자료 구조
    enum State {
        Fundraising,
        ExpiredRefund,
        Successful
    }
    struct Contribution {
        uint amount;
        address payable contributor;
    }

    // 상태 변수
    State public state = State.Fundraising; // 생성 시 초기화
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

    // 계약 파기를 허용하기 전에 최종 계약 상태 후 24주 대기
    modifier atEndOfLifecycle() {
    require(((state == State.ExpiredRefund || state == State.Successful) &&
        completeAt + 24 weeks < block.timestamp));
        _;
    }

    function crowdFund(
        uint timeInHoursForFundraising,
        string memory _campaignUrl,
        address payable _fundRecipient,
        uint _minimumToRaise)
        public
    {
        creator = msg.sender;
        fundRecipient = _fundRecipient;
        campaignUrl = _campaignUrl;
        minimumToRaise = _minimumToRaise;
        raiseBy = block.timestamp + (timeInHoursForFundraising * 1 hours);
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
                contributor: payable(msg.sender)
            }) // 배열 사용, 반복 가능
        );
        totalRaised += msg.value;

        emit LogFundingReceived(msg.sender, msg.value, totalRaised);

        checkIfFundingCompleteOrExpired();
        return contributions.length - 1; // id 반환
    }

    function checkIfFundingCompleteOrExpired()
    public
    {
        if (totalRaised > minimumToRaise) {
            state = State.Successful;
            payOut();

            // 상태 변경을 시작한 발신자에게 인센티브를 줄 수 있음
        } else if ( block.timestamp > raiseBy )  {
            state = State.ExpiredRefund; // 후원자는 이제 getRefund(id)를 호출하여 환불을 받을 수 있음
        }
        completeAt = block.timestamp;
    }

    function payOut()
    public
    inState(State.Successful)
    {
        fundRecipient.transfer(address(this).balance);
        emit LogWinnerPaid(fundRecipient);
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

    // 경고: "selfdestruct"는 더 이상 사용되지 않습니다.
    // 칸쿤 하드 포크부터 기본 opcode는 더 이상
    // 계정과 관련된 코드 및 데이터를 삭제하지 않고 이더만 수혜자에게 전송합니다.
    // 계약이 생성된 동일한 트랜잭션에서 실행되지 않는 한(EIP-6780 참조).

    // 새로운 동작을 고려하더라도 새로 배포된 계약에서 사용하는 것은
    // 강력히 권장되지 않습니다.
    // EVM에 대한 향후 변경 사항은 opcode의 기능을 더욱 감소시킬 수 있습니다.
    // function removeContract()
    // public
    // isCreator()
    // atEndOfLifecycle()
    // {
    //     selfdestruct(msg.sender);
    //     // 생성자는 청구되지 않은 모든 돈을 받습니다.
    // }
}
// ** 예제 끝 **
```

몇 가지 추가 기능.

```solidity
// 10. 기타 네이티브 함수

// 통화 단위
// 통화는 가장 작은 이더 단위인 wei를 사용하여 정의됩니다.
uint minAmount = 1 wei;
uint a = 1 finney; // 1 ether == 1000 finney
// 다른 단위, 참조: http://ether.fund/tool/converter

// 시간 단위
1 == 1 second
1 minutes == 60 seconds

// 단위는 변수에 저장되지 않으므로 변수에 단위를 곱할 수 있습니다.
uint x = 5;
(x * 1 days); // 5 days

// 시간에 대한 등호 문으로 윤초/년에 주의하십시오.
// (대신, 보다 큼/작음 선호)

// 암호화
// 전달된 모든 문자열은 해시 작업 전에 연결됩니다.
keccak256("ab", "cd");
ripemd160("abc");
sha256("def");

// 11. 보안

// 이더리움 계약에서 버그는 재앙이 될 수 있으며, 솔리디티의 인기 있는 패턴조차도
// 안티패턴으로 발견될 수 있습니다.

// 이 문서 끝에 있는 보안 링크 참조

// 12. 저수준 함수
// call - 저수준, 자주 사용되지 않음, 유형 안전성을 제공하지 않음
(bool success, bytes memory data) = someContractAddress.call(
    abi.encodeWithSignature("function_name(string,string)", "arg1", "arg2")
);

// delegatecall - 대상 주소의 코드가 호출 계약의 *컨텍스트*에서 실행됨
// 라이브러리 기능 제공
(bool success, bytes memory data) = someContractAddress.delegatecall(
    abi.encodeWithSignature("function_name()")
);


// 13. 스타일 노트
// Python의 PEP8 스타일 가이드 기반
// 전체 스타일 가이드: http://solidity.readthedocs.io/en/develop/style-guide.html

// 간단한 요약:
// 들여쓰기 4칸
// 계약 선언(및 기타 최상위 선언)을 구분하는 두 줄
// 괄호 안의 불필요한 공백 피하기
// 한 줄 문(if, for 등)에 대해 중괄호 생략 가능
// else는 자체 줄에 배치해야 함


// 14. NATSPEC 주석
// 문서화, 주석 및 외부 UI에 사용됨

// 계약 natspec - 항상 계약 정의 위에
/// @title 계약 제목
/// @author 저자 이름

// 함수 natspec
/// @notice 함수가 하는 일에 대한 정보, 실행할 함수일 때 표시됨
/// @dev 개발자를 위한 함수 문서

// 함수 매개변수/반환 값 natspec
/// @param someParam 매개변수가 하는 일에 대한 설명
/// @return 반환 값에 대한 설명
```

## 추가 자료
- [솔리디티 문서](https://solidity.readthedocs.org/en/latest/)
- [Cyfrin Updraft (솔리디티 개발 배우기)](https://updraft.cyfrin.io/)
- [체인링크 초보자 튜토리얼](https://docs.chain.link/docs/beginners-tutorial)
- [스마트 계약 모범 사례](https://github.com/ConsenSys/smart-contract-best-practices)
- [브라우저 기반 솔리디티 편집기](https://remix.ethereum.org/)
- [이더리움 계약을 위한 모듈식 디자인 전략](https://docs.erisindustries.com/tutorials/solidity/)
- [체인링크 문서](https://docs.chain.link/docs/getting-started)

## 스마트 계약 개발 프레임워크
- [Foundry](https://getfoundry.sh/)
- [Hardhat](https://hardhat.org/)
- [Brownie](https://github.com/eth-brownie/brownie)

## 중요한 라이브러리
- [OpenZeppelin](https://github.com/OpenZeppelin/openzeppelin-contracts): 일반적인 계약 패턴(크라우드펀딩, safemath 등)을 제공하는 라이브러리
- [체인링크](https://github.com/smartcontractkit/chainlink): 외부 데이터와 상호 작용할 수 있는 코드

## 샘플 계약
- [Dapp Bin](https://github.com/ethereum/dapp-bin)
- [Defi 예제](https://github.com/PatrickAlphaC/chainlink_defi)
- [솔리디티 아기 걸음 계약](https://github.com/fivedogit/solidity-baby-steps/tree/master/contracts)
- [ConsenSys 계약](https://github.com/ConsenSys/dapp-store-contracts)
- [Dapps의 상태](http://dapps.ethercasts.com/)
- [보안 코드베이스 예제](https://github.com/Cyfrin/sc-exploits-minimized/)

## 보안
- [스마트 계약 보안 커리큘럼](https://updraft.cyfrin.io/courses/security)
- [스마트 계약 보안 보고서 데이터베이스](https://solodit.xyz/)
- [스마트 계약 보안에 대한 생각](https://blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [스마트 계약 보안](https://blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Hacking Distributed 블로그](http://hackingdistributed.com/)

## 스타일
- [솔리디티 스타일 가이드](http://solidity.readthedocs.io/en/latest/style-guide.html): 이더리움의 스타일 가이드는 Python의 [PEP 8](https://www.python.org/dev/peps/pep-0008/) 스타일 가이드에서 크게 파생되었습니다.

## 편집기
- [Remix](https://remix.ethereum.org/)
- [Emacs 솔리디티 모드](https://github.com/ethereum/emacs-solidity)
- [Vim 솔리디티](https://github.com/tomlion/vim-solidity)
- 편집기 스니펫 ([Ultisnips 형식](https://gist.github.com/nemild/98343ce6b16b747788bc))

## 향후 할 일
- 일반적인 디자인 패턴 목록(조절, RNG, 버전 업그레이드)
- 일반적인 보안 안티 패턴

편집 내용이 있는 풀 리퀘스트를 보내거나 nemild -/at-/ gmail로 이메일을 보내주십시오.
