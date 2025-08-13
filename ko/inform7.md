---
name: Inform7
contributors:
    - ["Hyphz", "http://github.com/hyphz/"]
filename: LearnInform.Inform
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Inform 7은 Graham Nelson과 Emily Short가 텍스트 어드벤처를 작성하기 위해 만든 자연어 기반 언어이지만, 특히 데이터 기반의 다른 텍스트 기반 애플리케이션에도 잠재적으로 사용할 수 있습니다.

```inform7
[이것은 주석입니다.]

[Inform 7은 텍스트 어드벤처를 구축하기 위해 설계된 언어입니다.
다른 목적으로도 사용할 수 있지만, 기본 라이브러리는 텍스트 어드벤처를 구축합니다. Inform 7은 객체 지향입니다.]

[이것은 하위 클래스화를 통해 클래스를 만듭니다. "Value"는 보편적인 하위 클래스이지만, "object"는 OO 객체처럼 작동하는 가장 기본적인 것입니다.]
A datablock is a kind of object.

[클래스는 속성을 가질 수 있습니다.]
A datablock can be broken. [이것은 부울 속성을 만듭니다.]
A datablock is usually not broken. [이것은 기본값을 설정합니다.]
A datablock can be big or small. [이것은 열거형 속성을 만듭니다.]
A datablock is usually small. [이것은 기본값을 설정합니다.]
A datablock has a number called the sequence number. [이것은 유형이 지정된 속성을 만듭니다.]
A datablock has some text called the name. ["Some text"는 문자열을 의미합니다.]
A datablock has a datablock called the chain. [선언된 클래스는 유형이 됩니다.]

[이것은 전역 명명된 인스턴스를 만듭니다.]
Block1 is a datablock.
The sequence number of Block1 is 1.
The name of Block1 is "Block One."

[함수와 프로시저는 "구문"으로 정의됩니다.]
To do the thing everyone does with their first program:
	say "Hello World.". [마침표는 끝을 나타내고, 들여쓰기는 범위를 나타냅니다.]

To dump (the block - a datablock): [이것이 매개변수를 만드는 방법입니다.]
	say the sequence number of the block;
	say the name of the block;
	if the block is broken, say "(Broken)".

To toggle (the block - a datablock):
	if the block is broken: [조건문.]
		now the block is not broken; [속성 업데이트.]
	else:
		now the block is broken.

[여러 매개변수.]
To fix (the broken block - a datablock) using (the repair block - a datablock):
	if the broken block is not broken, stop; [들여쓰기 없는 단일 명령에 대한 쉼표.]
	if the repair block is broken, stop;
	now the sequence number of the broken block is the sequence number of the repair block;
	now the broken block is not broken.

[텍스트 어드벤처 기원 때문에 Inform 7은 일반적으로 객체를 동적으로 생성하는 것을 허용하지 않지만, 이를 가능하게 하는 언어 확장이 있습니다.]
Block2 is a datablock.
Block2 is broken.
The sequence number of Block2 is 2.
The name of Block2 is "Block two."

To demonstrate calling a phrase with two parameters:
	Let the second block be block2; [지역 포인터 변수.]
	fix the second block using Block1;
	say the sequence number of the second block. [1.]

[목록.]
To show how to use list types:
	let the list be a list of datablocks;
	add Block1 to the list;
	add Block2 to the list;
	say the list; ["Block1 and Block2"]
	[멤버십.]
	if Block1 is listed in the list:
		say "Block1 is there.";
	[루프.]
	repeat with the block running through the list:
		dump the block;  [1 Block One. 1 Block Two.]
		[Remember block two's sequence number was changed above.]
	let X be entry 2 of the list; [계산은 1부터 시작합니다.]
	dump X; ["1 Block two."]
	remove X from the list;
	say the list. [Block1]

[함수를 정의하고 산술을 수행하는 방법은 다음과 같습니다.]

To decide which number is the sum of all numbers up to (X - a number) (this is summing up):
	let the total so far be a number;
	repeat with the current number running from 1 to X:
		now the total so far is the total so far + the current number;
	decide on the total so far. [이것은 반환 문입니다.]

[ 고차 함수도 있습니다. ]

To demonstrate a higher order function:
	say summing up applied to {1, 2, 3, 4}.

To decide which number is the result of applying (phrase - phrase A -> A) twice to (B - a value of kind A):
	let b1 be phrase applied to B;
	let b2 be phrase applied to b1;
	decide on b2.

To demonstrate defining a higher order function:
	let X be 5;
	say the result of applying summing up twice to X.

[ 규칙집은 다른 조건에서 동일한 유형에 적용되는 여러 함수를 쌓을 수 있습니다. ]

Datablock validation rules is a datablock based rulebook.

A datablock validation rule for a broken datablock: rule fails.
A datablock validation rule for a datablock (called the block):
	dump the block;
	rule succeeds.

To demonstrate invoking a rulebook:
	follow datablock validation rules for Block1;
	follow datablock validation rules for Block2.

[ 객체는 관계형 데이터베이스의 관계와 유사한 관계를 가질 수도 있습니다. ]
A dog is a kind of thing.
Rover is a dog.
The kennel is a container. [이것은 내장된 기본 클래스입니다.]
Rover is in the kennel. [이것은 "포함"이라는 내장 관계를 만듭니다.]

[유형을 선언하여 관계를 만들 수 있습니다.]

Guide dog ownership relates one dog to one person. [일대일.]
Property ownership relates various things to one person. [다대일.]
Friendship relates various people to various people.  [다대다.]

[실제로 사용하려면 동사나 전치사를 할당해야 합니다.]

The verb to own means the property ownership relation.
The verb to be the guide dog of means the guide dog ownership relation.
The verb to be guided by means the reversed guide dog ownership relation.
The verb to be friends with means the friendship relation.

Edward is a person. A person can be blind. Edward is blind.
Edward is guided by Rover.
Benny is a person. Edward is friends with Benny.

To demonstrate looking something up with a relation:
	repeat with the dog running through things that are the guide dog of Edward:
		say the dog;
	repeat with the friend running through things that are friends with Edward:
		say the friend.

[절차적으로 존재하는 관계를 정의할 수도 있습니다.]

Helpfulness relates a person (called the helper) to a person (called the helpee) when the helpee is blind and the helper is not blind.
The verb to be helpful to means the helpfulness relation.
To demonstrate using a procedural relation:
	repeat with the helper running through people that are helpful to Edward:
		say the helper.


[ 위 코드를 실행할 수 있도록 텍스트 어드벤처 하네스에 대한 인터페이스입니다. ]
Tutorial room is a room.
"A rather strange room full of buttons. Push them to run the exercises, or turn on the robot to run them all."
A button is a kind of thing. A button is fixed in place.

The red button is a button in tutorial room.
Instead of pushing the red button, do the thing everyone does with their first program.
The green button is a button in tutorial room.
Instead of pushing the green button, demonstrate calling a phrase with two parameters.
The blue button is a button in tutorial room.
Instead of pushing the blue button, show how to use list types.
The cyan button is a button in tutorial room.
Instead of pushing the cyan button, say the sum of all numbers up to 5.
The purple button is a button in tutorial room.
Instead of pushing the purple button, demonstrate a higher order function.
The black button is a button in tutorial room.
Instead of pushing the black button, demonstrate defining a higher order function.
The white button is a button in tutorial room.
Instead of pushing the white button, demonstrate invoking a rulebook.
The puce button is a button in tutorial room.
Instead of pushing the puce button, demonstrate looking something up with a relation.
The orange button is a button in tutorial room.
Instead of pushing the orange button, demonstrate using a procedural relation.

The robot is an object in tutorial room.
Instead of switching on the robot:
	say "The robot begins to frantically flail its arms about.";
	repeat with button running through buttons in the tutorial room:
		say "The robot randomly hits [the button].";
		try pushing button.
```

## 더 배울 준비가 되셨습니까?

* [Inform 7](http://www.inform7.com/)