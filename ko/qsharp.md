---
name: Q#
contributors:
    - ["Vincent van Wingerden", "https://github.com/vivanwin"]
    - ["Mariia Mykhailova", "https://github.com/tcNickolas"]
    - ["Andrew Ryan Davis", "https://github.com/AndrewDavis1191"]
    - ["Alex Hansen", "https://github.com/sezna"]
filename: LearnQSharp.qs
---

Q#은 개발자가 양자 알고리즘을 작성할 수 있도록 하는 고급 도메인 특정 언어입니다. Q# 프로그램은 클래식 컴퓨터에서 실행되는 양자 시뮬레이터와 (향후) 양자 컴퓨터에서 실행할 수 있습니다.

```c#
// 한 줄 주석은 //로 시작합니다


/////////////////////////////////////
// 1. 양자 데이터 유형 및 연산자

// 양자 프로그램의 가장 중요한 부분은 큐비트입니다.
// Q#에서 Qubit 유형은 사용할 수 있는 큐비트를 나타냅니다.
// 이것은 변수 qs로 두 개의 새 큐비트 배열을 할당합니다.
operation QuantumDataTypes() : Unit {
    use qs = Qubit[2];

    // 큐비트에는 직접 읽거나 수정할 수 없는 내부 상태가 있습니다.
    // 클래식 시뮬레이터에서 실행하는 경우 양자 프로그램의
    // 현재 상태를 검사할 수 있습니다.
    // 실제 양자 하드웨어에서는 작동하지 않습니다!
    Std.Diagnostics.DumpMachine();

    // 큐비트의 상태를 변경하려면
    // 큐비트에 양자 게이트를 적용해야 합니다.
    H(qs[0]);   // 이것은 첫 번째 큐비트의 상태를 변경합니다
    // |0⟩ (할당된 큐비트의 초기 상태)에서
    // (|0⟩ + |1⟩) / sqrt(2)로.
    // qs[1] = |1⟩; - 이것은 작동하지 않으며, 게이트를 사용하여 큐비트를 조작해야 합니다.

    // 여러 큐비트에 다중 큐비트 게이트를 적용할 수 있습니다.
    CNOT(qs[0], qs[1]);

    // 게이트의 제어된 버전을 적용할 수도 있습니다:
    // 모든 제어 큐비트가 |1⟩ 상태일 때 적용되는 게이트입니다.
    // 첫 번째 인수는 제어 큐비트 배열이고,
    // 두 번째 인수는 대상 큐비트입니다.
    Controlled Y([qs[0]], qs[1]);

    // 역제어 게이트를 적용하려면
    // (모든 제어 큐비트가 |0⟩ 상태일 때 적용되는 게이트),
    // 라이브러리 함수를 사용할 수 있습니다.
    ApplyControlledOnInt(0, X, [qs[0]], qs[1]);

    // 양자 시스템에서 정보를 읽으려면 측정을 사용합니다.
    // 측정은 Result 데이터 유형의 값(Zero 또는 One)을 반환합니다.
    // 측정 결과를 클래식 값으로 인쇄할 수 있습니다.
    Message($"Measured {M(qs[0])}, {M(qs[1])}");
}


/////////////////////////////////////
// 2. 클래식 데이터 유형 및 연산자

function ClassicalDataTypes() : Unit {
    // Q#의 숫자는 Int, BigInt 또는 Double에 저장할 수 있습니다.
    let i = 1;            // 이것은 1과 같은 Int 변수 i를 정의합니다
    let bi = 1L;          // 이것은 1과 같은 BigInt 변수 bi를 정의합니다
    let d = 1.0;          // 이것은 1과 같은 Double 변수 d를 정의합니다

    // 유형이 동일한 한 산술은 예상대로 수행됩니다
    let n = 2 * 10;                // = 20
    // Q#에는 암시적 유형 캐스트가 없으므로
    // 다른 유형의 값에 대해 산술을 수행하려면
    // 명시적으로 유형을 캐스트해야 합니다
    let nd = Std.Convert.IntAsDouble(2) * 1.0; // = 20.0

    // 부울 유형은 Bool이라고 합니다
    let trueBool = true;
    let falseBool = false;

    // 논리 연산자는 예상대로 작동합니다
    let andBool = true and false;
    let orBool = true or false;
    let notBool = not false;

    // 문자열
    let str = "Hello World!";

    // 등호는 == 입니다
    let x = 10 == 15; // 거짓입니다

    // 범위는 정수 시퀀스이며 다음과 같이 정의할 수 있습니다: start..step..stop
    let xi = 1..2..7; // 시퀀스 1,3,5,7을 제공합니다

    // 변수에 새 값 할당:
    // 기본적으로 모든 Q# 변수는 불변입니다.
    // 변수가 let을 사용하여 정의된 경우 값을 다시 할당할 수 없습니다.

    // 변수를 변경 가능하게 만들려면 다음과 같이 선언해야 합니다.
    // 그리고 set 단어를 사용하여 값을 업데이트합니다
    mutable xii = true;
    set xii = false;

    // 다음과 같이 모든 데이터 유형에 대한 배열을 만들 수 있습니다
    let xiii = [0.0, size = 10];

    // 배열에서 요소 가져오기
    let xiv = xiii[8];

    // 배열 요소에 새 값 할당
    mutable xv = [0.0, size = 10];
    set xv w/= 5 <- 1.0;
}


/////////////////////////////////////
// 3. 제어 흐름

operation ControlFlow() : Unit {
    let a = 1;
    // If 표현식은 true 분기, elif 및 else를 지원합니다.
    if (a == 1) {
        // ...
    } elif (a == 2) {
        // ...
    } else {
        // ...
    }
    use qubits = Qubit[2];

    // For 루프를 사용하여 배열을 반복할 수 있습니다
    for qubit in qubits {
        X(qubit);
    }

    // 일반 for 루프를 사용하여 숫자 범위를 반복할 수 있습니다
    for index in 0..Length(qubits) - 1 {
        X(qubits[index]);
    }

    // While 루프는 클래식 컨텍스트에서만 사용하도록 제한됩니다
    mutable index = 0;
    while (index < 10) {
        set index += 1;
    }

    let success_criteria = true;
    // while 루프의 양자 등가물은 repeat-until-success 루프입니다.
    // 양자 컴퓨팅의 확률적 특성으로 인해 때로는
    // 특정 작업 시퀀스를 반복하고 싶을 수 있습니다
    // 특정 조건이 달성될 때까지; 이 루프를 사용하여 이를 표현할 수 있습니다.
    repeat {
        // 여기에 작업
    } until (success_criteria) // 상태가 도달했는지 확인하기 위한 측정일 수 있습니다
    fixup {
        // 필요한 경우 초기 조건으로 재설정
    }
}

/////////////////////////////////////
// 4. 모두 합치기

// Q# 코드는 연산과 함수로 작성됩니다
operation ApplyXGate(source : Qubit) : Unit {
    X(source);
}

// 연산이 단일 변환을 구현하는 경우
// 해당 연산의 인접 및 제어된 변형을 정의할 수 있습니다.
// 가장 쉬운 방법은 Unit 뒤에 "is Adj + Ctl"을 추가하는 것입니다.
// 이것은 컴파일러에 변형을 자동으로 생성하도록 지시합니다.
operation ApplyXGateCA(source : Qubit) : Unit is Adj + Ctl {
    X(source);
}

// 이제 Adjoint ApplyXGateCA 및 Controlled ApplyXGateCA를 호출할 수 있습니다.


// Q# 코드를 실행하려면 먼저 실행하려는 연산 앞에 @EntryPoint()를 넣을 수 있습니다
operation XGateDemo() : Unit {
    use q = Qubit();
    ApplyXGate(q);
}

// 간단한 예: 양자 난수 생성기입니다.
// 양자 코드를 사용하여 클래식 난수 비트 배열을 생성합니다.
// `Main`이라는 이름의 호출 가능 항목(함수 또는 연산)은 진입점으로 사용됩니다.
operation Main() : Unit {
    mutable bits = [0, size = 5];                // 비트를 저장하는 데 사용할 배열
    use q  = Qubit();
    {
        // 큐비트 할당
        for i in 0..4 {
            // 각 비트를 독립적으로 생성
            H(q);                             // Hadamard 게이트는 동일한 중첩을 설정합니다
            let result = M(q);                // 큐비트 측정은 50/50 확률로 0|1을 얻습니다
            let bit = result == Zero ? 0 | 1; // 측정 결과를 정수로 변환
            set bits w/= i <- bit;            // 생성된 비트를 배열에 씁니다
        }
    }
    Message($"{bits}");                       // 결과 인쇄
}
```


## 더 읽을거리

Quantum Katas ([저장소](https://github.com/microsoft/qsharp/tree/main/katas) [호스팅된 튜토리얼](https://quantum.microsoft.com/en-us/tools/quantum-katas))는 양자 컴퓨팅과 Q#을 배우기 위한 훌륭한 자습형 튜토리얼과 프로그래밍 연습을 제공합니다.

[Q# 문서](https://docs.microsoft.com/quantum/)는 언어 참조 및 사용자 가이드를 포함한 공식 Q# 문서입니다.
