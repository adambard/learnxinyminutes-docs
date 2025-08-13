---
name: Hjson
filename: learnhjson.hjson
contributors:
  - ["MrTeferi", "https://github.com/MrTeferi"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Hjson은 [JSON](../json/)을 더 사람이 읽기 쉽게 만들려는 시도입니다.

Hjson은 JSON에 대한 구문 확장입니다.
JSON을 대체하거나 JSON 사양 자체에 통합하려는 제안이 아닙니다.
사람이 읽고 편집한 후 JSON 데이터를 기계에 전달하기 위한 사용자 인터페이스처럼 사용하도록 만들어졌습니다.

주요 구문 차이점을 보기 위해 예제를 살펴보겠습니다!

```
{
    # 주석은 완전히 지원됩니다!

    // 슬래시로도 가능합니다!

    /*
        블록 스타일 주석도 가능합니다, 멋지네요!
    /*

    # 문자열은 따옴표가 필요 없습니다!
    # 한 줄로 유지하십시오.
    human: readable
    quotes: "are fine too"

    # 쉼표도 필요하지 않습니다!
    # 쉼표를 사용하는 경우 문자열에는 따옴표가 필요합니다!
    object: {
        name: Hjson
        properties: [
            readable
            exciting
            fun
        ]
        with_commas: [
            "quoted",
            "quoty",
            "quote"
        ]
        details: ["this", "is", "fine", "too"]
    }

    # 적절한 공백 처리가 있는 여러 줄 따옴표가 지원됩니다!
    diary:
        '''
        JSON이 더 사람이 읽기 쉬웠으면 좋겠습니다.
        내 필요에 맞는 JSON이 있다면!
        아, 잠깐.. 있습니다! Hjson이라고 합니다.
        '''

    # 백슬래시는 따옴표로 묶인 문자열에서만 이스케이프 문자로 해석됩니다.
    slash: This will not have a new line\n
    slash-quoted: "This will definitely have a new line\n"

    # 공백과 중요한 구두점을 혼합할 때 따옴표를 사용하십시오.
    example1: "If, you're, going, to, comma in a string, use, quotes!"
    example2: "Also if you want to use {} or [] or any JSON relevant punctuation!"
    example3: [because, this, is, totally, BROKEN!]
    example4: this is technically OK though: {}[],:

    # Hjson으로 즐겁게 작업하세요!
    party-time: {
        Hjson-lovers: [
            me
            my mom
            "my dad"
        ]
        Hjson-power-level: 9000
        supported: {
            python: yes
            java: yes
            javascript: yes
            c++: yes
            Go: yes
            C#: yes
            Rust: yes
        }
        partial-support: ["C", "Kotlin", "Ruby", "Rust"]
    }

}
```

## 추가 자료

* [Hjson.github.io](https://hjson.github.io/) 편집기 지원, 방법 등을 포함한 주요 Hjson 사이트.
* [Hjson 패키지](https://github.com/hjson/) 다양한 애플리케이션을 위한 다양한 Hjson 패키지.