---
category: tool
name: Protocol Buffers
filename: protocol-buffers.proto
contributors:
    - ["Shankar Shastri", "https://github.com/shankarshastri"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

프로토콜 버퍼는 구조화된 데이터를 직렬화하기 위한 구글의 언어 중립적, 플랫폼 중립적, 확장 가능한 메커니즘입니다. XML을 생각하되, 더 작고, 빠르고, 간단합니다.
데이터 구조를 한 번 정의하면, 특별히 생성된 소스 코드를 사용하여 다양한 데이터 스트림과 다양한 언어를 사용하여 구조화된 데이터를 쉽게 쓰고 읽을 수 있습니다.
프로토콜 버퍼는 메시지의 스키마입니다. 언어에 구애받지 않습니다.
다양한 언어에 대해 protoc 컴파일러가 생성한 코드를 사용하여 바이너리로 변환하고 다시 메시지 형식으로 변환할 수 있습니다.

```protobuf
/*
* 언어 구문
*/

/*
* 프로토콜 버퍼 버전 구문 지정
* 사용할 프로토콜 버퍼 버전 지정
* 보통 proto3 또는 proto2일 수 있습니다.
*/
syntax = "proto3";

/*
* 프로토콜 버퍼에서 메시지 선언:
* 보시다시피, 메시지 정의의 각 필드에는 고유한 번호가 있습니다.
* 이 필드 번호는 메시지 바이너리 형식에서 필드를 식별하는 데 사용되며,
* 메시지 유형이 사용되면 변경해서는 안 됩니다.
* 1에서 15 범위의 필드 번호는 필드 번호와 필드 유형을 포함하여 인코딩하는 데 1바이트가 걸립니다(자세한 내용은 프로토콜 버퍼 인코딩에서 확인할 수 있습니다).
* 16에서 2047 범위의 필드 번호는 2바이트가 걸립니다. 따라서 매우 자주 발생하는 메시지 요소에 대해 1에서 15까지의 번호를 예약해야 합니다.
* 나중에 추가될 수 있는 자주 발생하는 요소를 위해 약간의 공간을 남겨 두는 것을 잊지 마십시오.
* 지정할 수 있는 가장 작은 필드 번호는 1이고 가장 큰 필드 번호는 2^29 - 1 또는 536,870,911입니다.
* 또한 19000에서 19999까지의 숫자(FieldDescriptor::kFirstReservedNumber ~ FieldDescriptor::kLastReservedNumber)는 사용할 수 없습니다.
* 프로토콜 버퍼 구현을 위해 예약되어 있기 때문입니다. .proto에서 이러한 예약된 번호 중 하나를 사용하면 프로토콜 버퍼 컴파일러가 불평할 것입니다.
* 마찬가지로 이전에 예약된 필드 번호는 사용할 수 없습니다.
*
*/

/*
메시지 선언 구문:
    message ${MessageName} {
        ${Scalar Value Type} ${FieldName1} = ${Tag Number1};
                .
                .
                .
        ${Scalar Value Type} ${FieldNameN} = ${Tag NumberN};
    }

메시지에 정의된 기존 필드가 포함되어 있지 않은 경우 기본값이 적용됩니다.
*/

message MessageTypes {
    /*
    * 스칼라 값 유형
    */
    string stringType = 1; // 문자열은 항상 UTF-8로 인코딩되거나 7비트 ASCII 텍스트를 포함해야 합니다. 기본값 = ""

    // 숫자 유형, 기본값 = 0
    int32 int32Type = 2; // 가변 길이 인코딩을 사용합니다. 음수에는 비효율적이므로 대신 sint32를 사용하십시오.
    int64 int64Type = 3; // 가변 길이 인코딩을 사용합니다. 음수에는 비효율적이므로 대신 sint64를 사용하십시오.
    uint32 uInt32Type = 4; // 가변 길이 인코딩을 사용합니다.
    uint64 uInt64Type = 5; // 가변 길이 인코딩을 사용합니다.
    sint32 sInt32Type = 6; // 가변 길이 인코딩을 사용합니다. 음수 인코딩에 효율적입니다.
                           // 음수에는 int32 대신 이것을 사용하십시오.
    sint64 sInt64Type = 7; // 가변 길이 인코딩을 사용합니다. 음수 인코딩에 효율적입니다.
    // 음수에는 int64 대신 이것을 사용하십시오.

    fixed32 fixed32Type = 8; // 항상 4바이트입니다. 값이 종종 2^28보다 큰 경우 uint32보다 효율적입니다.
    fixed64 fixed64Type = 9; // 항상 8바이트입니다. 값이 종종 2^56보다 큰 경우 uint64보다 효율적입니다.

    sfixed32 sfixed32Type = 10; // 항상 4바이트입니다.
    sfixed64 sfixed64Type = 11; // 항상 8바이트입니다.

    bool boolType = 12; // 부울 유형. 기본값 = false

    bytes bytesType = 13; // 임의의 바이트 시퀀스를 포함할 수 있습니다. 기본값 = 빈 바이트

    double doubleType = 14;
    float floatType = 15;

    enum Week {
        UNDEFINED = 0; // 태그 0은 항상 열거형의 경우 기본값으로 사용됩니다.
        SUNDAY = 1;
        MONDAY = 2;
        TUESDAY = 3;
        WEDNESDAY = 4;
        THURSDAY = 5;
        FRIDAY = 6;
        SATURDAY = 7;
    }
    Week wkDayType = 16;

    /*
    * 스칼라 값 유형의 컬렉션 정의
    * 구문: repeated ${ScalarType} ${name} = TagValue
    */
    repeated string listOfString = 17; // List[String]
}

/*
* 다른 메시지 정의에서 정의된 메시지 유형 정의
*/
message Person {
    string fname = 1;
    string sname = 2;
}

message City {
    Person p = 1;
}

/*
* 중첩 메시지 정의
*/

message NestedMessages {
    message FirstLevelNestedMessage {
        string firstString = 1;
        message SecondLevelNestedMessage {
            string secondString = 2;
        }
    }
    FirstLevelNestedMessage msg = 1;
    FirstLevelNestedMessage.SecondLevelNestedMessage msg2 = 2;
}

/*
* 파일에서 메시지 가져오기
*/

// one.proto
// message One {
//     string oneMsg = 1;
// }

// two.proto
//  import "myproject/one.proto"
//  message Two {
//       string twoMsg = 2;
//  }


/*
* 고급 주제
*/

/*
* 메시지 유형 변경 처리:
* 제거된 메시지 필드의 태그 번호를 절대 변경하거나 사용하지 마십시오.
* 메시지 정의 업데이트 시에는 reserved를 사용해야 합니다.
* (https://developers.google.com/protocol-buffers/docs/proto3#updating)
*/

/*
* 예약된 필드
* 메시지에 새 필드를 추가/제거해야 하는 경우에 사용됩니다.
* Reserved를 사용하여 메시지의 이전 및 이후 호환성을 달성할 수 있습니다.
*/


message ReservedMessage {
    reserved 0, 1, 2, 3 to 10; // 재사용할 수 없는 태그 번호 집합.
    reserved "firstMsg", "secondMsg", "thirdMsg"; // 재사용할 수 없는 레이블 집합.
}

/*
* Any
* Any 메시지 유형을 사용하면 .proto 정의 없이 메시지를 포함된 유형으로 사용할 수 있습니다.
* Any에는 바이트로 된 임의의 직렬화된 메시지와
* 해당 메시지 유형에 대한 전역적으로 고유한 식별자 역할을 하고 확인되는 URL이 포함됩니다.
* Any가 작동하려면 아래와 같이 가져와야 합니다.
*/
/*
    import "google/protobuf/any.proto";
    message AnySampleMessage {
        repeated google.protobuf.Any.details = 1;
    }

*/


/*
*  OneOf
* 메시지의 일부로 최대 하나의 필드만 존재할 수 있는 경우가 있습니다.
* 참고: OneOf 메시지는 반복할 수 없습니다.
*/

message OneOfMessage {
    oneof msg {
        string fname = 1;
        string sname = 2;
    };
}

/*
* 맵
* 맵 필드는 반복할 수 없습니다.
* 맵의 순서는 보장되지 않습니다.
*/

message MessageWithMaps {
    map<string, string> mapOfMessages = 1;
}


/*
* 패키지
* 프로토콜 메시지 유형 간의 이름 충돌을 방지하는 데 사용됩니다.
* 구문:
    package ${packageName};

    패키지에 액세스하려면;
    ${packageName}.${messageName} = ${tagNumber};
*/

/*
* 서비스
* RPC 시스템에서 사용하기 위해 정의된 메시지 유형.
*  protoc 컴파일러가 다양한 언어에 대해 생성할 때 서비스에 대한 스텁 메서드를 생성합니다.
*/

message SearchRequest {
    string queryString = 1;
}

message SearchResponse {
    string queryResponse = 1;
}
service SearchService {
    rpc Search (SearchRequest) returns (SearchResponse);
}
```

## 다양한 언어로 프로토콜 버퍼용 클래스 생성

```shell
protoc --proto_path=IMPORT_PATH --cpp_out=DST_DIR --java_out=DST_DIR --python_out=DST_DIR --go_out=DST_DIR --ruby_out=DST_DIR --objc_out=DST_DIR --csharp_out=DST_DIR path/to/file.proto
```

## 참고 자료

[Google 프로토콜 버퍼](https://developers.google.com/protocol-buffers/)
