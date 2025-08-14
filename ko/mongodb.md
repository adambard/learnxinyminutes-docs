---
name: MongoDB
filename: mongo.js
contributors:
  - ["Raj Piskala", "https://www.rajpiskala.ml/"]
---

MongoDB는 대용량 데이터 저장을 위한 NoSQL 문서 데이터베이스입니다.

MongoDB는 저장을 위해 컬렉션과 문서를 사용합니다. 각 문서는
딕셔너리나 JavaScript 객체와 유사하게 JSON과 유사한 구문을 사용하는
키-값 쌍으로 구성됩니다.

마찬가지로 MongoDB는 NoSQL 데이터베이스이므로 자체 쿼리 언어인 Mongo
Query Language (MQL)를 사용하며 쿼리에 JSON을 사용합니다.

## 시작하기

### 설치

MongoDB는 [여기](https://docs.mongodb.com/manual/installation/)의 지침에 따라
로컬에 설치하거나 [여기](https://www.mongodb.com/cloud/atlas/register)에서
원격으로 호스팅되는 무료 512MB 클러스터를 생성할 수 있습니다. 설정에 대한
지침이 포함된 비디오 링크는 하단에 있습니다.

이 튜토리얼은 [여기](https://www.mongodb.com/try/download/shell)에서
MongoDB 셸을 가지고 있다고 가정합니다. 동일한 링크에서 그래픽 도구인
MongoDB Compass도 다운로드할 수 있습니다.

### 구성 요소

MongoDB를 설치한 후 여러 명령줄 도구가 있음을 알 수 있습니다.
가장 중요한 세 가지는 다음과 같습니다:

- `mongod` - 데이터 관리 및 쿼리 처리를 담당하는 데이터베이스 서버
- `mongos` - 데이터가 여러 시스템에 분산될 경우 필요한 샤딩 라우터
- `mongo` - 데이터베이스를 구성할 수 있는 데이터베이스 셸 (JavaScript 사용)

일반적으로 `mongod` 프로세스를 시작한 다음 별도의 터미널에서 `mongo`를 사용하여
컬렉션에 액세스하고 수정합니다.

### JSON & BSON

MongoDB의 쿼리는 JSON과 유사한\* 형식으로 만들어지지만, MongoDB는
내부적으로 문서를 이진 JSON (BSON 형식)으로 저장합니다. BSON은
이진 인코딩이므로 JSON처럼 사람이 읽을 수 없습니다. 그러나 이를 통해
최종 사용자는 정수 또는 부동 소수점 타입과 같이 일반 JSON보다 더 많은 타입에
액세스할 수 있습니다. 정규식, 날짜 또는 원시 이진과 같은 다른 많은 타입도
지원됩니다.

[여기](https://docs.mongodb.com/manual/reference/bson-types/)는 지원되는
모든 타입의 전체 목록입니다.

- JSON과 유사하다는 것은 이러한 확장된 타입을 가진 JSON을 의미합니다. 예를 들어,
  MongoDB에서 정규식이나 타임스탬프로 직접 쿼리를 만들 수 있으며
  해당 타입을 가진 데이터를 받을 수도 있습니다.

```js
/////////////////////////////////////////////////////////
/////////////////// 시작하기 /////////////////////
/////////////////////////////////////////////////////////

// mongo 데이터베이스 서버 시작
// 참고 - 프로세스가 터미널을 차지하므로 별도의 터미널에서 이 작업을 수행해야 합니다.
// --fork 옵션을 사용할 수 있습니다.
mongod // --fork

// 원격 Mongo 서버에 연결
// mongo "mongodb+srv://host.ip.address/admin" --username your-username

// Mongoshell에는 적절한 JavaScript 인터프리터가 내장되어 있습니다.
3 + 2 // 5

// 사용 가능한 데이터베이스 표시
// MongoDB에는 admin, config, local과 같은 데이터베이스가 내장되어 있습니다.
show dbs

// 새 데이터베이스로 전환 (기존 또는 존재할 예정)
// 참고: MongoDB에는 데이터베이스에 대한 "create" 명령이 없습니다.
// 데이터베이스는 컬렉션에 데이터가 삽입될 때 생성됩니다.
use employees

// 새 컬렉션 생성
// 참고: 문서를 삽입하면 암시적으로 컬렉션이 생성되므로
// 필수는 아닙니다.
db.createCollection('engineers')
db.createCollection('doctors')

// employees 아래에 어떤 컬렉션이 있는지 확인
show collections

/////////////////////////////////////////////////////////
// 기본 생성/읽기/업데이트/삭제 (CRUD) 작업: ///
/////////////////////////////////////////////////////////

/////////////// 삽입 (생성) /////////////////////////

// 데이터베이스에 한 명의 직원 삽입
// 각 삽입은 acknowledged true 또는 false를 반환합니다.
// 모든 문서에는 자동으로 고유한 _id 값이 할당됩니다.
db.engineers.insertOne({ name: "Jane Doe", age: 21, gender: 'Female' })

// `engineers` 컬렉션에 직원 목록 삽입
// 객체 배열로 삽입 가능
db.engineers.insert([
  { name: "Foo Bar", age: 25, gender: 'Male' },
  { name: "Baz Qux", age: 27, gender: 'Other' },
])

// MongoDB는 객체에 대한 스키마나 구조를 강제하지 않습니다.
// `engineers` 컬렉션에 빈 객체 삽입
db.engineers.insertOne({})

// 필드는 선택 사항이며 나머지 문서와 일치할 필요가 없습니다.
db.engineers.insertOne({ name: "Your Name", gender: "Male" })

// 타입은 다를 수 있으며 삽입 시 유지됩니다.
// 이로 인해 일부 언어에서는 문제를 방지하기 위해 추가 유효성 검사가 필요할 수 있습니다.
db.engineers.insert({ name: ['Foo', 'Bar'], age: 3.14, gender: true })

// 객체나 배열은 문서 내에 중첩될 수 있습니다.
db.engineers.insertOne({
  name: "Your Name",
  gender: "Female",
  skilledIn: [
    "MongoDB",
    "NoSQL",
  ],
  "date-of-birth": {
    "date": 1993-07-20T09:44:18.674Z,
    "age": 26
  },
})

// _id 필드를 재정의할 수 있습니다.
// 문제없이 작동합니다.
db.engineers.insertOne({
  _id: 1,
  name: "An Engineer",
  age: 25,
  gender: "Female",
})

// _id는 컬렉션에 대해 항상 고유해야 하므로 주의하십시오. 그렇지 않으면
// 삽입이 실패합니다.
// _id가 중복 값임을 나타내는 WriteError로 실패합니다.
db.engineers.insertOne({
  _id: 1,
  name: "Another Engineer",
  age: 25,
  gender: "Male",
})

// 이것은 다른 컬렉션이므로 문제없이 작동합니다.
db.doctors.insertOne({
  _id: 1,
  name: "Some Doctor",
  age: 26,
  gender: "Other",
})

/////////////////// 찾기 (읽기) ////////////////////////
// 쿼리는 db.collectionName.find(<filter>) 형식입니다.
// 여기서 <filter>는 객체입니다.

// 지금까지 데이터베이스의 모든 것을 표시하며, 한 번에 최대
// 20개의 문서로 제한됩니다.
// 이 커서를 다음 20개의 문서로 반복하려면 i를 누릅니다.
db.engineers.find({})

// 모든 find() 쿼리의 결과를 예쁘게 출력할 수 있습니다.
db.engineers.find({}).pretty()

// MongoDB 쿼리는 JS 객체를 받아 일치하는 키-값 쌍을 가진 문서를
// 검색합니다.
// 쿼리와 일치하는 첫 번째 문서를 반환합니다.
// 참고: 삽입 순서는 데이터베이스에 보존되지 않으므로 출력이 다를 수 있습니다.
db.engineers.findOne({ name: 'Foo Bar' })

// 일치하는 키-값 속성을 가진 모든 문서를 커서로 반환합니다.
// (배열로 변환 가능)
db.engineers.find({ age: 25 })

// 쿼리에서는 타입이 중요합니다.
// 위의 모든 나이는 정수 타입이므로 아무것도 반환하지 않습니다.
db.engineers.find({ age: '25' })

// find()는 create()와 마찬가지로 중첩된 객체와 배열을 지원합니다.
db.engineers.find({
  name: "Your Name",
  gender: "Female",
  skilledIn: [
    "MongoDB",
    "NoSQL",
  ],
  "date-of-birth": {
    "date": 1993-07-20T09:44:18.674Z,
    "age": 26
  },
})

///////////////////////// 업데이트 ////////////////////////
// 쿼리는 db.collectionName.update(<filter>, <update>) 형식입니다.
// 참고: <update>는 항상 $set 연산자를 사용합니다.
// 이 튜토리얼에서는 몇 가지 연산자를 다룹니다.

// 단일 객체를 업데이트할 수 있습니다.
db.engineers.updateOne({ name: 'Foo Bar' }, { $set: { name: 'John Doe', age: 100 }})

// 또는 동시에 여러 객체를 업데이트합니다.
db.engineers.update({ age: 25 }, { $set: { age: 26 }})

// 문서가 이미 존재하지 않으면 삽입하거나, 존재하면 업데이트하려면
// { upsert: true }를 사용할 수 있습니다.
// matched, upserted, modified 수를 반환합니다.
db.engineers.update({ name: 'Foo Baz' },
  { $set:
    {
      age: 26,
      gender: 'Other'
    }
  },
  { upsert: true }
)

/////////////////////// 삭제 /////////////////////////
// 쿼리는 db.collectionName.delete(<filter>) 형식입니다.

// 쿼리와 일치하는 첫 번째 문서를 삭제하고, 항상 deletedCount를 반환합니다.
db.engineers.deleteOne({ name: 'Foo Baz' })

// 동시에 여러 문서 삭제
db.engineers.deleteMany({ gender: 'Male' })

// 참고: db.collection.removeOne(<filter>) 및
// db.collection.removeMany(<filter>) 메서드도 객체를 삭제하지만
// 반환 값이 약간 다릅니다.
// NodeJS 드라이버에서 더 이상 사용되지 않으므로 여기에 포함되지 않았습니다.

/////////////////////////////////////////////////////////
//////////////////// 연산자 //////////////////////////
/////////////////////////////////////////////////////////

// MongoDB의 연산자는 $ 접두사를 가집니다. 이 튜토리얼에서는 비교 및
// 논리 연산자만 다루지만, 다른 많은 종류의 연산자가 있습니다.

//////////////// 비교 연산자 ///////////////////

// 어떤 조건보다 크거나 같음 찾기
db.engineers.find({ age: { $gt: 25 }})
db.engineers.find({ age: { $gte: 25 }})

// 어떤 조건보다 작거나 같음 찾기
db.engineers.find({ age: { $lt: 25 }})
db.engineers.find({ age: { $lte: 25 }})

// 같거나 같지 않음 찾기
// 참고: $eq 연산자는 대부분의 쿼리에서 암시적으로 추가됩니다.
db.engineers.find({ age: { $eq: 25 }})
db.engineers.find({ age: { $ne: 25 }})

// 배열의 모든 요소와 일치하거나 배열에 없는 모든 요소 찾기
db.engineers.find({ age: { $in: [ 20, 23, 24, 25 ]}})
db.engineers.find({ age: { $nin: [ 20, 23, 24, 25 ]}})

//////////////// 논리 연산자 ///////////////////

// 두 쿼리 절을 함께 연결
// 참고: MongoDB는 대부분의 쿼리에서 이를 암시적으로 수행합니다.
db.engineers.find({ $and: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// 쿼리 조건 중 하나와 일치
db.engineers.find({ $or: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// 쿼리 부정
db.engineers.find({ $not: {
  gender: 'Female'
}})

// 쿼리 조건 중 어느 것과도 일치하지 않아야 함
db.engineers.find({ $nor [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

/////////////////////////////////////////////////////////
//////////////// 데이터베이스 작업: ///////////////////
/////////////////////////////////////////////////////////

// employees 데이터베이스 삭제 (drop)
// 이 작업은 데이터베이스의 모든 문서를 삭제합니다!
db.dropDatabase()

// 일부 데이터가 있는 새 데이터베이스 생성
use example
db.test.insertOne({ name: "Testing data, please ignore!", type: "Test" })

// Mongo 셸 종료
exit

// 데이터베이스를 BSON으로 가져오기/내보내기:

// 모든 데이터베이스에 대해 BSON으로 데이터를 내보내는 Mongodump
// 내보낸 데이터는 "MongoDB Database Tools/bin/dump" 아래에 있습니다.
// 참고: 명령을 찾을 수 없는 경우 "MongoDB Database Tools/bin"으로 이동하여
// 거기서 실행 파일 mongodump를 사용하십시오.

// BSON에서 데이터를 복원하는 Mongorestore
mongorestore dump

// 데이터베이스를 JSON으로 가져오기/내보내기:
// 모든 데이터베이스에 대해 JSON으로 데이터를 내보내는 Mongoexport
mongoexport --collection=example

// 모든 데이터베이스에 대해 JSON으로 데이터를 가져오는 Mongoimport
mongoimport  --collection=example
```

## 더 읽을거리

### 설정 비디오

- [MongoDB 설치 - Windows 10](https://www.youtube.com/watch?v=85A6m1soKww)
- [MongoDB 설치 - Mac](https://www.youtube.com/watch?v=DX15WbKidXY)
- [MongoDB 설치 - Linux
  (Ubuntu)](https://www.youtube.com/watch?v=wD_2pojFWoE)

### 입력 유효성 검사

위의 예제에서 입력 유효성 검사나 구조가 우려되는 경우 다음
ORM을 살펴보는 것이 좋습니다.

- [Mongoose (Node.js)](https://mongoosejs.com/docs/) -
  타입, 필수 값, 최소 및 최대 값을 지원하는 스키마를 통한 입력 유효성 검사.
- [MongoEngine (Python)](http://mongoengine.org/) - Mongoose와 유사하지만
  제 경험상 다소 제한적이었습니다.
- [MongoKit (Python)](https://github.com/namlook/mongokit) - MongoEngine보다
  사용하기 쉬운 또 다른 훌륭한 대안

정적으로 강력한 타입 언어(예: Java, C++, Rust)의 경우, 컴파일 타임에
타입과 구조를 정의하므로 입력 유효성 검사에 라이브러리가 필요하지 않습니다.

### 자료

시간이 있다면 [MongoDB University](https://university.mongodb.com/)의
과정을 강력히 추천합니다. MongoDB에서 직접 제공하며 간결하면서도
훨씬 더 자세한 내용을 다룹니다. 비디오와 퀴즈 질문이 혼합되어 있으며,
이것이 제가 MongoDB에 대한 지식을 얻은 방법입니다.

MongoDB 학습을 위해 다음 비디오 시리즈를 추천합니다.

- [MongoDB 속성 과정 - Traversy
  Media](https://www.youtube.com/watch?v=-56x56UppqQ)
- [초보자를 위한 MongoDB 튜토리얼 -
  Amigoscode](https://www.youtube.com/watch?v=Www6cTUymCY)

제가 이전에 사용했던 언어별 자료:

- [Node.js, Express, & MongoDB로 REST API 구축 - Web Dev
  Simplified](https://www.youtube.com/watch?v=fgTGADljAeg)
- [Python과 MongoDB 속성 과정 - 초보자를 위한 튜토리얼 -
  FreeCodeCamp](https://www.youtube.com/watch?v=E-1xI85Zog8)
- [Java와 MongoDB 사용 방법 - Random
  Coder](https://www.youtube.com/watch?v=reYPUvu2Giw)
- [Rust와 MongoDB 사용 소개 -
  MongoDB](https://www.youtube.com/watch?v=qFlftfLGwPM)

위의 정보 대부분은 [MongoDB
docs](https://www.mongodb.com/)와 교차 참조되었습니다. 각 섹션에 대한 문서는 다음과 같습니다.

- [MongoDB 타입](https://docs.mongodb.com/manual/reference/bson-types/) -
  MongoDB가 기본적으로 지원하는 모든 타입 목록
- [MongoDB 연산자](https://docs.mongodb.com/manual/reference/operator/) -
  MongoDB가 기본적으로 지원하는 연산자 목록
- [MongoDB CRUD](https://docs.mongodb.com/manual/reference/command/nav-crud/) -
  생성, 읽기, 업데이트, 삭제에 대한 명령어

지금까지 MongoDB를 즐겼다면, 중급 기능을 탐색하고 싶을 것입니다.
[집계](https://docs.mongodb.com/manual/reference/command/nav-aggregation/),
[인덱싱](https://docs.mongodb.com/manual/indexes/),
[샤딩](https://docs.mongodb.com/manual/sharding/)을 살펴보는 것이 좋습니다.

- 집계 - 데이터베이스에서 실행될 고급 쿼리를 만드는 데 유용합니다.
- 인덱싱은 캐싱을 허용하여 쿼리 실행 속도를 훨씬 빠르게 합니다.
- 샤딩은 수평적 데이터 확장 및 여러 시스템 간의 분산을 허용합니다.
