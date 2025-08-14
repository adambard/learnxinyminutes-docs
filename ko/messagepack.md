---
category: framework
name: MessagePack
filename: learnmessagepack.mpac
contributors:
  - ["Gabriel Chuan", "https://github.com/gczh"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

MessagePack은 효율적인 바이너리 직렬화 형식입니다. JSON처럼 여러 언어 간에 데이터를 교환할 수 있습니다. 다른 형식에 비해 더 빠르고 작다는 장점이 있습니다.

MessagePack에서는 작은 정수가 단일 바이트로 인코딩되고, 일반적인 짧은 문자열은 문자열 자체 외에 추가로 1바이트만 필요합니다. 이로 인해 MessagePack은 유선으로 효율적인 전송에 유용합니다.

```
# 0. 구조 이해하기 ====

JSON, 40 Bytes UTF-8

----------------------------------------------
| {"name":"John Doe","age":12}		         |
----------------------------------------------
|  {"         | 7B 22                        |
|    name     | 6E 61 6D 65                  |
|  ":"        | 22 3A 22                     |
|    John Doe | 4A 6F 68 6E 20 44 6F 65      |
|  ","        | 22 2C 22                     |
|    age      | 61 67 65                     |
|  ":         | 22 3A 20                     |
|    12       | 31 32                        |
|  }          | 7D                           |
----------------------------------------------


MessagePack, 27 Bytes UTF-8

----------------------------------------------
| ‚¤name¨John Doe£age.12                     |
----------------------------------------------
|  ‚¤         | 82 84                        |
|    name     | 6E 61 6D 65                  |
|  ¨          | A8                           |
|    John Doe | 4A 6F 68 6E 20 44 6F 65      |
|  £          | A3                           |
|    age      | 61 67 65                     |
|  .          | 0C                           |
|    12       | 31 32                        |
----------------------------------------------

# 1. JAVA ====

""" Maven으로 설치하기
"""

<dependencies>
  ...
  <dependency>
    <groupId>org.msgpack</groupId>
    <artifactId>msgpack</artifactId>
    <version>${msgpack.version}</version>
  </dependency>
  ...
</dependencies>


""" 간단한 직렬화/역직렬화
"""

// 직렬화 객체 생성.
List<String> src = new ArrayList<String>();
src.add("msgpack");
src.add("kumofs");

MessagePack msgpack = new MessagePack();
// 직렬화
byte[] raw = msgpack.write(src);

// 템플릿을 사용하여 직접 역직렬화
List<String> dst1 = msgpack.read(raw, Templates.tList(Templates.TString));
System.out.println(dst1.get(0));
System.out.println(dst1.get(1));

// 또는, 값으로 역직렬화한 다음 타입 변환.
Value dynamic = msgpack.read(raw);
List<String> dst2 = new Converter(dynamic)
    .read(Templates.tList(Templates.TString));
System.out.println(dst2.get(0));
System.out.println(dst2.get(1));


# 2. RUBY ====

""" Gem 설치하기
"""

gem install msgpack

""" 스트리밍 API
"""

# 2개 요소 배열 [e1, e2] 직렬화
pk = MessagePack::Packer.new(io)
pk.write_array_header(2).write(e1).write(e2).flush

# IO에서 객체 역직렬화
u = MessagePack::Unpacker.new(io)
u.each { |obj| ... }

# 이벤트 기반 역직렬화
def on_read(data)
  @u ||= MessagePack::Unpacker.new
  @u.feed_each(data) { |obj| ... }
end

# 3. NODE.JS ====

""" NPM으로 설치하기
"""

npm install msgpack5 --save

""" Node에서 사용하기
"""

var msgpack = require('msgpack5')() // 우리 확장 기능의 네임스페이스
  , a       = new MyType(2, 'a')
  , encode  = msgpack.encode
  , decode  = msgpack.decode

msgpack.register(0x42, MyType, mytipeEncode, mytipeDecode)

console.log(encode({ 'hello': 'world' }).toString('hex'))
// 81a568656c6c6fa5776f726c64
console.log(decode(encode({ 'hello': 'world' })))
// { hello: 'world' }
console.log(encode(a).toString('hex'))
// d5426161
console.log(decode(encode(a)) instanceof MyType)
// true
console.log(decode(encode(a)))
// { value: 'a', size: 2 }

function MyType(size, value) {
  this.value = value
  this.size  = size
}

function mytipeEncode(obj) {
  var buf = new Buffer(obj.size)
  buf.fill(obj.value)
  return buf
}

function mytipeDecode(data) {
  var result = new MyType(data.length, data.toString('utf8', 0, 1))
    , i

  for (i = 0; i < data.length; i++) {
    if (data.readUInt8(0) != data.readUInt8(i)) {
      throw new Error('should all be the same')
    }
  }

  return result
}
```


# 참조

- [MessagePack](http://msgpack.org/index.html)
- [MsgPack vs. JSON: Cut your client-server exchange traffic by 50% with one line of code](http://indiegamr.com/cut-your-data-exchange-traffic-by-up-to-50-with-one-line-of-code-msgpack-vs-json/)
