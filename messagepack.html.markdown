---
language: messagepack
filename: learnmessagepack.mpac
contributors:
  - ["Gabriel Chuan", "https://github.com/gczh"]
---

MessagePack is an efficient binary serialization format. It lets you exchange data among multiple languages like JSON. The benefits over other formats is that it's faster and smaller. 

In MessagePack, small integers are encoded into a single byte, and typical short strings require only one extra byte in addition to the strings themselves. This makes MessagePack useful for efficient transmission over wire.

```

# 0. Understanding The Structure ====

JSON, 40 Bytes UTF-8
	
----------------------------------------------
| {“name“:”John Doe“,”age“:12}		         |
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

""" Installing with Maven
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


""" Simple Serialization/Deserialization
"""

// Create serialize objects.
List<String> src = new ArrayList<String>();
src.add("msgpack");
src.add("kumofs");

MessagePack msgpack = new MessagePack();
// Serialize
byte[] raw = msgpack.write(src);

// Deserialize directly using a template
List<String> dst1 = msgpack.read(raw, Templates.tList(Templates.TString));
System.out.println(dst1.get(0));
System.out.println(dst1.get(1));

// Or, Deserialze to Value then convert type.
Value dynamic = msgpack.read(raw);
List<String> dst2 = new Converter(dynamic)
    .read(Templates.tList(Templates.TString));
System.out.println(dst2.get(0));
System.out.println(dst2.get(1));


# 2. RUBY ====

""" Installing the Gem
"""

gem install msgpack

""" Streaming API
"""

# serialize a 2-element array [e1, e2]
pk = MessagePack::Packer.new(io)
pk.write_array_header(2).write(e1).write(e2).flush

# deserialize objects from an IO
u = MessagePack::Unpacker.new(io)
u.each { |obj| ... }

# event-driven deserialization
def on_read(data)
  @u ||= MessagePack::Unpacker.new
  @u.feed_each(data) { |obj| ... }
end

# 3. NODE.JS ====

""" Installing with NPM
"""

npm install msgpack5 --save

""" Using in Node
"""

var msgpack = require('msgpack5')() // namespace our extensions
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


# References

- [MessagePack](http://msgpack.org/index.html)
- [MsgPack vs. JSON: Cut your client-server exchange traffic by 50% with one line of code](http://indiegamr.com/cut-your-data-exchange-traffic-by-up-to-50-with-one-line-of-code-msgpack-vs-json/)