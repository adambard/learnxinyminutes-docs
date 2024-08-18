---
language: MongoDB 
filename: mongo.js 
contributors:
  - ["Raj Piskala", "https://www.rajpiskala.ml/"]
translator:
  - ["duzhuoshanwai", "https://blog.saint1337.top"]
---

MongoDB 是一种用于高容量数据存储的 NoSQL 文档数据库。

MongoDB 使用集合（collections）和文档（documents）进行存储。每个文档由键值对（key-value pairs）组成，采用类似 JSON 的语法，类似于字典或 JavaScript 对象。

同样，由于 MongoDB 是 NoSQL 数据库，它使用自己的查询语言 Mongo
Query Language（MQL），该语言使用 JSON 进行查询。

## 入门

### 安装

MongoDB 的本地安装说明可以参考
[官方的安装手册](https://docs.mongodb.com/manual/installation/) 或者你可以注册[MongoDB Atlas](https://www.mongodb.com/cloud/atlas/register)白嫖到一个免费的 512MB 集群。文章底部还有关于设置的教程视频链接.

本教程假设你已经从[https://www.mongodb.com/try/download/shell](https://www.mongodb.com/try/download/shell)下载了 MongoDB Shell。你也可以从同一链接下方下载带有图形界面的工具 MongoDB Compass。

### 组件

安装 MongoDB 后，你会发现其中包含多个命令行工具。

其中最重要的三个是：

- `mongod` - 数据库服务器，负责管理数据和处理查询信息，可以以单机模式运行，也可以作为集群的一部分运行
- `mongos` - 分片集群中的路由器，充当客户端和分片集群之间的接口，负责将客户端请求路由到正确的分片节点，并聚合来自多个分片节点的结果
- `mongo` - 数据库 shell（基于 JavaScript）用来配置和管理 MongoDB 数据库。

通常，我们先启动 `mongod` 进程，然后在另一个终端中使用 `mongo` 来访问和修改我们的集合。

### JSON & BSON

虽然 MongoDB 中的查询使用 类JSON\* 的格式，但 MongoDB 在内部将其文档存储为 BSON（Binary JSON）。

因为 BSON 是二进制编码，所以它不像 JSON 一样可以阅读。
但这也使得用户能够访问比普通 JSON 更多的数据类型，例如整数或浮点数。也支持很多其他类型，比如正则表达式、日期或二进制数据。
[这里](https://docs.mongodb.com/manual/reference/bson-types/) 列出了MongoDB支持的所有数据类型。

- 我们这里使用 类JSON（JSON-like）而不是直接使用 JSON 是因为它可以包含扩展的类型。 例如，你可以在 MongoDB 中直接使用正则表达式或时间戳进行查询，并且你还可以接收包含这些类型的数据。

```js
/////////////////////////////////////////////////////////
/////////////////// Getting Started /////////////////////
/////////////////////////////////////////////////////////

// 启动 MongoDB 服务器
// 注意 - 你需要在一个单独的终端中执行此操作，因为该过程会占用整个终端。
// 使用 --fork 选项会以守护进程方式启动
// 使用 --logpath /var/log/mongodb/mongod.log 会将日志输出到文件 /var/log/mongodb/mongod.log
mongod // --fork --logpath /var/log/mongodb/mongod.log

// 连接到一个远程 MongoDB 服务器 这里的admin是数据库名称
// mongo "mongodb+srv://host.ip.address/admin" --username your-username

// Mongoshell 内置了一个完善的 JavaScript 解释器。
3 + 2 // 5

// 显示可用数据库
// MongoDB 内置了以下数据库：admin、config、local
show dbs

// 切换到一个新数据库（可以是已经存在的或你想要创建的）
// 注意：在 MongoDB 中没有专门的 "create" 命令来创建数据库。
// 数据库会在向集合中插入数据时自动创建
use employees

// 创建新集合（Collections）
// 注意：插入文档时会自动创建集合，因此不需要提前创建
db.createCollection('engineers')
db.createCollection('doctors')

// 查看employees数据库中的所有集合
show collections

/////////////////////////////////////////////////////////
// Basic Create/Read/Update/Delete (CRUD) Operations: ///
/////////////////////////////////////////////////////////

/////////////// Insert (Create) /////////////////////////

// 向数据库中添加新的 employee 记录
// 每次插入数据都会返回 true 或 false
// 每个文档都会自动分配一个唯一的 `_id` 值
db.engineers.insertOne({ name: "Jane Doe", age: 21, gender: 'Female' })

// 向 `engineers` 集合中插入一组员工记录
// 可以作为包含对象的数组进行插入
db.engineers.insert([
  { name: "Foo Bar", age: 25, gender: 'Male' },
  { name: "Baz Qux", age: 27, gender: 'Other' },
])

// MongoDB 对插入对象的模式或结构没有强制要求
// 例如，向 `engineers` 集合中插入一个空对象
db.engineers.insertOne({})

/// 字段是可选的，并且不必与其他文档匹配
// 比如下面插入的文档中 没有 age 字段
db.engineers.insertOne({ name: "Your Name", gender: "Male" })

// 数据类型可以不同，并且在插入时会被保留
// 在某些编程语言中，这可能需要额外的验证以防止问题
db.engineers.insert({ name: ['Foo', 'Bar'], age: 3.14, gender: true })

// 对象或数组可以嵌套在文档内
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

// 我们可以自定义覆盖 `_id` 字段的值
// 比如下面的操作
db.engineers.insertOne({
  _id: 1,
  name: "An Engineer",
  age: 25,
  gender: "Female",
})

// 但是需要注意，`_id` 的值在集合中必须保持唯一，否则插入操作会失败
// 如果 `_id` 是重复值，插入会失败并返回 WriteError 错误，并表明 `_id` 出现重复
db.engineers.insertOne({
  _id: 1,
  name: "Another Engineer",
  age: 25,
  gender: "Male",
})

// `_id` 只在需要在当前的集合中保持唯一
// 例如，这里我们将数据插入 doctors 集合
db.doctors.insertOne({
  _id: 1,
  name: "Some Doctor",
  age: 26,
  gender: "Other",
})

/////////////////// Find (Read) ////////////////////////
// 查询的形式是 db.collectionName.find(<filter>)
// 其中 <filter> 是一个对象

// 显示我们数据库中目前的所有内容，每次最多显示 20 个文档
// 按 i 键可以翻页到下面 20 个文档
db.engineers.find({})

// 我们可以对 find() 查询的结果进行格式化打印
db.engineers.find({}).pretty()

// MongoDB 查询使用 JavaScript 对象并搜索具有匹配键值对的文档
// 返回第一个匹配查询条件的文档
// 注意：插入的顺序在数据库中不会被保留，因此输出可能会有所不同
db.engineers.findOne({ name: 'Foo Bar' })

/// 返回所有具有匹配键值对（key-value）属性的文档，并以游标（Cursor）形式返回
// 游标可以使用 .toArray() 转换为数组
db.engineers.find({ age: 25 })//.toArray()

// 查询时类型非常重要
// 因为上面的所有年龄都是整数类型，所以下面使用字符型的查询不会返回任何结果
db.engineers.find({ age: '25' })

// find() 支持嵌套对象和数组，就像创建时一样
db.engineers.find({
  name: "Your Name",
  gender: "Female",
  skilledIn: [
    "MongoDB",
    "NoSQL"
  ],
  "date-of-birth": {
    "date": new Date("1993-07-20T09:44:18.674Z"), // 使用 ISO 日期格式
    "age": 26
  }
})

///////////////////////// Update ////////////////////////
// 修改的形式是 db.collectionName.update(<filter>, <update>)
// 注意：<update> 总是使用 $set 操作符。
// 后面会介绍其他一些操作符。

// 修改单个文档
db.engineers.updateOne({ name: 'Foo Bar' }, { $set: { name: 'John Doe', age: 100 }})

// 修改多个文档
db.engineers.update({ age: 25 }, { $set: { age: 26 }})

// We can use { upsert: true } if we would like it to insert if the document doesn't already exist,
// or to update if it does
// Returns matched, upserted, modified count

// 使用 { upsert: true } 选项
// 如果文档存在则会被修改，如果不存在会自动创建
// 返回匹配的、插入的、修改的文档数量以及新创建文档的 `_id`
db.engineers.update({ name: 'Foo Baz' },
  { $set:
    {
      age: 26,
      gender: 'Other'
    }
  },
  { upsert: true } // 如果文档不存在，则插入新文档；如果存在，则更新现有文档
)

/////////////////////// Delete /////////////////////////
// 删除文档的形式是 db.collectionName.delete(<filter>)

// 删除第一个匹配查询条件的文档，返回 deletedCount
db.engineers.deleteOne({ name: 'Foo Baz' })

// 一次删除匹配的多个文档
db.engineers.deleteMany({ gender: 'Male' })

// 注意：有两个方法 db.collection.removeOne(<filter>) 和 
// db.collection.removeMany(<filter>) 也可以用于删除文档，但它们的返回值略有不同。
// 建议只使用 deleteOne() 和 deleteMany() 保证代码的兼容性。
// 这里没有包括这些方法，因为它们在 NodeJS 的数据库驱动中已被弃用。

/////////////////////////////////////////////////////////
//////////////////// Operators //////////////////////////
/////////////////////////////////////////////////////////

// Operators in MongoDB have a $ prefix. For this tutorial, we are only looking 
// at comparison and logical operators, but there are many other types of
// operators

// MongoDB 中的运算符（操作符）都有一个 $ 前缀。在本教程中，我们只讨论比较和逻辑运算符，
// 但实际上还有许多其他类型的运算符。

//////////////// Comparison Operators ///////////////////

//查找所有大于或大于等于某个条件的项。
db.engineers.find({ age: { $gt: 25 }}) // 查找 `age` 大于 25 的文档
db.engineers.find({ age: { $gte: 25 }}) // 查找 `age` 大于或者等于 25 的文档

//查找所有小于或小于等于某个条件的项。
db.engineers.find({ age: { $lt: 25 }}) // 查找 `age` 小于 25 的文档
db.engineers.find({ age: { $lte: 25 }}) // 查找 `age` 小于或者等于 25 的文档

// 查找等于或者不等于某个条件的项。
// 注意：在你没有指定查询条件的时候自动使用'$eq'。
db.engineers.find({ age: { $eq: 25 }}) // 查找 `age` 不等于 25 的文档
db.engineers.find({ age: { $ne: 25 }}) // 查找 `age` 不等于 25 的文档

// 查找所有匹配数组中任意一个元素的文档，或不在数组中的文档
db.engineers.find({ age: { $in: [ 20, 23, 24, 25 ]}})
db.engineers.find({ age: { $nin: [ 20, 23, 24, 25 ]}})

//////////////// Logical Operators ///////////////////

// 将两个查询条件连接在一起
// 注意：MongoDB 在没有指定连接条件时会自动使用 '$and'
db.engineers.find({ $and: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// 匹配任意一个查询条件
// 例如这里会返回符合 gender 为 'Female' 的文档，或符合 age 大于或等于 18 的文档
db.engineers.find({ $or: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// 取与查询结果相反的部分
// 这里返回 gender 不为 'Female' 的文档
db.engineers.find({ $not: {
  gender: 'Female'
}})

// 必须不匹配任意一个查询条件
// 这里返回既不满足 gender 为 'Female'，又不满足 age 大于或等于 18 的文档
db.engineers.find({ $nor: [
  { gender: 'Female' },
  { age: { $gte: 18 } }
]})

/////////////////////////////////////////////////////////
//////////////// Database Operations: ///////////////////
/////////////////////////////////////////////////////////

// 删除 (drop) `employees` 数据库
// 这将删除数据库中的所有文档！
db.dropDatabase()

// 创建一个带有一些数据的新数据库
use example // 使用 example 数据库
db.test.insertOne({ name: "Testing data, please ignore!", type: "Test" }) // 向test集合中添加一条文档

// 退出 Mongo shell
exit

// 导入/导出 数据库（BSON格式）：

// 使用 `mongodump` 导出所有数据库的数据为 BSON 格式
// 导出的数据保存在 "MongoDB Database Tools/bin/dump" 目录下
// MongoDB Database Tools 的具体位置以你的操作系统和安装方式为准
// 注意：如果找不到该命令，请导航到 "MongoDB Database Tools/bin" 并从那里使用可执行文件 mongodump（确保你有执行权限）

// 使用 `mongorestore` 从 BSON 数据中恢复数据库
mongorestore dump 
// 默认情况会在 dump 文件夹查找文件，也可以自定义文件路径
mongorestore /path/to/your/dump

// 导入/导出 数据库（JSON 格式）：
// 使用 `mongoexport` 将所有数据库的数据导出为 JSON 格式
mongoexport --collection=example

// 使用 `mongoimport` 将 JSON 格式的数据导入到数据库中
mongoimport  --collection=example
```

## 进阶阅读

### 安装以及设置视频

- [Install MongoDB - Windows 10](https://www.youtube.com/watch?v=85A6m1soKww)
- [Install MongoDB - Mac](https://www.youtube.com/watch?v=DX15WbKidXY)
- [Install MongoDB - Linux (Ubuntu)](https://www.youtube.com/watch?v=wD_2pojFWoE)

### 输入验证

根据上面的示例，如果输入验证或结构是一个问题，我建议查看以下 ORMs（对象关系映射，允许用户使用面向对象方式来操作数据库）：

- [Mongoose (Node.js)](https://mongoosejs.com/docs/) - 通过支持类型、必需值、最小值和最大值的模式进行输入验证。
- [MongoEngine (Python)](http://mongoengine.org/) - 类似于 Mongoose，但我发现它在某些方面有些限制
- [MongoKit (Python)](https://github.com/namlook/mongokit) - 另一个比 MongoEngine 更容易使用的替代方案

对于静态强类型语言（如 Java、C++、Rust），输入验证通常不需要库，因为它们在编译时定义了类型和结构。

### 资源

如果你有时间，我强烈推荐 [MongoDB University](https://university.mongodb.com/) 的课程。它们由 MongoDB 自己制作，更详细，同时仍然简明扼要。这些课程结合了视频和测试问题，这是我获取 MongoDB 知识的方式。

我推荐以下视频系列来学习 MongoDB：

- [MongoDB Crash Course - Traversy
  Media](https://www.youtube.com/watch?v=-56x56UppqQ)
- [MongoDB Tutorial for Beginners -
  Amigoscode](https://www.youtube.com/watch?v=Www6cTUymCY)

我之前使用过的与特定语言相关的资源：

- [Build A REST API With Node.js, Express, & MongoDB - Web Dev
  Simplified](https://www.youtube.com/watch?v=fgTGADljAeg)
- [MongoDB with Python Crash Course - Tutorial for Beginners -
  FreeCodeCamp](https://www.youtube.com/watch?v=E-1xI85Zog8)
- [How to Use MongoDB with Java - Random
  Coder](https://www.youtube.com/watch?v=reYPUvu2Giw)
- [An Introduction to Using MongoDB with Rust -
  MongoDB](https://www.youtube.com/watch?v=qFlftfLGwPM)

以上大部分信息与 [MongoDB 文档](https://www.mongodb.com/) 交叉引用。以下是每个部分的文档链接：

- [MongoDB Types](https://docs.mongodb.com/manual/reference/bson-types/) - 列出 MongoDB 本地支持的所有类型
- [MongoDB Operators](https://docs.mongodb.com/manual/reference/operator/) - 列出 MongoDB 本地支持的运算符
