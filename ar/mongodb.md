---
name: MongoDB
filename: mongo.js
contributors:
  - ["Raj Piskala", "https://www.rajpiskala.ml/"]
translators:
    - ["Learn X in Y Minutes (ar)", "https://github.com/adambard/learnxinyminutes-docs"]
---

<p dir="rtl">
مونغو دي بي (MongoDB) قاعدة بيانات وثائقية NoSQL مخصصة لتخزين كميات كبيرة من البيانات.
</p>

<p dir="rtl">
تستخدم مجموعات (collections) ومستندات (documents). كل مستند عبارة عن أزواج مفتاح–قيمة بصياغة شبيهة بـ JSON، مثل القاموس أو كائن جافاسكريبت.
</p>

<p dir="rtl">
كقاعدة NoSQL، لها لغة استعلام خاصة هي لغة استعلام مونغو (MQL) وتستخدم JSON في الاستعلام.
</p>

<h2 dir="rtl">البدء</h2>

<h3 dir="rtl">التثبيت</h3>

<p dir="rtl">
يمكن تثبيت مونغو دي بي محلياً حسب التعليمات في
<a href="https://docs.mongodb.com/manual/installation/">دليل التثبيت</a>
أو إنشاء مجموعة (cluster) مجانية 512 ميجابايت مستضافة عن بُعد من
<a href="https://www.mongodb.com/cloud/atlas/register">Atlas</a>.
روابط لفيديوهات الإعداد في أسفل الصفحة.
</p>

<p dir="rtl">
يفترض هذا الدليل أن لديك MongoDB Shell من
<a href="https://www.mongodb.com/try/download/shell">صفحة التنزيل</a>.
يمكنك أيضاً تنزيل أداة MongoDB Compass الرسومية من نفس الرابط.
</p>

<h3 dir="rtl">المكوّنات</h3>

<p dir="rtl">
بعد التثبيت ستجد عدة أدوات سطر أوامر. أهم ثلاثة:
</p>

<ul dir="rtl">
<li><code>mongod</code> — خادم قاعدة البيانات الذي يدير البيانات والاستعلامات.</li>
<li><code>mongos</code> — موجه التجزئة (sharding) عند توزيع البيانات على عدة أجهزة.</li>
<li><code>mongo</code> — الصدفة التفاعلية (تستخدم جافاسكريبت) لضبط قاعدة البيانات.</li>
</ul>

<p dir="rtl">
عادة نشغّل <code>mongod</code> ثم نستخدم طرفية أخرى مع <code>mongo</code> للوصول إلى المجموعات وتعديلها.
</p>

<h3 dir="rtl">JSON و BSON</h3>

<p dir="rtl">
الاستعلامات بصياغة شبيهة بـ JSON*، لكن التخزين الداخلي بصيغة BSON (JSON ثنائي). BSON ليس مقروءاً كالنص لأنه ترميز ثنائي، لكنه يتيح أنواعاً إضافية مثل الأعداد الصحيحة والعشرية والتعبيرات النمطية والتواريخ والبيانات الثنائية الخام.
</p>

<p dir="rtl">
<a href="https://docs.mongodb.com/manual/reference/bson-types/">قائمة أنواع BSON</a>
الكاملة في التوثيق.
</p>

<ul dir="rtl">
<li>بـ «شبيه JSON» نعني JSON مع هذه الأنواع الموسّعة؛ يمكنك الاستعلام بتاريخ أو تعبير نمطي واستلام بيانات بهذه الأنواع.</li>
</ul>

```js
/////////////////////////////////////////////////////////
/////////////////// البدء //////////////////////////////
/////////////////////////////////////////////////////////

// تشغيل خادم قاعدة بيانات مونغو
// ملاحظة: نفّذ في طرفية منفصلة لأن العملية تشغل الطرفية؛ يمكن --fork
mongod // --fork

// الاتصال بخادم بعيد
// mongo "mongodb+srv://host.ip.address/admin" --username your-username

// الصدفة تتضمن مفسّر جافاسكريبت كامل
3 + 2 // 5

// عرض قواعد البيانات المتاحة
// مدمج: admin و config و local
show dbs

// التبديل لقاعدة جديدة أو موجودة مسبقاً
// ملاحظة: لا أمر "create" للقاعدة؛ تُنشأ عند إدراج بيانات في مجموعة
use employees

// إنشاء مجموعة جديدة
// ملاحظة: إدراج مستند يُنشئ المجموعة ضمنياً؛ هذا غير إلزامي
db.createCollection('engineers')
db.createCollection('doctors')

// المجموعات ضمن employees الحالية
show collections

/////////////////////////////////////////////////////////
// عمليات CRUD أساسية ///////////////////////////////////
/////////////////////////////////////////////////////////

/////////////// إدراج (إنشاء) ////////////////////////////

// إدراج موظف واحد
// كل إدراج يعيد تأكيداً true/false
// لكل مستند _id فريد يُنشأ تلقائياً
db.engineers.insertOne({ name: "Jane Doe", age: 21, gender: 'Female' })

// إدراج قائمة في مجموعة engineers
// يمكن تمرير مصفوفة كائنات
db.engineers.insert([
  { name: "Foo Bar", age: 25, gender: 'Male' },
  { name: "Baz Qux", age: 27, gender: 'Other' },
])

// مونغو لا يفرض مخططاً ثابتاً
// إدراج كائن فارغ
db.engineers.insertOne({})

// الحقول اختيارية ولا يجب أن تطابق بقية المستندات
db.engineers.insertOne({ name: "Your Name", gender: "Male" })

// الأنواع قد تختلف وتُحفظ كما هي
// قد تحتاج لتحقق إضافي في بعض لغات البرمجة
db.engineers.insert({ name: ['Foo', 'Bar'], age: 3.14, gender: true })

// كائنات أو مصفوفات متداخلة داخل المستند
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

// يمكن تجاوز حقل _id
// يعمل طالما الفريدة محفوظة
db.engineers.insertOne({
  _id: 1,
  name: "An Engineer",
  age: 25,
  gender: "Female",
})

// احذر: _id يجب أن يكون فريداً في المجموعة وإلا
// يفشل الإدراج بـ WriteError (قيمة مكررة)
db.engineers.insertOne({
  _id: 1,
  name: "Another Engineer",
  age: 25,
  gender: "Male",
})

// يعمل لأن المجموعة مختلفة
db.doctors.insertOne({
  _id: 1,
  name: "Some Doctor",
  age: 26,
  gender: "Other",
})

/////////////////// بحث (قراءة) ////////////////////////
// الاستعلام: db.اسم_المجموعة.find(<مرشح>)
// <مرشح> كائن

// كل المستندات حتى 20 في المرة؛ اضغط i للدفعة التالية
db.engineers.find({})

// pretty() لتنسيق المخرجات
db.engineers.find({}).pretty()

// الاستعلام بكائن جافاسكريبت يطابق أزواج مفتاح–قيمة
// أول مستند مطابق
// ملاحظة: ترتيب الإدراج غير مضمون في العرض
db.engineers.findOne({ name: 'Foo Bar' })

// كل المستندات المطابقة كمؤشر (يمكن تحويله لمصفوفة)
db.engineers.find({ age: 25 })

// النوع مهم في الاستعلام
// لا نتائج لأن الأعمار أعلاه أعداد صحيحة وليس النص '25'
db.engineers.find({ age: '25' })

// find() يدعم التداخل مثل الإدراج
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

///////////////////////// تحديث ////////////////////////
// db.collection.update(<مرشح>, <تحديث>)
// ملاحظة: هنا <تحديث> يستخدم $set
// معاملات أخرى لاحقاً في الدليل

// تحديث مستند واحد
db.engineers.updateOne({ name: 'Foo Bar' }, { $set: { name: 'John Doe', age: 100 }})

// تحديث عدة مستندات
db.engineers.update({ age: 25 }, { $set: { age: 26 }})

// upsert: true — أدرج إن لم يوجد، أو حدّث إن وُجد
// يعيد أعداد المطابقة والإدراج والتعديل
db.engineers.update({ name: 'Foo Baz' },
  { $set:
    {
      age: 26,
      gender: 'Other'
    }
  },
  { upsert: true }
)

/////////////////////// حذف ////////////////////////////
// db.collection.delete(<مرشح>)

// حذف أول مستند مطابق؛ يعيد deletedCount
db.engineers.deleteOne({ name: 'Foo Baz' })

// حذف كل المطابقين
db.engineers.deleteMany({ gender: 'Male' })

// ملاحظة: removeOne/removeMany قديمة في سائق Node وبقيمة إرجاع مختلفة
// لم تُذكر هنا عمداً

/////////////////////////////////////////////////////////
//////////////////// المعاملات /////////////////////////
/////////////////////////////////////////////////////////

// المعاملات تبدأ بـ $؛ هنا مقارنة ومنطق فقط

//////////////// معاملات المقارنة //////////////////////

// أكبر / أكبر أو يساوي
db.engineers.find({ age: { $gt: 25 }})
db.engineers.find({ age: { $gte: 25 }})

// أصغر / أصغر أو يساوي
db.engineers.find({ age: { $lt: 25 }})
db.engineers.find({ age: { $lte: 25 }})

// يساوي / لا يساوي
// $eq يُضاف ضمنياً في أغلب الاستعلامات
db.engineers.find({ age: { $eq: 25 }})
db.engineers.find({ age: { $ne: 25 }})

// ضمن المصفوفة / ليس ضمنها
db.engineers.find({ age: { $in: [ 20, 23, 24, 25 ]}})
db.engineers.find({ age: { $nin: [ 20, 23, 24, 25 ]}})

//////////////// المعاملات المنطقية /////////////////////

// دمج شرطين بـ $and (غالباً ضمني في مونغو)
db.engineers.find({ $and: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// أحد الشرطين بـ $or
db.engineers.find({ $or: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// نفي بـ $not
db.engineers.find({ $not: {
  gender: 'Female'
}})

// لا يطابق أي شرط من $nor
db.engineers.find({ $nor [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

/////////////////////////////////////////////////////////
//////////////// عمليات قاعدة البيانات /////////////////
/////////////////////////////////////////////////////////

// حذف قاعدة employees بالكامل
// سيحذف كل المستندات!
db.dropDatabase()

// قاعدة جديدة مع بيانات تجريبية
use example
db.test.insertOne({ name: "Testing data, please ignore!", type: "Test" })

// الخروج من الصدفة
exit

// تصدير/استيراد BSON:

// mongodump يصدّر BSON لكل القواعد
// المخرجات في MongoDB Database Tools/bin/dump
// إن لم يُعثر على الأمر، انتقل لمجلد الأدوات وشغّل mongodump

// mongorestore يستعيد من BSON
mongorestore dump

// تصدير/استيراد JSON:
// mongoexport — تصدير JSON
mongoexport --collection=example

// mongoimport — استيراد JSON
mongoimport  --collection=example
```

<h2 dir="rtl">قراءة إضافية</h2>

<h3 dir="rtl">فيديوهات الإعداد</h3>

<ul dir="rtl">
<li><a href="https://www.youtube.com/watch?v=85A6m1soKww">تثبيت MongoDB — Windows 10</a></li>
<li><a href="https://www.youtube.com/watch?v=DX15WbKidXY">تثبيت MongoDB — Mac</a></li>
<li><a href="https://www.youtube.com/watch?v=wD_2pojFWoE">تثبيت MongoDB — Linux (Ubuntu)</a></li>
</ul>

<h3 dir="rtl">التحقق من المدخلات</h3>

<p dir="rtl">
إن كنت تحتاج التحقق من المدخلات أو هيكل البيانات، راجع أطر ORM التالية:
</p>

<ul dir="rtl">
<li><a href="https://mongoosejs.com/docs/">Mongoose (Node.js)</a> — مخططات وأنواع وقيم مطلوبة وحدود دنيا/قصوى.</li>
<li><a href="http://mongoengine.org/">MongoEngine (Python)</a> — مشابه لـ Mongoose.</li>
<li><a href="https://github.com/namlook/mongokit">MongoKit (Python)</a> — بديل عملي.</li>
</ul>

<p dir="rtl">
في اللغات ذات الأنواع القوية الثابتة (مثل Java وC++ وRust) غالباً لا حاجة لمكتبة للتحقق لأن الأنواع تُعرَّف عند الترجمة.
</p>

<h3 dir="rtl">موارد</h3>

<p dir="rtl">
أنصح بدورات
<a href="https://university.mongodb.com/">MongoDB University</a>
من الشركة نفسها: فيديوهات واختصار مفيد.
</p>

<p dir="rtl">سلاسل فيديو مقترحة:</p>

<ul dir="rtl">
<li><a href="https://www.youtube.com/watch?v=-56x56UppqQ">MongoDB Crash Course — Traversy Media</a></li>
<li><a href="https://www.youtube.com/watch?v=Www6cTUymCY">MongoDB Tutorial for Beginners — Amigoscode</a></li>
</ul>

<p dir="rtl">أمثلة حسب اللغة:</p>

<ul dir="rtl">
<li><a href="https://www.youtube.com/watch?v=fgTGADljAeg">REST API مع Node وExpress وMongoDB</a></li>
<li><a href="https://www.youtube.com/watch?v=E-1xI85Zog8">MongoDB مع Python — FreeCodeCamp</a></li>
<li><a href="https://www.youtube.com/watch?v=reYPUvu2Giw">MongoDB مع Java</a></li>
<li><a href="https://www.youtube.com/watch?v=qFlftfLGwPM">MongoDB مع Rust</a></li>
</ul>

<p dir="rtl">
راجع أيضاً
<a href="https://www.mongodb.com/">توثيق MongoDB</a>:
<a href="https://docs.mongodb.com/manual/reference/bson-types/">الأنواع</a>،
<a href="https://docs.mongodb.com/manual/reference/operator/">المعاملات</a>،
<a href="https://docs.mongodb.com/manual/reference/command/nav-crud/">CRUD</a>.
للمستوى المتوسط:
<a href="https://docs.mongodb.com/manual/reference/command/nav-aggregation/">التجميع</a>،
<a href="https://docs.mongodb.com/manual/indexes/">الفهرسة</a>،
<a href="https://docs.mongodb.com/manual/sharding/">التجزئة</a>.
</p>

<ul dir="rtl">
<li>التجميع — استعلامات متقدمة تنفَّذ في الخادم.</li>
<li>الفهرسة — تسريع الاستعلامات.</li>
<li>التجزئة — توسيع أفقي للبيانات على عدة أجهزة.</li>
</ul>
