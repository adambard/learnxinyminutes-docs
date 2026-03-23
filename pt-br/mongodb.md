---
name: MongoDB 
filename: mongo.js 
contributors:
  - ["Andre Veras Fernandes", "https://andreverasfernandes.github.io/"]
---

MongoDB é um banco de dados NoSQL orientado a documentos para armazenamento
de grandes volumes de dados.

MongoDB usa coleções e documentos para armazenamento. Cada documento consiste
em pares chave-valor usando uma sintaxe semelhante a JSON, parecida com um
dicionário ou objeto JavaScript.

Como MongoDB é um banco NoSQL, ele usa sua própria linguagem de consulta,
Mongo Query Language (MQL), que utiliza JSON para consultas.

## Começando

### Instalação

  MongoDB pode ser instalado localmente seguindo as instruções [aqui.](https://docs.mongodb.com/manual/installation/) Ou você pode criar um cluster gratuito (512 MB) hospedado
[aqui](https://www.mongodb.com/cloud/atlas/register). Links para videos com instruções de setup estarão embaixo.

Este tutorial assume que você possui o MongoDB Shell:
[link](https://www.mongodb.com/try/download/shell). Você também pode baixar a ferramenta gráfica MongoDB Compass no mesmo link.

### Componentes

Após instalar o MongoDB, você verá várias ferramentas de linha de comando.
As três mais importantes são:

- `mongod` - servidor de banco de dados (gerencia dados e consultas)
- `mongos` - roteador de sharding (para dados distribuídos)
- `mongo` - shell (JavaScript) para interagir com o banco

Normalmente iniciamos o mongod e usamos o mongo em outro terminal.

### JSON & BSON

Consultas usam formato semelhante a JSON, mas o MongoDB armazena dados em
BSON (Binary JSON). BSON não é legível para humanos, mas permite mais tipos de dados, como: 

- inteiros
- floats
- datas
- regex
- binário

Lista completa [aqui](https://docs.mongodb.com/manual/reference/bson-types/).


```js
/////////////////////////////////////////////////////////
/////////////////// Começando /////////////////////
/////////////////////////////////////////////////////////

// Iniciar o servidor do banco de dados MongoDB
// NOTA - Você precisará fazer isso em um terminal separado, pois o processo
// irá ocupar o terminal. Você pode usar a opção --fork
mongod // --fork

// Conectando a um servidor Mongo remoto
// mongo "mongodb+srv://host.ip.address/admin" --username seu-usuario

// O Mongo shell possui um interpretador JavaScript completo embutido
3 + 2 // 5

// Mostrar bancos de dados disponíveis
// O MongoDB vem com os seguintes bancos padrão: admin, config, local
show dbs

// Alternar para um novo banco de dados (existente ou a ser criado)
// NOTA: Não existe comando "create" para banco de dados no MongoDB.
// O banco é criado quando dados são inseridos em uma coleção
use employees

// Criar uma nova coleção
// NOTA: Inserir um documento já cria a coleção automaticamente,
// então isso não é obrigatório
db.createCollection('engineers')
db.createCollection('doctors')

// Ver quais coleções existem no banco employees
show collections

/////////////////////////////////////////////////////////
// Operações básicas de CRUD (Create/Read/Update/Delete) ///
/////////////////////////////////////////////////////////

/////////////// Insert (Criar) /////////////////////////

// Inserir um funcionário na base de dados
// Cada inserção retorna acknowledged true ou false
// Todo documento possui um _id único gerado automaticamente
db.engineers.insertOne({ name: "Jane Doe", age: 21, gender: 'Female' })

// Inserir vários funcionários na coleção `engineers`
// Pode inserir como um array de objetos
db.engineers.insert([
  { name: "Foo Bar", age: 25, gender: 'Male' },
  { name: "Baz Qux", age: 27, gender: 'Other' },
])

// MongoDB não exige esquema (schema) fixo para documentos
// Inserir um objeto vazio
db.engineers.insertOne({})

// Campos são opcionais e não precisam seguir o padrão
db.engineers.insertOne({ name: "Seu Nome", gender: "Male" })

// Tipos podem variar e são preservados na inserção
// Isso pode exigir validação adicional em algumas linguagens
db.engineers.insert({ name: ['Foo', 'Bar'], age: 3.14, gender: true })

// Objetos e arrays podem ser aninhados
db.engineers.insertOne({
  name: "Seu Nome",
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

// Podemos sobrescrever o campo _id
db.engineers.insertOne({
  _id: 1,
  name: "Um Engenheiro",
  age: 25,
  gender: "Female",
})

// Cuidado: _id deve ser SEMPRE único na coleção
// Caso contrário, ocorrerá erro de duplicidade
db.engineers.insertOne({
  _id: 1,
  name: "Outro Engenheiro",
  age: 25,
  gender: "Male",
})

// Funciona normalmente em outra coleção
db.doctors.insertOne({
  _id: 1,
  name: "Algum Médico",
  age: 26,
  gender: "Other",
})

/////////////////// Find (Ler) ////////////////////////

// Consultas seguem o formato: db.collection.find(<filtro>)
// Onde <filtro> é um objeto

// Mostrar todos os documentos (limite de 20 por vez)
// Pressione "i" para carregar mais resultados
db.engineers.find({})

// Exibir resultados formatados
db.engineers.find({}).pretty()

// Retorna o primeiro documento que corresponde
db.engineers.findOne({ name: 'Foo Bar' })

// Retorna todos os documentos com o filtro
db.engineers.find({ age: 25 })

// Tipo importa nas consultas
// Não retorna nada pois idade é número, não string
db.engineers.find({ age: '25' })

// Consultas com objetos aninhados
db.engineers.find({
  name: "Seu Nome",
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

///////////////////////// Update ////////////////////////

// Formato: db.collection.update(<filtro>, <update>)
// NOTA: usamos operadores como $set

// Atualizar um único documento
db.engineers.updateOne(
  { name: 'Foo Bar' },
  { $set: { name: 'John Doe', age: 100 }}
)

// Atualizar vários documentos
db.engineers.update(
  { age: 25 },
  { $set: { age: 26 }}
)

// Usar upsert (atualiza ou insere se não existir)
db.engineers.update(
  { name: 'Foo Baz' },
  {
    $set: {
      age: 26,
      gender: 'Other'
    }
  },
  { upsert: true }
)

/////////////////////// Delete /////////////////////////

// Formato: db.collection.delete(<filtro>)

// Deletar um documento
db.engineers.deleteOne({ name: 'Foo Baz' })

// Deletar vários documentos
db.engineers.deleteMany({ gender: 'Male' })

/////////////////////////////////////////////////////////
//////////////////// Operadores //////////////////////////
/////////////////////////////////////////////////////////

//////////////// Operadores de Comparação ///////////////////

db.engineers.find({ age: { $gt: 25 }})
db.engineers.find({ age: { $gte: 25 }})

db.engineers.find({ age: { $lt: 25 }})
db.engineers.find({ age: { $lte: 25 }})

db.engineers.find({ age: { $eq: 25 }})
db.engineers.find({ age: { $ne: 25 }})

db.engineers.find({ age: { $in: [20, 23, 24, 25] }})
db.engineers.find({ age: { $nin: [20, 23, 24, 25] }})

//////////////// Operadores Lógicos ///////////////////

db.engineers.find({
  $and: [
    { gender: 'Female' },
    { age: { $gte: 18 } }
  ]
})

db.engineers.find({
  $or: [
    { gender: 'Female' },
    { age: { $gte: 18 } }
  ]
})

db.engineers.find({
  $not: { gender: 'Female' }
})

db.engineers.find({
  $nor: [
    { gender: 'Female' },
    { age: { $gte: 18 } }
  ]
})

/////////////////////////////////////////////////////////
//////////////// Operações de Banco //////////////////////
/////////////////////////////////////////////////////////

// Deletar (drop) o banco employees
// ISSO APAGA TODOS OS DADOS!
db.dropDatabase()

// Criar novo banco com dados
use example
db.test.insertOne({
  name: "Dados de teste, ignore!",
  type: "Test"
})

// Sair do Mongo shell
exit

/////////////////////////////////////////////////////////
//////////////// Importação / Exportação //////////////////
/////////////////////////////////////////////////////////

// Exportar dados como BSON
mongodump

// Restaurar dados BSON
mongorestore dump

// Exportar como JSON
mongoexport --collection=example

// Importar JSON
mongoimport --collection=example
```

## Leitura adicional

### Videos de instalação

- [Instalar MongoDB - Windows](https://www.youtube.com/watch?v=r6QM1NTzkTI)
- [Instalar MongoDB - Mac (Inglês)](https://www.youtube.com/watch?v=DX15WbKidXY)
- [Instalar MongoDB - Linux
  (Ubuntu)](https://www.youtube.com/watch?v=vTd39hZVz2w) 

### Recursos

Cursos oficiais:
[MongoDB University](https://university.mongodb.com/).

Documentação Oficial:
[MongoDB docs](https://www.mongodb.com/pt-br/docs/).

### Videos em Inglês
Playlist para aprender mongodb:

- [MongoDB Crash Course - Traversy
  Media](https://www.youtube.com/watch?v=-56x56UppqQ)
- [MongoDB Tutorial for Beginners -
  Amigoscode](https://www.youtube.com/watch?v=Www6cTUymCY)

Linguagens especificas:

- [Build A REST API With Node.js, Express, & MongoDB - Web Dev
  Simplified](https://www.youtube.com/watch?v=fgTGADljAeg)
- [MongoDB with Python Crash Course - Tutorial for Beginners -
  FreeCodeCamp](https://www.youtube.com/watch?v=E-1xI85Zog8)
- [How to Use MongoDB with Java - Random
  Coder](https://www.youtube.com/watch?v=reYPUvu2Giw)
- [An Introduction to Using MongoDB with Rust -
  MongoDB](https://www.youtube.com/watch?v=qFlftfLGwPM)


### Proximos passos
Se você tem gostado do MongoDB até agora e quer explorar recursos intermediários, aqui está alguns tópicos: [Agregação](https://docs.mongodb.com/manual/reference/command/nav-aggregation/),
[Indexação](https://docs.mongodb.com/manual/indexes/), e
[sharding](https://docs.mongodb.com/manual/sharding/).

- Agregação (Aggregation) – útil para criar consultas avançadas que são executadas pelo banco de dados
- Indexação (Indexing) – permite o uso de cache, o que possibilita uma execução muito mais rápida das consultas
- Sharding – permite o escalonamento horizontal dos dados e a distribuição entre várias máquinas
