---
category: tool
tool: HappyX
contributors:
    - ["Ethosa", "https://github.com/Ethosa"]
translators:
    - ["Ethosa", "https://github.com/Ethosa"]
filename: learnHappyX-ru.html
lang: ru-ru
---

## HappyX Tutorial.

HappyX это макро-ориентированный фулл-стек веб фреймворк, написанный на Nim.

Разработка фреймворка стартовала в апреле 2023, а первая версия (1.0.0) была выпущена 1-го июня 2023-го года.

### Установка Фреймворка

Чтобы использовать HappyX у Вас должны присутствовать Nim и Nimble.

Установка последней версии:
```bash
nimble install happyx@#head
```

### Привет, Мир!

Для проектов серверной стороны простейший код выглядит следующим образом:
```nim
import happyx

serve("127.0.0.1", 5000):
  get "/":
    "Привет, мир!"
```

Для одностраничных приложений код выглядит немного по-другому:
```nim
import happyx

appRoutes("app"):
  "/":
    "Привет, мир!"
```


### Компоненты

В Одностраничных приложениях Вы можете использовать компоненты.

Объявление
```nim
component MyComponent:  # Здесь мы объявляем компонент под названием "MyComponent"
  componentRequiredField: int  # Здесь мы объявляем обязательный аргумент
  componentOptionalField: int = 10  # Здесь мы объявляем необязательный аргумент

  `template`:  # HTML начинается отсюда
    tDiv(class = "..."):  # <div class="..."></div>
      "Привет, мир!"  # Сырой текст
  
  # Вы также можете добавить стили для компонента.
  `style`: """
    .someClasss {
      ...
    }
  
  `script`:
    # А здесь Вы можете использовать реальный Nim код
    var x = 1  # Объявление изменяемой переменной
    var y = 2
    echo x + y
```

Usage:
```nim
var myHtml = buildHtml:  # Объявление HTML тега
  tDiv(class = "..."):
    ...
    component MyComponent(componentRequiredField = 100)  # Здесь мы используем компонент и передаем в него аргументы
    component MyComponent(componentRequiredField = 100, componentOptionalField = 100)
```

### Параметры пути

Declaration
```nim
pathParams:  # Начало объявления параметров пути
  param:  # Здесь мы объявляем параметр `param`
    type int  # Параметр это целое число
    optional  # Опциональный
    mutable  # Изменяемый
    default = 100  # Значение по умолчанию = 100
  param1? int[m] = 100  # А здесь шорткат для param
```

Usage
```nim
serve(...):
  # Использование param
  get "/<param>":
    ...
  
  # Использование param1
  get "/other/<param1>":
    ...
  
  # Вы можете объявить параметры пути в самом пути
  post "/user/$id:int":
    ...
```


### Модели Запросов
Серверная часть HappyX предоставляет инструмент для обработки JSON Body.

Объявление
```nim
model MyModel:  # Модель запроса, названная MyModel
  field1: int
  field2: string

model MyModel1:
  field: MyModel  # Вы также можете использовать другие модели
```

Использование
```nim
serve("127.0.0.1", 5000):
  get "/[val:MyModel1]":
    return {"response": {
      "field1": val.field.field1,
      "field2": val.field.field2
    }}
```

Теперь `127.0.0.1:5000/` будет обрабатывать следующее JSON Body:
```json
{
  "field": {
    "field1": 100,
    "field2": "Hello"
  }
}
```

## Источники и Примеры

**Примеры**

- [RestAPI with HappyX](https://dev.to/ethosa/writing-simple-restapi-in-nim-with-happyx-1-47f1)
- [Simple SPA with HappyX](https://dev.to/ethosa/writing-single-page-application-with-nim-mg8)
