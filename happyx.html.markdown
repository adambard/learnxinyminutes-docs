---
category: tool
tool: HappyX
contributors:
    - ["Ethosa", "https://github.com/Ethosa"]
filename: learnHappyX.html
---

## HappyX Tutorial.

HappyX is macro-oriented full-stack web framework written in Nim.

HappyX started development in April 2023.
Version 1.0.0 was released at June 1, 2023.

### Install

To use HappyX you should have Nim and Nimble.

Install last version
```bash
nimble install happyx@#head
```

### Hello World

For SSR/SSG projects simplest code may be
```nim
import happyx

serve("127.0.0.1", 5000):
  get "/":
    "Hello, world!"
```

For SPA projects simplest code may be
```nim
import happyx

appRoutes("app"):
  "/":
    "Hello, world!"
```


### Components

In single page application you can use components

Declaration
```nim
component MyComponent:  # declare component with name "MyComponent"
  componentRequiredField: int
  componentOptionalField: int = 10

  `template`:  # HTML starts here
    tDiv(class = "..."):  # <div class="..."></div>
      "Hello, world!"  # Raw text
  
  # You can add custom styles for components
  `style`: """
    .someClasss {
      ...
    }
  
  `script`:
    # Here you can use real Nim code
    var x = 1  # declare mutable variable named `x` with value `1`
    var y = 2
    echo x + y
```

Usage:
```nim
var myHtml = buildHtml:  # declare `myHtml` mutable variable that contains Tag object
  tDiv(class = "..."):
    ...
    component MyComponent(componentRequiredField = 100)  # Here component usage
    component MyComponent(componentRequiredField = 100, componentOptionalField = 100)  # Here component usage
```

### Path Params

Declaration
```nim
pathParams:  # Start path params declaration
  param:  # here is path param named `param`
    type int  # param is integer
    optional  # param is optional
    mutable  # param is mutable
    default = 100  # default value is 100
  param1? int[m] = 100  # Here is shortcut for param
```

Usage
```nim
serve(...):
  # Use param
  get "/<param>":
    ...
  
  # Use param1
  get "/other/<param1>":
    ...
  
  # You can assign path params in route
  post "/user/$id:int":
    ...
```


### Request Models

Declaration
```nim
model MyModel:  # request model named MyModel
  field1: int  # integer field named field1
  field2: string  # string field named field2

model MyModel1:  # request model named MyModel1
  field: MyModel  # MyModel field named field
```

Usage
```nim
serve("127.0.0.1", 5000):
  get "/[val:MyModel1]":
    return {"response": {
      "field1": val.field.field1,
      "field2": val.field.field2
    }}
```

Now `127.0.0.1:5000/` will handle JSON body:
```json
{
  "field": {
    "field1": 100,
    "field2": "Hello"
  }
}
```

## Source & References

**Examples**

- [RestAPI with HappyX](https://dev.to/ethosa/writing-simple-restapi-in-nim-with-happyx-1-47f1)
- [Simple SPA with HappyX](https://dev.to/ethosa/writing-single-page-application-with-nim-mg8)
