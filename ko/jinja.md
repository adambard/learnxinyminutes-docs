---
name: Jinja
contributors:
  - ["Adaías Magdiel", "https://github.com/AdaiasMagdiel"]
filename: learn-jinja.j2
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---


## Jinja 시작하기

Jinja는 Python 애플리케이션을 위한 빠르고 표현력이 풍부하며 확장 가능한 템플릿 엔진입니다.

Jinja에는 다음과 같은 많은 기능이 포함되어 있습니다:

- 템플릿 상속 및 포함;
- 템플릿 내에서 매크로 정의 및 가져오기;
- XSS 공격을 방지하기 위한 보안 메커니즘;
- 신뢰할 수 없는 템플릿을 안전하게 렌더링할 수 있는 샌드박스 환경;
- 확장 가능한 필터, 테스트, 함수 및 구문까지.

Jinja 템플릿은 단순히 텍스트 파일입니다. Jinja는 특정 확장자가 필요하지 않지만, 일부 IDE에서 더 쉽게 사용할 수 있도록 `.j2` 또는 `.jinja`를 사용하는 것이 일반적입니다.

몇 가지 종류의 구분 기호가 있습니다. 기본 Jinja 구분 기호는 다음과 같이 구성됩니다:

- `{% ... %}` 문
- `{{ ... }}` 템플릿 출력에 인쇄할 표현식
- `{# ... #}` 템플릿 출력에 포함되지 않는 주석

```jinja
{# 이것은 주석의 예입니다. #}

{#
  이 구문을 사용하여
  여러 줄 주석을
  작성할 수도 있습니다.
#}
```


## 변수

```jinja
{# 템플릿에 전달된 컨텍스트에서 변수에 액세스할 수 있습니다. #}

{{ foo }}

{#
  또한 점(.)을 사용하여 변수의 속성에 액세스하거나
  Python 구문을 사용하여 []를 사용할 수 있습니다.
#}

{{ foo.bar }}
{{ foo['bar'] }}

{# 템플릿 내에서 변수를 정의할 수도 있습니다. #}

{% set name = "Magdiel" %}
{{ name }}
```

## 루프

```html
<h1>Members</h1>
<ul>
{% for user in users %}
    <li>{{ user.username }}</li>
{% endfor %}
</ul>


<div>
{% for key, value in my_dict.items() %}
    <p>{{ key }}</p> - <p>{{ value }}</p>
{% endfor %}
</div>


<div>
{% for idx, url in enumerate(urls) %}
    <a href="{{ url }}">Go to url {{ idx + 1 }}</a>
{% endfor %}
</div>
```

## 조건문

Jinja의 if 문은 Python의 if 문과 유사합니다. 가장 기본적인 형태에서는 변수가 정의되어 있고, 비어 있지 않으며, 거짓이 아닌지 확인하는 데 일반적으로 사용됩니다.

```html
{% if users %}
<ul>
{% for user in users %}
    <li>{{ user.username }}</li>
{% endfor %}
</ul>
{% endif %}


{# 여러 분기의 경우 Python과 같이 elif 및 else를 사용할 수 있습니다. #}


{% if message.status == "error" %}
    <p class="text-red-400">{{ message.content }}</p>
{% elif message.status == "success" %}
    <p class="text-green-400">{{ message.content }}</p>
{% else %}
    <p class="text-blue-400">{{ message.content }}</p>
{% endif %}
```

## 템플릿 상속

Jinja의 가장 강력한 기능 중 하나는 템플릿 상속입니다. 다른 파일에서 확장하고 자신의 내용으로 재정의할 수 있는 미리 정의된 블록이 있는 기본 레이아웃을 만들 수 있습니다.

```html
{# file: base.html.j2 #}

<!DOCTYPE html>
<html lang="en">
<head>
    {% block head %}
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{% block title %}{% endblock title %} - Learning Jinja</title>
    {% endblock head %}
</head>
<body>
    <main>
        {% block content %}{% endblock %}
        {# endblock 태그에는 블록 이름이 필요하지 않습니다. #}
    </main>
</body>
</html>



{# file: child.html.j2 #}

{% extends "base.html.j2" %}

{% block head %}
    {{ super() }}
    <script>
        console.log("There's a console.log here")
    </script>
{% endblock %}

{% block title %}Home{% endblock %}

{% block content %}
    <h1>Index</h1>
    <p>Welcome to my home homepage.</p>
{% endblock %}



{# RESULT #}

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Home - Learning Jinja</title>
    <script>
        console.log("There's a console.log here")
    </script>
</head>
<body>
    <main>
        <h1>Index</h1>
        <p>Welcome to my home homepage.</p>
    </main>
</body>
</html>
```

### 내용 포함

`{% include "template/path" %}` 태그를 사용하여 현재 템플릿에 다른 템플릿의 내용을 포함할 수 있습니다.

```html
{# file: footer.html.j2 #}

<footer>
    <p>&copy; 2024 - John Doe</p>
</footer>



{# file: index.html.j2 #}
...
<body>
    <main>
        <h1>Hi! I'm John Doe!</h1>
    </main>
    {% include "footer.html.j2" %}
</body>
...



{# RESULT #}

...
<body>
    <main>
        <h1>Hi! I'm John Doe!</h1>
    </main>
    <footer>
        <p>&copy; 2024 - John Doe</p>
    </footer>
</body>
...
```

기본 템플릿에 전달된 변수는 포함된 템플릿이 기본 템플릿의 컨텍스트에 액세스할 수 있으므로 포함된 템플릿에서도 사용할 수 있습니다.

```html
{# file: greetings.html.j2 #}

<p>I'm the {{ name }} and i like to {{ hobby }}.</p>



{# file: index.html.j2 #}

{% set name = "Captain Nemo" %}
{% set hobby = "navigate through the depths of the ocean" %}

<div>
    {% include "greetings.html.j2" %}
</div>



{# RESULT #}

<div>
    <p>I'm the Captain Nemo and i like to navigate through the depths of the ocean.</p>
</div>
```

## 매크로

매크로는 기본적으로 다른 언어의 함수와 같습니다. 인수가 있거나 없는 매크로를 정의하고 템플릿의 다양한 부분에서 재사용할 수 있습니다.

```html
{% macro input(value="", type="text", placeholder="") -%}
    <input type="{{ type }}" value="{{ value }}" placeholder="{{ placeholder }}">
{%- endmacro %}

<p>{{ input(placeholder="Your username") }}</p>
<p>{{ input(type="password") }}</p>
```

## 공식 문서

더 자세히 알아보려면 [공식 문서](https://jinja.palletsprojects.com/en/)에 액세스하십시오.