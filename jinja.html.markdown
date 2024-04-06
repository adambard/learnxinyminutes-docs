---
language: Jinja
contributors:
  - ["Adaías Magdiel", "https://github.com/AdaiasMagdiel"]
filename: learn-jinja.j2
---

## Getting Started with Jinja

Jinja is a fast, expressive, and extensible templating engine for Python
applications.

Jinja includes a lot of functionalities, such as:
- Template inheritance and inclusion;
- Defining and importing macros within templates;
- Security mechanisms to prevent XSS attacks;
- A sandboxed environment that can safely render untrusted templates;
- Extensible filters, tests, functions, and even syntax.

A Jinja template is simply a text file. Jinja doesn't require a specific
extension, but it's common to use `.j2` or `.jinja` to make it easier for
some IDEs.

There are a few kinds of delimiters. The default Jinja delimiters are configured
as follows:

- `{% ... %}` for Statements
- `{{ ... }}` for Expressions to print to the template output
- `{# ... #}` for Comments not included in the template output

```jinja
{# This is an example of a comment. #}

{#
  You can use this syntax
  to write multiline comments
  as well.
#}
```


## VARIABLES

```jinja
{# You have the option to access variables from the context passed to the template #}

{{ foo }}

{# 
  Additionally, you can use a dot (.) to access attributes of a variable or
  use Python syntax, using []
#}

{{ foo.bar }}
{{ foo['bar'] }}

{# Within the template, you can define variables as well #}

{% set name = "Magdiel" %}
{{ name }}
```

## Loops

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

## Conditionals

The if statement in Jinja is similar to the if statement in Python. It is
commonly used to check if a variable is defined, not empty, and not false in
its most basic form.

```html
{% if users %}
<ul>
{% for user in users %}
    <li>{{ user.username }}</li>
{% endfor %}
</ul>
{% endif %}


{# For multiple branches, elif and else can be used like in Python. #}


{% if message.status == "error" %}
    <p class="text-red-400">{{ message.content }}</p>
{% elif message.status == "success" %}
    <p class="text-green-400">{{ message.content }}</p>
{% else %}
    <p class="text-blue-400">{{ message.content }}</p>
{% endif %}
```

## Template Inheritance

One of the most powerful features of Jinja is template inheritance. You can
create a base layout with predefined blocks that you can extend in another file
and override with your own content.

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
        {# the endblock tag doesn't need the name of the block #}
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

### Including Content

You can include content from another template on your current template using
the `{% include "template/path" %}` tag.

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

Variables passed to the main template can also be used in the include, as the
included template has access to the context of the main template.

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

## Macros

Macros are basically like functions in another languages. You can define macros with or without arguments and reuse them in various parts of your template.

```html
{% macro input(value="", type="text", placeholder="") -%}
    <input type="{{ type }}" value="{{ value }}" placeholder="{{ placeholder }}">
{%- endmacro %}

<p>{{ input(placeholder="Your username") }}</p>
<p>{{ input(type="password") }}</p>
```

## Official Documentation

To learn more, access the [official documentation](https://jinja.palletsprojects.com/en/).
