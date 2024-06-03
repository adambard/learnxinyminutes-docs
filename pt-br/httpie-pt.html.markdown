---
category: tool
tool: httpie
filename: learn-httpie-pt.sh
contributors:
  - ["Adaías Magdiel", "https://github.com/AdaiasMagdiel"]
translators:
  - ["Adaías Magdiel", "https://adaiasmagdiel.com/"]
lang: pt-br
---

HTTPie é um poderoso cliente HTTP para linha de comando, projetado para uma
integração suave com servidores HTTP. Oferece uma interface simples e intuitiva,
tornando-se uma excelente ferramenta para desenvolvedores, testadores e administradores de sistemas.

## Uso Básico

HTTPie possui uma sintaxe simples: http [flags] [MÉTODO] URL [itens].

```bash
http GET https://api.example.com/posts
```

Você pode exibir a requisição sem executá-la, de fato, usando a flag `--offline`.

```bash
http --offline https://api.example.com/posts
```

### Encurtando URLs `localhost`

HTTPie fornece suporte a atalhos para o localhost, similares aos do `curl`. Por exemplo, `:3000`
expande para `http://localhost:3000`. Se a porta for omitida, o padrão será a porta 80.

```bash
http :/users    # http://localhost/users
http :5000/rss  # http://localhost:5000/rss
```

### Métodos Opcionais GET e POST

Se você não especificar o método, o HTTPie usará o seguinte:

- GET para requisições sem corpo
- POST para requisições com corpo

```bash
http https://api.example.com/tags # GET - Seleciona as tags
http https://api.example.com/tags title="Tutorial" slug="tutorial" # POST - Cria uma nova tag
```

## Parâmetros Querystring

Se você adiciona querystrings manualmente no terminal, tente a seguinte sintaxe:
`param==value`. Isso evita que o shell tente reconhecer o operador & como comando
e automaticamente escape caracteres especiais nos parâmetros.
Isso difere dos parâmetros na URL completa, que o HTTPie não modifica.

```bash
http https://api.example.com/search q==httpie per_page==20
```

## Enviando Dados

Você pode enviar dados nos mais diversos formatos, como JSON, formulários ou arquivos.

### Enviando JSON

```bash
http POST https://api.example.com/posts title="Olá" body="Mundo"
```

### Enviando Formulário

```bash
http -f POST https://api.example.com/submit name=John email=john@example.com
```

### Enviando Arquivos

```bash
http --form POST https://api.example.com/upload file@/caminho/do/arquivo.txt
```

## Headers e Autenticação

HTTPie permite que você adicione headers e lide com autenticação de uma forma fácil.

### Headers

```bash
http GET https://api.example.com/posts Authorization:"Bearer Token" User-Agent:"HTTPie"
```

### Autenticação Básica

```bash
http -a usuario:senha GET https://api.example.com/protected
```

### Autenticação Bearer

```bash
https -A bearer -a token https://api.example.com/admin
```

## Lidando com Respostas

HTTPie fornece várias opções para lidar com respostas.

```bash
http GET https://api.example.com/data Accept:application/json  # Exibe o JSON de uma forma legível

http GET https://api.example.com/image --output image.png      # Grava a resposta em um arquivo

http --follow GET https://example.com  # Segue redirecionamentos
```

## Leitura Adicional

- [Documentação Oficial](https://httpie.io/docs/cli)
- [GitHub](https://github.com/httpie)
