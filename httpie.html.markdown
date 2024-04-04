---
category: tool
tool: httpie
contributors:
  - ["Adaías Magdiel", "https://github.com/AdaiasMagdiel"]
filename: learn-httpie.sh
---

HTTPie is a powerful command-line HTTP client designed for easy interaction
with HTTP servers. It provides a simple and intuitive interface, making it an
excellent tool for developers, testers, and system administrators.

## Basic Usage

HTTPie follows a simple syntax: http [flags] [METHOD] URL [items].

```bash
http GET https://api.example.com/posts
```

You can print the request without sending it by using the `--offline` flag.

```bash
http --offline https://api.example.com/posts
```

### URL shortcuts for `localhost`

HTTPie supports a curl-like shorthand for localhost. For instance, ":3000"
expands to "http://localhost:3000". If the port is omitted, it assumes port 80.

```bash
http :/users    # http://localhost/users
http :5000/rss  # http://localhost:5000/rss
```

### Optional GET and POST

If you don't specify the METHOD, the HTTPie will use:

- GET for requests without body
- POST for requests with body

```bash
http https://api.example.com/tags # GET tags
http https://api.example.com/tags title="Tutorial" slug="tutorial" # POST a new tag
```

## Querystring Parameters


If you're manually adding query string parameters in the terminal, try the
`param==value` syntax. It avoids shell escaping for & separators and
automatically URL-escapes special characters in parameter names and values.
This differs from parameters in the full URL, which HTTPie doesn't modify.

```bash
http https://api.example.com/search q==httpie per_page==20
```

## Sending Data

You can send data in various formats such as JSON, form data, or files.

### JSON Data

```bash
http POST https://api.example.com/posts title="Hello" body="World"
```

### Form Data

```bash
http -f POST https://api.example.com/submit name=John email=john@example.com
```

### Files

```bash
http --form POST https://api.example.com/upload file@/path/to/file.txt
```

## Headers and Authentication

HTTPie allows you to set headers and handle authentication easily.

### Headers

```bash
http GET https://api.example.com/posts Authorization:"Bearer Token" User-Agent:"HTTPie"
```

### Basic Authentication

```bash
http -a username:password GET https://api.example.com/protected
```

### Bearer Authentication

```bash
https -A bearer -a token https://api.example.com/admin
```

## Response Handling

HTTPie provides various options for handling responses.

```bash
http GET https://api.example.com/data Accept:application/json  # Pretty Print JSON

http GET https://api.example.com/image --output image.png      # Save Response to File

http --follow GET https://example.com  # Follow Redirects
```

## Further Reading

- [Official Documentation](https://httpie.io/docs/cli).
- [GitHub](https://github.com/httpie).
