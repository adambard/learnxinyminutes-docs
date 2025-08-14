---
category: tool
name: HTTPie
contributors:
  - ["Adaías Magdiel", "https://github.com/AdaiasMagdiel"]
filename: learn-httpie.sh
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

HTTPie는 HTTP 서버와 쉽게 상호 작용하도록 설계된 강력한 명령줄 HTTP 클라이언트입니다. 간단하고 직관적인 인터페이스를 제공하여 개발자, 테스터 및 시스템 관리자에게 훌륭한 도구입니다.

## 기본 사용법

HTTPie는 간단한 구문을 따릅니다: http [플래그] [메서드] URL [항목].

```bash
http GET https://api.example.com/posts
```

`--offline` 플래그를 사용하여 요청을 보내지 않고 인쇄할 수 있습니다.

```bash
http --offline https://api.example.com/posts
```

### `localhost`에 대한 URL 바로 가기

HTTPie는 localhost에 대한 curl과 유사한 약어를 지원합니다. 예를 들어, `:3000`은 `http://localhost:3000`으로 확장됩니다. 포트가 생략되면 포트 80을 가정합니다.

```bash
http :/users    # http://localhost/users
http :5000/rss  # http://localhost:5000/rss
```

### 선택적 GET 및 POST

메서드를 지정하지 않으면 HTTPie는 다음을 사용합니다:

- 본문 없는 요청의 경우 GET
- 본문 있는 요청의 경우 POST

```bash
http https://api.example.com/tags # GET 태그
http https://api.example.com/tags title="Tutorial" slug="tutorial" # 새 태그 POST
```

## 쿼리 문자열 매개변수

터미널에서 쿼리 문자열 매개변수를 수동으로 추가하는 경우 `param==value` 구문을 사용해 보십시오. & 구분 기호에 대한 셸 이스케이프를 피하고 매개변수 이름 및 값의 특수 문자를 자동으로 URL 이스케이프합니다. 이는 HTTPie가 수정하지 않는 전체 URL의 매개변수와 다릅니다.

```bash
http https://api.example.com/search q==httpie per_page==20
```

## 데이터 전송

JSON, 양식 데이터 또는 파일과 같은 다양한 형식으로 데이터를 보낼 수 있습니다.

### JSON 데이터

```bash
http POST https://api.example.com/posts title="Hello" body="World"
```

### 양식 데이터

```bash
http -f POST https://api.example.com/submit name=John email=john@example.com
```

### 파일

```bash
http --form POST https://api.example.com/upload file@/path/to/file.txt
```

## 헤더 및 인증

HTTPie를 사용하면 헤더를 설정하고 인증을 쉽게 처리할 수 있습니다.

### 헤더

```bash
http GET https://api.example.com/posts Authorization:"Bearer Token" User-Agent:"HTTPie"
```

### 기본 인증

```bash
http -a username:password GET https://api.example.com/protected
```

### 전달자 인증

```bash
https -A bearer -a token https://api.example.com/admin
```

## 응답 처리

HTTPie는 응답을 처리하기 위한 다양한 옵션을 제공합니다.

```bash
http GET https://api.example.com/data Accept:application/json  # JSON 예쁘게 인쇄

http GET https://api.example.com/image --output image.png      # 파일에 응답 저장

http --follow GET https://example.com  # 리디렉션 따르기
```

## 추가 자료

- [공식 문서](https://httpie.io/docs/cli)
- [GitHub](https://github.com/httpie)