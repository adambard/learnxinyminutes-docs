---
category: tool
name: Compojure
contributors:
    - ["Adam Bard", "http://adambard.com/"]
filename: learncompojure.clj
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

## Compojure 시작하기

Compojure는 최소한의 노력으로 Clojure에서 *빠르고* *성능이 뛰어난* 웹 애플리케이션을 만들기 위한 DSL입니다:

```clojure
(ns myapp.core
  (:require [compojure.core :refer :all]
            [org.httpkit.server :refer [run-server]])) ; httpkit은 서버입니다.

(defroutes myapp
  (GET "/" [] "Hello World"))

(defn -main []
  (run-server myapp {:port 5000}))
```

**1단계:** [Leiningen](http://leiningen.org/)으로 프로젝트를 생성합니다:

```
lein new myapp
```

**2단계:** 위 코드를 `src/myapp/core.clj`에 넣습니다.

**3단계:** `project.clj`에 의존성을 추가합니다:

```
[compojure "1.1.8"]
[http-kit "2.1.16"]
```

**4단계:** 실행:

```
lein run -m myapp.core
```

다음에서 확인: <http://localhost:5000/>

Compojure 앱은 모든 ring 호환 서버에서 실행되지만, 성능과 [대규모 동시성](http://http-kit.org/600k-concurrent-connection-http-kit.html)을 위해 [http-kit](http://http-kit.org/)을 권장합니다.

### 라우트

Compojure에서 각 라우트는 HTTP 메서드와 URL 일치 패턴, 인수 목록 및 본문으로 구성됩니다.

```clojure
(defroutes myapp
  (GET "/" [] "Show something")
  (POST "/" [] "Create something")
  (PUT "/" [] "Replace something")
  (PATCH "/" [] "Modify Something")
  (DELETE "/" [] "Annihilate something")
  (OPTIONS "/" [] "Appease something")
  (HEAD "/" [] "Preview something"))
```

Compojure 라우트 정의는 단순히 [요청 맵을 받아 응답 맵을 반환하는](https://github.com/mmcgrana/ring/blob/master/SPEC) 함수입니다:

```clojure
(myapp {:uri "/" :request-method :post})
; => {:status 200
;     :headers {"Content-Type" "text/html; charset=utf-8}
;     :body "Create Something"}
```

본문은 함수일 수 있으며, 요청을 매개변수로 받아야 합니다:

```clojure
(defroutes myapp
  (GET "/" [] (fn [req] "Do something with req")))
```

또는 요청을 직접 사용할 수도 있습니다:

```clojure
(defroutes myapp
  (GET "/" req "Do something with req"))
```

라우트 패턴에는 명명된 매개변수가 포함될 수 있습니다:

```clojure
(defroutes myapp
  (GET "/hello/:name" [name] (str "Hello " name)))
```

정규식을 제공하여 각 매개변수가 일치하는 것을 조정할 수 있습니다:

```clojure
(defroutes myapp
  (GET ["/file/:name.:ext" :name #".*", :ext #".*"] [name ext]
    (str "File: " name ext)))
```

### 미들웨어

Clojure는 라우팅에 [Ring](https://github.com/ring-clojure/ring)을 사용합니다.
핸들러는 단순히 요청 맵을 받아 응답 맵을 반환하는 함수입니다(Compojure는 문자열을 200 응답으로 변환합니다).

요청 또는 응답을 수정하기 위해 애플리케이션의 전체 또는 일부를 래핑하는 미들웨어를 쉽게 작성할 수 있습니다:

```clojure
(defroutes myapp
  (GET "/" req (str "Hello World v" (:app-version req))))

(defn wrap-version [handler]
  (fn [request]
    (handler (assoc request :app-version "1.0.1"))))

(defn -main []
  (run-server (wrap-version myapp) {:port 5000}))
```

[Ring-Defaults](https://github.com/ring-clojure/ring-defaults)는 사이트 및 API에 유용한 미들웨어를 제공하므로 의존성에 추가하십시오:

```
[ring/ring-defaults "0.1.1"]
```

그런 다음 ns에서 가져올 수 있습니다:

```
(ns myapp.core
  (:require [compojure.core :refer :all]
            [ring.middleware.defaults :refer :all]
            [org.httpkit.server :refer [run-server]]))
```

그리고 `wrap-defaults`를 사용하여 `site-defaults` 미들웨어를 앱에 추가합니다:

```
(defn -main []
  (run-server (wrap-defaults myapp site-defaults) {:port 5000}))
```

이제 핸들러는 쿼리 매개변수를 사용할 수 있습니다:

```clojure
(defroutes myapp
  (GET "/posts" req
    (let [title (get (:params req) :title)
          author (get (:params req) :author)]
      (str "Title: " title ", Author: " author))))
```

또는 POST 및 PUT 요청의 경우 폼 매개변수도 사용할 수 있습니다.

```clojure
(defroutes myapp
  (POST "/posts" req
    (let [title (get (:params req) :title)
          author (get (:params req) :author)]
      (str "Title: " title ", Author: " author))))
```


### 반환 값

라우트 블록의 반환 값은 HTTP 클라이언트에 전달되거나 Ring 스택의 다음 미들웨어에 전달되는 응답 본문을 결정합니다. 가장 일반적으로 이는 위 예제에서와 같이 문자열입니다.
그러나 [응답 맵](https://github.com/mmcgrana/ring/blob/master/SPEC)을 반환할 수도 있습니다:

```clojure
(defroutes myapp
  (GET "/" []
    {:status 200 :body "Hello World"})
  (GET "/is-403" []
    {:status 403 :body ""})
  (GET "/is-json" []
    {:status 200 :headers {"Content-Type" "application/json"} :body "{}"}))
```

### 정적 파일

정적 파일을 제공하려면 `compojure.route.resources`를 사용하십시오.
리소스는 프로젝트의 `resources/` 폴더에서 제공됩니다.

```clojure
(require '[compojure.route :as route])

(defroutes myapp
  (GET "/")
  (route/resources "/")) ; 루트 경로에서 정적 리소스 제공

(myapp {:uri "/js/script.js" :request-method :get})
; => resources/public/js/script.js의 내용
```

### 뷰 / 템플릿

Compojure에서 템플릿을 사용하려면 템플릿 라이브러리가 필요합니다. 몇 가지는 다음과 같습니다:

#### [Stencil](https://github.com/davidsantiago/stencil)

[Stencil](https://github.com/davidsantiago/stencil)은 [Mustache](http://mustache.github.com/) 템플릿 라이브러리입니다:

```clojure
(require '[stencil.core :refer [render-string]])

(defroutes myapp
  (GET "/hello/:name" [name]
    (render-string "Hello {{name}}" {:name name})))
```

리소스 디렉토리에서 템플릿을 쉽게 읽을 수 있습니다. 다음은 도우미 함수입니다.

```clojure
(require 'clojure.java.io)

(defn read-template [filename]
  (slurp (clojure.java.io/resource filename)))

(defroutes myapp
  (GET "/hello/:name" [name]
    (render-string (read-template "templates/hello.html") {:name name})))
```

#### [Selmer](https://github.com/yogthos/Selmer)

[Selmer](https://github.com/yogthos/Selmer)는 Django 및 Jinja2에서 영감을 받은 템플릿 언어입니다:

```clojure
(require '[selmer.parser :refer [render-file]])

(defroutes myapp
  (GET "/hello/:name" [name]
    (render-file "templates/hello.html" {:name name})))
```

#### [Hiccup](https://github.com/weavejester/hiccup)

[Hiccup](https://github.com/weavejester/hiccup)은 HTML을 Clojure 코드로 표현하기 위한 라이브러리입니다.

```clojure
(require '[hiccup.core :as hiccup])

(defroutes myapp
  (GET "/hello/:name" [name]
    (hiccup/html
      [:html
        [:body
          [:h1 {:class "title"}
            (str "Hello " name)]]]))
```

#### [Markdown](https://github.com/yogthos/markdown-clj)

[Markdown-clj](https://github.com/yogthos/markdown-clj)는 Markdown 구현입니다.

```clojure
(require '[markdown.core :refer [md-to-html-string]])

(defroutes myapp
  (GET "/hello/:name" [name]
    (md-to-html-string "## Hello, world")))
```

더 읽을거리:

* [공식 Compojure 문서](https://github.com/weavejester/compojure/wiki)

* [용감하고 진실한 Clojure](http://www.braveclojure.com/)