---
category: tool
name: Docker
filename: docker.bat
contributors:
    - ["Ruslan López", "http://javapro.org/"]
    - ["Michael Chen", "https://github.com/ML-Chen"]
    - ["Akshita Dixit", "https://github.com/akshitadixit"]
    - ["Marcel Ribeiro-Dantas", "https://github.com/mribeirodantas"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Docker는 다양한 머신에서 애플리케이션을 원활하게 빌드, 테스트, 배포 및 실행하는 데 도움이 되는 도구입니다. 소프트웨어에 필요한 환경을 모든 머신에서 복제합니다. [docs.docker.com/get-docker/](https://docs.docker.com/get-docker/)에서 Docker를 다운로드할 수 있습니다.

지난 10년 동안 부피가 크고 느린 가상 머신에 비해 가볍고 빠르기 때문에 인기가 높아졌습니다. VM과 달리 docker는 시작하기 위해 자체적으로 완전한 OS를 로드할 필요가 없으며 실행 중인 애플리케이션이 사용할 리소스 외에는 리소스를 놓고 경쟁하지 않습니다. 반면에 VM은 프로세서, 디스크 및 메모리에 대한 리소스 집약적이므로 제한된 용량 아키텍처에서 다양한 애플리케이션에 대해 여러 VM을 실행하는 것이 어려워집니다.

<pre>
┌────────────────────────┐ ┌───────────────────────┐
│      ┌───────────┐     │ │      ┌───────────┐    │
│      │   App     │     │ │      │   App     │    │
│      └───────────┘     │ │      └───────────┘    │
│  ┌────────┐ ┌────────┐ │ │  ┌────────┐ ┌───────┐ │
│  │  Libs  │ │  Deps  │ │ │  │  Libs  │ │  Deps │ │
│  └────────┘ └────────┘ │ │  └────────┘ └───────┘ │
│  ┌───────────────────┐ │ │  ┌──────────────────┐ │
│  │      Guest OS     │ │ │  │     Guest OS     │ │
│  └───────────────────┘ │ │  └──────────────────┘ │
│           VM1          │ │           VM2         │
└────────────────────────┘ └───────────────────────┘
┌──────────────────────────────────────────────────┐
│                     Hypervisor                   │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│                      Host OS                     │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│             Hardware Infrastructure              │
└──────────────────────────────────────────────────┘
              (VM 기반 아키텍처)

┌────────────────────────┐ ┌───────────────────────┐
│      ┌───────────┐     │ │      ┌───────────┐    │
│      │   App     │     │ │      │   App     │    │
│      └───────────┘     │ │      └───────────┘    │
│  ┌────────┐ ┌────────┐ │ │  ┌────────┐ ┌───────┐ │
│  │  Libs  │ │  Deps  │ │ │  │  Libs  │ │  Deps │ │
│  └────────┘ └────────┘ │ │  └────────┘ └───────┘ │
│        Container1      │ │       Container2      │
└────────────────────────┘ └───────────────────────┘
┌──────────────────────────────────────────────────┐
│                       Docker                     │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│                        OS                        │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│             Hardware Infrastructure              │
└──────────────────────────────────────────────────┘
            (Docker 기반 아키텍처)
</pre>

자주 접하게 될 몇 가지 용어는 Docker 이미지와 Docker 컨테이너입니다. 이미지는 [Docker Hub](https://hub.docker.com/)와 같은 컨테이너 레지스트리에 모두 저장된 컨테이너의 패키지 또는 템플릿입니다. 컨테이너는 코드, 런타임, 시스템 도구, 시스템 라이브러리 및 설정 등 소프트웨어를 시작하고 실행하는 데 필요한 모든 것을 포함하는 이러한 이미지의 독립 실행형 실행 가능 인스턴스입니다. Docker는 CLI 클라이언트가 서버 구성 요소인 Docker Engine과 RESTful API를 사용하여 통신하여 명령을 실행하는 클라이언트-서버 아키텍처를 따릅니다.

## Docker CLI

```bash
# https://docs.docker.com/get-docker/에서 Docker를 설치한 후
# 사용 가능한 명령을 나열하려면 매개변수 없이 `docker`를 실행하거나
# `docker help`를 실행하십시오.
$ docker

>>> docker [OPTIONS] COMMAND [ARG...]
       docker [ --help | -v | --version ]

    컨테이너를 위한 자급자족 런타임입니다.

    옵션:
        --config string      클라이언트 구성 파일 위치 (기본값 "/root/.docker")
    -c, --context string     데몬에 연결하는 데 사용할 컨텍스트 이름 (DOCKER_HOST env var 및 "docker context use"로 설정된 기본 컨텍스트 재정의)
    -D, --debug              디버그 모드 활성화
        --help               사용법 인쇄
    -H, --host value         연결할 데몬 소켓 (기본값 [])
    -l, --log-level string   로깅 수준 설정 ("debug"|"info"|"warn"|"error"|"fatal") (기본값 "info")
        --tls                TLS 사용; --tlsverify에 의해 암시됨
        --tlscacert string   이 CA에서 서명한 인증서만 신뢰 (기본값 "/root/.docker/ca.pem")
        --tlscert string     TLS 인증서 파일 경로 (기본값 "/root/.docker/cert.pem")
        --tlskey string      TLS 키 파일 경로 (기본값 "/root/.docker/key.pem")
        --tlsverify          TLS 사용 및 원격 확인
    -v, --version            버전 정보 인쇄 및 종료

    명령:
        attach    실행 중인 컨테이너에 연결
        # [...] 

$ docker run hello-world
# `docker run <container-name>`은 컨테이너를 실행하는 데 사용되며, 시스템에 이미 존재하지 않는 경우 Docker Hub에서 이미지를 가져옵니다. 여기서 docker 클라이언트는 데몬에 연결하고, 데몬은 Docker Hub에서 "hello-world" 이미지를 가져옵니다. 그런 다음 데몬은 이미지에서 새 컨테이너를 빌드하고, 이 컨테이너는 터미널에서 볼 수 있는 클라이언트로 스트리밍되는 출력을 생성하는 실행 파일을 실행합니다.

$ docker run -d ubuntu sleep 60s
# -d (또는 --detach) 플래그는 컨테이너를 백그라운드에서 실행하고 터미널로 돌아가고 싶을 때 사용합니다. 여기서 우리는 ubuntu 컨테이너를 터미널에서 분리하고, 출력은 id여야 하며 명령이 종료됩니다. 실행 중인 컨테이너를 확인하면 여전히 우리 컨테이너가 있어야 합니다:
# CONTAINER ID   IMAGE     COMMAND       CREATED         STATUS         PORTS     NAMES
# 133261b4894a   ubuntu    "sleep 60s"   3 seconds ago   Up 2 seconds             vigorous_gould

$ docker run <container-id> -p 3000:8000
# -p (또는 --publish) 플래그는 컨테이너 내부의 포트 8000을 컨테이너 외부의 포트 3000에 노출하는 데 사용됩니다. 이는 컨테이너 내부의 앱이 격리되어 실행되므로 앱이 실행되는 포트 8000이 컨테이너에 비공개이기 때문입니다.

$ docker run -i
# 또는
$ docker run -it
# Docker는 컨테이너를 비대화형 모드로 실행합니다. 즉, 실행 중에 입력을 받거나 동적으로 작동하지 않습니다. -i 플래그는 컨테이너에 대한 입력을 열어두고, -t 플래그는 셸이 연결할 수 있는 의사 터미널을 생성합니다(-it로 결합 가능).

$ docker ps -a
# `docker ps` 명령은 기본적으로 실행 중인 컨테이너만 표시합니다. 모든 컨테이너를 보려면 -a (또는 --all) 플래그를 사용하십시오.
# 위 명령을 실행하면 터미널에 다음과 유사한 내용이 출력되어야 합니다:
# CONTAINER ID   IMAGE         COMMAND    CREATED         STATUS                     PORTS     NAMES
# 82f84bf6912b   hello-world   "/hello"   9 minutes ago   Exited (0) 9 minutes ago             eloquent_sammet


$ docker stop hello-world
# 또는
$ docker start hello-world
# stop 명령은 하나 이상의 컨테이너를 중지하고, start 명령은 컨테이너를 다시 시작합니다! `docker start -a ubuntu`는 분리된 컨테이너를 터미널에 다시 연결합니다. 즉, 전경에서 실행됩니다.

$ docker create alpine
# `docker create`는 지정된 이미지(여기서는 alpine)로 새 컨테이너를 생성하며, 컨테이너는 `docker run`과 달리 자동 시작되지 않습니다. 이 명령은 컨테이너 구성을 설정한 다음 필요할 때 `docker start`를 사용하여 시작하는 데 사용됩니다. 상태가 "Created"인지 확인하십시오:
# CONTAINER ID   IMAGE         COMMAND       CREATED             STATUS           PORTS     NAMES
# 4c71c727c73d   alpine        "/bin/sh"     29 seconds ago      Created                   naughty_ritchie

$ docker rm 82f84
# 컨테이너 ID를 사용하여 하나 이상의 컨테이너를 제거합니다.
# P.S.: 전체 ID의 처음 몇 자만 사용하여 컨테이너를 식별할 수 있습니다.

$ docker images
# 모든 이미지와 해당 정보를 표시합니다. 여기서 created는 Docker Hub에서 업데이트된 최신 이미지 태그를 의미합니다:
# REPOSITORY    TAG       IMAGE ID       CREATED         SIZE
# ubuntu        latest    a8780b506fa4   9 days ago      77.8MB
# alpine        latest    9c6f07244728   3 months ago    5.54MB
# hello-world   latest    feb5d9fea6a5   13 months ago   13.3kB

$ docker rmi
# 인스턴스(또는 우리가 아는 컨테이너)가 실행 중이 아닌 시스템에서 하나 이상의 이미지를 제거합니다. 이미지에 연결된 컨테이너가 있는 경우 먼저 컨테이너를 삭제하거나 -f (또는 --force) 플래그를 사용하여 컨테이너와 이미지를 모두 강제로 삭제하십시오.

$ docker pull busybox
# pull 명령은 지정된 이미지를 Docker Hub에서 시스템으로 다운로드합니다.

$ docker exec -it 7b272 bash
# 이 명령은 실행 중인 컨테이너의 기본 디렉토리에서 명령을 실행하는 데 사용됩니다. 여기서 7b272는 ubuntu 컨테이너였으며 위 명령은 bash 세션을 열어 컨테이너와 상호 작용하는 데 도움이 됩니다.

$ docker logs <container-id>
# 지정된 컨테이너에서 기록된 정보를 표시합니다.
# root@7b27222e4bb7:/# whoami
# root
# root@7b27222e4bb7:/# pwd
# /
# root@7b27222e4bb7:/# ls
# bin  boot  dev  etc  home  lib  lib32  lib64 libx3 srv  sys  tmp  usr  var
# root@7b27222e4bb7:/# exit
# exit

# 더 많은 명령은 https://docs.docker.com/engine/reference/commandline/docker/에서 찾을 수 있습니다.
```

## Dockerfile
Dockerfile은 Docker 이미지의 청사진입니다. 애플리케이션의 아티팩트와 해당 구성을 이 파일에 특정 구문으로 언급하여 누구나 애플리케이션의 Docker 이미지를 만들 수 있도록 할 수 있습니다.

### 몇 가지 유의 사항:

* 항상 확장자 없이 `Dockerfile`이라는 이름으로 엄격하게 지정됩니다.
* 이미 사용 가능한 일부 Docker 기본 이미지 위에 사용자 지정 이미지를 빌드해야 합니다. (`scratch`라는 빈 이미지가 있어 말 그대로 처음부터 이미지를 빌드할 수 있습니다.)
* 대문자로 된 모든 명령은 구문의 일부이며 대소문자를 구분하지 않지만 규칙처럼 사용됩니다.
* 아래는 샘플 Dockerfile이지만 [공식 문서](https://docs.docker.com/engine/reference/builder/)에서 자세히 읽을 수 있습니다.

```dockerfile
FROM <base-image>
# 기본 이미지 정의

ENV USERNAME='admin'\
    PWD='****'
# 선택적으로 환경 변수 정의

RUN apt-get update
# 컨테이너 환경 내에서 linux 명령 실행, 호스트 환경에 영향을 주지 않음
# 이미지 생성 시 실행됩니다.

COPY <src> <target>
# 호스트에서 실행, src(보통 호스트에 있음)에서 컨테이너의 target으로 파일 복사

ENTRYPOINT ["some-script.sh"]
# 전체 스크립트를 진입점으로 실행

CMD [<args>...]
# 항상 dockerfile의 일부, 진입점 linux 명령 도입 예:
# `CMD node server.js`
# 이미지 생성 후 이미지의 컨테이너가 실행 중일 때만 실행됩니다.
```

### 이미지 빌드
애플리케이션을 Docker 이미지로 래핑한 후 `docker build` 명령을 사용하여 실행(또는 빌드)합니다.

```bash
$ docker build <path-to-dockerfile>
# 지정된 Dockerfile에서 이미지를 빌드하는 데 사용됩니다.
# 경로 대신 URL을 지정할 수도 있습니다.
# -t 태그는 선택 사항이며 이미지에 이름과 태그를 지정하는 데 사용됩니다. 예:
# `$ docker build -t my-image:0.1 ./home/app`
# dockerfile을 변경할 때마다 이미지를 다시 빌드하십시오.
```

## DockerHub에 이미지 푸시
애플리케이션의 Docker 이미지를 모든 Docker 사용자가 공개적으로 사용할 수 있도록 하려면 [Docker Hub](https://hub.docker.com/)에 푸시해야 합니다. Docker Hub는 Docker 이미지의 레지스트리입니다. Docker Hub에 사용자 이름과 암호가 있는 계정이 있는지 확인하십시오.

Docker Hub에 이미지를 푸시할 때 소스 이미지 이름의 일부로 Docker Hub 사용자 이름을 지정해야 합니다. GitHub 리포지토리와 마찬가지로 사용자 이름/이미지 이름의 태그 이름으로 대상 이미지를 만들어야 합니다.

```bash
$ docker login
# 사용자 이름과 암호를 사용하여 Docker Hub에 로그인

$ docker tag <src-image>[:<src-tag>] <target-image>[:<target-tag>]
# 로컬 src-image를 공용 target-image에 태그 지정
# 예: `docker tag my-sample-app:1.0.0  akshitadixit/my-sample-app`
# 태그가 지정되지 않은 경우 기본값은 `latest`입니다.

$ docker push <target-image>[:<target-tag>]
# 이미지를 Docker Hub에 업로드
# 예: `docker push akshitadixit/my-sample-app`
# 이 이미지는 프로필의 리포지토리에서
# `https://hub.docker.com/r/username/image-name`으로 액세스할 수 있습니다.
```