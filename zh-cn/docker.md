---
filename: docker-cn.bat
contributors:
    - ["Ruslan López", "http://javapro.org/"]
translators:
    - ["imba-tjd", "https://github.com/imba-tjd/"]
---

```bat
:: 下载、安装、运行 hello-world 镜像(image)
docker run hello-world

:: :: 如果这是第一次运行，你应该能见到这些信息
:: Unable to find image 'hello-world:latest' locally # 在本地找不到镜像xxx
:: latest: Pulling from library/hello-world
:: 1b930d010525: Pull complete
::   Digest: sha256:4fe721ccc2e8dc7362278a29dc660d833570ec2682f4e4194f4ee23e415e1064
:: Status: Downloaded newer image for hello-world:latest
::
:: Hello from Docker! # 来自Docker的欢迎
:: This message shows that your installation appears to be working correctly. # 此信息表明你的安装似乎成功了
::
:: To generate this message, Docker took the following steps: # Docker进行了如下步骤来产生此信息
:: 1. The Docker client contacted the Docker daemon. # Docker客户端联系Docker守护程序
:: 2. The Docker daemon pulled the "hello-world" image from the Docker Hub. # Docker守护程序从Docker Hub拉取镜像
::     (amd64)
:: 3. The Docker daemon created a new container from that image which runs the # Docker守护程序从镜像中创建了一个容器
::     executable that produces the output you are currently reading. # 运行了产生你正在读的输出的可执行文件
:: 4. The Docker daemon streamed that output to the Docker client, which sent it # Docker守护程序把输出流式传输给Docker客户端，后者发送到你的终端上
::     to your terminal.
::
:: To try something more ambitious, you can run an Ubuntu container with: # 若要尝试更强大的东西，你可以用该命令运行Ubuntu容器
::  $ docker run -it ubuntu bash
::
:: Share images, automate workflows, and more with a free Docker ID: # 使用免费的Docker ID来分享镜像，自动化工作流等
::  https://hub.docker.com/
::
:: For more examples and ideas, visit: # 欲获取更多例子和想法，访问
:: https://docs.docker.com/get-started/

:: 现在来看看当前正运行的镜像
docker ps
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS
::   NAMES

:: 看看之前运行过的镜像
docker ps -a

:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS
::          NAMES
:: 4a76281f9c53        hello-world         "/hello"            2 minutes ago       Exited (0) 2 minutes ago
::          happy_poincare
:: 名字(name)是自动生成的，因此它会和你的不同

:: 移除(remove)我们之前生成的镜像
docker rm happy_poincare

:: 测试是否真的删除了
docker ps -a
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS
::   NAMES

:: 为容器(container)指定自定义名字
docker run --name test_container hello-world
:: Hello from Docker!
:: This message shows that your installation appears to be working correctly.
::
:: To generate this message, Docker took the following steps:
::  1. The Docker client contacted the Docker daemon.
::  2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
::     (amd64)
::  3. The Docker daemon created a new container from that image which runs the
::     executable that produces the output you are currently reading.
::  4. The Docker daemon streamed that output to the Docker client, which sent it
::     to your terminal.
::
:: To try something more ambitious, you can run an Ubuntu container with:
:: $ docker run -it ubuntu bash
::
:: Share images, automate workflows, and more with a free Docker ID:
::  https://hub.docker.com/
::
:: For more examples and ideas, visit:
::  https://docs.docker.com/get-started/

docker ps -a
:: CONTAINER ID        IMAGE               COMMAND             CREATED              STATUS                         PORTS
::               NAMES
:: d345fe1a4f41        hello-world         "/hello"            About a minute ago   Exited (0) About a minute ago
::                test_container
:: 如你所见，名字现在是我们指定的了

:: 从命名过的容器中获取日志(logs)
docker logs test_container
:: Hello from Docker!
:: This message shows that your installation appears to be working correctly.
::
:: To generate this message, Docker took the following steps:
::  1. The Docker client contacted the Docker daemon.
::  2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
::     (amd64)
::  3. The Docker daemon created a new container from that image which runs the
::     executable that produces the output you are currently reading.
::  4. The Docker daemon streamed that output to the Docker client, which sent it
::     to your terminal.
::
:: To try something more ambitious, you can run an Ubuntu container with:
:: $ docker run -it ubuntu bash
::
:: Share images, automate workflows, and more with a free Docker ID:
::  https://hub.docker.com/
::
:: For more examples and ideas, visit:
::  https://docs.docker.com/get-started/

docker rm test_container

docker run ubuntu
::  Unable to find image 'ubuntu:latest' locally
::  latest: Pulling from library/ubuntu
::  2746a4a261c9: Pull complete
::                                                        4c1d20cdee96: Pull complete                                                                                                                                                 0d3160e1d0de: Pull complete                                                                                                                                                 c8e37668deea: Pull complete                                                                                                                                                 Digest: sha256:250cc6f3f3ffc5cdaa9d8f4946ac79821aafb4d3afc93928f0de9336eba21aa4
::  Status: Downloaded newer image for ubuntu:latest

docker ps -a
::  CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS
::           NAMES
::  c19e9e5b000a        ubuntu              "/bin/bash"         5 seconds ago       Exited (0) 4 seconds ago
::           relaxed_nobel

::  在交互模式(interactive mode)下运行容器
docker run -it ubuntu
::  root@e2cac48323d2:/# uname
::  Linux
::  root@e2cac48323d2:/# exit
::  exit

docker rm relaxed_nobel

docker ps -a
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                          PORTS
::               NAMES
:: e2cac48323d2        ubuntu              "/bin/bash"         2 minutes ago       Exited (0) About a minute ago
::               nifty_goldwasser

docker rm nifty_goldwasser
```
