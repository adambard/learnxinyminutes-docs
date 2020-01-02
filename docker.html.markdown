---
language: docker
filename: docker.bat
contributors:
    - ["Ruslan López", "http://javapro.org/"]
---

```
:: download, install and run hello-world image
docker run hello-world

:: if this is the first time you should be able to see the message
:: Unable to find image 'hello-world:latest' locally
:: latest: Pulling from library/hello-world
:: 1b930d010525: Pull complete                                                                                             Digest: sha256:4fe721ccc2e8dc7362278a29dc660d833570ec2682f4e4194f4ee23e415e1064
:: Status: Downloaded newer image for hello-world:latest
:: 
:: Hello from Docker!
:: This message shows that your installation appears to be working correctly.
:: 
:: To generate this message, Docker took the following steps:
:: 1. The Docker client contacted the Docker daemon.
:: 2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
::     (amd64)
:: 3. The Docker daemon created a new container from that image which runs the
::     executable that produces the output you are currently reading.
:: 4. The Docker daemon streamed that output to the Docker client, which sent it
::     to your terminal.
:: 
:: To try something more ambitious, you can run an Ubuntu container with:
::  $ docker run -it ubuntu bash
:: 
:: Share images, automate workflows, and more with a free Docker ID:
::  https://hub.docker.com/
:: 
:: For more examples and ideas, visit:
:: https://docs.docker.com/get-started/

:: now lets see currently running images
docker ps
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES

:: lets see the images we have ran previously
docker ps -a

:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS               NAMES
:: 4a76281f9c53        hello-world         "/hello"            2 minutes ago       Exited (0) 2 minutes ago                       happy_poincare
:: the name part is generated automatically so it probably will be different for you

:: let's remove our previously generated image
docker rm happy_poincare

:: lets test if it was really deleted
docker ps -a
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES

:: specify a custom name for the container
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
:: CONTAINER ID        IMAGE               COMMAND             CREATED              STATUS                          PORTS               NAMES
:: d345fe1a4f41        hello-world         "/hello"            About a minute ago   Exited (0) About a minute ago                       test_container
:: as you can see the name is now what we have specified

:: retireve logs from a named container
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
```