---
language: docker
filename: docker-es.bat
contributors:
    - ["Ruslan López", "http://javapro.org/"]
    - ["Michael Chen", "https://github.com/ML-Chen"]
lang: es-es
---

```bat
:: descargar, instalar y ejecutar la imágen del hola mundo
docker run hello-world

:: Si esta es la primera vez, deberíais de poder ver el mensaje
:: Unable to find image 'hello-world:latest' locally
:: latest: Pulling from library/hello-world
:: 1b930d010525: Pull complete                                                                                          
::   Digest: sha256:4fe721ccc2e8dc7362278a29dc660d833570ec2682f4e4194f4ee23e415e1064
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
:: El susodicho mensaje se podría traducir como:
:: 
:: Hola desde Docker!
:: Este mensaje muestra que su instalación parece estar funcionando crrectamente.
:: 
:: Para generar este mensaje, Docker realizó los siguientes pasos:
:: 1. El cliente de Docker contactó a Docker daemon.
:: 2. El Docker daemon obtubo la imágen "hello-world" desde Docker Hub.
::     (amd64)
:: 3. El Docker daemon creó un nuevo contenedor a partir de esa imagen con la cual ejecuta el
::     ejecutable que produce la salida que estás leyendo.
:: 4. El Docker daemon transmitió dicha salida el cliente Docker, el cual
::     la envió a tu terminal.
:: 
:: Para intentar algo más ambicioso, puede correr un contenedor Ubuntu mediante:
::  $ docker run -it ubuntu bash
:: 
:: Comparte imágenes, automatice flujos y más con un Docker ID gratuito:
::  https://hub.docker.com/

:: ahora veamos las imágenes que se están ejecutando actualmente
docker ps
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS            
::   NAMES

:: veamos las imágenes que hemos ejecutado previamente
docker ps -a

:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS     
::          NAMES
:: 4a76281f9c53        hello-world         "/hello"            2 minutes ago       Exited (0) 2 minutes ago             
::          happy_poincare
:: la parte del nombre se genera automáticamente, así que probablemente sea diferente para vos

:: eliminemos nuestra imagen previamente generada
docker rm happy_poincare

:: verifiquemos si realmente fue borrada
docker ps -a
:: CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS            
::   NAMES

:: especifiquemos un nombre personalizado para el contenedor
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
:: tal como podeis ver el nombre es el que especificamos

:: obtener los registros de un contenedor nombrado
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

::  ejecutando un contenedor en modo interactivo
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
