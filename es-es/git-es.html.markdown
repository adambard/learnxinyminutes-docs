---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translator:
    - ["Raúl Ascencio", "http://rscnt.github.io"]
filename: LearnGit.txt
lang: es-es

---

Git es un sistema de control de versiones distribuido diseñado para manejar
cualquier tipo de proyecto ya sea largos o pequeños, con velocidad y eficiencia.

Git realiza esto haciendo "snapshots" del proyecto, con ello permite
versionar y administrar nuestro código fuente.

## Versionamiento, conceptos.

### Que es el control de versiones?
El control de versiones es un sistema que guarda todos los cambios realizados a
uno o varios archivos, a lo largo del tiempo.

### Versionamiento centralizado vs Versionamiento Distribuido.

+ El versionamiento centralizado se enfoca en sincronizar, rastrear, y respaldar
  archivos.
+ El versionamiento distribuido se enfoca en compartir los cambios realizados.
  Cada cambio tiene un único identificador.
+ El versionamiento distribuido no tiene una estructura definida, incluso se
  puede mantener el estilo de los repositorios SVN con git.

[Informacion adicional](http://git-scm.com/book/es/Empezando-Acerca-del-control-de-versiones)

### Por que usar Git?

* Se puede trabajar sin conexion.
* Colaborar con otros es sencillo!.
* Derivar, Crear ramas del proyecto (aka: Branching) es facil!.
* Combinar (aka: Marging)
* Git es rapido.
* Git es flexible.

## Arquitectura de Git.

### Repositorio

Un repositorio es un conjunto de archivos, directorios, registros, cambios (aka:
comits), y encabezados (aka: heads). Imagina que un repositorio es una clase,
y que sus atributos otorgan  acceso al historial del elemento, ademas de otras
cosas.

Un repositorio esta compuesto por la carpeta .git y un "arbol de trabajo".

### Directorio .git (componentes del repositorio)

El directorio .git contiene todas las configuraciones, registros, branches, HEAD
y mas.

[Lista detallada.](http://es.gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Directorio de trabajo (componentes del repositorio)

Es basicamente los directorios y archivos dentro del repositorio. La mayorioa de
las veces se le llama "directorio de trabajo".

### Inidice (componentes del directorio .git)

El inidice es la area de inicio en git. Es basicamente la capa que separa el
directorio de trabajo, del repositorio en git. Esto otorga a los desarrolladores
mas poder sobre lo que envia y recibe en el repositorio.

### Commit (aka: cambios)

Un commit es una captura de un conjunto de cambios, o modificaciones hechas en
el directorio de trabajo. Por ejemplo, si se añaden 5 archivos, se remueven 2,
estos cambios se almacenaran en un commit (aka: captura). Este commit puede ser o
no ser enviado (aka: "pusheado") hacia un repositorio.

### Branch (rama)

Un "branch", es escencialmente un apuntador hacia el ultimo commit (cambio
registrado) que se ha realizado. A medida que se realizan mas commits, este
apuntador se actualizara automaticamente hacia el ultimo commit.

### "HEAD" y "head" (component of .git dir)

"HEAD" es un apuntador hacia la rama (branch) que se esta utilizando. Un
repositorio solo puede tener un HEAD activo. En cambio "head", es un apuntador a
cualquier commit realizado, un repositorio puede tener cualquier numero de
"heads".

### conceptos - recursos.

* [Git para informaticos](http://eagain.net/articles/git-for-computer-scientists/)
* [Git para diseñadores](http://hoth.entp.com/output/git_for_designers.html)


## Comandos.


### init

Crear un repositorio de git vacio. Las configuraciones, informacion almacenada y
demas son almacenadas en el directorio ".git".

```bash
$ git init
```

### config

Se utiliza para configurar las opciones ya sea globalmente, o solamente en el
repositorio.

```bash
# Imprime y guarda algunas variables de configuracion basicas. (Globalmente)
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "corre@gmail.com"
$ git config --global user.name "nombre"
```

[Mas sobre git config.](http://git-scm.com/book/es/Personalizando-Git-Configuración-de-Git)

### help

Otorga un accceso rapido a una guia extremadamente detallada de cada comando en
git. O puede ser usada simplemente como un recordatorio de estos.

```bash
# Una vista rapido de los comandos disponibles.
$ git help

# Chequear todos los comandos disponibles
$ git help -a

# Obtener ayuda especifica de un comando - manual de usuario
# git help <comando>
$ git help add
$ git help commit
$ git help init
```

### status

Muestra las diferencias entre el archivo indice y el commit al cual apunta el
HEAD actualmente.


```bash
# Mostrara el "branch", archivos sin añadir a la repo, cambios y otras
# diferencias
$ git status

# Devuelve ayuda sobre el comando status.
$ git help status
```

### add

Para añadir archivos al arbol (directorio, repositorio) de trabajo. Si no se
utiliza `git add`, los nuevos archivos no se añadiran al arbol de trabajo, por
lo que no se incluiran en los commits (cambios).

```bash
# Añade un archivo en el directorio de trabajo actual.
$ git add FooBar.java

# Añade un archivo que se encuentra bajo un directorio.
$ git add /directorio/del/archivo/Foo.c

# Soporte para expresiones regulares!
$ git add ./*.py
```

### branch

Administra las ramas del repositorios ("branches"). Puedes ver, editar, crear y
borrar ramas ("branches"), usando este comando.

```bash
# lista todas las ramas (remotas y locales)
$ git branch -a

# Añada una nueva rama ("branch").
$ git branch branchNueva

# Eliminar una rama.
$ git branch -d branchFoo

# Renombra una rama.
# git branch -m <anterior> <nuevo>
$ git branch -m youngling padawan

# Edita la descripcion de la rama.
$ git branch master --edit-description
```

### checkout

Actualiza todos los archivos en el directorio de trabajo para que sean igual que
las versiones almacenadas en el indice, o en un arbol de trabajo especificado.

```bash
# Despachar un repositorio. - Por defecto la master branch. (la rama principal llamada 'master')
$ git checkout
# Despacha una rama especifica.
$ git checkout padawan
# Crea una nueva rama y cambia hacia ella, es igual a utilizar: "git brach jedi; git checkout jedi"
$ git checkout -b jdei
```

### clone

Clona, o copia, una repo existente en un nuevo directorio. Tambien añada el
seguimiento hacia las ramas existentes del repo que ha sido clonada, lo que
permite subir (push) los archivos hacia una rama remota.

```bash
# Clonar la repo de jquery.
$ git clone https://github.com/jquery/jquery.git
```

### commit

Almacena los cambios que almacenados en el indice en un nuevo "commit". Este
commit contiene los cambios hechos mas un resumen hecho por el desarrollador.

```bash
# commit with a message
# realizar un commit y añadirle un mensaje.
$ git commit -m "jedi anakin wil be - jedis.list"
```

### diff

Muestra las diferencias entre un archivo en el directorio de trabajo, el indice
y commits.

```bash
# Muestra la diferencia entre un directorio de trabajo y el indice.
$ git diff

# Muestra la diferencia entre el indice y los commits mas recientes.
$ git diff --cached

# Muestra la diferencia entre el directorio de trabajo y el commit mas reciente.
$ git diff HEAD
```

### grep

Permite realizar una busqueda rapida en un repositorio.

Configuracion opcionales:

```bash
# Gracias a Travis Jeffery por compartir lo siguiente.
# Permite mostrar numeros de lineas en la salida de grep.
$ git config --global grep.lineNumber true

# Realiza una busqueda mas lejible, incluyendo agrupacion.
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Busca por "unaVariable" en todos los archivos ,java
$ git grep 'unaVariable' -- '*.java'

# Busca por una linea que contenga "nombreArreglo" y , "agregar" o "remover"
$ git grep -e 'nombreArreglo' --and \( -e agregar -e remover \)
```

Mas ejemplos:
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Muestra los commits (cambios) registrados en el repositotrio.

```bash
# Muestra todos los commits.
$ git log

# Muestra un numero x de commits.
$ git log -n 10

# Muestra solo los commits que se han combinado en el hisotrial
$ git log --merges
```

### merge

Combina los cambios de commits realizados externamente dentro de la rama en la
que se trabaja.

```bash
# Combina la rama especificada en la rama actual.
$ git merge jediMaster

# Siempre genere un solo merge commit cuando se utilizar merge.
$ git merge --no-ff jediMaster
```

### mv

Renombra o mueve un archivo

```bash
# Renombrando un archivo
$ git mv HolaMundo.c AdiosMundo.c

# Moviendo un archivo.
$ git mv HolaOtraVezMundo.c ./nuevo/directorio/NuevoArchivo.c

# Sustituye un archivo.
$ git mv -f archivoA archivoB
```

### pull

Sube (Empuja) de un repositorio y lo combina en otro en una rama diferente.

```bash
# Actualiza el repositorio local, combinando los nuevos cambios.
# de las ramas remotas "origin" y "master".
# from the remote "origin" and "master" branch.
# git pull <remota> <rama>
$ git pull origin master
```

### push

Push and merge changes from a branch to a remote & branch.

```bash
# Push and merge changes from a local repo to a
# Empuja y combina cambios de un repositorio local hacian un repositorio remoto
# llamados "origin" y "master", respectivamente.
# git push <remota> <rama>
# git push => por defecto es lo mismo que poner =>  git push origin master
$ git push origin master
```


Toma todos los cambios que fueron registrados en una rama, y los repite dentro
de otra rama.
*No reescribe los commits que se han empujado antes a un repositorio publico*

```bash
# Integrar ramaExperimento dentro de la rama "master"
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Informacion adicional.](http://git-scm.com/book/es/Ramificaciones-en-Git-Procedimientos-básicos-para-ramificar-y-fusionar)

### reset (precaucion)

Reinicia el cabezal actual hacia un estado especificado. Esto permite desacer
combinaciones (merges), pulls, commits, adds y mas. Es un comando util, pero
tambien peligrosa si no se sabe lo que se hace.

```bash
# Reinica el area principal, con el ultimo cambio registrado. (deja los
# directorios sin cambios)
$ git reset

# Reinica el area principal, con el ultimo cambio registrado, y reescribe el
# directorio de trabajo.
$ git reset --hard

# Mueve la rama actual hacia el commit especificado (no realiza cambios a los
# directorios), todos los cambios aun existen el directorio.
$ git reset 31f2bb1

# Mueve la rama actual devuelta a un commit especificado asi como el
# directorios (borra todos los cambios que no fueron registros y todos los
# cambios realizados despues del commit especificado).
$ git reset --hard 31f2bb1
```

### rm

Lo contrario de git add, git rm remueve los archivos del directorio de trabajo
actual.

```bash
# Remueve FooBar.c
$ git rm FooBar.c

# Remueve un archivo de un directorio.
$ git rm /directorio/del/archivo/FooBar.c
```

## Informacion Adicional

* [tryGit - Una forma entretenida y rapida de aprender Git.](http://try.github.io/levels/1/challenges/1)

* [git-scm - Video-tutoriales](http://git-scm.com/videos)

* [git-scm - Documentacion](http://git-scm.com/book/es)

* [Atlassian Git - Tutoriales y Flujos de trabajo](https://www.atlassian.com/git/)

* [SalesForce Chuleta](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)
