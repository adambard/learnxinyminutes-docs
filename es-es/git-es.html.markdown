---
category: tool
tool: git
filename: LearnGit-es.txt
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translator:
    - ["Raúl Ascencio", "http://rscnt.github.io"]
lang: es-es

---

Git es un sistema de control de versiones distribuido diseñado para manejar
cualquier tipo de proyecto, ya sea grande o pequeño, con velocidad y eficiencia.

Git realiza esto haciendo "snapshots" del proyecto, con ello permite
versionar y administrar nuestro código fuente.

## Versionamiento, conceptos.

### ¿Qué es el control de versiones?
El control de versiones es un sistema que guarda todos los cambios realizados en
uno o varios archivos, a lo largo del tiempo.

### Versionamiento centralizado vs versionamiento distribuido.

+ El versionamiento centralizado se enfoca en sincronizar, rastrear, y respaldar
  archivos.
+ El versionamiento distribuido se enfoca en compartir los cambios realizados.
  Cada cambio tiene un único identificador.
+ El versionamiento distribuido no tiene una estructura definida, incluso se
  puede mantener el estilo de los repositorios SVN con git.

[Información adicional](http://git-scm.com/book/es/Empezando-Acerca-del-control-de-versiones)

### ¿Por qué usar Git?

* Se puede trabajar sin conexión.
* ¡Colaborar con otros es sencillo!.
* Derivar, crear ramas del proyecto (aka: Branching) es fácil.
* Combinar (aka: Merging)
* Git es rápido.
* Git es flexible.

## Arquitectura de Git.

### Repositorio

Un repositorio es un conjunto de archivos, directorios, registros, cambios (aka:
commits), y encabezados (aka: heads). Imagina que un repositorio es una clase,
y que sus atributos otorgan acceso al historial del elemento, además de otras
cosas.

Un repositorio esta compuesto por la carpeta .git y un "árbol de trabajo".

### Directorio .git (componentes del repositorio)

El directorio .git contiene todas las configuraciones, registros, branches, HEAD
y mas.

[Lista detallada.](http://es.gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Directorio de trabajo (componentes del repositorio)

Es básicamente los directorios y archivos dentro del repositorio. La mayoría de
las veces se le llama "directorio de trabajo".

### Índice (componentes del directorio .git)

El índice es el área de inicio en git. Es básicamente la capa que separa el
directorio de trabajo del repositorio en git. Esto otorga a los desarrolladores
más poder sobre lo que se envía y se recibe del repositorio.

### Commit (aka: cambios)

Un commit es una captura de un conjunto de cambios, o modificaciones hechas en
el directorio de trabajo. Por ejemplo, si se añaden 5 archivos, se eliminan 2,
estos cambios se almacenarán en un commit (aka: captura). Este commit puede ser o
no ser enviado (aka: "pusheado") hacia un repositorio.

### Branch (rama)

Un "branch", es escencialmente un apuntador hacia el último commit (cambio
registrado) que se ha realizado. A medida que se realizan más commits, este
apuntador se actualizará automaticamente hacia el ultimo commit.

### "HEAD" y "head" (componentes del directorio .git)

"HEAD" es un apuntador hacia la rama (branch) que se esta utilizando. Un
repositorio solo puede tener un HEAD activo. En cambio "head", es un apuntador a
cualquier commit realizado, un repositorio puede tener cualquier número de
"heads".

### conceptos - recursos.

* [Git para informáticos](http://eagain.net/articles/git-for-computer-scientists/)
* [Git para diseñadores](http://hoth.entp.com/output/git_for_designers.html)


## Comandos.


### init

Crear un repositorio de git vacio. Las configuraciones, información almacenada y
demás son almacenadas en el directorio ".git".

```bash
$ git init
```

### config

Se utiliza para configurar las opciones ya sea globalmente, o solamente en el
repositorio.

```bash
# Imprime y guarda algunas variables de configuracion básicas. (Globalmente)
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "corre@gmail.com"
$ git config --global user.name "nombre"
```

[Más sobre git config.](http://git-scm.com/book/es/Personalizando-Git-Configuración-de-Git)

### help

Otorga un accceso rápido a una guía extremadamente detallada de cada comando en
git. O puede ser usada simplemente como un recordatorio de estos.

```bash
# Una vista rápida de los comandos disponibles.
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

Muestra las diferencias entre el archivo índice y el commit al cual apunta el
HEAD actualmente.


```bash
# Mostrará el "branch", archivos sin añadir al repo, cambios y otras
# diferencias
$ git status

# Devuelve ayuda sobre el comando status.
$ git help status
```

### add

Para añadir archivos al árbol (directorio, repositorio) de trabajo. Si no se
utiliza `git add`, los nuevos archivos no se añadirán al arbol de trabajo, por
lo que no se incluirán en los commits (cambios).

```bash
# Añade un archivo en el directorio de trabajo actual.
$ git add FooBar.java

# Añade un archivo que se encuentra bajo un directorio.
$ git add /directorio/del/archivo/Foo.c

# Soporte para expresiones regulares!
$ git add ./*.py
```

### branch

Administra las ramas del repositorio ("branches"). Puedes ver, editar, crear y
borrar ramas ("branches"), usando este comando.

```bash
# lista todas las ramas (remotas y locales)
$ git branch -a

# Añadir una nueva rama ("branch").
$ git branch branchNueva

# Eliminar una rama.
$ git branch -d branchFoo

# Renombrar una rama.
# git branch -m <anterior> <nuevo>
$ git branch -m youngling padawan

# Editar la descripcion de la rama.
$ git branch master --edit-description
```

### checkout

Actualiza todos los archivos en el directorio de trabajo para que sean igual que
las versiones almacenadas en el índice, o en un árbol de trabajo especificado.

```bash
# Despachar un repositorio. - Por defecto la master branch. (la rama principal llamada 'master')
$ git checkout
# Despacha una rama especifica.
$ git checkout padawan
# Crea una nueva rama y cambia hacia ella, es igual a utilizar: "git brach jedi; git checkout jedi"
$ git checkout -b jdei
```

### clone

Clona, o copia, un repositorio existente en un nuevo directorio. También añade el
seguimiento hacia las ramas existentes del repositorio que ha sido clonado, lo que
permite subir (push) los archivos hacia una rama remota.

```bash
# Clonar la repo de jquery.
$ git clone https://github.com/jquery/jquery.git
```

### commit

Almacena el contenido actual del índice en un nuevo "commit". Este
commit contiene los cambios hechos más un resumen proporcionado por el desarrollador.

```bash
# realizar un commit y añadirle un mensaje.
$ git commit -m "jedi anakin wil be - jedis.list"
```

### diff

Muestra las diferencias entre un archivo en el directorio de trabajo, el índice
y los commits.

```bash
# Muestra la diferencia entre un directorio de trabajo y el índice.
$ git diff

# Muestra la diferencia entre el índice y los commits más recientes.
$ git diff --cached

# Muestra la diferencia entre el directorio de trabajo y el commit más reciente.
$ git diff HEAD
```

### grep

Permite realizar una busqueda rápida en un repositorio.

Configuraciones opcionales:

```bash
# Gracias a Travis Jeffery por compartir lo siguiente.
# Permite mostrar numeros de lineas en la salida de grep.
$ git config --global grep.lineNumber true

# Realiza una búsqueda mas legible, incluyendo agrupación.
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Busca por "unaVariable" en todos los archivos .java
$ git grep 'unaVariable' -- '*.java'

# Busca por una línea que contenga "nombreArreglo" y "agregar" o "remover"
$ git grep -e 'nombreArreglo' --and \( -e agregar -e remover \)
```

Más ejemplos:

- [Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Muestra los commits (cambios) registrados en el repositorio.

```bash
# Muestra todos los commits.
$ git log

# Muestra un numero x de commits.
$ git log -n 10

# Muestra solo los commits que se han combinado en el historial.
$ git log --merges
```

### merge

Combina los cambios de commits realizados externamente dentro de la rama en la
que se trabaja.

```bash
# Combina la rama especificada en la rama actual.
$ git merge jediMaster

# Siempre genere un solo merge commit cuando se utiliza merge.
$ git merge --no-ff jediMaster
```

### mv

Renombra o mueve un archivo

```bash
# Renombrando un archivo.
$ git mv HolaMundo.c AdiosMundo.c

# Moviendo un archivo.
$ git mv HolaOtraVezMundo.c ./nuevo/directorio/NuevoArchivo.c

# Sustituye un archivo.
$ git mv -f archivoA archivoB
```

### pull

Trae los cambios de un repositorio y los combina en otro en una rama diferente.

```bash
# Actualiza el repositorio local, combinando los nuevos cambios
# de las ramas remotas "origin" y "master".
# git pull <remota> <rama>
$ git pull origin master
```

### push

Envía y combina los cambios de un repositorio local a un repositorio y rama remotos.

```bash
# Envía y combina cambios de un repositorio local hacia un repositorio remoto
# llamados "origin" y "master", respectivamente.
# git push <remota> <rama>
# git push => por defecto es lo mismo que poner =>  git push origin master
$ git push origin master
```

### rebase

Toma todos los cambios que fueron registrados en una rama, y los repite dentro
de otra rama. *No reescribe los commits que se han empujado antes a un repositorio público.*

```bash
# Integrar ramaExperimento dentro de la rama "master"
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Información adicional.](http://git-scm.com/book/es/Ramificaciones-en-Git-Procedimientos-básicos-para-ramificar-y-fusionar)

### reset (precaución)

Reinicia el HEAD actual hacia un estado especificado. Esto permite deshacer
combinaciones (merges), pulls, commits, adds y más. Es un comando útil, pero
tambien peligroso si no se sabe lo que se hace.

```bash
# Reinicia el área principal, con el último cambio registrado. (deja los
# directorios sin cambios)
$ git reset

# Reinicia el área principal, con el último cambio registrado, y reescribe el
# directorio de trabajo.
$ git reset --hard

# Mueve la rama actual hacia el commit especificado (no realiza cambios a los
# directorios), todos los cambios aún existen el directorio.
$ git reset 31f2bb1

# Mueve la rama actual devuelta a un commit especificado, así como el
# directorio (borra todos los cambios que no fueron registrados y todos los
# cambios realizados después del commit especificado).
$ git reset --hard 31f2bb1
```

### rm

Lo contrario de git add, git rm elimina los archivos del directorio de trabajo
actual.

```bash
# Elimina FooBar.c
$ git rm FooBar.c

# Elimina un archivo de un directorio.
$ git rm /directorio/del/archivo/FooBar.c
```

## Información Adicional

* [tryGit - Una forma entretenida y rapida de aprender Git.](http://try.github.io/levels/1/challenges/1)

* [Udemy tutorial de Git: Una guía completa](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [Inmersión Git - Una visita guiada caminando a través de los fundamentos de git](http://gitimmersion.com/)

* [git-scm - Video-tutoriales](http://git-scm.com/videos)

* [git-scm - Documentacion](http://git-scm.com/book/es)

* [Atlassian Git - Tutoriales y Flujos de trabajo](https://www.atlassian.com/git/)

* [SalesForce Chuleta](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [Git - La guía simple](http://rogerdudler.github.io/git-guide/index.html)

* [Pro Git](http://www.git-scm.com/book/en/v2)

* [Una introducción a Git y Github para principiantes (Tutorial)](http://product.hubspot.com/blog/git-and-github-tutorial-for-beginners)
