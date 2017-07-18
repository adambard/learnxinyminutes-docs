---
category: tool
tool: composer
contributors:
    - ["Brett Taylor", "https://github.com/glutnix"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
filename: LearnComposer-es.sh
---

[Composer](https://getcomposer.org/) es una herramienta para manejar las dependencias en PHP. Nos permite declarar las librerías de las cuales el proyecto depende y las maneja automáticamente (instala/actualiza) por ti.

# Instalando

```sh
# Instala el binario 'composer.phar' en el directorio actual
curl -sS https://getcomposer.org/installer | php
# Si utiliza este método, tendrá que invocar a 'composer' de esta manera:
php composer.phar about

# Instala el binario en ~/bin/composer
# Nota: asegúrese de que ~/bin está en la variable de entorno PATH del shell
curl -sS https://getcomposer.org/installer | php -- --install-dir=~/bin --filename=composer
```

Los usuarios de Windows deben seguir las [instrucciones de instalación de Windows (EN)](https://getcomposer.org/doc/00-intro.md#installation-windows)

## Confirmación de la instalación

```sh
# Comprobar la versión y lista las opciones
composer

# Obtener más ayuda para las opciones
composer help require

# Comprueba si composer es capaz hacer las cosas que necesita y si está actualizado
composer diagnose
composer diag # versión corta

# Actualiza el binario composer a la última versión
composer self-update
composer self # versión corta
```

# Uso

Composer almacena sus dependencias del proyecto en `composer.json`. Usted puede editar este archivo, pero lo mejor es dejar que composer lo gestione por usted.

```sh
# Crear un nuevo proyecto en la carpeta actual
composer init
# Este corre un cuestionario interactivo que le pide detalles sobre su proyecto. 
# Dejándolos en blanco está bien a menos que usted está haciendo otros proyectos que dependen de éste.

# Si un archivo 'composer.json' ya existe, descargar las dependencias
composer install

# Para descargar solo las dependencias de producción, es decir, excluyendo las 
# dependencias de desarrollo
composer install --no-dev

# Añadir una dependencia de producción a este proyecto
composer require guzzlehttp/guzzle
# Composer buscará cuál es la última versión de guzzlehttp/Guzzle, lo descarga,
# y finalmente añade la nueva dependencia al campo requerido en 'composer.json'.

composer require guzzlehttp/guzzle:6.0.*
# Composer descargará la versión más reciente que coincida con el patrón 
# (ej 6.0.2) y añade la dependencia al campo requerido en 'composer.json'.

composer require --dev phpunit/phpunit:~4.5.0
# Se requerirá como una dependencia de desarrollo. Se usará la última 
# versión >= 4.5.0 y < 4.6.0

composer require-dev phpunit/phpunit:^4.5.0
# Se requerirá como una dependencia de desarrollo. Se usará la última versión >= 4.5.0 y <  5.0

# Para obtener más información sobre las coincidencias de versiones de Composer, 
ver [La Documentación de Composer\'s Sobre Versiones (EN)](https://getcomposer.org/doc/articles/versions.md)

# Para ver qué opciones están disponibles para instalar y los paquetes instalados actualmente  
composer show

# Para ver qué paquetes están instalados actualmente
composer show --installed

# Para encontrar un paquete con 'mailgun' en su nombre o descripción
composer search mailgun
```

[Packagist.org (EN)](https://packagist.org/) es el repositorio principal de paquetes de Composer. Busca allí para paquetes existentes de terceros.

## `composer.json` vs `composer.lock`

El archivo `composer.json` almacena las preferencias de versión flotantes de su proyecto para cada dependencia, junto con otra información.

El archivo `composer.lock` almacena exactamente cuál es la versión que ha descargado para cada dependencia. Nunca editar este archivo.

Si se incluye el archivo `composer.lock` en su repositorio git, todos los desarrolladores instalarán la versión utilizada actualmente de la dependencia. Incluso cuando se libera una nueva versión de una dependencia, Composer continuará para descargar la versión grabada en el archivo '.lock'.

```sh
# Si desea actualizar todas las dependencias a su versión más reciente aún que coincidan con sus preferencias versión
composer update

# Si desea la nueva versión de una dependencia particular:
composer update phpunit/phpunit

# Si desea migrar la preferencia de un paquete a una versión más reciente, puede que tenga que quitar primero el paquete de más antiguo y sus dependencias.
composer remove --dev phpunit/phpunit
composer require --dev phpunit/phpunit:^5.0

```

## Autocargador

Composer crea una clase de cargador automático que puede requerir su aplicación. Se puede hacer instancias de clases a través de su espacio de nombres.

```php
require __DIR__ . '/vendor/autoload.php';

$mailgun = new Mailgun\Mailgun("key");
```

### PSR-4 Autocargador

Usted puede añadir sus propios espacios de nombres para el cargador automático.

En `composer.json`, añadir el campo 'autoload':

```json
{
  "autoload": {
    "psr-4": {"Acme\\": "src/"}
  }
}
```
Esto le indicará al cargador automático que busque cualquier cosa en el espacio de nombres `\Acme\` dentro de la carpeta src`.

También puedes usar [usar PSR-0, un mapa de clase o simplemente una lista de archivos para incluir (EN)](https://getcomposer.org/doc/04-schema.md#autoload). También está el campo `autoload-dev` para espacios de nombres de sólo desarrollo.

Al añadir o modificar la clave de carga automática, tendrá que reconstruir el cargador automático:

```sh
composer dump-autoload
composer dump # shorthand

# Optimiza los paquetes PSR0 y PSR4 a ser cargados con classmaps también. Es lento para correr, pero mejora el rendimiento en producción.
composer dump-autoload --optimize --no-dev
```

# El Cache de Composer

```sh
# Composer retendrá los paquetes descargados para su uso en el futuro. Puede removerlos con:
composer clear-cache
```

# Solución de problemas

```sh
composer diagnose
composer self-update
composer clear-cache
```

## Temas (todavía) no cubiertos en este tutorial

* Crear y distribuir tus propios paquetes en Packagist.org o en otra parte
* Pre- y post- script: ejecutar tareas cuando ciertos eventos tienen lugar composer

### Referencias

* [Composer - Dependency Manager for PHP (EN)](https://getcomposer.org/)
* [Packagist.org (EN)](https://packagist.org/)