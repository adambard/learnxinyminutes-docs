---
category: tool
tool: zfs
contributors:
    - ["sarlalian", "http://github.com/sarlalian"]
    - ["A1EF", "https://github.com/A1EF"]
filename: LearnZfs-ru.txt
translators:
    - ["A1EF", "https://github.com/A1EF"]
lang: ru-ru
---


[ZFS](http://open-zfs.org/wiki/Main_Page)
представляет собой переосмысление системы хранения данных, комбинирующее в едином инструменте
традиционные файловые системы и системы управления томами. ZFS обладает некоторой специфичной
терминологией, которая отличает её от более традиционных систем хранения данных, однако имеет
отличный набор возможностей, акцентированных на удобстве использования системными администраторами.


## Концепции ZFS

### Виртуальные устройства

Виртуальное устройство (VDEV) подобно устройству RAID, представляемого RAID-контроллером.
Есть несколько типов виртуальных устройств (VDEV), которые предлагают различные преимущества,
включая отказоустойчивость и скорость доступа. В основном виртуальные устройства (VDEV)
предоставляют лучшую отказоустойчивость и безопасность, нежели RAID-контроллеры. Не рекомендуется
использовать установку RAID с ZFS, поскольку ZFS рассчитывает непосредственно управлять дисками.

Типы виртуальных устройств (VDEV)

* mirror (поддерживается n-кратное зеркалирование)
* raidz
	* raidz1 (1-диск четности, аналог RAID 5)
	* raidz2 (2-диска четности, аналог RAID 6)
	* raidz3 (3-диска четности, нет имеет аналогичного RAID-массива)
* disk
* file (не рекомендовано для промышленного применения из-за добавления нежелательного промежуточного слоя иной файловой системы)

Ваши данные распределяются среди всех виртуальных устройств (VDEV), представленных в пуле хранения,
так что увеличив число виртуальных устройств (VDEV), вы увеличите количество IOPS.

### Пулы хранения

ZFS использует пулы хранения как абстракцию над нижним уровнем провайдера хранения (VDEV), позволяя вам отделить видимые пользователю
файловые системы от физического их размещения.

### ZFS датасеты

ZFS датасеты подобны традиционным файловым системам, но имеют гораздо больше возможностей, обеспечивающих обилие преимуществ ZFS.
Датасеты поддерживают [Copy on Write](https://ru.wikipedia.org/wiki/%D0%9A%D0%BE%D0%BF%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5_%D0%BF%D1%80%D0%B8_%D0%B7%D0%B0%D0%BF%D0%B8%D1%81%D0%B8), снапшоты, квоты, сжатие и дедубликацию.


### Ограничения

Один каталог может содержать до 2^48 файлов, каждый до 16 эксабайт. Один пул хранения может содержать до 256 зеттабайт (2^78) данных
и может быть распределён по 2^64 устройствам. А один хост может иметь 2^64 пула хранения. Лимиты огромны.


## Команды

### Пулы хранения

Действия:

* Просмотр
* Статус
* Удаление
* Получение/установка свойств

Просмотр пулов

```bash
# Создание пула типа raidz
$ zpool create zroot raidz1 gpt/zfs0 gpt/zfs1 gpt/zfs2

# Просмотр пулов
$ zpool list
NAME    SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP  HEALTH  ALTROOT
zroot   141G   106G  35.2G         -    43%    75%  1.00x  ONLINE  -

# Просмотр детализованной информации о конкретном пуле
$ zpool list -v zroot
NAME                                     SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP HEALTH  ALTROOT
zroot                                    141G   106G  35.2G         -    43%    75%  1.00x ONLINE  -
  gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655   141G   106G  35.2G         -    43%    75%
```

Статус пулов

```bash
# Получение информации о статусе пулов
$ zpool status
  pool: zroot
 state: ONLINE
  scan: scrub repaired 0 in 2h51m with 0 errors on Thu Oct  1 07:08:31 2015
config:

        NAME                                          STATE     READ WRITE CKSUM
        zroot                                         ONLINE       0     0     0
          gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655  ONLINE       0     0     0

errors: No known data errors

# Скрабинг пула для исправления любых ошибок
$ zpool scrub zroot
$ zpool status -v zroot
  pool: zroot
 state: ONLINE
  scan: scrub in progress since Thu Oct 15 16:59:14 2015
        39.1M scanned out of 106G at 1.45M/s, 20h47m to go
        0 repaired, 0.04% done
config:

        NAME                                          STATE     READ WRITE CKSUM
        zroot                                         ONLINE       0     0     0
          gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655  ONLINE       0     0     0

errors: No known data errors
```

Свойства пулов

```bash
# Получение свойств пула, свойства могут быть заданы пользователем или системой.
$ zpool get all zroot
NAME   PROPERTY                       VALUE                          SOURCE
zroot  size                           141G                           -
zroot  capacity                       75%                            -
zroot  altroot                        -                              default
zroot  health                         ONLINE                         -
...

# Установка значения свойства пула
$ zpool set comment="Storage of mah stuff" zroot
$ zpool get comment
NAME   PROPERTY  VALUE                 SOURCE
tank   comment   -                     default
zroot  comment   Storage of mah stuff  local
```

Удаление пула

```bash
$ zpool destroy test
```


### Датасеты

Действия:

* Создание
* Просмотр
* Переименование
* Удаление
* Получение/установка свойств

Создание датасетов

```bash
# Создание датасета
$ zfs create zroot/root/data
$ mount | grep data
zroot/root/data on /data (zfs, local, nfsv4acls)

# Создание дочернего датасета
$ zfs create zroot/root/data/stuff
$ mount | grep data
zroot/root/data on /data (zfs, local, nfsv4acls)
zroot/root/data/stuff on /data/stuff (zfs, local, nfsv4acls)


# Создание тома
$ zfs create -V zroot/win_vm
$ zfs list zroot/win_vm
NAME                 USED  AVAIL  REFER  MOUNTPOINT
zroot/win_vm         4.13G  17.9G    64K  -
```

Просмотр датасетов

```bash
# Просмотр всех датасетов
$ zfs list
NAME                                                                       USED  AVAIL  REFER  MOUNTPOINT
zroot                                                                      106G  30.8G   144K  none
zroot/ROOT                                                                18.5G  30.8G   144K  none
zroot/ROOT/10.1                                                              8K  30.8G  9.63G  /
zroot/ROOT/default                                                        18.5G  30.8G  11.2G  /
zroot/backup                                                              5.23G  30.8G   144K  none
zroot/home                                                                 288K  30.8G   144K  none
...

# Просмотр конкретного датасета
$ zfs list zroot/home
NAME         USED  AVAIL  REFER  MOUNTPOINT
zroot/home   288K  30.8G   144K  none

# Просмотр снапшотов
$ zfs list -t snapshot
zroot@daily-2015-10-15                                                                  0      -   144K  -
zroot/ROOT@daily-2015-10-15                                                             0      -   144K  -
zroot/ROOT/default@daily-2015-10-15                                                     0      -  24.2G  -
zroot/tmp@daily-2015-10-15                                                           124K      -   708M  -
zroot/usr@daily-2015-10-15                                                              0      -   144K  -
zroot/home@daily-2015-10-15                                                             0      -  11.9G  -
zroot/var@daily-2015-10-15                                                           704K      -  1.42G  -
zroot/var/log@daily-2015-10-15                                                       192K      -   828K  -
zroot/var/tmp@daily-2015-10-15                                                          0      -   152K  -
```

Переименование датасетов

```bash
$ zfs rename zroot/root/home zroot/root/old_home
$ zfs rename zroot/root/new_home zroot/root/home
```

Удаление датасета

```bash
# Датасеты не могут быть удалены, если у них имеются какие-то снапшоты
$ zfs destroy zroot/root/home
```

Получение / установка свойств датасета

```bash
# Получение всех свойств
$ zfs get all zroot/usr/home
NAME            PROPERTY              VALUE                  SOURCE
zroot/home      type                  filesystem             -
zroot/home      creation              Mon Oct 20 14:44 2014  -
zroot/home      used                  11.9G                  -
zroot/home      available             94.1G                  -
zroot/home      referenced            11.9G                  -
zroot/home      mounted               yes                    -
...

# Получения свойства для датасета
$ zfs get compression zroot/usr/home
NAME            PROPERTY     VALUE     SOURCE
zroot/home      compression  off       default

# Установка значения свойства на датасете
$ zfs set compression=lz4 zroot/lamb

# Получение значений выбранных свойств всех датасетов
$ zfs list -o name,quota,reservation
NAME                                                               QUOTA  RESERV
zroot                                                               none    none
zroot/ROOT                                                          none    none
zroot/ROOT/default                                                  none    none
zroot/tmp                                                           none    none
zroot/usr                                                           none    none
zroot/home                                                          none    none
zroot/var                                                           none    none
...
```


### Снапшоты

ZFS снапшоты -- одна из очень важных особенностей zfs

* Место, которое они занимают, равно разнице данных между файловой системой и её снапшотом
* Время создания занимает считанные секунды
* Восстановление настолько быстро, насколько позволяет вам запись данных
* Они очень просты в автоматизации

Действия:

* Создание
* Удаление
* Переименование
* Получение доступа к снапшотам
* Отправка / Получение
* Клонирование


Создание снапшотов

```bash
# Создание снапшота единственного датасета
zfs snapshot zroot/home/sarlalian@now

# Создание снапшота для родительского и дочерних датасетов
$ zfs snapshot -r zroot/home@now
$ zfs list -t snapshot
NAME                       USED  AVAIL  REFER  MOUNTPOINT
zroot/home@now                 0      -    26K  -
zroot/home/sarlalian@now       0      -   259M  -
zroot/home/alice@now           0      -   156M  -
zroot/home/bob@now             0      -   156M  -
...
```

Удаление снапшотов

```bash
# Как удалить снапшот
$ zfs destroy zroot/home/sarlalian@now

# Удаление снапшота в родительском и дочерних датасетах
$ zfs destroy -r zroot/home/sarlalian@now
```

Переименование снапшотов

```bash
# Переименование снапшота
$ zfs rename zroot/home/sarlalian@now zroot/home/sarlalian@today
$ zfs rename zroot/home/sarlalian@now today

$ zfs rename -r zroot/home@now @yesterday
```

Получение доступа к спапшотам

```bash
# CD в каталог снапшота
$ cd /home/.zfs/snapshot/
```

Отправка и получение

```bash
# Сохранение снапшота в файл
$ zfs send zroot/home/sarlalian@now | gzip > backup_file.gz

# Отправка снапшота в другой датасет
$ zfs send zroot/home/sarlalian@now | zfs recv backups/home/sarlalian

# Отправка снапшота на удаленный хост
$ zfs send zroot/home/sarlalian@now | ssh root@backup_server 'zfs recv zroot/home/sarlalian'

# Отправка полного датасета со снапшотами на новый хост
$ zfs send -v -R zroot/home@now | ssh root@backup_server 'zfs recv zroot/home'
```

Клонирование снапшотов

```bash
# Клонирование снапшота
$ zfs clone zroot/home/sarlalian@now zroot/home/sarlalian_new

# Повышение клона, чтобы он больше не зависел от снапшота
$ zfs promote zroot/home/sarlalian_new
```

### Собираем всё вместе

Нижеследующий скрипт использует FreeBSD, jails и ZFS для автоматизации
подготовки чистой копии стейджинговой базы mysql с живой реплики слейв-ноды.

```bash
#!/bin/sh

echo "==== Остановка стейджингового сервера баз данных ===="
jail -r staging

echo "==== Очистка существующих стейджингового сервера и снапшота ===="
zfs destroy -r zroot/jails/staging
zfs destroy zroot/jails/slave@staging

echo "==== Фиксация базы на слейве ===="
echo "FLUSH TABLES WITH READ LOCK;" | /usr/local/bin/mysql -u root -pmyrootpassword -h slave

echo "==== Сохранение снапшота файлов базы слейва как zroot/jails/slave@staging ===="
zfs snapshot zroot/jails/slave@staging

echo "==== Запуск слейв-ноды сервера баз данных ===="
jail -c slave

echo "==== Клонирование снапшота слейва на стейджинговый сервер ===="
zfs clone zroot/jails/slave@staging zroot/jails/staging

echo "==== Установка конфига стейджингово mysql ===="
mv /jails/staging/usr/local/etc/my.cnf /jails/staging/usr/local/etc/my.cnf.slave
cp /jails/staging/usr/local/etc/my.cnf.staging /jails/staging/usr/local/etc/my.cnf

echo "==== Настройка стейджингового файла rc.conf ===="
mv /jails/staging/etc/rc.conf.local /jails/staging/etc/rc.conf.slave
mv /jails/staging/etc/rc.conf.staging /jails/staging/etc/rc.conf.local

echo "==== Запуск стейджингово сервера баз данных ===="
jail -c staging

echo "==== Отключение синхронизации стейджинговой базы с мастером ===="
echo "STOP SLAVE;" | /usr/local/bin/mysql -u root -pmyrootpassword -h staging
echo "RESET SLAVE;" | /usr/local/bin/mysql -u root -pmyrootpassword -h staging
```


### Почитать дополнительно

* [BSDNow's Crash Course on ZFS](http://www.bsdnow.tv/tutorials/zfs)
* [FreeBSD Handbook on ZFS](https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/zfs.html)
* [BSDNow's Crash Course on ZFS](http://www.bsdnow.tv/tutorials/zfs)
* [Oracle's Tuning Guide](http://www.oracle.com/technetwork/articles/servers-storage-admin/sto-recommended-zfs-settings-1951715.html)
* [OpenZFS Tuning Guide](http://open-zfs.org/wiki/Performance_tuning)
* [FreeBSD ZFS Tuning Guide](https://wiki.freebsd.org/ZFSTuningGuide)
