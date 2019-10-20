---
category: tool
tool: zfs
contributors:
    - ["sarlalian", "http://github.com/sarlalian"]
translators:
    - ["Christian Grasso","https://grasso.io"]
filename: LearnZfs-it.txt
lang: it-it
---


[ZFS](http://open-zfs.org/wiki/Main_Page) è un sistema di storage che combina file system
tradizionali e volume manager in un unico strumento. ZFS utilizza della terminologia
specifica, diversa da quella usata da altri sistemi di storage, ma le sue funzioni lo
rendono un ottimo tool per gli amministratori di sistema.


## Concetti base di ZFS

### Virtual Device

Un VDEV è simile a un dispositivo gestito da una scheda RAID. Esistono diversi tipi di
VDEV che offrono diversi vantaggi, tra cui ridondanza e velocità. In generale,
i VDEV offrono una maggiore affidabilità rispetto alle schede RAID. Si sconsiglia di
utilizzare ZFS insieme a RAID, poichè ZFS è fatto per gestire direttamente i dischi fisici.

Tipi di VDEV:

* stripe (disco singolo, senza ridondanza)
* mirror (mirror su più dischi)
* raidz
	* raidz1 (parity a 1 disco, simile a RAID 5)
	* raidz2 (parity a 2 dischi, simile a RAID 6)
	* raidz3 (parity a 3 dischi)
* disk
* file (non consigliato in production poichè aggiunge un ulteriore filesystem)

I dati vengono distribuiti tra tutti i VDEV presenti nella Storage Pool, per cui un maggior
numero di VDEV aumenta le operazioni al secondo (IOPS).

### Storage Pool

Le Storage Pool di ZFS sono un'astrazione del livello inferiore (VDEV) e consentono di
separare il filesystem visibile agli utenti dal layout reale dei dischi.

### Dataset

I dataset sono simili ai filesystem tradizionali, ma con molte più funzioni che rendono
vantaggioso l'utilizzo di ZFS. I dataset supportano il [Copy on Write](https://en.wikipedia.org/wiki/Copy-on-write)
gli snapshot, la gestione delle quota, compressione e deduplicazione.


### Limiti

Una directory può contenere fino a 2^48 file, ognuno dei quali di 16 exabyte.
Una storage pool può contenere fino a 256 zettabyte (2^78), e può essere distribuita
tra 2^64 dispositivi. Un singolo host può avere fino a 2^64 storage pool.


## Comandi

### Storage Pool

Azioni:

* List (lista delle pool)
* Status (stato)
* Destroy (rimozione)
* Get/Set (lettura/modifica proprietà)

Lista delle zpool

```bash
# Crea una zpool raidz
$ zpool create bucket raidz1 gpt/zfs0 gpt/zfs1 gpt/zfs2

# Lista delle zpool
$ zpool list
NAME    SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP  HEALTH  ALTROOT
zroot   141G   106G  35.2G         -    43%    75%  1.00x  ONLINE  -

# Informazioni dettagliate su una zpool
$ zpool list -v zroot
NAME                                     SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP HEALTH  ALTROOT
zroot                                    141G   106G  35.2G         -    43%    75%  1.00x ONLINE  -
  gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655   141G   106G  35.2G         -    43%    75%
```

Stato delle zpool

```bash
# Informazioni sullo stato delle zpool
$ zpool status
  pool: zroot
 state: ONLINE
  scan: scrub repaired 0 in 2h51m with 0 errors on Thu Oct  1 07:08:31 2015
config:

        NAME                                          STATE     READ WRITE CKSUM
        zroot                                         ONLINE       0     0     0
          gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655  ONLINE       0     0     0

errors: No known data errors

# "Scrubbing" (correzione degli errori)
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

Proprietà delle zpool

```bash

# Proprietà di una zpool (gestite dal sistema o dall'utente)
$ zpool get all zroot
NAME   PROPERTY                       VALUE                          SOURCE
zroot  size                           141G                           -
zroot  capacity                       75%                            -
zroot  altroot                        -                              default
zroot  health                         ONLINE                         -
...

# Modifica di una proprietà
$ zpool set comment="Dati" zroot
$ zpool get comment
NAME   PROPERTY  VALUE  SOURCE
tank   comment   -      default
zroot  comment   Dati   local
```

Rimozione di una zpool

```bash
$ zpool destroy test
```


### Dataset

Azioni:

* Create
* List
* Rename
* Delete 
* Get/Set (proprietà)

Creazione dataset

```bash
# Crea un dataset
$ zfs create tank/root/data
$ mount | grep data
tank/root/data on /data (zfs, local, nfsv4acls)

# Crea un sottodataset
$ zfs create tank/root/data/stuff
$ mount | grep data
tank/root/data on /data (zfs, local, nfsv4acls)
tank/root/data/stuff on /data/stuff (zfs, local, nfsv4acls)


# Crea un volume
$ zfs create -V zroot/win_vm
$ zfs list zroot/win_vm
NAME                 USED  AVAIL  REFER  MOUNTPOINT
tank/win_vm         4.13G  17.9G    64K  -
```

Lista dei dataset

```bash
# Lista dei dataset
$ zfs list
NAME                                                                       USED  AVAIL  REFER  MOUNTPOINT
zroot                                                                      106G  30.8G   144K  none
zroot/ROOT                                                                18.5G  30.8G   144K  none
zroot/ROOT/10.1                                                              8K  30.8G  9.63G  /
zroot/ROOT/default                                                        18.5G  30.8G  11.2G  /
zroot/backup                                                              5.23G  30.8G   144K  none
zroot/home                                                                 288K  30.8G   144K  none
...

# Informazioni su un dataset
$ zfs list zroot/home
NAME         USED  AVAIL  REFER  MOUNTPOINT
zroot/home   288K  30.8G   144K  none

# Lista degli snapshot
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

Rinominare un dataset

```bash
$ zfs rename tank/root/home tank/root/old_home
$ zfs rename tank/root/new_home tank/root/home
```

Eliminare un dataset

```bash
# I dataset non possono essere eliminati se hanno degli snapshot
$ zfs destroy tank/root/home
```

Lettura/modifica proprietà

```bash
# Tutte le proprietà di un dataset
$ zfs get all  zroot/usr/home                                                                                              │157 # Create Volume
NAME            PROPERTY              VALUE                  SOURCE                                                                          │158 $ zfs create -V zroot/win_vm
zroot/home      type                  filesystem             -                                                                               │159 $ zfs list zroot/win_vm
zroot/home      creation              Mon Oct 20 14:44 2014  -                                                                               │160 NAME                 USED  AVAIL  REFER  MOUNTPOINT
zroot/home      used                  11.9G                  -                                                                               │161 tank/win_vm         4.13G  17.9G    64K  -
zroot/home      available             94.1G                  -                                                                               │162 ```
zroot/home      referenced            11.9G                  -                                                                               │163
zroot/home      mounted               yes                    -
...

# Proprietà specifica
$ zfs get compression zroot/usr/home
NAME            PROPERTY     VALUE     SOURCE
zroot/home      compression  off       default

# Modifica di una proprietà
$ zfs set compression=gzip-9 mypool/lamb

# Specifiche proprietà per tutti i dataset
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


### Snapshot

Gli snapshot sono una delle funzioni più importanti di ZFS:

* Lo spazio occupato è la differenza tra il filesystem e l'ultimo snapshot
* Il tempo di creazione è di pochi secondi
* Possono essere ripristinati alla velocità di scrittura del disco
* Possono essere automatizzati molto semplicemente

Azioni:

* Create
* Delete
* Rename
* Access
* Send / Receive
* Clone


Creazione di uno snapshot

```bash
# Crea uno snapshot di un singolo dataset
zfs snapshot tank/home/sarlalian@now

# Crea uno snapshot di un dataset e dei suoi sottodataset
$ zfs snapshot -r tank/home@now
$ zfs list -t snapshot
NAME                       USED  AVAIL  REFER  MOUNTPOINT
tank/home@now                 0      -    26K  -
tank/home/sarlalian@now       0      -   259M  -
tank/home/alice@now           0      -   156M  -
tank/home/bob@now             0      -   156M  -
...
```

Eliminazione di uno snapshot

```bash
# Elimina uno snapshot
$ zfs destroy tank/home/sarlalian@now

# Elimina uno snapshot ricorsivamente
$ zfs destroy -r tank/home/sarlalian@now

```

Rinominare uno snapshot

```bash
$ zfs rename tank/home/sarlalian@now tank/home/sarlalian@today
$ zfs rename tank/home/sarlalian@now today

$ zfs rename -r tank/home@now @yesterday
```

Accedere ad uno snapshot

```bash
# Utilizzare il comando cd come per una directory
$ cd /home/.zfs/snapshot/
```

Invio e ricezione

```bash
# Backup di uno snapshot su un file
$ zfs send tank/home/sarlalian@now | gzip > backup_file.gz

# Invia uno snapshot ad un altro dataset
$ zfs send tank/home/sarlalian@now | zfs recv backups/home/sarlalian

# Invia uno snapshot ad un host remoto
$ zfs send tank/home/sarlalian@now | ssh root@backup_server 'zfs recv tank/home/sarlalian'

# Invia l'intero dataset e i suoi snapshot ad un host remoto
$ zfs send -v -R tank/home@now | ssh root@backup_server 'zfs recv tank/home'
```

Clonare gli snapshot

```bash
# Clona uno snapshot
$ zfs clone tank/home/sarlalian@now tank/home/sarlalian_new

# Rende il clone indipendente dallo snapshot originale
$ zfs promote tank/home/sarlalian_new
```

### Letture aggiuntive (in inglese)

* [BSDNow's Crash Course on ZFS](http://www.bsdnow.tv/tutorials/zfs)
* [FreeBSD Handbook on ZFS](https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/zfs.html)
* [BSDNow's Crash Course on ZFS](http://www.bsdnow.tv/tutorials/zfs)
* [Oracle's Tuning Guide](http://www.oracle.com/technetwork/articles/servers-storage-admin/sto-recommended-zfs-settings-1951715.html)
* [OpenZFS Tuning Guide](http://open-zfs.org/wiki/Performance_tuning)
* [FreeBSD ZFS Tuning Guide](https://wiki.freebsd.org/ZFSTuningGuide)
