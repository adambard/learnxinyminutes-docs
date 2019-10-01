---
category: tool
tool: zfs
contributors:
    - ["sarlalian", "http://github.com/sarlalian"]
filename: LearnZfs.txt
---


[ZFS](http://open-zfs.org/wiki/Main_Page)
repensa a pilha de armazenamento, combinando sistemas de arquivos tradicionais e gerenciadores de volume em uma ferramenta coesa. O ZFS possui alguma terminologia específica que o diferencia dos sistemas de armazenamento mais tradicionais, no entanto, possui um ótimo conjunto de recursos com foco na usabilidade para administradores de sistemas.


## ZFS Conceitos

### Dispositivos Virtuais

Um VDEV é semelhante a um dispositivo RAID apresentado por uma placa RAID; existem vários tipos diferentes de VDEV que oferecem várias vantagens, incluindo redundância e velocidade. Em geral, os VDEVs oferecem melhor confiabilidade e segurança do que uma placa RAID. É desencorajado o uso de uma configuração RAID com o ZFS, pois o ZFS espera gerenciar diretamente os discos subjacentes.

Tipos de VDEVs

* faixa (um único disco, sem redundância)
* espelho (espelhos n-way suportados)
* raidz
	* raidz1 (paridade de 1 disco, semelhante ao RAID 5)
	* raidz2 (paridade de 2 discos, semelhante ao RAID 6)
	* raidz3 (paridade de 3 discos, sem analógico RAID)
* disco
* arquivo (não recomendado para produção devido a outro sistema de arquivos que adiciona camadas desnecessárias)

Seus dados são distribuídos por todos os VDEVs presentes no seu Pool de Armazenamento, portanto, mais VDEVs aumentarão suas IOPS.

### Pools de armazenamento

O ZFS usa Pools de armazenamento como uma abstração sobre o provedor de armazenamento de nível inferior (VDEV), permitindo separar o sistema de arquivos visível do usuário do layout físico.

### Conjunto de dados ZFS

Os conjuntos de dados do ZFS são análogos aos sistemas de arquivos tradicionais, mas com muitos outros recursos. Eles fornecem muitas das vantagens do ZFS. Os conjuntos de dados oferecem suporte a instantâneos [Copiar na gravação](https://en.wikipedia.org/wiki/Copy-on-write), cotas, compactação e desduplicação.


### Limites

Um diretório pode conter até 2^48 arquivos, com até 16 exabytes cada. Um único pool de armazenamento pode conter até 256 zettabytes (2^78) de espaço e pode ser distribuído por 2^64 dispositivos. Um único host pode ter 2^64 pools de armazenamento. Os limites são enormes.


## Commandos

### Pools de armazenamento

Ações:

* Lista
* Status
* Destruir
* Obter/Definir propriedades

Lista de zpools

```bash
# Criar um raidz zpool
$ zpool create bucket raidz1 gpt/zfs0 gpt/zfs1 gpt/zfs2

# Listar ZPools
$ zpool list
NAME    SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP  HEALTH  ALTROOT
zroot   141G   106G  35.2G         -    43%    75%  1.00x  ONLINE  -

# Listar informações detalhadas sobre um zpool específico
$ zpool list -v zroot
NAME                                     SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP HEALTH  ALTROOT
zroot                                    141G   106G  35.2G         -    43%    75%  1.00x ONLINE  -
  gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655   141G   106G  35.2G         -    43%    75%
```

Status dos zpools

```bash
# Obter informações de status sobre zpools
$ zpool status
  pool: zroot
 state: ONLINE
  scan: scrub repaired 0 in 2h51m with 0 errors on Thu Oct  1 07:08:31 2015
config:

        NAME                                          STATE     READ WRITE CKSUM
        zroot                                         ONLINE       0     0     0
          gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655  ONLINE       0     0     0

errors: No known data errors

# Esfregando um zpool para corrigir erros
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

Properties of zpools

```bash

# Obter propriedades das propriedades do pool pode ser um conjunto de usuários ou um sistema fornecido.
$ zpool get all zroot
NAME   PROPERTY                       VALUE                          SOURCE
zroot  size                           141G                           -
zroot  capacity                       75%                            -
zroot  altroot                        -                              default
zroot  health                         ONLINE                         -
...

# Configurando uma propriedade zpool
$ zpool set comment="Storage of mah stuff" zroot
$ zpool get comment
NAME   PROPERTY  VALUE                 SOURCE
tank   comment   -                     default
zroot  comment   Storage of mah stuff  local
```

Remover um zpool

```bash
$ zpool destroy test
```


### Conjuntos de dados

Ações:

* Criar
* Listar
* Renomear
* Deletar
* Obter/Definir propriedades

Criar conjutos de dados

```bash
# Crisr um conjunto de dados
$ zfs create tank/root/data
$ mount | grep data
tank/root/data on /data (zfs, local, nfsv4acls)

# Criar conjunto de dados filho
$ zfs create tank/root/data/stuff
$ mount | grep data
tank/root/data on /data (zfs, local, nfsv4acls)
tank/root/data/stuff on /data/stuff (zfs, local, nfsv4acls)


# Criar um Volume
$ zfs create -V zroot/win_vm
$ zfs list zroot/win_vm
NAME                 USED  AVAIL  REFER  MOUNTPOINT
tank/win_vm         4.13G  17.9G    64K  -
```

List datasets

```bash
# Listar todos os conjuntos de dados
$ zfs list
NAME                                                                       USED  AVAIL  REFER  MOUNTPOINT
zroot                                                                      106G  30.8G   144K  none
zroot/ROOT                                                                18.5G  30.8G   144K  none
zroot/ROOT/10.1                                                              8K  30.8G  9.63G  /
zroot/ROOT/default                                                        18.5G  30.8G  11.2G  /
zroot/backup                                                              5.23G  30.8G   144K  none
zroot/home                                                                 288K  30.8G   144K  none
...

# Listar um conjunto de dados específico
$ zfs list zroot/home
NAME         USED  AVAIL  REFER  MOUNTPOINT
zroot/home   288K  30.8G   144K  none

# Listar capturas instantâneas
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

Renomear cojunto de dados

```bash
$ zfs rename tank/root/home tank/root/old_home
$ zfs rename tank/root/new_home tank/root/home
```

Deletar um conjunto de dados

```bash
# Os conjuntos de dados não podem ser excluídos se eles tiverem capturas instantâneas
$ zfs destroy tank/root/home
```

Obter/Definir propriedades de um conjunto de dados

```bash
# Obter todas as propriedades
$ zfs get all  zroot/usr/home                                                                                              │157 # Criar Volume
NAME            PROPERTY              VALUE                  SOURCE                                                                          │158 $ zfs create -V zroot/win_vm
zroot/home      type                  filesystem             -                                                                               │159 $ zfs list zroot/win_vm
zroot/home      creation              Mon Oct 20 14:44 2014  -                                                                               │160 NAME                 USED  AVAIL  REFER  MOUNTPOINT
zroot/home      used                  11.9G                  -                                                                               │161 tank/win_vm         4.13G  17.9G    64K  -
zroot/home      available             94.1G                  -                                                                               │162 ```
zroot/home      referenced            11.9G                  -                                                                               │163
zroot/home      mounted               yes                    -
...

# Obter propriedade do conjunto de dados
$ zfs get compression zroot/usr/home
NAME            PROPERTY     VALUE     SOURCE
zroot/home      compression  off       default

# Definir propriedade no conjunto de dados
$ zfs set compression=gzip-9 mypool/lamb

# Obter um conjunto de propriedades de todos os conjuntos de dados
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


### Capturas Instantâneas

As capturas instantâneas do ZFS são uma das coisas mais importantes do zfs

* O espaço que ocupam é igual à diferença de dados entre o sistema de arquivos e seu instantâneo
* O tempo de criação é de apenas segundos
* A recuperação é tão rápida quanto você pode gravar dados.
* Eles são fáceis de automatizar.

Ações:

* Criar
* Deletar
* Renomear
* Accesar  capturas instantâneas
* Enviar / Receber
* Clonar


Criar capturas instantâneas

```bash
# Criar uma captura instantânea de um simples conjunto de dados
zfs snapshot tank/home/sarlalian@now

# Criar uma captura instantânea de um conjunto de dados e seus filhos
$ zfs snapshot -r tank/home@now
$ zfs list -t snapshot
NAME                       USED  AVAIL  REFER  MOUNTPOINT
tank/home@now                 0      -    26K  -
tank/home/sarlalian@now       0      -   259M  -
tank/home/alice@now           0      -   156M  -
tank/home/bob@now             0      -   156M  -
...
```

Destruir capturas instantâneas

```bash
# Como destruir capturas instantâneas
$ zfs destroy tank/home/sarlalian@now

# Excluir uma captura instantânea em um conjunto de dados pai e seus filhos
$ zfs destroy -r tank/home/sarlalian@now

```

Renomeando Captura Instantâneas

```bash
# Renomear uma captura instantânea
$ zfs rename tank/home/sarlalian@now tank/home/sarlalian@today
$ zfs rename tank/home/sarlalian@now today

$ zfs rename -r tank/home@now @yesterday
```

Acessando capturas instantâneas

```bash
# Entre em um diretório de captura instantânea
$ cd /home/.zfs/snapshot/
```

Sending and Receiving

```bash
# Fazer backup de uma captura instantânea em um arquivo
$ zfs send tank/home/sarlalian@now | gzip > backup_file.gz

# Enviar uma captura instantânea para outro conjunto de dados
$ zfs send tank/home/sarlalian@now | zfs recv backups/home/sarlalian

# Enviar uma captura instantânea para um host remoto
$ zfs send tank/home/sarlalian@now | ssh root@backup_server 'zfs recv tank/home/sarlalian'

# Enviar conjunto de dados completo com uma captura instantânea para o novo host
$ zfs send -v -R tank/home@now | ssh root@backup_server 'zfs recv tank/home'
```

Clonando capturas instantâneas

```bash
# Clone uma captura instantânea
$ zfs clone tank/home/sarlalian@now tank/home/sarlalian_new

# Promovendo o clone para que ele não seja mais dependente da captura instantânea
$ zfs promote tank/home/sarlalian_new
```

### Juntando tudo

A seguir, um script utilizando FreeBSD, jails e ZFS para automatizar
provisionando uma cópia limpa de um banco de dados temporário mysql de um escravo de replicação ao vivo.

```bash
#!/bin/sh

echo "==== Parando o servidor de banco de dados temporário ===="
jail -r staging

echo "==== Limpando o Servidor Temporário e as Capturas Instantâneas Existentes ===="
zfs destroy -r zroot/jails/staging
zfs destroy zroot/jails/slave@staging

echo "==== Desativando o banco de dados escravo ===="
echo "FLUSH TABLES WITH READ LOCK;" | /usr/local/bin/mysql -u root -pmyrootpassword -h slave

echo "==== Capturando instantaneamente o sistema de arquivos db slave como zroot/jails/slave@staging ===="
zfs snapshot zroot/jails/slave@staging

echo "==== Iniciando o Servidor de Banco de Dados Escravo ===="
jail -c slave

echo "==== Clonando a captura instantânea do escravo no servidor intermediário ===="
zfs clone zroot/jails/slave@staging zroot/jails/staging

echo "==== Instalando a configuração temporária do mysql ===="
mv /jails/staging/usr/local/etc/my.cnf /jails/staging/usr/local/etc/my.cnf.slave
cp /jails/staging/usr/local/etc/my.cnf.staging /jails/staging/usr/local/etc/my.cnf

echo "==== Configurando o arquivo temporário do rc.conf ===="
mv /jails/staging/etc/rc.conf.local /jails/staging/etc/rc.conf.slave
mv /jails/staging/etc/rc.conf.staging /jails/staging/etc/rc.conf.local

echo "==== Iniciando o servidor de banco de dados temporário ===="
jail -c staging

echo "==== Faz com que o banco de dados temporário não seja extraído do mestre ===="
echo "STOP SLAVE;" | /usr/local/bin/mysql -u root -pmyrootpassword -h staging
echo "RESET SLAVE;" | /usr/local/bin/mysql -u root -pmyrootpassword -h staging
```


### Leitura Adicional

* [Curso de Crash do BSDNow sobre ZFS](http://www.bsdnow.tv/tutorials/zfs)
* [Manual do FreeBSD sobre ZFS](https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/zfs.html)
* [Curso de Crash do BSDNow sobre ZFS](http://www.bsdnow.tv/tutorials/zfs)
* [Guia de Ajuste da Oracle](http://www.oracle.com/technetwork/articles/servers-storage-admin/sto-recommended-zfs-settings-1951715.html)
* [Guia de Ajuste do OpenZFS](http://open-zfs.org/wiki/Performance_tuning)
* [Guia de Ajuste do ZFS do FreeBSD](https://wiki.freebsd.org/ZFSTuningGuide)
