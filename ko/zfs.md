---
category: tool
name: ZFS
contributors:
    - ["sarlalian", "http://github.com/sarlalian"]
    - ["81reap", "https://github.com/81reap"]
    - ["A1EF", "https://github.com/A1EF"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
filename: LearnZfs.txt
---

[ZFS](http://open-zfs.org/wiki/Main_Page)는 스토리지 스택을 재고한 것으로, 전통적인 파일 시스템과 볼륨 관리자를 하나의 통합된 도구로 결합한 것입니다. ZFS는 기존의 스토리지 시스템과 구별되는 몇 가지 고유한 용어를 가지고 있지만, 시스템 관리자를 위한 사용성에 초점을 맞춘 훌륭한 기능들을 제공합니다.

## ZFS 개념

### 가상 장치 (Virtual De[1]vices)

ZFS의 VDEV(가상 장치)는 RAID 장치와 유사하며, 중복성 및 성능 측면에서 다양한 이점을 제공합니다. 일반적으로 VDEV는 RAID 카드보다 더 나은 신뢰성과 안전성을 제공합니다. ZFS는 기본 디스크를 직접 관리하도록 설계되었기 때문에 ZFS와 함께 RAID 설정을 사용하는 것은 권장되지 않습니다.

| VDEV 유형 | 유사 RAID | 참고 |
|-----------|----------------|---------------------------------------|
| 미러(Mirror) | RAID 1 | 중복성을 위해 n-way 미러링을 지원합니다. |
| raidz1 | RAID 5 | 단일 디스크 패리티로, 디스크 하나 장애를 허용합니다. |
| rai[1]dz2 | RAID 6 | 2-디스크 패리티로, 디스크 두 개 장애를 허용합니다. |
| raidz3 | - | 3-디스크 패리티로, 디스크 세 개 장애를 허용합니다. |
| 디스크(Disk) | - | VDEV 내의 단일 물리적 디스크를 나타냅니다. |
| 파일(File) | - | 파일 기반 VDEV로, 복잡성을 더하고 신뢰성을 떨어뜨려 운영 환경에는 권장되지 않습니다. |

ZFS 스토리지 풀의 데이터는 모든 VDEV에 걸쳐 스트라이프됩니다. 더 많은 VDEV, 로그 또는 캐시를 추가하면 IOPS(초당 입출력 연산)가 증가하여 성능이 향상됩니다. 최적의 성능과 중복성을 위해 VDEV의 균형를 맞추는 것이 중요합니다.

### 스토리지 풀

ZFS는 하위 레벨 스토리지 제공자(VDEV)에 대한 추상화로 스토리지 풀을 사용하여, 사용자에게 보이는 파일 시스템을 물리적 레이아웃과 분리할 수 있게 해줍니다.

### ZFS 데이터셋

ZFS 데이터셋은 전통적인 파일 시스템과 유사하지만 훨씬 더 많은 기능을 가지고 있습니다. ZFS의 많은 장점을 제공합니다[1]. 데이터셋은 [COW(기록 중 복사)](https://en.wikipedia.org/wiki/Copy-on-write) 스냅샷, 할당량, 압축 및 중복 제거를 지원합니다[1].

### 제한

하나의 디렉터리는 최대 2^48개의 파일을 포함할 수 있으며, 각 파일은 최대 16엑사바이트까지 가능합니다. 단일 스토리지 풀은 최대 [1]256제타바이트(2^78)의 공간을 가질 수 있으며, 2^64개의 장치에 걸쳐 스트라이프될 수 있습니다. 단일 호스트는 2^64개의 스토리지 풀을 가질 수 있습니다. 이 제한은 엄청나게 큽니다.

## 명령어

### 스토리지 풀

작업:

* 목록 보기
* 상태 확인
* 제거
* 속성 가져오기/설정하기

zpool 목록 보기

```bash
# raidz zpool 생성
$ zpool create zroot raidz1 gpt/zfs0 gpt/zfs1 gpt/zfs2

# ZPool 목록 보기
$ zpool list
NAME    SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP  HEALTH  ALTROOT
zroot   141G   106G  35.2G         -    43%    75%  1.00x  ONLINE  -

# 특정 zpool에 대한 상세 정보 보기
$ zpool list -v zroot
NAME                                     SIZE  ALLOC   FREE  EXPANDSZ   FRAG    CAP  DEDUP HEALTH  ALTROOT
zroot                                    141G   106G  35.2G         -    43%    75%  1.00x ONLINE  -
  gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655   141G   106G  35.2G         -    [2]43%    75%
```

zpool 상태

```bash
# zpool 상태 정보 가져오기
$ zpool status
  pool: zroot
 state: ONLINE
  scan: scrub repaired 0 in 2h51m with 0 errors[3] on Thu Oct  1 07:08:31 2015
config:

        NAME                                          STATE     READ WRITE CKSUM
        zroot                                         ONLINE       0     0 [4]    0
          gptid/c92a5ccf-a5bb-11e4-a77d-001b2172c655  ONLINE       0     0     0

errors: No known data errors

# 오류 수정을 위해 zpool 스크럽하기
$ zpool scru[5]b zroot
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

zpool 속성

```bash
# 풀 속성 가져오기. 속성은 사용자가 설정하거나 시스템이 제공할 수 있습니다.
$ zpool get all zroot
NAME   PROPERTY                       VALUE                          SOURCE
zroot  size                           141G                           -
zroot  capacity                       75%                            -
zroot  altroot                        -                              default
zroot  health                         ONLINE                         -
...

# zpool 속성 설정하기
$ zpool set comment="내 자료 저장소" zroot
$ zpool get comment
NAME   PROPERTY  VALUE                 SOURCE
tank   comment   -                     default
zroot  comment   내 자료 저장소         local
```

zpool 제거

```bash
$ zpool destroy test
```

### 데이터셋

작업:

* 생성
* 목록 보기
* 이름 변경
* 삭제
* 속성 가져오기/설정하기

데이터셋 생성

```bash
# 데이터셋 생성
$ zfs create zroot/root/data
$ mount | grep data
zroot/root/data on /data (zfs, local, nfsv4acls)

# 자식 데이터셋 생성
$ zfs create zroot/root/data/stuff
$ mount | grep data
zroot/root/data on /data (zfs, local, nfsv4acls)
zroot/root/data/stuff on /data/stuff (zfs, local, nfsv4acls)


# 볼륨 생성
$ zfs create -V zroot/win_vm
$ zfs list zroot/win_vm
NAME                 USED  AVAIL  REFER  MOUNTPOINT
zroot/win_vm         4.13G  17.9G    64K  -
```

데이터셋 목록 보기

```bash
# 모든 데이터셋 목록 보기
$ zfs list
NAME                                                                       USED  AVAIL  REFER  MOUNTPOINT
zroot                                                                      106G  30.8G   144K  none
zroot/ROOT                                                                18.5G  30.8G   144K  none
zroot/ROOT/10.1                                                              8K  30.8G  9.63G  /
zroot/ROOT/default                                                        18.5G  30.8G  11.2G  /
zroot/backup                                                              5.23G  30.8G   144K  none
zroot/home                                                                 288K  30.8G   144K  none
...

# 특정 데이터셋 목록 보기
$ zfs list zroot/home
NAME         USED  AVAIL  REFER  MOUNTPOINT
zroot/home   288K  30.8G   144K  none

# 스냅샷 목록 보기
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

데이터셋 이름 변경

```bash
$ zfs rename zroot/root/home zroot/root/old_home
$ zfs rename zroot/root/new_home zroot/root/home
```

데이터셋 삭제

```bash
# 데이터셋에 스냅샷이 있으면 삭제할 수 없습니다
$ zfs destroy zroot/root/home
```

데이터셋 속성 가져오기 / 설정하기

```bash
# 모든 속성 가져오기
$ zfs get all zroot/usr/home
NAME            PROPERTY              VALUE                  SOURCE
zroot/home      type                  filesystem             -
zroot/home      creation              Mon Oct 20 14:44 2014  -
zroot/home      used                  11.9G                  -
zroot/home      available             94.1G                  -
zroot/home      referenced            11.9G                  -
zroot/home      mounted               yes                    -
...

# 데이터셋에서 속성 가져오기
$ zfs get compression zroot/usr/home
NAME            PROPERTY     VALUE     SOURCE
zroot/home      compression  off       default

# 데이터셋에 속성 설정하기
$ zfs set compression=lz4 zroot/lamb

# 모든 데이터셋에서 속성 집합 가져오기
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

### 쓰기 로그 풀

ZFS 인텐트 로그(ZIL)는 동기적 쓰기 속도를 높이기 위해 설계된 쓰기 로그입니다. 이는 일반적으로 대용량 스토리지 풀보다 빠른 드라이브 또는 드라이브 파티션입니다.

```bash
# 로그 풀 추가
$ zpool add mypool/lamb log /dev/sdX

# 설정 확인
$ zpool status mypool/lamb
```

### 읽기 캐시 풀

레벨 2 적응형 교체 캐시(L2ARC)는 주 ARC(RAM 내 캐시)를 확장하며 읽기 캐싱에 사용됩니다. 이는 일반적으로 대용량 스토리지 풀보다 빠른 드라이브 또는 드라이브 파티션입니다.

```bash
# 캐시 풀 추가
$ zpool add mypool/lamb cache /dev/sdY

# 설정 확인
$ zpool status mypool/lamb
```

### 데이터 압축

데이터 압축은 약간의 추가 CPU 사용량을 대가로 데이터가 디스크에서 차지하는 공간의 양을 줄입니다. 활성화하면 디스크 I/O 양을 줄여 성능을 향상시킬 수 있습니다. 특히 디스크 대역폭보다 CPU 리소스가 더 많은 시스템에서 유용합니다.

```bash
# 압축 옵션 가져오기
$ zfs get -help
...
compression     NO       YES   on | off | lzjb | gzip | gzip-[1-9] | zle | lz4 | zstd | zstd-[1-19] | zstd-fast | zstd-fast-[1-10,20,30,40,50,60,70,80,90,100,500,1000]
...

# 압축 설정
$ zfs set compression=on mypool/lamb

# 설정 확인
$ zpool get compression mypool/lamb
```

### 저장 데이터 암호화

암호화를 사용하면 추가 CPU 사이클을 비용으로 장치에서 데이터를 암호화할 수 있습니다. 이 속성은 데이터셋을 생성할 때만 설정할 수 있습니다.

```bash
# 풀에 암호화 활성화
$ zpool set feature@encryption=enabled black_hole

# 프롬프트와 함께 암호화된 데이터셋 생성
$ zfs create -o encryption=on -o keyformat=passphrase black_hole/enc

# 설정 확인
$ zfs get encryption black_hole/enc
```

데이터가 암호화되지 않는 시스템의 일부가 있다는 점에 유의해야 합니다. 자세한 내용은 아래 표를 참조하십시오.

| 구성 요소 | 암호화 여부 | 참고 |
|----------------------|-------------------------------------------|------------------------------------------------------|
| 주 데이터 스토리지 | 예 | 데이터셋/볼륨의 데이터는 암호화됩니다. |
| ZFS 인텐트 로그 (ZIL) | 예 | 동기적 쓰기 요청은 암호화됩니다. |
| L2ARC (캐시) | 예 | 캐시된 데이터는 암호화된 형태로 저장됩니다. |
| RAM (ARC) | 아니요 | RAM에 있는 주 ARC의 데이터는 암호화되지 않습니다. |
| 스왑 영역 | 조건부 | ZFS 스왑 데이터셋이 암호화된 경우 암호화됩니다. |
| ZFS 메타데이터 | 예 | 암호화된 데이터셋의 메타데이터는 암호화됩니다. |
| 스냅샷 데이터 | 예 | 암호화된 데이터셋의 스냅샷도 암호화됩니다. |
| ZFS 전송/수신 | 조건부 | 데이터셋이 암호화되고 -w 플래그가 사용된 경우 전송/수신 중에 암호화됩니다. |

### 스냅샷

ZFS 스냅샷은 zfs의 정말 중요한 기능 중 하나입니다.

* 차지하는 공간은 파일 시스템과 스냅샷 간의 데이터 차이와 같습니다.
* 생성 시간은 단 몇 초에 불과합니다.
* 복구는 데이터를 쓰는 속도만큼 빠릅니다.
* 자동화하기 쉽습니다.

작업:

* 생성
* 삭제
* 이름 변경
* 스냅샷 접근
* 전송 / 수신
* 복제

스냅샷 생성

```bash
# 단일 데이터셋의 스냅샷 생성
zfs snapshot zroot/home/sarlalian@now

# 데이터셋과 그 자식들의 스냅샷 생성
$ zfs snapshot -r zroot/home@now
$ zfs list -t snapshot
NAME                       USED  AV[6]AIL  REFER  MOUNTPOINT
zroot/home@now                 0      -    26K  -
zroot/home/sarlalian@now       0      -   259M  -
zroot/home/alice@now           0      -   156M  -
zroot/home/bob@now             0      -   156M  -
...
```

스냅샷 제거

```bash
# 스냅샷 제거 방법
$ zfs destroy zroot/home/sarlalian@now

# 부모 데이터셋과 그 자식들의 스냅샷 삭제
$ zfs destroy -r zroot/home/sarlalian@now
```
[6]
스냅샷 이름 변경

```bash
# 스냅샷 이름 변경
$ zfs rename zroot/home/sarlalian@now zroot/home/sarlalian@today
$ zfs rename zroot/home/sarlalian@now today

$ zfs rename -r zroot/home@now @yesterday
```

스냅샷 접근

```bash
# 스냅샷 디렉터리로 이동(CD)
$ cd /home/.zfs/snapshot/
```

전송 및 수신

```bash
# 스냅샷을 파일로 백업
$ zfs send zroot/home/sarlalian@now | gzip > backup_file.gz

# 스냅샷을 다른 데이터셋으로 전송
$ zfs send zroot/home/sar[7]lalian@now | zfs recv backups/home/sarlalian

# 스냅샷을 원격 호스트로 전송
$ zfs send zroot/home/sarlalian@now | ssh root@backup_server 'zfs recv zroot/home/sarlalian'

# 스냅샷과 함께 전체 데이터셋을 새 호스트로 전송
$ zfs send -v -R zroot/home@now | ssh root@backup_server 'zfs recv zroot/home'
```

스냅샷 복제

```bash
# 스냅샷 복제
$ zfs clone zroot/home/sarlalian@now zroot/home/sarlalian_new

# 스냅샷에 더 이상 종속되지 않도록 클론을 승격
$ zfs promote zroot/home/sarlalian_new
```

### 종합 예제

다음은 FreeBSD, Jails 및 ZFS를 활용하여 라이브 복제 슬레이브에서 MySQL 스테이징 데이터베이스의 깨끗한 복사본을 프로비저닝하는 것을 자동화하는 스크립트입니다.

```bash
#!/bin/sh

echo "==== 스테이징 데이터베이스 서버 중지 ===="
jail -r staging

echo "==== 기존 스테이징 서버 및 스냅샷 정리 ===="
zfs destroy -r zroot/jails/staging
zfs destroy zroot/jails/slave@staging

echo "==== 슬레이브 데이터베이스 정지 ===="
echo "FLUSH TABLES WITH READ LOCK;" | /usr/local/bin/mysql -u root -pmyrootpassword -h slave

echo "==== 슬레이브 DB 파일 시스템을 zroot/jails/slave@staging으로 스냅샷 생성 ===="
zfs snapshot zroot/jails/slave@staging

echo "==== 슬레이브 데이터베이스 서버 시작 ===="
jail -c slave

echo "==== 슬레이브 스냅샷을 스테이징 서버로 복제 ===="
zfs clone zroot/jails/slave@staging zroot/jails/staging

echo "==== 스테이징 MySQL 설정 설치 ===="
mv /jails/staging/usr/local/etc/my.cnf /jails/staging/usr/local/etc/my.cnf.slave
cp /jails/staging/usr/local/etc/my.cnf.staging /jails/staging/usr/local/etc/my.cnf

echo "==== 스테이징 rc.conf 파일 설정 ===="
mv /jails/staging/etc/rc.conf.local /jails/staging/etc/rc.conf.slave
mv /jails/staging/etc/rc.conf.staging /jails/staging/etc/rc.conf.local

echo "==== 스테이징 DB 서버 시작 ===="
jail -c staging

echo "==== 스테이징 데이터베이스가 마스터에서 데이터를 가져오지 않도록 설정 ===="
echo "STOP SLAVE;" | /usr/local/bin/mysql -u root -pmyrootpassword -h staging
echo "RESET SLAVE;" | /usr/local/bin/mysql -u root -pmyrootpassword -h staging
```

### 추가 자료

* [BSDNow의 ZFS 속성 코스](http://www.bsdnow.tv/tutorials/zfs)
* [FreeBSD 핸드북 - ZFS](https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/zfs.html)
* [BSDNow의 ZFS 속성 코스](http://www.bsdnow.tv/tutorials/zfs)
* [Oracle 튜닝 가이드](http://www.oracle.com/technetwork/articles/servers-storage-admin/sto-recommended-zfs-settings-1951715.html)
* [OpenZFS 튜닝 가이드](http://open-zfs.org/wiki/Performance_tuning)
* [FreeBSD ZFS 튜닝 가이드](https://wiki.freebsd.org/ZFSTuningGuide)