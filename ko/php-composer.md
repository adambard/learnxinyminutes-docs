---
category: tool
name: Composer
contributors:
    - ["Brett Taylor", "https://github.com/glutnix"]
filename: LearnComposer.sh
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Composer](https://getcomposer.org/)는 PHP의 의존성 관리 도구입니다. 프로젝트가 의존하는 라이브러리를 선언할 수 있으며, Composer가 이를 관리(설치/업데이트)해 줍니다.

# 설치

```sh
# 현재 디렉토리에 composer.phar 바이너리를 설치합니다
curl -sS https://getcomposer.org/installer | php
# 이 방법을 사용하는 경우 다음과 같이 composer를 호출해야 합니다:
php composer.phar about

# 바이너리를 ~/bin/composer에 설치합니다
# 참고: ~/bin이 셸의 PATH 환경 변수에 있는지 확인하십시오
curl -sS https://getcomposer.org/installer | php -- --install-dir=~/bin --filename=composer
```

Windows 사용자는 [Windows 설치 지침](https://getcomposer.org/doc/00-intro.md#installation-windows)을 따라야 합니다.

## 설치 확인

```sh
# 버전 확인 및 옵션 목록
composer

# 옵션에 대한 추가 도움말 얻기
composer help require

# Composer가 필요한 작업을 수행할 수 있는지, 최신 버전인지 확인
composer diagnose
composer diag # 약어

# Composer 바이너리를 최신 버전으로 업데이트
composer self-update
composer self # 약어
```

# 사용법

Composer는 프로젝트 의존성을 `composer.json`에 저장합니다. 이 파일을 편집할 수 있지만, Composer가 관리하도록 하는 것이 가장 좋습니다.

```sh
# 현재 폴더에 새 프로젝트 생성
composer init
# 프로젝트에 대한 세부 정보를 묻는 대화형 설문지를 실행합니다. 다른 프로젝트가 이 프로젝트에 의존하지 않는 한 비워 두어도 괜찮습니다.

# composer.json 파일이 이미 있는 경우 의존성 다운로드
composer install

# 개발 의존성을 제외한 프로덕션 의존성만 다운로드하려면
composer install --no-dev

# 이 프로젝트에 프로덕션 의존성 추가
composer require guzzlehttp/guzzle
# guzzlehttp/guzzle의 최신 버전을 파악하고, 다운로드한 다음, composer.json의 require 필드에 새 의존성을 추가합니다.

composer require guzzlehttp/guzzle:6.0.*
# 패턴과 일치하는 최신 버전(예: 6.0.2)을 다운로드하고 composer.json의 require 필드에 의존성을 추가합니다

composer require --dev phpunit/phpunit:~4.5.0
# 개발 의존성으로 필요합니다. >=4.5.0 및 < 4.6.0의 최신 버전을 사용합니다

composer require-dev phpunit/phpunit:^4.5.0
# 개발 의존성으로 필요합니다. >=4.5.0 및 < 5.0의 최신 버전을 사용합니다

# Composer 버전 일치에 대한 자세한 내용은 [Composer 버전 설명서](https://getcomposer.org/doc/articles/versions.md)를 참조하십시오

# 설치 가능하거나 현재 설치된 패키지를 보려면
composer show

# 현재 설치된 패키지를 보려면
composer show --installed

# 이름이나 설명에 'mailgun'이 포함된 패키지를 찾으려면
composer search mailgun
```

[Packagist.org](https://packagist.org/)는 Composer 패키지의 기본 저장소입니다. 기존 타사 패키지를 검색하려면 여기를 검색하십시오.

## `composer.json` 대 `composer.lock`

`composer.json` 파일에는 각 의존성에 대한 프로젝트의 유동적인 버전 기본 설정과 기타 정보가 저장됩니다.

`composer.lock` 파일에는 각 의존성에 대해 다운로드한 정확한 버전이 저장됩니다. 이 파일은 절대 편집하지 마십시오.

git 저장소에 `composer.lock` 파일을 포함하면 모든 개발자가 현재 사용 중인 버전의 의존성을 설치하게 됩니다. 의존성의 새 버전이 출시되어도 Composer는 잠금 파일에 기록된 버전을 계속 다운로드합니다.

```sh
# 모든 의존성을 버전 기본 설정과 여전히 일치하는 최신 버전으로 업데이트하려면
composer update

# 특정 의존성의 새 버전을 원하면:
composer update phpunit/phpunit

# 패키지를 최신 버전 기본 설정으로 마이그레이션하려면 이전 패키지와 해당 의존성을 먼저 제거해야 할 수 있습니다.
composer remove --dev phpunit/phpunit
composer require --dev phpunit/phpunit:^5.0
```

## 오토로더

Composer는 애플리케이션에서 require할 수 있는 오토로더 클래스를 만듭니다. 네임스페이스를 통해 클래스의 인스턴스를 만들 수 있습니다.

```php
require __DIR__ . '/vendor/autoload.php';

$mailgun = new Mailgun\Mailgun("key");
```

### PSR-4 오토로더

오토로더에 자신만의 네임스페이스를 추가할 수 있습니다.

`composer.json`에 'autoload' 필드를 추가합니다:

```json
{
  "autoload": {
    "psr-4": {"Acme\\": "src/"}
  }
}
```

이렇게 하면 오토로더가 `src` 폴더 내에서 `\Acme\` 네임스페이스의 모든 것을 찾도록 지시합니다.

또한 [PSR-0, 클래스맵 또는 포함할 파일 목록을 사용할 수 있습니다](https://getcomposer.org/doc/04-schema.md#autoload). 개발 전용 네임스페이스를 위한 `autoload-dev` 필드도 있습니다.

autoload 키를 추가하거나 수정할 때 오토로더를 다시 빌드해야 합니다:

```sh
composer dump-autoload
composer dump # 약어

# PSR0 및 PSR4 패키지를 클래스맵으로도 로드하도록 최적화합니다. 실행 속도는 느리지만 프로덕션 성능은 향상됩니다.
composer dump-autoload --optimize --no-dev
```

# Composer 캐시

```sh
# Composer는 나중에 사용할 다운로드한 패키지를 보관합니다. 다음으로 지웁니다:
composer clear-cache
```

# 문제 해결

```sh
composer diagnose
composer self-update
composer clear-cache
```

## 이 튜토리얼에서 (아직) 다루지 않은 주제

* Packagist.org 또는 다른 곳에서 자신만의 패키지 생성 및 배포
* 사전 및 사후 스크립트 후크: 특정 composer 이벤트가 발생할 때 작업 실행

### 참조

* [Composer - PHP용 의존성 관리자](https://getcomposer.org/)
* [Packagist.org](https://packagist.org/)
