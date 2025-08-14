---
category: tool
name: Ansible
contributors:
    - ["Jakub Muszynski" , "http://github.com/sirkubax"]
    - ["Pat Myron" , "https://github.com/patmyron"]
    - ["Divay Prakash", "https://github.com/divayprakash"]
filename: LearnAnsible.txt
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

## 소개

```yaml
---
"{{ Ansible }}"은 Python으로 작성된 오케스트레이션 도구입니다.
...
```

Ansible은 (많은 것 중 하나인) 오케스트레이션 도구입니다.
환경(인프라 및 코드)을 제어하고 수동 작업을 자동화할 수 있습니다.

Ansible은 여러 운영 체제(Windows 포함) 및 일부 하드웨어(스위치, 방화벽 등)와 훌륭하게 통합됩니다. 클라우드 제공업체와 통합되는 여러 도구가 있습니다. 거의 모든 주목할 만한 클라우드 제공업체가 생태계에 존재합니다(AWS, Azure, Google, DigitalOcean, OVH 등).

하지만 ansible은 그 이상입니다! 실행 계획, API, 라이브러리 및 콜백을 제공합니다.

### 주요 장단점

#### 장점

* 에이전트 없는 도구입니다. 대부분의 시나리오에서 ssh를 전송 계층으로 사용합니다.
어떤 면에서는 '강화된 bash'로 사용할 수 있습니다.
* 시작하기가 매우 쉽습니다. ssh 개념에 익숙하다면 이미 Ansible을 알고 있는 것입니다(거의).
* '있는 그대로' 실행됩니다 - 다른 도구(salt, puppet, chef - 예상과 다른 시나리오에서 실행될 수 있음)
* 문서는 세계 최고 수준입니다!
* 자신만의 모듈과 확장을 작성하는 것은 상당히 쉽습니다.
* Ansible AWX는 우리가 기다려온 Ansible Tower의 오픈 소스 버전으로, 훌륭한 UI를 제공합니다.

#### 단점

* 에이전트 없는 도구입니다 - 모든 에이전트는 최대 16MB의 RAM을 소비합니다 - 일부 환경에서는 눈에 띄는 양일 수 있습니다.
* 에이전트가 없습니다 - 환경 일관성을 '온디맨드'로 확인해야 합니다 - 일부 변경 사항에 대해 자동으로 경고하는 내장 메커니즘이 없습니다(합리적인 노력으로 달성 가능).
* 공식 GUI - Ansible Tower -는 훌륭하지만 비쌉니다.
* '소규모 기업' 지불 계획은 없지만, Ansible AWX는 우리 모두가 기다려온 무료 오픈 소스 버전입니다.

#### 중립

마이그레이션 - Ansible <-> Salt는 상당히 쉽습니다 - 따라서 이벤트 기반 에이전트 환경이 필요한 경우 Ansible로 빠르게 시작하고 필요할 때 Salt로 변환하는 것이 좋은 선택이 될 것입니다.

#### 일부 개념

Ansible은 ssh 또는 paramiko를 전송 계층으로 사용합니다. 어떤 면에서는 작업을 수행하기 위해 API와 함께 ssh를 사용한다고 상상할 수 있습니다. 가장 간단한 방법은 더 제어된 방식으로 원격 명령을 실행하는 것입니다(여전히 ssh 사용).
반면에 고급 범위에서는 자신의 Python 스크립트로 Ansible을 래핑(Python Ansible 코드를 라이브러리로 사용)할 수 있습니다! 그러면 Fabric처럼 작동합니다.

## 예제

apache를 설치하고 로그 수준을 구성하는 예제 플레이북

```yaml
---
- hosts: apache

  vars:
      apache2_log_level: "warn"

  handlers:
  - name: restart apache
    service:
      name: apache2
      state: restarted
      enabled: True
    notify:
      - Wait for instances to listen on port 80
    become: True

  - name: reload apache
    service:
      name: apache2
      state: reloaded
    notify:
      - Wait for instances to listen on port 80
    become: True

  - name: Wait for instances to listen on port 80
    wait_for:
      state: started
      host: localhost
      port: 80
      timeout: 15
      delay: 5

  tasks:
  - name: Update cache
    apt:
      update_cache: yes
      cache_valid_time: 7200
    become: True

  - name: Install packages
    apt:
      name={{ item }}
    with_items:
      - apache2
      - logrotate
    notify:
      - restart apache
    become: True

  - name: Configure apache2 log level
    lineinfile:
      dest: /etc/apache2/apache2.conf
      line: "LogLevel {{ apache2_log_level }}"
      regexp: "^LogLevel"
    notify:
      - reload apache
    become: True
...
```

## 설치

```bash
# 범용 방법
$ pip install ansible

# Debian, Ubuntu
$ apt-get install ansible
```

* [부록 A - ansible을 어떻게 설치하나요?](#infrastructure-as-a-code)
* [추가 자료.](http://docs.ansible.com/ansible/latest/intro_installation.html)

### 첫 번째 ansible 명령 (셸 실행)

```bash
# 명령이 localhost를 ping합니다 (기본 인벤토리: /etc/ansible/hosts에 정의됨)
$ ansible -m ping localhost
# 이 출력이 표시되어야 합니다
localhost | SUCCESS => {
    "changed": false,
    "ping": "pong"
}
```

### 셸 명령

알아야 할 몇 가지 명령이 있습니다

* `ansible` (CLI에서 모듈 실행)
* `ansible-playbook` (플레이북 실행)
* `ansible-vault` (비밀 관리)
* `ansible-galaxy` (github/galaxy에서 역할 설치)

### 모듈

실행되고, 일부 작업을 수행하고, 적절한 JSON 출력을 반환하는 프로그램(보통 python)입니다. 이 프로그램은 전문화된 작업/액션(클라우드에서 인스턴스 관리, 셸 명령 실행 등)을 수행합니다. 가장 간단한 모듈은 `ping`이라고 하며, `pong` 메시지가 있는 JSON을 반환합니다.

모듈 예제:

* 모듈: `ping` - 호스트 연결을 확인하는 데 유용한 가장 간단한 모듈
* 모듈: `shell` - 지정된 호스트에서 셸 명령을 실행하는 모듈.


```bash
$ ansible -m ping all
$ ansible -m shell -a 'date; whoami' localhost #hostname_or_a_group_name
```

* 모듈: `command` - 셸을 통해 처리되지 않는 단일 명령을 실행하므로 `$HOME`과 같은 변수나 ``|` `;``와 같은 피연산자는 작동하지 않습니다. command 모듈은 사용자의 환경에 영향을 받지 않으므로 더 안전합니다. 더 복잡한 명령의 경우 - 셸 모듈을 사용하십시오.

```bash
$ ansible -m command -a 'date; whoami' # 실패
$ ansible -m command -a 'date' all
$ ansible -m command -a 'whoami' all
```

* 모듈: `file` - 파일 작업 수행 (stat, link, dir, ...)
* 모듈: `raw` - 모듈 하위 시스템을 거치지 않고 저수준의 더러운 SSH 명령을 실행합니다 (python2.7 설치에 유용).

### 작업

단일 Ansible **모듈**의 실행을 **작업**이라고 합니다. 위에서 볼 수 있듯이 가장 간단한 모듈은 `ping`이라고 합니다.

여러 리소스에서 원격으로 명령을 실행할 수 있는 또 다른 모듈 예제는 `shell`입니다. 위에서 이미 사용한 방법을 참조하십시오.

### 플레이북

스크립트 파일 형식으로 작성된 **실행 계획**을 **플레이북**이라고 합니다.
플레이북은 여러 요소로 구성됩니다 -
* '플레이'가 실행되는 호스트 목록(또는 그룹)
* 실행될 `작업` 또는 `역할`
* 여러 선택적 설정 (기본 변수 등)

플레이북 스크립트 언어는 YAML입니다. 플레이북은 실행하는 매우 고급 CLI 스크립트라고 생각할 수 있습니다.

#### 플레이북 예제

이 예제 플레이북은 (인벤토리에 정의된 모든 호스트에서) 두 가지 작업을 실행합니다:
* 메시지 *pong*을 반환하는 `ping`
* 세 가지 명령을 실행하고 출력을 터미널로 반환하는 `shell`

```yaml
- hosts: all

  tasks:
    - name: "ping all"
      ping:

    - name: "execute a shell command"
      shell: "date; whoami; df -h;"
```

다음 명령으로 플레이북을 실행합니다:

```bash
$ ansible-playbook path/name_of_the_playbook.yml
```

참고: 예제 플레이북은 다음 장인 '역할'에서 설명합니다.

### ansible 개념에 대한 추가 정보

### 인벤토리

인벤토리는 플레이북 또는 셸 명령을 통해 단일 작업을 실행하는 대상 개체 또는 호스트 집합입니다. 이 몇 분 동안은 기본 ansible 인벤토리(Debian 기반 시스템에서는 `/etc/ansible/hosts`에 있음)를 사용한다고 가정해 보겠습니다.

```
localhost

[some_group]
hostA.mydomain.com
hostB.localdomain
1.2.3.4

[a_group_of_a_groups:children]
some_group
some_other_group
```

* [추가 자료.](http://docs.ansible.com/ansible/latest/intro_inventory.html)

### ansible-roles ('올바른 구조를 가진 템플릿 플레이북')

이미 CLI를 통해 작업(모듈)을 실행할 수 있다는 것을 알고 있습니다. 또한 여러 작업(변수 및 논리가 있는)의 실행 계획인 플레이북도 알고 있습니다.

재사용해야 하는 코드 부분(플레이북)에 대해 `역할`이라는 개념이 도입되었습니다.

**역할**은 작업, 변수, 핸들러, 기본 설정 등을 관리하는 구조화된 방법입니다(메타, 파일, 템플릿). 역할은 여러 플레이북에서 동일한 코드 부분을 재사용할 수 있도록 합니다(실행 중에 역할을 '추가로' 매개변수화할 수 있음). 애플리케이션에 대한 `객체 지향` 관리를 도입하는 좋은 방법입니다.

역할은 플레이북에 포함될 수 있습니다(플레이북을 통해 실행됨).


```yaml
- hosts: all

  tasks:
      - name: "ping all"
        ping:
      - name: "execute a shell command"
        shell: "date; whoami; df -h;"

  roles:
      - some_role
      - { role: another_role, some_variable: 'learnxiny', tags: ['my_tag'] }

  pre_tasks:
      - name: some pre-task
        shell: echo 'this task is the last, but would be executed before roles, and before tasks'
```

#### 나머지 예제에서는 추가 리포지토리를 사용합니다
이 예제는 `virtualenv`에 ansible을 설치하므로 시스템과 독립적입니다.
`source environment.sh` 명령으로 셸 컨텍스트에 초기화해야 합니다.

다음 예제 리포지토리를 사용합니다: [https://github.com/sirkubax/ansible-for-learnXinYminutes](https://github.com/sirkubax/ansible-for-learnXinYminutes)

```bash
$ # 다음 예제에는 venv 및 상대 경로를 나타내는 셸 프롬프트가 포함되어 있습니다.
$ git clone git@github.com:sirkubax/ansible-for-learnXinYminutes.git
user@host:~/$ cd ansible-for-learnXinYminutes
user@host:~/ansible-for-learnXinYminutes$ source environment.sh
$
$ # 먼저 simple_playbook.yml을 실행해 보겠습니다.
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/simple_playbook.yml
```

역할 예제로 플레이북 실행

```bash
$ source environment.sh
$ # 이제 역할이 있는 위 플레이북을 실행합니다.
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/simple_role.yml
```

#### 역할 디렉토리 구조

```
roles/
   some_role/
     defaults/      # 기본 변수 포함
     files/         # 정적 파일용
     templates/     # jinja 템플릿용
     tasks/         # 작업
     handlers/      # 핸들러
     vars/          # 더 많은 변수 (우선 순위 높음)
     meta/          # 메타 - 패키지 (역할) 정보
```

#### 역할 핸들러
핸들러는 플레이북 실행 중에 트리거(알림)될 수 있는 작업이지만, 플레이북의 맨 끝에서 실행됩니다. 서비스를 다시 시작하거나, 애플리케이션 포트가 활성 상태인지 확인(성공적인 배포 기준)하는 등의 가장 좋은 방법입니다.

simple_apache_role 예제에서 역할을 사용하는 방법을 숙지하십시오.

```
playbooks/roles/simple_apache_role/
├── tasks
│   └── main.yml
└── templates
    └── main.yml
```

### ansible - 변수

Ansible은 유연합니다 - 21단계의 변수 우선 순위가 있습니다.
[더 읽기](http://docs.ansible.com/ansible/latest/playbooks_variables.html#variable-precedence-where-should-i-put-a-variable)
지금은 CLI 변수가 최우선 순위를 갖는다는 것을 알아야 합니다.
또한 일부 데이터를 풀링하는 좋은 방법은 **조회**라는 것을 알아야 합니다.

### 조회
다양한 소스에서 데이터를 쿼리하는 멋진 도구!!! 멋지다!
쿼리 대상:
* 파이프 (셸 명령 출력을 변수로 로드!)
* 파일
* 스트림
* etcd
* 암호 관리 도구
* url

```bash
# playbooks/lookup.yml 읽기
# 그런 다음 실행
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/lookup.yml
```

CLI에서도 사용할 수 있습니다.

```yaml
ansible -m shell -a 'echo "{{ my_variable }}"' -e 'my_variable="{{ lookup("pipe", "date") }}"' localhost
ansible -m shell -a 'echo "{{ my_variable }}"' -e 'my_variable="{{ lookup("pipe", "hostname") }}"' all

# 또는 플레이북에서 사용

(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/lookup.yml
```

### 등록 및 조건부

#### 등록

변수 내용을 동적으로 생성하는 또 다른 방법은 `register` 명령입니다.
`Register`는 작업의 출력을 저장하고 해당 값을 사용하여 추가 작업을 실행하는 데에도 유용합니다.

```
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/register_and_when.yml
```

```yaml
---
- hosts: localhost
  tasks:
   - name: check the system capacity
     shell: df -h /
     register: root_size

   - name: debug root_size
     debug:
        msg: "{{ root_size }}"

   - name: debug root_size return code
     debug:
       msg:  "{{ root_size.rc }}"

# when: 예제

   - name: Print this message when return code of 'check the system capacity' was ok
     debug:
       msg:  "{{ root_size.rc }}"
     when: root_size.rc == 0
...
```

#### 조건부 - when:

Ansible 및 Jinja 함수를 사용하여 복잡한 논리를 정의할 수 있습니다. 가장 일반적인 것은 `when:`을 일부 변수(종종 이전 플레이북 단계에서 `register` 또는 `lookup`으로 동적으로 생성됨)와 함께 사용하는 것입니다.

```yaml
---
- hosts: localhost
  tasks:
   - name: check the system capacity
     shell: df -h /
     when: some_variable in 'a string'
  roles:
   - { role: mid_nagios_probe, when: allow_nagios_probes }
...
```

### ansible - 태그, 제한

이 간단한 기능으로 효율성을 높이는 방법에 대해 알아야 합니다.

#### 태그

작업, 역할(및 해당 작업), 포함 등을 태그 지정한 다음 태그가 지정된 리소스만 실행할 수 있습니다.

```
ansible-playbook playbooks/simple_playbook.yml --tags=tagA,tag_other
ansible-playbook playbooks/simple_playbook.yml -t tagA,tag_other

특수 태그가 있습니다:
    always

--skip-tags를 사용하여 코드 블록을 제외할 수 있습니다.
--list-tags를 사용하여 사용 가능한 태그를 나열합니다.
```

[더 읽기](http://docs.ansible.com/ansible/latest/playbooks_tags.html)

#### 제한

작업 실행을 정의된 호스트로 제한할 수 있습니다.

```
ansible-playbook playbooks/simple_playbook.yml --limit localhost

--limit my_hostname
--limit groupname
--limit some_prefix*
--limit hostname:group #JM
```

### 템플릿

템플릿은 일부 (부분적으로) 동적 콘텐츠를 제공하는 강력한 방법입니다.
Ansible은 **Jinja2** 언어를 사용하여 템플릿을 설명합니다.

```
일부 정적 콘텐츠

{{ a_variable }}

{% for item in loop_items %}
    이 줄 항목은 {{ item }}입니다.
{% endfor %}
```

Jinja에는 몇 가지 제한 사항이 있을 수 있지만, 좋아할 만한 강력한 도구입니다.

apache2를 설치하고 템플릿에서 index.html을 생성하는 이 간단한 예제를 검토하십시오.
"playbooks/roles/simple_apache_role/templates/index.html"

```bash
$ source environment.sh
$ # 이제 역할이 있는 위 플레이북을 실행합니다.
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/simple_role.yml --tags apache2
```

#### Jinja2 CLI

CLI에서도 jinja를 사용할 수 있습니다.

```bash
ansible -m shell -a 'echo {{ my_variable }}' -e 'my_variable=something, playbook_parameter=twentytwo' localhost
```

사실 - jinja는 플레이북의 일부를 템플릿화하는 데에도 사용됩니다.

```yaml
# 이 플레이북의 일부 확인: playbooks/roles/sys_debug/tasks/debug_time.yml
- local_action: shell date +'%F %T'
  register: ts
  become: False
  changed_when: False

- name: Timestamp
  debug: msg="{{ ts.stdout }}"
  when: ts is defined and ts.stdout is defined
  become: False
```

#### Jinja2 필터

Jinja는 강력합니다. 유용한 내장 함수가 많이 있습니다.

```
# 목록의 첫 번째 항목 가져오기
{{ some_list | first() }}
# 변수가 정의되지 않은 경우 - 기본값 사용
{{ some_variable | default('default_value') }}
```

[더 읽기](http://docs.ansible.com/ansible/latest/playbooks_filters.html)

### ansible-vault

**코드로 인프라**를 유지하려면 비밀을 저장해야 합니다. Ansible은 기밀 파일을 암호화하는 방법을 제공하므로 리포지토리에 저장할 수 있지만, 파일은 ansible 실행 중에 즉시 해독됩니다.

가장 좋은 방법은 비밀을 안전한 위치에 저장하고 런타임에 사용하도록 ansible을 구성하는 것입니다.

```bash
# 시도 (실패함)
$ ansible-playbook playbooks/vault_example.yml

$ echo some_very_very_long_secret > ~/.ssh/secure_located_file

# ansible.cfg에서 비밀 파일 경로 설정
$ vi ansible.cfg
  ansible_vault_password_file = ~/.ssh/secure_located_file

# 또는 env 사용
$ export ANSIBLE_VAULT_PASSWORD_FILE=~/.ssh/secure_located_file

$ ansible-playbook playbooks/vault_example.yml

  # 파일 암호화
$ ansible-vault encrypt path/somefile

  # 파일 보기
$ ansible-vault view path/somefile

  # 파일 내용 확인:
$ cat path/somefile

  # 파일 해독
$ ansible-vault decrypt path/somefile
```

### 동적 인벤토리

인벤토리를 동적으로 빌드할 수 있다는 것을 좋아할 것입니다.
(Ansible의 경우) 인벤토리는 적절한 구조를 가진 JSON일 뿐입니다 - ansible에 전달할 수 있다면 무엇이든 가능합니다.

바퀴를 다시 발명할 필요가 없습니다 - 가장 인기 있는 클라우드 제공업체 및 많은 사내 인기 사용 사례에 대한 즉시 사용 가능한 인벤토리 스크립트가 많이 있습니다.

[AWS 예제](http://docs.ansible.com/ansible/latest/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script)

```bash
$ etc/inv/ec2.py --refresh
$ ansible -m ping all -i etc/inv/ec2.py
```

[더 읽기](http://docs.ansible.com/ansible/latest/intro_dynamic_inventory.html)

### ansible 프로파일링 - 콜백

플레이북 실행에는 시간이 걸립니다. 괜찮습니다. 먼저 실행되도록 한 다음 속도를 높이고 싶을 수 있습니다. ansible 2.x부터 작업 실행 프로파일링을 위한 내장 콜백이 있습니다.

```
vi ansible.cfg
# 이것을 다음으로 설정:
callback_whitelist = profile_tasks
```

### facts-cache 및 ansible-cmdb

다른 호스트에서 환경에 대한 일부 정보를 가져올 수 있습니다. 정보가 변경되지 않으면 facts_cache를 사용하여 속도를 높이는 것을 고려할 수 있습니다.

```
vi ansible.cfg

# 영구 유형('memory'가 아닌, 예를 들어 'redis')으로 설정하면 이전 Ansible 실행의 팩트 값이 저장됩니다. 예를 들어, 현재 IP 정보를 얻기 위해 동일한 플레이북 실행에서 통신할 필요 없이 한 그룹의 서버에서 IP 정보를 사용하려는 경우 유용할 수 있습니다.
fact_caching = jsonfile
fact_caching_connection = ~/facts_cache
fact_caching_timeout = 86400
```

백엔드로 `jsonfile`을 사용하는 것을 좋아합니다. 인벤토리 리소스의 HTML 페이지를 생성하는 다른 프로젝트 `ansible-cmdb` [(GitHub 프로젝트)](https://github.com/fboender/ansible-cmdb)를 사용할 수 있습니다. 좋은 '무료' 추가 기능입니다!

### ansible 디버깅 [진행 중인 장]

작업이 실패하면 디버깅에 효과적이어야 합니다.

1. 여러 -v **[ -vvvvv]**를 사용하여 상세도를 높입니다.
2. 변수가 정의되지 않은 경우 -
`grep -R path_of_your_inventory -e missing_variable`
3. 변수(사전 또는 목록)가 정의되지 않은 경우 -
`grep -R path_of_your_inventory -e missing_variable`
4. Jinja 템플릿 디버그
5. 이상한 동작 - '대상에서' 코드를 실행해 보십시오.

### 코드로 인프라

ansible-vault를 사용하면 기밀 데이터를 코드와 함께 저장할 수 있다는 것을 이미 알고 있습니다. 더 나아가 ansible 설치 및 구성을 코드로 정의할 수 있습니다. `environment.sh`를 참조하여 운영 체제에 연결되지 않은 `virtualenv` 내에 ansible 자체를 설치하는 방법을 알아보십시오(권한 없는 사용자가 변경할 수 있음). 추가적인 이점으로 ansible 버전 업그레이드는 새 virtualenv에 새 버전을 설치하는 것만큼 쉽습니다. 또한 여러 버전의 Ansible을 동시에 사용할 수 있습니다.

```bash
# ansible 2.x venv 다시 만들기
$ rm -rf venv2
$ source environment2.sh

# 플레이북 실행
(venv2)$ ansible-playbook playbooks/ansible1.9_playbook.yml # 실패함 - 더 이상 사용되지 않는 구문

# 이제 ansible 2.x 옆에 ansible 1.9.x 설치
(venv2)$ deactivate
$ source environment.1.9.sh

# 플레이북 실행
(venv1.9)$ ansible-playbook playbooks/ansible1.9_playbook.yml # 작동함!

# venv1.9와 venv2가 모두 존재한다는 점에 유의하십시오 - 하나를 (비)활성화해야 합니다 - 그게 전부입니다.
```

#### become-user, become

Ansible에서 `sudo`가 되려면 `become` 매개변수를 사용하십시오. 사용자 이름을 지정하려면 `become_user`를 사용하십시오.

```
- name: Ensure the httpd service is running
  service:
    name: httpd
    state: started
  become: true
```

참고: '관리자' 권한이 필요한 경우 감독되지 않은 실행을 허용하려면 `--ask-sudo-pass`로 Ansible을 실행하거나 sudoers 파일에 사용자를 추가해야 할 수 있습니다.

[더 읽기](http://docs.ansible.com/ansible/latest/become.html)

## 팁과 요령

#### --check -C

플레이북이 '드라이 런' 모드(--check)에서 실행될 수 있고, 실행 시 '변경된' 개체를 선언하지 않는지 항상 확인하십시오.

#### --diff -D

Diff는 변경된 파일의 세부 정보를 보기 위해 유용합니다.
`diff -BbruN fileA fileB`와 같이 '메모리에서' 파일을 비교합니다.


#### '정규식'으로 호스트 실행

```bash
ansible -m ping web*
```

#### 호스트 그룹은 결합, 부정 등이 가능합니다.

```bash
ansible -m ping web*:!backend:monitoring:&allow_change
```

#### 태그 지정

일부(전부는 아님) 개체(플레이북의 작업, 역할에서 포함된 모든 작업 등)를 태그 지정해야 합니다. 플레이북의 선택된 부분을 실행할 수 있습니다.

#### no_logs: True

일부 역할은 상세 모드에서 많은 출력을 인쇄하는 것을 볼 수 있습니다. 디버그 모듈도 있습니다. 자격 증명이 유출될 수 있는 곳입니다. 출력을 숨기려면 `no_log`를 사용하십시오.

#### 디버그 모듈

화면에 값을 인쇄할 수 있습니다 - 사용하십시오!

#### 작업의 출력 등록

`register` 명령으로 작업의 출력(stdout), rc(반환 코드), stderr을 등록할 수 있습니다.

#### 조건부: when:

#### 루프: with, with_items, with_dict, with_together

[더 읽기](http://docs.ansible.com/ansible/latest/playbooks_conditionals.html)

## 추가 자료

* [해커를 위한 서버: Ansible 튜토리얼](https://serversforhackers.com/c/an-ansible-tutorial)
* [시스템 관리자를 위한 Ansible 시작 가이드 - FAST!](https://www.redhat.com/en/blog/system-administrators-guide-getting-started-ansible-fast)
* [Ansible Tower](https://www.ansible.com/products/tower) - Ansible Tower는 ansible에 대한 웹 UI, 대시보드 및 나머지 인터페이스를 제공합니다.
* [Ansible AWX](https://github.com/ansible/awx) - Ansible Tower의 오픈 소스 버전.
* [초보자를 위한 Ansible 튜토리얼: 궁극의 플레이북 및 예제](https://spacelift.io/blog/ansible-tutorial)