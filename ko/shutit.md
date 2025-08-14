---
category: framework
name: ShutIt
contributors:
    - ["Ian Miell", "http://ian.meirionconsulting.tk"]
filename: learnshutit.py
---

ShutIt은 사용하기 쉽게 설계된 셸 자동화 프레임워크입니다.

Python 기반 expect 클론(pexpect)을 감싸는 래퍼입니다.

'고통 없는 expect'로 볼 수 있습니다.

pip 설치로 사용할 수 있습니다.

## Hello World

가장 간단한 예부터 시작하겠습니다. example.py라는 파일을 만드십시오:

```python
import shutit
session = shutit.create_session('bash')
session.send('echo Hello World', echo=True)
```

다음과 같이 실행합니다:

```bash
python example.py
```

출력:

```bash
$ python example.py
echo "Hello World"
echo "Hello World"
Hello World
Ians-MacBook-Air.local:ORIGIN_ENV:RhuebR2T#
```

'send'의 첫 번째 인수는 실행하려는 명령어입니다. 'echo'
인수는 터미널 상호 작용을 출력합니다. 기본적으로 ShutIt은 조용합니다.

'send'는 expect에서 익숙할 수 있는 프롬프트와 'expect'에 대한 모든
혼란스러운 부분을 처리합니다.

## 서버에 로그인

서버에 로그인하여 명령어를 실행하고 싶다고 가정해 봅시다. example.py를
다음과 같이 변경하십시오:

```python
import shutit
session = shutit.create_session('bash')
session.login('ssh you@example.com', user='you', password='mypassword')
session.send('hostname', echo=True)
session.logout()
```

이렇게 하면 서버에 로그인하고(세부 정보를 바꾸면)
호스트 이름을 출력합니다.

```
$ python example.py
hostname
hostname
example.com
example.com:cgoIsdVv:heDa77HB#
```

물론 이것은 안전하지 않습니다! 대신 다음을 실행할 수 있습니다:

```python
import shutit
session = shutit.create_session('bash')
password = session.get_input('', ispass=True)
session.login('ssh you@example.com', user='you', password=password)
session.send('hostname', echo=True)
session.logout()
```

이렇게 하면 암호를 입력해야 합니다:

```
$ python example.py
Input Secret:
hostname
hostname
example.com
example.com:cgoIsdVv:heDa77HB#
```

다시 말하지만, 'login' 메서드는 로그인에서 변경되는 프롬프트를 처리합니다.
ShutIt에 로그인 명령어, 로그인할 것으로 예상되는 사용자, 암호(필요한 경우)를
제공하면 ShutIt이 나머지를 처리합니다.

'logout'은 'login'의 종료를 처리하며, 프롬프트 변경 사항을
처리합니다.

## 여러 서버에 로그인

두 개의 서버로 구성된 서버 팜이 있고 둘 다에 로그인하고 싶다고 가정해 봅시다.
두 개의 세션을 만들고 유사한 로그인 및 전송 명령어를 실행하기만 하면 됩니다:

```python
import shutit
session1 = shutit.create_session('bash')
session2 = shutit.create_session('bash')
password1 = session1.get_input('Password for server1', ispass=True)
password2 = session2.get_input('Password for server2', ispass=True)
session1.login('ssh you@one.example.com', user='you', password=password1)
session2.login('ssh you@two.example.com', user='you', password=password2)
session1.send('hostname', echo=True)
session2.send('hostname', echo=True)
session1.logout()
session2.logout()
```

다음과 같이 출력됩니다:

```bash
$ python example.py
Password for server1
Input Secret:

Password for server2
Input Secret:
hostname
hostname
one.example.com
one.example.com:Fnh2pyFj:qkrsmUNs# hostname
hostname
two.example.com
two.example.com:Gl2lldEo:D3FavQjA#
```

## 예: 여러 서버 모니터링

위의 내용을 명령어 출력을 검사하는 일부 논리를 추가하여
간단한 모니터링 도구로 바꿀 수 있습니다:

```python
import shutit
capacity_command="""df / | awk '{print $5}' | tail -1 | sed s/[^0-9]//"""
session1 = shutit.create_session('bash')
session2 = shutit.create_session('bash')
password1 = session.get_input('Password for server1', ispass=True)
password2 = session.get_input('Password for server2', ispass=True)
session1.login('ssh you@one.example.com', user='you', password=password1)
session2.login('ssh you@two.example.com', user='you', password=password2)
capacity = session1.send_and_get_output(capacity_command)
if int(capacity) < 10:
	print('RUNNING OUT OF SPACE ON server1!')
capacity = session2.send_and_get_output(capacity_command)
if int(capacity) < 10:
	print('RUNNING OUT OF SPACE ON server2!')
session1.logout()
session2.logout()
```

여기서는 'send\_and\_get\_output' 메서드를 사용하여 용량
명령어(df)의 출력을 검색합니다.

위의 작업을 수행하는 훨씬 더 우아한 방법이 있습니다(예:
반복할 서버의 사전을 갖는 것). 하지만 Python이 얼마나
똑똑해야 하는지는 여러분에게 달려 있습니다.

## 더 복잡한 IO - 예상

자동화하려는 대화형 명령줄 애플리케이션과의
상호 작용이 있다고 가정해 봅시다. 여기서는 telnet을 간단한 예로 사용합니다:

```python
import shutit
session = shutit.create_session('bash')
session.send('telnet', expect='elnet>', echo=True)
session.send('open google.com 80', expect='scape character', echo=True)
session.send('GET /', echo=True, check_exit=False)
session.logout()
```

'expect' 인수를 참고하십시오. 계속하려면 telnet의 프롬프트 일부만
제공하면 됩니다.

또한 위의 'check\_exit' 인수도 새로운 것입니다. 나중에 다시
살펴보겠습니다. 위의 출력은 다음과 같습니다:

```bash
$ python example.py
telnet
telnet> open google.com 80
Trying 216.58.214.14...
Connected to google.com.
Escape character is '^]'.
GET /
HTTP/1.0 302 Found
Cache-Control: private
Content-Type: text/html; charset=UTF-8
Referrer-Policy: no-referrer
Location: http://www.google.co.uk/?gfe_rd=cr&ei=huczWcj3GfTW8gfq0paQDA
Content-Length: 261
Date: Sun, 04 Jun 2017 10:57:10 GMT

<HTML><HEAD><meta http-equiv="content-type" content="text/html;charset=utf-8">
<TITLE>302 Moved</TITLE></HEAD><BODY>
<H1>302 Moved</H1>
The document has moved
<A HREF="http://www.google.co.uk/?gfe_rd=cr&amp;ei=huczWcj3GfTW8gfq0paQDA">
here
</A>.
</BODY></HTML>
Connection closed by foreign host.
```

이제 'check\_exit=False'로 돌아가겠습니다. telnet 명령어가 실패 종료
코드(1)를 반환하고 스크립트가 실패하지 않도록 하려면 'check\_exit=False'를
설정하여 ShutIt에 종료 코드에 신경 쓰지 않는다는 것을 알립니다.

해당 인수를 전달하지 않으면 ShutIt은 통신할 터미널이 있는 경우
대화형 터미널을 제공합니다. 이것을 '일시 중지 지점'이라고 합니다.

## 일시 중지 지점

다음을 호출하여 언제든지 '일시 중지 지점'을 트리거할 수 있습니다.

```python
[...]
session.pause_point('This is a pause point')
[...]
```

스크립트 내에서 CTRL과 ']'를 동시에 눌러 스크립트를 계속 진행합니다.
이것은 디버깅에 좋습니다: 일시 중지 지점을 추가하고, 둘러본 다음,
계속하십시오. 이것을 시도해 보십시오:

```python
import shutit
session = shutit.create_session('bash')
session.pause_point('Have a look around!')
session.send('echo "Did you enjoy your pause point?"', echo=True)
```

다음과 같은 출력으로:

```bash
$ python example.py
Have a look around!

Ians-Air.home:ORIGIN_ENV:I00LA1Mq#  bash
imiell@Ians-Air:/space/git/shutit  ⑂ master +  
CTRL-] caught, continuing with run...
2017-06-05 15:12:33,577 INFO: Sending:  exit
2017-06-05 15:12:33,633 INFO: Output (squashed):  exitexitIans-Air.home:ORIGIN_ENV:I00LA1Mq#  [...]
echo "Did you enjoy your pause point?"
echo "Did you enjoy your pause point?"
Did you enjoy your pause point?
Ians-Air.home:ORIGIN_ENV:I00LA1Mq#
```

## 더 복잡한 IO - 백그라운드 작업

'여러 서버 모니터링' 예제로 돌아가서, 각 서버에서
실행하려는 장기 실행 작업이 있다고 상상해 봅시다. 기본적으로 ShutIt은
직렬로 작동하므로 시간이 오래 걸립니다. 하지만 백그라운드에서 작업을
실행하여 속도를 높일 수 있습니다.

여기서는 간단한 명령어: 'sleep 60'으로 예를 들어 볼 수 있습니다.

```python
import shutit
import time
long_command="""sleep 60"""
session1 = shutit.create_session('bash')
session2 = shutit.create_session('bash')
password1 = session1.get_input('Password for server1', ispass=True)
password2 = session2.get_input('Password for server2', ispass=True)
session1.login('ssh you@one.example.com', user='you', password=password1)
session2.login('ssh you@two.example.com', user='you', password=password2)
start = time.time()
session1.send(long_command, background=True)
session2.send(long_command, background=True)
print('That took: ' + str(time.time() - start) + ' seconds to fire')
session1.wait()
session2.wait()
print('That took: ' + str(time.time() - start) + ' seconds to complete')
```

제 노트북은 두 명령어를 실행하는 데 0.5초가 걸렸고, 그 다음
1분 이상 걸려 완료되었습니다('wait' 메서드 사용).

다시 말하지만, 이것은 사소하지만, 수백 대의 서버를 이와 같이
관리해야 한다고 상상해 보십시오. 그러면 몇 줄의 코드와 하나의
Python 가져오기로 얼마나 강력한 힘을 발휘할 수 있는지 알 수 있습니다.

## 더 알아보기

ShutIt으로 할 수 있는 일은 훨씬 더 많습니다.

더 알아보려면 다음을 참조하십시오:

[ShutIt](https://ianmiell.github.io/shutit/)
[GitHub](https://github.com/ianmiell/shutit/blob/master/README.md)

이것은 더 넓은 자동화 프레임워크이며, 위의 내용은 '독립 실행형 모드'입니다.
