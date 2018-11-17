---
category: tool
filename: learnshutit-de.html
tool: ShutIt
contributors:
    - ["Ian Miell", "http://ian.meirionconsulting.tk"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

## ShutIt

ShuIt ist eine Shellautomationsframework, welches für eine einfache 
Handhabung entwickelt wurde.

Er ist ein Wrapper, der auf einem Python expect Klon (pexpect) basiert.

Es ist damit ein 'expect ohne Schmerzen'.

Es ist verfügbar als pip install.

## Hello World

Starten wir mit dem einfachsten Beispiel. Erstelle eine Datei names example.py

```python

import shutit
session = shutit.create_session('bash')
session.send('echo Hello World', echo=True)
```

Führe es hiermit aus:

```bash
python example.py
```

gibt aus:

```bash
$ python example.py
echo "Hello World"
echo "Hello World"
Hello World
Ians-MacBook-Air.local:ORIGIN_ENV:RhuebR2T#
```

Das erste Argument zu 'send' ist der Befehl, den du ausführen möchtest.
Das 'echo' Argument gibt die Terminalinteraktion aus. ShuIt ist standardmäßig leise.

'Send' kümmert sich um die nervige Arbeiten mit den Prompts und macht 
alles was du von 'expect' erwarten würdest.


## Logge dich auf einen Server ein

Sagen wir du möchtest dich auf einen Server einloggen und einen Befehl ausführen.
Ändere dafür example.py folgendermaßen:

```python
import shutit
session = shutit.create_session('bash')
session.login('ssh you@example.com', user='du', password='meinpassword')
session.send('hostname', echo=True)
session.logout()
```

Dies erlaubt dir dich auf deinen Server einzuloggen
(ersetze die Details mit deinen Eigenen) und das Programm gibt dir deinen Hostnamen aus.

```
$ python example.py
hostname
hostname
example.com
example.com:cgoIsdVv:heDa77HB#
```


Es ist klar das das nicht sicher ist. Stattdessen kann man Folgendes machen:

```python
import shutit
session = shutit.create_session('bash')
password = session.get_input('', ispass=True)
session.login('ssh you@example.com', user='du', password=password)
session.send('hostname', echo=True)
session.logout()
```

Dies zwingt dich dein Passwort einzugeben:

```
$ python example.py
Input Secret:
hostname
hostname
example.com
example.com:cgoIsdVv:heDa77HB#
```


Die 'login' Methode übernimmt wieder das veränderte Prompt für den Login.
Du übergibst ShutIt den User und das Passwort, falls es benötigt wird,
mit den du dich einloggen möchtest. ShutIt übernimmt den Rest.

'logout' behandelt das Ende von 'login' und übernimmt alle Veränderungen des
Prompts für dich.

## Einloggen auf mehrere Server

Sagen wir, dass du eine Serverfarm mit zwei Servern hast und du dich in
beide Server einloggen möchtest. Dafür musst du nur zwei Session und 
Logins erstellen und kannst dann Befehle schicken:

```python
import shutit
session1 = shutit.create_session('bash')
session2 = shutit.create_session('bash')
password1 = session1.get_input('Password für server1', ispass=True)
password2 = session2.get_input('Password für server2', ispass=True)
session1.login('ssh you@one.example.com', user='du', password=password1)
session2.login('ssh you@two.example.com', user='du', password=password2)
session1.send('hostname', echo=True)
session2.send('hostname', echo=True)
session1.logout()
session2.logout()
```

Gibt aus:

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

## Beispiel: Überwachen mehrerer Server

Wir können das obige Programm in ein einfaches Überwachungstool bringen indem 
wir Logik hinzufügen um die Ausgabe von einem Befehl zu betrachten.

```python
import shutit
capacity_command="""df / | awk '{print $5}' | tail -1 | sed s/[^0-9]//"""
session1 = shutit.create_session('bash')
session2 = shutit.create_session('bash')
password1 = session.get_input('Passwort für Server1', ispass=True)
password2 = session.get_input('Passwort für Server2', ispass=True)
session1.login('ssh you@one.example.com', user='du', password=password1)
session2.login('ssh you@two.example.com', user='du', password=password2)
capacity = session1.send_and_get_output(capacity_command)
if int(capacity) < 10:
	print(kein Platz mehr auf Server1!')
capacity = session2.send_and_get_output(capacity_command)
if int(capacity) < 10:
	print(kein Platz mehr auf Server2!')
session1.logout()
session2.logout()
```

Hier kannst du die 'send\_and\_get\_output' Methode verwenden um die Ausgabe von dem 
Kapazitätsbefehl (df) zu erhalten.

Es gibt elegantere Wege als oben (z.B. kannst du ein Dictionary verwenden um über 
die Server zu iterieren), aber es hängt and dir wie clever das Python sein muss.


## kompliziertere IO - Expecting

Sagen wir du hast eine Interaktion mit einer interaktiven Kommandozeilenprogramm, 
die du automatisieren möchtest. Hier werden wir Telnet als triviales Beispiel verwenden:

```python
import shutit
session = shutit.create_session('bash')
session.send('telnet', expect='elnet>', echo=True)
session.send('open google.com 80', expect='scape character', echo=True)
session.send('GET /', echo=True, check_exit=False)
session.logout()
```

Beachte das 'expect' Argument. Du brauchst nur ein Subset von Telnets 
Eingabeaufforderung um es abzugleichen und fortzufahren.

Beachte auch das neue Argument 'check\_exit'. Wir werden nachher nochmal 
darauf zurückkommen. Die Ausgabe von oben ist:

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

Nun zurück zu 'check\_exit=False'. Da das Telnet Programm einen Fehler mit 
Fehlercode (1) zurückgibt und wir nicht möchten das das Skript fehlschlägt
kannst du 'check\_exit=False' setzen und damit ShuIt wissen lassen, dass 
der Ausgabecode dich nicht interessiert.

Wenn du das Argument nicht mitgegeben hättest, dann hätte dir ShutIt 
ein interaktives Terminal zurückgegeben, falls es ein Terminal zum
kommunizieren gibt. Dies nennt sich ein 'Pause point'.


## Pause Points

Du kannst jederzeit 'pause point' auslösen, wenn du Folgendes in deinem Skript aufrufst:

```python
[...]
session.pause_point('Das ist ein pause point')
[...]
```

Danach kannst du das Skript fortführen, wenn du CTRL und ']' zur selben Zeit drückst.
Dies ist gut für Debugging: Füge ein Pause Point hinzu und schaue dich um.
Danach kannst du das Programm weiter ausführen. Probiere folgendes aus:

```python
import shutit
session = shutit.create_session('bash')
session.pause_point('Schaue dich um!')
session.send('echo "Hat dir der Pause point gefallen?"', echo=True)
```

Dies würde folgendes ausgeben:

```bash
$ python example.py
Schaue dich um!

Ians-Air.home:ORIGIN_ENV:I00LA1Mq#  bash
imiell@Ians-Air:/space/git/shutit  ⑂ master +    
CTRL-] caught, continuing with run...
2017-06-05 15:12:33,577 INFO: Sending:  exit
2017-06-05 15:12:33,633 INFO: Output (squashed):  exitexitIans-Air.home:ORIGIN_ENV:I00LA1Mq#  [...]
echo "Hat dir der Pause point gefallen?"
echo "Hat dir der Pause point gefallen?"
Hat dir der Pause point gefallen?
Ians-Air.home:ORIGIN_ENV:I00LA1Mq#
```


## noch kompliziertere IO - Hintergrund

Kehren wir zu unseren Beispiel mit dem Überwachen von mehreren Servern zurück.
Stellen wir uns vor, dass wir eine langlaufende Aufgabe auf jedem Server durchführen möchten.
Standardmäßig arbeitet ShutIt seriell, was sehr lange dauern würde.
Wir können jedoch die Aufgaben im Hintergrund laufen lassen um sie zu beschleunigen.

Hier ist ein Beispiel, welches du ausprobieren kannst.
Es verwendet den trivialen Befehl: 'sleep'.


```python
import shutit
import time
long_command="""sleep 60"""
session1 = shutit.create_session('bash')
session2 = shutit.create_session('bash')
password1 = session1.get_input('Password for server1', ispass=True)
password2 = session2.get_input('Password for server2', ispass=True)
session1.login('ssh you@one.example.com', user='du', password=password1)
session2.login('ssh you@two.example.com', user='du', password=password2)
start = time.time()
session1.send(long_command, background=True)
session2.send(long_command, background=True)
print('Es hat: ' + str(time.time() - start) + ' Sekunden zum Starten gebraucht')
session1.wait()
session2.wait()
print('Es hat:' + str(time.time() - start) + ' Sekunden zum Vollenden gebraucht')
```

Mein Computer meint, dass er 0.5 Sekunden gebraucht hat um die Befehle zu starten 
und dann nur etwas über eine Minute gebraucht um sie zu beenden
(mit Verwendung der 'wait' Methode).


Das alles ist trivial, aber stelle dir vor das du hunderte an Servern so managen
kannst und man kann nun das Potential sehen, die in ein paar Zeilen Code und ein Python 
import liegen können.


## Lerne mehr

Es gibt noch viel mehr, was mit ShutIt erreicht werden kann.

Um mehr zu erfahren, siehe:

[ShutIt](https://ianmiell.github.io/shutit/)
[GitHub](https://github.com/ianmiell/shutit/blob/master/README.md)

Es handelt sich um ein breiteres Automatiesierungsframework, und das oben
genannte ist der sogennante 'standalone Modus'.

Feedback, feature requests, 'Wie mache ich es' sind herzlich willkommen! Erreiche mit unter
[@ianmiell](https://twitter.com/ianmiell)
