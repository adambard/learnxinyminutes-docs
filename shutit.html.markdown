---
category: tool
tool: ShutIt
contributors:
    - ["Ian Miell", "http://ian.meirionconsulting.tk"]
filename: learnshutit.html
---

## ShutIt

ShutIt is an shell automation framework designed to be easy to use.

It is a wrapper around a Python-based expect clone (pexpect).

You can look at it as 'expect without the pain'.

It is available as a pip install.

## Hello World

Starting with the simplest example. Create a file called example.py:

```python
import shutit
session = shutit.create_session('bash')
session.send('echo Hello World', echo=True)
```

Running this with:

```bash
python example.py
```

outputs:

```bash
$ python example.py
echo "Hello World"
echo "Hello World"
Hello World
Ians-MacBook-Air.local:ORIGIN_ENV:RhuebR2T#
```

The first argument to 'send' is the command you want to run. The 'echo'
argument outputs the terminal interactions. By default ShutIt is silent.

'send' takes care of all the messing around with prompts and 'expects' that
you might be familiar with from expect.


## Log Into a Server

Let's say you want to log into a server and run a command. Change example.py
to:

```python
import shutit
session = shutit.create_session('bash')
session.login('ssh you@example.com', user='you', password='mypassword')
session.send('hostname', echo=True)
session.logout()
```

which will log you into your server (if you replace with your details) and
output the hostname.

```
$ python example.py
hostname
hostname
example.com
example.com:cgoIsdVv:heDa77HB#
```

Obviously that's insecure! Instead you can run:

```python
import shutit
session = shutit.create_session('bash')
password = session.get_input('', ispass=True)
session.login('ssh you@example.com', user='you', password=password)
session.send('hostname', echo=True)
session.logout()
```

which forces you to input the password:

```
$ python example.py
Input Secret:
hostname
hostname
example.com
example.com:cgoIsdVv:heDa77HB#
```

Again, the 'login' method handles the changing prompt from a login. You give
ShutIt the login command, the user you expect to log in as, and a password
(if needed), and ShutIt takes care of the rest.

'logout' handles the ending of a 'login', handling any changes to the prompt
for you.

## Log Into Multiple Servers

Let's say you have a server farm of two servers, and want to log onto both.
Just create two sessions and run similar login and send commands:

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

would output:

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

## Example: Monitor Multiple Servers

We can turn the above into a simple monitoring tool by adding some logic to
examine the output of a command:

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

Here you use the 'send\_and\_get\_output' method to retrieve the output of the
capacity command (df).

There are much more elegant ways to do the above (e.g. have a dictionary of the
servers to iterate over), but it's up to you how clever you need the Python to
be.


## More Intricate IO - Expecting

Let's say you have an interaction with an interactive command line application
you want to automate. Here we will use telnet as a trivial example:

```python
import shutit
session = shutit.create_session('bash')
session.send('telnet', expect='elnet>', echo=True)
session.send('open google.com 80', expect='scape character', echo=True)
session.send('GET /', echo=True, check_exit=False)
session.logout()
```

Note the 'expect' argument. You only need to give a subset of telnet's
prompt to match and continue.

Note also the 'check\_exit' argument in the above, which is new. We'll come back
to that. The output of the above is:

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

Now back to 'check\_exit=False'. Since the telnet command returns a failure exit
code (1) and we don't want the script to fail, you set 'check\_exit=False' to
let ShutIt know you don't care about the exit code.

If you didn't pass that argument in, ShutIt gives you an interactive terminal
if there is a terminal to communicate with. This is called a 'pause point'.


## Pause Points

You can trigger a 'pause point' at any point by calling

```python
[...]
session.pause_point('This is a pause point')
[...]
```

within your script, and then continue with the script by hitting CTRL and ']'
at the same time. This is great for debugging: add a pause point, have a look
around, then continue. Try this:

```python
import shutit
session = shutit.create_session('bash')
session.pause_point('Have a look around!')
session.send('echo "Did you enjoy your pause point?"', echo=True)
```

with output like this:

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


## More Intricate IO - Backgrounding

Returning to our 'monitoring multiple servers' example, let's imagine we
have a long-running task that we want to run on each server. By default, ShutIt
works serially which would take a long time. But we can run tasks in the
background to speed things up.

Here you can try an example with the trivial command: 'sleep 60'.


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

My laptop says it took 0.5 seconds to run fire those two commands, and then just
over a minute to complete (using the 'wait' method).

Again, this is trivial, but imagine you have hundreds of servers to manage like
this and you can see the power it can bring in a few lines of code and one
Python import.


## Learn More

There's a lot more that can be done with ShutIt.

To learn more, see:

[ShutIt](https://ianmiell.github.io/shutit/)
[GitHub](https://github.com/ianmiell/shutit/blob/master/README.md)

It's a broader automation framework, and the above is its 'standalone mode'.

Feedback, feature requests, 'how do I?'s highly appreciated! Reach me at
[@ianmiell](https://twitter.com/ianmiell)
