---
category: tool
tool: ansible
contributors:
    - ["Jakub Muszynski" , "http://github.com/sirkubax"]
    - ["Pat Myron" , "https://github.com/patmyron"]
    - ["Divay Prakash", "https://github.com/divayprakash"]
filename: LearnAnsible.txt
---

## Introduction

```yaml
---
"{{ Ansible }}" is an orchestration tool written in Python.
...
```

Ansible is (one of many) orchestration tools. It allows you to control your
environment (infrastructure and code) and automate the manual tasks.

Ansible has great integration with multiple operating systems (even Windows)
and some hardware (switches, Firewalls, etc). It has multiple tools that
integrate with the cloud providers. Almost every noteworthy cloud provider is
present in the ecosystem (AWS, Azure, Google, DigitalOcean, OVH, etc...).

But ansible is way more! It provides execution plans, an API, library, and callbacks.

### Main pros and cons

#### Pros

* It is an agent-less tool. In most scenarios, it uses ssh as a transport layer.
In some way you can use it as 'bash on steroids'.
* It is very easy to start. If you are familiar with the concept of ssh - you already
know Ansible (ALMOST).
* It executes 'as is' - other tools (salt, puppet, chef - might execute in
different scenario than you would expect)
* Documentation is at the world-class standard!
* Writing your own modules and extensions is fairly easy.
* Ansible AWX is the open source version of Ansible Tower we have been waiting
for, which provides an excellent UI.

#### Cons

* It is an agent-less tool - every agent consumes up to 16MB ram - in some
environments, it may be noticeable amount.
* It is agent-less - you have to verify your environment consistency
'on-demand' - there is no built-in mechanism that would warn you about some
change automatically (this can be achieved with reasonable effort)
* Official GUI - Ansible Tower - is great but expensive.
* There is no 'small enterprise' payment plan, however Ansible AWX is the free
open source version we were all waiting for.

#### Neutral

Migration - Ansible <-> Salt is fairly easy - so if you would need an
event-driven agent environment - it would be a good choice to start quick with
Ansible, and convert to Salt when needed.

#### Some concepts

Ansible uses ssh or paramiko as a transport layer. In a way you can imagine
that you are using a ssh with API to perform your action. The simplest way is
to execute remote command in more controlled way (still using ssh).
On the other hand - in advanced scope - you can wrap Ansible (use python Ansible
code as a library) with your own Python scripts! It would act a
bit like Fabric then.

## Example

An example playbook to install apache and configure log level

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

## Installation

```bash
# Universal way
$ pip install ansible

# Debian, Ubuntu
$ apt-get install ansible
```

* [Appendix A - How do I install ansible](#infrastructure-as-a-code)
* [Additional Reading.](http://docs.ansible.com/ansible/latest/intro_installation.html)

### Your first ansible command (shell execution)

```bash
# Command pings localhost (defined in default inventory: /etc/ansible/hosts)
$ ansible -m ping localhost
# You should see this output
localhost | SUCCESS => {
    "changed": false,
    "ping": "pong"
}
```

### Shell Commands

There are few commands you should know about

* `ansible` (to run modules in CLI)
* `ansible-playbook` (to run playbooks)
* `ansible-vault` (to manage secrets)
* `ansible-galaxy` (to install roles from github/galaxy)

### Module

A program (usually python) that executes, does some work and returns proper
JSON output. This program performs specialized task/action (like manage
instances in the cloud, execute shell command). The simplest module is called
`ping` - it just returns a JSON with `pong` message.

Example of modules:

* Module: `ping` - the simplest module that is useful to verify host connectivity
* Module: `shell` - a module that executes a shell command on a specified host(s).


```bash
$ ansible -m ping all
$ ansible -m shell -a 'date; whoami' localhost #hostname_or_a_group_name
```

* Module: `command` - executes a single command that will not be processed
through the shell, so variables like `$HOME` or operands like ``|` `;`` will not
work. The command module is more secure, because it will not be affected by the
user’s environment. For more complex commands - use shell module.

```bash
$ ansible -m command -a 'date; whoami' # FAILURE
$ ansible -m command -a 'date' all
$ ansible -m command -a 'whoami' all
```

* Module: `file` - performs file operations (stat, link, dir, ...)
* Module: `raw` - executes a low-down and dirty SSH command, not going through
the module subsystem (useful to install python2.7)

### Task

Execution of a single Ansible **module** is called a **task**. The simplest
module is called `ping` as you could see above.

Another example of the module that allows you to execute a command remotely on
multiple resources is called `shell`. See above how you were using them already.

### Playbook

**Execution plan** written in a form of script file(s) is called **playbook**.
Playbooks consist of multiple elements -
* a list (or group) of hosts that 'the play' is executed against
* `task(s)` or `role(s)` that are going to be executed
* multiple optional settings (like default variables, and way more)

Playbook script language is YAML. You can think that playbook is very advanced
CLI script that you are executing.

#### Example of the playbook

This example-playbook would execute (on all hosts defined in inventory) two tasks:
* `ping` that would return message *pong*
* `shell` that execute three commands and return the output to our terminal

```yaml
- hosts: all

  tasks:
    - name: "ping all"
      ping:

    - name: "execute a shell command"
      shell: "date; whoami; df -h;"
```

Run the playbook with the command:

```bash
$ ansible-playbook path/name_of_the_playbook.yml
```

Note: Example playbook is explained in the next chapter: 'Roles'

### More on ansible concept

### Inventory

An inventory is a set of objects or hosts, against which we are executing our
playbooks or single tasks via shell commands. For these few minutes, let's
assume that we are using the default ansible inventory (which in Debian based
system is placed in `/etc/ansible/hosts`).

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

* [Additional Reading.](http://docs.ansible.com/ansible/latest/intro_inventory.html)

### ansible-roles (a 'template-playbooks' with right structure)

You already know that the tasks (modules) can be run via CLI. You also know the
playbooks - the execution plans of multiple tasks (with variables and logic).

A concept called `role` was introduced for parts of the code (playbooks) that
should be reusable.

**Role** is a structured way to manage your set of tasks, variables, handlers,
default settings, and way more (meta, files, templates). Roles allow reusing
the same parts of code in multiple playbooks (you can parametrize the role
'further' during its execution). Its a great way to introduce `object oriented`
management for your applications.

Role can be included in your playbook (executed via your playbook).


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

#### For remaining examples we would use additional repository
This example installs ansible in `virtualenv` so it is independent from the system.
You need to initialize it into your shell-context with the `source environment.sh`
command.

We are going to use this repository with examples: [https://github.com/sirkubax/ansible-for-learnXinYminutes](https://github.com/sirkubax/ansible-for-learnXinYminutes)

```bash
$ # The following example contains a shell-prompt to indicate the venv and relative path
$ git clone git@github.com:sirkubax/ansible-for-learnXinYminutes.git
user@host:~/$ cd ansible-for-learnXinYminutes
user@host:~/ansible-for-learnXinYminutes$ source environment.sh
$
$ # First lets execute the simple_playbook.yml
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/simple_playbook.yml
```

Run the playbook with roles example

```bash
$ source environment.sh
$ # Now we would run the above playbook with roles
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/simple_role.yml
```

#### Role directory structure

```
roles/
   some_role/
     defaults/      # contains default variables
     files/         # for static files
     templates/     # for jinja templates
     tasks/         # tasks
     handlers/      # handlers
     vars/          # more variables (higher priority)
     meta/          # meta - package (role) info
```

#### Role Handlers
Handlers are tasks that can be triggered (notified) during execution of a
playbook, but they execute at the very end of a playbook. It is the best way to
restart a service, check if the application port is active (successful
deployment criteria), etc.

Get familiar with how you can use roles in the simple_apache_role example

```
playbooks/roles/simple_apache_role/
├── tasks
│   └── main.yml
└── templates
    └── main.yml
```

### ansible - variables

Ansible is flexible - it has 21 levels of variable precedence.
[read more](http://docs.ansible.com/ansible/latest/playbooks_variables.html#variable-precedence-where-should-i-put-a-variable)
For now you should know that CLI variables have the top priority.
You should also know, that a nice way to pool some data is a **lookup**

### Lookups
Awesome tool to query data from various sources!!! Awesome!
query from:
* pipe  (load shell command output into variable!)
* file
* stream
* etcd
* password management tools
* url

```bash
# read playbooks/lookup.yml
# then run
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/lookup.yml
```

You can use them in CLI too

```yaml
ansible -m shell -a 'echo "{{ my_variable }}"' -e 'my_variable="{{ lookup("pipe", "date") }}"' localhost
ansible -m shell -a 'echo "{{ my_variable }}"' -e 'my_variable="{{ lookup("pipe", "hostname") }}"' all

# Or use in playbook

(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/lookup.yml
```

### Register and Conditional

#### Register

Another way to dynamically generate the variable content is the `register` command.
`Register` is also useful to store an output of a task and use its value
for executing further tasks.

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

# when: example

   - name: Print this message when return code of 'check the system capacity' was ok
     debug:
       msg:  "{{ root_size.rc }}"
     when: root_size.rc == 0
...
```

#### Conditionals - when:

You can define complex logic with Ansible and Jinja functions. Most common is
usage of `when:`, with some variable (often dynamically generated in previous
playbook steps with `register` or `lookup`)

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

### ansible - tags, limit

You should know about a way to increase efficiency by this simple functionality

#### TAGS

You can tag a task, role (and its tasks), include, etc, and then run only the
tagged resources

```
ansible-playbook playbooks/simple_playbook.yml --tags=tagA,tag_other
ansible-playbook playbooks/simple_playbook.yml -t tagA,tag_other

There are special tags:
    always

--skip-tags can be used to exclude a block of code
--list-tags to list available tags
```

[Read more](http://docs.ansible.com/ansible/latest/playbooks_tags.html)

#### LIMIT

You can limit an execution of your tasks to defined hosts

```
ansible-playbook playbooks/simple_playbook.yml --limit localhost

--limit my_hostname
--limit groupname
--limit some_prefix*
--limit hostname:group #JM
```

### Templates

Templates are a powerful way to deliver some (partially) dynamic content.
Ansible uses **Jinja2** language to describe the template.

```
Some static content

{{ a_variable }}

{% for item in loop_items %}
    this line item is {{ item }}
{% endfor %}
```

Jinja may have some limitations, but it is a powerful tool that you might like.

Please examine this simple example that installs apache2 and generates
index.html from the template
"playbooks/roles/simple_apache_role/templates/index.html"

```bash
$ source environment.sh
$ # Now we would run the above playbook with roles
(venv) user@host:~/ansible-for-learnXinYminutes$ ansible-playbook playbooks/simple_role.yml --tags apache2
```

#### Jinja2 CLI

You can use the jinja in the CLI too

```bash
ansible -m shell -a 'echo {{ my_variable }}' -e 'my_variable=something, playbook_parameter=twentytwo' localhost
```

In fact - jinja is used to template parts of the playbooks too

```yaml
# check part of this playbook: playbooks/roles/sys_debug/tasks/debug_time.yml
- local_action: shell date +'%F %T'
  register: ts
  become: False
  changed_when: False

- name: Timestamp
  debug: msg="{{ ts.stdout }}"
  when: ts is defined and ts.stdout is defined
  become: False
```

#### Jinja2 filters

Jinja is powerful. It has many built-in useful functions.

```
# get first item of the list
{{ some_list | first() }}
# if variable is undefined - use default value
{{ some_variable | default('default_value') }}
```

[Read More](http://docs.ansible.com/ansible/latest/playbooks_filters.html)

### ansible-vault

To maintain **infrastructure as code** you need to store secrets. Ansible
provides a way to encrypt confidential files so you can store them in the
repository, yet the files are decrypted on-the-fly during ansible execution.

The best way to use it is to store the secret in some secure location, and
configure ansible to use them during runtime.

```bash
# Try (this would fail)
$ ansible-playbook playbooks/vault_example.yml

$ echo some_very_very_long_secret > ~/.ssh/secure_located_file

# in ansible.cfg set the path to your secret file
$ vi ansible.cfg
  ansible_vault_password_file = ~/.ssh/secure_located_file

#or use env
$ export ANSIBLE_VAULT_PASSWORD_FILE=~/.ssh/secure_located_file

$ ansible-playbook playbooks/vault_example.yml

  # encrypt the file
$ ansible-vault encrypt path/somefile

  # view the file
$ ansible-vault view path/somefile

  # check the file content:
$ cat path/somefile

  # decrypt the file
$ ansible-vault decrypt path/somefile
```

### dynamic inventory

You might like to know, that you can build your inventory dynamically.
(For Ansible) inventory is just JSON with proper structure - if you can
deliver that to ansible - anything is possible.

You do not need to reinvent the wheel - there are plenty of ready to use
inventory scripts for the most popular Cloud providers and a lot of in-house
popular usecases.

[AWS example](http://docs.ansible.com/ansible/latest/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script)

```bash
$ etc/inv/ec2.py --refresh
$ ansible -m ping all -i etc/inv/ec2.py
```

[Read more](http://docs.ansible.com/ansible/latest/intro_dynamic_inventory.html)

### ansible profiling - callback

Playbook execution takes some time. It is OK. First make it run, then you may
like to speed things up. Since ansible 2.x there is built-in callback for task
execution profiling.

```
vi ansible.cfg
# set this to:
callback_whitelist = profile_tasks
```

### facts-cache and ansible-cmdb

You can pull some information about your environment from another host.
If the information does not change - you may consider using a facts_cache
to speed things up.

```
vi ansible.cfg

# if set to a persistent type (not 'memory', for example 'redis') fact values
# from previous runs in Ansible will be stored.  This may be useful when
# wanting to use, for example, IP information from one group of servers
# without having to talk to them in the same playbook run to get their
# current IP information.
fact_caching = jsonfile
fact_caching_connection = ~/facts_cache
fact_caching_timeout = 86400
```

I like to use `jsonfile` as my backend. It allows to use another project
`ansible-cmdb` [(project on github)](https://github.com/fboender/ansible-cmdb) that generates a HTML page of your inventory
resources. A nice 'free' addition!

### Debugging ansible [chapter in progress]

When your job fails - it is good to be effective with debugging.

1. Increase verbosity by using multiple -v **[ -vvvvv]**
2. If variable is undefined -
`grep -R path_of_your_inventory -e missing_variable`
3. If variable (dictionary or a list) is undefined -
`grep -R path_of_your_inventory -e missing_variable`
4. Jinja template debug
5. Strange behaviour - try to run the code 'at the destination'

### Infrastructure as code

You already know, that ansible-vault allows you to store your confidential data
along with your code. You can go further - and define your
ansible installation and configuration as code.
See `environment.sh` to learn how to install the ansible itself inside a
`virtualenv` that is not attached to your operating system (can be changed by
non-privileged user), and as additional benefit - upgrading version of ansible
is as easy as installing new version in new virtualenv. What is more, you can
have multiple versions of Ansible present at the same time.

```bash
# recreate ansible 2.x venv
$ rm -rf venv2
$ source environment2.sh

# execute playbook
(venv2)$ ansible-playbook playbooks/ansible1.9_playbook.yml # would fail - deprecated syntax

# now lets install ansible 1.9.x next to ansible 2.x
(venv2)$ deactivate
$ source environment.1.9.sh

# execute playbook
(venv1.9)$ ansible-playbook playbooks/ansible1.9_playbook.yml # works!

# please note that you have both venv1.9 and venv2 present - you need to (de)activate one - that is all
```

#### become-user, become

In Ansible - to become `sudo` - use the `become` parameter. Use `become_user`
to specify the username.

```
- name: Ensure the httpd service is running
  service:
    name: httpd
    state: started
  become: true
```

Note: You may like to execute Ansible with `--ask-sudo-pass` or add the user to
sudoers file in order to allow non-supervised execution if you require 'admin'
privileges.

[Read more](http://docs.ansible.com/ansible/latest/become.html)

## Tips and tricks

#### --check -C

Always make sure that your playbook can execute in 'dry run' mode (--check),
and its execution is not declaring 'Changed' objects.

#### --diff -D

Diff is useful to see nice detail of the files changed.
It compare 'in memory' the files like `diff -BbruN fileA fileB`.


#### Execute hosts with 'regex'

```bash
ansible -m ping web*
```

#### Host groups can be joined, negated, etc

```bash
ansible -m ping web*:!backend:monitoring:&allow_change
```

#### Tagging

You should tag some (not all) objects - a task in a playbook, all tasks
included form a role, etc. It allows you to execute the chosen parts of the
playbook.

#### no_logs: True

You may see, that some roles print a lot of output in verbose mode. There is
also a debug module. This is the place where credentials may leak. Use `no_log`
to hide the output.

#### Debug module

allows to print a value to the screen - use it!

#### Register the output of a task

You can register the output (stdout), rc (return code), stderr of a task with
the `register` command.

#### Conditionals: when:

#### Loop: with, with\_items, with\_dict, with\_together

[Read more](http://docs.ansible.com/ansible/latest/playbooks_conditionals.html)

## Additional Resources

* [Servers For Hackers: An Ansible Tutorial](https://serversforhackers.com/c/an-ansible-tutorial)
* [A system administrator's guide to getting started with Ansible - FAST!](https://www.redhat.com/en/blog/system-administrators-guide-getting-started-ansible-fast)
* [Ansible Tower](https://www.ansible.com/products/tower) - Ansible Tower provides a web UI, dashboard and rest interface to ansible.
* [Ansible AWX](https://github.com/ansible/awx) - The Open Source version of Ansible Tower.
* [Ansible Tutorial for Beginners: Ultimate Playbook & Examples](https://spacelift.io/blog/ansible-tutorial)
