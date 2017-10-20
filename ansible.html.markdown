---
category: tool
tool: ansible
contributors:
    - ["Jakub Muszynski" , "http://github.com/sirkubax"]
filename: LearnAnsible.txt
---

## Ansible: the easiest orchestration tool

```yaml
---
"{{ Why Ansible and detailed Intro }}" written in the second part of document

```

## Installation
```bash
# Universal way
$ pip install ansible

# Debian, Ubuntu
$ apt-get install ansible

```
* Appendix A - How do I install ansible
* [Additional Reading.](http://docs.ansible.com/ansible/latest/intro_installation.html)

### Your first ansible command (shell execution)
```bash
# This command ping the localhost (defined in default inventory /etc/ansible/hosts) 
$ ansible -m ping localhost
# you should see this output
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
* and other!

### Module
_program (usally python) that execute, do some work and return proper JSON output_

This *program* perform specialized task/action (like manage instances in the cloud, execute shell command).

The simplest module is called `ping` - it just returns a JSON with `pong` message.

Example of modules:
* Module: `ping` - the simplest module that is usefull to verify host connectivity
* Module: `shell` - a module that executes shell command on a specified host(s).

Example of execution - `ping`, `shell`

```bash
$ ansible -m ping all
$ ansible -m shell -a 'date; whoami' localhost #hostname_or_a_group_name
```

* Module: `command` - executes a single command that will not be processed through the shell, so variables like $HOME or operands like `|` `;` will not work
#JM


```bash
$ ansible -m command -a 'date; whoami' # FAILURE

$ ansible -m command -a 'date' all
$ ansible -m command -a 'whoami' all
```

* Module: `file` - performs file operations (stat, link, dir, ...) 
* Module: `raw` - executes a low-down and dirty SSH command, not going through the module subsystem (usefull to install python2.7)

### Task
  Execution of a single Ansible **module** is called a **task**

   The simplest module is called `ping` as you could see above

   Another example of the module that allow you to execute command remotly on multiple resources is called `shell`. See above how you were using them already.


### Playbook
**Execution plan** written in a form of script file(s) is called **playbook**.
Playbook consist of multiple elements
* a list (or group) of hosts that 'the play' is executed against
* `task(s)` or `role(s)` that are going to be executed
* multiple optional settings (like default variables, and way more)

Playbook script language is YAML.

You can think that playbook is very advanced CLI script that you are executing.


#### Example of the playbook:
This example-playbook would execute (on all hosts defined in the inventory) two tasks:
* `ping` that would return message *pong*
* `shell` that execute three commands and return the output to our terminal

```yml
hosts: all

tasks:
  - name: "ping all"
    ping:
  - name: "execute a shell command"
    shell: "date; whoami; df -h;"
```

You can run the playbook with the command:
```bash
$ ansible-playbook path/name_of_the_playbook.yml
```
### More on ansible concept

### Inventory
Inventory is a set of an objects or hosts, against which we are executing our playbooks or single tasks via shell commands
For this few minutes, lets asume that we are using default ansible inventory (which in Debian based system is placed in /etc/ansible/hosts)

`/etc/ansible/hosts`
```
localhost

[some_group]
hostA.mydomain.com
hostB.localdomain

[a_group_of_a_groups:children]
some_group
some_other_group

```
* [Additional Reading.](http://docs.ansible.com/ansible/latest/intro_inventory.html)
### ansible-roles (a 'template-playbooks' with right structure)

   You already know the tasks (modules) that can be run via CLI. You also know the playbooks - the execution plans of multiple tasks (with variables and logic).

A concept called `role` was introduced for parts of the code (playbooks) that should be reusable.

**Role** is a structured way to manage your set of tasks, variables, handlers, default settings, and way more (meta, files, templates).
Role allows to reuse the same parts of code in multiple plybooks (you can parametrize the role 'further' during it's execution).
It is a great way to introduce `object oriented` management for your applications.

Role can be included in your playbook (executed via your playbook).


```yml
hosts: all

tasks:
    - name: "ping all"
      ping:
    - name: "execute a shell command"
      shell: "date; whoami; df -h;"

role: 
    - some_role
    - { role: another_role, some_variable: 'learnxiny', tags: ['my_tag'] }

pre_tasks:
    - name: some pre-task
      shell: echo 'this task is the last, but would be executed before roles, and before tasks'
```

Example->role

We would clone the ready-to-use examples from additional repository
```bash
$ git colone git@github.com:sirkubax/ansible-for-learnXinYminutes.git
$ cd ansible-for-learnXinYminutes
$ source environment
$(venv) ansible-playbook playbooks/role_example.yml
```

#### Role directory structure:
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
Handlers are a tasks that can be triggered (notified) during execution of a playbook, but they itself execute at the very end of a playbook.
It is a best way to restart a service, check if application port is active (successfull deployment criteria), etc.

### ansible - variables

Ansible is flexible - it has 21 levels of variable precedence

[read more] 

For now you might like to know, that CLI variables has the top priority.

You should also know, that a nice way to pool some data is a **lookup**

### Lookups

* pipe
* file
* stream
* etcd

You can use them in CLI too
```yaml
ansible -m shell -a 'echo {{ my_variable }}` -e '{{ lookup('pipe'; 'date' }}" localhost

```

### Templates

Template is a powerfull way to deliver some (partially) dynamic content. Ansible uses **Jinja2** langueage to describe the template.

```jinja2
Some static content

{{ a_variable }}

{% for item in loop_items %} 
    this line item is {{ item }}
{% endfor %}
```
Jinja may have some limitations, but it is a powerfull tool that you might like.

### Jinja2 CLI
You can use the jinja in the CLI too
```bash
ansible -m shell -a 'echo {{ my_variable }}` -e 'my_variable=something, playbook_parameter=twentytwo" localhost
```

### Jinja2 filters
Junja is powerfull. It has built-in many usefull functions.
```jinja
# get first item of the list
{{ some_list | first() }}
# if variable is undefined - use default value
{{ some_variable | default('default_value') }}
```

### ansible-vault
To maintain **ifrastructure as a code** you need to store secrets. 
  Ansible provides a way to encrypt the poufne files so you can store it in the repository, yet the files are decrypted in-fly during ansible execution.

The best way to use the **ansible-vault** is to store the secret in some secure location, and configure ansible to use during runtime.

```bash
$ echo some_very_very_long_secret > ~/.ssh/secure_located_file

$ vi ansible.cfg
  ansible_vault_password_file = ~/.ssh/secure_located_file

#or to use env
export ANSIBLE_VAULT_PASSWORD_FILE=~/.ssh/secure_located_file

$ ansible-playbook playbooks/vault_example.yml

  # decrypt the file
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

(For Ansible) inventory is just a JSON with proper structure - if you can deliver that to ansible - anything is possible.

You do not need to invent the wheel - there are plenty ready to use inventory script for most popular Cloud provicers and a lot of in-house popular usecaseses.

```bash
$ etc/inv/ec2.py --refresh

$ ansible -m ping all -i etc/inv/ec2.py
```

Read also about `dynamic inventory` below

### ansible profiling - callback
It is ok that your playbook executes some time. Sometimes you may like to speed things up 

Since ansible 2.x there is bouilt-in callback for task execution profiling

```
vi ansible.cfg 
#set this to:
callback_whitelist = profile_tasks
```

### facts-cache and ansible-cmdb
You can pool some infrmations of you environment from another hosts.
If the informations does not change - you may consider using a facts_cache to speed things up.

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
`ansible-cmdb` [github] that generates a HTML page of your inventory resources. A nice 'free' addition!

### debugging ansible
When your job fails - it is good to be effective with debugging.

1. Increase verbosiy by using multiple -v  **[ -vvvvv]**
2. If variable is undefined
3. If variable (dictionary or a list) is undefined
4. Jinja template debug 

### Infrastructure as a code - what about Ansible
You already know, that ansible-vault allow you to store your poufne data along with your code (in repository). You can go further - and define your ansible installation and configuration as-a-code. 
See `environment.sh` to learn how to install the ansible itself inside a `virtualenv` that is not attached to your operating system (can be changed by non-privilages user), and as additiinal benefit - upgrading version of ansible is as easy as installing new version in new virtualenv. You can have multiple versions of Ansible present in the same time. This is very helpfull!

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
### Naming

### Bonus

### writing own module

### Python API

### Web-UI: Ansible Tower, Jenkins, Rundeck

#### Ansible Tower
Ansible provides a Web User Interface called `Ansible Tower`.
It is a convienient way to run Ansible Playbooks, have proper user management, log retention, and cron (periodic jobs).

Personaly I'm not a fan of it - it's to expensive for my cases, and the trial is 10 inventory-hosts only.

For my usecases I hide the 'pure ansible' commands behind other projects.

#### Rundeck
This is nice, secure interface, that allows you to execute a jobs of your choice (CLI, script, execution plan).
It can perform  roling-deployment (without Ansible), can integrate with clouds, etc.

#### Jenkins
For my 'business cases' I use Jenkins - it has a 'cron', jobs can be binded into 'pipelines'.

#### become-user, become

## Tips and tricks

#### --check -C
Always make sure that your playbook can executes in 'dry run' mode (--check), and it's execution is not declaring 'Changed' objects.

#### --diff -D
Diff is usefull to see nice detail of the files changed

It compare 'in memory' the files like `diff -BbruN fileA fileB`


#### Execute hosts with 'regex'
```bash
ansible -m ping web*
```

####
Host groups can be joined, negated, etc

```bash
ansible -m ping web*:!backend:monitoring:&allow_change
```

#### Tagging
You should tag some (not all) objects - a task in a playbook, all tasks included form a role, etc.
It allwos you to execute the choosen parts of the playbook.

#### no_logs: True
You may see, that some roles print a lot of output in verbose mode. There is also a debug module.
This is the place where credentials may leak. Use `no_log` to hide the output.

#### Debug module
allows to print a value to the screen

#### Register the output of a task
You can register the output (stdout), rc (return code), stderr of a task with the `register` command.

#### Conditionals: when: 

#### Loop: with, with_items, with_dict, with_together


## Introduction
Ansible is (one of the many) orchestration tools. It allows you to controll your environment (infrastructure and a code) and automate the manual tasks.
'You can think as simple as writing in bash with python API 
Of course the rabit hole is way deeper.'

Ansible have great integration with multiple operating systems (even Windows) and some hardware (switches, Firewalls, etc). It has multiple tools that integrate with the could providers. Almost every worth-notice cloud provider is present in the ecosystem (AWS, Azure, Google, DigitalOcean, OVH, etc...)



## Main cons and pros

### Cons

It is an agent-less tool - every agent consumes up to 16MB ram - in some environments, it may be noticable amount.
It is agent-less - you have to verify your environment consistency 'on-demand' - there is no built-in mechanism taht would warn you about some change automatically (this can be achieved with reasonable effort - but it must be known)
Official GUI Tool (web inferface) - Ansible Tower - is more than GUI, but it is expensive. There is no 'small enterprice' payment plan. Easy workaround with Rundeck or Jenkins is possible with reasonable workload.

### Pros

It is an agent-less tools In most scenarios, it use ssh as a transport layer. 
In some way you can use it as 'bash on steroids'.
It is very-very-very easy to start. If you are familiar with ssh concept - you already know ansible (ALMOST). My personal record is: 'I did show how to install and use ansible (for simple raspberry pi cluster management) and it tool me 30 seconds to deliver a working tool !!!)'
I do provide a training services - I'm able to teach a production-ready person - in 8 hours (1 training day)! It covers all needed to work aspects! No other tool can match this ease of use!
It executes when you do it - other tools (salt, puppet, chef - might execute in different scenario than you would expect)
Documentation is at the world-class standard!
The comunity (github, stackOverflow) would help you very fast.
Writing own modules and extension is fairly easy.


### Neutral
Migration Ansible<->Salt is failrly easy - so if you would need an event-driven agent environment - it would be a good choice to start quick with Ansible, and convert to salt when needed.

## Basics on ansible

Ansible uses ssh or paramiko as a transport layer. In a way you can imagine that you are using a ssh with API to perform your action.
In the 'low-level' way you can use it to execute remote command in more controlled way (still using ssh). 
On the other hand - in advanced scope - you can use python anible code as a library to your own python scrips! This is awesome! (if you know what you are doing). It is a bit like fabric then.

But ansible is way more! It provides an execution plans, an API, library, callbacks, not forget to mention - COMUNITY! and great support by developers!




# JM inventory dynamic aws ec2
# vault
# roles

#### ansible - dynamic in AWS
#### create instance in AWS
#### create env in AWS
