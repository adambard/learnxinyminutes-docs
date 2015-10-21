---
language: Nginx
contributors:
    - ["Harsh Vardhan", "http://royalharsh.github.io"]
filename: learnNginx.txt
---

## What is NGINX ?

NGINX is a free, open-source, high-performance HTTP server and reverse proxy, as well as an IMAP/POP3 proxy server. NGINX
is known for its high performance, stability, rich feature set, simple configuration, and low resource consumption. It is good at handling many concurrent connections and excels at serving static content.

### Nginx Configuration Directory

Nginx stores its configuration files within the `/etc/nginx ` directory.
Inside this directory you will find various directories and modular configuration files.
```bash
cd /etc/nginx
ls -F
```
```
conf.d/         koi-win           naxsi.rules   scgi_params       uwsgi_params
fastcgi_params  mime.types        nginx.conf    sites-available/  win-utf
koi-utf         naxsi_core.rules  proxy_params  sites-enabled/
```
These directories are used to define configurations for your websites.
Files are generally created in the "sites-available" directory, and then symbolically linked to the "sites-enabled" directory when they are ready to go live.

 The "nginx.conf" file is the main configuration file.
 
 For many distributions, the file will be located at `/etc/nginx/nginx.conf`. If it does not exist there, it may also be at `/usr/local/nginx/conf/nginx.conf` or `/usr/local/etc/nginx/nginx.conf`.
 
nginx.conf file :

Open the file : 

`sudo nano /etc/nginx/nginx.conf`

```
user www-data;             
worker_processes 4;         # It defines how many concurrent processes that Nginx will use.
pid /run/nginx.pid;         # This directive specifies where the process pid will be stored for internal reference.

events {
	worker_connections 768;
	# multi_accept on;
}

http {

	##
	# Basic Settings
	##

	sendfile on;
	tcp_nopush on;
	tcp_nodelay on;
	keepalive_timeout 65;
	types_hash_max_size 2048;
	# server_tokens off;

	# server_names_hash_bucket_size 64;
	# server_name_in_redirect off;

	include /etc/nginx/mime.types;
	default_type application/octet-stream;

	##
	# SSL Settings
	##

	ssl_protocols TLSv1 TLSv1.1 TLSv1.2; # Dropping SSLv3, ref: POODLE
	ssl_prefer_server_ciphers on;

	##
	# Logging Settings
	##

	access_log /var/log/nginx/access.log;
	error_log /var/log/nginx/error.log;

	##
	# Gzip Settings
	##
    
	gzip on;       
	gzip_disable "msie6";
     #  This tells Nginx to enable gzip to compress data that is sent to clients, 
    but to disable gzip compression when the client is Internet Explorer version 6, 
    because that browser does not understand gzip compression.



	# gzip_vary on;
	# gzip_proxied any;
	# gzip_comp_level 6;
	# gzip_buffers 16 8k;
	# gzip_http_version 1.1;
	# gzip_types text/plain text/css application/json application/javascript text/
	xml application/xml application/xml+rss text/javascript;

	##
	# Virtual Host Configs
	##

	include /etc/nginx/conf.d/*.conf;
	include /etc/nginx/sites-enabled/*;
	
	#  This tells us that the server and location blocks that define specific sites and 
	url match locations will take place outside of this file.
}
```
The Nginx configuration file is managed in "blocks".

The first block was the events block. The next one is the http block, and the start of the main hierarchy within the configuration file.

#### The Main Context

The most general context is the "main" or "global" context. It is the only context that is not contained within the typical context blocks that look like this:

```
# The main context is here, outside any other contexts

. . .

context {

    . . .

}
```
Any directive that exist entirely outside of these blocks is said to inhabit the "main" context. 
The main context represents the broadest environment for Nginx configuration. It is used to configure details that affect the entire application on a basic level.

#### The Events Context

The "events" context is contained within the "main" context. 
It is used to set global options that affect how Nginx handles connections at a general level. 
There can only be a single events context defined within the Nginx configuration.

```
# main context

events {

    # events context
    . . .

}
```
Nginx uses an event-based connection processing model, so the directives defined within this context 
determine how worker processes should handle connections.

#### HTTP Context

When configuring Nginx as a web server or reverse proxy, the "http" context will hold the majority of the configuration.
This context will contain all of the directives and other contexts necessary to define how the program will handle HTTP or HTTPS connections.

```
# main context

events {
    # events context

    . . .

}

http {
    # main context

    . . .

}
```
The http context is a sibling of the events context, so they should be listed side-by-side, rather than nested. 

#### Server Context

The "server" context is declared within the "http" context.

The general format for server context may look something like this. Remember that these reside within the http context:

```
# main context

http: {

    # http context

    server {

        # first server context

    }

    server {

        # second server context

    }

}
```
The reason for allowing multiple declarations of the server context is that each instance defines a specific virtual server to handle client requests. 
You can have as many server blocks as you need, each of which can handle a specific subset of connections.

#### Starting, Stopping, and Reloading Configuration

Once nginx is started, it can be controlled by invoking the executable with the -s parameter. We can use the following syntax:

`nginx -s signal`

Where signal may be one of the following:

   * stop — fast shutdown
   * quit — graceful shutdown
   * reload — reloading the configuration file
   * reopen — reopening the log files

 For getting the list of all running nginx processes, the ps utility may be used, for example, in the following way: 
 
 `ps -ax | grep nginx`

To test the syntax of nginx.conf file, use : `nginx -t`


#### Additional readings 

[Installing NGINX](https://www.digitalocean.com/community/tutorials/how-to-install-nginx-on-ubuntu-14-04-lts)
[Beginner's Guide](http://nginx.org/en/docs/beginners_guide.html)
