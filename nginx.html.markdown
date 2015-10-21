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
user www-data;              # 
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
}
```
The Nginx configuration file is managed in "blocks".

The first block was the events block. The next one is the http block, and the start of the main hierarchy within the configuration file.
