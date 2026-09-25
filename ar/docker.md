---
category: tool
name: Docker
filename: docker.bat
contributors:
    - ["Ruslan López", "http://javapro.org/"]
    - ["Michael Chen", "https://github.com/ML-Chen"]
    - ["Akshita Dixit", "https://github.com/akshitadixit"]
    - ["Marcel Ribeiro-Dantas", "https://github.com/mribeirodantas"]
translators:
    - ["Learn X in Y Minutes (ar)", "https://github.com/adambard/learnxinyminutes-docs"]
---

<p dir="rtl">
دوكر أداة تساعدك على بناء وتجربة ونشر وتشغيل التطبيقات بسلاسة على أجهزة مختلفة. يعيد إنتاج البيئة التي يحتاجها برنامجك على أي جهاز. يمكنك تنزيل دوكر من
<a href="https://docs.docker.com/get-docker/">docs.docker.com/get-docker/</a>
</p>

<p dir="rtl">
ازدادت شهرته خلال العقد الماضي لأنه خفيف وسريع مقارنة بالآلات الافتراضية الثقيلة والبطيئة. على عكس الأجهزة الافتراضية، دوكر لا يحتاج نظام تشغيل كاملاً ليُحمَّل عند البدء ولا يتنافس على موارد أكثر مما يستخدمه التطبيق الذي يشغّله. الأجهزة الافتراضية مكثّفة للمعالج والقرص والذاكرة، لذا تشغيل عدة أجهزة افتراضية لتطبيقات مختلفة يصبح تحدياً عندما الموارد محدودة.
</p>

<pre>
┌────────────────────────┐ ┌───────────────────────┐
│      ┌───────────┐     │ │      ┌───────────┐    │
│      │   App     │     │ │      │   App     │    │
│      └───────────┘     │ │      └───────────┘    │
│  ┌────────┐ ┌────────┐ │ │  ┌────────┐ ┌───────┐ │
│  │  Libs  │ │  Deps  │ │ │  │  Libs  │ │  Deps │ │
│  └────────┘ └────────┘ │ │  └────────┘ └───────┘ │
│  ┌───────────────────┐ │ │  ┌──────────────────┐ │
│  │      Guest OS     │ │ │  │     Guest OS     │ │
│  └───────────────────┘ │ │  └──────────────────┘ │
│           VM1          │ │           VM2         │
└────────────────────────┘ └───────────────────────┘
┌──────────────────────────────────────────────────┐
│                     Hypervisor                   │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│                      Host OS                     │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│             Hardware Infrastructure              │
└──────────────────────────────────────────────────┘
              (VM based architecture)

┌────────────────────────┐ ┌───────────────────────┐
│      ┌───────────┐     │ │      ┌───────────┐    │
│      │   App     │     │ │      │   App     │    │
│      └───────────┘     │ │      └───────────┘    │
│  ┌────────┐ ┌────────┐ │ │  ┌────────┐ ┌───────┐ │
│  │  Libs  │ │  Deps  │ │ │  │  Libs  │ │  Deps │ │
│  └────────┘ └────────┘ │ │  └────────┘ └───────┘ │
│        Container1      │ │       Container2      │
└────────────────────────┘ └───────────────────────┘
┌──────────────────────────────────────────────────┐
│                       Docker                     │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│                        OS                        │
└──────────────────────────────────────────────────┘
┌──────────────────────────────────────────────────┐
│             Hardware Infrastructure              │
└──────────────────────────────────────────────────┘
            (Docker based architecture)
</pre>

<p dir="rtl">
من المصطلحات الشائعة: <strong>صور دوكر</strong> و<strong>حاويات دوكر</strong>. الصور حزم أو قوالب للحاويات تُخزَّن في سجل مثل
<a href="https://hub.docker.com/">Docker Hub</a>.
الحاويات نسخ قابلة للتنفيذ منفصلة من هذه الصور وتتضمن الشيفرة ووقت التشغيل وأدوات النظام والمكتبات والإعدادات — كل ما يلزم لتشغيل البرنامج. دوكر يتبع بنية عميل-خادم: عميل سطر الأوامر يتواصل مع مكوّن الخادم، أي محرك دوكر، عبر واجهة REST لإصدار الأوامر.
</p>

<h2 dir="rtl">سطر أوامر دوكر</h2>

```bash
# بعد تثبيت دوكر من https://docs.docker.com/get-docker/
# لعرض الأوامر: نفّذ docker بلا معاملات أو docker help
$ docker

>>> docker [OPTIONS] COMMAND [ARG...]
       docker [ --help | -v | --version ]

    A self-sufficient runtime for containers.

    Options:
        --config string      Location of client config files (default "/root/.docker")
    -c, --context string     Name of the context to use to connect to the daemon (overrides DOCKER_HOST env var and default context set with "docker context use")
    -D, --debug              Enable debug mode
        --help               Print usage
    -H, --host value         Daemon socket(s) to connect to (default [])
    -l, --log-level string   Set the logging level ("debug"|"info"|"warn"|"error"|"fatal") (default "info")
        --tls                Use TLS; implied by --tlsverify
        --tlscacert string   Trust certs signed only by this CA (default "/root/.docker/ca.pem")
        --tlscert string     Path to TLS certificate file (default "/root/.docker/cert.pem")
        --tlskey string      Path to TLS key file (default "/root/.docker/key.pem")
        --tlsverify          Use TLS and verify the remote
    -v, --version            Print version information and quit

    Commands:
        attach    Attach to a running container
        # […]

$ docker run hello-world
# docker run <اسم-الصورة> يشغّل حاوية؛ تُسحَب الصورة من Docker Hub إن لم تكن محلياً.
# العميل يتصل بالخادم الذي يسحب hello-world ويبني حاوية ويشغّلها ويعيد المخرجات للطرفية.

$ docker run -d ubuntu sleep 60s
# -d (--detach) يشغّل الحاوية في الخلفية ويعيد الطرفية.
# المخرجات: معرّف الحاوية؛ تحقق بـ docker ps:
# CONTAINER ID   IMAGE     COMMAND       CREATED         STATUS         PORTS     NAMES
# 133261b4894a   ubuntu    "sleep 60s"   3 seconds ago   Up 2 seconds             vigorous_gould

$ docker run -p 3000:8000 <image-id>
# -p ينشر المنفذ 8000 داخل الحاوية على 3000 على المضيف (التطبيق معزول داخل الحاوية).

$ docker run -i
# أو
$ docker run -it
# افتراضياً غير تفاعلي؛ -i يبقي الإدخال مفتوحاً، -t طرفية زائفة (غالباً -it معاً).

$ docker ps -a
# docker ps يعرض الشغّالة فقط؛ -a لكل الحاويات
# مثال مخرجات:
# CONTAINER ID   IMAGE         COMMAND    CREATED         STATUS                     PORTS     NAMES
# 82f84bf6912b   hello-world   "/hello"   9 minutes ago   Exited (0) 9 minutes ago             eloquent_sammet


$ docker stop hello-world
# أو
$ docker start hello-world
# stop يوقف؛ start يعيد التشغيل. docker start -a يربط الحاوية المنفصلة بالمقدمة.

$ docker create alpine
# docker create يجهّز حاوية من الصورة دون تشغيل تلقائي (عكس run). ثم docker start.
# الحالة Created:
# CONTAINER ID   IMAGE         COMMAND       CREATED             STATUS           PORTS     NAMES
# 4c71c727c73d   alpine        "/bin/sh"     29 seconds ago      Created                   naughty_ritchie

$ docker rm 82f84
# حذف حاوية (أو أكثر) بمعرّفها؛ يكفي بادئة من المعرّف.

$ docker images
# كل الصور؛ «created» هنا يعني آخر تحديث للوسم على Docker Hub:
# REPOSITORY    TAG       IMAGE ID       CREATED         SIZE
# ubuntu        latest    a8780b506fa4   9 days ago      77.8MB
# alpine        latest    9c6f07244728   3 months ago    5.54MB
# hello-world   latest    feb5d9fea6a5   13 months ago   13.3kB

$ docker rmi
# حذف صورة (أو أكثر) بلا حاويات شغّالة. إن وُجدت حاوية احذفها أولاً أو استخدم -f.

$ docker pull busybox
# تنزيل الصورة من Docker Hub.

$ docker exec -it 7b272 bash
# تنفيذ أمر داخل حاوية شغّالة (هنا جلسة bash في ubuntu 7b272).

$ docker logs <container-id>
# سجلّ مخرجات الحاوية
# root@7b27222e4bb7:/# whoami
# root
# root@7b27222e4bb7:/# pwd
# /
# root@7b27222e4bb7:/# ls
# bin  boot  dev  etc  home  lib  lib32  lib64 libx3 srv  sys  tmp  usr  var
# root@7b27222e4bb7:/# exit
# exit

# المزيد: https://docs.docker.com/engine/reference/commandline/docker/
```

<h2 dir="rtl">ملف Dockerfile</h2>

<p dir="rtl">
ملف Dockerfile مخطط لصورة دوكر. نضع فيه مخرجات التطبيق وإعداداتها بصياغة محددة ليتمكن أي شخص من بناء صورة لتطبيقنا.
</p>

<h3 dir="rtl">نقاط يجب تذكرها</h3>

<ul dir="rtl">
<li>الاسم ثابت: <code>Dockerfile</code> بلا امتداد.</li>
<li>نبني الصورة المخصصة فوق صورة أساس موجودة مسبقاً (هناك صورة فارغة اسمها <code>scratch</code>).</li>
<li>الأوامر بأحرف كبيرة جزء من الصياغة؛ غير حساسة لحالة الأحرف لكنها اتفاقية شائعة.</li>
<li>في الأسفل مثال؛ للتفصيل راجع <a href="https://docs.docker.com/engine/reference/builder/">التوثيق الرسمي</a>.</li>
</ul>

```dockerfile
FROM <base-image>
# صورة الأساس

ENV USERNAME='admin'\
    PWD='****'
# متغيرات بيئة؛ الاعتماديات الحقيقية أفضل عبر .env أو أسرار آمنة

RUN apt-get update
# أوامر لينكس أثناء بناء الصورة؛ لا تؤثر على المضيف

COPY <src> <target>
# نسخ من المضيف (src) إلى مسار داخل الصورة (target)

ENTRYPOINT ["some-script.sh"]
# نقطة الدخول: سكربت كامل

CMD [<args>,...]
# أمر التشغيل عند تشغيل الحاوية من الصورة، مثلاً CMD node server.js
# يُنفَّذ بعد اكتمال بناء الصورة فقط عند تشغيل حاوية
```

<h3 dir="rtl">بناء الصور</h3>

<p dir="rtl">
استخدم الأمر <code>docker build</code> بعد تجهيز التطبيق في Dockerfile.
</p>

```bash
$ docker build -t <image-name>:<tag> <path-to-dockerfile>
# بناء صورة من Dockerfile
# -t اسم ووسم للصورة
# المسار يمكن أن يكون . للمجلد الحالي أو رابطاً
```

<h2 dir="rtl">رفع الصورة إلى Docker Hub</h2>

<p dir="rtl">
لجعل صورة تطبيقك متاحة علناً لأي مستخدم دوكر، يمكنك رفعها إلى
<a href="https://hub.docker.com/">Docker Hub</a>
، سجل صور دوكر. تأكد أن لديك حساباً باسم مستخدم وكلمة مرور.
عند الرفع يجب أن يكون اسم الصورة يتضمن اسم مستخدم Docker Hub كما في مستودعات GitHub.
</p>

```bash
$ docker login
# تسجيل الدخول لـ Docker Hub

$ docker tag <src-image>[:<src-tag>] <target-image>[:<target-tag>]
# إعادة وسم صورة محلية لاسم عام، مثلاً docker tag تطبيقي:1.0.0 مستخدم/تطبيقي
# بلا وسوم يُفترض latest

$ docker push <target-image>[:<target-tag>]
# رفع الصورة إلى Docker Hub
# تظهر تحت https://hub.docker.com/r/username/image-name
```
