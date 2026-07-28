---
contributors:
    - ["Andrii Puhachenko", "https://github.com/andrqxa"]
translators:
    - ["Andrii Puhachenko", "https://github.com/andrqxa"]
---

**Active Oberon** — останній представник родини Pascal / Modula-2 / Oberon,
створений у ETH Zürich (1996-1998, групою навколо Ніклауса Вірта та
Юрга Гуткнехта). Він зберігає все, чим відомий Oberon — граматику, що
вміщається на одну сторінку, модулі з явним експортом, сувору статичну
типізацію та збирач сміття — і додає те, від чого походить його назва:
**активні об'єкти**.

Об'єкт може мати власний потік (`BEGIN {ACTIVE}`), будь-який блок можна
оголосити критичною секцією (`BEGIN {EXCLUSIVE}`), а `AWAIT(умова)` замінює
умовні змінні. Паралелізм є частиною мови, а не бібліотекою. Крім того, мова
отримала перевизначення операторів, переліки, математичні (тензорні) масиви
з поелементними операторами та комірки (cells) для опису апаратури.

Уся [операційна система
A2](https://uk.wikipedia.org/wiki/A2_%28операційна_система%29) — ядро, драйвери,
компілятор, мережевий стек і графічний інтерфейс — написана мовою Active
Oberon, і саме A2 є природним місцем, щоб спробувати цю мову.

Наведений нижче код приймає поточний компілятор Fox із A2. Старіші тексти
A2 / Bluebottle використовують історичні назви типів (`LONGINT`, `LONGREAL`,
`SHORTINT`) там, де сучасний код пише `SIGNED32`, `FLOAT64`, `SIGNED8`.

```componentpascal
(*  Коментарі виглядають так, і (* вони вкладаються *).  *)
(** Коментар із двома зірочками — це документація, її збирають
  документаційні інструменти. *)

(* Одиниця компіляції — це завжди MODULE. Ключові слова пишуть великими
   літерами, ідентифікатори чутливі до регістру, а ім'я модуля
   повторюється після останнього END. Файл зазвичай називають так само,
   як модуль. *)
MODULE LearnActiveOberon;

(* Домовленість про регістр: сканер читає ключові слова незалежно від
   регістру, а стиль файлу визначає його розширення. Файл X.Mod пишуть
   ВЕЛИКИМИ літерами, файл x.mod — малими, разом із наперед оголошеними
   типами та вбудованими процедурами:

     module HelloLower;
     import KernelLog;
     procedure Do*;
     var i: signed32;
     begin
       KernelLog.String("hi"); KernelLog.Ln
     end Do;
     end HelloLower.
*)

IMPORT
  KernelLog,            (* вивід у системний журнал *)
  Streams,              (* абстракція Reader / Writer *)
  Commands,             (* контекст команди: ввід, вивід, аргументи *)
  Log := KernelLog;     (* імпорт можна перейменувати *)

CONST
  Max* = 100;           (* "*" експортує ідентифікатор *)
  Greeting = "Hello";   (* без позначки: видно лише в цьому модулі *)
  EvenBits = {0, 2, 4, 6};                  (* константа-множина *)

TYPE
  Name* = ARRAY 32 OF CHAR;                 (* рядок, що завершується 0X *)
  Handler* = PROCEDURE {DELEGATE} (n: SIGNED32);

  Point* = RECORD
    x*, y*: SIGNED32;
  END;

  Point3d* = RECORD (Point)                 (* розширення запису *)
    z*: SIGNED32;
  END;

  Color* = ENUM Red*, Green*, Blue* END;    (* перелічуваний тип *)

  Vec2* = RECORD x*, y*: FLOAT64 END;

VAR
  counter-: SIGNED32;   (* "-" експортує лише для читання *)
  origin: Point;

(* ------------------------------------------------------------------ *)
(* Типи, літерали та оператори                                         *)
(* ------------------------------------------------------------------ *)

PROCEDURE Basics;
VAR
  b: BOOLEAN;
  c: CHAR;
  i8: SIGNED8; i16: SIGNED16; i32: SIGNED32; i64: SIGNED64;
  u32: UNSIGNED32;
  f32: FLOAT32; f64: FLOAT64;
  z: COMPLEX64;
  s: SET;                        (* невелика бітова множина *)
  adr: ADDRESS; size: SIZE;      (* цілі розміру вказівника *)
  any: ANY;                      (* посилання на будь-який об'єкт *)
BEGIN
  (* INTEGER, REAL і SET — варіанти типової ширини; код A2 зазвичай
     вказує ширину явно: SIGNED32, FLOAT64, SET32, ... *)
  i32 := 42;  i32 := 2AH;  i32 := 0x2A;  i32 := 0b101010;
  i32 := 1'000'000;              (* роздільники розрядів дозволені *)
  i8 := -1; i16 := 1000; i64 := 1; u32 := 0FFFFFFFFH;
  c := "A";  c := 41X;           (* символ, шістнадцятковий символ *)
  f32 := 1.5;  f64 := 1.5E-3;
  z := 1 + 2*IMAG;               (* IMAG — уявна одиниця *)
  f64 := RE(z);  f64 := IM(z);

  (* Арифметика: + - * / DIV MOD, цілочисельне ділення — це DIV *)
  i32 := 7 DIV 2;  i32 := 7 MOD 2;  f64 := 7 / 2;
  i32 := ABS(-3);  INC(i32);  DEC(i32, 2);

  (* Відношення: = # < <= > >= IN IS. Логічні: & OR ~ (не) *)
  b := (i32 = 5) OR (i32 # 6) & ~(i32 < 0);

  (* Множини *)
  s := {0, 3, 5..7};  s := s + {1};  s := s * EvenBits;  s := s - {3};
  b := 3 IN s;  INCL(s, 9);  EXCL(s, 0);

  (* Перетворення типів завжди явні *)
  i32 := ORD("A");  c := CHR(65);  f64 := i32;  i32 := ENTIER(f64);
  adr := ADDRESSOF(i32);  size := SIZEOF(Point);

  any := NIL;
  ASSERT(i32 >= 0);              (* пастка (trap), якщо умова хибна *)
END Basics;

(* ------------------------------------------------------------------ *)
(* Керування виконанням                                                *)
(* ------------------------------------------------------------------ *)

PROCEDURE ControlFlow(n: SIGNED32): SIGNED32;
VAR i, sum: SIGNED32; c: CHAR; color: Color;
BEGIN
  IF n < 0 THEN
    n := -n
  ELSIF n = 0 THEN
    n := 1
  ELSE
    (* нічого *)
  END;

  c := "x";
  CASE c OF
      "a".."m": sum := 1
    | "n".."z": sum := 2
  ELSE
    sum := 0
  END;

  color := Color.Green;          (* елементи переліку кваліфіковані *)
  IF color = Color.Green THEN sum := sum + 1 END;

  WHILE n > 1 DO n := n DIV 2 END;

  REPEAT INC(n) UNTIL n >= 4;

  FOR i := 0 TO 10 BY 2 DO sum := sum + i END;

  LOOP
    INC(i);
    IF i > 100 THEN EXIT END
  END;

  VAR local := sum + 1;          (* оголошення VAR усередині блоку *)
  RETURN local
END ControlFlow;

(* ------------------------------------------------------------------ *)
(* Масиви, рядки, записи та вказівники                                 *)
(* ------------------------------------------------------------------ *)

TYPE
  IntArray* = POINTER TO ARRAY OF SIGNED32;    (* динамічний масив *)
  PointPtr* = POINTER TO Point;

PROCEDURE Sum(CONST a: ARRAY OF SIGNED32): SIGNED32;   (* відкритий масив *)
VAR i: SIZE; s: SIGNED32;                (* LEN повертає SIZE *)
BEGIN
  FOR i := 0 TO LEN(a) - 1 DO s := s + a[i] END;
  RETURN s
END Sum;

PROCEDURE Swap(VAR a, b: SIGNED32);      (* VAR = за посиланням *)
VAR t: SIGNED32;
BEGIN t := a; a := b; b := t
END Swap;

PROCEDURE Aggregates;
VAR
  fixed: ARRAY 4 OF SIGNED32;
  matrix: ARRAY 3, 3 OF FLOAT64;       (* багатовимірний *)
  dyn: IntArray;
  name: Name;
  p: Point3d;
  pp: PointPtr;
BEGIN
  fixed[0] := 1; fixed[1] := 2;
  matrix[1, 2] := 0.5;
  NEW(dyn, 10);                        (* довжина відома під час виконання *)
  dyn[0] := Sum(fixed);
  Swap(fixed[0], fixed[1]);

  COPY(Greeting, name);                (* безпечне копіювання рядка *)
  Strings0(name);

  p.x := 1; p.y := 2; p.z := 3;        (* Point3d успадковує x та y *)
  origin.x := p.x; origin.y := p.y;

  NEW(pp);                             (* збирач сміття, звільняти не треба *)
  pp.x := 10;
  pp := NIL;
END Aggregates;

PROCEDURE Strings0(VAR name: Name);
VAR i: SIGNED32;
BEGIN
  i := 0;
  WHILE name[i] # 0X DO INC(i) END;    (* рядки завершуються 0X *)
  Log.String(name); Log.Int(i, 1); Log.Ln
END Strings0;

(* ------------------------------------------------------------------ *)
(* Процедури, делегати та обробка пасток                               *)
(* ------------------------------------------------------------------ *)

PROCEDURE Outer(n: SIGNED32): SIGNED32;

  PROCEDURE Inner(k: SIGNED32): SIGNED32;   (* вкладена, бачить n *)
  BEGIN RETURN k * n
  END Inner;

BEGIN
  RETURN Inner(2)
END Outer;

PROCEDURE Print(n: SIGNED32);
BEGIN Log.Int(n, 1); Log.Ln
END Print;

PROCEDURE UseDelegate;
VAR h: Handler;
BEGIN
  h := Print;                          (* звичайна процедура *)
  h(42);
  (* DELEGATE може містити й метод разом з його об'єктом. *)
END UseDelegate;

PROCEDURE MayTrap(a, b: SIGNED32): SIGNED32;
VAR result: SIGNED32;
BEGIN
  result := a DIV b;                   (* пастка, якщо b = 0 *)
  RETURN result
FINALLY                                (* виконується також після пастки *)
  RETURN 0
END MayTrap;

(* ------------------------------------------------------------------ *)
(* Перевизначення операторів                                           *)
(* ------------------------------------------------------------------ *)

OPERATOR "+"* (CONST a, b: Vec2): Vec2;
VAR r: Vec2;
BEGIN
  r.x := a.x + b.x; r.y := a.y + b.y;
  RETURN r
END "+";

(* ------------------------------------------------------------------ *)
(* Об'єкти: методи, конструктор, успадкування                          *)
(* ------------------------------------------------------------------ *)

TYPE
  Shape* = OBJECT
  VAR
    name-: Name;

    PROCEDURE &Init*(CONST n: ARRAY OF CHAR);   (* конструктор *)
    BEGIN
      COPY(n, name)
    END Init;

    PROCEDURE Area*(): FLOAT64;                 (* можна перевизначити *)
    BEGIN
      RETURN 0
    END Area;

    PROCEDURE Describe*;
    BEGIN
      Log.String(name); Log.String(" area=");
      Log.Int(ENTIER(SELF.Area()), 1); Log.Ln   (* SELF — цей об'єкт *)
    END Describe;
  END Shape;

  Circle* = OBJECT (Shape)                      (* одиничне успадкування *)
  VAR radius: FLOAT64;

    PROCEDURE &InitCircle*(r: FLOAT64);
    BEGIN
      Init("circle");                           (* успадкований метод *)
      radius := r
    END InitCircle;

    PROCEDURE Area*(): FLOAT64;                 (* перевизначення *)
    BEGIN
      RETURN 3.14159 * radius * radius
    END Area;
  END Circle;

PROCEDURE UseObjects;
VAR s: Shape; c: Circle;
BEGIN
  NEW(c, 2.0);                (* NEW передає аргументи конструктора *)
  c.Describe;
  s := c;                     (* Circle є Shape *)
  s.Describe;                 (* динамічна диспетчеризація *)

  IF s IS Circle THEN         (* перевірка типу під час виконання *)
    Log.String(s(Circle).name); Log.Ln    (* охоронець типу *)
  END;

  WITH s: Circle DO           (* охоронець типу для цілої області *)
    Log.Int(ENTIER(s.Area()), 1); Log.Ln
  END
END UseObjects;

(* ------------------------------------------------------------------ *)
(* Активні об'єкти — те, за що мову названо *Active* Oberon            *)
(* ------------------------------------------------------------------ *)

TYPE
  (* Об'єкт із тілом BEGIN {ACTIVE} має власний потік, який стартує
     одразу після створення об'єкта. {EXCLUSIVE} перетворює об'єкт на
     монітор: усередині такого блоку одночасно виконується лише одна
     активність, а AWAIT блокує до виконання умови. *)
  Buffer* = OBJECT
  VAR
    data: ARRAY 16 OF SIGNED32;
    head, tail, count: SIGNED32;
    alive: BOOLEAN;

    PROCEDURE &Init*;
    BEGIN
      head := 0; tail := 0; count := 0; alive := TRUE
    END Init;

    PROCEDURE Put*(x: SIGNED32);
    BEGIN {EXCLUSIVE}
      AWAIT(count < LEN(data));         (* чекати, доки з'явиться місце *)
      data[tail] := x;
      tail := (tail + 1) MOD LEN(data);
      INC(count)
    END Put;

    PROCEDURE Get*(): SIGNED32;
    VAR x: SIGNED32;
    BEGIN {EXCLUSIVE}
      AWAIT(count > 0);                 (* чекати на елемент *)
      x := data[head];
      head := (head + 1) MOD LEN(data);
      DEC(count);
      RETURN x
    END Get;

    PROCEDURE Close*;
    BEGIN {EXCLUSIVE}
      alive := FALSE
    END Close;

  BEGIN {ACTIVE}                        (* тіло об'єкта — це потік *)
    WHILE alive DO
      Log.Int(SELF.Get(), 1); Log.Ln
    END
  END Buffer;

PROCEDURE Producer*;
VAR b: Buffer; i: SIGNED32;
BEGIN
  NEW(b);                               (* тут стартує потік *)
  FOR i := 1 TO 5 DO b.Put(i) END;
  b.Close
END Producer;

(* Тілом критичної секції може бути ціла процедура, а простий блок
   BEGIN {EXCLUSIVE} ... END можна написати й усередині процедури. *)

PROCEDURE Guarded(o: Buffer);
BEGIN
  BEGIN {EXCLUSIVE}
    INC(counter)
  END
END Guarded;

(* ------------------------------------------------------------------ *)
(* Математичні масиви з поелементними операторами                      *)
(* ------------------------------------------------------------------ *)

PROCEDURE MathArrays;
VAR
  v: ARRAY [*] OF FLOAT64;         (* одновимірний, відкритий *)
  m: ARRAY [*, *] OF FLOAT64;      (* двовимірний *)
  t: ARRAY [?] OF FLOAT64;         (* тензор довільного рангу *)
  w: ARRAY [3] OF FLOAT64;         (* фіксована довжина *)
  f: FLOAT64;
BEGIN
  NEW(v, 3); NEW(m, 3, 3);
  v := [1.0, 2.0, 3.0];            (* конструктор масиву *)
  v := v .* v;                     (* поелементний добуток *)
  v := 2 * v;                      (* скаляр поширюється на всі елементи *)
  m := m + m;
  v := m * v;                      (* добуток матриці на вектор *)
  f := SUM(v);
  f := v[1];  v[0..1] := v[1..2];  (* діапазони та зрізи *)
  Log.Int(LEN(v, 0), 1); Log.Ln;   (* довжина нульового виміру *)
  Log.Int(DIM(m), 1); Log.Ln       (* кількість вимірів *)
END MathArrays;

(* ------------------------------------------------------------------ *)
(* Команди: процедури, які запускають з оболонки A2                    *)
(* ------------------------------------------------------------------ *)

(* Експортовану процедуру без параметрів або з параметром
   Commands.Context можна запустити з командного рядка чи клацанням
   по будь-якому тексту в системі. *)

PROCEDURE Hello*(context: Commands.Context);
VAR name: Name;
BEGIN
  context.arg.SkipWhitespace;
  context.arg.String(name);                    (* прочитати аргумент *)
  IF name = "" THEN COPY("world", name) END;
  context.out.String(Greeting); context.out.String(", ");
  context.out.String(name); context.out.String("!");
  context.out.Ln;
  context.out.Update                           (* виштовхнути буфер *)
END Hello;

PROCEDURE WriteTo(w: Streams.Writer);
BEGIN
  w.String("Streams.Writer works with files, network and screen");
  w.Ln; w.Update
END WriteTo;

(* Тіло модуля виконується один раз, під час завантаження модуля. *)
BEGIN
  counter := 0;
  KernelLog.String("LearnActiveOberon loaded"); KernelLog.Ln
END LearnActiveOberon.

(* Текст після останньої крапки компілятор ігнорує, тому вихідні тексти
   A2 традиційно завершують командами для збирання й перевірки модуля.
   В A2 їх виконують клацанням середньою кнопкою миші:

Compiler.Compile LearnActiveOberon.Mod ~
LearnActiveOberon.Hello Active Oberon ~
LearnActiveOberon.Producer ~
System.Free LearnActiveOberon ~
*)
```

## Модифікатори у фігурних дужках

Майже кожне оголошення — процедура, тип, змінна, параметр, блок операторів,
комірка — може мати список прапорців у фігурних дужках. Синтаксичний
аналізатор приймає там будь-який ідентифікатор, а компілятор відхиляє ті,
яких не знає.

Паралелізм (тіла об'єктів і блоки операторів):

* `{ACTIVE}` — тіло виконується як власна активність (потік), що стартує
  після створення об'єкта.
* `{EXCLUSIVE}` — блок є критичною секцією: у будь-якому ексклюзивному блоці
  об'єкта одночасно перебуває не більш ніж одна активність.
* `{PRIORITY(n)}` — пріоритет активного тіла.
* `{SAFE}` — активне тіло перезапускається після пастки й опирається
  завершенню.
* `{REALTIME}` — тіло є активністю реального часу й може використовувати
  лише операції, безпечні для реального часу.
* `{UNCOOPERATIVE}` — блок не бере участі в кооперативному плануванні
  (ядро та низькорівневий код).

Об'єктна орієнтація:

* `{ABSTRACT}` — тип-запис / об'єкт або метод без реалізації.
* `{FINAL}` — запис не можна розширити, метод не можна перевизначити.
* `{OVERRIDE}` — явно вказує, що метод перевизначає успадкований (інакше
  компілятор виводить це сам).
* `{DELEGATE}` — процедурний тип, значенням якого може бути й метод разом
  з його об'єктом.
* `{DYNAMIC}` — оператор, що диспетчеризується під час виконання.

Пам'ять і безпека:

* `{UNTRACED}` — змінну-вказівник не відстежує збирач сміття.
* `{UNTRACKED}` — локальні посилання блоку не відстежуються.
* `{UNSAFE}` — `POINTER {UNSAFE} TO ...` — сирий вказівник: сумісний з
  `ADDRESS`, без охоронців типу й перевірок.
* `{UNCHECKED}` — блок компілюється без перевірок NIL, меж та стека.
* `{DISPOSABLE}` — вказівник / об'єкт звільняють через `DISPOSE`, а не
  збирачем сміття.
* `{ALIGNED(n)}` — вирівняти символ на n байтів.
* `{OFFSET(n)}` — розмістити поле або змінну за фіксованим зміщенням.
* `{MOVABLE}` — параметр типу `ADDRESS`, що може вказувати на пам'ять, яку
  збирач сміття переміщує.
* `{REGISTER}` — тримати змінну чи параметр у регістрі, якщо можливо.

Процедури та компонування:

* `{WINAPI}`, `{C}`, `{PlatformCC}` — угода про виклик процедури (типу).
* `{INTERRUPT}` — процедура є обробником переривання.
* `{NORETURN}` — процедура ніколи не повертає керування.
* `{PLAIN}` — без кадру активації, тож без локальних змінних і параметрів.
* `{OPENING}` / `{CLOSING}` — компонувати процедуру перед усіма тілами
  модулів / після них; обидва прапорці означають також `PLAIN`.
* `{ALIGNSTACK}` — вирівняти стек під час входу в процедуру.
* `{PCOFFSET(n)}` — зміщення лічильника команд для процедурного типу
  (генератор коду).
* `{Fingerprint=x}` — зафіксувати відбиток символу замість обчисленого.
* `{TEST}` — позначає тестову процедуру для опції компілятора `--test`.

Active Cells (опис апаратури, генерація для FPGA) — властивості створюваної
комірки чи каналу: `{DataMemorySize(n)}`, `{CodeMemorySize(n)}`,
`{InstructionWidth(n)}`, `{ChannelWidth(n)}`, `{ChannelDepth(n)}`,
`{Channels}`, `{Vector}`, `{FloatingPoint}`, `{NoMul}`,
`{HasNonBlockingIO}`, `{FrequencyDivider(n)}`, `{Engine}`, `{TRM}`, `{TRMS}`,
`{BaseMem}`, `{BaseDiv}`, `{Backend(s)}`, `{Runtime(s)}`.

## Важливі дрібниці

* Ідентифікатори чутливі до регістру. Ключові слова читаються незалежно від
  регістру, а стиль задає розширення файлу: `X.Mod` пишуть ВЕЛИКИМИ
  літерами, `x.mod` — малими.
* `*` після імені експортує його, `-` експортує лише для читання.
* Немає `free`: система має збирач сміття.
* Будь-який модуль можна завантажити й вивантажити під час роботи системи
  (`System.Free Module ~`) — саме так A2 розробляють, не перезапускаючи її.
* `SYSTEM` дає доступ до небезпечних операцій (`SYSTEM.GET`, `SYSTEM.PUT`,
  `SYSTEM.VAL`, `SYSTEM.MOVE`), а `CODE ... END` вбудовує асемблер.
* Умовна компіляція: `#if ... #else ... #end` із символами, які передають
  компіляторові (`--define=UNIX,AMD64`).

## Що читати далі

* [t.me/A2OperatingSystem](https://t.me/A2OperatingSystem) — головний канал
  спільноти. Він переважно російськомовний, але на питання англійською теж
  відповідають.
* [Active Oberon](https://uk.wikipedia.org/wiki/Active_Oberon) та
  [A2](https://uk.wikipedia.org/wiki/A2_%28операційна_система%29) у Вікіпедії
* [Сторінка проєкту A2 в ETH Zürich](http://cas.inf.ethz.ch/projects/a2)
* [Офіційні тексти A2 (ETH GitLab)](https://gitlab.inf.ethz.ch/felixf/oberon)
* [a2oberon](https://gitlab.com/a25665725/a2oberon) — форк, який активно
  розвивають, гілка `dev-andrii`, з повною історією, перенесеною з SVN;
  у каталозі `docs/` лежать Oberon Language Report, короткий посібник і
  стаття про фреймворк паралелізму
* [minia2](https://github.com/active-oberon/minia2) — SDK у стилі Go:
  компілятор, мовний сервер і менеджер пакетів в одному образі Docker.
  Збирає самодостатні консольні програми для Linux і Windows, тож писати
  мовою Active Oberon можна взагалі без встановлення A2
* [a2-registry](https://active-oberon.github.io/a2-registry/) — опис
  модулів A2
* [oberon.org](https://oberon.org/en) — каталог ресурсів про Oberon
