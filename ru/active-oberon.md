---
contributors:
    - ["Andrii Puhachenko", "https://github.com/andrqxa"]
translators:
    - ["Andrii Puhachenko", "https://github.com/andrqxa"]
---

**Active Oberon** — последний представитель семейства Pascal / Modula-2 /
Oberon, созданный в ETH Zürich (1996-1998, группой вокруг Никлауса Вирта и
Юрга Гуткнехта). Он сохраняет всё, чем известен Oberon — грамматику, которая
умещается на одну страницу, модули с явным экспортом, строгую статическую
типизацию и сборщик мусора — и добавляет то, от чего происходит его название:
**активные объекты**.

У объекта может быть собственный поток (`BEGIN {ACTIVE}`), любой блок можно
объявить критической секцией (`BEGIN {EXCLUSIVE}`), а `AWAIT(условие)`
заменяет условные переменные. Параллелизм — часть языка, а не библиотека.
Кроме того, язык получил перегрузку операторов, перечисления, математические
(тензорные) массивы с поэлементными операциями и ячейки (cells) для описания
аппаратуры.

Вся [операционная система
A2](https://ru.wikipedia.org/wiki/A2_%28операционная_система%29) — ядро,
драйверы, компилятор, сетевой стек и графический интерфейс — написана на
Active Oberon, и именно A2 — естественное место, чтобы попробовать язык.

Приведённый ниже код принимает текущий компилятор Fox из A2. Более старые
тексты A2 / Bluebottle используют исторические имена типов (`LONGINT`,
`LONGREAL`, `SHORTINT`) там, где современный код пишет `SIGNED32`, `FLOAT64`,
`SIGNED8`.

```componentpascal
(*  Комментарии выглядят так, и (* они вкладываются *).  *)
(** Комментарий с двумя звёздочками — это документация, её собирают
  инструменты документирования. *)

(* Единица компиляции — всегда MODULE. Ключевые слова пишутся заглавными
   буквами, идентификаторы чувствительны к регистру, а имя модуля
   повторяется после последнего END. Файл обычно называют так же, как
   модуль. *)
MODULE LearnActiveOberon;

(* Соглашение о регистре: сканер читает ключевые слова независимо от
   регистра, а стиль файла задаётся его расширением. Файл X.Mod пишут
   ЗАГЛАВНЫМИ буквами, файл x.mod — строчными, вместе с предобъявленными
   типами и встроенными процедурами:

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
  KernelLog,            (* вывод в системный журнал *)
  Streams,              (* абстракция Reader / Writer *)
  Commands,             (* контекст команды: ввод, вывод, аргументы *)
  Log := KernelLog;     (* импорт можно переименовать *)

CONST
  Max* = 100;           (* "*" экспортирует идентификатор *)
  Greeting = "Hello";   (* без пометки: видно только в этом модуле *)
  EvenBits = {0, 2, 4, 6};                  (* константа-множество *)

TYPE
  Name* = ARRAY 32 OF CHAR;                 (* строка, оканчивающаяся 0X *)
  Handler* = PROCEDURE {DELEGATE} (n: SIGNED32);

  Point* = RECORD
    x*, y*: SIGNED32;
  END;

  Point3d* = RECORD (Point)                 (* расширение записи *)
    z*: SIGNED32;
  END;

  Color* = ENUM Red*, Green*, Blue* END;    (* перечислимый тип *)

  Vec2* = RECORD x*, y*: FLOAT64 END;

VAR
  counter-: SIGNED32;   (* "-" экспортирует только для чтения *)
  origin: Point;

(* ------------------------------------------------------------------ *)
(* Типы, литералы и операции                                           *)
(* ------------------------------------------------------------------ *)

PROCEDURE Basics;
VAR
  b: BOOLEAN;
  c: CHAR;
  i8: SIGNED8; i16: SIGNED16; i32: SIGNED32; i64: SIGNED64;
  u32: UNSIGNED32;
  f32: FLOAT32; f64: FLOAT64;
  z: COMPLEX64;
  s: SET;                        (* небольшое битовое множество *)
  adr: ADDRESS; size: SIZE;      (* целые размером с указатель *)
  any: ANY;                      (* ссылка на любой объект *)
BEGIN
  (* INTEGER, REAL и SET — варианты ширины по умолчанию; код A2 обычно
     указывает ширину явно: SIGNED32, FLOAT64, SET32, ... *)
  i32 := 42;  i32 := 2AH;  i32 := 0x2A;  i32 := 0b101010;
  i32 := 1'000'000;              (* разделители разрядов разрешены *)
  i8 := -1; i16 := 1000; i64 := 1; u32 := 0FFFFFFFFH;
  c := "A";  c := 41X;           (* символ, шестнадцатеричный символ *)
  f32 := 1.5;  f64 := 1.5E-3;
  z := 1 + 2*IMAG;               (* IMAG — мнимая единица *)
  f64 := RE(z);  f64 := IM(z);

  (* Арифметика: + - * / DIV MOD, целочисленное деление — это DIV *)
  i32 := 7 DIV 2;  i32 := 7 MOD 2;  f64 := 7 / 2;
  i32 := ABS(-3);  INC(i32);  DEC(i32, 2);

  (* Отношения: = # < <= > >= IN IS. Логические: & OR ~ (не) *)
  b := (i32 = 5) OR (i32 # 6) & ~(i32 < 0);

  (* Множества *)
  s := {0, 3, 5..7};  s := s + {1};  s := s * EvenBits;  s := s - {3};
  b := 3 IN s;  INCL(s, 9);  EXCL(s, 0);

  (* Преобразования типов всегда явные *)
  i32 := ORD("A");  c := CHR(65);  f64 := i32;  i32 := ENTIER(f64);
  adr := ADDRESSOF(i32);  size := SIZEOF(Point);

  any := NIL;
  ASSERT(i32 >= 0);              (* ловушка (trap), если условие ложно *)
END Basics;

(* ------------------------------------------------------------------ *)
(* Управление выполнением                                              *)
(* ------------------------------------------------------------------ *)

PROCEDURE ControlFlow(n: SIGNED32): SIGNED32;
VAR i, sum: SIGNED32; c: CHAR; color: Color;
BEGIN
  IF n < 0 THEN
    n := -n
  ELSIF n = 0 THEN
    n := 1
  ELSE
    (* ничего *)
  END;

  c := "x";
  CASE c OF
      "a".."m": sum := 1
    | "n".."z": sum := 2
  ELSE
    sum := 0
  END;

  color := Color.Green;          (* элементы перечисления квалифицированы *)
  IF color = Color.Green THEN sum := sum + 1 END;

  WHILE n > 1 DO n := n DIV 2 END;

  REPEAT INC(n) UNTIL n >= 4;

  FOR i := 0 TO 10 BY 2 DO sum := sum + i END;

  LOOP
    INC(i);
    IF i > 100 THEN EXIT END
  END;

  VAR local := sum + 1;          (* объявление VAR внутри блока *)
  RETURN local
END ControlFlow;

(* ------------------------------------------------------------------ *)
(* Массивы, строки, записи и указатели                                 *)
(* ------------------------------------------------------------------ *)

TYPE
  IntArray* = POINTER TO ARRAY OF SIGNED32;    (* динамический массив *)
  PointPtr* = POINTER TO Point;

PROCEDURE Sum(CONST a: ARRAY OF SIGNED32): SIGNED32;   (* открытый массив *)
VAR i: SIZE; s: SIGNED32;                (* LEN возвращает SIZE *)
BEGIN
  FOR i := 0 TO LEN(a) - 1 DO s := s + a[i] END;
  RETURN s
END Sum;

PROCEDURE Swap(VAR a, b: SIGNED32);      (* VAR = по ссылке *)
VAR t: SIGNED32;
BEGIN t := a; a := b; b := t
END Swap;

PROCEDURE Aggregates;
VAR
  fixed: ARRAY 4 OF SIGNED32;
  matrix: ARRAY 3, 3 OF FLOAT64;       (* многомерный *)
  dyn: IntArray;
  name: Name;
  p: Point3d;
  pp: PointPtr;
BEGIN
  fixed[0] := 1; fixed[1] := 2;
  matrix[1, 2] := 0.5;
  NEW(dyn, 10);                        (* длина известна во время работы *)
  dyn[0] := Sum(fixed);
  Swap(fixed[0], fixed[1]);

  COPY(Greeting, name);                (* безопасное копирование строки *)
  Strings0(name);

  p.x := 1; p.y := 2; p.z := 3;        (* Point3d наследует x и y *)
  origin.x := p.x; origin.y := p.y;

  NEW(pp);                             (* сборка мусора, освобождать не надо *)
  pp.x := 10;
  pp := NIL;
END Aggregates;

PROCEDURE Strings0(VAR name: Name);
VAR i: SIGNED32;
BEGIN
  i := 0;
  WHILE name[i] # 0X DO INC(i) END;    (* строки оканчиваются 0X *)
  Log.String(name); Log.Int(i, 1); Log.Ln
END Strings0;

(* ------------------------------------------------------------------ *)
(* Процедуры, делегаты и обработка ловушек                             *)
(* ------------------------------------------------------------------ *)

PROCEDURE Outer(n: SIGNED32): SIGNED32;

  PROCEDURE Inner(k: SIGNED32): SIGNED32;   (* вложенная, видит n *)
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
  h := Print;                          (* обычная процедура *)
  h(42);
  (* DELEGATE может хранить и метод вместе с его объектом. *)
END UseDelegate;

PROCEDURE MayTrap(a, b: SIGNED32): SIGNED32;
VAR result: SIGNED32;
BEGIN
  result := a DIV b;                   (* ловушка, если b = 0 *)
  RETURN result
FINALLY                                (* выполняется и после ловушки *)
  RETURN 0
END MayTrap;

(* ------------------------------------------------------------------ *)
(* Перегрузка операторов                                               *)
(* ------------------------------------------------------------------ *)

OPERATOR "+"* (CONST a, b: Vec2): Vec2;
VAR r: Vec2;
BEGIN
  r.x := a.x + b.x; r.y := a.y + b.y;
  RETURN r
END "+";

(* ------------------------------------------------------------------ *)
(* Объекты: методы, конструктор, наследование                          *)
(* ------------------------------------------------------------------ *)

TYPE
  Shape* = OBJECT
  VAR
    name-: Name;

    PROCEDURE &Init*(CONST n: ARRAY OF CHAR);   (* конструктор *)
    BEGIN
      COPY(n, name)
    END Init;

    PROCEDURE Area*(): FLOAT64;                 (* можно переопределить *)
    BEGIN
      RETURN 0
    END Area;

    PROCEDURE Describe*;
    BEGIN
      Log.String(name); Log.String(" area=");
      Log.Int(ENTIER(SELF.Area()), 1); Log.Ln   (* SELF — этот объект *)
    END Describe;
  END Shape;

  Circle* = OBJECT (Shape)                      (* одиночное наследование *)
  VAR radius: FLOAT64;

    PROCEDURE &InitCircle*(r: FLOAT64);
    BEGIN
      Init("circle");                           (* унаследованный метод *)
      radius := r
    END InitCircle;

    PROCEDURE Area*(): FLOAT64;                 (* переопределение *)
    BEGIN
      RETURN 3.14159 * radius * radius
    END Area;
  END Circle;

PROCEDURE UseObjects;
VAR s: Shape; c: Circle;
BEGIN
  NEW(c, 2.0);                (* NEW передаёт аргументы конструктора *)
  c.Describe;
  s := c;                     (* Circle является Shape *)
  s.Describe;                 (* динамическая диспетчеризация *)

  IF s IS Circle THEN         (* проверка типа во время выполнения *)
    Log.String(s(Circle).name); Log.Ln    (* охранник типа *)
  END;

  WITH s: Circle DO           (* охранник типа для целой области *)
    Log.Int(ENTIER(s.Area()), 1); Log.Ln
  END
END UseObjects;

(* ------------------------------------------------------------------ *)
(* Активные объекты — то, за что язык назван *Active* Oberon           *)
(* ------------------------------------------------------------------ *)

TYPE
  (* Объект с телом BEGIN {ACTIVE} имеет собственный поток, который
     стартует сразу после создания объекта. {EXCLUSIVE} превращает
     объект в монитор: внутри такого блока одновременно выполняется
     только одна активность, а AWAIT блокирует до истинности условия. *)
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
      AWAIT(count < LEN(data));         (* ждать, пока появится место *)
      data[tail] := x;
      tail := (tail + 1) MOD LEN(data);
      INC(count)
    END Put;

    PROCEDURE Get*(): SIGNED32;
    VAR x: SIGNED32;
    BEGIN {EXCLUSIVE}
      AWAIT(count > 0);                 (* ждать элемент *)
      x := data[head];
      head := (head + 1) MOD LEN(data);
      DEC(count);
      RETURN x
    END Get;

    PROCEDURE Close*;
    BEGIN {EXCLUSIVE}
      alive := FALSE
    END Close;

  BEGIN {ACTIVE}                        (* тело объекта — это поток *)
    WHILE alive DO
      Log.Int(SELF.Get(), 1); Log.Ln
    END
  END Buffer;

PROCEDURE Producer*;
VAR b: Buffer; i: SIGNED32;
BEGIN
  NEW(b);                               (* здесь стартует поток *)
  FOR i := 1 TO 5 DO b.Put(i) END;
  b.Close
END Producer;

(* Критической секцией может быть целая процедура, а простой блок
   BEGIN {EXCLUSIVE} ... END можно написать и внутри процедуры. *)

PROCEDURE Guarded(o: Buffer);
BEGIN
  BEGIN {EXCLUSIVE}
    INC(counter)
  END
END Guarded;

(* ------------------------------------------------------------------ *)
(* Математические массивы с поэлементными операциями                   *)
(* ------------------------------------------------------------------ *)

PROCEDURE MathArrays;
VAR
  v: ARRAY [*] OF FLOAT64;         (* одномерный, открытый *)
  m: ARRAY [*, *] OF FLOAT64;      (* двумерный *)
  t: ARRAY [?] OF FLOAT64;         (* тензор произвольного ранга *)
  w: ARRAY [3] OF FLOAT64;         (* фиксированная длина *)
  f: FLOAT64;
BEGIN
  NEW(v, 3); NEW(m, 3, 3);
  v := [1.0, 2.0, 3.0];            (* конструктор массива *)
  v := v .* v;                     (* поэлементное произведение *)
  v := 2 * v;                      (* скаляр распространяется на элементы *)
  m := m + m;
  v := m * v;                      (* произведение матрицы на вектор *)
  f := SUM(v);
  f := v[1];  v[0..1] := v[1..2];  (* диапазоны и срезы *)
  Log.Int(LEN(v, 0), 1); Log.Ln;   (* длина нулевого измерения *)
  Log.Int(DIM(m), 1); Log.Ln       (* количество измерений *)
END MathArrays;

(* ------------------------------------------------------------------ *)
(* Команды: процедуры, запускаемые из оболочки A2                      *)
(* ------------------------------------------------------------------ *)

(* Экспортируемую процедуру без параметров или с параметром
   Commands.Context можно запустить из командной строки или щелчком
   по любому тексту в системе. *)

PROCEDURE Hello*(context: Commands.Context);
VAR name: Name;
BEGIN
  context.arg.SkipWhitespace;
  context.arg.String(name);                    (* прочитать аргумент *)
  IF name = "" THEN COPY("world", name) END;
  context.out.String(Greeting); context.out.String(", ");
  context.out.String(name); context.out.String("!");
  context.out.Ln;
  context.out.Update                           (* вытолкнуть буфер *)
END Hello;

PROCEDURE WriteTo(w: Streams.Writer);
BEGIN
  w.String("Streams.Writer works with files, network and screen");
  w.Ln; w.Update
END WriteTo;

(* Тело модуля выполняется один раз, при загрузке модуля. *)
BEGIN
  counter := 0;
  KernelLog.String("LearnActiveOberon loaded"); KernelLog.Ln
END LearnActiveOberon.

(* Текст после последней точки компилятор игнорирует, поэтому исходные
   тексты A2 традиционно заканчиваются командами сборки и проверки
   модуля. В A2 их выполняют щелчком средней кнопки мыши:

Compiler.Compile LearnActiveOberon.Mod ~
LearnActiveOberon.Hello Active Oberon ~
LearnActiveOberon.Producer ~
System.Free LearnActiveOberon ~
*)
```

## Модификаторы в фигурных скобках

Почти каждое объявление — процедура, тип, переменная, параметр, блок
операторов, ячейка — может нести список флагов в фигурных скобках. Парсер
принимает там любой идентификатор, а компилятор отвергает те, которых не
знает.

Параллелизм (тела объектов и блоки операторов):

* `{ACTIVE}` — тело выполняется как собственная активность (поток), которая
  стартует при создании объекта.
* `{EXCLUSIVE}` — блок является критической секцией: в любом эксклюзивном
  блоке объекта одновременно находится не более одной активности.
* `{PRIORITY(n)}` — приоритет активного тела.
* `{SAFE}` — активное тело перезапускается после ловушки и сопротивляется
  завершению.
* `{REALTIME}` — тело является активностью реального времени и может
  использовать только операции, безопасные для реального времени.
* `{UNCOOPERATIVE}` — блок не участвует в кооперативном планировании (ядро
  и низкоуровневый код).

Объектная ориентация:

* `{ABSTRACT}` — тип-запись / объект или метод без реализации.
* `{FINAL}` — запись нельзя расширить, метод нельзя переопределить.
* `{OVERRIDE}` — явно указывает, что метод переопределяет унаследованный
  (иначе компилятор выводит это сам).
* `{DELEGATE}` — процедурный тип, значением которого может быть и метод
  вместе с его объектом.
* `{DYNAMIC}` — оператор, диспетчеризуемый во время выполнения.

Память и безопасность:

* `{UNTRACED}` — переменную-указатель не отслеживает сборщик мусора.
* `{UNTRACKED}` — локальные ссылки блока не отслеживаются.
* `{UNSAFE}` — `POINTER {UNSAFE} TO ...` — сырой указатель: совместим с
  `ADDRESS`, без охранников типа и проверок.
* `{UNCHECKED}` — блок компилируется без проверок NIL, границ и стека.
* `{DISPOSABLE}` — указатель / объект освобождают через `DISPOSE`, а не
  сборщиком мусора.
* `{ALIGNED(n)}` — выровнять символ по n байтам.
* `{OFFSET(n)}` — разместить поле или переменную по фиксированному смещению.
* `{MOVABLE}` — параметр типа `ADDRESS`, который может указывать на память,
  перемещаемую сборщиком мусора.
* `{REGISTER}` — держать переменную или параметр в регистре, если возможно.

Процедуры и компоновка:

* `{WINAPI}`, `{C}`, `{PlatformCC}` — соглашение о вызове процедуры (типа).
* `{INTERRUPT}` — процедура является обработчиком прерывания.
* `{NORETURN}` — процедура никогда не возвращает управление.
* `{PLAIN}` — без кадра активации, а значит без локальных переменных и
  параметров.
* `{OPENING}` / `{CLOSING}` — компоновать процедуру перед всеми телами
  модулей / после них; оба флага подразумевают также `PLAIN`.
* `{ALIGNSTACK}` — выровнять стек при входе в процедуру.
* `{PCOFFSET(n)}` — смещение счётчика команд для процедурного типа
  (генератор кода).
* `{Fingerprint=x}` — зафиксировать отпечаток символа вместо вычисленного.
* `{TEST}` — помечает тестовую процедуру для опции компилятора `--test`.

Active Cells (описание аппаратуры, генерация для FPGA) — свойства
создаваемой ячейки или канала: `{DataMemorySize(n)}`, `{CodeMemorySize(n)}`,
`{InstructionWidth(n)}`, `{ChannelWidth(n)}`, `{ChannelDepth(n)}`,
`{Channels}`, `{Vector}`, `{FloatingPoint}`, `{NoMul}`,
`{HasNonBlockingIO}`, `{FrequencyDivider(n)}`, `{Engine}`, `{TRM}`, `{TRMS}`,
`{BaseMem}`, `{BaseDiv}`, `{Backend(s)}`, `{Runtime(s)}`.

## Важные мелочи

* Идентификаторы чувствительны к регистру. Ключевые слова читаются независимо
  от регистра, а стиль задаётся расширением файла: `X.Mod` пишут ЗАГЛАВНЫМИ
  буквами, `x.mod` — строчными.
* `*` после имени экспортирует его, `-` экспортирует только для чтения.
* Нет `free`: система использует сборщик мусора.
* Любой модуль можно загрузить и выгрузить во время работы системы
  (`System.Free Module ~`) — именно так A2 разрабатывают, не перезапуская её.
* `SYSTEM` даёт доступ к небезопасным операциям (`SYSTEM.GET`, `SYSTEM.PUT`,
  `SYSTEM.VAL`, `SYSTEM.MOVE`), а `CODE ... END` встраивает ассемблер.
* Условная компиляция: `#if ... #else ... #end` с символами, которые передают
  компилятору (`--define=UNIX,AMD64`).

## Что читать дальше

* [t.me/A2OperatingSystem](https://t.me/A2OperatingSystem) — основной канал
  сообщества. Он преимущественно русскоязычный, но на вопросы на английском
  тоже отвечают.
* [Active Oberon](https://ru.wikipedia.org/wiki/Active_Oberon) и
  [A2](https://ru.wikipedia.org/wiki/A2_%28операционная_система%29) в
  Википедии
* [Страница проекта A2 в ETH Zürich](http://cas.inf.ethz.ch/projects/a2)
* [Официальные тексты A2 (ETH GitLab)](https://gitlab.inf.ethz.ch/felixf/oberon)
* [a2oberon](https://gitlab.com/a25665725/a2oberon) — активно развиваемый
  форк, ветка `dev-andrii`, с полной историей, перенесённой из SVN; в
  каталоге `docs/` лежат Oberon Language Report, краткое руководство и
  статья о фреймворке параллелизма
* [minia2](https://github.com/active-oberon/minia2) — SDK в стиле Go:
  компилятор, языковой сервер и менеджер пакетов в одном образе Docker.
  Собирает самодостаточные консольные программы для Linux и Windows, так
  что писать на Active Oberon можно вообще без установки A2
* [a2-registry](https://active-oberon.github.io/a2-registry/) — описание
  модулей A2
* [oberon.org](https://oberon.org/en) — каталог ресурсов об Oberon
