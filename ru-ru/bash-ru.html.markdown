---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
translators:
    - ["Andrey Samsonov", "https://github.com/kryzhovnik"]
    - ["Andre Polykanine", "https://github.com/Oire"]
filename: LearnBash-ru.sh
lang: ru-ru
---

Bash - это командная оболочка unix (unix shell), которая распространялась как оболочка для операционной системы GNU и используется в качестве оболочки по умолчанию для Linux и Mac OS X.
Почти все нижеприведенные примеры могут быть частью shell-скриптов или исполнены напрямую в shell.

[Подробнее.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Первая строка скрипта - это shebang, который сообщает системе, как исполнять
# этот скрипт: http://en.wikipedia.org/wiki/Shebang_(Unix)
# Как вы уже поняли, комментарии начинаются с #. Shebang - тоже комментарий.

# Простой пример hello world:
echo Hello world!

# Отдельные команды начинаются с новой строки или разделяются точкой с запятой:
echo 'Это первая строка'; echo 'Это вторая строка'

# Вот так объявляется переменная:
VARIABLE="Просто строка"

# но не так:
VARIABLE = "Просто строка"
# Bash решит, что VARIABLE - это команда, которую он должен исполнить,
# и выдаст ошибку, потому что не сможет найти ее.

# и не так:
VARIABLE= 'Просто строка'
# Тут Bash решит, что 'Просто строка' - это команда, которую он должен исполнить,
# и выдаст ошибку, потому что не сможет найти такой команды
# (здесь 'VARIABLE=' выглядит как присвоение значения переменной,
# но только в контексте исполнения команды 'Просто строка').

# Использование переменой:
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# Когда вы используете переменную - присваиваете, экспортируете и т.д. -
# пишите её имя без $. А для получения значения переменной используйте $.
# Заметьте, что ' (одинарные кавычки) не раскрывают переменные в них.

# Подстановка строк в переменные
echo ${VARIABLE/Просто/A}
# Это выражение заменит первую встреченную подстроку "Просто" на "A"

# Взять подстроку из переменной
LENGTH=7
echo ${VARIABLE:0:LENGTH}
# Это выражение вернет только первые 7 символов переменной VARIABLE

# Значение по умолчанию
echo ${FOO:-"DefaultValueIfFOOIsMissingOrEmpty"}
# Это сработает при отсутствующем значении (FOO=) и пустой строке (FOO="");
# ноль (FOO=0) вернет 0.
# Заметьте, что в любом случае значение самой переменной FOO не изменится.

# Встроенные переменные:
# В bash есть полезные встроенные переменные, например
echo "Последнее возвращенное значение: $?"
echo "PID скрипта: $$"
echo "Количество аргументов: $#"
echo "Аргументы скрипта: $@"
echo "Аргументы скрипта, распределённые по отдельным переменным: $1 $2..."

# Чтение аргументов из устройста ввода:
echo "Как Вас зовут?"
read NAME # Обратите внимание, что нам не нужно определять новую переменную
echo Привет, $NAME!

# У нас есть обычная структура if:
# наберите 'man test' для получения подробной информации о форматах условия
if [ $NAME -ne $USER ]
then
    echo "Имя не совпадает с именем пользователя"
else
    echo "Имя совпадает с именем пользователя"
fi

# Примечание: если $Name пустой, bash интерпретирует код как:
if [ -ne $USER ]
# а это ошибочная команда
# поэтому такие переменные нужно использовать так:
if [ "$Name" -ne $USER ] ...
# когда $Name пустой, bash видит код как:
if [ "" -ne $USER ] ...
# что работает правильно

# Также есть условное исполнение
echo "Исполнится всегда" || echo "Исполнится, если первая команда завершится ошибкой"
echo "Исполнится всегда" && echo "Исполнится, если первая команда выполнится удачно"

# Можно использовать && и || в выражениях if, когда нужно несколько пар скобок:
if [ $NAME == "Steve" ] && [ $AGE -eq 15 ]
then
    echo "Исполнится, если $NAME равно Steve И $AGE равно 15."
fi

if [ $NAME == "Daniya" ] || [ $NAME == "Zach" ]
then
    echo "Исполнится, если $NAME равно Daniya ИЛИ Zach."
fi

# Выражения обозначаются таким форматом:
echo $(( 10 + 5 ))

# В отличие от других языков программирования, Bash - это командная оболочка,
# а значит, работает в контексте текущей директории.
# Вы можете просматривать файлы и директории в текущей директории командой ls:
ls

# У этой команды есть опции:
ls -l # Показать каждый файл и директорию на отдельной строке

# Результат предыдущей команды может быть направлен на вход следующей.
# Команда grep фильтрует ввод по шаблону.
# Так мы можем просмотреть только *.txt файлы в текущей директории:
ls -l | grep "\.txt"

# Вы можете перенаправить ввод и вывод команды (stdin, stdout и stderr).
# Следующая команда означает: читать из stdin, пока не встретится ^EOF$, и
# перезаписать hello.py следующим строками (до строки "EOF"):
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Запуск hello.py с разными вариантами перенаправления потоков
# стандартных ввода, вывода и ошибок:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# Поток ошибок перезапишет файл, если этот файл существует,
# поэтому, если вы хотите дописывать файл, используйте ">>":
python hello.py >> "output.out" 2>> "error.err"

# Переписать output.txt, дописать error.err и сосчитать строки:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Запустить команду и вывести ее файловый дескриптор (смотрите: man fd)
echo <(echo "#helloworld")

# Перезаписать output.txt строкой "#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Подчистить временные файлы с подробным выводом ('-i' - интерактивый режим)
rm -v output.out error.err output-and-error.log

# Команды могут быть подставлены в строку с помощью $( ):
# следующие команды выводят число файлов и директорий в текущей директории.
echo "Здесь $(ls | wc -l) элементов."

# То же самое можно сделать с использованием обратных кавычек,
# но они не могут быть вложенными, поэтому предпочтительно использовать $( ).
echo "Здесь `ls | wc -l` элементов."

# В Bash есть структура case, которая похожа на switch в Java и C++:
case "$VARIABLE" in 
    # Перечислите шаблоны для условий, которые хотите отловить
    0) echo "Тут ноль.";;
    1) echo "Тут один.";;
    *) echo "Это не пустое значение.";;
esac

# Цикл for перебирает элементы переданные в аргументе:
# Содержимое $VARIABLE будет напечатано три раза.
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# Или с использованием "традиционного" синтаксиса цикла for:
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Цикл for можно использовать для действий с файлами.
# Запустим команду 'cat' для файлов file1 и file2
for VARIABLE in file1 file2
do
    cat "$VARIABLE"
done

# ... или выводом из команд
# Запустим cat для вывода из ls.
for OUTPUT in $(ls)
do
    cat "$OUTPUT"
done

# Цикл while:
while [ true ]
do
    echo "тело цикла здесь..."
    break
done

# Вы можете определять функции
# Определение:
function foo ()
{
    echo "Аргументы работают также, как аргументы скрипта: $@"
    echo "и: $1 $2..."
    echo "Это функция"
    return 0
}

# или просто
bar ()
{
    echo "Другой способ определить функцию!"
    return 0
}

# Вызов функции
foo "Мое имя" $NAME

# Есть много полезных команд, которые нужно знать:
# напечатать последние 10 строк файла file.txt
tail -n 10 file.txt
# напечатать первые 10 строк файла file.txt
head -n 10 file.txt
# отсортировать строки file.txt
sort file.txt
# отобрать или наоборот пропустить повторяющиеся строки (с опцией -d отбирает)
uniq -d file.txt
# напечатать только первую колонку перед символом ','
cut -d ',' -f 1 file.txt
# заменить каждое 'okay' на 'great' в файле file.txt (regex поддерживается)
sed -i 's/okay/great/g' file.txt
# вывести в stdout все строки из file.txt, совпадающие с шаблоном regex;
# этот пример выводит строки, которые начинаются на "foo" и оканчиваются "bar"
grep "^foo.*bar$" file.txt
# передайте опцию -c чтобы вывести число строк, в которых совпал шаблон
grep -c "^foo.*bar$" file.txt
# чтобы искать по строке, а не шаблону regex, используйте fgrep (или grep -F)
fgrep "^foo.*bar$" file.txt 

# Читайте встроенную документацию оболочки Bash командой 'help':
help
help help
help for
help return
help source
help .

# Читайте Bash man-документацию
apropos bash
man 1 bash
man bash

# Читайте документацию info (? для помощи)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Читайте bash info документацию:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
