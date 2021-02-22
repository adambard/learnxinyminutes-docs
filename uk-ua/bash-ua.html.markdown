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
    - ["Ehreshi Ivan", "https://github.com/IvanEh"]
    - ["Serhii Maksymchuk", "https://github.com/Serg-Maximchuk"]
lang: uk-ua
---

Bash - командна оболонка unix (unix shell), що також розповсюджувалась як оболонка для
операційної системи GNU і зараз використовується як командна оболонка за замовчуванням
для Linux i Max OS X.
Майже всі приклади, що наведені нижче можуть бути частиною shell-скриптів або
виконані в оболонці

[Більш детально тут.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Перший рядок скрипта - це shebang, який вказує системі, як потрібно виконувати
# скрипт. Як ви вже зрозуміли, коментарі починаються з #. Shebang - також коментар

# Простий приклад hello world:
echo Hello world!

# Окремі команди починаються з нового рядка або розділяються крапкою з комкою:
echo 'Перший рядок'; echo 'Другий рядок'

# Оголошення змінної
VARIABLE="Просто рядок"

# Але не так!
VARIABLE = "Просто рядок"
# Bash вирішить, що VARIABLE - це команда, яку він може виконати,
# і видасть помилку, тому що не зможе знайти її

# І так також не можна писати:
VARIABLE= 'Просто рядок'
# Bash сприйме рядок 'Просто рядок' як команду. Але такої команди не має, тому
# видасть помилку. 
# (тут 'VARIABLE=' інтерпретується як присвоєння тільки в контексті
# виконання команди 'Просто рядок')

# Використання змінних:
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# Коли ви використовуєте змінну - присвоюєте значення, експортуєте і т.д. -
# пишіть її імя без $. А для отримання значення змінної використовуйте $.
# Одинарні лапки ' не розкривають значення змінних

# Підстановка рядків в змінні
echo ${VARIABLE/Просто/A}
# Цей вираз замінить перше входження підрядка "Просто" на "А"

# Отримання підрядка із рядка
LENGTH=7
echo ${VARIABLE:0:LENGTH}
# Цей вираз поверне тільки перші 7 символів змінної VARIABLE

# Значення за замовчуванням
echo ${FOO:-"DefaultValueIfFOOIsMissingOrEmpty"}
# Це спрацює при відсутності значення (FOO=) і при пустому рядку (FOO="")
# Нуль (FOO=0) поверне 0.
# Зауважте, що у всіх випадках значення самої змінної FOO не зміниться

# Вбудовані змінні:
# В bash є корисні вбудовані змінні, наприклад
echo "Значення, яке було повернуте в останній раз: $?"
echo "PID скрипта: $$"
echo "Кількість аргументів: $#"
echo "Аргументи скрипта: $@"
echo "Аргументи скрипта, розподілені по різним змінним: $1 $2..."

# Зчитування змінних з пристроїв  введення
echo "Як вас звати?"
read NAME # Зверніть увагу, що вам не потрібно оголошувати нову змінну
echo Привіт, $NAME!

# В bash є звичайна умовна конструкція if:
# наберіть 'man test', щоб переглянути детальну інформацію про формати умов
if [ $NAME -ne $USER ]
then
    echo "Ім’я користувача не збігається з введеним"
else
    echo "Ім’я збігаєтьяс з іменем користувача"
fi

# Зауважте! якщо $Name пуста, bash інтерпретує код вище як:
if [ -ne $USER ]
# що є неправильним синтаксисом
# тому безпечний спосіб використання потенційно пустих змінних має вигляд:
if [ "$Name" -ne $USER ] ...
# коли $Name пуста, інтерпретується наступним чином:
if [ "" -ne $USER ] ...
# що працює як і очікувалося

# Умовне виконання (conditional execution)
echo "Виконується завжди" || echo "Виконається, якщо перша команда завершиться з помилкою"
echo "Виконується завжди" && echo "Виконається, якщо перша команда завершиться успішно"

# Щоб використати && і || у конструкції if, потрібно декілька пар дужок:
if [ $NAME == "Steve" ] && [ $AGE -eq 15 ]
then
    echo "Виконається, якщо $NAME="Steve" i AGE=15."
fi

if [ $NAME == "Daniya" ] || [ $NAME == "Zach" ]
then
    echo "Виконається, якщо NAME="Steve" або NAME="Zach"."
fi

# Вирази позначаються наступним форматом:
echo $(( 10 + 5 ))

# На відміну від інших мов програмування, Bash - це командна оболонка, а
# отже, працює в контексті поточної директорії
ls

# Ця команда може використовуватися з опціями
ls -l # Показати кожен файл і директорію на окремому рядку

# Результат попередньої команди можна перенаправити на вхід наступної.
# Команда grep фільтрує вхід по шаблону.
# Таким чином ми можемо переглянути тільки *.txt файли в поточній директорії:
ls -l | grep "\.txt"

# Ви можете перенаправити вхід і вихід команди (stdin, stdout, stderr).
# Наступна команда означає: читати із stdin, поки не зустрінеться ^EOF$, і
# перезаписати hello.py наступними рядками (до рядка "EOF"):
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Запуск hello.py з різними варіантами перенаправлення stdin, 
# stdout, stderr (стандартні потоки введення, виведення і помилок):
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# Потік помилок перезапише файл, якщо цей файл існує
# тому, якщо ви хочете дописувати до файлу, використовуйте ">>":
python hello.py >> "output.out" 2>> "error.err"

# Перезаписати output.txt, дописати error.err і порахувати кількість рядків:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Запустити команду і вивести її файловий дескриптор (див.: man fd; наприклад /dev/fd/123)
echo <(echo "#helloworld")

# Перезаписати output.txt рядком "#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Очистити тимчасові файли з детальним виводом (додайте '-i'
# для інтерактивного режиму)
rm -v output.out error.err output-and-error.log

# Команди можуть бути підставлені в інші команди використовуючи $():
# наступна команда виводить кількість файлів і директорій в поточній директорії
echo "Тут $(ls | wc -l) елементів."

# Те саме можна зробити використовуючи зворотні лапки
# Але вони не можуть бути вкладеними, тому перший варіант бажаніший
echo "Тут `ls | wc -l` елементів."

# В Bash є структура case, яка схожа на switch в Java и C++:
case "$VARIABLE" in 
    # перерахуйте шаблони, які будуть використовуватися в якості умов
    0) echo "Тут нуль.";;
    1) echo "Тут один.";;
    *) echo "Не пусте значення.";;
esac

# Цикл for перебирає елементи передані в аргумент:
# Значення $VARIABLE буде надруковано тричі.
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# Aбо можна використати звичний синтаксис for:
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Цикл for можно використати, щоб виконувати дії над файлами.
# Цей код запустить команду 'cat' для файлів file1 и file2
for VARIABLE in file1 file2
do
    cat "$VARIABLE"
done

# ... або дії над виводом команд
# Запустимо cat для виведення із ls.
for OUTPUT in $(ls)
do
    cat "$OUTPUT"
done

# Цикл while:
while [ true ]
do
    echo "Тіло циклу..."
    break
done

# Ви також можете оголосити функцію
# Оголошення:
function foo ()
{
    echo "Аргументи функції доступні так само, як і аргументи скрипта: $@"
    echo "$1 $2..."
    echo "Це функція"
    return 0
}

# Або просто
bar ()
{
    echo "Інший спосіб оголошення функцій!"
    return 0
}

# Виклик функцій
foo "Мое имя" $NAME

# Є багато корисних команд:
# вивести останні 10 рядків файла file.txt
tail -n 10 file.txt
# вивести перші 10 рядків файла file.txt
head -n 10 file.txt
# відсортувати рядки file.txt
sort file.txt
# відібрати або пропустити рядки, що дублюються (з опцією -d відбирає)
uniq -d file.txt
# вивести тільки першу колонку перед символом ','
cut -d ',' -f 1 file.txt
# замінити кожне 'okay' на 'great' у файлі file.txt (підтримується regex)
sed -i 's/okay/great/g' file.txt
# вивести в stdout всі рядки з file.txt, що задовольняють шаблону regex;
# цей приклад виводить рядки, що починаються на foo і закінчуються на bar:
grep "^foo.*bar$" file.txt
# використайте опцію -c, щоб вивести кількість входжень
grep -c "^foo.*bar$" file.txt
# щоб здійснити пошук по рядку, а не по шаблону regex, використовуйте fgrea (або grep -F)
fgrep "^foo.*bar$" file.txt 

# Читайте вбудовану документацію Bash командою 'help':
help
help help
help for
help return
help source
help .

# Читайте Bash man-документацію
apropos bash
man 1 bash
man bash

# Читайте документацію info (? для допомоги)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Читайте bash info документацію:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
