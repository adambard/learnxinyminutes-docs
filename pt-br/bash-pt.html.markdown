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
translators:
    - ["Davidson Mizael", "https://github.com/davidsonmizael"]
filename: LearnBash-pt_br.sh
lang: pt-br
---

Tutorial de shell em português

Bash é o nome do shell do Unix, que também é distribuido como shell do sistema 
operacional GNU e como shell padrão para Linux e Mac OS X. Praticamente todos 
os exemplos abaixo podem fazer parte de um shell script e pode ser executados
diretamente no shell.

[Leia mais sobre](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# A primeira linha do script é o shebang, que conta para o sistema como executar
# o script: http://en.wikipedia.org/wiki/Shebang_(Unix)
# Como você já deve ter percebido, comentários começam com #. 
# Shebang também é um comentário.

# Exemplo simples de hello world:
echo Hello World!

# Cada comando começa com uma nova linha, ou após um ponto virgula:
echo 'Essa é a primeira linha'; echo 'Essa é a segunda linha'

# A declaração de variáveis é mais ou menos assim
Variavel="Alguma string"

# Mas não assim:
Variavel = "Alguma string"
# Bash interpretará Variavel como um comando e tentará executar e lhe retornar
# um erro porque o comando não pode ser encontrado.

# Ou assim:
Variavel= 'Alguma string'
# Bash interpretará 'Alguma string' como um comando e tentará executar e lhe retornar
# um erro porque o comando não pode ser encontrado. (Nesse caso a a parte 'Variavel=' 
# é vista com uma declaração de variável valida apenas para o escopo do comando 'Uma string').

# Usando a variável:
echo $Variavel
echo "$Variavel"
echo '$Variavel'
# Quando você usa a variável em si — declarando valor, exportando, etc — você escreve
# seu nome sem o $. Se você quer usar o valor da variável você deve usar o $.
# Note que ' (aspas simples) não expandirão as variáveis!

# Substituição de strings em variáveis
echo ${Variavel/Alguma/Uma}
# Isso substituirá a primeira ocorrência de "Alguma" por "Uma"

# Substring de uma variável
Tamanho=7
echo ${Variavel:0:Tamanho}
# Isso retornará apenas os 7 primeiros caractéres da variável

# Valor padrão de uma variável
echo ${Foo:-"ValorPadraoSeFooNaoExistirOuEstiverVazia"}
# Isso funciona para nulo (Foo=) e (Foo=""); zero (Foo=0) retorna 0.
# Note que isso apenas retornar o valor padrão e não mudar o valor da variável.

# Variáveis internas
# Tem algumas variáveis internas bem uteis, como
echo "O ultimo retorno do programa: $?"
echo "PID do script: $$"
echo "Numero de argumentos passados para o script $#"
echo "Todos os argumentos passados para o script $@"
echo "Os argumentos do script em variáveis diferentes: $1, $2..."

# Lendo o valor do input:
echo "Qual o seu nome?"
read Nome # Note que nós não precisamos declarar a variável
echo Ola, $Nome

# Nós temos a estrutura if normal:
# use 'man test' para mais infomações para as condicionais
if [ $Nome -ne $USER ]
then
	echo "Seu nome não é o seu username"
else
	echo "Seu nome é seu username"
fi

# Tem também execução condicional
echo "Sempre executado" || echo "Somente executado se o primeiro falhar"
echo "Sempre executado" && "Só executado se o primeiro NÃO falhar"

# Para usar && e || com o if, você precisa multiplicar os pares de colchetes
if [ $Nome == "Estevao"] && [ $Idade -eq 15]
then
	echo "Isso vai rodar se $Nome é igual Estevao E $Idade é 15."
fi

fi [ $Nome == "Daniela" ] || [ $Nome = "Jose" ] 
then
	echo "Isso vai rodar se $Nome é Daniela ou Jose."
fi

# Expressões são denotadas com o seguinte formato
echo $(( 10 + 5))

# Diferentemente das outras linguagens de programação, bash é um shell, então ele 
# funciona no diretório atual. Você pode listar os arquivos e diretórios no diretório
# atual com o comando ls:
ls

#Esse comando tem opções que controlam sua execução
ls -l # Lista todo arquivo e diretorio em linhas separadas

# Os resultados do comando anterior pode ser passado para outro comando como input.
# O comando grep filtra o input com o padrão passado. É assim que listamos apenas
# os arquivos .txt no diretório atual:
ls -l | grep "\.txt"

# Você pode redirecionar o comando de input e output (stdin, stdout e stderr).
# Lê o stdin até ^EOF$ e sobrescreve hello.py com as linhas entre "EOF":
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ imprt print_function
import sys
print("#stdout", file=sys.stdout)
print("stderr", file=sys.stderr)
for line in sys.stdin:
	print(line, file=sys.stdout)
EOF

# Rode hello.py com várias instruções stdin, stdout e stderr:
python hello.py < "input.in"
python hello.py > "ouput.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# O erro no output sobrescreverá o arquivo se existir,
# se ao invés disso você quiser complementar, use ">>":
python hello.py >> "output.out" 2>> "error.err"

# Sobrescreve output.out, complemente para error.err e conta as linhas
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

#Roda um comando e imprime o desencriptador (e.g. /dev/fd/123)
# veja: man fd
echo <(echo "#helloworld")

# Sobrescreve ouput.out com "#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out > /dev/null

# Limpa os arquivos temporarios detalhando quais foram deletados (use '-i' para confirmar exlusão)
rm -v output.out error.err output-and-error.log

# Comando podem ser substituidos por outros comandos usando $( ):
# O comand oa seguir mostra o número de arquivos e diretórios no diretorio atual
echo "Existem $(ls | wc -l) itens aqui."

# O mesmo pode ser feito usando crase `` mas elas não podem ser aninhadas - dá se 
# preferência ao uso do $( )
echo "Existem `ls | wc -l` itens aqui."

# Bash usa o comando case que funciona de uma maneira similar ao switch de Java e C++:
case "$Variavel" in
	# Lista de parametros para condições que você quer encontrar
	0) echo "Isso é um Zero.";;
	1) echo "Isso é um Um.";;
	*) echo "Isso não é null.";;
esac

# loops for iteragem para quantos argumentos passados:
# O conteudo de $Variavel é exibido três vezes.
for Variavel in {1..3}
do
	echo "$Variavel"
done

# Ou use o loop da "maneira tradicional":
for ((a=1; a <= 3; a++))
do
	echo $a
done

# Eles também podem ser usados em arquivos...
# Isso irá rodar o comando 'cat' em arquivo1 e arquivo2
for Variavel in arquivo1 arquivo2
do
	cat "$Variavel"
done

# ...ou o output de um comando
# Isso irá usar cat no output do ls.
for Output in $(ls)
do
	cat "$Output"
done

# loop while:
while [ true ]
do
	echo "corpo do loop aqui..."
	break
done

# Você também pode usar funções
# Definição:
function foo() {
	echo "Argumentos funcionam bem assim como os dos scripts: $@"
	echo "E: $1 $2..."
	echo "Isso é uma função"
	return 0
}

# ou simplesmente
bar () {
	echo "Outro jeito de declarar funções!"
	return 0
}

# Chamando sua função
foo "Meu nome é" $Nome

# Existe um monte de comandos úteis que você deveria aprender:
# exibe as 10 ultimas linhas de arquivo.txt
tail -n 10 arquivo.txt
# exibe as primeiras 10 linhas de arquivo.txt
head -n 10 arquivo.txt
# ordena as linhas de arquivo.txt
sort arquivo.txt
# reporta ou omite as linhas repetidas, com -d você as reporta
uniq -d arquivo.txt
# exibe apenas a primeira coluna após o caráctere ','
cut -d ',' -f 1 arquivo.txt
# substitui todas as ocorrencias de 'okay' por 'legal' em arquivo.txt (é compativel com regex)
sed -i 's/okay/legal/g' file.txt
# exibe para o stdout todas as linhas do arquivo.txt que encaixam com o regex
# O exemplo exibe linhas que começam com "foo" e terminam com "bar"
grep "^foo.*bar$" arquivo.txt
# passe a opção "-c" para ao invês de imprimir o numero da linha que bate com o regex
grep -c "^foo.*bar$" arquivo.txt
# se você quer literalmente procurar por uma string,
# e não pelo regex, use fgrep (ou grep -F)
fgrep "^foo.*bar$" arquivo.txt


# Leia a documentação interna do shell Bash com o comando interno 'help':
help
help help
help for 
help return
help source
help .

# Leia a página principal da documentação com man
apropos bash
man 1 bash
man bash

# Leia a documentação de informação com info (? para ajuda)
apropos info | grep '^info.*('
man info
info info 
info 5 info 

#Leia a documentação informativa do Bash:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash 
```
