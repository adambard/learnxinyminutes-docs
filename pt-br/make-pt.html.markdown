---
category: tool
tool: make
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Stephan Fuhrmann", "https://github.com/sfuhrm"]
filename: Makefile

lang: pt-br
---

Um Makefile define um gráfico de regras para criar um alvo (ou alvos). Sua finalidade é fazer o mínimo de trabalho necessário para atualizar um alvo para a versão mais recente da fonte. Famosamente escrito ao longo de um fim de semana por Stuart Feldman em 1976, ainda é amplamente usada (particularmente no Unix e no Linux) apesar de muitos concorrentes e críticas.

Existem muitas variedades de make na existência, no entanto, este artigo pressupõe que estamos usando o GNU make, que é o padrão no Linux.

```make

# Comentários podem ser escritos assim.

# O arquivo deve ser nomeado Makefile e então pode ser executado como `make <alvo>`.
# Caso contrário, nós usamos `make -f "nome-do-arquivo" <alvo>`.

# Aviso - use somente TABS para identar em Makefiles, nunca espaços!

#-----------------------------------------------------------------------
# Noções básicas
#-----------------------------------------------------------------------

# Regras são do formato
# alvo: <pré-requisito>
# onde os pré-requisitos são opcionais.

# Uma regra - esta regra só será executada se o arquivo0.txt não existir.
arquivo0.txt:
	echo "foo" > arquivo0.txt
	# Mesmo os comentários nestas seções da 'receita' são passados ​​para o shell.
	# Experimentar `make arquivo0.txt` or simplyou simplesmente `make` - primeira regra é o padrão.

# Esta regra só será executada se arquivo0.txt for mais recente que arquivo1.txt.
arquivo1.txt: arquivo0.txt
	cat arquivo0.txt > arquivo1.txt
	# se as mesmas regras de citação do shell.
	@cat arquivo0.txt >> arquivo1.txt
	# @ pára o comando de ser ecoado para stdout.
	-@echo 'hello'
	# - significa que make continuará em caso de erro.
	# Experimentar `make arquivo1.txt` na linha de comando.

# Uma regra pode ter vários alvos e vários pré-requisitos
arquivo2.txt arquivo3.txt: arquivo0.txt arquivo1.txt
	touch arquivo2.txt
	touch arquivo3.txt

# Make vai reclamar sobre várias receitas para a mesma regra. Esvaziar
# receitas não contam e podem ser usadas para adicionar novas dependências.

#-----------------------------------------------------------------------
# Alvos falsos
#-----------------------------------------------------------------------

# Um alvo falso. Qualquer alvo que não seja um arquivo.
# Ele nunca será atualizado, portanto, o make sempre tentará executá-lo.
all: maker process

# Podemos declarar as coisas fora de ordem.
maker:
	touch ex0.txt ex1.txt

# Pode evitar quebrar regras falsas quando um arquivo real tem o mesmo nome
.PHONY: all maker process
# Este é um alvo especial. Existem vários outros.

# Uma regra com dependência de um alvo falso sempre será executada
ex0.txt ex1.txt: maker

# Alvos falsos comuns são: todos fazem instalação limpa ...

#-----------------------------------------------------------------------
# Variáveis ​​Automáticas e Curingas
#-----------------------------------------------------------------------

process: Arquivo*.txt	# Usando um curinga para corresponder nomes de arquivos
	@echo $^	# $^ é uma variável que contém a lista de pré-requisitos
	@echo $@	# imprime o nome do alvo
	#(fpara várias regras alvo, $@ é o que causou a execução da regra)
	@echo $<	# o primeiro pré-requisito listado
	@echo $?	# somente as dependências que estão desatualizadas
	@echo $+	# todas as dependências, incluindo duplicadas (ao contrário do normal)
	#@echo $|	# todos os pré-requisitos 'somente pedidos'

# Mesmo se dividirmos as definições de dependência de regra, $^ vai encontrá-los
process: ex1.txt arquivo0.txt
# ex1.txt será encontrado, mas arquivo0.txt será desduplicado.

#-----------------------------------------------------------------------
# Padrões
#-----------------------------------------------------------------------

# Pode ensinar make a converter certos arquivos em outros arquivos.

%.png: %.svg
	inkscape --export-png $^

# As regras padrões só farão qualquer coisa se decidirem criar o alvo.

# Os caminhos de diretório são normalmente ignorados quando as regras de
# padrões são correspondentes. Mas make tentará usar a regra mais
# apropriada disponível.
small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

# make utilizará a última versão para uma regra de padrão que encontrar.
%.png: %.svg
	@echo esta regra é escolhida

# No entanto, o make usará a primeira regra padrão que pode se tornar o alvo
%.png: %.ps
	@echo esta regra não é escolhida se *.svg and *.ps estão ambos presentes

# make já tem algumas regras padrões embutidas. Por exemplo, ele sabe
# como transformar arquivos *.c em arquivos *.o.

# Makefiles antigos podem usar regras de sufixo em vez de regras padrões
.png.ps:
	@echo essa regra é semelhante a uma regra de padrão.

# make sobre a regra de sufixo
.SUFFIXES: .png

#-----------------------------------------------------------------------
# Variáveis
#-----------------------------------------------------------------------
# aka. macros

# As variáveis ​​são basicamente todos os tipos de string

name = Ted
name2="Sarah"

echo:
	@echo $(name)
	@echo ${name2}
	@echo $name    # Isso não funcionará, tratado como $ (n)ame.
	@echo $(name3) # Variáveis ​​desconhecidas são tratadas como strings vazias.

# Existem 4 lugares para definir variáveis.
# Em ordem de prioridade, do maior para o menor:
# 1: argumentos de linha de comando
# 2: Makefile
# 3: variáveis ​​de ambiente do shell - faça importações automaticamente.
# 4: make tem algumas variáveis ​​predefinidas

name4 ?= Jean
# Somente defina a variável se a variável de ambiente ainda não estiver definida.

override name5 = David
# Pára os argumentos da linha de comando de alterar essa variável.

name4 +=grey
# Anexar valores à variável (inclui um espaço).

# Valores variáveis ​​específicos de padrões (extensão GNU).
echo: name2 = Sara # Verdadeiro dentro da regra de correspondência
	# e também dentro de suas recursivas dependências
	# (exceto que ele pode quebrar quando seu gráfico ficar muito complicado!)

# Algumas variáveis ​​definidas automaticamente pelo make
echo_inbuilt:
	echo $(CC)
	echo ${CXX}
	echo $(FC)
	echo ${CFLAGS}
	echo $(CPPFLAGS)
	echo ${CXXFLAGS}
	echo $(LDFLAGS)
	echo ${LDLIBS}

#-----------------------------------------------------------------------
# Variáveis 2
#-----------------------------------------------------------------------

# O primeiro tipo de variáveis ​​é avaliado a cada vez que elas são usadas.
# TIsso pode ser caro, então existe um segundo tipo de variável que é
# avaliado apenas uma vez. (Esta é uma extensão do GNU make)

var := hello
var2 ::=  $(var) hello
#:= e ::= são equivalentes.

# Essas variáveis ​​são avaliadas procedimentalmente (na ordem em que
# aparecem), quebrando assim o resto da línguagem!

# Isso não funciona
var3 ::= $(var4) and good luck
var4 ::= good night

#-----------------------------------------------------------------------
# Funções
#-----------------------------------------------------------------------

# make tem muitas funções disponíveis.

sourcefiles = $(wildcard *.c */*.c)
objectfiles = $(patsubst %.c,%.o,$(sourcefiles))

# O formato é $(func arg0,arg1,arg2...)

# Alguns exemplos
ls:	* src/*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# Diretivas
#-----------------------------------------------------------------------

# Inclua outros makefiles, úteis para código específico da plataforma
include foo.mk

sport = tennis
# Compilação condicional
report:
ifeq ($(sport),tennis)
	@echo 'game, set, match'
else
	@echo "They think it's all over; it is now"
endif

# Há também ifneq, ifdef, ifndef

foo = true

ifdef $(foo)
bar = 'hello'
endif
```

### More Resources

+ [documentação gnu make](https://www.gnu.org/software/make/manual/)
+ [tutorial de carpintaria de software](http://swcarpentry.github.io/make-novice/)
+ aprenda C da maneira mais difícil [ex2](http://c.learncodethehardway.org/book/ex2.html) [ex28](http://c.learncodethehardway.org/book/ex28.html)
