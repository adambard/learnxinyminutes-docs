---
category: tool
tool: awk
filename: learnawk-pt.awk
contributors:
    - ["Marshall Mason", "http://github.com/marshallmason"]
translators:
    - ["Paulo Henrique Rodrigues Pinheiro", "https://github.com/paulohrpinheiro"]
lang: pt-br

---

AWK é uma ferramenta padrão em todos os sistemas UNIX compatíveis com POSIX. É
como um Perl despojado, perfeito para tarefas de processamento de texto e
outras tarefas de script.  Possui uma sintaxe C-like, mas sem ponto e vírgula,
gerenciamento manual de memória, ou tipagem estática. Destaca-se no
processamento de texto. Você pode chamá-lo a partir de um shell-script, ou você
pode usá-lo como uma linguagem de script autônomo.

Por que usar AWK ao invés de Perl? Principalmente porque AWK faz parte do UNIX.
Você pode sempre contar com ele, enquanto o futuro do Perl é indefinido. AWK é
também mais fácil de ler que Perl. Para scripts simples de processamento de
texto, particularmente aqueles que leem arquivos linha por linha e fatiam texto
por delimitadores, AWK é provavelmente a ferramenta certa para a tarefa.

```awk
#!/usr/bin/awk -f

# Comentários são assim

# Programas AWK consistem de uma coleção de padrões e ações. O mais
# importante padrão é chamado BEGIN. Ações estão dentro de blocos
# entre chaves.

BEGIN {

    # O bloco BEGIN será executado no começo do programa. É onde você coloca
    # todo código que prepara a execução, antes que você processe qualquer
    # arquivo de texto. Se você não tem arquivos de texto, então pense no
    # BEGIN como o ponto principal de entrada.

    # Variáveis são globais. Simplesmente atribua valores ou as use, sem
    # necessidade de declarar.

    # Operadores são como em C e similares
    a = count + 1
    b = count - 1
    c = count * 1
    d = count / 1 # divisão inteira
    e = count % 1 # módulo
    f = count ^ 1 # exponenciação

    a += 1
    b -= 1
    c *= 1
    d /= 1
    e %= 1
    f ^= 1

    # Incrementando e decrementando por um
    a++
    b--

    # Como um operador pré-fixado, retorna o valor incrementado
    ++a
    --b

    # Perceba, não há pontuação, como ponto-e-vírgula, ao final das declarações

    # Declarações de controle
    if (count == 0)
        print "Começando com count em 0"
    else
        print "Como é que é?"

    # Ou você pode usar o operador ternário
    print (count == 0) ? "Começando com count em 0" : "Como é que é?"

    # Blocos multilinhas devem usar chaves
    while (a < 10) {
        print "Concatenação de texto é feita" " com uma série" " de"
            " textos separados por espaço"
        print a

        a++
    }

    for (i = 0; i < 10; i++)
        print "Uma boa opção para um loop de uma linha"

    # Quanto a comparações, eis os padrões:
    a < b   # Menor que
    a <= b  # Menor ou igual a
    a != b  # Não igual
    a == b  # Igual
    a > b   # Maior que
    a >= b  # Maior ou igual a

    # Bem como operadores lógicos
    a && b  # E
    a || b  # OU (inclusivo)

    # Em adição, há o utilíssimo operador para expressões regulares
    if ("foo" ~ "^fo+$")
        print "Fooey!"
    if ("boo" !~ "^fo+$")
        print "Boo!"

    # Matrizes
    arr[0] = "foo"
    arr[1] = "bar"
    # Infelizmente, não há outra forma para inicializar uma matriz. Apenas
    # coloque cada valor em uma linha, como mostrado acima.

    # Você também pode ter matrizes associativas
    assoc["foo"] = "bar"
    assoc["bar"] = "baz"

    # E matrizes multidimensionais, com algumas limitações que não mencionarei
    multidim[0,0] = "foo"
    multidim[0,1] = "bar"
    multidim[1,0] = "baz"
    multidim[1,1] = "boo"

    # Você pode testar a pertinência de um elemento em uma matriz
    if ("foo" in assoc)
        print "Fooey!"

    # Você pode também usar o operador 'in' para percorrer as chaves de uma
    # matriz associativa
    for (key in assoc)
        print assoc[key]

    # Os argumentos da linha de comando estão em uma matriz especial ARGV
    for (argnum in ARGV)
        print ARGV[argnum]

    # Você pode remover elementos de uma matriz
    # Isso é muito útil para prevenir que o AWK assuma que os argumentos são
    # arquivo para ele processar
    delete ARGV[1]

    # A quantidade de argumentos passados está na variável ARGC
    print ARGC

    # O AWK tem várias funções nativas. Elas estão separadas em três categorias.
    # Demonstrarei cada uma delas logo mais abaixo.

    return_value = arithmetic_functions(a, b, c)
    string_functions()
    io_functions()
}

# Eis como você deve definir uma função
function arithmetic_functions(a, b, c,    d) {

    # Provavelmente a parte mais irritante do AWK é ele não possuir variáveis
    # locais. Tudo é global. Para pequenos scripts, isso não é problema, e
    # pode até mesmo ser considerado útil, mas para grandes scripts, isso pode
    # ser um problema.

    # Mas há como contornar isso (um hack). Os argumentos de função são locais
    # para a função e o AWK permite que você defina mais argumentos de função
    # do que ele precise. Então, coloque a variável local na declaração de
    # função, como eu fiz acima. Como uma convenção, adicione alguns espaços
    # extras para distinguir entre parâmetros de função reais e variáveis
    # locais. Neste exemplo, a, b e c são parâmetros reais, enquanto d é
    # meramente uma variável local.

    # Agora, serão demonstradas as funções aritméticas

    # Muitas implementações AWK possuem algumas funções trigonométricas padrão
    localvar = sin(a)
    localvar = cos(a)
    localvar = atan2(b, a) # arco-tangente de b / a

    # E conteúdo logarítmico
    localvar = exp(a)
    localvar = log(a)

    # Raiz quadrada
    localvar = sqrt(a)

    # Descartando a parte não inteira de um número em ponto flutuante.
    localvar = int(5.34) # localvar => 5

    # Números aleatórios
    srand() # Forneça uma semente como argumento. Por padrão, ele usa a hora atual
    localvar = rand() # Número aleatório entre 0 e 1.

    # Aqui mostramos como retornar um valor
    return localvar
}

function string_functions(    localvar, arr) {

    # Sendo o AWK uma linguagem para processamento de texto, ele possui
    # várias funções para manipulação de texto, muitas das quais
    # fortemente dependentes de expressões regulares.

    # Procurar e substituir, primeira instância (sub), ou todas (gsub)
    # Ambas retornam o número de instâncias substituídas
    localvar = "fooooobar"
    sub("fo+", "Meet me at the ", localvar) # localvar => "Meet me at the bar"
    gsub("e+", ".", localvar) # localvar => "m..t m. at th. bar"

    # Localiza um texto que casa com uma expressão regular
    # index() faz a mesma coisa, mas não permite uma expressão regular
    match(localvar, "t") # => 4, pois 't' é o quarto carácter

    # Separa por delimitador
    split("foo-bar-baz", arr, "-") # a => ["foo", "bar", "baz"]

    # Outras coisas úteis
    sprintf("%s %d %d %d", "Testing", 1, 2, 3) # => "Testing 1 2 3"
    substr("foobar", 2, 3) # => "oob"
    substr("foobar", 4) # => "bar"
    length("foo") # => 3
    tolower("FOO") # => "foo"
    toupper("foo") # => "FOO"
}

function io_functions(    localvar) {

    # Você já viu como imprimir
    print "Hello world"

    # Também há o printf
    printf("%s %d %d %d\n", "Testing", 1, 2, 3)

    # O AWK não disponibiliza manipuladores de arquivo. Ele irá automaticamente
    # manipular um arquivo quando você fizer algo que precise disso. O texto
    # que você usou para isso pode ser usado como um manipulador de arquivo,
    # para propósitos de E/S. Isso faz ele parecer com um shell script:

    print "foobar" >"/tmp/foobar.txt"

    # Agora a string "/tmp/foobar.txt" é um manipulador de arquivos. Você pode
    # fechá-lo:
    close("/tmp/foobar.txt")

    # Aqui está como você pode executar alguma coisa no shell
    system("echo foobar") # => prints foobar

    # Lê uma linha da entrada padrão e armazena em localvar
    getline localvar

    # Lê uma linha de um pipe
    "echo foobar" | getline localvar # localvar => "foobar"
    close("echo foobar")

    # Lê uma linha de um arquivo e armazena em localvar
    getline localvar <"/tmp/foobar.txt"
    close("/tmp/foobar.txt")
}

# Como dito no início, os programas AWK consistem de uma coleção de padrões
# e ações. Você já viu o padrão BEGIN, o mais importante. Outros padrões são
# usados apenas se você estiver processando linhas de arquivos ou a entrada
# padrão.

# Quando você passa argumentos para o AWK, eles são tratados como nomes de
# arquivos para processar. Todos serão processados, em ordem. Pense nisso como
# um implícito para loop, iterando sobre as linhas nesses arquivos. Esses
# padrões e ações são como instruções de mudança dentro do loop.

/^fo+bar$/ {

    # Esta ação será executada para cada linha que corresponda à expressão
    # regular, / ^ fo + bar $ /, e será ignorada para qualquer linha que não
    # corresponda. Vamos apenas imprimir a linha:

    print

    # Opa, sem argumento! Isso ocorre pois print tem um argumento padrão: $0.
    # $0 é o nome da linha atual que está sendo processada. Essa variável é
    # criada automaticamente para você.

    # Você provavelmente pode adivinhar que existem outras variáveis $. Toda
    # linha é implicitamente dividida antes de cada ação ser chamada, como
    # o shell faz. E, como o shell, cada campo pode ser acessado com um sinal
    # de cifrão

    # Isso irá imprimir o segundo e quarto campos da linha
    print $2, $4

    # O AWK automaticamente define muitas outras variáveis para ajudar você
    # a inspecionar processar cada linha. A mais importante delas é NF.

    # Imprime o número de campos da linha atual
    print NF

    # Imprime o último campo da linha atual
    print $NF
}

# Todo padrão é na verdade um teste verdadeiro/falso. A expressão regular no
# último padrão também é um teste verdadeiro/falso, mas parte dele estava
# escondido. Se você não informar um texto para testar, AWK assumirá $0,
# a linha que está atualmente sendo processada. Assim, a versão completa
# é a seguinte:

$0 ~ /^fo+bar$/ {
    print "Equivalente ao último padrão"
}

a > 0 {
    # Isso será executado uma vez para cada linha, quando a for positivo
}

# Você entendeu. Processar arquivos de texto, ler uma linha de cada vez, e
# fazer algo com ela, particularmente dividir com base em um delimitador, é
# tão comum no UNIX que AWK é uma linguagem de script que faz tudo por você,
# sem você precisa perguntar. Tudo o que você precisa fazer é escrever os
# padrões e ações com base no que você espera da entrada, e o que você quer
# fazer com isso.

# Aqui está um exemplo rápido de um script simples, o tipo de coisa que o AWK
# é perfeito para fazer. Ele irá ler um nome da entrada padrão e depois
# imprimirá a média de idade de todos com esse primeiro nome. Digamos que você
# forneça como argumento o nome de um arquivo com esses dados:

# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# Eis o script:

BEGIN {

    # Primeiro, pergunte o nome do usuário
    print "Para qual nome você quer calcular a média de idade?"

    # Pega uma linha da entrada padrão, não dos arquivos indicados na
    # linha de comando
    getline name <"/dev/stdin"
}

# Agora, processa cada linha em que o primeiro nome é o nome informado
$1 == name {

    # Dentro desse bloco, nós temos acesso a algumas variáveis uteis, que
    # foram pré-carregadas para nós:
    # $0  é a linha corrente completa
    # $3 é o terceiro campo, que é o que nos interessa aqui
    # NF é a quantidade de campos, que deve ser 3
    # NR é o número de registros (linhas) lidas até agora
    # FILENAME é o nome do arquivo sendo processado
    # FS é o delimitador em uso, que é " " aqui
    # ...etc. Há muito mais, documentadas no manual.

    # Mantenha um registro do total e da quantidade de linhas encontradas
    sum += $3
    nlines++
}

# Outro padrão especial é chamado END. Ele será executado após o processamento
# de todos os arquivos de texto. Ao contrário de BEGIN, ele só será executado
# se você tiver dado a ele dados para processar. Ele será executado depois de
# todos os arquivos terem sido lidos e processados de acordo com as regras e
# ações que você forneceu. O objetivo disso em geral é produzir algum tipo de
# relatório final, ou fazer algo com o agregado dos dados acumulados ao longo
# do script.

END {
    if (nlines)
        print "A média da idade para " name " é " sum / nlines
}

```
Leituras adicionais (em inglês):

* [Awk tutorial](http://www.grymoire.com/Unix/Awk.html)
* [Awk man page](https://linux.die.net/man/1/awk)
* [The GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html) GNU AWK é encontrado na maioria dos sistemas GNU/Linux.
