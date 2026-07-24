---
name: COBOL
filename: learn.cob
contributors:
    - ["Hyphz", "https://github.com/hyphz/"]
translators:
    - ["Adaías Magdiel", "https://github.com/adaiasmagdiel"]
---

COBOL (Common Business-Oriented Language) é uma linguagem voltada para negócios, revisada diversas vezes desde seu design original em 1960.

```cobol
      *COBOL. Programando como se fosse 1985.
      *Compila com GnuCOBOL no OpenCobolIDE 4.7.6.

      *COBOL tem diferenças significativas entre as versões legadas (COBOL-85)
      *e as modernas (COBOL-2002 e COBOL-2014).
      *Versões legadas exigem que as colunas de 1 a 6 fiquem em branco (elas eram
      *usadas para armazenar o número de índice do cartão perfurado).
      *Um '*' na coluna 7 significa um comentário.
      *No COBOL legado, um comentário só pode ocupar a linha inteira.
      *O COBOL moderno não exige colunas fixas e usa *> para
      *um comentário, que pode aparecer no meio de uma linha.
      *O COBOL legado também impõe um limite ao tamanho máximo da linha.
      *As palavras-chave precisam estar em maiúsculas no COBOL legado,
      *mas não diferenciam maiúsculas de minúsculas no moderno.
      *Embora o COBOL moderno permita usar caracteres em caixa mista,
      *ainda é comum usar tudo em maiúsculas ao escrever código COBOL.
      *É isso que a maioria dos desenvolvedores profissionais de COBOL faz.
      *Instruções COBOL terminam com um ponto.

      *O código COBOL é dividido em 4 divisões.
      *Essas divisões, em ordem, são:
      *IDENTIFICATION DIVISION.
      *ENVIRONMENT DIVISION.
      *DATA DIVISION.
      *PROCEDURE DIVISION.

      *Primeiro, precisamos dar um ID ao nosso programa.
      *A IDENTIFICATION DIVISION pode incluir outros valores também,
      *mas eles são apenas comentários. PROGRAM-ID é o único
      *obrigatório.
       IDENTIFICATION DIVISION.
           PROGRAM-ID.    LEARN.
           AUTHOR.        JOHN DOE.
           DATE-WRITTEN.  24/07/2026.

      *Vamos declarar algumas variáveis.
      *Fazemos isso na seção WORKING-STORAGE dentro da DATA DIVISION.
      *Cada item de dado (ou seja, variável) começa com um número de nível,
      *seguido pelo nome do item, seguido por uma cláusula PICTURE
      *descrevendo o tipo de dado que a variável vai conter.
      *Quase todo programador COBOL abrevia PICTURE como PIC.
      *A é para alfabético, X é para alfanumérico, e 9 é para numérico.

      *exemplo:
       01  MYNAME PIC XXXXXXXXXX.    *> Uma string de 10 caracteres.

      *Mas ficar contando os Xs pode facilmente causar erros,
      *então o código acima pode ser reescrito como
       01  MYNAME PIC X(10).

      *Veja mais alguns exemplos:
       01  AGE             PICTURE  9(3).   *> Um número de até 3 dígitos.
       01  BIRTH_YEAR      PIC      S9(7).  *> Um número com sinal de até 7 dígitos.
       01  LAST_NAME       PIC      X(10).  *> Uma string de até 10 caracteres.

      *Em COBOL, múltiplos espaços equivalem a um único espaço, então é
      *comum usar múltiplos espaços para alinhar seu código, tornando-o
      *mais fácil de ler para outros programadores.


      *Agora vamos escrever algum código. Aqui está um programa simples, Hello World.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 THE-MESSAGE      PIC X(20).
       PROCEDURE DIVISION.
           DISPLAY "STARTING PROGRAM".
           MOVE "HELLO WORLD" TO THE-MESSAGE.
           DISPLAY THE-MESSAGE.
           STOP RUN.

      *O código acima vai imprimir:
      *STARTING PROGRAM
      *HELLO WORLD



      ********COBOL pode fazer contas***************
       ADD 1 TO AGE GIVING NEW-AGE.
       SUBTRACT 1 FROM COUNT.
       DIVIDE VAR-1 INTO VAR-2 GIVING VAR-3.
       COMPUTE TOTAL-COUNT = COUNT1 PLUS COUNT2.


      *********PERFORM********************
      *A palavra-chave PERFORM permite que você pule para outra seção
      *especificada do código, e depois retorne para a próxima instrução
      *executável assim que a seção especificada do código for concluída.
      *Você precisa escrever a palavra completa, PERFORM, não é possível abreviá-la.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOCOBOL.

       PROCEDURE DIVISION.
           FIRST-PARA.
               DISPLAY 'THIS IS IN FIRST-PARA'.
      *pula SECOND-PARA e executa a 3ª & 4ª
      *depois de executar THIRD-PARA e FOURTH-PARA,
      *retorna aqui e continua o programa até STOP RUN.
           PERFORM THIRD-PARA THRU FOURTH-PARA.

           SECOND-PARA.
               DISPLAY 'THIS IS IN SECOND-PARA'.

           STOP RUN.

           THIRD-PARA.
               DISPLAY 'THIS IS IN THIRD-PARA'.

           FOURTH-PARA.
               DISPLAY 'THIS IS IN FOURTH-PARA'.


      *Quando você compila e executa o programa acima, ele produz o
      *seguinte resultado (observe a ordem):
      *THIS IS IN FIRST-PARA
      *THIS IS IN THIRD-PARA
      *THIS IS IN FOURTH-PARA
      *THIS IS IN SECOND-PARA


      **********Combinando variáveis usando STRING ***********

      *Agora é hora de aprender sobre dois verbos COBOL relacionados: STRING e
      *UNSTRING.

      *O verbo STRING é usado para concatenar, ou juntar, duas ou
      *mais strings.
      *UNSTRING é usado, sem surpresa, para separar uma
      *string em duas ou mais strings menores.
      *É importante lembrar de usar DELIMITED BY quando você
      *estiver usando STRING ou UNSTRING no seu programa.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEARNING.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FULL-NAME PIC X(20).
       01 FIRST-NAME PIC X(13) VALUE "BOB GIBBERISH".
       01 LAST-NAME PIC X(5) VALUE "COBB".
       PROCEDURE DIVISION.
           STRING FIRST-NAME DELIMITED BY SPACE
             " "
             LAST-NAME DELIMITED BY SIZE
             INTO FULL-NAME
           END-STRING.
           DISPLAY "THE FULL NAME IS: "FULL-NAME.
       STOP RUN.


      *O código acima vai imprimir:
      *THE FULL NAME IS: BOB COBB


      *Vamos examinar o motivo.

      *Primeiro, declaramos todas as nossas variáveis, incluindo a que
      *estamos criando pelo comando string, na DATA DIVISION.

      *A ação acontece lá embaixo na PROCEDURE DIVISION.
      *Começamos com a palavra-chave STRING e terminamos com END-STRING. No
      *meio, listamos o que queremos combinar na variável maior,
      *a principal. Aqui, estamos combinando FIRST-NAME, um espaço, e
      *LAST-NAME.

      *A frase DELIMITED BY que segue FIRST-NAME e
      *LAST-NAME informa ao programa quanto de cada variável queremos
      *capturar.
      *DELIMITED BY SPACE informa ao programa para começar do início
      *e capturar a variável até encontrar um espaço.
      *DELIMITED BY SIZE informa ao programa para capturar o tamanho
      *completo da variável.
      *Como temos DELIMITED BY SPACE depois de FIRST-NAME, a parte
      *GIBBERISH é ignorada.

      *Para deixar isso mais claro, mude a linha 10 do código acima para
           STRING FIRST-NAME DELIMITED BY SIZE
      *e então execute o programa novamente. Dessa vez a saída é:
      *THE FULL NAME IS: BOB GIBBERISH COBB
```

## Leitura adicional

* [GnuCOBOL](https://gnucobol.sourceforge.io/)
* [O que é COBOL? (IBM)](https://www.ibm.com/br-pt/think/topics/cobol)
* [COBOL (Wikipédia)](https://pt.wikipedia.org/wiki/COBOL)
