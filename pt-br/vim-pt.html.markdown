---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
    - ["David Lima", "https://github.com/davelima"]
lang: pt-br
filename: LearnVim-pt.txt
---


[Vim](www.vim.org)
(Vi IMproved - Vi Melhorado) é um clone do editor vi para Unix. Ele é um
editor de texto projetado para ter velocidade e produtividade, e está presente
na maioria dos systemas UNIX. O editor tem um grande número de atalhos de teclado
para agilizar a navegação para pontos específicos no arquivo, além de edição rápida.

## Navegação do Vim: o básico

```
    vim <nome-do-arquivo>   # Abre <nome-do-arquivo> no vim
    :q               # Fecha o vim
    :w               # Salva o arquivo atual
    :wq              # Salva o arquivo e fecha o vim
    :q!              # Fecha o vim e descarta as alterações no arquivo
                     # ! *força* :q a executar, fechando o vim sem salvar antes
    :x               # Salvar o arquivo e fechao vim (atalho para :wq)

    u                # Desfazer
    CTRL+R           # Refazer

    h                # Move o cursor para a esquerda
    j                # Move o cursor para baixo
    k                # Move o cursor para cima
    l                # Move o cursor para a direita

    # Movendo o cursor dentro da linha

    0                # Move para o início da linha
    $                # Move para o final da linha
    ^                # Move para o primeiro caractere da linha (ignora caracteres em branco)

    # Pesquisa no texto

    /palavra         # Destaca todas as ocorrências de 'palavra' após o cursor
    ?palavra         # Destaca todas as ocorrências de 'palavra' antes do cursor
    n                # Move o cursor para a próxima ocorrência após a pesquisa
    N                # Move o cursor para a ocorrência anterior após a pesquisa

    :%s/foo/bar/g    # Substitui 'foo' por 'bar' no arquivo inteiro
    :s/foo/bar/g     # Substitui 'foo' por 'bar' na linha atual

    # Pulando para caracteres específicos

    f<caracter>      # Posiciona o cursor no próximo <caracter>
    t<character>     # Posiciona o cursor antes do próximo <caracter> 

    # Por exemplo,    
    f<               # Posiciona o cursor no <
    t<               # Posiciona o cursor logo antes do <
    
    # Movendo por palavras

    w                # Move o cursor uma palavra a diante
    b                # Move o cursor uma palavra atrás
    e                # Move o cursor ao fim da palavra atual

    # Outros caracteres para mover o cursor no arquivo

    gg               # Move para o topo do arquivo
    G                # Move para o final do arquivo
    :NUM             # Move para a linha NUM (NUM é qualquer número)
    H                # Move para o topo da tela visível
    M                # Move para o meio da tela visível
    L                # Move para o final da tela visível
```

## Modos:

O Vim é baseado no conceito de **modos**.

Modo Comando  - usado para navegar e escrever comandos - o Vim já inicia nesse modo
Modo Inserção - usado para fazer alterações no arquivo
Modo Visual   - usado para destacar textos e executar comandos neles
Modo Ex       - usado para ir a linha com ':' no final da tela para executar comandos 

```
    i                # Coloca o Vim no Modo Inserção, logo antes do cursor
    a                # Coloca o Vim no Modo Inserção, logo após o cursor
    v                # Coloca o Vim no Modo Visual 
    :                # Coloca o Vim no Modo Ex
    <esc>            # Sai de qualquer modo que você estiver, e coloca o Vim no Modo Comando

    # Copiando e colando texto

    y                # Coloca a seleção atual na área de transferência
    yy               # Coloca a linha atual na área de transferência
    d                # Deleta a seleção tual
    dd               # Deleta a linha atual
    p                # Cola o texto copiado após a posição do cursor
    P                # Cola o texto copiado antes da posição do cursor
    x                # Deleta o caractere que está na posição do cursor
```

## A 'Gramática' do Vim

Podemos pensar no Vim como uma série de comendos
em um formato 'Verbo-Modificador-Nome', onde:

Verbo       - sua ação 
Modificador - como você executará sua ação
Nome        - o objeto onde você vai executar sua acão

Alguns exemplos importantes de 'Verbos', 'Modificadores' e 'Nomes':

```
    # 'Verbos'
 
    d                # Apagar (Delete)
    c                # Alterar (Change)
    y                # Copiar (Yank)
    v                # Seleção Visual

    # 'Modificadores'

    i                # Dentro (Inside)
    a                # Em torno de (Around)
    NUM              # Número (NUM qualquer número)
    f                # Pesquisa algo e posiciona o cursor acima do resultado
    t                # Pesquisa algo e posiciona o cursor logo antes do resultado
    /                # Encontra algo a frente do cursor
    ?                # Encontra algo antes do cursor

    # 'Nomes'

    w                # Palavra (word)
    s                # Sentência
    p                # Parágrafo
    b                # Bloco
    
    # Exemplos de comandos

    d2w              # Apaga 2 palavras
    cis              # Altera dentro de uma sentência
    yip              # Coloca o parágrafo atual da área de transferência)
    ct<              # Altera para '<'
                     # Altera todo o texto a partir da posição do cursor até o próximo '<'
    d$               # Apaga tudo da posição do cursor até o final da linha
```

## Alguns atalhos e dicas

        <!--TODO: Adicionar mais!-->
```
    >                # Adiciona um bloco de indentação
    <                # Remove um bloco de indentação
    :earlier 15m     # Reverte o documento para como ele estava há 15 minutos atrás
    :later 15m       # Reverte o comando acima
    ddp              # Troca linhas consecutivas de posição, dd e depois p
    .                # Repete a última ação
```

## Macros

Macros, basicamente, são ações graváveis.
Quando você começa a gravar uma macro, ele salva **toda** ação e comando
que você usar, até que você pare de gravar. Ao executar uma macro, ele aplica
exatamente a mesma sequencia de ações e comandos na seleção atual.

```
    qa               # Inicia a gravação de uma macro chamado 'a'
    q                # Para a gravação
    @a               # Executa a macro
```

### Configurando o ~/.vimrc

O arquivo .vimrc pode ser usado para configurar o Vim no seu início.

Exemplo de arquivo ~/.vimrc

```
" Exemplo de ~/.vimrc
" 2015.10 

" Obrigatório para rodar apenas no Vim (Vi Improved)
set nocompatible

" Determina o tipo de arquivo pelo nome para habilitar indentação automática, etc
filetype indent plugin on

" Habilita sintaxe colorida
syntax on

" Ativa um 'auto-completar' melhor para a linha de comando
set wildmenu

" Faz as buscas não diferenciarem maiúsculas-minúsculas (case insensitive)
" Exceto quando você usar letras maiúsculas
set ignorecase
set smartcase

" Quando criar uma nova linha e a indentação por tipo de arquivo estiver
" desabilitada, mantem a mesma indentação da linha atual
set autoindent

" Mostra o número das linhas à esquerda
set number

" Opções de indentação, aqui você pode mudar como preferir

" Número de espaços visíveis por TAB
set tabstop=4

" Número de espaços por TAB ao editar um arquivo
set softtabstop=4

" Número de espaços usados nas funções de indentação (>> e <<)
set shiftwidth=4

" Converte TABs em espaços
set expandtab

" Habilita indentação/alinhamento inteligente
set smarttab
```

### Referências

[Vim | Home](http://www.vim.org/index.php) (EN)

`$ vimtutor pt`

[Vim: um tutorial/cartilha](https://danielmiessler.com/study/vim/) (EN)

[O que são as partes sombrias do Vim que sua mãe nunca te explicou? (tópico no Stack Overflow)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about) (EN)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim) (EN)
