---
category: tool
tool: cmake
contributors:
    - ["Bruno Alano", "https://github.com/brunoalano"]
filename: CMake-br
translators:
    - ["Lucas Pugliesi", "https://github.com/fplucas"]
lang: pt-br
---

CMake é um programa de compilação open-source e multiplataforma. Essa ferramenta
permitirá testar, compilar e criar pacotes a partir do seu código fonte.

O problema que o CMake tenta resolver são os problemas de configurar os Makefiles
e Autoconfigure (diferente dos interpretadores make que tem comandos diferentes)
e sua facilidade de uso envolvendo bibliotecas terceiras.

CMake é um sistema open-source extensível que gerencia o processo de build em um
sistema operacional e um método independente de compilador. Diferente de sistemas
multiplataformas, CMake é designado a usar em conjunto ao ambiente de compilação
nativo. Seus simples arquivos de configuração localizados em seus diretórios
(chamados arquivos CMakeLists.txt) que são usados para gerar padrões de arquivos
de compilação (ex: makefiles no Unix e projetos em Windows MSVC) que são usados
de maneira simples.

```cmake
# No CMake, isso é um comentário

# Para rodar nosso código, iremos utilizar esses comandos:
#  - mkdir build && cd build
#  - cmake ..
#  - make
#
# Com esses comandos, iremos seguir as melhores práticas para compilar em um
# subdiretório e na segunda linha pediremos ao CMake para gerar um novo Makefile
# independente de sistema operacional. E finalmente, rodar o comando make.

#------------------------------------------------------------------------------
# Básico
#------------------------------------------------------------------------------
#
# O arquivo CMake deve ser chamado de "CMakeLists.txt".

# Configura a versão mínima requerida do CMake para gerar o Makefile
cmake_minimum_required (VERSION 2.8)

# Exibe FATAL_ERROR se a versão for menor que  2.8
cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

# Configuramos o nome do nosso projeto. Mas antes disso, iremos alterar alguns
# diretórios em nome da convenção gerada pelo CMake. Podemos enviar a LANG do
# código como segundo parâmetro
project (learncmake C)

# Configure o diretório do código do projeto (somente convenção)
set( LEARN_CMAKE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )
set( LEARN_CMAKE_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} )

# Isso é muito útil para configurar a versão do nosso código no sistema de compilação
# usando um estilo `semver`
set (LEARN_CMAKE_VERSION_MAJOR 1)
set (LEARN_CMAKE_VERSION_MINOR 0)
set (LEARN_CMAKE_VERSION_PATCH 0)

# Envie as variáveis (número da versão) para o cabeçalho de código-fonte
configure_file (
  "${PROJECT_SOURCE_DIR}/TutorialConfig.h.in"
  "${PROJECT_BINARY_DIR}/TutorialConfig.h"
)

# Inclua Diretórios
# No GCC, isso irá invocar o comando "-I"
include_directories( include )

# Onde as bibliotecas adicionais estão instaladas? Nota: permite incluir o path
# aqui, na sequência as checagens irão resolver o resto
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# Condições
if ( CONDICAO )
  # reposta!

  # Informação incidental
  message(STATUS "Minha mensagem")

  # Aviso CMake, continua processando
  message(WARNING "Minha mensagem")

  # Aviso (dev) CMake, continua processando
  message(AUTHOR_WARNING "Minha mensagem")

  # Erro CMake, continua processando, mas pula a geração
  message(SEND_ERROR "Minha mensagem")

  # Erro CMake, para o processamento e a geração
  message(FATAL_ERROR "Minha mensagem")
endif()

if( CONDICAO )

elseif( CONDICAO )

else( CONDICAO )

endif( CONDICAO )

# Loops
foreach(loop_var arg1 arg2 ...)
  COMANDO1(ARGS ...)
  COMANDO2(ARGS ...)
  ...
endforeach(loop_var)

foreach(loop_var RANGE total)
foreach(loop_var RANGE start stop [step])

foreach(loop_var IN [LISTS [list1 [...]]]
                    [ITEMS [item1 [...]]])

while(condicao)
  COMANDO1(ARGS ...)
  COMANDO2(ARGS ...)
  ...
endwhile(condicao)


# Operações Lógicas
if(FALSE AND (FALSE OR TRUE))
  message("Não exiba!")
endif()

# Configure um cache normal, ou uma variável de ambiente com o dado valor.
# Se a opção PARENT_SCOPE for informada em uma variável que será setada no escopo
# acima do escopo corrente.
# `set(<variavel> <valor>... [PARENT_SCOPE])`

# Como refencia variáveis dentro de aspas ou não, argumentos com strings vazias
# não serão setados
${nome_da_variavel}

# Listas
# Configure a lista de arquivos código-fonte
set( LEARN_CMAKE_SOURCES
  src/main.c
  src/imagem.c
  src/pather.c
)

# Chama o compilador
#
# ${PROJECT_NAME} referencia ao Learn_CMake
add_executable( ${PROJECT_NAME} ${LEARN_CMAKE_SOURCES} )

# Linka as bibliotecas
target_link_libraries( ${PROJECT_NAME} ${LIBS} m )

# Onde as bibliotecas adicionais serão instaladas? Nota: nos permite incluir o path
# aqui, em seguida os testes irão resolver o restante
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# Condição do compilador (gcc ; g++)
if ( "${CMAKE_C_COMPILER_ID}" STREQUAL "GNU" )
  message( STATUS "Setting the flags for ${CMAKE_C_COMPILER_ID} compiler" )
  add_definitions( --std=c99 )
endif()

# Checa o Sistema Operacional
if( UNIX )
    set( LEARN_CMAKE_DEFINITIONS
        "${LEARN_CMAKE_DEFINITIONS} -Wall -Wextra -Werror -Wno-deprecated-declarations -Wno-unused-parameter -Wno-comment" )
endif()
```

### Mais Recursos

+ [cmake tutorial](https://cmake.org/cmake-tutorial/)
+ [cmake documentation](https://cmake.org/documentation/)
+ [mastering cmake](http://amzn.com/1930934319/)
