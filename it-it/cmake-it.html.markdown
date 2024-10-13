---
category: tool
tool: cmake
contributors:
    - ["Bruno Alano", "https://github.com/brunoalano"]
translators:
    - ["Mario Stabile", "https://github.com/mariostabile1"]
lang: it-it
---

CMake è un build tool multi-piattaforma e open-source. Questo tool ti permette di testare, compilare e creare pacchetti del tuo codice sorgente.

I problemi che CMake provara a risolvere sono quelli dei Makefile, 
dell'Autoconfigurazione multi-piattaforma (diversi interpreti di Make hanno comandi diversi) e la facilità d'uso nel collegamento di librerie di terze parti.

CMake è un sistema estensibile e open-source che gestisce il processo di compilazione in maniera simile a come farebbero i sistemi operativi, indipendentemente dal formato usato. A differenza di altri sistemi multi-piattaforma, CMake è progettato per essere usato insieme all'ambiente di compilazione nativo. Semplici file di configurazione collocati in ogni cartella dei sorgenti (chiamati CMakeLists.txt) sono usati per generare i file di compilazione standard (ad esempio, makefile su Unix e project/workspace in Windows MSVC) che sono utilizzati nel classico modo.

```cmake
# In CMake, questo è un commento

# Per eseguire il nostro codice, usa questi comandi:
#  - mkdir build && cd build
#  - cmake ..
#  - make
# 
# Con questi passaggi, seguiremo la pratica migliore per compilare in una sotto-cartella.
# La seconda riga chiderà a CMake di generare un nuovo Makefile dipendente dal sistema operativo.
# Infine, eseguiremo il comando nativo Make.

#------------------------------------------------------------------------------
# Le basi
#------------------------------------------------------------------------------
#
# Il file CMake DEVE essere chiamato "CMakeLists.txt".

# Configuriamo la versione minima di CMake per generare il Makefile
cmake_minimum_required (VERSION 2.8)

# Lancerà un errore FATAL_ERROR se la versione < 2.8
cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

# Definiamo il nome del nostro progetto, questo modificherà 
# le convenzioni di denominazione generate da CMake per alcune cartelle. 
# Possiamo passare il LANG del codice come secondo parametro
project (learncmake C)

# Settiamo la cartella del sorgente (è solo una convenzione)
set( LEARN_CMAKE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )
set( LEARN_CMAKE_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} )

# Risulta utile settare l'attuale versione del nostro codice nel sistema di compilazione
# usando uno stile `semver`
set (LEARN_CMAKE_VERSION_MAJOR 1)
set (LEARN_CMAKE_VERSION_MINOR 0)
set (LEARN_CMAKE_VERSION_PATCH 0)

# Passiamo le variabili (numero di versione) all'header del sorgente
configure_file (
  "${PROJECT_SOURCE_DIR}/TutorialConfig.h.in"
  "${PROJECT_BINARY_DIR}/TutorialConfig.h"
)

# Includiamo le Librerie
# In GCC, questo invocherà il comando "-I" 
include_directories( include )

# Dove sono installate le librerie aggiuntive? Nota: includi i percorsi
# delle librerie qui, i controlli successivi risolveranno il resto
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# Condizioni
if ( CONDITION )
  # Output!

  # Informazioni accessorie
  message(STATUS "My message")

  # Warning di CMake, continua a elaborare
  message(WARNING "My message")

  # Warning di CMake (dev), continua a elaborare
  message(AUTHOR_WARNING "My message")

  # Errore di CMake, continua a elaborare, ma salta la generazione
  message(SEND_ERROR "My message")

  # Errore di CMake, ferma l'elaborazione e la generazione
  message(FATAL_ERROR "My message")
endif()

if( CONDITION )

elseif( CONDITION )

else( CONDITION )

endif( CONDITION )

# Cicli
foreach(loop_var arg1 arg2 ...)
  COMMAND1(ARGS ...)
  COMMAND2(ARGS ...)
  ...
endforeach(loop_var)

foreach(loop_var RANGE total)
foreach(loop_var RANGE start stop [step])

foreach(loop_var IN [LISTS [list1 [...]]]
                    [ITEMS [item1 [...]]])

while(condition)
  COMMAND1(ARGS ...)
  COMMAND2(ARGS ...)
  ...
endwhile(condition)


# Operazioni logiche
if(FALSE AND (FALSE OR TRUE))
  message("Don't display!")
endif()

# Impostiamo una variabile regolare, di cache o di ambiente a un determinato valore.
# Se viene fornita l'opzione PARENT_SCOPE, la variabile verrà settata nello scope
# sopra lo scope corrente.
# `set(<variable> <value>... [PARENT_SCOPE])`

# Come fare riferimento a variabili all'interno di argomenti tra virgolette e non?
# Un riferimento a una variabile è sostituito sia dal valore della variabile, sia da 
# una stringa vuota se la variabile non è settata.
${variable_name}

# Liste
# Prepariamo la lista dei file sorgente
set( LEARN_CMAKE_SOURCES 
  src/main.c
  src/imagem.c
  src/pather.c
)

# Chiamate al compilatore
#
# ${PROJECT_NAME} fa riferimento a Learn_CMake 
add_executable( ${PROJECT_NAME} ${LEARN_CMAKE_SOURCES} )

# Link alle librerie
target_link_libraries( ${PROJECT_NAME} ${LIBS} m )

# Dove sono installate le librerie aggiuntive? Nota: includi i percorsi
# delle librerie qui, i controlli successivi risolveranno il resto
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# Condizioni del compilatore (gcc ; g++)
if ( "${CMAKE_C_COMPILER_ID}" STREQUAL "GNU" )
  message( STATUS "Setting the flags for ${CMAKE_C_COMPILER_ID} compiler" )
  add_definitions( --std=c99 )
endif()

# Controllo del sistema operativo
if( UNIX )
    set( LEARN_CMAKE_DEFINITIONS
        "${LEARN_CMAKE_DEFINITIONS} -Wall -Wextra -Werror -Wno-deprecated-declarations -Wno-unused-parameter -Wno-comment" )
endif()
```

### Maggiori risorse

+ [Tutorial CMake](https://cmake.org/cmake-tutorial/)
+ [Documentazione CMake](https://cmake.org/documentation/)
+ [Mastera CMake](http://amzn.com/1930934319/)
+ [Un'introduzione al CMake moderno](https://cliutils.gitlab.io/modern-cmake/)
