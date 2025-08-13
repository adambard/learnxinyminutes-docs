---
category: tool
name: CMake
contributors:
    - ["Bruno Alano", "https://github.com/brunoalano"]
filename: CMake
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

CMake는 크로스 플랫폼, 오픈 소스 빌드 시스템입니다. 이 도구를 사용하면 소스 코드를 테스트, 컴파일 및 패키지를 만들 수 있습니다.

CMake가 해결하려는 문제는 크로스 플랫폼(다른 make 인터프리터는 다른 명령을 가짐)에서 Makefile 및 Autoconfigure 문제와 타사 라이브러리 연결의 용이성입니다.

CMake는 운영 체제 및 컴파일러에 구애받지 않는 방식으로 빌드 프로세스를 관리하는 확장 가능한 오픈 소스 시스템입니다. 많은 크로스 플랫폼 시스템과 달리 CMake는 네이티브 빌드 환경과 함께 사용하도록 설계되었습니다. 각 소스 디렉토리에 있는 간단한 구성 파일(CMakeLists.txt 파일이라고 함)은 일반적인 방식으로 사용되는 표준 빌드 파일(예: Unix의 makefile 및 Windows MSVC의 프로젝트/작업 공간)을 생성하는 데 사용됩니다.

```cmake
# CMake에서 이것은 주석입니다.

# 코드를 실행하려면 다음 명령을 수행하십시오:
#  - mkdir build && cd build
#  - cmake ..
#  - make
#
# 이러한 단계를 통해 하위 디렉토리로 컴파일하는 모범 사례를 따르고 두 번째 줄은 CMake에 새 OS 종속 Makefile을 생성하도록 요청합니다. 마지막으로 네이티브 Make 명령을 실행합니다.

#------------------------------------------------------------------------------
# 기본
#------------------------------------------------------------------------------
#
# CMake 파일 이름은 "CMakeLists.txt"여야 합니다.

# Makefile을 생성하는 데 필요한 최소 CMake 버전을 설정합니다.
cmake_minimum_required (VERSION 2.8)

# 버전이 2.8 미만이면 FATAL_ERROR를 발생시킵니다.
cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

# 프로젝트 이름을 정의하면 CMake에서 생성된 일부 디렉토리 명명 규칙이 변경됩니다. 두 번째 매개변수로 코드의 LANG을 보낼 수 있습니다.
project (learncmake C)

# 프로젝트 소스 디렉토리를 설정합니다(규칙일 뿐).
set( LEARN_CMAKE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )
set( LEARN_CMAKE_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} )

# `semver` 스타일을 사용하여 빌드 시스템에 현재 코드 버전을 설정하는 것이 유용합니다.
set (LEARN_CMAKE_VERSION_MAJOR 1)
set (LEARN_CMAKE_VERSION_MINOR 0)
set (LEARN_CMAKE_VERSION_PATCH 0)

# 변수(버전 번호)를 소스 코드 헤더로 보냅니다.
configure_file (
  "${PROJECT_SOURCE_DIR}/TutorialConfig.h.in"
  "${PROJECT_BINARY_DIR}/TutorialConfig.h"
)

# 디렉토리 포함
# GCC에서 이것은 "-I" 명령을 호출합니다.
include_directories( include )

# 추가 라이브러리는 어디에 설치됩니까? 참고: 여기에 포함 경로를 제공하면 후속 확인에서 모든 것이 해결됩니다.
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# 조건
if ( CONDITION )
  # 출력!

  # 부수적인 정보
  message(STATUS "My message")

  # CMake 경고, 처리 계속
  message(WARNING "My message")

  # CMake 경고(dev), 처리 계속
  message(AUTHOR_WARNING "My message")

  # CMake 오류, 처리 계속하지만 생성 건너뛰기
  message(SEND_ERROR "My message")

  # CMake 오류, 처리 및 생성 중지
  message(FATAL_ERROR "My message")
endif()

if( CONDITION )

elseif( CONDITION )

else( CONDITION )

endif( CONDITION )

# 루프
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


# 논리 연산
if(FALSE AND (FALSE OR TRUE))
  message("Don't display!")
endif()

# 일반, 캐시 또는 환경 변수를 지정된 값으로 설정합니다.
# PARENT_SCOPE 옵션이 지정되면 변수는 현재 범위 위의 범위에 설정됩니다.
# `set(<variable> <value>... [PARENT_SCOPE])`

# 인용 및 비인용 인수 내에서 변수를 참조하는 방법은 무엇입니까?
# 변수 참조는 변수 값 또는 변수가 설정되지 않은 경우 빈 문자열로 대체됩니다.
${variable_name}

# 목록
# 소스 파일 목록을 설정합니다.
set( LEARN_CMAKE_SOURCES
  src/main.c
  src/imagem.c
  src/pather.c
)

# 컴파일러를 호출합니다.
#
# ${PROJECT_NAME}은 Learn_CMake를 참조합니다.
add_executable( ${PROJECT_NAME} ${LEARN_CMAKE_SOURCES} )

# 라이브러리를 링크합니다.
target_link_libraries( ${PROJECT_NAME} ${LIBS} m )

# 추가 라이브러리는 어디에 설치됩니까? 참고: 여기에 포함 경로를 제공하면 후속 확인에서 모든 것이 해결됩니다.
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# 컴파일러 조건 (gcc ; g++)
if ( "${CMAKE_C_COMPILER_ID}" STREQUAL "GNU" )
  message( STATUS "Setting the flags for ${CMAKE_C_COMPILER_ID} compiler" )
  add_definitions( --std=c99 )
endif()

# OS 확인
if( UNIX )
    set( LEARN_CMAKE_DEFINITIONS
        "${LEARN_CMAKE_DEFINITIONS} -Wall -Wextra -Werror -Wno-deprecated-declarations -Wno-unused-parameter -Wno-comment" )
endif()
```

### 더 많은 자료

+ [CMake 튜토리얼](https://cmake.org/cmake-tutorial/)
+ [CMake 문서](https://cmake.org/documentation/)
+ [CMake 마스터하기](http://amzn.com/1930934319/)
+ [현대 CMake 소개](https://cliutils.gitlab.io/modern-cmake/)