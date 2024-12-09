---
category: tool
contributors:
    - ["Bruno Alano", "https://github.com/brunoalano"]
translators:
    - ["tx23", "https://github.com/tx23"]
filename: CMake-cn
---

CMake 是一个跨平台且开源的自动化构建系统工具。通过该工具你可以对你的源代码进行测试、编译或创建安装包。

CMake 试图去解决Makefile 跨平台的自动配置问题(不同的make解释器有不同的命令)，以及链接第三方库时的易用性问题。

CMake是一个可扩展的开源系统，它以操作系统和与编译器无关的方式管理构建过程。与其他许多跨平台系统不同的是，
CMake被设计为与本机的构建环境结合使用。它通过被放置于每个源文件目录下的简单配置文件（名为 CMakeLists.txt 的文件）
来生成常规使用的标准构建文件（比如：Unix 下的 makefiles 文件或 Windows MSVC 中的 projects/workspaces）。

```cmake
# 在 CMake 中, 这是一条命令

# 要运行我们的代码，请执行以下命令：
#  - mkdir build && cd build
#  - cmake ..
#  - make
# 
# 通过上述命令，我们将遵循最佳实践在子目录中进行编译
# 在第二行命令中我们请求Cmake 生成新的依赖于系统的Makefile文件。
# 最后，我们运行本地的make 命令。

#------------------------------------------------------------------------------
# 基础部分
#------------------------------------------------------------------------------
#
# Cmake文件必须被命令为 “CMakeLists.txt” 。

# 设置生成Makefile的CMake所需最低版本要求
cmake_minimum_required (VERSION 2.8)

# 当版本小于2.8时，需要加入关键字 FATAL_ERROR。
cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

# 在这里定义了项目的名称，同时会影响Cmake 生成的目录命名约定。
# 我们可以将代码的语言作为第二个参数传入。
project (learncmake C)

# 设置项目的源目录（仅仅是由于惯例）
set( LEARN_CMAKE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )
set( LEARN_CMAKE_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} )

# 在构建系统中用“semver”风格为我们代码设置当前版本是很有用的。
set (LEARN_CMAKE_VERSION_MAJOR 1)
set (LEARN_CMAKE_VERSION_MINOR 0)
set (LEARN_CMAKE_VERSION_PATCH 0)

# 将变量（版本号）发送到源代码头
configure_file (
  "${PROJECT_SOURCE_DIR}/TutorialConfig.h.in"
  "${PROJECT_BINARY_DIR}/TutorialConfig.h"
)

# 包含目录
# 在 GCC中, 该语句等同于 "-I" 命令
include_directories( include )

# 在哪里安装其他库？注意：在此处提供includes路径，后续检查将解决所有其他问题
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# 条件
if ( CONDITION )
  # 输出！

  # 附带信息
  message(STATUS "My message")

  # CMake 警告，继续处理
  message(WARNING "My message")

  # CMake 警告 (dev)，继续处理
  message(AUTHOR_WARNING "My message")

  # CMake 错误，继续处理但是会跳过生成
  message(SEND_ERROR "My message")

  # CMake 错误，停止处理和生成
  message(FATAL_ERROR "My message")
endif()

if( CONDITION )

elseif( CONDITION )

else( CONDITION )

endif( CONDITION )

# 循环
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


# 逻辑运算
if(FALSE AND (FALSE OR TRUE))
  message("Don't display!")
endif()

# 将常规，缓存或环境变量设置为给定值。
# 如果指定了PARENT_SCOPE选项，则将在当前作用域上的作用域中设置变量
# `set(<variable> <value>... [PARENT_SCOPE])`

# 如何在带引号和不带引号的参数中引用变量？How to reference variables inside quoted and unquoted arguments?
# 如果未设置变量，变量引用由变量值或空字符串替换。
${variable_name}

# 清单
# 设置源文件列表
set( LEARN_CMAKE_SOURCES 
  src/main.c
  src/imagem.c
  src/pather.c
)

# 调用编译器
#
# ${PROJECT_NAME} 即 Learn_CMake 
add_executable( ${PROJECT_NAME} ${LEARN_CMAKE_SOURCES} )

# 链接库
target_link_libraries( ${PROJECT_NAME} ${LIBS} m )

# 在哪里安装其他库？注意：在此处提供includes路径，后续检查将解决所有其他问题
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# 编译条件 (gcc ; g++)
if ( "${CMAKE_C_COMPILER_ID}" STREQUAL "GNU" )
  message( STATUS "Setting the flags for ${CMAKE_C_COMPILER_ID} compiler" )
  add_definitions( --std=c99 )
endif()

# 检查 OS
if( UNIX )
    set( LEARN_CMAKE_DEFINITIONS
        "${LEARN_CMAKE_DEFINITIONS} -Wall -Wextra -Werror -Wno-deprecated-declarations -Wno-unused-parameter -Wno-comment" )
endif()
```

### 资源

+ [CMake tutorial](https://cmake.org/cmake-tutorial/)
+ [CMake documentation](https://cmake.org/documentation/)
+ [Mastering CMake](http://amzn.com/1930934319/)
+ [An Introduction to Modern CMake](https://cliutils.gitlab.io/modern-cmake/)
