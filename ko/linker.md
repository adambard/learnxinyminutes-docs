---
category: tool
name: Linker script
contributors:
    - ["Alexander Kovalchuk", "https://github.com/Zamuhrishka"]
translators:
    - ["Anuj Shah", "https://github.com/ShahAnuj2610"]
filename: learn.ld
---

**위치 카운터** - 링커에는 특수 변수가 있습니다.
"`.`" (점)은 항상 현재 출력 위치를 포함합니다.

`ADDR (section)` - 지정된 섹션의 절대 주소를 반환합니다. 그러나
이 섹션은 ADDR 함수를 사용하기 전에 정의되어야 합니다.

`ALIGN (exp)` - exp 표현식 다음의 경계에 정렬된 위치 카운터의 값을
반환합니다.

`SIZEOF (section)` - 섹션의 크기를 바이트 단위로 반환합니다.

`FILL (param)` - 현재 섹션의 채우기 패턴을 정의합니다. 모든
섹션 내의 다른 지정되지 않은 영역은 함수 인수에 표시된 값으로
채워집니다.

`KEEP (param)` - param을 치명적인 것으로 표시하는 데 사용됩니다.

`ENTRY (func)` - 프로그램의 진입점이 될 함수를
정의합니다.

```bash
# 프로그램의 진입점을 결정합니다.
ENTRY(Reset_Handler)

# 스택의 맨 위 주소를 포함하는 변수를 정의합니다.
_estack = 0x20020000;
# 힙 크기 값을 포함하는 변수를 정의합니다.
_Min_Heap_Size = 0x200;
# 스택 크기 값을 포함하는 변수를 정의합니다.
_Min_Stack_Size = 0x400;

# 이 프로세서에서 사용할 수 있는 메모리 카드 설명
# MEMORY
# {
#   MEMORY_DOMAIN_NAME (액세스 권한) : ORIGIN = START_ADDRESS, LENGTH = SIZE
# }
# 이 예에서 컨트롤러에는 세 개의 메모리 영역이 있습니다:
# RAM - 주소 0x20000000에서 시작하여 128KB를 차지합니다.
# CCMRAM - 주소 0x10000000에서 시작하여 64KB를 차지합니다.
# FLASH - 주소 0x8000000에서 시작하여 1024KB를 차지합니다.
# 또한 RAM 메모리 액세스는 읽기, 쓰기 및 실행이 가능합니다.
# CCMRAM 메모리는 읽기-쓰기만 가능합니다.
# FLASH 메모리는 읽기 및 실행이 가능합니다.
MEMORY
{
    RAM    (xrw) : ORIGIN = 0x20000000,  LENGTH = 128K
    CCMRAM (rw)  : ORIGIN = 0x10000000,  LENGTH = 64K
    FLASH  (rx)  : ORIGIN = 0x8000000,   LENGTH = 1024K
}

# 출력 섹션을 설명합니다.
SECTIONS
{
  # 첫 번째 섹션에는 인터럽트 벡터 테이블이 포함됩니다.
  .isr_vector :
  {
    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 사용하지 않는 입력 섹션에서 가비지를 수집할 수 있는 --gc-sections 옵션이 있습니다.
    # 그리고 가비지 수집기가 건드리지 않아야 하는 섹션이 있는 경우
    # KEEP () 함수의 인수로 지정해야 합니다 (volatile 키워드와 유사).
    # 항목 (* (. Isr_vector))은 모든 객체 파일의 .isr_vector 섹션을 의미합니다. 왜냐하면
    # 일반적으로 섹션에 대한 호소는 다음과 같이 보입니다: (FILE_NAME (SECTION_NAME))
    KEEP(*(.isr_vector))

    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 표현식 "> MEMORY AREA"는 어떤 메모리 영역이 배치될지를 나타냅니다.
    # 이 섹션. 이 섹션에서 .isr_vector 섹션은 FLASH 메모리에 위치합니다.
  } >FLASH

# 총계: 인터럽트 벡터 테이블을 포함하는 .isr_vector 섹션은
# 4바이트 경계에 정렬되고, 가비지 수집기에 액세스할 수 없는 것으로 표시되며, 맨 처음에 배치됩니다.
# FLASH 마이크로컨트롤러 메모리.

  # 두 번째 섹션에는 프로그램 코드가 포함됩니다.
  .text :
  {
    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 이 섹션에서 모든 객체 파일의 .text 영역을
    # 나타냅니다.
    *(.text)
    *(.text*)

    # 가비지 수집기로부터 .init 및 .fini 섹션을 보호합니다.
    KEEP (*(.init))
    KEEP (*(.fini))

    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 변수 _etext는 .text 섹션의 끝 주소를 저장하는 변수로 정의되며,
    # 프로그램 소스 코드에서 다음 선언을 통해 사용할 수 있습니다.
    # volaile unsigned int extern _etext;
    _etext = .;
  } >FLASH

# 총계: 프로그램 코드를 포함하는 .text 섹션은 4바이트 경계에 정렬되고,
# 모든 객체 파일의 모든 프로그램 코드 섹션과 모든 객체 파일의 .init 및 .fini 섹션의
# 가비지 수집기로부터 보호되며, 벡터 테이블 바로 뒤에 FLASH
# 마이크로컨트롤러 메모리에 위치합니다.
# text, .init 및 .fini 섹션은 스크립트에 선언된 순서대로
# 메모리에 위치합니다.

  # 세 번째 섹션에는 상수 데이터가 포함됩니다.
  .rodata :
  {
    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 이 섹션 영역 .rodata가 저장될 것임을 나타냅니다.
    # 객체 파일
    *(.rodata)
    *(.rodata*)

    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);
  } >FLASH

  # _sidata 변수에 .data 섹션의 절대 주소를 저장합니다.
  _sidata = LOADADDR(.data);

  # 네 번째 섹션에는 초기화된 변수가 포함됩니다.
  .data :
  {
    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 현재 위치(섹션 시작)의 주소를 _sdata 변수에 저장합니다.
    _sdata = .;

    # 이 섹션에서 모든 객체 파일의 .data 영역을
    # 나타냅니다.
    *(.data)
    *(.data*)

    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 현재 위치(섹션 끝)의 주소를 _sdata 변수에 저장합니다.
    _edata = .;

    # AT 함수는 이 섹터가 한 메모리 영역에 저장됨을 나타냅니다.
    # (이 경우 FLASH), 다른 메모리 영역(이 경우 RAM)에서 실행됩니다.
    # 두 가지 유형의 주소가 있습니다:
    # * VMA (가상 메모리 주소) - 컴파일러가 예상하는 런타임 주소입니다.
    # 데이터를 봅니다.
    # * LMA (로드 메모리 주소)는 링커가 데이터를 저장하는 주소입니다.

    # 시작 코드는 .data 섹션을 LMA 주소에서 VMA 주소로 복사해야 합니다.

  } >RAM AT> FLASH

  # 다섯 번째 섹션에는 0으로 초기화된 변수가 포함됩니다.
  .bss :
  {
    # 현재 위치(섹션 시작)의 주소를 _sbss 및 __bss_start__ 변수에 저장합니다.
    _sbss = .;
    __bss_start__ = _sbss;

    # 이 섹션에서 모든 객체 파일의 .bss 영역을
    # 나타냅니다.
    *(.bss)
    *(.bss*)

    # 현재 위치를 4바이트 경계에 맞춥니다.
    . = ALIGN(4);

    # 현재 위치(섹션 시작)의 주소를 _ebss 및 __bss_end__ 변수에 저장합니다.
    _ebss = .;
    __bss_end__ = _ebss;
  } >RAM

  # 여섯 번째 섹션에는 힙과 스택이 포함됩니다. RAM의 맨 끝에 위치합니다.
  ._user_heap_stack :
  {
    . = ALIGN(4);
    PROVIDE ( end = . );
    PROVIDE ( _end = . );
    . = . + _Min_Heap_Size;
    . = . + _Min_Stack_Size;
    . = ALIGN(4);
  } >RAM
}
```
