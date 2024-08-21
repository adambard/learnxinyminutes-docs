---
category: tool
tool: linker
contributors:
    - ["Alexander Kovalchuk", "https://github.com/Zamuhrishka"]
translators:
    - ["Anuj Shah", "https://github.com/ShahAnuj2610"]
---

## Basic concepts and definitions

**Position counter** - the linker has a special variable
"." (dot) always contains the current output position.

## Functions

**ADDR (section)** - returns the absolute address of the specified section. However
this section must be defined before using the ADDR function.

**ALIGN (exp)** - returns the value of the position counter aligned to the border
following the exp expression.

**SIZEOF (section)** - returns the size of the section in bytes.

**FILL (param)** - defines the fill pattern for the current section. All
other unspecified regions within the section are filled with the value indicated
in function argument.

**KEEP (param)** - used to mark param as fatal.

**ENTRY (func)** - defines the function that will be the entry point
into the program.

```bash
# Determine the entry point to the program
ENTRY(Reset_Handler)

# Define a variable that contains the address of the top of the stack
_estack = 0x20020000;
# Define a variable that contains a heap size value
_Min_Heap_Size = 0x200;
# Define a variable that contains the value of the stack size
_Min_Stack_Size = 0x400;

# Description of the memory card available for this processor
# MEMORY
# {
#MEMORY_DOMAIN_NAME (access rights): ORIGIN = START_ADDRESS, LENGTH = SIZE
# }
# In our example, the controller contains three memory areas:
# RAM - starts with the address 0x20000000 and takes 128 KB;
# CCMRAM - starts with the address 0x10000000 and occupies 64 KB;
# FLASH - starts with the address 0x8000000; takes 1024 Kb;
# Moreover, RAM memory access for reading, writing and execution.
# CCMRAM memory is read-write only.
# FLASH memory is available for reading and execution.
MEMORY
{
	RAM 		(xrw)     : 	ORIGIN = 0x20000000, 	LENGTH = 128K
	CCMRAM 		(rw)      : 	ORIGIN = 0x10000000, 	LENGTH = 64K
	FLASH 		(rx)      : 	ORIGIN = 0x8000000, 	LENGTH = 1024K
}

# We describe output sections
SECTIONS
{
	# The first section contains a table of interrupt vectors
  .isr_vector :
  {
	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

	# There is an option --gc-sections, which allows you to collect garbage from unused
	# input sections. And if there are sections that the garbage collector should not touch,
	# you need to specify them as an argument to the KEEP () function (analogue of the keyword
	# volatile).
	# The entry (* (. Isr_vector)) means the .isr_vector sections in all object files. Because
	# appeal to the section in general terms looks like this: (FILE_NAME (SECTION_NAME))
    KEEP(*(.isr_vector))

	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

	# The expression "> MEMORY AREA" indicates which area of ​​memory will be placed
	# this section. In our section, the .isr_vector section will be located in FLASH memory.
  } >FLASH

# TOTAL: The .isr_vector section that contains the table of interrupt vectors is aligned
# on the border of 4 bytes, marked as inaccessible to the garbage collector and placed at the beginning
# FLASH microcontroller memory.

  # The second section contains the program code.
  .text :
  {
	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

    # We indicate that in this section the .text areas of all
	# object files
    *(.text)
    *(.text*)

	# Protect the .init and .fini sections from the garbage collector
    KEEP (*(.init))
    KEEP (*(.fini))

	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

	# The variable _etext is defined, which stores the address of the end of the .text section and which
	# may be available in the source code of the program through the announcement
	# volaile unsigned int extern _etext;
    _etext = .;
  } >FLASH

# TOTAL: The .text section that contains the program code is aligned on the border of 4 bytes,
# includes: all sections with program code in all object files and protected
# from the garbage collector of the .init and .fini sections in all object files, located in FLASH
# microcontroller memory immediately after the table of vectors.
# The text, .init, and .fini sections. are located in memory in the order in which they
# declared in the script.

  # The third section contains constant data.
  .rodata :
  {
	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

	# We indicate that in this section areas .rodata will be stored
	# object files
    *(.rodata)
    *(.rodata*)

	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);
  } >FLASH

  # Save the absolute address of the .data section in the _sidata variable
  _sidata = LOADADDR(.data);

  # The fourth section contains initialized variables.
  .data :
  {
	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

	# Save the address of the current position (beginning of the section) in the variable _sdata
    _sdata = .;

	# We indicate that in this section the .data areas of all
	# object files
    *(.data)
    *(.data*)

	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

    # Save the address of the current position (end of section) in the variable _sdata
    _edata = .;

	# AT function indicates that this sector is stored in one memory area
	# (in our case, FLASH), and it will be executed from another area of ​​memory (in our case, RAM).
	# There are two types of addresses:
	# * VMA (Virtual memory address) - this is the run-time address at which the compiler expects
	# see data.
	# * LMA (Load memory address) is the address at which the linker stores data.

	#Startup must code to copy the .data section from the LMA addresses to the VMA addresses.

  } >RAM AT> FLASH

  # The fifth section contains zero-initialized variables.
  .bss :
  {
	# Save the address of the current position (beginning of the section) in the variable _sbss and __bss_start__
    _sbss = .;
    __bss_start__ = _sbss;

	# We indicate that in this section the .bss areas of all
	# object files
    *(.bss)
    *(.bss*)

	# Align the current position to the border of 4 bytes.
    . = ALIGN(4);

    # Save the address of the current position (beginning of the section) in the variable _ebss and __bss_end__
    _ebss = .;
    __bss_end__ = _ebss;
  } >RAM

  # The sixth section contains a bunch and a stack. It is located at the very end of RAM.
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
