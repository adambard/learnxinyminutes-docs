---
language: Embedded C
contributors:
    - ["Ahmad Hegazy", "https://github.com/ahegazy/"]
filename: embeddedc.c
---

This is **NOT** a C programming tutorial, you can learn c from [here](https://learnxinyminutes.com/docs/c/). Actually, you should learn c first before getting into the embedded part. 

Here, I will talk about the microcontrollers related stuff, so hold you datasheet and let's start.
I will be using TM4C123XXX Arm Cortex-M4 Tiva C series' (datasheet)[www.ti.com/lit/ds/symlink/tm4c123gh6pm.pdf] but the ideas are similar in all microcontrollers. 

Your feedback is appreciated, you can contact me (Here)[https://github.com/ahegazy]

# TableOfContent @TODO 
- [x] BitMasking
- [] GPIO
- [] Interrupts
- [] UART
- [] I2C
- [] SPI 
- [] DMA
- [] ADC 
- [] PWM
- [] CAN 
- [] Watchdog

```c
/*
* Every Microcontroller (UC) consists of some pins used as IO pins and attached to
* one or more peripheral like UART, SPI/SSI, I2C, CAN, USB, 
* PWM, Interrupts, ADC, DMA, and Watchdog.
*/

/* UCs in general have multiple ports used as general purpose IO ports, A port consists of 8 pins
 * TivaC has 6 ports A->E each port can be addressed individually using a certain register
 * accessed as a memory location.  
 */


/********************************
* 1. BitMasking
*********************************/
/* 
  * We all know the basic operators and their truth tables.
    *  | bit OR & bit AND
    *  ~ bit NOT
    *  ^ bit EXLUSIVE OR (XOR)
    *  << bit LEFT SHIFT
    *  >> bit RIGHT SHIFT
  * These operators can be used to address a certain pin in the port.
  * A MASK is a value specifying a certain bit/group of bits in the byte,
  * by putting a 1 in its location (setting the bit), and zero in the rest (clearing)
  */

unsigned char MASK;
/* Masking the 2st bit */
unsigned char MASK = 0x02; /* =>binary: 00000010 */
/* Masking the 3rd and 8th bit */
unsigned char MASK = 0x84; /* =>binary: 10000100 */

/* let's consider an unsigned 8 bit variable */
unsigned char foo = 0; /* => binary: 00000000 */

/* To set bit 0 in foo and then store the result back into foo: */
foo |= 0x01; /* => binary: 00000001 */
/* So ORing operation can be used to set a certain bit without affecting the rest of the byte */

/* To clear bit 0 in foo: */
foo &= ~0x01; /* => binary: 00000000 */
/* same as the ORing operation, ANDing can be used to clear a certain bit in the byte
 * we use NOT to make the mask wherein the bit that we are interested in changing, is set.
 * so the mask will look like 0x01(00000001b) instead of 0xFE(11111110b)
*/

/* To check if a certain bit is set or clear without chaning it */
if (foo & 0x80) {
  /* True if bit 7 (8th bit) is set (binary: 1xxxxxxx), x: don't care*/
}

/* To toggle a bit use XOR */
foo ^= 0x01; /* => binary: 00000001 */

/* To specify the bit NUMBER instead the bit MASK, shifts can be used.
 * The bit number always starts at 0 and increases by 1 for each bit.
 */
MASK = (0x01 << 2) /* =>binary: 00000100, Hexa: 0x04*/

/* To sumup, Macros can be used to automate the operation, 
 * We can create macros to set, clear, and toggle. 
*/
#define SET_BITS(VAR,MASK) VAR |= (MASK)
#define CLR_BITS(VAR,MASK) VAR &= ~(MASK)
#define TOGGLE_BITS(VAR,MASK) VAR ^= (MASK)
#define GET_BITS(VAR,MASK) ((VAR) & (MASK))
#define BIT(x) (0x01 << (x))

/* set 3rd bit in foo: */
SET_BITS(foo,BIT(2));

/* Check if the 5th bit is 1 */
if( GET_BITS( foo, BIT(4) ) ){
  /* True if bit 4 (5th bit) is 1 (binary: xxx1xxxx)*/
}

/********************************
* 2. GPIO (General Purpose Input/Output)
*********************************/

```
## Further Reading
