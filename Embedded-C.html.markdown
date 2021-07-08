---
language: Embedded C
contributors:
    - ["Ahmad Hegazy", "https://github.com/ahegazy/"]
filename: embeddedc.c
---

This is **NOT** a C programming tutorial, you can learn c from [here](https://learnxinyminutes.com/docs/c/). Actually, you should learn c first before getting into the embedded part. 

Here, I will talk about the microcontrollers related stuff, so hold you datasheet and let's start.
I will be using TM4C123XXX Arm Cortex-M4 Tiva C series' [datasheet](https://www.ti.com/lit/ds/symlink/tm4c123gh6pm.pdf) but the ideas are similar in all microcontrollers. 

Your feedback is appreciated, you can contact me [Here](https://ahegazy.github.io)

### TableOfContent @TODO 
- [x] BitMasking
- [ ] GPIO
- [ ] Interrupts
- [ ] UART
- [ ] I2C
- [ ] SPI 
- [ ] DMA
- [ ] Timer
- [ ] ADC 
- [ ] PWM
- [ ] CAN 
- [ ] Watchdog timer

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
  /* True if bit 7 (8th bit) is set (binary: 1xxxxxxx), x: don't care */
  /* NOTE: (foo & 0x80) won't necessarily return 1, but it'll return a value depending on `foo`'s value. */
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

/*
* NOTES regarding the Micro controllers in general.
*/
/*
* Every controller consists of some registers you'll need to configure to make it work, 
* Every thing needed is mostley mentioned in the datasheet,
* Registers are memory mapped, so you'll control them by writing in their respective address
* or reading from it.
* Every vendor provides a hardware memory mapping macros file contains the register and its address.
* you can find our kits TivaC-tm4c123 here < https://github.com/ahegazy/C/blob/master/TivaC-lcdDriver/tm4c123gh6pm.h > or search for yours online. 
* I will be including and using it directly without mentioning.
*/


/********************************
* 2. GPIO (General Purpose Input/Output)
*********************************/
/* The GPIO has about 35 register, you need to configure a handful of them depending on your application, I'll show you the way around the datasheet and you can apply the same technique for every thing else. 
*/

/* we will be deriving a LED to toggle depending on PushButton push.
* Our LED is connected to PORT F, Pin 2
* Our PushButton is connected to PORT F, Pin 4
* NOTE: Those are the built-in button and LED in the kit.
*/
/* We will be following the Initialization and Configuration section in the datasheet.*/
/* first we include our HW address macros.*/
#inlude "tm4c123gh6pm.h"

int main(void){
  /* Initialization. */
  
    /* Enanbe the clock in the port F, the 6th port*/
    SYSCTL_RCGCGPIO_R |= 1 << 5; 

    /* LED PF2 */
    GPIO_PORTF_DIR_R |= 1 << 2;  /* set the direction in bit 4 as output.*/
    GPIO_PORTF_AFSEL_R &= ~(1 << 2); /* set the alternate function as GPIO .. 0*/
    GPIO_PORTF_DR2R_R |= 1 << 2; /* set the output current 2mA for the LED.*/
    GPIO_PORTF_DEN_R |= 1 << 2; /* Enable Digital control in this pin NOT analog.*/

    /* we can use our macros too, it'll be using them from now on.*/
    /* ACTIVE-LOW push button PF4 */
    CLR_BITS(GPIO_PORTF_DIR_R, (1 << 4)); /* set the direction in bit 4 as input.*/
    CLR_BITS(GPIO_PORTF_AFSEL_R, (1 << 4)); /* set the alternate function as GPIO .. 0*/
    SET_BITS(GPIO_PORTF_DEN_R, (1 << 4)); /* Enable Digital control in this pin NOT analog.*/
    SET_BITS(GPIO_PORTF_PUR_R, (1 << 4)); /* Enable a pullup resistor connected to the button to prevent floating input.*/

    /* Now let's light the LED, by writing 1 on its respective pin.*/
    GPIO_PORTF_BIT_DATA(2) = 0xff;
    
    /* NOTE: The previous macro shifts the pin number to the right by two location, 
    * as per the data sheet PAGE 654, section 10.2.1.2 DATA Register operation,
    * which says the for the efficiency every pin/bit is controlled directly by using
    * bits [9:2] as a mask and the 1st two bits are zeros, so software can modify individual
    * GPIO pins in a single instruction instead of read-modify-write operations. */

    /* now let's get to the control.*/
    while(1){
      /* if the button is push, toggle the led.*/
      if( GPIO_PORTF_BIT_DATA(4) == 0){
        TOGGLE_BITS(GPIO_PORTF_BIT_DATA(2), 0xff);
      }
    }
}

/* That's it, we controlled a led by simply reading the datasheet initialization seciton
* We can control all peripherals using the same approach, that's the beauty in this datasheet.*/

/********************************
* 3. Interrupt
*********************************/

/********************************
* 4. UART (Universal Asynchronous Receiver Transmitter)
*********************************/

/********************************
* 5. I2C (Inter-Integrated Circuit)
*********************************/

/********************************
* 6. SPI (Serial Peripheral Interface)
*********************************/

/********************************
* 7. DMA (Direct Memory Access)
*********************************/

/********************************
* 8. Timer
*********************************/

/********************************
* 9. ADC (Analog to Digital Converter)
*********************************/

/********************************
* 10. PWM (Pulse Width Modulation)
*********************************/

/********************************
* 11. CAN (Controlled Area Network)
*********************************/

/********************************
* 12. Watchdog timer
*********************************/


```
## Further Reading
- Datasheet is your only friend, Every MicroController has a datasheet contains all the needed information.
- An Embedded Software Primer (book)
- Design Patterns for Embedded Systems in C: An Embedded Software Engineering Toolkit (book)
- MisraC coding standards