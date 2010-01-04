

/*  Macros from libAVR (included for reference; not used).
 */
#define _MMIO_BYTE(mem_addr) (*(volatile uint8_t *)(mem_addr))
#define _SFR_IO8(io_addr) _MMIO_BYTE((io_addr) + 0x20)


/*  Macros and types for registers.
 */
#define _OFFSET(io_addr) ((io_addr) + 0x20)
typedef volatile uint8_t const * r;


/*  Registers for ATtiny25 (not all of them).
 */
r DDRB = _OFFSET(0x17);
r PORTB = _OFFSET(0x18);





