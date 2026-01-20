#ifndef CPU_H_
#define CPU_H_

#include "common.h"

#include <stdint.h>

#define FLAG_CARRY (1 << 4)
#define FLAG_HALFCARRY (1 << 5)
#define FLAG_NEGATIVE (1 << 6)
#define FLAG_ZERO (1 << 7)

#define FLAGS_IS_SET(f) (registers.F & (f))
#define FLAGS_SET(f) (registers.F |= (f))
#define FLAGS_CLEAR(f) (registers.F &= ~(f)) 

#define INTERRUPT_VBLANK (1 << 0)
#define INTERRUPT_LCDSTAT (1 << 1)
#define INTERRUPT_TIMER (1 << 2)
#define INTERRUPT_SERIAL (1 << 3)
#define INTERRUPT_JOYPAD (1 << 4)

struct cpu_t {
    uint64_t ticks;
} extern cpu;

struct registers_t {
    word_t PC;
    word_t SP;

    union {
        word_t AF;
        struct {
            byte_t F;
            byte_t A;
        };
    };

    union {
        word_t BC;
        struct {
            byte_t C;
            byte_t B;
        };
    };

    union {
        word_t DE;
        struct {
            byte_t E;
            byte_t D;
        };
    };

    union {
        word_t HL;
        struct {
            byte_t L;
            byte_t H;
        };
    };
} extern registers;

struct interrupts_t {
    byte_t master;
    byte_t enable;
    byte_t flags;
    byte_t pending;
} extern interrupts;

struct memory_t {
    byte_t cart[0x8000];
    byte_t vram[0x2000];
    byte_t sram[0x2000];
    byte_t wram[0x2000];
    byte_t oam[0xA0];
    byte_t io[0x80];
    byte_t hram[0x80];
} extern memory;

void storeb(byte_t value, word_t address);
byte_t loadb(word_t address);

void storew(word_t value, word_t address);
word_t loadw(word_t address);

void pushw(word_t value);
word_t popw();

void cpu_print_registers();

void cpu_reset();
void cpu_step();

void interrupt_vblank();
void interrupt_lcdstat();
void interrupt_timer();
void interrupt_serial();
void interrupt_joypad();

#endif // CPU_H_
