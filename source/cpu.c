#include "cpu.h"
#include "ppu.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct cpu_t cpu;
struct registers_t registers;
struct interrupts_t interrupts;
struct memory_t memory;

void storeb(byte_t value, word_t address) {
    if (address <= 0x7FFF) {
        memory.cart[address] = value;
    } else if (address >= 0x8000 && address <= 0x9FFF) {
        memory.vram[address - 0x8000] = value;
    } else if (address >= 0xA000 && address <= 0xBFFF) {
        memory.sram[address - 0xA000] = value;
    } else if (address >= 0xC000 && address <= 0xDFFF) {
        memory.wram[address - 0xC000] = value;
    } else if (address >= 0xE000 && address <= 0xFDFF) {
        memory.wram[address - 0xE000] = value;
    } else if (address >= 0xFE00 && address <= 0xFE9F) {
        memory.oam[address - 0xFE00] = value;
    } else if (address >= 0xFF00 && address <= 0xFF7F) {
        if (address == 0xFF0F) {
            interrupts.flags = value;
        } else {
            memory.io[address - 0xFF00] = value;
        }
    } else if (address >= 0xFF80 && address <= 0xFFFE) {
        memory.hram[address - 0xFF80] = value;
    } else if (address == 0xFFFF) {
        interrupts.enable = value;
    }
}

byte_t loadb(word_t address) {
    if (address <= 0x7FFF) {
        return memory.cart[address];
    } else if (address >= 0x8000 && address <= 0x9FFF) {
        return memory.vram[address - 0x8000];
    } else if (address >= 0xA000 && address <= 0xBFFF) {
        return memory.sram[address - 0xA000];
    } else if (address >= 0xC000 && address <= 0xDFFF) {
        return memory.wram[address - 0xC000];
    } else if (address >= 0xE000 && address <= 0xFDFF) {
        return memory.wram[address - 0xE000];
    } else if (address >= 0xFE00 && address <= 0xFE9F) {
        return memory.oam[address - 0xFE00];
    } else if (address >= 0xFF00 && address <= 0xFF7F) {
        if (address == 0xFF0F) {
            return interrupts.flags;
        } else {
            return memory.io[address - 0xFF00];
        }
    } else if (address >= 0xFF80 && address <= 0xFFFE) {
        return memory.hram[address - 0xFF80];
    } else if (address == 0xFFFF) {
        return interrupts.enable;
    } else {
        return 0xFF;
    }
}

void storew(word_t value, word_t address) {
    storeb((byte_t)(value & 0x00FF), address);
    storeb((byte_t)((value & 0xFF00) >> 8), address + 1);
}

word_t loadw(word_t address) {
    return loadb(address) | (loadb(address + 1) << 8);
}

void pushw(word_t value) {
    registers.SP -= 2;
    storew(value, registers.SP);
}

word_t popw() {
    word_t value = loadw(registers.SP);
    registers.SP += 2;
    return value;    
}

/// Logical AND |value| with register A, stores result in A.
static void op_and(byte_t value) {
    registers.A &= value;

    if (registers.A == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    FLAGS_SET(FLAG_HALFCARRY);
    FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_CARRY);
}

/// Logical OR |value| with register A, stores result in A.
static void op_or(byte_t value) {
    registers.A |= value;

    if (registers.A == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY | FLAG_CARRY);
}

/// Logical exclusive OR |value| with register A, stores result in A.
static void op_xor(byte_t value) {
    registers.A ^= value;

    if (registers.A == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY | FLAG_CARRY);
}

/// Add |value| + carry flag to A.
static void op_adc(byte_t value) {
    if (FLAGS_IS_SET(FLAG_CARRY)) {
        ++value;
    }

    int32_t add = registers.A + value;

    if (add & 0xFF00) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    if (add == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    if (((value & 0x0F) + (registers.A & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    FLAGS_SET(FLAG_NEGATIVE);

    registers.A = (byte_t)(add & 0xFF);
}

static void op_ldb(byte_t value, byte_t* reg) {
    *reg = value;
}

static void op_ldw(word_t value, word_t* reg) {
    *reg = value;
}

/// Increment register |value|.
static byte_t op_inc(byte_t value) {
    if ((value & 0x0F) == 0x0F) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    ++value;

    if (value == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    FLAGS_CLEAR(FLAG_NEGATIVE);
    return value;
}

/// Decrement register |value|.
static byte_t op_dec(byte_t value) {
    if ((value & 0x0F) == 0) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    --value;

    if (value == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    FLAGS_SET(FLAG_NEGATIVE);
    return value;
}

/// Compare register A with |value|. This is an A - |value| subtraction that
/// discards the result, but sets flags accordingly.
static void op_cp(byte_t value) {
    // If A == |value|, then A - |value| == 0.
    if (registers.A == value) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    if (registers.A < value) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    if ((registers.A & 0x0F) < (value & 0x0F)) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    FLAGS_SET(FLAG_NEGATIVE);
}

void cpu_print_registers() {
    printf(": PC 0x%04X : SP 0x%04X : A 0x%02X F 0x%02X : B 0x%02X C 0x%02X : D 0x%02X E 0x%02X : H 0x%02X L 0x%02X\n", 
        registers.PC, registers.SP, registers.A, registers.F, registers.B, registers.C, registers.D, registers.E, registers.H, registers.L);

    printf(": IME 0x%04X : IE 0x%04X : IF 0x%04X : I0xFF44 0x%02x\n", interrupts.master, interrupts.enable, interrupts.flags, loadb(0xFF44));
}

void cpu_reset() {
    registers.PC = 0x100;
    registers.SP = 0xFFFE;
    registers.A = 0x01; // GB/SGB: 0x01, GBP: 0xFF, GBC: 0x11
    registers.F = 0xB0;
    registers.B = 0x00;
    registers.C = 0x13;
    registers.D = 0x00;
    registers.E = 0xD8;
    registers.H = 0x01;
    registers.L = 0x4D;

    interrupts.master = 0;
    interrupts.enable = 0;
    interrupts.flags = 0;

    cpu.ticks = 0;

    storeb(0x00, 0xFF05);
    storeb(0x00, 0xFF06);
    storeb(0x00, 0xFF07);
    storeb(0x80, 0xFF10);
    storeb(0xBF, 0xFF11);
    storeb(0xF3, 0xFF12);
    storeb(0xBF, 0xFF14);
    storeb(0x3F, 0xFF16);
    storeb(0x00, 0xFF17);
    storeb(0xBF, 0xFF19);
    storeb(0x7F, 0xFF1A);
    storeb(0xFF, 0xFF1B);
    storeb(0x9F, 0xFF1C);
    storeb(0xBF, 0xFF1E);
    storeb(0xFF, 0xFF20);
    storeb(0x00, 0xFF21);
    storeb(0x00, 0xFF22);
    storeb(0xBF, 0xFF23);
    storeb(0x77, 0xFF24);
    storeb(0xF3, 0xFF25);
    storeb(0xF1, 0xFF26);
    storeb(0x91, 0xFF40);
    storeb(0x00, 0xFF42);
    storeb(0x00, 0xFF43);
    storeb(0x00, 0xFF45);
    storeb(0xFC, 0xFF47);
    storeb(0xFF, 0xFF48);
    storeb(0xFF, 0xFF49);
    storeb(0x00, 0xFF4A);
    storeb(0x00, 0xFF4B);
    storeb(0x00, 0xFFFF);
}

void cpu_step() {
    cpu_print_registers();

    if (registers.PC == 0x2817) {
        printf("reached 0x2817\n");
        exit(1);
    }

    if (registers.PC == 0x282A) {
        FILE* f = fopen("tile0.bin", "wb");
        fwrite(memory.vram, 16, 1, f);
        fclose(f);
    }

    // Load the opcode at the program counter (PC).
    byte_t opcode = loadb(registers.PC++);

    switch (opcode) {
        case 0x00: {
            cpu.ticks += 4;

            printf("NOP\n");
            break;
        }

        case 0x01: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;
            registers.BC = operand;
            cpu.ticks += 12;

            printf("LD BC, 0x%04X\n", operand);
            break;
        }

        case 0x02: {
            storeb(registers.A, registers.BC);
            cpu.ticks += 8;

            printf("LD (BC), A\n");
            break;
        }

        case 0x03: {
            registers.BC += 1;
            cpu.ticks += 8;

            printf("INC BC\n");
            break;
        }

        case 0x05: {
            registers.B = op_dec(registers.B);
            cpu.ticks += 4;

            printf("DEC B\n");
            break;
        }

        case 0x06: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            op_ldb(operand, &registers.B);
            cpu.ticks += 8;

            printf("LD B, 0x%02X\n", operand);
            break;
        }

        case 0x0B: {
            registers.BC--;
            cpu.ticks += 8;

            printf("DEC BC\n");
            break;
        }

        case 0x0C: {
            registers.C = op_inc(registers.C);
            cpu.ticks += 4;

            printf("INC C\n");
            break;
        }

        case 0x0D: {
            registers.C = op_dec(registers.C);
            cpu.ticks += 4;

            printf("DEC C\n");
            break;
        }

        case 0x0E: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            op_ldb(operand, &registers.C);

            printf("LD C, 0x%02X\n", operand);
            break;
        }

        case 0x14: {
            registers.D = op_inc(registers.D);
            cpu.ticks += 4;

            printf("INC B\n");
            break;
        }

        case 0x15: {
            registers.D = op_dec(registers.D);
            cpu.ticks += 4;

            printf("DEC D\n");
            break;
        }

        case 0x1F: {
            int32_t carry = FLAGS_IS_SET(FLAG_CARRY);
            carry <<= 7;

            if (registers.A & 1) {
                FLAGS_SET(FLAG_CARRY);
            } else {
                FLAGS_CLEAR(FLAG_CARRY);
            }

            registers.A >>= 1;
            registers.A += carry;

            if (registers.A == 0) {
                FLAGS_SET(FLAG_ZERO);
            } else {
                FLAGS_CLEAR(FLAG_ZERO);
            }

            FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY);

            printf("RRA\n");
            break;
        }

        case 0x20: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;

            if (!FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC += (signed char) operand;
                cpu.ticks += 12;
            } else {
                cpu.ticks += 8;
            }

            printf("JR NZ, 0x%02X\n", operand);
            break;
        }

        case 0x21: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;
            op_ldw(operand, &registers.HL);

            printf("LD HL, 0x%04X\n", operand);
            break;
        }

        case 0x28: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            
            if (FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC += (signed char) operand;
                cpu.ticks += 12;
            } else {
                cpu.ticks += 8;
            }

            printf("0x28: JR Z, 0x02%X\n", operand);
            break;
        }

        case 0x2A: {
            registers.A = loadb(registers.HL++);
            cpu.ticks += 8;

            printf("LD A, (HL+)\n");
            break;
        }

        case 0x2F: {
            registers.A = ~registers.A;
            FLAGS_SET(FLAG_NEGATIVE | FLAG_HALFCARRY);

            printf("CPL\n");
            break;
        }

        case 0x31: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;
            registers.SP = operand;
            cpu.ticks += 12;

            printf("LD SP, 0x%04X\n", operand);
            break;
        }

        case 0x32: {
            storeb(registers.A, registers.HL--);
            cpu.ticks += 8;

            printf("LD (HL-), A\n");
            break;
        }

        case 0x36: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            storeb(operand, registers.HL);
            cpu.ticks += 12;

            printf("LD (HL), 0x%02X\n", operand);
            break;
        }

        case 0x3E: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            op_ldb(operand, &registers.A);

            printf("LD A, 0x%02X\n", operand);
            break;
        }

        case 0x54: {
            registers.D = registers.H;
            cpu.ticks += 4;

            printf("LD D, H\n");
            break;
        }

        case 0x62: {
            registers.H = registers.D;
            cpu.ticks += 4;

            printf("LD H, D\n");
            break;
        }

        case 0x6B: {
            registers.L = registers.E;
            cpu.ticks += 4;

            printf("LD L, E\n");
            break;
        }

        case 0x70: {
            storeb(registers.B, registers.HL);
            cpu.ticks += 8;

            printf("LD (HL), B\n");
            break;
        }

        case 0x78: {
            registers.A = registers.B;
            cpu.ticks += 4;

            printf("LD A, B\n");
            break;
        }

        case 0x79: {
            op_ldb(registers.C, &registers.A);
            cpu.ticks += 4;

            printf("LD A, C\n");
            break;
        }

        case 0x7A: {
            op_ldb(registers.D, &registers.A);
            cpu.ticks += 4;

            printf("LD A, D\n");
            break;
        }

        case 0x89: {
            op_adc(registers.C);
            cpu.ticks += 4;

            printf("ADC A,C\n");
            break;
        }

        case 0xA7: {
            op_and(registers.A);
            cpu.ticks += 4;

            printf("AND A, A\n");
            break;
        }

        case 0xAF: {
            op_xor(registers.A);
            cpu.ticks += 4;

            printf("XOR A, A\n");
            break;
        }

        case 0xB1: {
            op_or(registers.C);
            cpu.ticks += 4;

            printf("OR A, C\n");
            break;
        }

        case 0xC0: {
            if (!FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = popw();
                cpu.ticks += 20;
            } else {
                cpu.ticks += 8;
            }

            printf("RET NZ\n");
            break;
        }

        case 0xC3: {
            word_t operand = loadw(registers.PC);
            registers.PC = operand;
            cpu.ticks += 16;

            printf("JP 0x%04X\n", operand);
            break;
        }

        case 0xC5: {
            pushw(registers.BC);
            cpu.ticks += 16;

            printf("PUSH BC\n");
            break;
        }

        case 0xC9: {
            registers.PC = popw();
            cpu.ticks += 16;

            printf("RET\n");
            break;
        }

        case 0xCD: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;
            pushw(registers.PC);
            registers.PC = operand;
            cpu.ticks += 24;

            printf("CALL 0x%04X\n", operand);
            break;
        }

        case 0xD5: {
            pushw(registers.DE);
            cpu.ticks += 16;

            printf("PUSH DE\n");
            break;
        }

        case 0xD9: {
            registers.PC = popw();
            interrupts.master = 1;
            cpu.ticks += 16;

            printf("RETI\n");
            exit(1);
        }

        case 0xDF: {
            pushw(registers.PC);
            registers.PC = 0x0018;
            cpu.ticks += 16;

            printf("RST 18H\n");
            break;
        }

        case 0xE0: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            storeb(registers.A, 0xFF00 + operand);
            cpu.ticks += 12;

            printf("LD (FF00+0x%02X), A\n", operand);
            break;
        }

        case 0xE2: {
            storeb(registers.A, 0xFF00 + registers.C);
            cpu.ticks += 8;

            printf("LD (FF00+C), A\n");
            break;
        }

        case 0xEA: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;
            storeb(registers.A, operand);
            cpu.ticks += 16;

            printf("LD (0x%04X), A\n", operand);
            break;
        }

        case 0xE5: {
            pushw(registers.HL);
            cpu.ticks += 16;

            printf("PUSH HL\n");
            break;
        }

        case 0xF0: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            registers.A = loadb((word_t)(0xFF00 + operand));
            cpu.ticks += 12;

            printf("LD A, (FF00+0x%02X)\n", operand);
            break;
        }

        case 0xF3: {
            interrupts.master = 0;
            cpu.ticks += 4;

            printf("DI\n");
            break;
        }

        case 0xF5: {
            pushw(registers.AF);
            cpu.ticks += 16;

            printf("PUSH AF\n");
            break;
        }

        case 0xFA: {
            word_t operand = loadw(registers.PC);
            registers.PC += operand;
            registers.A = loadw(operand);
            cpu.ticks += 16;

            printf("LD A, (0x%04X)\n", operand);
            break;
        }

        case 0xFB: {
            interrupts.pending = 2;
            cpu.ticks += 4;

            printf("EI\n");
            break;
        }

        case 0xFE: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            op_cp(operand);
            cpu.ticks += 8;
                
            printf("CP A, 0x%02X\n", operand);
            break;
        }

        case 0xFF: {
            pushw(registers.PC);
            registers.PC = 0x0038;
            cpu.ticks += 16;

            printf("RST 38H\n");
            break;
        }
    
        default:
            printf("Unimplemented opcode 0x%02X!\n", opcode);
            exit(1);
    }

    if (interrupts.pending > 0) {
        interrupts.pending--;
        if (interrupts.pending == 0)
            interrupts.master ^= 0xFFFF;
    }

    if (interrupts.master) {
        byte_t pending = interrupts.enable & interrupts.flags;
        if (pending) {
            if (pending & INTERRUPT_VBLANK) {
                interrupt_vblank();
            } else if (pending & INTERRUPT_LCDSTAT) {
                interrupt_lcdstat();
            } else if (pending & INTERRUPT_TIMER) {
                interrupt_timer();
            } else if (pending & INTERRUPT_SERIAL) {
                interrupt_serial();
            } else if (pending & INTERRUPT_JOYPAD) {
                interrupt_joypad();
            }
        }
    }    
}

void interrupt_vblank() {
    interrupts.flags &= ~INTERRUPT_VBLANK;
    interrupts.master = 0;

    draw();

    pushw(registers.PC);
    registers.PC = 0x40;
    cpu.ticks += 12;
}

void interrupt_lcdstat() {
    interrupts.flags &= ~INTERRUPT_LCDSTAT;
    interrupts.master = 0;

    pushw(registers.PC);
    registers.PC = 0x48;
    cpu.ticks += 12;
}

void interrupt_timer() {
    interrupts.flags &= ~INTERRUPT_TIMER;
    interrupts.master = 0;

    pushw(registers.PC);
    registers.PC = 0x50;
    cpu.ticks += 12;
}

void interrupt_serial() {
    interrupts.flags &= ~INTERRUPT_SERIAL;
    interrupts.master = 0;

    pushw(registers.PC);
    registers.PC = 0x58;
    cpu.ticks += 12;
}

void interrupt_joypad() {
    interrupts.flags &= ~INTERRUPT_JOYPAD;
    interrupts.master = 0;

    pushw(registers.PC);
    registers.PC = 0x60;
    cpu.ticks += 12;
}
