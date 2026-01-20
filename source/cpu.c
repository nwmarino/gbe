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
        if (address <= 0x97FF)
            update_tile(value, address - 0x8000);
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
            return;
        }

        memory.io[address - 0xFF00] = value;

        if (address == 0xFF47) {
            for (uint32_t i = 0; i < 4; ++i) {
                bg_palette[i] = palette[(value >> (i * 2)) & 3];
            }
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

/// Add |value| to register A, stores result in A.
static void op_add8(byte_t value) {
    if (((registers.A & 0x0F) + (value & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }
    
    registers.A += value;

    if (registers.A & 0xFF00) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    if (registers.A == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    FLAGS_CLEAR(FLAG_NEGATIVE);
}

/// Add |value| to HL, stores result in HL.
static void op_add16(word_t value) {
    if (((registers.HL & 0x0F) + (value & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    registers.HL += value;

    if (registers.HL & 0xFFFF0000) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    FLAGS_CLEAR(FLAG_NEGATIVE);
}

/// Subtract |value| from register A, stores result in A.
static void op_sub8(byte_t value) {
    FLAGS_SET(FLAG_NEGATIVE);

    if (value > registers.A) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    if ((value & 0x0F) > (registers.A & 0x0F)) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    registers.A -= value;

    if (registers.A == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }
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

/// Subtract |value| + carry flag from A.
static void op_sbc(byte_t value) {
    if (FLAGS_IS_SET(FLAG_CARRY))
        value += 1;

    FLAGS_SET(FLAG_NEGATIVE);

    if (value > registers.A) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    if (value == registers.A) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

    if ((value & 0x0F) > (registers.A & 0x0F)) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    registers.A -= value;
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

/// Swap upper & lower nibbles of |value|.
static byte_t swap(byte_t value) {
	value = ((value & 0xF) << 4) | ((value & 0xF0) >> 4);
	
	if (value == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

	FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY | FLAG_CARRY);
	return value;
}

void handle_cb() {
    byte_t opcode = loadb(registers.PC++);

    switch (opcode) {
        case 0x37: {
            registers.A = swap(registers.A);
            cpu.ticks += 8;
            
            printf("SWAP A\n");
            break;
        }

        case 0x87: {
            registers.A &= ~(1 << 0);
            cpu.ticks += 8;

            printf("RES 0, A\n");
            break;
        }

        default:
            printf("Unimplemented CB opcode 0x%02X!\n", opcode);
            exit(1);
    }
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
            registers.C = operand;
            cpu.ticks += 8;

            printf("LD C, 0x%02X\n", operand);
            break;
        }

        case 0x11: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;
            registers.DE = operand;
            cpu.ticks += 12;

            printf("LD DE, 0x%04X\n", operand);
            break;
        }

        case 0x12: {
            storeb(registers.A, registers.DE);
            cpu.ticks += 8;

            printf("LD (DE), A\n");
            break;
        }

        case 0x13: {
            registers.DE += 1;
            cpu.ticks += 8;

            printf("INC DE\n");
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

        case 0x16: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            registers.D = operand;
            cpu.ticks += 8;

            printf("LD D, 0x%02X\n", operand);
            break;
        }

        case 0x18: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            registers.PC += (signed char) operand;
            cpu.ticks += 12;

            printf("JR 0x%02X\n", operand);
            break;
        }

        case 0x19: {
            op_add16(registers.DE);
            cpu.ticks += 8;
            
            printf("ADD HL, DE\n");
            break;
        }

        case 0x1A: {
            registers.A = loadb(registers.DE);
            cpu.ticks += 8;

            printf("LD A, (DE)\n");
            break;
        }

        case 0x1C: {
            registers.E = op_inc(registers.E);
            cpu.ticks += 4;

            printf("INC E\n");
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
            cpu.ticks += 4;

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
            registers.HL = operand;
            cpu.ticks += 12;

            printf("LD HL, 0x%04X\n", operand);
            break;
        }

        case 0x22: {
            storeb(registers.A, registers.HL++);
            cpu.ticks += 8;

            printf("LD (HL+), A\n");
            break;
        }

        case 0x23: {
            registers.HL++;
            cpu.ticks += 8;

            printf("INC HL\n");
            break;
        }

        case 0x27: {
            byte_t A = registers.A;
            byte_t adjust = 0;
            byte_t carry = 0;

            if (!FLAGS_IS_SET(FLAG_NEGATIVE)) {
                if (FLAGS_IS_SET(FLAG_HALFCARRY) || (A & 0x0F) > 0x09) {
                    adjust |= 0x06;
                }

                if (FLAGS_IS_SET(FLAG_CARRY) || A > 0x99) {
                    adjust |= 0x60;
                    carry = 1;
                }

                A += adjust;
            } else {
                if (FLAGS_IS_SET(FLAG_HALFCARRY)) {
                    adjust |= 0x06;
                }

                if (FLAGS_IS_SET(FLAG_CARRY)) {
                    adjust |= 0x60;
                }

                A -= adjust;
            }

            FLAGS_CLEAR(FLAG_HALFCARRY);

            if (A == 0) {
                FLAGS_SET(FLAG_ZERO);
            } else {
                FLAGS_CLEAR(FLAG_ZERO);
            }

            if (carry) {
                FLAGS_SET(FLAG_CARRY);
            } else {
                FLAGS_CLEAR(FLAG_CARRY);
            }

            cpu.ticks += 4;

            printf("DAA\n");
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
            cpu.ticks += 4;

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

        case 0x34: {
            storeb(op_inc(loadb(registers.HL)), registers.HL);
            cpu.ticks += 12;

            printf("INC (HL)\n");
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

        case 0x3B: {
            registers.SP--;
            cpu.ticks += 8;

            printf("DEC SP\n");
            break;
        }

        case 0x3C: {
            registers.A = op_inc(registers.A);
            cpu.ticks += 4;

            printf("INC A\n");
            break;
        }

        case 0x3D: {
            registers.A = op_dec(registers.A);
            cpu.ticks += 4;

            printf("DEC A\n");
            break;
        }

        case 0x3E: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            registers.A = operand;
            cpu.ticks += 8;

            printf("LD A, 0x%02X\n", operand);
            break;
        }

        case 0x47: {
            registers.B = registers.A;
            cpu.ticks += 4;

            printf("LD B, A\n");
            break;
        }

        case 0x4F: {
            registers.C = registers.A;
            cpu.ticks += 4;

            printf("LD C, A\n");
            break;
        }

        case 0x54: {
            registers.D = registers.H;
            cpu.ticks += 4;

            printf("LD D, H\n");
            break;
        }

        case 0x56: {
            registers.D = loadb(registers.HL);
            cpu.ticks += 8;

            printf("LD D, (HL)\n");
            break;
        }

        case 0x5E: {
            registers.E = loadb(registers.HL);
            cpu.ticks += 8;

            printf("LD E, (HL)\n");
            break;
        }

        case 0x5F: {
            registers.E = registers.A;
            cpu.ticks += 4;

            printf("LD E, A\n");
            break;
        }

        case 0x62: {
            registers.H = registers.D;
            cpu.ticks += 4;

            printf("LD H, D\n");
            break;
        }

        case 0x67: {
            registers.H = registers.A;
            cpu.ticks += 4;

            printf("LD H, A\n");
            break;
        }

        case 0x6B: {
            registers.L = registers.E;
            cpu.ticks += 4;

            printf("LD L, E\n");
            break;
        }

        case 0x6F: {
            registers.L = registers.A;
            cpu.ticks += 4;

            printf("LD L, A\n");
            break;
        }

        case 0x70: {
            storeb(registers.B, registers.HL);
            cpu.ticks += 8;

            printf("LD (HL), B\n");
            break;
        }

        case 0x77: {
            storeb(registers.A, registers.HL);
            cpu.ticks += 8;

            printf("LD (HL), A\n");
            break;
        }

        case 0x78: {
            registers.A = registers.B;
            cpu.ticks += 4;

            printf("LD A, B\n");
            break;
        }

        case 0x79: {
            registers.A = registers.C;
            cpu.ticks += 4;

            printf("LD A, C\n");
            break;
        }

        case 0x7A: {
            registers.A = registers.D;
            cpu.ticks += 4;

            printf("LD A, D\n");
            break;
        }

        case 0x7B: {
            registers.A = registers.E;
            cpu.ticks += 4;

            printf("LD A, E\n");
            break;
        }

        case 0x7C: {
            registers.A = registers.H;
            cpu.ticks += 4;

            printf("LD A, H\n");
            break;
        }

        case 0x7E: {
            registers.A = loadb(registers.HL);
            cpu.ticks += 8;

            printf("LD A, (HL)\n");
            break;
        }

        case 0x80: {
            op_add8(registers.B);
            cpu.ticks += 4;

            printf("ADD A, B\n");
            break;
        }

        case 0x86: {
            op_add8(loadb(registers.HL));
            cpu.ticks += 8;

            printf("ADD A, (HL)\n");
            break;
        }

        case 0x87: {
            op_add8(registers.A);
            cpu.ticks += 4;

            printf("ADD A, A\n");
            break;
        }

        case 0x89: {
            op_adc(registers.C);
            cpu.ticks += 4;

            printf("ADC A,C\n");
            break;
        }

        case 0x8C: {
            op_adc(registers.H);
            cpu.ticks += 4;

            printf("ADC A, H\n");
            break;
        }

        case 0x8E: {
            op_adc(loadb(registers.HL));
            cpu.ticks += 8;

            printf("ADC A, (HL)\n");
            break;
        }

        case 0x92: {
            op_sub8(registers.D);
            cpu.ticks += 4;

            printf("SUB A, D\n");
            break;
        }

        case 0x99: {
            op_sbc(registers.C);
            cpu.ticks += 4;

            printf("SBC A, C\n");
            break;
        }

        case 0xA1: {
            op_and(registers.C);
            cpu.ticks += 4;

            printf("AND A, C\n");
            break;
        }

        case 0xA7: {
            op_and(registers.A);
            cpu.ticks += 4;

            printf("AND A, A\n");
            break;
        }

        case 0xA9: {
            op_xor(registers.C);
            cpu.ticks += 4;

            printf("XOR A, C\n");
            break;
        }

        case 0xAF: {
            op_xor(registers.A);
            cpu.ticks += 4;

            printf("XOR A, A\n");
            break;
        }

        case 0xB0: {
            op_or(registers.B);
            cpu.ticks += 4;

            printf("OR A, B\n");
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

        case 0xC1: {
            registers.BC = popw();
            cpu.ticks += 12;

            printf("POP BC\n");
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

        case 0xC8: {
            if (FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = popw();
                cpu.ticks += 20;
            } else {
                cpu.ticks += 8;
            }

            printf("RET Z\n");
            break;
        }

        case 0xC9: {
            registers.PC = popw();
            cpu.ticks += 16;

            printf("RET\n");
            break;
        }

        case 0xCA: {
            word_t operand = loadw(registers.PC);
            registers.PC += 2;

            if (FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = operand;
                cpu.ticks += 16;
            } else {
                cpu.ticks += 12;
            }

            printf("JP Z, 0x%04X\n", operand);
            break;
        }

        case 0xCB: {
            handle_cb();
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

        case 0xD0: {
            if (!FLAGS_IS_SET(FLAG_CARRY)) {
                registers.PC = popw();
                cpu.ticks += 20;
            } else {
                cpu.ticks += 8;
            }

            printf("RET NC\n");
            break;
        }

        case 0xD1: {
            registers.DE = popw();
            cpu.ticks += 12;

            printf("POP DE\n");
            break;
        }

        case 0xD5: {
            pushw(registers.DE);
            cpu.ticks += 16;

            printf("PUSH DE\n");
            break;
        }

        case 0xD6: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            op_sub8(operand);
            cpu.ticks += 8;

            printf("SUB A, 0x%02X\n", operand);
            break;
        }

        case 0xD8: {
            if (FLAGS_IS_SET(FLAG_CARRY)) {
                registers.PC = popw();
                cpu.ticks += 20;
            } else {
                cpu.ticks += 8;
            }

            printf("RET C\n");
            break;
        }

        case 0xD9: {
            registers.PC = popw();
            interrupts.master = 1;
            cpu.ticks += 16;

            printf("RETI\n");
            break;
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

        case 0xE1: {
            registers.HL = popw();
            cpu.ticks += 12;

            printf("POP HL\n");
            break;
        }

        case 0xE2: {
            storeb(registers.A, 0xFF00 + registers.C);
            cpu.ticks += 8;

            printf("LD (FF00+C), A\n");
            break;
        }

        case 0xE9: {
            registers.PC = registers.HL;
            cpu.ticks += 4;

            printf("JP HL\n");
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

        case 0xE6: {
            byte_t operand = loadb(registers.PC);
            registers.PC += 1;
            op_and(operand);
            cpu.ticks += 8;

            printf("AND A, 0x%02X\n", operand);
            break;
        }

        case 0xEF: {
            pushw(registers.PC);
            registers.PC = 0x0028;
            cpu.ticks += 16;

            printf("RST 28H\n");
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

        case 0xF1: {
            registers.AF = popw();
            cpu.ticks += 12;

            printf("POP AF\n");
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
            registers.PC += 2;
            registers.A = loadb(operand);
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
            interrupts.master = 1;
    }
}

void interrupt_step() {
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
