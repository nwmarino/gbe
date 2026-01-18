//
//  GBE, Nick Marino.
// 
//  MIT LICENSE
//

#include "common.h"
#include "cpu.h"

#include "GL/gl.h"
#include "GLFW/glfw3.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FLAG_Z (1 << 7)
#define FLAG_N (1 << 6)
#define FLAG_H (1 << 5)
#define FLAG_C (1 << 4)

#define FLAGS_IS_ZERO (registers.F & FLAG_Z)
#define FLAGS_IS_NEGATIVE (registers.F & FLAG_N)
#define FLAGS_IS_HALF_CARRY (registers.F & FLAG_H)
#define FLAGS_IS_CARRY (registers.F & FLAG_C)

#define FLAGS_IS_SET(f) (registers.F & (f))
#define FLAGS_SET(f) (registers.F |= (f))
#define FLAGS_CLEAR(f) (registers.F &= ~(f)) 

struct {
    word_t PC;
    word_t SP;

    struct {
        union {
            word_t AF;
            struct {
                byte_t A;
                byte_t F;
            };
        };
    };

    struct {
        union {
            word_t BC;
            struct {
                byte_t B;
                byte_t C;
            };
        };
    };

    struct {
        union {
            word_t DE;
            struct {
                byte_t E;
                byte_t D;
            };
        };
    };

    struct {
        union {
            word_t HL;
            struct {
                byte_t H;
                byte_t L;
            };
        };
    };
} registers;

byte_t cart[32768]; // 32kB cartridge [0x0000, 0x8000)
byte_t vram[8192]; // 8kB video RAM [0x8000, 0xA000)
byte_t sram[8192]; // 8kB switchable RAM [0xA000, 0xC000)
byte_t iram[8192]; // 8kB internal RAM [0xC000, 0xE000), echoed [0xE000, 0xFE00)
byte_t oram[256]; // 256 bytes sprite attrib RAM [0xFE00, 0xFEA0)
byte_t iop[128]; // 128 bytes for io ports [0xFF00, 0xFF4C) 
byte_t hram[128]; // 128 bytes internal RAM [0xFF80, 0xFFFF)

struct {
    uint64_t ticks;
    bool_t stopped;
} state = { .ticks = 0, .stopped = 0 };

struct inst_t {
    char* dsym;
    void* exec;
    byte_t operands;
};

static byte_t inc_b(byte_t data) {
    if ((data & 0x0F) == 0x0F) {
        FLAGS_IS_SET(FLAG_H);
    } else { 
        FLAGS_CLEAR(FLAG_H);
    }

    ++data;
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N);
	return data;
}

static byte_t dec_b(byte_t data) {
    if ((data & 0x0F)) {
        FLAGS_CLEAR(FLAG_H);
    } else {
        FLAGS_SET(FLAG_H);
    }

    --data;

    if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

    FLAGS_SET(FLAG_N);
    return data;
}

static void add_b(byte_t* dest, byte_t data) {
    uint32_t res = *dest + data;

    if (res & 0xFF00) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    *dest = (byte_t)(res & 0xFF);

    if (*dest) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

    if (((*dest & 0x0F) + (data & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    FLAGS_CLEAR(FLAG_N);
}

static void add_w(word_t* dest, word_t data) {
    uint64_t res = *dest + data;

    if (res & 0xFFFF0000) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    *dest = (word_t)(res & 0xFFFF);

    if (((*dest & 0x0F) + (data & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    FLAGS_CLEAR(FLAG_N);
}

static void adc(byte_t data) {
    if (FLAGS_IS_SET(FLAG_C)) {
        data += 1;
    }

    int32_t res = registers.A + data;

    if (res & 0xFF00) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    if (data == registers.A) {
        FLAGS_SET(FLAG_Z);
    } else {
        FLAGS_CLEAR(FLAG_Z);
    }

    if (((data & 0x0F) + (registers.A & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    FLAGS_SET(FLAG_N);

    registers.A = (byte_t)(res & 0xFF);
}

static void sbc(byte_t data) {
    if (FLAGS_IS_SET(FLAG_C)) {
        data += 1;
    }

    FLAGS_SET(FLAG_N);

    if (data > registers.A) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    if (data == registers.A) {
        FLAGS_SET(FLAG_Z);
    } else {
        FLAGS_CLEAR(FLAG_Z);
    }

    if ((data & 0x0F) > (registers.A & 0x0F)) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    registers.A -= data;
}

static void sub(byte_t data) {
    FLAGS_SET(FLAG_N);

    if (data > registers.A) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    if ((data & 0x0F) > (registers.A & 0x0F)) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    registers.A -= data;

    if (registers.A) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }
}

static void and(byte_t data) {
    registers.A &= data;

    if (registers.A) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

    FLAGS_CLEAR(FLAG_N | FLAG_C);
    FLAGS_SET(FLAG_H);
}

static void or(byte_t data) {
    registers.A |= data;

    if (registers.A) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

    FLAGS_CLEAR(FLAG_N | FLAG_C | FLAG_H);
}

static void xor(byte_t data) {
    registers.A ^= data;

    if (registers.A) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

    FLAGS_CLEAR(FLAG_N | FLAG_C | FLAG_H);
}

static void cp(byte_t data) {
    if (registers.A == data) {
        FLAGS_SET(FLAG_Z);
    } else {
        FLAGS_CLEAR(FLAG_Z);
    }

    if (data > registers.A) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    if ((data & 0x0F) > (registers.A & 0x0F)) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    FLAGS_SET(FLAG_N);
}

const struct inst_t insts[256] = {
    { "NOP", cmd_nop, 0 }, // 0x00
    { "LD BC, 0x%04X", cmd_ld_bc_nn, 2 }, // 0x01
    { "LD (BC), A", cmd_ld_bcp_a, 0 }, // 0x02
    { "INC BC", cmd_inc_bc, 0 }, // 0x03
    { "INC B", cmd_inc_b, 0 }, // 0x04
    { "DEC B", cmd_dec_b, 0 }, // 0x05
    { "LD B, 0x%02X", cmd_ld_b_n, 1 }, // 0x06
    { "RLCA", cmd_rlca, 0 }, // 0x07
    { "LD (0x%04X), SP", cmd_ld_nnp_sp, 2 }, // 0x08
    { "ADD HL, BC", cmd_add_hl_bc, 0 }, // 0x09
    { "LD A, (BC)", cmd_ld_a_bcp, 0 }, // 0x0A
    { "DEC BC", cmd_dec_bc, 0 }, // 0x0B
    { "INC C", cmd_inc_c, 0 }, // 0x0C
    { "DEC C", cmd_dec_c, 0 }, // 0x0D
    { "LD C, 0x%02X", cmd_ld_c_n, 1 }, // 0x0E
    { "RRCA", cmd_rrca, 0 }, // 0x0F
    { "STOP", cmd_stop, 1 }, // 0x10
    { "LD DE, 0x%04X", cmd_ld_de_nn, 2 }, // 0x11
    { "LD (DE), A", cmd_ld_dep_a, 0 }, // 0x12
    { "INC DE", cmd_inc_de, 0 }, // 0x13
    { "INC D", cmd_inc_d, 0 }, // 0x14
    { "DEC D", cmd_dec_d, 0 }, // 0x15
    { "LD D, 0x%02X", cmd_ld_d_n, 1 }, // 0x16
    { "RLA", cmd_rla, 0 }, // 0x17
    { "JR 0x%02X", cmd_jr_n, 1 }, // 0x18
    { "ADD HL, DE", cmd_add_hl_de, 0 }, // 0x19
    { "LD A, (DE)", cmd_ld_a_dep, 0 }, // 0x1A
    { "DEC DE", cmd_dec_de, 0 }, // 0x1B
    { "INC E", cmd_inc_e, 0 }, // 0x1C
    { "DEC E", cmd_dec_e, 0 }, // 0x1D
    { "LD E, 0x%02X", cmd_ld_e_n, 1 }, // 0x1E
    { "RRA", cmd_rra, 0 }, // 0x1F
    { "JR NZ, 0x%02X", cmd_jr_nz_n, 1 }, // 0x20
    { "LD HL, 0x%04X", cmd_ld_hl_nn, 2 }, // 0x21
    { "LDI (HL), A", cmd_ldi_hlp_a, 0 }, // 0x22
    { "INC HL", cmd_inc_hl, 0 }, // 0x23
    { "INC H", cmd_inc_h, 0 }, // 0x24
    { "DEC H", cmd_dec_h, 0 }, // 0x25
    { "LD H, 0x%02X", cmd_ld_h_n, 1 }, // 0x26
    { "DAA", cmd_daa, 0 }, // 0x27
    { "JR Z, 0x%02X", cmd_jr_z_n, 1 }, // 0x28
    { "ADD HL, HL", cmd_add_hl_hl, 0 }, // 0x29
    { "LDI A, (HL)", cmd_ldi_a_hlp, 0 }, // 0x2A
    { "DEC HL", cmd_dec_hl, 0 }, // 0x2B
    { "INC L", cmd_inc_l, 0 }, // 0x2C
    { "DEC L", cmd_dec_l, 0 }, // 0x2D
    { "LD L, 0x%02X", cmd_ld_l_n, 1 }, // 0x2E
    { "CPL", cmd_cpl, 0 }, // 0x2F
    { "JR NC, 0x%02X", cmd_jr_nc_n, 1 }, // 0x30
    { "LD SP, 0x%04X", cmd_ld_sp_nn, 2 }, // 0x31
    { "LDD (HL), A", cmd_ldd_hlp_a, 0 }, // 0x32
    { "INC SP", cmd_inc_sp, 0 }, // 0x33
    { "INC (HL)", cmd_inc_hlp, 0 }, // 0x34
    { "DEC (HL)", cmd_dec_hlp, 0 }, // 0x35
    { "LD (HL), 0x%02X", cmd_ld_hlp_n, 1 }, // 0x36
    { "SCF", cmd_scf, 0 }, // 0x37
    { "JR C, 0x%02X", cmd_jr_c_n, 1 }, // 0x38
    { "ADD HL, SP", cmd_add_hl_sp, 0 }, // 0x39
    { "LDD A, (HL)", cmd_ldd_a_hlp, 0 }, // 0x3A
    { "DEC SP", cmd_dec_sp, 0 }, // 0x3B
    { "INC A", cmd_inc_a, 0 }, // 0x3C
    { "DEC A", cmd_dec_a, 0 }, // 0x3D
    { "LD A, 0x%02X", cmd_ld_a_n, 1 }, // 0x3E
    { "CCF", cmd_ccf, 0 }, // 0x3F
    { "LD B, B", cmd_ld_b_b, 0 }, // 0x40
    { "LD B, C", cmd_ld_b_c, 0 }, // 0x41
    { "LD B, D", cmd_ld_b_d, 0 }, // 0x42
    { "LD B, E", cmd_ld_b_e, 0 }, // 0x43
    { "LD B, H", cmd_ld_b_h, 0 }, // 0x44
    { "LD B, L", cmd_ld_b_l, 0 }, // 0x45
    { "LD B, (HL)", cmd_ld_b_hlp, 0 }, // 0x46
    { "LD B, A", cmd_ld_b_a, 0 }, // 0x47
    { "LD C, B", cmd_ld_c_b, 0 }, // 0x48
    { "LD C, C", cmd_ld_c_c, 0 }, // 0x49
    { "LD C, D", cmd_ld_c_d, 0 }, // 0x4A
    { "LD C, E", cmd_ld_c_e, 0 }, // 0x4B
    { "LD C, H", cmd_ld_c_h, 0 }, // 0x4C
    { "LD C, L", cmd_ld_c_l, 0 }, // 0x4D
    { "LD C, (HL)", cmd_ld_c_hlp, 0 }, // 0x4E
    { "LD C, A", cmd_ld_c_a, 0 }, // 0x4F
    { "LD D, B", cmd_ld_d_b, 0 }, // 0x50
    { "LD D, C", cmd_ld_d_c, 0 }, // 0x51
    { "LD D, D", cmd_ld_d_d, 0 }, // 0x52
    { "LD D, E", cmd_ld_d_e, 0 }, // 0x53
    { "LD D, H", cmd_ld_d_h, 0 }, // 0x54
    { "LD D, L", cmd_ld_d_l, 0 }, // 0x55
    { "LD D, (HL)", cmd_ld_d_hlp, 0 }, // 0x56
    { "LD D, A", cmd_ld_d_a, 0 }, // 0x57
    { "LD E, B", cmd_ld_e_b, 0 }, // 0x58
    { "LD E, C", cmd_ld_e_c, 0 }, // 0x59
    { "LD E, D", cmd_ld_e_d, 0 }, // 0x5A
    { "LD E, E", cmd_ld_e_e, 0 }, // 0x5B
    { "LD E, H", cmd_ld_e_h, 0 }, // 0x5C
    { "LD E, L", cmd_ld_e_l, 0 }, // 0x5D
    { "LD E, (HL)", cmd_ld_e_hlp, 0 }, // 0x5E
    { "LD E, A", cmd_ld_e_a, 0 }, // 0x5F
    { "LD H, B", cmd_ld_h_b, 0 }, // 0x60
    { "LD H, C", cmd_ld_h_c, 0 }, // 0x61
    { "LD H, D", cmd_ld_h_d, 0 }, // 0x62
    { "LD H, E", cmd_ld_h_e, 0 }, // 0x63
    { "LD H, H", cmd_ld_h_h, 0 }, // 0x64
    { "LD H, L", cmd_ld_h_l, 0 }, // 0x65
    { "LD H, (HL)", cmd_ld_h_hlp, 0 }, // 0x66
    { "LD H, A", cmd_ld_h_a, 0 }, // 0x67
    { "LD L, B", cmd_ld_l_b, 0 }, // 0x68
    { "LD L, C", cmd_ld_l_c, 0 }, // 0x69
    { "LD L, D", cmd_ld_l_d, 0 }, // 0x6A
    { "LD L, E", cmd_ld_l_e, 0 }, // 0x6B
    { "LD L, H", cmd_ld_l_h, 0 }, // 0x6C
    { "LD L, L", cmd_ld_l_l, 0 }, // 0x6D
    { "LD L, (HL)", cmd_ld_l_hlp, 0 }, // 0x6E
    { "LD L, A", cmd_ld_l_a, 0 }, // 0x6F
    { "LD (HL), B", cmd_ld_hlp_b, 0 }, // 0x70
    { "LD (HL), C", cmd_ld_hlp_c, 0 }, // 0x71
    { "LD (HL), D", cmd_ld_hlp_d, 0 }, // 0x72
    { "LD (HL), E", cmd_ld_hlp_e, 0 }, // 0x73
    { "LD (HL), H", cmd_ld_hlp_h, 0 }, // 0x74
    { "LD (HL), L", cmd_ld_hlp_l, 0 }, // 0x75
    { "HALT", cmd_halt, 0 }, // 0x76
    { "LD (HL), A", cmd_ld_hlp_a, 0 }, // 0x77
    { "LD A, B", cmd_ld_a_b, 0 }, // 0x78
    { "LD A, C", cmd_ld_a_c, 0 }, // 0x79
    { "LD A, D", cmd_ld_a_d, 0 }, // 0x7A
    { "LD A, E", cmd_ld_a_e, 0 }, // 0x7B
    { "LD A, H", cmd_ld_a_h, 0 }, // 0x7C
    { "LD A, L", cmd_ld_a_l, 0 }, // 0x7D
    { "LD A, (HL)", cmd_ld_a_hlp, 0 }, // 0x7E
    { "LD A, A", cmd_ld_a_a, 0 }, // 0x7F
    { "ADD A, B", cmd_add_a_b, 0 }, // 0x80
    { "ADD A, C", cmd_add_a_c, 0 }, // 0x81
    { "ADD A, D", cmd_add_a_d, 0 }, // 0x82
    { "ADD A, E", cmd_add_a_e, 0 }, // 0x83
    { "ADD A, H", cmd_add_a_h, 0 }, // 0x84
    { "ADD A, L", cmd_add_a_l, 0 }, // 0x85
    { "ADD A, (HL)", cmd_add_a_hlp, 0 }, // 0x86
    { "ADD A", cmd_add_a_a, 0 }, // 0x87
    { "ADC B", cmd_adc_b, 0 }, // 0x88
    { "ADC C", cmd_adc_c, 0 }, // 0x89
    { "ADC D", cmd_adc_d, 0 }, // 0x8A
    { "ADC E", cmd_adc_e, 0 }, // 0x8B
    { "ADC H", cmd_adc_h, 0 }, // 0x8C
    { "ADC L", cmd_adc_l, 0 }, // 0x8D
    { "ADC (HL)", cmd_adc_hlp, 0 }, // 0x8E
    { "ADC A", cmd_adc_a, 0 }, // 0x8F
    { "SUB B", cmd_sub_b, 0 }, // 0x90
    { "SUB C", cmd_sub_c, 0 }, // 0x91
    { "SUB D", cmd_sub_d, 0 }, // 0x92
    { "SUB E", cmd_sub_e, 0 }, // 0x93
    { "SUB H", cmd_sub_h, 0 }, // 0x94
    { "SUB L", cmd_sub_l, 0 }, // 0x95
    { "SUB (HL)", cmd_sub_hlp, 0 }, // 0x96
    { "SUB A", cmd_sub_a, 0 }, // 0x97
    { "SBC B", cmd_sbc_b, 0 }, // 0x98
    { "SBC C", cmd_sbc_c, 0 }, // 0x99
    { "SBC D", cmd_sbc_d, 0 }, // 0x9A
    { "SBC E", cmd_sbc_e, 0 }, // 0x9B
    { "SBC H", cmd_sbc_h, 0 }, // 0x9C
    { "SBC L", cmd_sbc_l, 0 }, // 0x9D
    { "SBC (HL)", cmd_sbc_hlp, 0 }, // 0x9E
    { "SBC A", cmd_sbc_a, 0 }, // 0x9F
    { "AND B", cmd_and_b, 0 }, // 0xA0
    { "AND C", cmd_and_c, 0 }, // 0xA1
    { "AND D", cmd_and_d, 0 }, // 0xA2
    { "AND E", cmd_and_e, 0 }, // 0xA3
    { "AND H", cmd_and_h, 0 }, // 0xA4
    { "AND L", cmd_and_l, 0 }, // 0xA5
    { "AND (HL)", cmd_and_hlp, 0 }, // 0xA6
    { "AND A", cmd_and_a, 0 }, // 0xA7
    { "XOR B", cmd_xor_b, 0 }, // 0xA8
    { "XOR C", cmd_xor_c, 0 }, // 0xA9
    { "XOR D", cmd_xor_d, 0 }, // 0xAA
    { "XOR E", cmd_xor_e, 0 }, // 0xAB
    { "XOR H", cmd_xor_h, 0 }, // 0xAC
    { "XOR L", cmd_xor_l, 0 }, // 0xAD
    { "XOR (HL)", cmd_or_hlp, 0 }, // 0xAE
    { "XOR A", cmd_or_a, 0 }, // 0xAF
    { "OR B", cmd_or_b, 0 }, // 0xB0
    { "OR C", cmd_or_c, 0 }, // 0xB1
    { "OR D", cmd_or_d, 0 }, // 0xB2
    { "OR E", cmd_or_e, 0 }, // 0xB3
    { "OR H", cmd_or_h, 0 }, // 0xB4
    { "OR L", cmd_or_l, 0 }, // 0xB5
    { "OR (HL)", cmd_or_hlp, 0 }, // 0xB6
    { "OR A", cmd_or_a, 0 }, // 0xB7
    { "CP B", cmd_cp_b, 0 }, // 0xB8
    { "CP C", cmd_cp_c, 0 }, // 0xB9
    { "CP D", cmd_cp_d, 0 }, // 0xBA
    { "CP E", cmd_cp_e, 0 }, // 0xBB
    { "CP H", cmd_cp_h, 0 }, // 0xBC
    { "CP L", cmd_cp_l, 0 }, // 0xBD
    { "CP (HL)", cmd_cp_hlp, 0 }, // 0xBE
    { "CP A", cmd_cp_a, 0 }, // 0xBF
    { "RET NZ", cmd_ret_nz, 0 }, // 0xC0
    { "POP BC", cmd_pop_bc, 0 }, // 0xC1
    { "JP NZ, 0x%04X", cmd_jp_nz_nn, 2 }, // 0xC2
    { "JP 0x%04X", cmd_jp_nn, 2 }, // 0xC3
    { "CALL NZ, 0x%04X", cmd_call_nz_nn, 2 }, // 0xC4
    { "PUSH BC", cmd_push_bc, 0 }, // 0xC5
    { "ADD A, 0x%02X", cmd_add_a_n, 1 }, // 0xC6
    { "RST 0x00", cmd_rst_0, 0 }, // 0xC7
    { "RET Z", cmd_ret_z, 0 }, // 0xC8
    { "RET", cmd_ret, 0 }, // 0xC9
    { "JP Z, 0x%04X", cmd_jp_z_nn, 2 }, // 0xCA
    { "CB %02X", null, 1 }, // 0xCB
    { "CALL Z, 0x%04X", cmd_call_z_nn, 2 }, // 0xCC
    { "CALL 0x%04X", cmd_call_nn, 2 }, // 0xCD
    { "ADC 0x%02X", cmd_adc_n, 1 }, // 0xCE
    { "RST 0x08", cmd_rst_08, 0 }, // 0xCF
    { "RET NC", cmd_ret_nc, 0 }, // 0xD0
    { "POP DE", cmd_pop_de, 0 }, // 0xD1
    { "JP NC, 0x%04X", cmd_jp_nc_nn, 2 }, // 0xD2
    { "UNKNOWN", cmd_undefined, 0 }, // 0xD3
    { "CALL NC, 0x%04X", cmd_call_nc_nn, 2 }, // 0xD4
    { "PUSH DE", cmd_push_de, 0 }, // 0xD5
    { "SUB 0x%02X", cmd_sub_n, 1 }, // 0xD6
    { "RST 0x10", cmd_rst_10, 0 }, // 0xD7
    { "RET C", cmd_ret_c, 0 }, // 0xD8
    { "RETI", null, 0 }, // 0xD9
    { "JP C, 0x%04X", cmd_jp_c_nn, 2 }, // 0xDA
    { "UNKNOWN", cmd_undefined, 0 }, // 0xDB
    { "CALL C, 0x%04X", cmd_call_c_nn, 2 }, // 0xDC
    { "UNKNOWN", cmd_undefined, 0 }, // 0xDD
    { "SBC 0x%02X", cmd_sbc_n, 1 }, // 0xDE
    { "RST 0x18", cmd_rst_18, 0 }, // 0xDF
    { "LD (0xFF00 + 0x%02X), A", cmd_ld_ff_n_ap, 1 }, // 0xE0
    { "POP HL", cmd_pop_hl, 0 }, // 0xE1
    { "LD (0xFF00 + C), A", cmd_ld_ff_c_a, 0 }, // 0xE2
    { "UNKNOWN", cmd_undefined, 0 }, // 0xE3
    { "UNKNOWN", cmd_undefined, 0 }, // 0xE4
    { "PUSH HL", cmd_push_hl, 0 }, // 0xE5
    { "AND 0x%02X", cmd_and_n, 1 }, // 0xE6
    { "RST 0x20", cmd_rst_20, 0 }, // 0xE7
    { "ADD SP,0x%02X", cmd_add_sp_n, 1 }, // 0xE8
    { "JP HL", cmd_jp_hl, 0 }, // 0xE9
    { "LD (0x%04X), A", cmd_ld_nnp_a, 2 }, // 0xEA
    { "UNKNOWN", cmd_undefined, 0 }, // 0xEB
    { "UNKNOWN", cmd_undefined, 0 }, // 0xEC
    { "UNKNOWN", cmd_undefined, 0 }, // 0xED
    { "XOR 0x%02X", cmd_or_n, 1 }, // 0xEE
    { "RST 0x28", cmd_rst_28, 0 }, // 0xEF
    { "LD A, (0xFF00 + 0x%02X)", cmd_ld_ff_ap_n, 1 }, // 0xF0
    { "POP AF", cmd_pop_af, 0 }, // 0xF1
    { "LD A, (0xFF00 + C)", cmd_ld_a_ff_c, 0 }, // 0xF2
    { "DI", cmd_di_inst, 0 }, // 0xF3
    { "UNKNOWN", cmd_undefined, 0 }, // 0xF4
    { "PUSH AF", cmd_push_af, 0 }, // 0xF5
    { "OR 0x%02X", cmd_or_n, 1 }, // 0xF6
    { "RST 0x30", cmd_rst_30, 0 }, // 0xF7
    { "LD HL, SP+0x%02X", cmd_ld_hl_sp_n, 1 }, // 0xF8
    { "LD SP, HL", cmd_ld_sp_hl, 0 }, // 0xF9
    { "LD A, (0x%04X)", cmd_ld_a_nnp, 2 }, // 0xFA
    { "EI", cmd_ei, 0 }, // 0xFB
    { "UNKNOWN", cmd_undefined, 0 }, // 0xFC
    { "UNKNOWN", cmd_undefined, 0 }, // 0xFD
    { "CP 0x%02X", cmd_cp_n, 1 }, // 0xFE
    { "RST 0x38", cmd_rst_38, 0 }, // 0xFF
};

byte_t load_b(word_t address);
word_t load_w(word_t address);
void store_b(byte_t data, word_t address);
void store_w(word_t data, word_t address);

void copy(word_t dest, word_t source, word_t n) {
    for (word_t i = 0; i < n; ++i)
        store_b(dest + i, load_b(source + i));
}

byte_t load_b(word_t address) {
    if (address < 0x8000) {
        return cart[address];
    } else if (address >= 0xA000 && address < 0xC000) {
        return sram[address - 0xA000];
    } else if (address >= 0xC000 && address < 0xE000) {
        return iram[address - 0xC000];
    } else if (address >= 0xE000 && address < 0xFE00) {
        return iram[address - 0xE000]; // echoed internal memory
    } else if (address >= 0xFE00 && address < 0xFF00) {
        return oram[address - 0xFE00];
    } else if (address >= 0xFF80 && address < 0xFFFF) {
        return hram[address - 0xFF80];
    }

    fprintf(stderr, "invalid load address: 0x%d\n", address);
    return 0;
}

word_t load_w(word_t address) {
    // Concatenate the byte at |address| with the byte after.
    return load_b(address) | (load_b(address + 1) << 8);
}

void store_b(byte_t data, word_t address) {
    if (address >= 0xA000 && address < 0xC000) {
        sram[address - 0xA000] = data;
    } else if (address >= 0x8000 && address < 0xA000) {
        vram[address - 0x8000] = data;
    } else if (address >= 0xC000 && address < 0xE000) {
        iram[address - 0xC000] = data;
    } else if (address >= 0xE000 && address < 0xFE00) {
        iram[address - 0xE000] = data; // echoed internal memory
    } else if (address >= 0xFE00 && address < 0xFF00) {
        oram[address - 0xFE00] = data;
    } else if (address >= 0xFF00 && address < 0xFF4C) {
        iop[address - 0xFF00] = data;
    } else if (address >= 0xFF80 && address < 0xFFFF) {
        hram[address - 0xFF80] = data;
    } else {
        fprintf(stderr, "invalid store address: 0x%d\n", address);
    }
}

void store_w(word_t data, word_t address) {
    store_b(address, (byte_t) (data & 0x00FF));
    store_b(address + 1, (byte_t) ((data & 0xFF00) >> 8));
}

void load_rom(byte_t* mem, const char* path) {
    memset(mem, 0, 32768);
    
    FILE* rom = fopen(path, "rb");
    if (!rom) {
        fprintf(stderr, "failed to open rom file: %s\n", path);
        exit(1);
    }

    fread(mem, 1, 32768, rom);
    fclose(rom);
}

word_t pop_w() {
    word_t data = load_w(registers.SP);
    registers.SP += 2;

    printf("stack read 0x%04x\n", data);
    return data;
}

void push_w(word_t data) {
    registers.SP -= 2;
    store_w(data, registers.SP);

    printf("stack write 0x%04x\n", data);
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

    store_b(0x00, 0xFF05);
    store_b(0x00, 0xFF06);
    store_b(0x00, 0xFF07);
    store_b(0x80, 0xFF10);
    store_b(0xBF, 0xFF11);
    store_b(0xF3, 0xFF12);
    store_b(0xBF, 0xFF14);
    store_b(0x3F, 0xFF16);
    store_b(0x00, 0xFF17);
    store_b(0xBF, 0xFF19);
    store_b(0x7F, 0xFF1A);
    store_b(0xFF, 0xFF1B);
    store_b(0x9F, 0xFF1C);
    store_b(0xBF, 0xFF1E);
    store_b(0xFF, 0xFF20);
    store_b(0x00, 0xFF21);
    store_b(0x00, 0xFF22);
    store_b(0xBF, 0xFF23);
    store_b(0x77, 0xFF24);
    store_b(0xF3, 0xFF25);
    store_b(0xF1, 0xFF26);
    store_b(0x91, 0xFF40);
    store_b(0x00, 0xFF42);
    store_b(0x00, 0xFF43);
    store_b(0x00, 0xFF45);
    store_b(0xFC, 0xFF47);
    store_b(0xFF, 0xFF48);
    store_b(0xFF, 0xFF49);
    store_b(0x00, 0xFF4A);
    store_b(0x00, 0xFF4B);
    store_b(0x00, 0xFFFF);
}

void cpu_step() {
    byte_t opcode = load_b(registers.PC++);
    word_t operands = 0;

    if (insts[opcode].operands == 1)
        operands = (word_t) load_b(registers.PC);

    if (insts[opcode].operands == 2)
        operands = load_w(registers.PC);

    registers.PC += insts[opcode].operands;

    switch (insts[opcode].operands) {
        case 0:
            printf("%s\n", insts[opcode].dsym);
            ((void (*)()) insts[opcode].exec)();
            break;
        case 1:
            printf(insts[opcode].dsym, operands);
            printf("\n");
            ((void (*)(byte_t)) insts[opcode].exec)((byte_t) operands);
            break;
        case 2:
            printf(insts[opcode].dsym, operands);
            printf("\n");
            ((void (*)(word_t)) insts[opcode].exec)(operands);
            break;
    }
}

void cmd_undefined() {
    --registers.PC;

    byte_t opcode = load_b(registers.PC);

    printf("undefined opcode 0x%02x\n", opcode);

    exit(1);
}

void cmd_nop() { // 0x00
    return;
}

void cmd_ld_bc_nn(byte_t operand) { // 0x01
    registers.BC = operand;
}

void cmd_ld_bcp_a() { // 0x02
    store_b(registers.A, registers.BC);
}

void cmd_inc_bc() { // 0x03
    ++registers.BC;
}

void cmd_inc_b() { // 0x04
    registers.B = inc_b(registers.B);
}

void cmd_dec_b() { // 0x05
    registers.B = dec_b(registers.B);
}

void cmd_ld_b_n(byte_t operand) { // 0x06
    registers.B = operand;
}

void cmd_rlca() { // 0x07
    byte_t carry = (registers.A & 0x80) >> 7;

    if (carry) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    registers.A <<= 1;
    registers.A += carry;

    FLAGS_CLEAR(FLAG_N | FLAG_Z | FLAG_H);
}

void cmd_ld_nnp_sp(word_t operand) { // 0x08
    store_w(operand, registers.SP);
}

void cmd_add_hl_bc() { // 0x09
    add_w(&registers.HL, registers.BC);
}

void cmd_ld_a_bcp() { // 0x0A
    registers.A = load_b(registers.BC);
}

void cmd_dec_bc() { // 0x0B
    --registers.BC;
}

void cmd_inc_c() { // 0x0C
    registers.C = inc_b(registers.C);
}

void cmd_dec_c() { // 0x0D
    registers.C = dec_b(registers.C);
}

void cmd_ld_c_n(byte_t operand) { // 0x0E
    registers.C = operand;
}

void cmd_rrca() { // 0x0F
    byte_t carry = registers.A & 0x01;
    if (carry) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    registers.A >>= 1;
    if (carry) {
        registers.A |= 0x80;
    }

    FLAGS_CLEAR(FLAG_N | FLAG_Z | FLAG_H);
}

void cmd_stop(byte_t operand) { // 0x10
    state.stopped = 1;
}

void cmd_ld_de_nn(word_t operand) { // 0x11
    registers.DE = operand;
}

void cmd_ld_dep_a() { // 0x12
    store_b(registers.A, registers.DE);
}

void cmd_inc_de() { // 0x13
    ++registers.DE;
}

void cmd_inc_d() { // 0x14
    registers.D = inc_b(registers.D);
}

void cmd_dec_d() { // 0x15
    registers.D = dec_b(registers.D);
}

void cmd_ld_d_n(byte_t operand) { // 0x16
    registers.D = operand;
}

void cmd_rla() { // 0x17
    int32_t carry = FLAGS_IS_SET(FLAG_C);

    if (registers.A & 0x80) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    registers.A <<= 1;
    registers.A += carry;

    FLAGS_CLEAR(FLAG_N | FLAG_Z | FLAG_H);
}

void cmd_jr_n(byte_t operand) { // 0x18
    registers.PC += operand;
}

void cmd_add_hl_de() { // 0x19
    add_w(&registers.HL, registers.DE);
}

void cmd_ld_a_dep() { // 0x1A
    registers.A = load_b(registers.DE);
}

void cmd_dec_de() { // 0x1B
    --registers.DE;
}

void cmd_inc_e() { // 0x1C
    registers.E = inc_b(registers.E);
}

void cmd_dec_e() { // 0x1D
    registers.E = dec_b(registers.E);
}

void cmd_ld_e_n(byte_t operand) { // 0x1E
    registers.E = operand;
}

void cmd_rra() { // 0x1F
    int32_t carry = FLAGS_IS_SET(FLAG_C);
    carry <<= 7;

    if (registers.A & 0x01) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    registers.A >>= 1;
    registers.A += carry;

    FLAGS_CLEAR(FLAG_N | FLAG_Z | FLAG_H);
}

void cmd_jr_nz_n(byte_t operand) { // 0x20
    if (FLAGS_IS_SET(FLAG_Z)) {
        state.ticks += 8;
    } else {
        registers.PC += operand;
        state.ticks += 12;
    }
}

void cmd_ld_hl_nn(word_t operand) { // 0x21
    registers.HL = operand;
}

void cmd_ldi_hlp_a() { // 0x22
    store_b(registers.A, registers.HL++);
}

void cmd_inc_hl() { // 0x23
    ++registers.HL;
}

void cmd_inc_h() { // 0x24
    registers.H = inc_b(registers.H);
}

void cmd_dec_h() { // 0x25
    registers.H = dec_b(registers.H);
}

void cmd_ld_h_n(byte_t operand) { // 0x26
    registers.H = operand;
}

void cmd_daa() { // 0x27
    cmd_undefined();
}

void cmd_jr_z_n(byte_t operand) { // 0x28
    if (FLAGS_IS_SET(FLAG_Z)) {
        registers.PC += operand;
        state.ticks += 12;
    } else {
        state.ticks += 8;
    }
}

void cmd_add_hl_hl() { // 0x29
    add_w(&registers.HL, registers.HL);
}

void cmd_ldi_a_hlp() { // 0x2A
    registers.A = load_b(registers.HL++);
}

void cmd_dec_hl() { // 0x2B
    --registers.HL;
}

void cmd_inc_l() { // 0x2C
    registers.L = inc_b(registers.L);
}

void cmd_dec_l() { // 0x2D
    registers.L = dec_b(registers.L);
}

void cmd_ld_l_n(byte_t operand) { // 0x2E
    registers.L = operand;
}

void cmd_cpl() { // 0x2F
    registers.A = ~registers.A;
    FLAGS_SET(FLAG_N | FLAG_H);
}

void cmd_jr_nc_n(byte_t operand) { // 0x30
    if (FLAGS_IS_SET(FLAG_C)) {
        state.ticks += 8;
    } else {
        registers.PC += operand;
        state.ticks += 12;
    }
}

void cmd_ld_sp_nn(word_t operand) { // 0x31
    registers.SP = operand;
}

void cmd_ldd_hlp_a() { // 0x32
    store_b(registers.A, registers.HL--);
}

void cmd_inc_sp() { // 0x33
    ++registers.SP;
}

void cmd_inc_hlp() { // 0x34
    store_b(inc_b(load_b(registers.HL)), registers.HL);
}

void cmd_dec_hlp() { // 0x35
    store_b(dec_b(load_b(registers.HL)), registers.HL);
}

void cmd_ld_hlp_n(byte_t operand) { // 0x36
    store_b(operand, registers.HL);
}

void cmd_scf() { // 0x37
    FLAGS_SET(FLAG_C);
    FLAGS_CLEAR(FLAG_N | FLAG_C);
}

void cmd_jr_c_n(byte_t operand) { // 0x38
    if (FLAGS_IS_SET(FLAG_C)) {
        registers.PC += operand;
        state.ticks += 12;
    } else {
        state.ticks += 8;
    }
}

void cmd_add_hl_sp() { // 0x39
    add_w(&registers.HL, registers.SP);
}

void cmd_ldd_a_hlp() { // 0x3A
    registers.A = load_b(registers.HL--);
}

void cmd_dec_sp() { // 0x3B
    --registers.SP;
}

void cmd_inc_a() { // 0x3C
    registers.A = inc_b(registers.A);
}

void cmd_dec_a() { // 0x3D
    registers.A = dec_b(registers.A);
}

void cmd_ld_a_n(byte_t operand) { // 0x3E
    registers.A = operand;
}

void cmd_ccf() { // 0x3F
    if (FLAGS_IS_SET(FLAG_C)) {
        FLAGS_CLEAR(FLAG_C);
    } else {
        FLAGS_SET(FLAG_C);
    }

    FLAGS_CLEAR(FLAG_N | FLAG_H);
}

void cmd_ld_b_b() { // 0x40
    return; // Nothing to do.
}

void cmd_ld_b_c() { // 0x41
    registers.B = registers.C;
}

void cmd_ld_b_d() { // 0x42
    registers.B = registers.D;
}

void cmd_ld_b_e() { // 0x43
    registers.B = registers.E;
}

void cmd_ld_b_h() { // 0x44
    registers.B = registers.H;
}

void cmd_ld_b_l() { // 0x45
    registers.B = registers.L;
}

void cmd_ld_b_hlp() { // 0x46
    registers.B = load_b(registers.HL);
}

void cmd_ld_b_a() { // 0x47
    registers.B = registers.A;
}

void cmd_ld_c_b() { // 0x48
    registers.C = registers.B;
}

void cmd_ld_c_c() { // 0x49
    return; // Nothing to do.
}

void cmd_ld_c_d() { // 0x4A
    registers.C = registers.D;
}

void cmd_ld_c_e() { // 0x4B
    registers.C = registers.E;
}

void cmd_ld_c_h() { // 0x4C
    registers.C = registers.H;
}

void cmd_ld_c_l() { // 0x4D
    registers.C = registers.L;
}

void cmd_ld_c_hlp() { // 0x4E
    registers.C = load_b(registers.HL);
}

void cmd_ld_c_a() { // 0x4F
    registers.C = registers.A;
}

void cmd_ld_d_b() { // 0x50
    registers.D = registers.B;
}

void cmd_ld_d_c() { // 0x51
    registers.D = registers.C;
}

void cmd_ld_d_d() { // 0x52
    return; // Nothing to do.
}

void cmd_ld_d_e() { // 0x53
    registers.D = registers.E;
}

void cmd_ld_d_h() { // 0x54
    registers.D = registers.H;
}

void cmd_ld_d_l() { // 0x55
    registers.D = registers.L;
}

void cmd_ld_d_hlp() { // 0x56
    registers.D = load_b(registers.HL);
}

void cmd_ld_d_a() { // 0x57
    registers.D = registers.A;
}

void cmd_ld_e_b() { // 0x58
    registers.E = registers.B;
}

void cmd_ld_e_c() { // 0x59
    registers.E = registers.C;
}

void cmd_ld_e_d() { // 0x5A
    registers.E = registers.D;
}

void cmd_ld_e_e() { // 0x5B
    return; // Nothing to do.
}

void cmd_ld_e_h() { // 0x5C
    registers.E = registers.H;
}

void cmd_ld_e_l() { // 0x5D
    registers.E = registers.L;
}

void cmd_ld_e_hlp() { // 0x5E
    registers.E = load_b(registers.HL);
}

void cmd_ld_e_a() { // 0x5F
    registers.E = registers.A;
}

void cmd_ld_h_b() { // 0x60
    registers.H = registers.B;
}

void cmd_ld_h_c() { // 0x61
    registers.H = registers.C;
}

void cmd_ld_h_d() { // 0x62
    registers.H = registers.D;
}

void cmd_ld_h_e() { // 0x63
    registers.H = registers.E;
}

void cmd_ld_h_h() { // 0x64
    return; // Nothing to do.
}

void cmd_ld_h_l() { // 0x65
    registers.H = registers.L;
}

void cmd_ld_h_hlp() { // 0x66
    registers.H = load_b(registers.HL);
}

void cmd_ld_h_a() { // 0x67
    registers.H = registers.A;
}

void cmd_ld_l_b() { // 0x68
    registers.L = registers.B;
}

void cmd_ld_l_c() { // 0x69
    registers.L = registers.C;
}

void cmd_ld_l_d() { // 0x6A
    registers.L = registers.D;
}

void cmd_ld_l_e() { // 0x6B
    registers.L = registers.E;
}

void cmd_ld_l_h() { // 0x6C
    registers.L = registers.H;
}

void cmd_ld_l_l() { // 0x6D
    return; // Nothing to do.
}

void cmd_ld_l_hlp() { // 0x6E
    registers.L = load_b(registers.HL);
}

void cmd_ld_l_a() { // 0x6F
    registers.L = registers.A;
}

void cmd_ld_hlp_b() { // 0x70
    store_b(registers.B, registers.HL);
}

void cmd_ld_hlp_c() { // 0x71
    store_b(registers.C, registers.HL);
}

void cmd_ld_hlp_d() { // 0x72
    store_b(registers.D, registers.HL);
}

void cmd_ld_hlp_e() { // 0x73
    store_b(registers.E, registers.HL);
}

void cmd_ld_hlp_h() { // 0x74
    store_b(registers.H, registers.HL);
}

void cmd_ld_hlp_l() { // 0x75
    store_b(registers.L, registers.HL);
}

void cmd_halt() { // 0x76
    cmd_undefined();
    // if master interrupt, halt execution until an interrupt occurs.
    ++registers.PC;
}

void cmd_ld_hlp_a() { // 0x77
    store_b(registers.A, registers.HL);
}

void cmd_ld_a_b() { // 0x78
    registers.A = registers.B;
}

void cmd_ld_a_c() { // 0x79
    registers.A = registers.C;
}

void cmd_ld_a_d() { // 0x7A
    registers.A = registers.D;
}

void cmd_ld_a_e() { // 0x7B
    registers.A = registers.E;
}

void cmd_ld_a_h() { // 0x7C
    registers.A = registers.H;
}

void cmd_ld_a_l() { // 0x7D
    registers.A = registers.L;
}

void cmd_ld_a_hlp() { // 0x7E
    registers.A = load_b(registers.HL);
}

void cmd_ld_a_a() { // 0x7F
    return; // Nothing to do.
}

void cmd_add_a_b() { // 0x80
    add_b(&registers.A, registers.B);
}

void cmd_add_a_c() { // 0x81
    add_b(&registers.A, registers.C);
}

void cmd_add_a_d() { // 0x82
    add_b(&registers.A, registers.D);
}

void cmd_add_a_e() { // 0x83
    add_b(&registers.A, registers.E);
}

void cmd_add_a_h() { // 0x84
    add_b(&registers.A, registers.H);
}

void cmd_add_a_l() { // 0x85
    add_b(&registers.A, registers.L);
}

void cmd_add_a_hlp() { // 0x86
    registers.A = load_b(registers.HL);
}

void cmd_add_a_a() { // 0x87
    return; // Nothing to do.
}

void cmd_adc_b() { // 0x88
    adc(registers.B);
}

void cmd_adc_c() { // 0x89
    adc(registers.C);
}

void cmd_adc_d() { // 0x8A
    adc(registers.D);
}

void cmd_adc_e() { // 0x8B
    adc(registers.E);
}

void cmd_adc_h() { // 0x8C
    adc(registers.H);
}

void cmd_adc_l() { // 0x8D
    adc(registers.L);
}

void cmd_adc_hlp() { // 0x8E
    adc(load_b(registers.HL));
}

void cmd_adc_a() { // 0x8F
    adc(registers.A);
}

void cmd_sub_b() { // 0x90
    sub(registers.B);
}

void cmd_sub_c() { // 0x91
    sub(registers.C);
}

void cmd_sub_d() { // 0x92
    sub(registers.D);
}

void cmd_sub_e() { // 0x93
    sub(registers.E);
}

void cmd_sub_h() { // 0x94
    sub(registers.H);
}

void cmd_sub_l() { // 0x95
    sub(registers.L);
}

void cmd_sub_hlp() { // 0x96
    sub(load_b(registers.HL));
}

void cmd_sub_a() { // 0x97
    sub(registers.A);
}

void cmd_sbc_b() { // 0x98
    sbc(registers.B);
}

void cmd_sbc_c() { // 0x99
    sbc(registers.C);
}

void cmd_sbc_d() { // 0x9A
    sbc(registers.D);
}

void cmd_sbc_e() { // 0x9B
    sbc(registers.E);
}

void cmd_sbc_h() { // 0x9C
    sbc(registers.H);
}

void cmd_sbc_l() { // 0x9D
    sbc(registers.L);
}

void cmd_sbc_hlp() { // 0x9E
    sbc(load_b(registers.HL));
}

void cmd_sbc_a() { // 0x9F
    sbc(registers.A);
}

void cmd_and_b() { // 0xA0
    and(registers.B);
}

void cmd_and_c() { // 0xA1
    and(registers.C);
}

void cmd_and_d() { // 0xA2
    and(registers.D);
}

void cmd_and_e() { // 0xA3
    and(registers.E);
}

void cmd_and_h() { // 0xA4
    and(registers.H);
}

void cmd_and_l() { // 0xA5
    and(registers.L);
}

void cmd_and_hlp() { // 0xA6
    and(load_b(registers.HL));
}

void cmd_and_a() { // 0xA7
    and(registers.A);
}

void cmd_xor_b() { // 0xA8
    xor(registers.B);
}

void cmd_xor_c() { // 0xA9
    xor(registers.C);
}

void cmd_xor_d() { // 0xAA
    xor(registers.D);
}

void cmd_xor_e() { // 0xAB
    xor(registers.E);
}

void cmd_xor_h() { // 0xAC
    xor(registers.H);
}

void cmd_xor_l() { // 0xAD
    xor(registers.L);
}

void cmd_xor_hlp() { // 0xAE
    xor(load_b(registers.HL));
}

void cmd_xor_a() { // 0xAF
    xor(registers.A);
}

void cmd_or_b() { // 0xB0
    or(registers.B);
}

void cmd_or_c() { // 0xB1
    or(registers.C);
}

void cmd_or_d() { // 0xB2
    or(registers.D);
}

void cmd_or_e() { // 0xB3
    or(registers.E);
}

void cmd_or_h() { // 0xB4
    or(registers.H);
}

void cmd_or_l() { // 0xB5
    or(registers.L);
}

void cmd_or_hlp() { // 0xB6
    or(load_b(registers.HL));
}

void cmd_or_a() { // 0xB7
    or(registers.A);
}

void cmd_cp_b() { // 0xB8
    cp(registers.B);
}

void cmd_cp_c() { // 0xB9
    cp(registers.C);
}

void cmd_cp_d() { // 0xBA
    cp(registers.D);
}

void cmd_cp_e() { // 0xBB
    cp(registers.E);
}

void cmd_cp_h() { // 0xBC
    cp(registers.H);
}

void cmd_cp_l() { // 0xBD
    cp(registers.L);
}

void cmd_cp_hlp() { // 0xBE
    cp(load_b(registers.HL));
}

void cmd_cp_a() { // 0xBF
    cp(registers.A);
}

void cmd_ret_nz() { // 0xC0
    if (FLAGS_IS_SET(FLAG_Z)) {
        state.ticks += 8;
    } else {
        registers.PC = pop_w();
        state.ticks += 20;
    }
}

void cmd_pop_bc() { // 0xC1
    registers.BC = pop_w();
}

void cmd_jp_nz_nn(word_t operand) { // 0xC2
    if (FLAGS_IS_SET(FLAG_Z)) {
        state.ticks += 12;
    } else {
        registers.PC = operand;
        state.ticks += 16;
    }
}

void cmd_jp_nn(word_t operand) { // 0xC3
    registers.PC = operand;
}

void cmd_call_nz_nn(word_t operand) { // 0xC4
    if (FLAGS_IS_SET(FLAG_Z)) {
        state.ticks += 12;
    } else {
        push_w(registers.PC);
        registers.PC = operand;
        state.ticks += 24;
    }
}

void cmd_push_bc() { // 0xC5
    push_w(registers.BC);
}

void cmd_add_a_n(byte_t operand) { // 0xC6
    add_b(&registers.A, operand);
}

void cmd_rst_0() { // 0xC7
    push_w(registers.PC);
    registers.PC = 0x0000;
}

void cmd_ret_z() { // 0xC8
    if (FLAGS_IS_SET(FLAG_Z)) {
        registers.PC = pop_w();
        state.ticks += 20;
    } else {
        state.ticks += 8;
    }
}

void cmd_ret() { // 0xC9
    registers.PC = pop_w();
}

void cmd_jp_z_nn(word_t operand) { // 0xCA
    if (FLAGS_IS_SET(FLAG_Z)) {
        registers.PC = operand;
        state.ticks += 16;
    } else {
        state.ticks += 12;
    }
}

void cmd_call_z_nn(word_t operand) { // 0xCC
    if (FLAGS_IS_SET(FLAG_Z)) {
        push_w(registers.PC);
        registers.PC = operand;
        state.ticks += 24;
    } else {
        state.ticks += 12;
    }
}

void cmd_call_nn(word_t operand) { // 0xCD
    push_w(registers.PC);
    registers.PC = operand;
}

void cmd_adc_n(byte_t operand) { // 0xCE
    adc(operand);
}

void cmd_rst_08() { // 0xCF
    push_w(registers.PC);
    registers.PC = 0x0008;
}

void cmd_ret_nc() { // 0xD0
    if (FLAGS_IS_SET(FLAG_C)) {
        state.ticks += 8;
    } else {
        registers.PC = pop_w();
        state.ticks += 20;
    }
}

void cmd_pop_de() { // 0xD1
    registers.DE = pop_w();
}

void cmd_jp_nc_nn(word_t operand) { // 0xD2
    if (FLAGS_IS_SET(FLAG_C)) {
        state.ticks += 12;
    } else {
        registers.PC = operand;
        state.ticks += 16;
    }
}

void cmd_call_nc_nn(word_t operand) { // 0xD4
    if (FLAGS_IS_SET(FLAG_C)) {
        push_w(registers.PC);
        registers.PC = operand;
        state.ticks += 24;
    } else {
        state.ticks += 12;
    }
}

void cmd_push_de() { // 0xD5
    push_w(registers.DE);
}

void cmd_sub_n(byte_t operand) { // 0xD6
    sub(operand);
}

void cmd_rst_10() { // 0xD7
    push_w(registers.PC);
    registers.PC = 0x0010;
}

void cmd_ret_c() { // 0xD8
    if (FLAGS_IS_SET(FLAG_C)) {
        registers.PC = pop_w();
        state.ticks += 20;
    } else {
        state.ticks += 8;
    }
}

void cmd_jp_c_nn(word_t operand) { // 0xDA
    if (FLAGS_IS_SET(FLAG_C)) {
        registers.PC = operand;
        state.ticks += 16;
    } else {
        state.ticks += 12;
    }
}

void cmd_call_c_nn(word_t operand) { // 0xDC
    if (FLAGS_IS_SET(FLAG_C)) {
        push_w(registers.PC);
        registers.PC = operand;
        state.ticks += 24;
    } else {
        state.ticks += 12;
    }
}

void cmd_sbc_n(byte_t operand) { // 0xDE
    sbc(operand);
}

void cmd_rst_18() { // 0xDF
    push_w(registers.PC);
    registers.PC = 0x0018;
}

void cmd_ld_ff_n_ap(byte_t operand) { // 0xE0
    store_b(registers.A, 0xFF00 + operand);
}

void cmd_pop_hl() { // 0xE1
    registers.HL = pop_w();
}

void cmd_ld_ff_c_a() { // 0xE2
    store_b(registers.A, 0xFF00 + registers.C);
}

void cmd_push_hl() { // 0xE5
    push_w(registers.HL);
}

void cmd_and_n(byte_t operand) { // 0xE6
    registers.A &= operand;

    FLAGS_CLEAR(FLAG_C | FLAG_N);
    FLAGS_SET(FLAG_H);

    if (registers.A) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }
}

void cmd_rst_20() { // 0xE7
    push_w(registers.PC);
    registers.PC = 0x0020;
}

void cmd_add_sp_n(byte_t operand) { // 0xE8
    int32_t res = registers.SP + operand;

    if (res & 0xFFFF0000) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    registers.SP = res & 0xFFFF;

    if (((registers.SP & 0x0F) + (operand & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    FLAGS_CLEAR(FLAG_Z | FLAG_N); // Check.
}

void cmd_jp_hl() { // 0xE9
    registers.PC = registers.HL;
}

void cmd_ld_nnp_a(word_t operand) { // 0xEA
    store_b(registers.A, operand);
}

void cmd_xor_n(byte_t operand) { // 0xEE
    xor(operand);
}

void cmd_rst_28() { // 0xEF
    push_w(registers.PC);
    registers.PC = 0x0028;
}

void cmd_ld_ff_ap_n(byte_t operand) { // 0xF0
    registers.A = load_b(0xFF00 + operand);
}

void cmd_pop_af() { // 0xF1
    registers.AF = pop_w();
}

void cmd_ld_a_ff_c() { // 0xF2
    registers.A = load_b(0xFF00 + registers.C);
}

void cmd_di_inst() { // 0xF3
    // Master interrupt = 0
    cmd_undefined();
}

void cmd_push_af() { // 0xF5
    push_w(registers.AF);
}

void cmd_or_n(byte_t operand) { // 0xF6
    or(operand);
}

void cmd_rst_30() { // 0xF7
    push_w(registers.PC);
    registers.PC = 0x0030;
}

void cmd_ld_hl_sp_n(byte_t operand) { // 0xF8
    int32_t res = registers.SP + operand;

    if (res & 0xFFFF0000) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    if (((registers.SP & 0x0F) + (operand & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }

    FLAGS_CLEAR(FLAG_Z | FLAG_N);

    registers.HL = (word_t)(res & 0xFFFF);
}

void cmd_ld_sp_hl() { // 0xF9
    registers.SP = registers.HL;
}

void cmd_ld_a_nnp(word_t operand) { // 0xFA
    registers.A = load_b(operand);
}

void cmd_ei() { // 0xFB
    // mMaster interrupt = 1
    cmd_undefined();
}

void cmd_cp_n(byte_t operand) { // 0xFE
    FLAGS_SET(FLAG_N);

    if (registers.A == operand) {
        FLAGS_SET(FLAG_Z);
    } else {
        FLAGS_CLEAR(FLAG_Z);
    }

    if (operand > registers.A) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

    if ((operand & 0x0F) > (registers.A & 0x0F)) {
        FLAGS_SET(FLAG_H);
    } else {
        FLAGS_CLEAR(FLAG_H);
    }
}

void cmd_rst_38() { // 0xFF
    push_w(registers.PC);
    registers.PC = 0x0038;
}

int32_t main() {
    load_rom(cart, "tetris.gb");
    cpu_reset();

    if (!glfwInit()) {
        printf("failed to initialize glfw\n");
        return 1;
    }
    
    GLFWwindow* window = glfwCreateWindow(640, 640, "gbe", null, null);
    if (!window) {
        glfwTerminate();
        return 1;
    }

    glfwMakeContextCurrent(window);
    
    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();

        cpu_step();

        glClear(GL_COLOR_BUFFER_BIT);
        glfwSwapBuffers(window);
    }

    glfwDestroyWindow(window);

    glfwTerminate();

    return 0;
}
