#include "cpu.h"
#include "ppu.h"

#include <stdio.h>
#include <stdlib.h>

struct cpu_t cpu;
struct registers_t registers;
struct interrupts_t interrupts;
struct memory_t memory;

/* Debug: count VRAM writes to diagnose missing tile data */
static int debug_vram_writes = 0;

static const char* disassemblies[256] = {
    // 0x0_
    "nop",             "ld bc, 0x%04X", "ld (bc), a",   "inc bc", 
    "inc b",           "dec b",         "ld b, 0x%02X", "rlca", 
    "ld (0x%04X), sp", "add hl, bc",    "ld a, (bc)",   "dec bc", 
    "inc c",           "dec c",         "ld c, 0x%02X", "rrca",

    // 0x1_
    "stop",      "ld de, 0x%04X", "ld (de), a",   "inc de",
    "inc d",     "dec d",         "ld d, 0x%02X", "rla",
    "jr 0x%02X", "add hl, de",    "ld a, (de)",   "dec de",
    "inc e",     "dec e",         "ld e, 0x%02X", "rra",

    // 0x2_
    "jr nz, 0x%02X", "ld hl, 0x%04X", "ld (hl+), a",  "inc hl",
    "inc h",         "dec h",         "ld h, 0x%02X", "daa",
    "jr z, 0x%02X",  "add hl, hl",    "ld a, (hl+)",  "dec hl",
    "inc l",         "dec l",         "ld l, 0x%02X", "cpl",

    // 0x3_
    "jr nc, 0x%02X", "ld sp, 0x%04X", "ld (hl-), a",     "inc sp",
    "inc (hl)",       "dec (hl)",     "ld (hl), 0x%02X", "scf",
    "jr c, 0x%02X",   "add hl, sp",   "ld a, (hl-)",     "dec sp",
    "inc a",          "dec a",        "ld a, 0x%02X",    "ccf",

    // 0x4_
    "ld b, b", "ld b, c", "ld b, d",    "ld b, e",
    "ld b, h", "ld b, l", "ld b, (hl)", "ld b, a",
    "ld c, b", "ld c, c", "ld c, d",    "ld c, e",
    "ld c, h", "lc c, l", "lc c, (hl)", "ld c, a",

    // 0x5_
    "ld d, b", "ld d, c", "ld d, d",    "ld d, e",
    "ld d, h", "ld d, l", "ld d, (hl)", "ld d, a",
    "ld e, b", "ld e, c", "ld e, d",    "ld e, e",
    "ld e, h", "ld e, l", "ld e, (hl)", "ld e, a",

    // 0x6_
    "ld h, b", "ld h, c", "ld h, d",    "ld h, e",
    "ld h, h", "ld h, l", "ld h, (hl)", "ld h, a",
    "ld l, b", "ld l, c", "ld l, d",    "ld l, e",
    "ld l, h", "ld l, l", "ld l, (hl)", "ld l, a",

    // 0x7_
    "ld (hl), b", "ld (hl), c", "ld (hl), d", "ld (hl), e",
    "ld (hl), h", "ld (hl), l", "halt",       "ld (hl), a",
    "ld a, b",    "ld a, c",    "ld a, d",    "ld a, e",
    "ld a, h",    "ld a, l",    "ld a, (hl)", "ld a, a",
    
    // 0x8_
    "add a, b", "add a, c", "add a, d",    "add a, e",
    "add a, h", "add a, l", "add a, (hl)", "add a, a",
    "adc a, b", "adc a, c", "adc a, d",    "adc a, e", 
    "adc a, h", "adc a, l", "adc a, (hl)", "adc a, a", 

    // 0x9_
    "sub a, b", "sub a, c", "sub a, d",    "sub a, e",
    "sub a, h", "sub a, l", "sub a, (hl)", "sub a, a",
    "sbc a, b", "sbc a, c", "sbc a, d",    "sbc a, e",
    "sbc a, h", "sbc a, l", "sbc a, (hl)", "sbc a, a",

    // 0xA_
    "and a, b", "and a, c", "and a, d",    "and a, e",
    "and a, h", "and a, l", "and a, (hl)", "and a, a",
    "xor a, b", "xor a, c", "xor a, d",    "xor a, e",
    "xor a, h", "xor a, l", "xor a, (hl)", "xor a, a",

    // 0xB_
    "or a, b", "or a, c", "or a, d",    "or a, e",
    "or a, h", "or a, l", "or a, (hl)", "or a, a",
    "cp a, b", "cp a, c", "cp a, d",    "cp a, e",
    "cp a, h", "cp a, l", "cp a, (hl)", "cp a, a",
    
    // 0xC_
    "ret nz",          "pop bc",      "jp nz, 0x%04X", "jp 0x%04X",
    "call nz, 0x%04X", "push bc",     "add a, 0x%02X", "rst 00h",
    "ret z",           "ret",         "jp z, 0x%04X",  "cb", 
    "call z, 0x%04X",  "call 0x%04X", "add a, 0x%02X", "rst 08h",

    // 0xD_
    "ret nc",          "pop de",  "jp nc, 0x%04X", "_", 
    "call nc, 0x%04X", "push de", "sub a, 0x%02X", "rst 10h", 
    "ret c",           "reti",    "jp c, 0x%04X",  "_", 
    "call c, 0x%04X",  "_",       "sbc a, 0x%02X", "rst 18h",

    // 0xE_
    "ld (FF00+0x%02X), a", "pop hl",  "ld (FF00+c), a", "_",
    "_",                   "push hl", "and a, 0x%02X",  "rst 20h", 
    "add sp, 0x%02X",      "jp hl",   "ld (0x%04X), a", "_",
    "_",                   "_",       "xor a, 0x%02X",  "rst 28h",

    // 0xF_
    "ld a, (FF00+0x%02X)", "pop af",    "ld a, (FF00+c)", "di",
    "_",                   "push af",   "or a, 0x%02X",   "rst 30h", 
    "ld hl, sp+0x%02X",    "ld sp, hl", "ld a, (0x%04X)", "ei", 
    "_",                   "_",         "cp a, 0x%02X",   "rst 38h",
};

static const word_t operand_counts[256] = {
    /* 0x0_ */ 0, 2, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 1, 0,
    /* 0x1_ */ 0, 2, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0,
    /* 0x2_ */ 1, 2, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0,
    /* 0x3_ */ 1, 2, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0,
    /* 0x4_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0x5_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0x6_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0x7_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0x8_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0x9_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0xA_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0xB_ */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 0xC_ */ 0, 0, 2, 2, 2, 0, 1, 0, 0, 0, 2, 0, 2, 2, 1, 0,
    /* 0xD_ */ 0, 0, 2, 0, 2, 0, 1, 0, 0, 0, 2, 0, 2, 0, 1, 0,
    /* 0xE_ */ 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0, 0, 1, 0, 
    /* 0xF_ */ 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0, 0, 1, 0,
};

static const char* cb_disassemblies[256] = {
    // 0x0_
    "rlc b", "rlc c", "rlc d",    "rlc e",
    "rlc h", "rlc l", "rlc (hl)", "rlc a",
    "rrc b", "rrc c", "rrc d",    "rrc e",
    "rrc h", "rrc l", "rrc (hl)", "rrc a",

    // 0x1_
    "rl b", "rl c", "rl d",    "rl e",
    "rl h", "rl l", "rl (hl)", "rl a",
    "rr b", "rr c", "rr d",    "rr e",
    "rr h", "rr l", "rr (hl)", "rr a",

    // 0x2_
    "sla b", "sla c", "sla d",    "sla e",
    "sla h", "sla l", "sla (hl)", "sla a",
    "sra b", "sra c", "sra d",    "sra e",
    "sra h", "sra l", "sra (hl)", "sra r",

    // 0x3_
    "swap b", "swap c", "swap d",    "swap e",
    "swap h", "swap l", "swap (hl)", "swap a",
    "srl b", "srl c",   "srl d",     "srl e", 
    "srl h", "srl l",   "srl (hl)",  "srl a",

    // 0x4_
    "bit 0, b", "bit 0, c", "bit 0, d",    "bit 0, e",
    "bit 0, h", "bit 0, l", "bit 0, (hl)", "bit 0, a",
    "bit 1, b", "bit 1, c", "bit 1, d",    "bit 1, e",
    "bit 1, h", "bit 1, l", "bit 1, (hl)", "bit 1, a",

    // 0x5_
    "bit 2, b", "bit 2, c", "bit 2, d",    "bit 2, e",
    "bit 2, h", "bit 2, l", "bit 2, (hl)", "bit 2, a",
    "bit 3, b", "bit 3, c", "bit 3, d",    "bit 3, e",
    "bit 3, h", "bit 3, l", "bit 3, (hl)", "bit 3, a",

    // 0x6_
    "bit 4, b", "bit 4, c", "bit 4, d",    "bit 4, e",
    "bit 4, h", "bit 4, l", "bit 4, (hl)", "bit 4, a",
    "bit 5, b", "bit 5, c", "bit 5, d",    "bit 5, e",
    "bit 5, h", "bit 5, l", "bit 5, (hl)", "bit 5, a",

    // 0x7_
    "bit 6, b", "bit 6, c", "bit 6, d",    "bit 6, e",
    "bit 6, h", "bit 6, l", "bit 6, (hl)", "bit 6, a",
    "bit 7, b", "bit 7, c", "bit 7, d",    "bit 7, e",
    "bit 7, h", "bit 7, l", "bit 7, (hl)", "bit 7, a",

    // 0x8_
    "res 0, b", "res 0, c", "res 0, d",    "res 0, e",
    "res 0, h", "res 0, l", "res 0, (hl)", "res 0, a",
    "res 1, b", "res 1, c", "res 1, d",    "res 1, e",
    "res 1, h", "res 1, l", "res 1, (hl)", "res 1, a",

    // 0x9_
    "res 2, b", "res 2, c", "res 2, d",    "res 2, e",
    "res 2, h", "res 2, l", "res 2, (hl)", "res 2, a",
    "res 3, b", "res 3, c", "res 3, d",    "res 3, e",
    "res 3, h", "res 3, l", "res 3, (hl)", "res 3, a",

    // 0xA_
    "res 4, b", "res 4, c", "res 4, d",    "res 4, e",
    "res 4, h", "res 4, l", "res 4, (hl)", "res 4, a",
    "res 5, b", "res 5, c", "res 5, d",    "res 5, e",
    "res 5, h", "res 5, l", "res 5, (hl)", "res 5, a",

    // 0xB_
    "res 6, b", "res 6, c", "res 6, d",    "res 6, e",
    "res 6, h", "res 6, l", "res 6, (hl)", "res 6, a",
    "res 7, b", "res 7, c", "res 7, d",    "res 7, e",
    "res 7, h", "res 7, l", "res 7, (hl)", "res 7, a",

    // 0xC_
    "set 0, b", "set 0, c", "set 0, d",    "set 0, e",
    "set 0, h", "set 0, l", "set 0, (hl)", "set 0, a",
    "set 1, b", "set 1, c", "set 1, d",    "set 1, e",
    "set 1, h", "set 1, l", "set 1, (hl)", "set 1, a",

    // 0xD_
    "set 2, b", "set 2, c", "set 2, d",    "set 2, e",
    "set 2, h", "set 2, l", "set 2, (hl)", "set 2, a",
    "set 3, b", "set 3, c", "set 3, d",    "set 3, e",
    "set 3, h", "set 3, l", "set 3, (hl)", "set 3, a",

    // 0xE_
    "set 4, b", "set 4, c", "set 4, d",    "set 4, e",
    "set 4, h", "set 4, l", "set 4, (hl)", "set 4, a",
    "set 5, b", "set 5, c", "set 5, d",    "set 5, e",
    "set 5, h", "set 5, l", "set 5, (hl)", "set 5, a",

    // 0xF_
    "set 6, b", "set 6, c", "set 6, d",    "set 6, e",
    "set 6, h", "set 6, l", "set 6, (hl)", "set 6, a",
    "set 7, b", "set 7, c", "set 7, d",    "set 7, e",
    "set 7, h", "set 7, l", "set 7, (hl)", "set 7, a",
};

static const byte_t tcycles[256] = {
	/* 0x0_ */  4, 12,  8,  8,  4,  4,  8,  4, 20,  8,  8, 8,  4,  4, 8,  4,
	/* 0x1_ */  4, 12,  8,  8,  4,  4,  8,  4, 12,  8,  8, 8,  4,  4, 8,  4,
	/* 0x2_ */  8, 12,  8,  8,  4,  4,  8,  4,  8,  8,  8, 8,  4,  4, 8,  4,
	/* 0x3_ */  8, 12,  8,  8, 12, 12, 12,  4,  8,  8,  8, 8,  4,  4, 8,  4,
	/* 0x4_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0x5_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0x6_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0x7_ */  8,  8,  8,  8,  8,  8,  4,  8,  4,  4,  4, 4,  4,  4, 8,  4, 
	/* 0x8_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0x9_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0xA_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0xB_ */  4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4, 4,  4,  4, 8,  4,
	/* 0xC_ */  8, 12, 12, 16, 12, 16,  8, 16,  8, 16, 12, 4, 12, 24, 8, 16,
	/* 0xD_ */  8, 12, 12,  0, 12, 16,  8, 16,  8, 16, 12, 0, 12,  0, 8, 16,
	/* 0xE_ */ 12, 12,  8,  0,  0, 16,  8, 16, 16,  4, 16, 0,  0,  0, 8, 16,
	/* 0xF_ */ 12, 12,  8,  4,  0, 16,  8, 16, 12,  8, 16, 4,  0,  0, 8, 16,
};

static const byte_t cb_tcycles[256] = {
    /* 0x0_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, 
    /* 0x1_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0x2_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0x3_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0x4_ */ 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 
    /* 0x5_ */ 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 
    /* 0x6_ */ 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 
    /* 0x7_ */ 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 
    /* 0x8_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, 
    /* 0x9_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0xA_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0xB_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0xC_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0xD_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0xE_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
    /* 0xF_ */ 8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
};

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
    int32_t sum = registers.A + value;

    if (((registers.A & 0x0F) + (value & 0x0F)) > 0x0F) {
        FLAGS_SET(FLAG_HALFCARRY);
    } else {
        FLAGS_CLEAR(FLAG_HALFCARRY);
    }

    if (sum & 0xFF00) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

    registers.A = (byte_t)(sum & 0xFF);

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
static byte_t op_swap(byte_t value) {
	value = ((value & 0xF) << 4) | ((value & 0xF0) >> 4);
	
	if (value == 0) {
        FLAGS_SET(FLAG_ZERO);
    } else {
        FLAGS_CLEAR(FLAG_ZERO);
    }

	FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY | FLAG_CARRY);
	return value;
}

static void op_bit(byte_t bit, byte_t value) {
	if (value & bit) {
        FLAGS_CLEAR(FLAG_ZERO);
    } else {
        FLAGS_SET(FLAG_ZERO);
    }
	
	FLAGS_CLEAR(FLAG_NEGATIVE);
	FLAGS_SET(FLAG_HALFCARRY);
}

static byte_t srl(byte_t value) {
	if (value & 0x01) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

	value >>= 1;
	
	if (value) {
        FLAGS_CLEAR(FLAG_ZERO);
    } else {
        FLAGS_SET(FLAG_ZERO);
    }

	FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY);
	return value;
}

static byte_t sla(byte_t value) {
    if (value & 0x80) {
        FLAGS_SET(FLAG_CARRY);
    } else {
        FLAGS_CLEAR(FLAG_CARRY);
    }

	value <<= 1;
	
    value == 0 ? FLAGS_SET(FLAG_ZERO) : FLAGS_CLEAR(FLAG_ZERO);
	FLAGS_CLEAR(FLAG_NEGATIVE | FLAG_HALFCARRY);
	return value;
}

void store8(byte_t value, word_t address) {
    if (address <= 0x7FFF) {
        memory.cart[address] = value;
    } else if (address >= 0x8000 && address <= 0x9FFF) {
        memory.vram[address - 0x8000] = value;
        /* Debug: log first several VRAM writes so we can see when tile data is written */
        if (debug_vram_writes < 64) {
            printf("DEBUG: store8 VRAM write addr=0x%04X value=0x%02X vram_index=0x%04X\n",
                   address, value, address - 0x8000);
            debug_vram_writes++;
        }
    } else if (address >= 0xA000 && address <= 0xBFFF) {
        memory.sram[address - 0xA000] = value;
    } else if (address >= 0xC000 && address <= 0xDFFF) {
        memory.wram[address - 0xC000] = value;
    } else if (address >= 0xE000 && address <= 0xFDFF) {
        memory.wram[address - 0xE000] = value;
    } else if (address >= 0xFE00 && address <= 0xFE9F) {
        memory.oam[address - 0xFE00] = value;
    } else if (address >= 0xFF00 && address <= 0xFF7F) {
        memory.io[address - 0xFF00] = value;

        if (address == 0xFF0F) {
            interrupts.flags = value;
        } else if (address == 0xFF40) {
            ppu.lcdc = value;
        } else if (address == 0xFF42) {
            ppu.scy = value;
        } else if (address == 0xFF43) {
            ppu.scx = value;
        } else if (address == 0xFF44) {
            ppu.ly = value; /* Should be read-only. */
            printf("0xFF44 (LY) is read-only!\n");
            exit(1);
        } else if (address == 0xFF47) {
            update_bg_palette(value);
        }
    } else if (address >= 0xFF80 && address <= 0xFFFE) {
        if (address == 0xFF80) {
            printf("saw 0xFF80 write, skipping\n");
            return;
        }

        memory.hram[address - 0xFF80] = value;
    } else if (address == 0xFFFF) {
        interrupts.enable = value;
    }
}

byte_t load8(word_t address) {
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
        if (address == 0xFF05 || address == 0xFF06 || address == 0xFF07) {
            printf("timer not implemented!\n");
            exit(1);
        } else if (address == 0xFF0F) {
            return interrupts.flags;
        } else if (address == 0xFF40) {
            return ppu.lcdc;
        } else if (address == 0xFF42) {
            return ppu.scy;
        } else if (address == 0xFF43) {
            return ppu.scx;
        } else if (address == 0xFF44) {
            return ppu.ly;
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

void store16(word_t value, word_t address) {
    store8((byte_t)(value & 0x00FF), address);
    store8((byte_t)((value & 0xFF00) >> 8), address + 1);
}

word_t load16(word_t address) {
    return load8(address) | (load8(address + 1) << 8);
}

void push(word_t value) {
    registers.SP -= 2;
    store16(value, registers.SP);
}

word_t pop() {
    word_t value = load16(registers.SP);
    registers.SP += 2;
    return value;    
}

FILE* lg = null;

void handle_cb() {
    byte_t opcode = load8(registers.PC++);

    fprintf(lg, "%s\n", cb_disassemblies[opcode]);

    switch (opcode) {
        case 0x27:
            registers.A = sla(registers.A);
            break;

        case 0x37:
            registers.A = op_swap(registers.A);
            break;

        case 0x38:
            registers.B = srl(registers.B);
            break;

        case 0x50:
            op_bit(1 << 2, registers.B);
            break;
        
        case 0x7E:
            op_bit(1 << 7, load8(registers.HL));
            break;

        case 0x7F:
            op_bit(1 << 7, registers.A);
            break;
        
        case 0x86:
            store8(load8(registers.HL) & ~(1 << 0), registers.HL);
            break;

        case 0x87:
            registers.A &= ~(1 << 0);
            break;

        default:
            printf("Unimplemented CB opcode 0x%02X!\n", opcode);
            exit(1);
    }

    cpu.ticks += cb_tcycles[opcode];
}

void cpu_print_registers() {
    if (!lg)
        lg = fopen("log.txt", "w");    

    fprintf(lg, "A:%02X F:%02X B:%02X C:%02X D:%02X E:%02X H:%02X L:%02X SP:%04X PC:%04X PCMEM:%02X,%02X,%02X,%02X\n", 
        registers.A,
        registers.F, 
        registers.B, 
        registers.C, 
        registers.D, 
        registers.E, 
        registers.H, 
        registers.L, 
        registers.SP, 
        registers.PC, 
        load8(registers.PC), 
        load8(registers.PC + 1), 
        load8(registers.PC + 2), 
        load8(registers.PC + 3));

    //printf("IME:0x%04X IE:0x%04X IF:0x%04X\n", 
     //   interrupts.master, 
      //  interrupts.enable, 
      //  interrupts.flags);
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

    store8(0x00, 0xFF05);
    store8(0x00, 0xFF06);
    store8(0x00, 0xFF07);
    store8(0x80, 0xFF10);
    store8(0xBF, 0xFF11);
    store8(0xF3, 0xFF12);
    store8(0xBF, 0xFF14);
    store8(0x3F, 0xFF16);
    store8(0x00, 0xFF17);
    store8(0xBF, 0xFF19);
    store8(0x7F, 0xFF1A);
    store8(0xFF, 0xFF1B);
    store8(0x9F, 0xFF1C);
    store8(0xBF, 0xFF1E);
    store8(0xFF, 0xFF20);
    store8(0x00, 0xFF21);
    store8(0x00, 0xFF22);
    store8(0xBF, 0xFF23);
    store8(0x77, 0xFF24);
    store8(0xF3, 0xFF25);
    store8(0xF1, 0xFF26);
    store8(0x91, 0xFF40);
    store8(0x00, 0xFF42);
    store8(0x00, 0xFF43);
    store8(0x00, 0xFF45);
    store8(0xFC, 0xFF47);
    store8(0xFF, 0xFF48);
    store8(0xFF, 0xFF49);
    store8(0x00, 0xFF4A);
    store8(0x00, 0xFF4B);
    store8(0x00, 0xFFFF);
}

void cpu_step() {
    cpu_print_registers();

    // Load the opcode at the program counter (PC).
    byte_t opcode = load8(registers.PC++);
    word_t num_operands = operand_counts[opcode];
    word_t operands = 0;
    if (num_operands == 0) {
        fprintf(lg, "%s", disassemblies[opcode]);
    } else if (num_operands == 1) {
        operands = (word_t) load8(registers.PC);
        fprintf(lg, disassemblies[opcode], (byte_t) operands);
    } else if (num_operands == 2) {
        operands = load16(registers.PC);
        fprintf(lg, disassemblies[opcode], (word_t) operands);
    }

    fprintf(lg, "\n");

    registers.PC += num_operands;

    switch (opcode) {
        case 0x00: // NOP
        case 0x40: // LD B, B
        case 0x49: // LD C, C
        case 0x52: // LD D, D
        case 0x5B: // LD E, E
        case 0x64: // LD H, H
        case 0x6D: // LD L, L
        case 0x7F: // LD A, A
            break;

        case 0x01:
            registers.BC = operands;
            break;

        case 0x02:
            store8(registers.A, registers.BC);
            break;

        case 0x03:
            registers.BC++;
            break;

        case 0x04:
            registers.B = op_inc(registers.B);
            break;

        case 0x05:
            registers.B = op_dec(registers.B);
            break;

        case 0x06:
            registers.B = (byte_t) operands;
            break;

        case 0x08:
            store16(registers.SP, operands);
            break;

        case 0x09:
            op_add16(registers.BC);
            break;

        case 0x0A:
            registers.A = load8(registers.BC);
            break;

        case 0x0B:
            registers.BC--;
            break;

        case 0x0C:
            registers.C = op_inc(registers.C);
            break;

        case 0x0D:
            registers.C = op_dec(registers.C);
            break;

        case 0x0E:
            registers.C = (byte_t) operands;
            break;

        case 0x11:
            registers.DE = operands;
            break;

        case 0x12:
            store8(registers.A, registers.DE);
            break;

        case 0x13:
            registers.DE++;
            break;

        case 0x14:
            registers.D = op_inc(registers.D);
            break;

        case 0x15:
            registers.D = op_dec(registers.D);
            break;

        case 0x16:
            registers.D = (byte_t) operands;
            break;

        case 0x18:
            registers.PC += (signed char) operands;
            break;

        case 0x19:
            op_add16(registers.DE);
            break;

        case 0x1A:
            registers.A = load8(registers.DE);
            break;

        case 0x1C:
            registers.E = op_inc(registers.E);
            break;

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
            break;
        }

        case 0x20:
            if (!FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC += (signed char) operands;
                cpu.ticks += 4;
            }

            break;

        case 0x21:
            registers.HL = operands;
            break;

        case 0x22:
            store8(registers.A, registers.HL++);
            break;

        case 0x23:
            registers.HL++;
            break;

        case 0x24:
            registers.H = op_inc(registers.H);
            break;

        case 0x26:
            registers.H = (byte_t) operands;
            break;

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

            A == 0 ? FLAGS_SET(FLAG_ZERO) : FLAGS_CLEAR(FLAG_ZERO);
            carry == 0 ? FLAGS_CLEAR(FLAG_CARRY) : FLAGS_SET(FLAG_CARRY);

            break;
        }

        case 0x28: {
            if (FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC += (signed char) operands;
                cpu.ticks += 4;
            }

            break;
        }

        case 0x2A:
            registers.A = load8(registers.HL++);
            break;

        case 0x2C:
            registers.L = op_inc(registers.L);
            break;

        case 0x2D:
            registers.L = op_dec(registers.L);
            break;

        case 0x2F:
            registers.A = ~registers.A;
            FLAGS_SET(FLAG_NEGATIVE | FLAG_HALFCARRY);
            break;

        case 0x31:
            registers.SP = operands;
            break;

        case 0x30:
            if (!FLAGS_IS_SET(FLAG_CARRY)) {
                registers.PC += (signed char) operands;
                cpu.ticks += 4;
            }

            break;

        case 0x32:
            store8(registers.A, registers.HL--);
            break;

        case 0x34:
            store8(op_inc(load8(registers.HL)), registers.HL);
            break;

        case 0x35:
            store8(op_inc(load8(registers.HL)), registers.HL);
            break;

        case 0x36:
            store8((byte_t) operands, registers.HL);
            break;

        case 0x38:
            if (FLAGS_IS_SET(FLAG_CARRY)) {
                registers.PC += (signed char) operands;
                cpu.ticks += 4;
            }

            break;

        case 0x3A:
            registers.A = load8(registers.HL--);
            break;

        case 0x3B:
            registers.SP--;
            break;

        case 0x3C:
            registers.A = op_inc(registers.A);
            break;

        case 0x3D:
            registers.A = op_dec(registers.A);
            break;

        case 0x3E:
            registers.A = (byte_t) operands;
            break;

        case 0x46:
            registers.B = load8(registers.HL);
            break;

        case 0x47:
            registers.B = registers.A;
            break;

        case 0x4E:
            registers.C = load8(registers.HL);
            break;

        case 0x4F:
            registers.C = registers.A;
            break;

        case 0x54:
            registers.D = registers.H;
            break;

        case 0x56:
            registers.D = load8(registers.HL);
            break;

        case 0x57:
            registers.D = registers.A;
            break;

        case 0x5A:
            registers.E = registers.D;
            break;

        case 0x5C:
            registers.E = registers.H;
            break;

        case 0x5D:
            registers.E = registers.L;
            break;

        case 0x5E:
            registers.E = load8(registers.HL);
            break;

        case 0x5F:
            registers.E = registers.A;
            break;

        case 0x60:
            registers.H = registers.B;
            break;

        case 0x62:
            registers.H = registers.D;
            break;

        case 0x67:
            registers.H = registers.A;
            break;

        case 0x69:
            registers.L = registers.C;
            break;

        case 0x6B:
            registers.L = registers.E;
            break;

        case 0x6F:
            registers.L = registers.A;
            break;

        case 0x70:
            store8(registers.B, registers.HL);
            break;

        case 0x77:
            store8(registers.A, registers.HL);
            break;

        case 0x78:
            registers.A = registers.B;
            break;

        case 0x79:
            registers.A = registers.C;
            break;

        case 0x7A:
            registers.A = registers.D;
            break;

        case 0x7B:
            registers.A = registers.E;
            break;

        case 0x7C:
            registers.A = registers.H;
            break;

        case 0x7D:
            registers.A = registers.L;
            break;

        case 0x7E:
            registers.A = load8(registers.HL);
            break;

        case 0x80:
            op_add8(registers.B);
            break;

        case 0x85:
            op_add8(registers.L);
            break;

        case 0x86:
            op_add8(load8(registers.HL));
            break;
        
        case 0x87:
            op_add8(registers.A);
            break;

        case 0x89:
            op_adc(registers.C);
            break;

        case 0x8C:
            op_adc(registers.H);
            break;

        case 0x8E:
            op_adc(load8(registers.HL));
            break;

        case 0x92:
            op_sub8(registers.D);
            break;

        case 0x99:
            op_sbc(registers.C);
            break;

        case 0xA1:
            op_and(registers.C);
            break;

        case 0xA7:
            op_and(registers.A);
            break;

        case 0xA9:
            op_xor(registers.C);
            break;

        case 0xAE:
            op_xor(load8(registers.HL));
            break;

        case 0xAF:
            op_xor(registers.A);
            break;

        case 0xB0:
            op_or(registers.B);
            break;

        case 0xB1:
            op_or(registers.C);
            break;

        case 0xB7:
            op_or(registers.A);
            break;

        case 0xC0:
            if (!FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = pop();
                cpu.ticks += 12;
            }

            break;

        case 0xC1:
            registers.BC = pop();
            break;

        case 0xC2:
            if (!FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = operands;
                cpu.ticks += 4;
            }

            break;

        case 0xC3:
            registers.PC = operands;
            break;

        case 0xC4:
            if (!FLAGS_IS_SET(FLAG_ZERO)) {
                push(registers.PC);
                registers.PC = operands;
                cpu.ticks += 12;
            }

            break;

        case 0xC5:
            push(registers.BC);
            break;

        case 0xC6:
            op_add8((byte_t) operands);
            break;

        case 0xC8:
            if (FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = pop();
                cpu.ticks += 12;
            }

            break;

        case 0xC9:
            registers.PC = pop();
            break;

        case 0xCA: 
            if (FLAGS_IS_SET(FLAG_ZERO)) {
                registers.PC = operands;
                cpu.ticks += 4;
            }

            break;

        case 0xCB:
            handle_cb();
            break;

        case 0xCD:
            push(registers.PC);
            registers.PC = operands;
            break;

        case 0xD0:
            if (!FLAGS_IS_SET(FLAG_CARRY)) {
                registers.PC = pop();
                cpu.ticks += 12;
            }

            break;

        case 0xD1:
            registers.DE = pop();
            break;

        case 0xD5:
            push(registers.DE);
            break;

        case 0xD6:
            op_sub8((byte_t) operands);
            break;

        case 0xD8:
            if (FLAGS_IS_SET(FLAG_CARRY)) {
                registers.PC = pop();
                cpu.ticks += 12;
            }

            break;

        case 0xD9: {
            registers.PC = pop();
            interrupts.master = 1;
            break;
        }

        case 0xDF:
            push(registers.PC);
            registers.PC = 0x0018;
            break;

        case 0xE0:
            store8(registers.A, 0xFF00 + (byte_t) operands);
            break;

        case 0xE1:
            registers.HL = pop();
            break;
        
        case 0xE2:
            store8(registers.A, 0xFF00 + registers.C);
            break;

        case 0xE5:
            push(registers.HL);
            break;

        case 0xE6:
            op_and((byte_t) operands);
            break;

        case 0xE9:
            registers.PC = registers.HL;
            break;

        case 0xEA:
            store8(registers.A, operands);
            break;

        case 0xEE:
            op_xor((byte_t) operands);
            break;

        case 0xEF:
            push(registers.PC);
            registers.PC = 0x0028;
            break;

        case 0xF0:
            registers.A = load8((word_t)(0xFF00 + (byte_t) operands));
            break;

        case 0xF1:
            registers.AF = pop();
            break;

        case 0xF3:
            interrupts.master = 0;
            break;

        case 0xF5:
            push(registers.AF);
            break;

        case 0xF6:
            op_or((byte_t) operands);
            break;

        case 0xF7:
            push(registers.PC);
            registers.PC = 0x0030;
            break;

        case 0xF8:
            registers.HL = registers.SP + (byte_t) operands;
            break;

        case 0xF9:
            registers.SP = registers.HL;
            break;

        case 0xFA:
            registers.A = load8(operands);
            break;

        case 0xFB:
            interrupts.pending = 2;
            break;

        case 0xFE:
            op_cp((byte_t) operands);
            break;

        case 0xFF:
            push(registers.PC);
            registers.PC = 0x0038;
            break;
    
        default:
            printf("Unimplemented opcode 0x%02X!\n", opcode);
            exit(1);
    }

    cpu.ticks += tcycles[opcode];

    if (interrupts.pending > 0) {
        interrupts.pending--;
        if (interrupts.pending == 0)
            interrupts.master = 1;
    }
}

void cpu_cleanup() {
    //fclose(lg);
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

    push(registers.PC);
    registers.PC = 0x40;
    cpu.ticks += 12;
}

void interrupt_lcdstat() {
    interrupts.flags &= ~INTERRUPT_LCDSTAT;
    interrupts.master = 0;

    push(registers.PC);
    registers.PC = 0x48;
    cpu.ticks += 12;
}

void interrupt_timer() {
    interrupts.flags &= ~INTERRUPT_TIMER;
    interrupts.master = 0;

    push(registers.PC);
    registers.PC = 0x50;
    cpu.ticks += 12;
}

void interrupt_serial() {
    interrupts.flags &= ~INTERRUPT_SERIAL;
    interrupts.master = 0;

    push(registers.PC);
    registers.PC = 0x58;
    cpu.ticks += 12;
}

void interrupt_joypad() {
    interrupts.flags &= ~INTERRUPT_JOYPAD;
    interrupts.master = 0;

    push(registers.PC);
    registers.PC = 0x60;
    cpu.ticks += 12;
}
