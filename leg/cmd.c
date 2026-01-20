#include "cmd.h"
#include "cpu.h"
#include "mem.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

const struct cmd_t insts[256] = {
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
    { "XOR (HL)", cmd_xor_hlp, 0 }, // 0xAE
    { "XOR A", cmd_xor_a, 0 }, // 0xAF
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
    { "CB %02X", cmd_cb, 1 }, // 0xCB
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
    { "RETI", interrupt_finalize, 0 }, // 0xD9
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

const struct cb_t extended_insts[256] = {
    { "RLC B", cmd_rlc_b }, // 0x00
    { "RLC C", cmd_rlc_c }, // 0x01
    { "RLC D", cmd_rlc_d }, // 0x02
    { "RLC E", cmd_rlc_e }, // 0x03
    { "RLC H", cmd_rlc_h }, // 0x04
    { "RLC L", cmd_rlc_l }, // 0x05
    { "RLC (HL)", cmd_rlc_hlp }, // 0x06
    { "RLC A", cmd_rlc_a }, // 0x07
    { "RRC B", cmd_rrc_b }, // 0x08
    { "RRC C", cmd_rrc_c }, // 0x09
    { "RRC D", cmd_rrc_d }, // 0x0a
    { "RRC E", cmd_rrc_e }, // 0x0b
    { "RRC H", cmd_rrc_h }, // 0x0c
    { "RRC L", cmd_rrc_l }, // 0x0d
    { "RRC (HL)", cmd_rrc_hlp }, // 0x0e
    { "RRC A", cmd_rrc_a }, // 0x0f
    { "RL B", cmd_rl_b }, // 0x10
    { "RL C", cmd_rl_c }, // 0x11
    { "RL D", cmd_rl_d }, // 0x12
    { "RL E", cmd_rl_e }, // 0x13
    { "RL H", cmd_rl_h }, // 0x14
    { "RL L", cmd_rl_l }, // 0x15
    { "RL (HL)", cmd_rl_hlp }, // 0x16
    { "RL A", cmd_rl_a }, // 0x17
    { "RR B", rr_b }, // 0x18
    { "RR C", rr_c }, // 0x19
    { "RR D", rr_d }, // 0x1a
    { "RR E", rr_e }, // 0x1b
    { "RR H", rr_h }, // 0x1c
    { "RR L", rr_l }, // 0x1d
    { "RR (HL)", rr_hlp }, // 0x1e
    { "RR A", rr_a }, // 0x1f
    { "SLA B", cmd_sla_b }, // 0x20
    { "SLA C", cmd_sla_c }, // 0x21
    { "SLA D", cmd_sla_d }, // 0x22
    { "SLA E", cmd_sla_e }, // 0x23
    { "SLA H", cmd_sla_h }, // 0x24
    { "SLA L", cmd_sla_l }, // 0x25
    { "SLA (HL)", cmd_sla_hlp }, // 0x26
    { "SLA A", cmd_sla_a }, // 0x27
    { "SRA B", cmd_sra_b }, // 0x28
    { "SRA C", cmd_sra_c }, // 0x29
    { "SRA D", cmd_sra_d }, // 0x2a
    { "SRA E", cmd_sra_e }, // 0x2b
    { "SRA H", cmd_sra_h }, // 0x2c
    { "SRA L", cmd_sra_l }, // 0x2d
    { "SRA (HL)", cmd_sra_hlp }, // 0x2e
    { "SRA A", cmd_sra_a }, // 0x2f
    { "SWAP B", cmd_swap_b }, // 0x30
    { "SWAP C", cmd_swap_c }, // 0x31
    { "SWAP D", cmd_swap_d }, // 0x32
    { "SWAP E", cmd_swap_e }, // 0x33
    { "SWAP H", cmd_swap_h }, // 0x34
    { "SWAP L", cmd_swap_l }, // 0x35
    { "SWAP (HL)", cmd_swap_hlp }, // 0x36
    { "SWAP A", cmd_swap_a }, // 0x37
    { "SRL B", cmd_srl_b }, // 0x38
    { "SRL C", cmd_srl_c }, // 0x39
    { "SRL D", cmd_srl_d }, // 0x3a
    { "SRL E", cmd_srl_e }, // 0x3b
    { "SRL H", cmd_srl_h }, // 0x3c
    { "SRL L", cmd_srl_l }, // 0x3d
    { "SRL (HL)", cmd_srl_hlp }, // 0x3e
    { "SRL A", cmd_srl_a }, // 0x3f
    { "BIT 0, B", cmd_bit_0_b }, // 0x40
    { "BIT 0, C", cmd_bit_0_c }, // 0x41
    { "BIT 0, D", cmd_bit_0_d }, // 0x42
    { "BIT 0, E", cmd_bit_0_e }, // 0x43
    { "BIT 0, H", cmd_bit_0_h }, // 0x44
    { "BIT 0, L", cmd_bit_0_l }, // 0x45
    { "BIT 0, (HL)", cmd_bit_0_hlp }, // 0x46
    { "BIT 0, A", cmd_bit_0_a }, // 0x47
    { "BIT 1, B", cmd_bit_1_b }, // 0x48
    { "BIT 1, C", cmd_bit_1_c }, // 0x49
    { "BIT 1, D", cmd_bit_1_d }, // 0x4a
    { "BIT 1, E", cmd_bit_1_e }, // 0x4b
    { "BIT 1, H", cmd_bit_1_h }, // 0x4c
    { "BIT 1, L", cmd_bit_1_l }, // 0x4d
    { "BIT 1, (HL)", cmd_bit_1_hlp }, // 0x4e
    { "BIT 1, A", cmd_bit_1_a }, // 0x4f
    { "BIT 2, B", cmd_bit_2_b }, // 0x50
    { "BIT 2, C", cmd_bit_2_c }, // 0x51
    { "BIT 2, D", cmd_bit_2_d }, // 0x52
    { "BIT 2, E", cmd_bit_2_e }, // 0x53
    { "BIT 2, H", cmd_bit_2_h }, // 0x54
    { "BIT 2, L", cmd_bit_2_l }, // 0x55
    { "BIT 2, (HL)", cmd_bit_2_hlp }, // 0x56
    { "BIT 2, A", cmd_bit_2_a }, // 0x57
    { "BIT 3, B", cmd_bit_3_b }, // 0x58
    { "BIT 3, C", cmd_bit_3_c }, // 0x59
    { "BIT 3, D", cmd_bit_3_d }, // 0x5a
    { "BIT 3, E", cmd_bit_3_e }, // 0x5b
    { "BIT 3, H", cmd_bit_3_h }, // 0x5c
    { "BIT 3, L", cmd_bit_3_l }, // 0x5d
    { "BIT 3, (HL)", cmd_bit_3_hlp }, // 0x5e
    { "BIT 3, A", cmd_bit_3_a }, // 0x5f
    { "BIT 4, B", cmd_bit_4_b }, // 0x60
    { "BIT 4, C", cmd_bit_4_c }, // 0x61
    { "BIT 4, D", cmd_bit_4_d }, // 0x62
    { "BIT 4, E", cmd_bit_4_e }, // 0x63
    { "BIT 4, H", cmd_bit_4_h }, // 0x64
    { "BIT 4, L", cmd_bit_4_l }, // 0x65
    { "BIT 4, (HL)", cmd_bit_4_hlp }, // 0x66
    { "BIT 4, A", cmd_bit_4_a }, // 0x67
    { "BIT 5, B", cmd_bit_5_b }, // 0x68
    { "BIT 5, C", cmd_bit_5_c }, // 0x69
    { "BIT 5, D", cmd_bit_5_d }, // 0x6a
    { "BIT 5, E", cmd_bit_5_e }, // 0x6b
    { "BIT 6, H", cmd_bit_5_h }, // 0x6c
    { "BIT 6, L", cmd_bit_5_l }, // 0x6d
    { "BIT 5, (HL)", cmd_bit_5_hlp }, // 0x6e
    { "BIT 5, A", cmd_bit_5_a }, // 0x6f
    { "BIT 6, B", cmd_bit_6_b }, // 0x70
    { "BIT 6, C", cmd_bit_6_c }, // 0x71
    { "BIT 6, D", cmd_bit_6_d }, // 0x72
    { "BIT 6, E", cmd_bit_6_e }, // 0x73
    { "BIT 6, H", cmd_bit_6_h }, // 0x74
    { "BIT 6, L", cmd_bit_6_l }, // 0x75
    { "BIT 6, (HL)", cmd_bit_6_hlp }, // 0x76
    { "BIT 6, A", cmd_bit_6_a }, // 0x77
    { "BIT 7, B", cmd_bit_7_b }, // 0x78
    { "BIT 7, C", cmd_bit_7_c }, // 0x79
    { "BIT 7, D", cmd_bit_7_d }, // 0x7a
    { "BIT 7, E", cmd_bit_7_e }, // 0x7b
    { "BIT 7, H", cmd_bit_7_h }, // 0x7c
    { "BIT 7, L", cmd_bit_7_l }, // 0x7d
    { "BIT 7, (HL)", cmd_bit_7_hlp }, // 0x7e
    { "BIT 7, A", cmd_bit_7_a }, // 0x7f
    { "RES 0, B", cmd_res_0_b }, // 0x80
    { "RES 0, C", cmd_res_0_c }, // 0x81
    { "RES 0, D", cmd_res_0_d }, // 0x82
    { "RES 0, E", cmd_res_0_e }, // 0x83
    { "RES 0, H", cmd_res_0_h }, // 0x84
    { "RES 0, L", cmd_res_0_l }, // 0x85
    { "RES 0, (HL)", cmd_res_0_hlp }, // 0x86
    { "RES 0, A", cmd_res_0_a }, // 0x87
    { "RES 1, B", cmd_res_1_b }, // 0x88
    { "RES 1, C", cmd_res_1_c }, // 0x89
    { "RES 1, D", cmd_res_1_d }, // 0x8a
    { "RES 1, E", cmd_res_1_e }, // 0x8b
    { "RES 1, H", cmd_res_1_h }, // 0x8c
    { "RES 1, L", cmd_res_1_l }, // 0x8d
    { "RES 1, (HL)", cmd_res_1_hlp }, // 0x8e
    { "RES 1, A", cmd_res_1_a }, // 0x8f
    { "RES 2, B", cmd_res_2_b }, // 0x90
    { "RES 2, C", cmd_res_2_c }, // 0x91
    { "RES 2, D", cmd_res_2_d }, // 0x92
    { "RES 2, E", cmd_res_2_e }, // 0x93
    { "RES 2, H", cmd_res_2_h }, // 0x94
    { "RES 2, L", cmd_res_2_l }, // 0x95
    { "RES 2, (HL)", cmd_res_2_hlp }, // 0x96
    { "RES 2, A", cmd_res_2_a }, // 0x97
    { "RES 3, B", cmd_res_3_b }, // 0x98
    { "RES 3, C", cmd_res_3_c }, // 0x99
    { "RES 3, D", cmd_res_3_d }, // 0x9a
    { "RES 3, E", cmd_res_3_e }, // 0x9b
    { "RES 3, H", cmd_res_3_h }, // 0x9c
    { "RES 3, L", cmd_res_3_l }, // 0x9d
    { "RES 3, (HL)", cmd_res_3_hlp }, // 0x9e
    { "RES 3, A", cmd_res_3_a }, // 0x9f
    { "RES 4, B", cmd_res_4_b }, // 0xa0
    { "RES 4, C", cmd_res_4_c }, // 0xa1
    { "RES 4, D", cmd_res_4_d }, // 0xa2
    { "RES 4, E", cmd_res_4_e }, // 0xa3
    { "RES 4, H", cmd_res_4_h }, // 0xa4
    { "RES 4, L", cmd_res_4_l }, // 0xa5
    { "RES 4, (HL)", cmd_res_4_hlp }, // 0xa6
    { "RES 4, A", cmd_res_4_a }, // 0xa7
    { "RES 5, B", cmd_res_5_b }, // 0xa8
    { "RES 5, C", cmd_res_5_c }, // 0xa9
    { "RES 5, D", cmd_res_5_d }, // 0xaa
    { "RES 5, E", cmd_res_5_e }, // 0xab
    { "RES 5, H", cmd_res_5_h }, // 0xac
    { "RES 5, L", cmd_res_5_l }, // 0xad
    { "RES 5, (HL)", cmd_res_5_hlp }, // 0xae
    { "RES 5, A", cmd_res_5_a }, // 0xaf
    { "RES 6, B", cmd_res_6_b }, // 0xb0
    { "RES 6, C", cmd_res_6_c }, // 0xb1
    { "RES 6, D", cmd_res_6_d }, // 0xb2
    { "RES 6, E", cmd_res_6_e }, // 0xb3
    { "RES 6, H", cmd_res_6_h }, // 0xb4
    { "RES 6, L", cmd_res_6_l }, // 0xb5
    { "RES 6, (HL)", cmd_res_6_hlp }, // 0xb6
    { "RES 6, A", cmd_res_6_a }, // 0xb7
    { "RES 7, B", cmd_res_7_b }, // 0xb8
    { "RES 7, C", cmd_res_7_c }, // 0xb9
    { "RES 7, D", cmd_res_7_d }, // 0xba
    { "RES 7, E", cmd_res_7_e }, // 0xbb
    { "RES 7, H", cmd_res_7_h }, // 0xbc
    { "RES 7, L", cmd_res_7_l }, // 0xbd
    { "RES 7, (HL)", cmd_res_7_hlp }, // 0xbe
    { "RES 7, A", cmd_res_7_a }, // 0xbf
    { "SET 0, B", cmd_set_0_b }, // 0xc0
    { "SET 0, C", cmd_set_0_c }, // 0xc1
    { "SET 0, D", cmd_set_0_d }, // 0xc2
    { "SET 0, E", cmd_set_0_e }, // 0xc3
    { "SET 0, H", cmd_set_0_h }, // 0xc4
    { "SET 0, L", cmd_set_0_l }, // 0xc5
    { "SET 0, (HL)", cmd_set_0_hlp }, // 0xc6
    { "SET 0, A", cmd_set_0_a }, // 0xc7
    { "SET 1, B", cmd_set_1_b }, // 0xc8
    { "SET 1, C", cmd_set_1_c }, // 0xc9
    { "SET 1, D", cmd_set_1_d }, // 0xca
    { "SET 1, E", cmd_set_1_e }, // 0xcb
    { "SET 1, H", cmd_set_1_h }, // 0xcc
    { "SET 1, L", cmd_set_1_l }, // 0xcd
    { "SET 1, (HL)", cmd_set_1_hlp }, // 0xce
    { "SET 1, A", cmd_set_1_a }, // 0xcf
    { "SET 2, B", cmd_set_2_b }, // 0xd0
    { "SET 2, C", cmd_set_2_c }, // 0xd1
    { "SET 2, D", cmd_set_2_d }, // 0xd2
    { "SET 2, E", cmd_set_2_e }, // 0xd3
    { "SET 2, H", cmd_set_2_h }, // 0xd4
    { "SET 2, L", cmd_set_2_l }, // 0xd5
    { "SET 2, (HL)", cmd_set_2_hlp }, // 0xd6
    { "SET 2, A", cmd_set_2_a }, // 0xd7
    { "SET 3, B", cmd_set_3_b }, // 0xd8
    { "SET 3, C", cmd_set_3_c }, // 0xd9
    { "SET 3, D", cmd_set_3_d }, // 0xda
    { "SET 3, E", cmd_set_3_e }, // 0xdb
    { "SET 3, H", cmd_set_3_h }, // 0xdc
    { "SET 3, L", cmd_set_3_l }, // 0xdd
    { "SET 3, (HL)", cmd_set_3_hlp }, // 0xde
    { "SET 3, A", cmd_set_3_a }, // 0xdf
    { "SET 4, B", cmd_set_4_b }, // 0xe0
    { "SET 4, C", cmd_set_4_c }, // 0xe1
    { "SET 4, D", cmd_set_4_d }, // 0xe2
    { "SET 4, E", cmd_set_4_e }, // 0xe3
    { "SET 4, H", cmd_set_4_h }, // 0xe4
    { "SET 4, L", cmd_set_4_l }, // 0xe5
    { "SET 4, (HL)", cmd_set_4_hlp }, // 0xe6
    { "SET 4, A", cmd_set_4_a }, // 0xe7
    { "SET 5, B", cmd_set_5_b }, // 0xe8
    { "SET 5, C", cmd_set_5_c }, // 0xe9
    { "SET 5, D", cmd_set_5_d }, // 0xea
    { "SET 5, E", cmd_set_5_e }, // 0xeb
    { "SET 5, H", cmd_set_5_h }, // 0xec
    { "SET 5, L", cmd_set_5_l }, // 0xed
    { "SET 5, (HL)", cmd_set_5_hlp }, // 0xee
    { "SET 5, A", cmd_set_5_a }, // 0xef
    { "SET 6, B", cmd_set_6_b }, // 0xf0
    { "SET 6, C", cmd_set_6_c }, // 0xf1
    { "SET 6, D", cmd_set_6_d }, // 0xf2
    { "SET 6, E", cmd_set_6_e }, // 0xf3
    { "SET 6, H", cmd_set_6_h }, // 0xf4
    { "SET 6, L", cmd_set_6_l }, // 0xf5
    { "SET 6, (HL)", cmd_set_6_hlp }, // 0xf6
    { "SET 6, A", cmd_set_6_a }, // 0xf7
    { "SET 7, B", cmd_set_7_b }, // 0xf8
    { "SET 7, C", cmd_set_7_c }, // 0xf9
    { "SET 7, D", cmd_set_7_d }, // 0xfa
    { "SET 7, E", cmd_set_7_e }, // 0xfb
    { "SET 7, H", cmd_set_7_h }, // 0xfc
    { "SET 7, L", cmd_set_7_l }, // 0xfd
    { "SET 7, (HL)", cmd_set_7_hlp }, // 0xfe
    { "SET 7, A", cmd_set_7_a }, // 0xff
};

static const byte_t extended_inst_ticks[256] = {
	8, 8, 8, 8, 8,  8, 16, 8,  8, 8, 8, 8, 8, 8, 16, 8, // 0x0_
	8, 8, 8, 8, 8,  8, 16, 8,  8, 8, 8, 8, 8, 8, 16, 8, // 0x1_
	8, 8, 8, 8, 8,  8, 16, 8,  8, 8, 8, 8, 8, 8, 16, 8, // 0x2_
	8, 8, 8, 8, 8,  8, 16, 8,  8, 8, 8, 8, 8, 8, 16, 8, // 0x3_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0x4_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0x5_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0x6_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0x7_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0x8_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0x9_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0xa_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0xb_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0xc_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0xd_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8, // 0xe_
	8, 8, 8, 8, 8,  8, 12, 8,  8, 8, 8, 8, 8, 8, 12, 8  // 0xf_
};

static byte_t inc_b(byte_t data) {
    if ((data & 0x0F) == 0x0F) {
        FLAGS_IS_SET(FLAG_H);
    } else { 
        FLAGS_CLEAR(FLAG_H);
    }

    data += 1;
	
	if (data == 0) {
        FLAGS_SET(FLAG_Z);
    } else {
        FLAGS_CLEAR(FLAG_Z);
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

    data -= 1;

    if (data == 0) {
        FLAGS_SET(FLAG_Z);
    } else {
        FLAGS_CLEAR(FLAG_Z);
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


static byte_t rlc(byte_t data) {
	int32_t carry = (data & 0x80) >> 7;
	
	if(data & 0x80) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }
	
	data <<= 1;
	data += carry;
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N | FLAG_H);
	return data;
}

static byte_t rrc(byte_t data) {
	int32_t carry = data & 0x01;
	
	data >>= 1;
	
	if (carry) {
		FLAGS_SET(FLAG_C);
		data |= 0x80;
	} else {
        FLAGS_CLEAR(FLAG_C);
    }
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }
	
	FLAGS_CLEAR(FLAG_N | FLAG_H);
	return data;
}

static byte_t rl(byte_t data) {
	int carry = FLAGS_IS_SET(FLAG_C) ? 1 : 0;
	
	if (data & 0x80) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

	data <<= 1;
	data += carry;
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N| FLAG_H);
	return data;
}

static byte_t rr(byte_t data) {
	data >>= 1;
	if (FLAGS_IS_SET(FLAG_C)) 
        data |= 0x80;
	
	if (data & 0x01) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N | FLAG_H);
	return data;
}

static byte_t sla(byte_t data) {
	if (data & 0x80) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

	data <<= 1;
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N | FLAG_H);
	return data;
}

static byte_t sra(byte_t data) {
	if (data & 0x01) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }
	
	data = (data & 0x80) | (data >> 1);
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }
	
	FLAGS_CLEAR(FLAG_N | FLAG_H);
	return data;
}

static byte_t swap(byte_t data) {
	data = ((data & 0xF) << 4) | ((data & 0xF0) >> 4);
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N | FLAG_H | FLAG_C);
	return data;
}

static byte_t srl(byte_t data) {
	if (data & 0x01) {
        FLAGS_SET(FLAG_C);
    } else {
        FLAGS_CLEAR(FLAG_C);
    }

	data >>= 1;
	
	if (data) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }

	FLAGS_CLEAR(FLAG_N | FLAG_H);
	return data;
}

static void bit(byte_t bit, byte_t data) {
	if (data & bit) {
        FLAGS_CLEAR(FLAG_Z);
    } else {
        FLAGS_SET(FLAG_Z);
    }
	
	FLAGS_CLEAR(FLAG_N);
	FLAGS_SET(FLAG_H);
}

static byte_t set(byte_t bit, byte_t data) {
	data |= bit;
	return data;
}

void cmd_undefined() {
    byte_t opcode = load_b(--registers.PC);

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
    registers.PC += (signed char) operand;
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
        registers.PC += (signed char) operand;
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
        registers.PC += (signed char) operand;
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
    if (interrupts.master) {

    } else {
        ++registers.PC;
    }
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

void cmd_cb(byte_t opcode) { // 0xCB
    extended_insts[opcode].exec();

    state.ticks += extended_inst_ticks[opcode];
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
    interrupts.master = 0;
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
    interrupts.master = 1;
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

void cmd_rlc_b() {
    registers.B = rlc(registers.B);
}

void cmd_rlc_c() {
    registers.C = rlc(registers.C);
}

void cmd_rlc_d() {
    registers.D = rlc(registers.D);
}

void cmd_rlc_e() {
    registers.E = rlc(registers.E);
}

void cmd_rlc_h() {
    registers.H = rlc(registers.H);
}

void cmd_rlc_l() {
    registers.L = rlc(registers.L);
}

void cmd_rlc_hlp() {
    store_b(rlc(load_b(registers.HL)), registers.HL);
}

void cmd_rlc_a() {
    registers.A = rlc(registers.A);
}

void cmd_rrc_b() {
    registers.B = rrc(registers.B);
}

void cmd_rrc_c() {
    registers.C = rrc(registers.C);
}

void cmd_rrc_d() {
    registers.D = rrc(registers.D);
}

void cmd_rrc_e() {
    registers.E = rrc(registers.E);
}

void cmd_rrc_h() {
    registers.H = rrc(registers.H);
}

void cmd_rrc_l() {
    registers.L = rrc(registers.L);
}

void cmd_rrc_hlp() {
    store_b(rrc(load_b(registers.HL)), registers.HL);
}

void cmd_rrc_a() {
    registers.A = rrc(registers.A);
}

void cmd_rl_b() {
    registers.B = rl(registers.B);
}

void cmd_rl_c() {
    registers.C = rl(registers.C);
}

void cmd_rl_d() {
    registers.D = rl(registers.D);
}

void cmd_rl_e() {
    registers.E = rl(registers.E);
}

void cmd_rl_h() {
    registers.H = rl(registers.H);
}

void cmd_rl_l() {
    registers.L = rl(registers.L);
}

void cmd_rl_hlp() {
    store_b(rl(load_b(registers.HL)), registers.HL);
}

void cmd_rl_a() {
    registers.A = rl(registers.A);
}

void rr_b() {
    registers.B = rr(registers.B);
}

void rr_c() {
    registers.C = rr(registers.C);
}

void rr_d() {
    registers.D = rr(registers.D);
}

void rr_e() {
    registers.E = rr(registers.E);
}

void rr_h() {
    registers.H = rr(registers.H);
}

void rr_l() {
    registers.L = rr(registers.L);
}

void rr_hlp() {
    store_b(rr(load_b(registers.HL)), registers.HL);
}

void rr_a() {
    registers.A = rr(registers.A);
}

void cmd_sla_b() {
    registers.B = sla(registers.B);
}

void cmd_sla_c() {
    registers.C = sla(registers.C);
}

void cmd_sla_d() {
    registers.D = sla(registers.D);
}

void cmd_sla_e() {
    registers.E = sla(registers.E);
}

void cmd_sla_h() {
    registers.H = sla(registers.H);
}

void cmd_sla_l() {
    registers.L = sla(registers.L);
}

void cmd_sla_hlp() {
    store_b(sla(load_b(registers.HL)), registers.HL);
}

void cmd_sla_a() {
    registers.A = sla(registers.A);
}

void cmd_sra_b() {
    registers.B = sra(registers.B);
}

void cmd_sra_c() {
    registers.C = sra(registers.C);
}

void cmd_sra_d() {
    registers.D = sra(registers.D);
}

void cmd_sra_e() {
    registers.E = sra(registers.E);
}

void cmd_sra_h() {
    registers.H = sra(registers.H);
}

void cmd_sra_l() {
    registers.L = sra(registers.L);
}

void cmd_sra_hlp() {
    store_b(sra(load_b(registers.HL)), registers.HL);
}

void cmd_sra_a() {
    registers.A = sra(registers.A);
}

void cmd_swap_b() {
    registers.B = swap(registers.B);
}

void cmd_swap_c() {
    registers.C = swap(registers.C);
}

void cmd_swap_d() {
    registers.D = swap(registers.D);
}

void cmd_swap_e() {
    registers.E = swap(registers.E);
}

void cmd_swap_h() {
    registers.H = swap(registers.H);
}

void cmd_swap_l() {
    registers.L = swap(registers.L);
}

void cmd_swap_hlp() {
    store_b(swap(load_b(registers.HL)), registers.HL);
}

void cmd_swap_a() {
    registers.A = swap(registers.A);
}

void cmd_srl_b() {
    registers.B = srl(registers.B);
}

void cmd_srl_c() {
    registers.C = srl(registers.C);
}

void cmd_srl_d() {
    registers.D = srl(registers.D);
}

void cmd_srl_e() {
    registers.E = srl(registers.E);
}

void cmd_srl_h() {
    registers.H = srl(registers.H);
}

void cmd_srl_l() {
    registers.L = srl(registers.L);
}

void cmd_srl_hlp() {
    store_b(srl(load_b(registers.HL)), registers.HL);
}

void cmd_srl_a() {
    registers.A = srl(registers.A);
}

void cmd_bit_0_b() {
    bit(1 << 0, registers.B);
}

void cmd_bit_0_c() {
    bit(1 << 0, registers.C);
}

void cmd_bit_0_d() {
    bit(1 << 0, registers.D);
}

void cmd_bit_0_e() {
    bit(1 << 0, registers.E);
}

void cmd_bit_0_h() {
    bit(1 << 0, registers.H);
}

void cmd_bit_0_l() {
    bit(1 << 0, registers.L);
}

void cmd_bit_0_hlp() {
    bit(1 << 0, load_b(registers.HL));
}

void cmd_bit_0_a() {
    bit(1 << 0, registers.A);
}

void cmd_bit_1_b() {
    bit(1 << 1, registers.B);
}

void cmd_bit_1_c() {
    bit(1 << 1, registers.C);
}

void cmd_bit_1_d() {
    bit(1 << 1, registers.D);
}

void cmd_bit_1_e() {
    bit(1 << 1, registers.E);
}

void cmd_bit_1_h() {
    bit(1 << 1, registers.H);
}

void cmd_bit_1_l() {
    bit(1 << 1, registers.L);
}

void cmd_bit_1_hlp() {
    bit(1 << 1, load_b(registers.HL));
}

void cmd_bit_1_a() {
    bit(1 << 1, registers.A);
}

void cmd_bit_2_b() {
    bit(1 << 2, registers.B);
}

void cmd_bit_2_c() {
    bit(1 << 2, registers.C);
}

void cmd_bit_2_d() {
    bit(1 << 2, registers.D);
}

void cmd_bit_2_e() {
    bit(1 << 2, registers.E);
}

void cmd_bit_2_h() {
    bit(1 << 2, registers.H);
}

void cmd_bit_2_l() {
    bit(1 << 2, registers.L);
}

void cmd_bit_2_hlp() {
    bit(1 << 2, load_b(registers.HL));
}

void cmd_bit_2_a() {
    bit(1 << 2, registers.A);
}

void cmd_bit_3_b() {
    bit(1 << 3, registers.B);
}

void cmd_bit_3_c() {
    bit(1 << 3, registers.C);
}

void cmd_bit_3_d() {
    bit(1 << 3, registers.D);
}

void cmd_bit_3_e() {
    bit(1 << 3, registers.E);
}

void cmd_bit_3_h() {
    bit(1 << 3, registers.H);
}

void cmd_bit_3_l() {
    bit(1 << 3, registers.L);
}

void cmd_bit_3_hlp() {
    bit(1 << 3, load_b(registers.HL));
}

void cmd_bit_3_a() {
    bit(1 << 3, registers.A);
}

void cmd_bit_4_b() {
    bit(1 << 4, registers.B);
}

void cmd_bit_4_c() {
    bit(1 << 4, registers.C);
}

void cmd_bit_4_d() {
    bit(1 << 4, registers.D);
}

void cmd_bit_4_e() {
    bit(1 << 4, registers.E);
}

void cmd_bit_4_h() {
    bit(1 << 4, registers.H);
}

void cmd_bit_4_l() {
    bit(1 << 4, registers.L);
}

void cmd_bit_4_hlp() {
    bit(1 << 4, load_b(registers.HL));
}

void cmd_bit_4_a() {
    bit(1 << 4, registers.A);
}

void cmd_bit_5_b() {
    bit(1 << 5, registers.B);
}

void cmd_bit_5_c() {
    bit(1 << 5, registers.C);
}

void cmd_bit_5_d() {
    bit(1 << 5, registers.D);
}

void cmd_bit_5_e() {
    bit(1 << 5, registers.E);
}

void cmd_bit_5_h() {
    bit(1 << 5, registers.H);
}

void cmd_bit_5_l() {
    bit(1 << 5, registers.L);
}

void cmd_bit_5_hlp() {
    bit(1 << 5, load_b(registers.HL));
}

void cmd_bit_5_a() {
    bit(1 << 5, registers.A);
}

void cmd_bit_6_b() {
    bit(1 << 6, registers.B);
}

void cmd_bit_6_c() {
    bit(1 << 6, registers.C);
}

void cmd_bit_6_d() {
    bit(1 << 6, registers.D);
}

void cmd_bit_6_e() {
    bit(1 << 6, registers.E);
}

void cmd_bit_6_h() {
    bit(1 << 6, registers.H);
}

void cmd_bit_6_l() {
    bit(1 << 6, registers.L);
}

void cmd_bit_6_hlp() {
    bit(1 << 6, load_b(registers.HL));
}

void cmd_bit_6_a() {
    bit(1 << 6, registers.A);
}

void cmd_bit_7_b() {
    bit(1 << 7, registers.B);
}

void cmd_bit_7_c() {
    bit(1 << 7, registers.C);
}

void cmd_bit_7_d() {
    bit(1 << 7, registers.D);
}

void cmd_bit_7_e() {
    bit(1 << 7, registers.E);
}

void cmd_bit_7_h() {
    bit(1 << 7, registers.H);
}

void cmd_bit_7_l() {
    bit(1 << 7, registers.L);
}

void cmd_bit_7_hlp() {
    bit(1 << 7, load_b(registers.HL));
}

void cmd_bit_7_a() {
    bit(1 << 7, registers.A);
}

void cmd_res_0_b() {
    registers.B &= ~(1 << 0);
}

void cmd_res_0_c() {
    registers.C &= ~(1 << 0);
}

void cmd_res_0_d() {
    registers.D &= ~(1 << 0);
}

void cmd_res_0_e() {
    registers.E &= ~(1 << 0);
}

void cmd_res_0_h() {
    registers.H &= ~(1 << 0);
}

void cmd_res_0_l() {
    registers.L &= ~(1 << 0);
}

void cmd_res_0_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 0), registers.HL);
}

void cmd_res_0_a() {
    registers.A &= ~(1 << 0);
}

void cmd_res_1_b() {
    registers.B &= ~(1 << 1);
}

void cmd_res_1_c() {
    registers.C &= ~(1 << 1);
}

void cmd_res_1_d() {
    registers.D &= ~(1 << 1);
}

void cmd_res_1_e() {
    registers.E &= ~(1 << 1);
}

void cmd_res_1_h() {
    registers.H &= ~(1 << 1);
}

void cmd_res_1_l() {
    registers.L &= ~(1 << 1);
}

void cmd_res_1_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 1), registers.HL);
}

void cmd_res_1_a() {
    registers.A &= ~(1 << 1);
}

void cmd_res_2_b() {
    registers.B &= ~(1 << 2);
}

void cmd_res_2_c() {
    registers.C &= ~(1 << 2);
}

void cmd_res_2_d() {
    registers.D &= ~(1 << 2);
}

void cmd_res_2_e() {
    registers.E &= ~(1 << 2);
}

void cmd_res_2_h() {
    registers.H &= ~(1 << 2);
}

void cmd_res_2_l() {
    registers.L &= ~(1 << 2);
}

void cmd_res_2_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 2), registers.HL);
}

void cmd_res_2_a() {
    registers.A &= ~(1 << 2);
}

void cmd_res_3_b() {
    registers.B &= ~(1 << 3);
}

void cmd_res_3_c() {
    registers.C &= ~(1 << 3);
}

void cmd_res_3_d() {
    registers.D &= ~(1 << 3);
}

void cmd_res_3_e() {
    registers.E &= ~(1 << 3);
}

void cmd_res_3_h() {
    registers.H &= ~(1 << 3);
}

void cmd_res_3_l() {
    registers.L &= ~(1 << 3);
}

void cmd_res_3_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 3), registers.HL);
}

void cmd_res_3_a() {
    registers.A &= ~(1 << 3);
}

void cmd_res_4_b() {
    registers.B &= ~(1 << 4);
}

void cmd_res_4_c() {
    registers.C &= ~(1 << 4);
}

void cmd_res_4_d() {
    registers.D &= ~(1 << 4);
}

void cmd_res_4_e() {
    registers.E &= ~(1 << 4);
}

void cmd_res_4_h() {
    registers.H &= ~(1 << 4);
}

void cmd_res_4_l() {
    registers.L &= ~(1 << 4);
}

void cmd_res_4_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 4), registers.HL);
}

void cmd_res_4_a() {
    registers.A &= ~(1 << 4);
}

void cmd_res_5_b() {
    registers.B &= ~(1 << 5);
}

void cmd_res_5_c() {
    registers.C &= ~(1 << 5);
}

void cmd_res_5_d() {
    registers.D &= ~(1 << 5);
}

void cmd_res_5_e() {
    registers.E &= ~(1 << 5);
}

void cmd_res_5_h() {
    registers.H &= ~(1 << 5);
}

void cmd_res_5_l() {
    registers.L &= ~(1 << 5);
}

void cmd_res_5_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 5), registers.HL);
}

void cmd_res_5_a() {
    registers.A &= ~(1 << 5);
}

void cmd_res_6_b() {
    registers.B &= ~(1 << 6);
}

void cmd_res_6_c() {
    registers.C &= ~(1 << 6);
}

void cmd_res_6_d() {
    registers.D &= ~(1 << 6);
}

void cmd_res_6_e() {
    registers.E &= ~(1 << 6);
}

void cmd_res_6_h() {
    registers.H &= ~(1 << 6);
}

void cmd_res_6_l() {
    registers.L &= ~(1 << 6);
}

void cmd_res_6_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 6), registers.HL);
}

void cmd_res_6_a() {
    registers.A &= ~(1 << 6);
}

void cmd_res_7_b() {
    registers.B &= ~(1 << 7);
}

void cmd_res_7_c() {
    registers.C &= ~(1 << 7);
}

void cmd_res_7_d() {
    registers.D &= ~(1 << 7);
}

void cmd_res_7_e() {
    registers.E &= ~(1 << 7);
}

void cmd_res_7_h() {
    registers.H &= ~(1 << 7);
}

void cmd_res_7_l() {
    registers.L &= ~(1 << 7);
}

void cmd_res_7_hlp() {
    store_b(load_b(registers.HL) & ~(1 << 7), registers.HL);
}

void cmd_res_7_a() {
    registers.A &= ~(1 << 7);
}

void cmd_set_0_b() {
    registers.B = set(1 << 0, registers.B);
}

void cmd_set_0_c() {
    registers.C = set(1 << 0, registers.C);
}

void cmd_set_0_d() {
    registers.D = set(1 << 0, registers.D);
}

void cmd_set_0_e() {
    registers.E = set(1 << 0, registers.E);
}

void cmd_set_0_h() {
    registers.H = set(1 << 0, registers.H);
}

void cmd_set_0_l() {
    registers.L = set(1 << 0, registers.L);
}

void cmd_set_0_hlp() {
    store_b(set(1 << 0, load_b(registers.HL)), registers.HL);
}

void cmd_set_0_a() {
    registers.A = set(1 << 0, registers.A);
}

void cmd_set_1_b() {
    registers.B = set(1 << 1, registers.B);
}

void cmd_set_1_c() {
    registers.C = set(1 << 1, registers.C);
}

void cmd_set_1_d() {
    registers.D = set(1 << 1, registers.D);
}

void cmd_set_1_e() {
    registers.E = set(1 << 1, registers.E);
}

void cmd_set_1_h() {
    registers.H = set(1 << 1, registers.H);
}

void cmd_set_1_l() {
    registers.L = set(1 << 1, registers.L);
}

void cmd_set_1_hlp() {
    store_b(set(1 << 1, load_b(registers.HL)), registers.HL);
}

void cmd_set_1_a() {
    registers.A = set(1 << 1, registers.A);
}

void cmd_set_2_b() {
    registers.B = set(1 << 2, registers.B);
}

void cmd_set_2_c() {
    registers.C = set(1 << 2, registers.C);
}

void cmd_set_2_d() {
    registers.D = set(1 << 2, registers.D);
}

void cmd_set_2_e() {
    registers.E = set(1 << 2, registers.E);
}

void cmd_set_2_h() {
    registers.H = set(1 << 2, registers.H);
}

void cmd_set_2_l() {
    registers.L = set(1 << 2, registers.L);
}

void cmd_set_2_hlp() {
    store_b(set(1 << 2, load_b(registers.HL)), registers.HL);
}

void cmd_set_2_a() {
    registers.A = set(1 << 2, registers.A);
}

void cmd_set_3_b() {
    registers.B = set(1 << 3, registers.B);
}

void cmd_set_3_c() {
    registers.C = set(1 << 3, registers.C);
}

void cmd_set_3_d() {
    registers.D = set(1 << 3, registers.D);
}

void cmd_set_3_e() {
    registers.E = set(1 << 3, registers.E);
}

void cmd_set_3_h() {
    registers.H = set(1 << 3, registers.H);
}

void cmd_set_3_l() {
    registers.L = set(1 << 3, registers.L);
}

void cmd_set_3_hlp() {
    store_b(set(1 << 3, load_b(registers.HL)), registers.HL);
}

void cmd_set_3_a() {
    registers.A = set(1 << 3, registers.A);
}

void cmd_set_4_b() {
    registers.B = set(1 << 4, registers.B);
}

void cmd_set_4_c() {
    registers.C = set(1 << 4, registers.C);
}

void cmd_set_4_d() {
    registers.D = set(1 << 4, registers.D);
}

void cmd_set_4_e() {
    registers.E = set(1 << 4, registers.E);
}

void cmd_set_4_h() {
    registers.H = set(1 << 4, registers.H);
}

void cmd_set_4_l() {
    registers.L = set(1 << 4, registers.L);
}

void cmd_set_4_hlp() {
    store_b(set(1 << 4, load_b(registers.HL)), registers.HL);
}

void cmd_set_4_a() {
    registers.A = set(1 << 4, registers.A);
}

void cmd_set_5_b() {
    registers.B = set(1 << 5, registers.B);
}

void cmd_set_5_c() {
    registers.C = set(1 << 5, registers.C);
}

void cmd_set_5_d() {
    registers.D = set(1 << 5, registers.D);
}

void cmd_set_5_e() {
    registers.E = set(1 << 5, registers.E);
}

void cmd_set_5_h() {
    registers.H = set(1 << 5, registers.H);
}

void cmd_set_5_l() {
    registers.L = set(1 << 5, registers.L);
}

void cmd_set_5_hlp() {
    store_b(set(1 << 5, load_b(registers.HL)), registers.HL);
}

void cmd_set_5_a() {
    registers.A = set(1 << 5, registers.A);
}

void cmd_set_6_b() {
    registers.B = set(1 << 6, registers.B);
}

void cmd_set_6_c() {
    registers.C = set(1 << 6, registers.C);
}

void cmd_set_6_d() {
    registers.D = set(1 << 6, registers.D);
}

void cmd_set_6_e() {
    registers.E = set(1 << 6, registers.E);
}

void cmd_set_6_h() {
    registers.H = set(1 << 6, registers.H);
}

void cmd_set_6_l() {
    registers.L = set(1 << 6, registers.L);
}

void cmd_set_6_hlp() {
    store_b(set(1 << 6, load_b(registers.HL)), registers.HL);
}

void cmd_set_6_a() {
    registers.A = set(1 << 6, registers.A);
}

void cmd_set_7_b() {
    registers.B = set(1 << 7, registers.B);
}

void cmd_set_7_c() {
    registers.C = set(1 << 7, registers.C);
}

void cmd_set_7_d() {
    registers.D = set(1 << 7, registers.D);
}

void cmd_set_7_e() {
    registers.E = set(1 << 7, registers.E);
}

void cmd_set_7_h() {
    registers.H = set(1 << 7, registers.H);
}

void cmd_set_7_l() {
    registers.L = set(1 << 7, registers.L);
}

void cmd_set_7_hlp() {
    store_b(set(1 << 7, load_b(registers.HL)), registers.HL);
}

void cmd_set_7_a() {
    registers.A = set(1 << 7, registers.A);
}
