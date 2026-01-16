//
//  GBE, Nick Marino.
// 
//  MIT LICENSE
//

#include "common.h"

#include "GLFW/glfw3.h"

#include <GL/gl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ROM_SIZE 0x200000

#define FLAG_Z 7
#define FLAG_N 6
#define FLAG_H 5
#define FLAG_C 4

byte g_bytes[0x200000];
byte g_screen[160][144][3];
byte g_mem[0x10000];

union register_t {
    word reg;
    struct {
        byte low;
        byte high;
    };
};

struct {
    union register_t AF;
    union register_t BC;
    union register_t DE;
    union register_t HL;
    union register_t SP;
} registers;

word PC;

void load_rom(byte* mem, const char* path) {
    memset(mem, 0, ROM_SIZE);
    
    FILE* rom = fopen(path, "rb");
    if (!rom)
        exit(1);

    fread(mem, 1, ROM_SIZE, rom);
    fclose(rom);
}

void setup_memory() {
    PC = 0x100;
    registers.AF.reg = 0x01B0; // GB/SGB: $01, GBP: $FF, GBC: $11
    registers.BC.reg = 0x0013;
    registers.DE.reg = 0x00D8;
    registers.HL.reg = 0x014D;
    registers.SP.reg = 0xFFFE;

    g_mem[0xFF05] = 0x00; // TIMA
    g_mem[0xFF06] = 0x00; // TMA
    g_mem[0xFF07] = 0x00; // TAC
    g_mem[0xFF10] = 0x80; // NR10
    g_mem[0xFF11] = 0xBF; // NR11
    g_mem[0xFF12] = 0xF3; // NR12
    g_mem[0xFF14] = 0xBF; // NR14
    g_mem[0xFF16] = 0x3F; // NR21
    g_mem[0xFF17] = 0x00; // NR22
    g_mem[0xFF19] = 0xBF; // NR24
    g_mem[0xFF1A] = 0x7F; // NR30
    g_mem[0xFF1B] = 0xFF; // NR31
    g_mem[0xFF1C] = 0x9F; // NR32
    g_mem[0xFF1E] = 0xBF; // NR33
    g_mem[0xFF20] = 0xFF; // NR41
    g_mem[0xFF21] = 0x00; // NR42
    g_mem[0xFF22] = 0x00; // NR43
    g_mem[0xFF23] = 0xBF; // NR30
    g_mem[0xFF24] = 0x77; // NR50
    g_mem[0xFF25] = 0xF3; // NR51
    g_mem[0xFF26] = 0xF1; // GB: $F1, SGB: $F0, NR52
    g_mem[0xFF40] = 0x91; // LCDC
    g_mem[0xFF42] = 0x00; // SCY
    g_mem[0xFF43] = 0x00; // SCX
    g_mem[0xFF45] = 0x00; // LYC
    g_mem[0xFF47] = 0xFC; // BGP
    g_mem[0xFF48] = 0xFF; // OBP0
    g_mem[0xFF49] = 0xFF; // OBP1
    g_mem[0xFF4A] = 0x00; // WY
    g_mem[0xFF4B] = 0x00; // WX
    g_mem[0xFFFF] = 0x00; // IE
}

int32_t main(int32_t argc, char** argv) {
    load_rom(g_bytes, "tetris.gb");
    setup_memory();

    if (!glfwInit())
        return 1;
    
    GLFWwindow* window = glfwCreateWindow(640, 640, "gbe", null, null);
    if (!window) {
        glfwTerminate();
        return 1;
    }

    glfwMakeContextCurrent(window);
    
    while (!glfwWindowShouldClose(window)) {
        glClear(GL_COLOR_BUFFER_BIT);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    glfwDestroyWindow(window);

    glfwTerminate();
    return 0;
}
