#include "common.h"
#include "cpu.h"
#include "ppu.h"

#include <GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

GLFWwindow* window = null;

void load_rom(byte_t* cart, const char* path) {
    memset(cart, 0, 32768);
    
    FILE* rom = fopen(path, "rb");
    if (!rom) {
        printf("failed to open rom file: %s\n", path);
        exit(1);
    }

    size_t n = fread(cart, 1, 32768, rom);
    printf("ROM loaded: %s (%zu bytes read, truncated to 0x8000)\n", path, n);
    fclose(rom);
}

int32_t main(int32_t argc, char* argv[]) {
    load_rom(memory.cart, "tetris.gb");
    cpu_reset();
    ppu_reset();

    if (!glfwInit()) {
        printf("failed to initialize GLFW!\n");
        return 1;
    }

    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    //glfwWindowHint(GLFW_DECORATED, GLFW_FALSE);

    window = glfwCreateWindow(160, 144, "gbe", null, null);
    if (!window) {
        printf("failed to open window!\n");
        return 1;
    }

    glfwMakeContextCurrent(window);

	//glMatrixMode(GL_MODELVIEW);
	//glLoadIdentity();
	//glOrtho(0, 160, 144, 0, -1.0, 1.0);
	//glShadeModel(GL_FLAT);

	glEnable(GL_TEXTURE_2D);
	glDisable(GL_DEPTH_TEST);
	glDisable(GL_CULL_FACE);
	glDisable(GL_DITHER);
	glDisable(GL_BLEND);
    //glfwSwapInterval(0); // no vsync

    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();
        
        uint64_t before = cpu.ticks;
        cpu_step();
        ppu_step(cpu.ticks - before);
        interrupt_step();
    }

    cpu_cleanup();

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
