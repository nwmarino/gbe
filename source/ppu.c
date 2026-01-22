#include "ppu.h"
#include "cpu.h"

#include "GL/gl.h"

struct ppu_t ppu;
struct rgb_t framebuffer[160 * 144];

byte_t tiles[512][8][8];
struct rgb_t bg_palette[4];

const struct rgb_t palette[4] = {
    { 255, 255, 255 },
    { 192, 192, 192 },
    { 96, 96, 96 },
    { 0, 0, 0 },
};

void update_bg_palette(byte_t value) {
    for (uint32_t i = 0; i < 4u; ++i) {
        byte_t idx = (value >> (i * 2)) & 0x3;
        bg_palette[i] = palette[idx];
    }
}

void ppu_reset() {
    ppu.mode = PPU_MODE_OAM;
    ppu.ly = 0;
    ppu.tick = 0;
    
    update_bg_palette(load8(0xFF47));
}

void ppu_step(uint64_t delta) {
    ppu.tick += delta;
    
    switch (ppu.mode) {
        case PPU_MODE_HBLANK:
            if (ppu.tick >= 204) {
                ppu.tick -= 204;
                ppu.ly++;

                if (ppu.ly == 144) {
                    ppu.mode = PPU_MODE_VBLANK;
                    interrupts.flags |= INTERRUPT_VBLANK;
                } else {
                    ppu.mode = PPU_MODE_OAM;
                }
            }

            break;

        case PPU_MODE_VBLANK:
            if (ppu.tick >= 456) {
                ppu.tick -= 456;
                ppu.ly++;

                if (ppu.ly > 153) {
                    ppu.ly = 0;
                    ppu.mode = PPU_MODE_OAM;
                }
            }

            break;

        case PPU_MODE_OAM:
            if (ppu.tick >= 80) {
                ppu.tick -= 80;
                ppu.mode = PPU_MODE_VRAM;
            }

            break;

        case PPU_MODE_VRAM:
            if (ppu.tick >= 172) {
                ppu.tick -= 172;
                ppu.mode = PPU_MODE_HBLANK;
            }

            break;
    }
}

void draw() {
    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glRasterPos2f(-1.f, 1.f);
    glPixelZoom(1, -1);
    glDrawPixels(160, 144, GL_RGB, GL_UNSIGNED_BYTE, framebuffer);
    glfwSwapBuffers(window);
}
