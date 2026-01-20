#include "ppu.h"
#include "cpu.h"

#include "GL/gl.h"

struct ppu_t ppu;
struct rgb_t framebuffer[160 * 144];

void ppu_reset() {
    ppu.scanline = 0;
    ppu.mode = PPU_MODE_OAM;
    ppu.tick = 0;
}

void ppu_step(uint64_t delta) {
    ppu.tick += delta;
    
    switch (ppu.mode) {
        case PPU_MODE_HBLANK:
            if (ppu.tick >= 204) {
                ppu.tick -= 204;
                ppu.scanline += 1;

                storeb(ppu.scanline, 0xFF44);

                if (ppu.scanline == 144) {
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
                ppu.scanline += 1;

                storeb(ppu.scanline, 0xFF44);

                if (ppu.scanline > 153) {
                    ppu.scanline = 0;
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

                int32_t y = ppu.scanline;
                for (int32_t x = 0; x < 160; ++x)
                    framebuffer[y * 160 + x] = (struct rgb_t) { 255, 0, 255 };

                ppu.mode = PPU_MODE_HBLANK;
            }

            break;
    }
}

void draw() {
    glClearColor(1.f, 0.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    
    glRasterPos2f(-1.f, 1.f);
    glPixelZoom(1, -1);
    glDrawPixels(160, 144, GL_RGB, GL_UNSIGNED_BYTE, framebuffer);
}
