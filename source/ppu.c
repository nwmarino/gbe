#include "ppu.h"
#include "cpu.h"

#include "GL/gl.h"

struct ppu_t ppu;
struct rgb_t framebuffer[160 * 144];

byte_t tiles[384][8][8];
struct rgb_t bg_palette[4];

const struct rgb_t palette[4] = {
    { 255, 255, 255 },
    { 192, 192, 192 },
    { 96, 96, 96 },
    { 0, 0, 0 },
};

void render_scanline() {
    int32_t map_offset = (ppu.control & GRAPHICS_CONTROL_TILEMAP) ? 0x1C00 : 0x1800;
    map_offset += (((ppu.scanline + ppu.y_scroll) & 255) >> 3) << 5;

    int32_t line_offset = (ppu.x_scroll >> 3);

    int32_t x = ppu.x_scroll & 7;
    int32_t y = (ppu.scanline + ppu.y_scroll) & 7;

    int32_t pixel_offset = ppu.scanline * 160;

    word_t tile = (word_t) memory.vram[map_offset + line_offset];
    byte_t scanline_row[160];

    int32_t i;
    for (i = 0; i < 160; ++i) {
        byte_t color = tiles[tile][y][x];
        scanline_row[i] = color;

        framebuffer[pixel_offset].r = bg_palette[color].r;
        framebuffer[pixel_offset].g = bg_palette[color].g;
        framebuffer[pixel_offset].b = bg_palette[color].b;

        x++;

        if (x == 8) {
            x = 0;
            line_offset = (line_offset + 1) & 31;
            tile = memory.vram[map_offset + line_offset];
        }
    }
}

void update_tile(byte_t data, word_t address) {
    address &= 0x1FFE;

    word_t tile = (address >> 4) & 511;
    word_t y = (address >> 1) & 7;

    byte_t x, bit_index;
    for (x = 0; x < 8; ++x) {
        bit_index = 1 << (7 - x);

        tiles[tile][y][x] = ((memory.vram[address] & bit_index) ? 1 : 0) + ((memory.vram[address + 1] & bit_index) ? 2 : 0);
    }
}

void ppu_reset() {
    ppu.scanline = 0;
    ppu.mode = PPU_MODE_OAM;
    ppu.tick = 0;
    ppu.control = 0;
    ppu.x_scroll = 0;
    ppu.y_scroll = 0;
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
                render_scanline();
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
