#include "gfx.h"

#include "GL/gl.h"
#include "cpu.h"
#include "mem.h"

struct rgb_t framebuffer[160 * 144];

struct rgb_t palette[4] = {
    { 255, 255, 255 },
    { 192, 192, 192 },
    { 96, 96, 96 },
    { 0, 0, 0 }
};

struct graphics_t gfx;

byte_t tiles[384][8][8];
struct rgb_t bg_palette[4];
struct rgb_t sprite_palette[2][4];

void gfx_step() {
    enum mode_t {
        MODE_HBLANK = 0,
        MODE_VBLANK = 1,
        MODE_OAM = 2,
        MODE_VRAM = 3,
    } static mode = MODE_HBLANK;

    static int32_t last_ticks = 0;

    gfx.tick += state.ticks - last_ticks;
    last_ticks = state.ticks;

    switch (mode) {
        case MODE_HBLANK:
            if (gfx.tick >= 204) {
                hblank();

                if (gfx.scanline == 143) {
                    if (interrupts.enable & INTERRUPT_VBLANK)
                        interrupts.flags |= INTERRUPT_VBLANK;
                        
                    mode = MODE_VBLANK;
                } else {
                    mode = MODE_OAM;
                }

                gfx.tick -= 204;
            }

            break;

        case MODE_VBLANK:
            if (gfx.tick >= 456) {
                ++gfx.scanline;

                if (gfx.scanline > 153) {
                    gfx.scanline = 0;
                    mode = MODE_OAM;
                }

                gfx.tick -= 456;
            }

            break;

        case MODE_OAM:
            if (gfx.tick >= 80) {
                mode = MODE_VRAM;
                render_scanline();
                gfx.tick -= 80;
            }

            break;

        case MODE_VRAM:
            if (gfx.tick >= 172) {
                mode = MODE_HBLANK;
                gfx.tick -= 172;
            }

            break;
    }
}

void hblank() {
    ++gfx.scanline;
}

void render_scanline() {
    int32_t map_offset = (gfx.control & GRAPHICS_CONTROL_TILEMAP) ? 0x1C00 : 0x1800;
    map_offset += (((gfx.scanline + gfx.y_scroll) & 255) >> 3) << 5;

    int32_t line_offset = (gfx.x_scroll >> 3);

    int32_t x = gfx.x_scroll & 7;
    int32_t y = (gfx.scanline + gfx.y_scroll) & 7;

    int32_t pixel_offset = gfx.scanline * 160;

    word_t tile = (word_t) vram[map_offset + line_offset];
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
            tile = vram[map_offset + line_offset];
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

        tiles[tile][y][x] = ((vram[address] & bit_index) ? 1 : 0) + ((vram[address + 1] & bit_index) ? 2 : 0);
    }
}

void draw() {
    glClear(GL_COLOR_BUFFER_BIT);
    glClearColor(255.f, 0.f, 0.f, 200.f);
    glRasterPos2f(-1.f, 1.f);
    glPixelZoom(1, -1);
    glDrawPixels(160, 144, GL_RGB, GL_UNSIGNED_BYTE, framebuffer);
}
