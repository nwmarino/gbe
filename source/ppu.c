#include "ppu.h"
#include "cpu.h"

#include "GL/gl.h"

#include <stdio.h>

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

void render_scanline() {
    if (!(ppu.lcdc & GRAPHICS_CONTROL_DISPLAY_ENABLE)) {
        for (int x = 0; x < 160; ++x)
            framebuffer[ppu.ly * 160 + x] = palette[0];
        
        return;
    }

    /* Determine tile map and tile data bases from LCDC. */
    uint16_t tile_map_base = (ppu.lcdc & GRAPHICS_CONTROL_TILEMAP) ? 0x9C00 : 0x9800;
    uint16_t tile_data_select = (ppu.lcdc & GRAPHICS_CONTROL_TILESET) ? 0x8000 : 0x8800;

    /* Coordinates in the 256x256 bg map after applying scroll */
    for (int x = 0; x < 160; ++x) {
        uint16_t scrolled_x = (uint16_t)((x + ppu.scx) & 0xFF);
        uint16_t scrolled_y = (uint16_t)((ppu.ly + ppu.scy) & 0xFF);

        uint16_t tile_col = scrolled_x / 8; /* 0..31 */
        uint16_t tile_row = scrolled_y / 8; /* 0..31 */

        uint16_t tile_map_offset = (uint16_t)(tile_map_base - 0x8000);
        uint16_t tile_index_offset = tile_map_offset + tile_row * 32 + tile_col;

        byte_t tile_index = memory.vram[tile_index_offset];

        /* Debug: on first scanline, print some VRAM/tile info once */
        if (ppu.ly == 0 && x == 0) {
            printf("PPU debug: tile_map_base=0x%04X tile_data_select=0x%04X tile_index_offset=0x%04X tile_index=0x%02X\n",
                tile_map_base, tile_data_select, tile_index_offset + 0x8000, tile_index);
            /* print first 16 bytes of tile map area */
            for (int i = 0; i < 16; ++i) {
                printf("vram[0x%04X]=0x%02X\n", tile_map_offset + i + 0x8000, memory.vram[tile_map_offset + i]);
            }
        }

        /* Which line inside the tile we want (0..7) */
        uint16_t line_in_tile = scrolled_y % 8;

        uint16_t tile_address;
        if (tile_data_select == 0x8000) {
            /* Unsigned indexing from 0x8000 */
            tile_address = 0x8000 + (uint16_t)tile_index * 16 + line_in_tile * 2;
        } else {
            /* Signed indexing from 0x9000 (8800 mode uses signed indices)
               tile 0 corresponds to 0x9000 */
            int8_t sindex = (int8_t) tile_index;
            tile_address = (uint16_t)(0x9000 + sindex * 16 + line_in_tile * 2);
        }

        uint16_t vram_offset = (uint16_t)(tile_address - 0x8000);

        /* Read the two pattern bytes for this tile line */
        byte_t low = memory.vram[vram_offset];
        byte_t high = memory.vram[vram_offset + 1];

        /* bit index inside the byte for this pixel */
        int bit = 7 - (scrolled_x % 8);

        byte_t color_id = (byte_t)(((high >> bit) & 0x1) << 1) | (byte_t)((low >> bit) & 0x1);

        /* Map through background palette */
        struct rgb_t color = bg_palette[color_id];

        framebuffer[ppu.ly * 160 + x] = color;
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
                /* Render the scanline after VRAM period (before HBlank) */
                render_scanline();
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
