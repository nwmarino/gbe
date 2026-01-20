//
//  GBE, Nick Marino
//
//  MIT LICENSE
//

#ifndef GBE_GFX_H_
#define GBE_GFX_H_

#include "common.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

#define GRAPHICS_CONTROL_BGENABLE (1 << 0)
#define GRAPHICS_CONTROL_SPRITE_ENABLE (1 << 1)
#define GRAPHICS_CONTROL_SPRITE_VDOUBLE (1 << 2)
#define GRAPHICS_CONTROL_TILEMAP (1 << 3)
#define GRAPHICS_CONTROL_TILESET (1 << 4)
#define GRAPHICS_CONTROL_WINDOW_ENABLE (1 << 5)
#define GRAPHICS_CONTROL_WINDOW_TILEMAP (1 << 6)
#define GRAPHICS_CONTROL_DISPLAY_ENABLE (1 << 7)

struct graphics_t {
    byte_t control;
    byte_t x_scroll;
    byte_t y_scroll;
    byte_t scanline;
    uint64_t tick;
} extern gfx;

struct rgb_t {
    byte_t r, g, b;
};

struct sprite_t {
    byte_t x;
    byte_t y;
    byte_t tile;
    byte_t prio;
    byte_t v_flip;
    byte_t h_flip;
    byte_t palette;
};

extern struct rgb_t framebuffer[160 * 144];

extern byte_t tiles[384][8][8];
extern struct rgb_t bg_palette[4];
extern struct rgb_t sprite_palette[2][4];

void gfx_step();
void hblank();
void render_scanline();
void update_tile(byte_t data, word_t address);

void draw();

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // GBE_GFX_H_
