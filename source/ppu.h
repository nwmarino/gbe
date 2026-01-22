#ifndef PPU_H_
#define PPU_H_

#include "common.h"

#include <GLFW/glfw3.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

#define PPU_MODE_HBLANK 0
#define PPU_MODE_VBLANK 1
#define PPU_MODE_OAM 2
#define PPU_MODE_VRAM 3

#define GRAPHICS_CONTROL_BGENABLE (1 << 0)
#define GRAPHICS_CONTROL_SPRITE_ENABLE (1 << 1)
#define GRAPHICS_CONTROL_SPRITE_VDOUBLE (1 << 2)
#define GRAPHICS_CONTROL_TILEMAP (1 << 3)
#define GRAPHICS_CONTROL_TILESET (1 << 4)
#define GRAPHICS_CONTROL_WINDOW_ENABLE (1 << 5)
#define GRAPHICS_CONTROL_WINDOW_TILEMAP (1 << 6)
#define GRAPHICS_CONTROL_DISPLAY_ENABLE (1 << 7)

struct rgb_t {
    byte_t r, g, b;
};

struct ppu_t {
    byte_t mode;
    byte_t lcdc;
    byte_t scy;
    byte_t scx;
    byte_t ly;
    uint64_t tick;
};

extern GLFWwindow* window;

const extern struct rgb_t palette[4];

extern struct ppu_t ppu;
extern struct rgb_t framebuffer[160 * 144];

extern byte_t tiles[512][8][8];
extern struct rgb_t bg_palette[4];

void ppu_reset();
void ppu_step(uint64_t delta);

void draw();

void update_bg_palette(byte_t value);

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // PPU_H_
