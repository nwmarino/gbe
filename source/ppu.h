#ifndef PPU_H_
#define PPU_H_

#include "common.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

#define PPU_MODE_HBLANK 0
#define PPU_MODE_VBLANK 1
#define PPU_MODE_OAM 2
#define PPU_MODE_VRAM 3

struct rgb_t {
    byte_t r, g, b;
};

struct ppu_t {
    byte_t scanline;
    byte_t mode;
    uint64_t tick;
};

extern struct ppu_t ppu;
extern struct rgb_t framebuffer[160 * 144];

void ppu_reset();
void ppu_step(uint64_t delta);

void draw();

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // PPU_H_
