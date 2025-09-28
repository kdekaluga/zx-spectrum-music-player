#pragma once

#define LOBYTE(n) ((n) & 0xFF)
#define HIBYTE(n) ((n) >> 8)
#define COUNTOF(n) (sizeof(n)/sizeof(n[0]))

#include <string.h>
#include <stdint.h>
#include <avr/pgmspace.h>

#include "common.h"
#include "data.h"
#include "ay-3-8910.h"
#include "stc_player.h"

extern "C" void Timer50Hz();

template <typename T>
void ZeroStructure(T& p)
{
    memset(&p, 0, sizeof(T));
}
