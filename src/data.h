#pragma once

// 'var' should be defined to the empty string in one cpp unit to instantiate variables.
#ifndef var
#define var extern
#endif

#include <stdint.h>

// Global variables
var uint16_t g_interruptCounter;

// AY emulator variables
var uint8_t g_ayMixer;

var __uint24 g_ayToneAPos;
var __uint24 g_ayToneAAdd;
var uint8_t g_ayVolumeA;

var __uint24 g_ayToneBPos;
var __uint24 g_ayToneBAdd;
var uint8_t g_ayVolumeB;

var __uint24 g_ayToneCPos;
var __uint24 g_ayToneCAdd;
var uint8_t g_ayVolumeC;

var __uint24 g_ayEnvelopePos;
var __uint24 g_ayEnvelopeAdd;
var uint8_t g_ayEnvelopeSeq;

var uint16_t g_ayNoisePos;
var __uint24 g_ayNoiseAdd;
var uint32_t g_ayNoiseRndState;
