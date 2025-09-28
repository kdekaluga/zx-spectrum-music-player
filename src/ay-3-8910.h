#pragma once

#include <stdint.h>

namespace ay {

// AY-3-8910 registers
struct Ay38910
{
    uint16_t m_tonePeriodA;
    uint16_t m_tonePeriodB;
    uint16_t m_tonePeriodC;

    uint8_t m_noisePeriod;

    uint8_t m_mixer;

    uint8_t m_volumeA;
    uint8_t m_volumeB;
    uint8_t m_volumeC;

    uint16_t m_envelopePeriod;
    uint8_t m_envelopeShape;
};

void UpdateEmulatorParameters(const Ay38910& ay, bool updateEnvelope);

} // namespace ay

