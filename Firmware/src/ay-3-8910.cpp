#include "includes.h"

namespace ay {

extern "C" void SetEnvelope(uint8_t shape);

void UpdateEmulatorParameters(const Ay38910& ay, bool updateEnvelope)
{
    // Tone constant: 109375/43750*2^24 = 41943040
    constexpr uint32_t toneConst = 41943040u;
    uint32_t toneAddA = toneConst/static_cast<uint32_t>(ay.m_tonePeriodA ? ay.m_tonePeriodA : 1);
    uint32_t toneAddB = toneConst/static_cast<uint32_t>(ay.m_tonePeriodB ? ay.m_tonePeriodB : 1);
    uint32_t toneAddC = toneConst/static_cast<uint32_t>(ay.m_tonePeriodC ? ay.m_tonePeriodC : 1);

    // Noise constant: 109375/43750*2^16 = 163840
    constexpr uint32_t noiseConst = 163840u;
    uint32_t noiseAdd = noiseConst/static_cast<uint32_t>(ay.m_noisePeriod ? ay.m_noisePeriod : 1);

    // Envelope constant: 109375/43750*2^20 = 2621440
    constexpr uint32_t envConst = 2621440u;
    uint32_t envAdd = envConst/static_cast<uint32_t>(ay.m_envelopePeriod ? ay.m_envelopePeriod : 1);

    // Update emulator params all at once
    cli();
    g_ayMixer = ay.m_mixer;

    g_ayToneAAdd = static_cast<__uint24>(toneAddA);
    g_ayVolumeA = ay.m_volumeA;

    g_ayToneBAdd = static_cast<__uint24>(toneAddB);
    g_ayVolumeB = ay.m_volumeB;

    g_ayToneCAdd = static_cast<__uint24>(toneAddC);
    g_ayVolumeC = ay.m_volumeC;

    g_ayNoiseAdd = static_cast<__uint24>(noiseAdd);

    if (updateEnvelope)
    {
        g_ayEnvelopeAdd = static_cast<__uint24>(envAdd);
        SetEnvelope(ay.m_envelopeShape);
    }

    sei();
}

} // namespace ay
