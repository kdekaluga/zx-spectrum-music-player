#pragma once

#include <stdint.h>

namespace player {

class StcPlayer
{
public:
    void DisablePlay();
    void Init(const uint8_t* melody);
    void Play();
    ay::Ay38910& GetAy();
    bool UpdateEnvelope();

private:
    struct Position
    {
        uint8_t m_patternNumber;
        uint8_t m_transposition;
    };

    struct PositionArray
    {
        uint8_t m_count;
        Position m_positions[];
    };

    struct Ornament
    {
        uint8_t m_id;
        uint8_t m_data[32];
    };

    struct Pattern
    {
        uint8_t m_id;
        uint16_t m_ofsDataA;
        uint16_t m_ofsDataB;
        uint16_t m_ofsDataC;
    };

    struct SamplePoint
    {
        // 0-3: volume
        // 4-7: pitch bits 8-11
        uint8_t m_byte0;

        // 7: noise mask (0 = enabled)
        // 6: tone mask (0 = enabled)
        // 5: pitch shift direction (1 = +, 0 = -)
        // 0-4: noise frequency
        uint8_t m_byte1;

        // pitch bits 0-7
        uint8_t m_byte2;

        // Returns pitch shift and fills other variables
        int16_t Decode(uint8_t& outputFlags, uint8_t& noisePeriod, uint8_t& volume);
    };

    struct Sample
    {
        uint8_t m_id;
        SamplePoint m_points[32];
        uint8_t m_repeatPosition; // 0 == Don't repeat
        uint8_t m_repeatLength;
    };

// Uncomment the following lines to get a compilation error with the charger profile size
//template<size_t s> class PrintSize;
//PrintSize<sizeof(SamplePoint)> printSize;

    struct ChannelState
    {
        int8_t m_newNoteCounter;
        int8_t m_newNoteCounterStartValue;

        const uint8_t* m_currentPatternPos;
        const Ornament* m_ornament;
        const Sample* m_sample;

        // 0xFF = disabled
        uint8_t m_samplePointsLeft;
        uint8_t m_samplePosition;

        uint8_t m_currentNote;
        uint8_t m_envelopeState;
    };

    volatile bool m_allowPlay;

    // Pointers
    const uint8_t* m_melodyStart;
    const PositionArray* m_positionArray;
    const Ornament* m_ornaments;
    const Pattern* m_patterns;
    const Sample* m_samples;
    
    // Music tempo in 50Hz ticks
    uint8_t m_tempo;

    // Current 50Hz tick counter
    uint8_t m_tickCounter;

    uint8_t m_nextPlayPosition;

    uint8_t m_patternTransposition;

    ChannelState m_stateA;
    ChannelState m_stateB;
    ChannelState m_stateC;

    ay::Ay38910 m_ay;

private:
    const uint8_t* ReadPointer(const uint8_t*& p);
    const Ornament* FindOrnament(uint8_t number);
    const Pattern* FindPattern(uint8_t number);
    const Sample* FindSample(uint8_t number);

    void GetNextPattern();
    void ReadPatternCommand(ChannelState& ch);

    // Returns current sample position
    uint8_t UpdateSamplePosition(ChannelState& ch);

    // Decodes the provided sample and possibly sets noise period. Returns pitch shift
    int16_t DecodeSample(const SamplePoint* data, uint8_t& mixerFlags, uint8_t& volume);

    void UpdateChannelSound(ChannelState& ch, uint16_t& tonePeriod, uint8_t& volume);
};


} // namespace player