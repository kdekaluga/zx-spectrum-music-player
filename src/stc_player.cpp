#include "includes.h"

namespace player {

static const uint16_t PROGMEM g_notePeriods[] =
{
    0x0EF8, 0x0E10, 0x0D60, 0x0C80, 0x0BD8, 0x0B28, 0x0A88, 0x09F0,
    0x0960, 0x08E0, 0x0858, 0x07E0, 0x077C, 0x0708, 0x06B0, 0x0640,
    0x05EC, 0x0594, 0x0544, 0x04F8, 0x04B0, 0x0470, 0x042C, 0x03F0,
    0x03BE, 0x0384, 0x0358, 0x0320, 0x02F6, 0x02CA, 0x02A2, 0x027C,
    0x0258, 0x0238, 0x0216, 0x01F8, 0x01DF, 0x01C2, 0x01AC, 0x0190,
    0x017B, 0x0165, 0x0151, 0x013E, 0x012C, 0x011C, 0x010B, 0x00FC,
    0x00EF, 0x00E1, 0x00D6, 0x00C8, 0x00BD, 0x00B2, 0x00A8, 0x009F,
    0x0096, 0x008E, 0x0085, 0x007E, 0x0077, 0x0070, 0x006B, 0x0064,
    0x005E, 0x0059, 0x0054, 0x004F, 0x004B, 0x0047, 0x0042, 0x003F,
    0x003B, 0x0038, 0x0035, 0x0032, 0x002F, 0x002C, 0x002A, 0x0027,
    0x0025, 0x0023, 0x0021, 0x001F, 0x001D, 0x001C, 0x001A, 0x0019,
    0x0017, 0x0016, 0x0015, 0x0013, 0x0012, 0x0011, 0x0010, 0x000F,
};

void StcPlayer::DisablePlay()
{
    m_allowPlay = false;
}

void StcPlayer::Init(const uint8_t* melody)
{
    m_allowPlay = false;

    m_melodyStart = melody;

    // Melody tempo
    m_tempo = reinterpret_cast<uint8_t>(pgm_read_byte(melody++));

    m_positionArray = reinterpret_cast<const PositionArray*>(ReadPointer(melody));
    m_ornaments = reinterpret_cast<const Ornament*>(ReadPointer(melody));
    m_patterns = reinterpret_cast<const Pattern*>(ReadPointer(melody));
    m_samples = reinterpret_cast<const Sample*>(m_melodyStart + 0x1B);

    ZeroStructure(m_stateA);
    ZeroStructure(m_stateB);
    ZeroStructure(m_stateC);
    ZeroStructure(m_ay);
    m_nextPlayPosition = 0;

    m_stateA.m_ornament = m_stateB.m_ornament = m_stateC.m_ornament = FindOrnament(0);
    m_stateA.m_samplePointsLeft = m_stateB.m_samplePointsLeft = m_stateC.m_samplePointsLeft = 0xFF;

    // Set channel A pattern data pointer to an FF byte
    static const PROGMEM uint8_t ffByte = 0xFF;
    m_stateA.m_currentPatternPos = &ffByte;

    m_tickCounter = 1;
 
    m_allowPlay = true;
}

const uint8_t* StcPlayer::ReadPointer(const uint8_t*& p)
{
    uint16_t offset = pgm_read_word(p);
    p += 2;
    return m_melodyStart + offset;
}

const StcPlayer::Ornament* StcPlayer::FindOrnament(uint8_t number)
{
    for (const Ornament* p = m_ornaments;; ++p)
        if (pgm_read_byte(&p->m_id) == number)
            return p;
}

const StcPlayer::Pattern* StcPlayer::FindPattern(uint8_t number)
{
    for (const Pattern* p = m_patterns;; ++p)
        if (pgm_read_byte(&p->m_id) == number)
            return p;
}

const StcPlayer::Sample* StcPlayer::FindSample(uint8_t number)
{
    for (const Sample* p = m_samples;; ++p)
        if (pgm_read_byte(&p->m_id) == number)
            return p;
}

void StcPlayer::GetNextPattern()
{
    const Position* position = &m_positionArray->m_positions[m_nextPlayPosition];

    m_patternTransposition = pgm_read_byte(&position->m_transposition);
    const Pattern* pattern = FindPattern(pgm_read_byte(&position->m_patternNumber));

    m_stateA.m_currentPatternPos = m_melodyStart + pgm_read_word(&pattern->m_ofsDataA);
    m_stateB.m_currentPatternPos = m_melodyStart + pgm_read_word(&pattern->m_ofsDataB);
    m_stateC.m_currentPatternPos = m_melodyStart + pgm_read_word(&pattern->m_ofsDataC);

    // Increment position (strange arithmetics)
    if (++m_nextPlayPosition >= pgm_read_byte(&m_positionArray->m_count) + 1)
        m_nextPlayPosition = 0;
}

void StcPlayer::ReadPatternCommand(ChannelState& ch)
{
    for (;;)
    {
        uint8_t data = pgm_read_byte(ch.m_currentPatternPos++);

        // Note
        if (data < 0x60)
        {
            ch.m_currentNote = data;
            ch.m_samplePosition = 0;
            ch.m_samplePointsLeft = 0x20;
            return;
        }

        // Sample
        if (data < 0x70)
        {
            ch.m_sample = FindSample(data - 0x60);
            continue;
        }

        // Ornament
        if (data < 0x80)
        {
            ch.m_ornament = FindOrnament(data - 0x70);
            ch.m_envelopeState = 0;
            continue;
        }

        // Mute
        if (data == 0x80)
        {
            ch.m_samplePointsLeft = 0xFF;
            return;
        }

        // Empty note
        if (data == 0x81)
        {
            return;
        }

        // Disable envelope and ornament
        if (data == 0x82)
        {
            ch.m_ornament = FindOrnament(0);
            ch.m_envelopeState = 0;
            continue;
        }

        // Envelope
        if (data < 0x8F)
        {
            m_ay.m_envelopeShape = data - 0x80;
            m_ay.m_envelopePeriod = static_cast<uint16_t>(pgm_read_byte(ch.m_currentPatternPos++));
            ch.m_envelopeState = 0x01;
            ch.m_ornament = FindOrnament(0);
            continue;
        }

        // 0xA1 .. 0xFE, new delay
        ch.m_newNoteCounter = ch.m_newNoteCounterStartValue = static_cast<int8_t>(data - 0xA1);
    }
}

uint8_t StcPlayer::UpdateSamplePosition(ChannelState& ch)
{
    if (ch.m_samplePointsLeft == 0xFF)
        return 0;

    if (--ch.m_samplePointsLeft)
    {
        uint8_t position = ch.m_samplePosition;
        ch.m_samplePosition = (ch.m_samplePosition + 1) & 0x1F;
        return position;
    }

    uint8_t position = pgm_read_byte(&ch.m_sample->m_repeatPosition);
    if (!position)
    {
        ch.m_samplePointsLeft = 0xFF;
        return 0;
    }

    ch.m_samplePosition = position & 0x1F;
    ch.m_samplePointsLeft = pgm_read_byte(&ch.m_sample->m_repeatLength) + 1;
    return position - 1;
}

int16_t StcPlayer::DecodeSample(const SamplePoint* data, uint8_t& mixerFlags, uint8_t& volume)
{
    uint8_t b0 = pgm_read_byte(&data->m_byte0);
    uint8_t b1 = pgm_read_byte(&data->m_byte1);
    uint8_t b2 = pgm_read_byte(&data->m_byte2);
    
    mixerFlags = 0;
    if (b1 & 0x80)
        mixerFlags = 0x20;
    else
        m_ay.m_noisePeriod = b1 & 0x1F;

    if (b1 & 0x40)
        mixerFlags |= 0x04;

    volume = b0 & 0x0F;

    int16_t pitchShift = (static_cast<int16_t>(b0 & 0xF0) << 4) | b2;
    return (b1 & 0x20) ? pitchShift : -pitchShift;
}

void StcPlayer::UpdateChannelSound(ChannelState& ch, uint16_t& tonePeriod, uint8_t& volume)
{
    m_ay.m_mixer >>= 1;
    uint8_t samplePosition = UpdateSamplePosition(ch);
    if (ch.m_samplePointsLeft == 0xFF)
    {
        volume = 0;
        return;
    }

    uint8_t mixerFlags;
    // The following line should be like this:
    // int16_t pitchShift = DecodeSample(&ch.m_sample->m_points[samplePosition], mixerFlags, volume);
    // But for some strange reason GCC inserts a full multiplication code (__mulhi3) here!
    // Instead of just multiplying by 3. So I have to do some twists...
    const SamplePoint* samplePoint = reinterpret_cast<const SamplePoint*>(
        reinterpret_cast<const uint8_t*>(ch.m_sample) + samplePosition + samplePosition*2 + 1
    ); 
    int16_t pitchShift = DecodeSample(samplePoint, mixerFlags, volume);
    m_ay.m_mixer |= mixerFlags;

    uint8_t note = ch.m_currentNote + pgm_read_byte(&ch.m_ornament->m_data[samplePosition]) + m_patternTransposition;
    tonePeriod = pgm_read_word(&g_notePeriods[note]) + pitchShift;

    if (!ch.m_envelopeState)
        return;

    // Set AY envelope only once
    if (ch.m_envelopeState == 0x02)
        m_ay.m_envelopeShape = 0;
    else
        ch.m_envelopeState = 0x02;

    volume |= 0x10;
}

void StcPlayer::Play()
{
    if (!m_allowPlay)
        return;

    // New note?
    if (!--m_tickCounter)
    {
        m_tickCounter = m_tempo;

        if (--m_stateA.m_newNoteCounter < 0)
        {
            m_stateA.m_newNoteCounter = m_stateA.m_newNoteCounterStartValue;

            // Get next pattern
            if (pgm_read_byte(m_stateA.m_currentPatternPos) == 0xFF)
                GetNextPattern();

            ReadPatternCommand(m_stateA);
        }

        if (--m_stateB.m_newNoteCounter < 0)
        {
            m_stateB.m_newNoteCounter = m_stateB.m_newNoteCounterStartValue;
            ReadPatternCommand(m_stateB);
        }

        if (--m_stateC.m_newNoteCounter < 0)
        {
            m_stateC.m_newNoteCounter = m_stateC.m_newNoteCounterStartValue;
            ReadPatternCommand(m_stateC);
        }
    }

    m_ay.m_mixer = 0x00;
    UpdateChannelSound(m_stateA, m_ay.m_tonePeriodA, m_ay.m_volumeA);
    UpdateChannelSound(m_stateB, m_ay.m_tonePeriodB, m_ay.m_volumeB);
    UpdateChannelSound(m_stateC, m_ay.m_tonePeriodC, m_ay.m_volumeC);
}

ay::Ay38910& StcPlayer::GetAy()
{
    return m_ay;
}

bool StcPlayer::UpdateEnvelope()
{
    return m_ay.m_envelopeShape != 0;
}


} // namespace player