#include "assembler_defines.inc"

.global TIM0_COMPA_vect, SetEnvelope

; In most Russian versions of ZX-Spectrum the CPU clock frequency was 3.5 MHz.
; It was divided by two and then fed into the AY chip, thus its base clock frequency
; was 1 750 000 Hz. BTW, the correct 44100 Hz-friendly frequency for AY would be
; 1 764 000 Hz (= 44100*40).
;
; This gives us the following formulas for the output frequency calculations:
;
; F(tone) = 1750000/(16*tone_period) = 109375/tone_period
; F(noise) = 1750000/(16*noise_period) = 109375/noise_period
; F(envelope) = 1750000/(256*env_period) = 6835.9375/env_period
;
; But each envelope period consists of 16 counts, so the effective
; envelope points change frequency is:
;
; F(env_pt) = 1750000*16/(256*env_period) = 109375/env_period

; So, basically 109375 Hz is the base AY frequency for all sound sources.
;
; Based on this, I'll chose the base output frequency for the emulator
; 43750 Hz = 109375/2.5. This will give me 366 clocks between the two timer
; interrupts.

env_samples:
    ; Each envelope sample consists of 16 bytes and represents one of four possible envelope samples
    ; the AY chip can produce

    ; Low
    .set    ENV_LOW, 0
    .byte   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

    ; High
    .set    ENV_HIGH, 16
    .byte   0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F

    ; Rise
    .set    ENV_RISE, 32
    .byte   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F

    ; Fall
    .set    ENV_FALL, 48
    .byte   0x0F, 0x0E, 0x0D, 0x0C, 0x0B, 0x0A, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00

env_seq:
    ; The envelope sequence consists of four envelope samples which are played one after
    ; another in a sequence. The first and the second samples are played just once, but the
    ; third and the fourth ones are repeated forever.

    ; Fall-Low: FLLL
    ; \___.
    .set    ENV_SEQ_FL, 0
    .byte   ENV_FALL, ENV_LOW, ENV_LOW, ENV_LOW

    ; Rise-Low: RLLL
    ; /___.
    .set    ENV_SEQ_RL, 4
    .byte   ENV_RISE, ENV_LOW, ENV_LOW, ENV_LOW

    ; Fall-Fall: FFFF
    ; \\\\.
    .set    ENV_SEQ_FF,8
    .byte   ENV_FALL, ENV_FALL, ENV_FALL, ENV_FALL

    ; Fall-Rise: FRFR
    ; \/\/.
    .set    ENV_SEQ_FR, 12
    .byte   ENV_FALL, ENV_RISE, ENV_FALL, ENV_RISE

    ; Fall-High: FHHH
    ; \---.
    .set    ENV_SEQ_FH, 16
    .byte   ENV_FALL, ENV_HIGH, ENV_HIGH, ENV_HIGH

    ; Rise-Rise: RRRR
    ; ////.
    .set    ENV_SEQ_RR, 20
    .byte   ENV_RISE, ENV_RISE, ENV_RISE, ENV_RISE

    ; Rise-High: RHHH
    ; /---.
    .set    ENV_SEQ_RH, 24
    .byte   ENV_RISE, ENV_HIGH, ENV_HIGH, ENV_HIGH

    ; Rise-Fall: RFRF
    ; /\/\.
    .set    ENV_SEQ_RF, 28
    .byte   ENV_RISE, ENV_FALL, ENV_RISE, ENV_FALL

env_mapping:
    ; Envelope mapping, maps envelope shapes to the correct envelope sequence
    .byte   ENV_SEQ_FL, ENV_SEQ_FL, ENV_SEQ_FL, ENV_SEQ_FL
    .byte   ENV_SEQ_RL, ENV_SEQ_RL, ENV_SEQ_RL, ENV_SEQ_RL
    .byte   ENV_SEQ_FF, ENV_SEQ_FL, ENV_SEQ_FR, ENV_SEQ_FH
    .byte   ENV_SEQ_RR, ENV_SEQ_RH, ENV_SEQ_RF, ENV_SEQ_RL

volume_table:
    ; For the maximum value 255 this table could look like
    ; 0, 2, 3, 4, 6, 8, 11, 16, 23, 32, 45, 64, 90, 128, 180, 255

    ; Mathematically correct table:
    ; .byte   0, 1, 1, 1, 2, 3, 4, 5, 8, 11, 15, 21, 30, 43, 60, 85

    ; Sound-optimal table
    .byte   0, 1, 2, 3, 4, 5, 7, 10, 14, 20, 27, 35, 44, 54, 65, 85

; void SetEnvelope(uint8_t shape);
SetEnvelope:
    ; R24 = shape
    andi    R24, 0x0F
    mov     ZL, R24
    clr     ZH
    subi    ZL,lo8(-(env_mapping))
    sbci    ZH,hi8(-(env_mapping))
    lpm     R24, Z

    sts     (g_ayEnvelopeSeq), R24
    sts     (g_ayEnvelopePos + 0), R1
    sts     (g_ayEnvelopePos + 1), R1
    sts     (g_ayEnvelopePos + 2), R1

    ret

TIM0_COMPA_vect:
    push    R0
    in      R0, (SREG)
    push    R0

    MPUSH   18, 21
    MPUSH   30, 31

ay_emul:
    ; *** Envelope ***
    ;
    lds     R19, (g_ayEnvelopePos + 0)
    lds     R18, (g_ayEnvelopeAdd + 0)
    add     R19, R18
    sts     (g_ayEnvelopePos + 0), R19

    lds     R19, (g_ayEnvelopePos + 1)
    lds     R18, (g_ayEnvelopeAdd + 1)
    adc     R19, R18
    sts     (g_ayEnvelopePos + 1), R19

    lds     R19, (g_ayEnvelopePos + 2)
    lds     R18, (g_ayEnvelopeAdd + 2)
    adc     R19, R18
    sts     (g_ayEnvelopePos + 2), R19
    ; 21c, R19:7-4 = Envelope sample position, carry flag is set if we need to
    ; proceed to the next sample

    lds     ZL, (g_ayEnvelopeSeq)
    brcc    env_no_next_sample

    ; Are we playing the fourth sample in the sequence?
    mov     R18, ZL
    andi    R18, 0x03
    cpi     R18, 0x03
    brne    env_not_fourth_sample

    ; Return back to #1, so it will be the third sample again after the increment
    subi    ZL, 2

env_not_fourth_sample:
    ; Increment the current sequence position
    inc     ZL
    sts     (g_ayEnvelopeSeq), ZL

env_no_next_sample:
    clr     ZH
    subi    ZL, lo8(-(env_seq))
    sbci    ZH, hi8(-(env_seq))
    ; 35c, Z = point in the envelope sequence table

    lpm     ZL, Z
    ; 38c, ZL = Current envelope sample offset

    swap    R19
    andi    R19, 0x0F
    add     ZL, R19
    clr     ZH
    subi    ZL, lo8(-(env_samples))
    sbci    ZH, hi8(-(env_samples))
    lpm     R0, Z
    ; 47c, R0 = current envelope volume


    ; *** Noise ***
    ;
    lds     R19, (g_ayNoisePos + 0)
    lds     R18, (g_ayNoiseAdd + 0)
    add     R19, R18
    sts     (g_ayNoisePos + 0), R19

    lds     R19, (g_ayNoisePos + 1)
    lds     R18, (g_ayNoiseAdd + 1)
    adc     R19, R18
    sts     (g_ayNoisePos + 1), R19

    ldi     R19, 0x00
    lds     R18, (g_ayNoiseAdd + 2)
    adc     R19, R18
    ; 65c, R19 = how many noise bits we must shift, maximum value is 3.
    ; But since we are making a white noise here, i.e. a fully random sequence, there is
    ; no point to shift more than one bit at a time

	lds		R20, (g_ayNoiseRndState + 0)
    breq    ay_no_noise_shift
    ; 68c

	lsl		R20

	lds		R21, (g_ayNoiseRndState + 1)
	rol		R21
	sts		(g_ayNoiseRndState + 1), R21

	lds		R21, (g_ayNoiseRndState + 2)
	rol		R21
	sts		(g_ayNoiseRndState + 2), R21

	lds		R21, (g_ayNoiseRndState + 3)
	rol		R21
	sts		(g_ayNoiseRndState + 3), R21

	// We need to XNOR bits 4 and 7 from R21 (since R21 was already shifted left)
	bst		R21, 4
	bld		R20, 0
    bst     R21, 7
    bld     R21, 0
	com		R21
	andi	R21, 0x01
	eor		R20, R21
	sts		(g_ayNoiseRndState + 0), R20
    ; +25c

ay_no_noise_shift:
    ; 93c, R20:0 = noise bit

    
    ; *** Tone A ***
    ;
    lds     R19, (g_ayToneAPos + 0)
    lds     R18, (g_ayToneAAdd + 0)
    add     R19, R18
    sts     (g_ayToneAPos + 0), R19

    lds     R19, (g_ayToneAPos + 1)
    lds     R18, (g_ayToneAAdd + 1)
    adc     R19, R18
    sts     (g_ayToneAPos + 1), R19

    lds     R19, (g_ayToneAPos + 2)
    lds     R18, (g_ayToneAAdd + 2)
    adc     R19, R18
    sts     (g_ayToneAPos + 2), R19
    ; +21c, R19:7 = Tone A bit

    lds     ZL, (g_ayVolumeA)
    sbrc    ZL, 4
    mov     ZL, R0
    ; +4c, ZL = Either volume A or the envelope volume

    lds     R18, (g_ayMixer)
    bst     R19, 7
    bld     R19, 0
    ; R19:0 = Tone A bit
    or      R19, R18
    ; R19:0 = Tone A bit | Tone A enable
    sbrs    R19, 0
    clr     ZL
    ; +7c

    bst     R18, 3
    bld     R18, 0
    ; R18:0 = Noise A enable
    or      R18, R20
    ; R18:0 = Noise bit | Noise A enable
    sbrs    R18, 0
    clr     ZL
    ; +5c, ZL = Final A volume

    clr     ZH
    subi    ZL, lo8(-(volume_table))
    sbci    ZH, hi8(-(volume_table))
    lpm     R21, Z
    ; +6c, R21 = PWM value for the A channel
    ; +43c = 136c total

    ; *** Tone B ***
    ;
    lds     R19, (g_ayToneBPos + 0)
    lds     R18, (g_ayToneBAdd + 0)
    add     R19, R18
    sts     (g_ayToneBPos + 0), R19

    lds     R19, (g_ayToneBPos + 1)
    lds     R18, (g_ayToneBAdd + 1)
    adc     R19, R18
    sts     (g_ayToneBPos + 1), R19

    lds     R19, (g_ayToneBPos + 2)
    lds     R18, (g_ayToneBAdd + 2)
    adc     R19, R18
    sts     (g_ayToneBPos + 2), R19
    ; +21c, R19:7 = Tone B bit

    lds     ZL, (g_ayVolumeB)
    sbrc    ZL, 4
    mov     ZL, R0
    ; +4c, ZL = Either volume B or the envelope volume

    lds     R18, (g_ayMixer)
    bst     R19, 7
    bld     R19, 1
    ; R19:1 = Tone B bit
    or      R19, R18
    ; R19:1 = Tone B bit | Tone B enable
    sbrs    R19, 1
    clr     ZL
    ; +7c

    bst     R18, 4
    bld     R18, 0
    ; R18:0 = Noise B enable
    or      R18, R20
    ; R18:0 = Noise bit | Noise B enable
    sbrs    R18, 0
    clr     ZL
    ; +5c, ZL = Final A volume

    clr     ZH
    subi    ZL, lo8(-(volume_table))
    sbci    ZH, hi8(-(volume_table))
    lpm     R18, Z
    add     R21, R18
    ; +7c, R21 = PWM value for the A and B channels
    ; +44c = 180c total

    ; *** Tone C ***
    ;
    lds     R19, (g_ayToneCPos + 0)
    lds     R18, (g_ayToneCAdd + 0)
    add     R19, R18
    sts     (g_ayToneCPos + 0), R19

    lds     R19, (g_ayToneCPos + 1)
    lds     R18, (g_ayToneCAdd + 1)
    adc     R19, R18
    sts     (g_ayToneCPos + 1), R19

    lds     R19, (g_ayToneCPos + 2)
    lds     R18, (g_ayToneCAdd + 2)
    adc     R19, R18
    sts     (g_ayToneCPos + 2), R19
    ; +21c, R19:7 = Tone C bit

    lds     ZL, (g_ayVolumeC)
    sbrc    ZL, 4
    mov     ZL, R0
    ; +4c, ZL = Either volume C or the envelope volume

    lds     R18, (g_ayMixer)
    bst     R19, 7
    bld     R19, 2
    ; R19:2 = Tone C bit
    or      R19, R18
    ; R19:2 = Tone C bit | Tone C enable
    sbrs    R19, 2
    clr     ZL
    ; +7c

    bst     R18, 5
    bld     R18, 0
    ; R18:0 = Noise C enable
    or      R18, R20
    ; R18:0 = Noise bit | Noise C enable
    sbrs    R18, 0
    clr     ZL
    ; +5c, ZL = Final C volume

    clr     ZH
    subi    ZL, lo8(-(volume_table))
    sbci    ZH, hi8(-(volume_table))
    lpm     R18, Z
    add     R21, R18
    ; +7c, R21 = PWM value for the A, B and C channels
    ; +44c = 224c total

    out     (OCR1B), R21

    ; Because of the Timer0 limitations, the real interrupt frequency is 43478.261 Hz
    ; Divide it by 870 to get ~50 Hz
    lds     ZL, (g_interruptCounter + 0)
    lds     ZH, (g_interruptCounter + 1)
    adiw    Z, 1

    cpi     ZL, lo8(870)
    brne    no_counter_reset
    cpi     ZH, hi8(870)
    brne    no_counter_reset

    clr     ZL
    clr     ZH

no_counter_reset:
    sts     (g_interruptCounter + 0), ZL
    sts     (g_interruptCounter + 1), ZH

    brne    no_50_hz_call

    ; Enable interrupts to allow 50 Hz timer to run longer
    sei

    MPUSH   22,27
    rcall   Timer50Hz
    MPOP    22,27

no_50_hz_call:    

    MPOP    30, 31
    MPOP    18, 21

    pop     R0
    out     (SREG), R0
    pop     R0
    reti
