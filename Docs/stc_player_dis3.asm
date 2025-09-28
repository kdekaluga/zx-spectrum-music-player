            EXT1        EQU 16F1h
            EXT2        EQU 0A202h
            EXT3        EQU 0AC01h
            EXT4        EQU 0EE5Eh
            EXT5        EQU 0F4F3h


                ORG 2700h
2700:           V_2700: DS 2          ; Variable at address 2700h (2 bytes)

                ORG 2F00h
2F00:           V_2F00: DS 1          ; Variable at address 2F00h (1 byte)

                ORG 0C04Ch
C04C: 01 FD FF        LD BC, 0FFFDh   ; Load BC with FFFDh (I/O port address for sound chip)
C04F: 3E 0C           LD A, 0Ch       ; Load A with 0Ch (sound chip register number)
C051: ED 79           OUT (C), A      ; Select sound chip register 12
C053: AF              XOR A           ; Clear A (value 0)
C054: 06 BF           LD B, 0BFh      ; Load B with BFh (sound chip data port)
C056: C3 95 C4        JP LBL30        ; Jump to initialization routine

C059: 21 A8 C4        LD HL, 0C4A8h   ; Load HL with address C4A8h
C05C: C3 62 C0        JP LBL1         ; Jump to main entry point

C05F: C3 9D C1        JP LBL5         ; Jump to playback routine

            LBL1:                     ; Main initialization entry point
C062: F3              DI              ; Disable interrupts
C063: 7E              LD A, (HL)      ; Load A from (HL) - get tempo value
C064: 32 D1 C0        LD (VAR4), A    ; Store tempo in VAR4
C067: 22 14 C1        LD (SMC5+1), HL ; Self-modifying code: store HL in SUB3
C06A: 23              INC HL          ; Increment HL to next data pointer

; Positions
C06B: CD 0E C1        CALL SUB2       ; Calculate address from pointer table
C06E: 1A              LD A, (DE)      ; Load A from calculated address
C06F: 13              INC DE          ; Increment DE
C070: 3C              INC A           ; Increment A
C071: 32 D3 C0        LD (VAR5), A    ; Store pattern count in VAR5
C074: ED 53 C9 C0     LD (VAR1), DE   ; Store pattern table pointer in VAR1

; Ornaments
C078: CD 0E C1        CALL SUB2       ; Calculate next address
C07B: ED 53 CB C0     LD (VAR2), DE   ; Store instrument table pointer in VAR2
C07F: D5              PUSH DE         ; Save DE on stack (DE = ornaments pointer + 1)

; Patterns
C080: CD 0E C1        CALL SUB2       ; Calculate next address
C083: ED 53 CD C0     LD (VAR3), DE   ; Store sequence table pointer in VAR3

; Samples
C087: 21 1B 00        LD HL, 001Bh    ; Load HL with 001Bh (offset value)
C08A: CD 13 C1        CALL SUB3       ; Add offset to base address
C08D: EB              EX DE, HL       ; Exchange DE and HL
C08E: 22 CF C0        LD (SMC1), HL   ; Store note table address

; Set Channel A pattern data pointer to an 0xFF byte
C091: 21 DA C0        LD HL, REF1     ; Load HL with reference address
C094: 22 D4 C0        LD (VAR6), HL   ; Initialize VAR6

; Reset channels (3*10), AY state (14) and current positon (1) = 45 bytes
C097: 21 DB C0        LD HL, REF2     ; Load HL with reference address
C09A: 11 DC C0        LD DE, REF3     ; Load DE with reference address
C09D: 01 2C 00        LD BC, 002Ch    ; Load BC with 44 bytes to clear
C0A0: 70              LD (HL), B      ; Clear memory area (B=0 from previous LD BC)
C0A1: ED B0           LDIR            ; Clear 44 bytes of channel data

; Find ornament #0
C0A3: E1              POP HL          ; Restore instrument table pointer (Ornaments + 1)
C0A4: 01 21 00        LD BC, 0021h    ; Load BC with 33 (instrument record size)
C0A7: AF              XOR A           ; Clear A (search for value 0)
C0A8: CD 08 C1        CALL SUB1       ; Search for end of instrument table
; HL = ornament #0

; Set channel states to 255
C0AB: 3D              DEC A           ; Decrement A to FFh
C0AC: 32 E4 C0        LD (VAR10), A   ; Initialize channel 1 state
C0AF: 32 EE C0        LD (VAR12), A   ; Initialize channel 2 state
C0B2: 32 F8 C0        LD (0C0F8h), A  ; Initialize channel 3 state

; Set tick counter to 1
C0B5: 3E 01           LD A, 01h       ; Load A with 1
C0B7: 32 D2 C0        LD (SMC2), A    ; Initialize tempo counter

; Store channel ornament pointers + 1
C0BA: 23              INC HL          ; Move to first instrument data
C0BB: 22 E2 C0        LD (VAR9), HL   ; Store channel 1 instrument pointer
C0BE: 22 EC C0        LD (VAR11), HL  ; Store channel 2 instrument pointer
C0C1: 22 F6 C0        LD (VAR14), HL  ; Store channel 3 instrument pointer

; Output zeros to AY
C0C4: CD 78 C4        CALL SUB12      ; Initialize sound chip
C0C7: FB              EI              ; Enable interrupts
C0C8: C9              RET             ; Return

            VAR1:                     ; Pattern table pointer
C0C9: 77 F1           DW 0F177h

            VAR2:                     ; Instrument table pointer
C0CB: 8F F1           DW 0F18Fh

            VAR3:                     ; Sequence table pointer
C0CD: 13 F2           DW 0F213h

            SMC1:                     ; Self-modifying code location
C0CF: 5E EE           DW 0EE5Eh
            VAR4:                     ; Tempo value
C0D1: 06              DB 06h          ; Current tempo counter
            SMC2:
C0D2: 03              DB 03h
            VAR5:                     ; Pattern count
C0D3: 0C              DB 0Ch

            VAR6:                     ; Current pattern pointer
C0D4: B1 F4           DW 0F4B1h

            VAR7:                     ; Channel 3 pattern pointer
C0D8: 35 F5           DW 0F535h

            REF1:                     ; Reference data
C0DA: FF              RST 38h

            REF2:
C0DB: 00              NOP

            REF3:
C0DC: 00              NOP

            REF4:                     ; Channel 1 data structure start
C0DD: 04              INC B
C0DE: 24              INC H
C0DF: 00              NOP

            VAR8:                     ; Channel 1 instrument pointer
C0E0: 88 EF           DW 0EF88h

            VAR9:                     ; Channel 1 pattern pointer
C0E2: 90 F1           DW 0F190h

            VAR10:                    ; Channel 1 state/volume
C0E4: 1C              DB 1Ch

C0E5: 02              LD (BC), A
C0E6: 01 04 1A        LD BC, 1A04h    ; Channel 1 data structure

            SMC4:                     ; Channel 2 instrument pointer
C0E9: 01 4E F0        LD BC, 0F04Eh

            VAR11:                    ; Channel 2 pattern pointer
C0EC: 90 F1           DW 0F190h

            VAR12:                    ; Channel 2 state/volume
C0EE: 1C              DB 1Ch

C0EF: 00              NOP
C0F0: 01 0A 35        LD BC, 350Ah    ; Channel 2 data structure

            VAR13:                    ; Channel 3 instrument pointer
C0F3: 00              NOP
C0F4: B1 F0           DW 0F0B1h

            VAR14:                    ; Channel 3 pattern pointer
C0F6: D2 F1           DW 0F1D2h

            VAR15:                    ; Current pattern number
C0F9: 05              DB 05h

            VAR16:                    ; Channel 1 frequency
C0FA: DF 09           DW 09DFh

            VAR17:                    ; Channel 2 frequency
C0FC: 58 03           DW 0358h

            VAR18:                    ; Channel 3 frequency
C0FE: 68 01           DW 0168h

C102:           REF5:                 ; Channel 1 output value
C102: 0B              DEC BC

            REF6:                     ; Channel 2 output value
C103: 1A              LD A, (DE)

            REF7:                     ; Channel 3 output value
C104: 07              RLCA

            VAR19:                    ; Temporary storage
C105: D9              DB 0D9h
C106: 00              NOP

            VAR20:                    ; Noise/enable flags
C107: 00              DB 00h

; Find sample/pattern/ornament
            SUB1:                     ; Search memory for value in A
C108: BE              CP (HL)         ; Compare with (HL)
C109: C8              RET Z           ; Return if found
C10A: 09              ADD HL, BC      ; Add BC to HL (next record)
C10B: C3 08 C1        JP SUB1         ; Continue searching

; Read offset from [HL] and add it to the melody start address
            SUB2:                     ; Calculate address from pointer table
C10E: 5E              LD E, (HL)      ; Load low byte from (HL)
C10F: 23              INC HL          ; Increment HL
C110: 56              LD D, (HL)      ; Load high byte from (HL)
C111: 23              INC HL          ; Increment HL
C112: EB              EX DE, HL       ; Exchange DE and HL

; Add HL with the melody start address
            SUB3:                     ; Add offset to base address
            SMC5:                     ; Self-modifying code location
C113: 01 43 EE        LD BC, 0EE43h   ; Load offset value
C116: 09              ADD HL, BC      ; Add offset to HL
C117: EB              EX DE, HL       ; Exchange back
C118: C9              RET             ; Return

; Decode sample point
; IX = Sample pointer + 1
; A = Sample position
; Returns:
; B = 00h if tone enabled, 02h otherwise
; C = 00h if noise enabled, 10h otherwise
; DE = pitch shift, 0XXXh = negative, 1XXXh = positive
; H = Noise period
; L = Volume
            SUB4:                     ; Process instrument data
C119: 16 00           LD D, 00h       ; Clear D
C11B: 5F              LD E, A         ; Copy A to E
C11C: 87              ADD A, A        ; Multiply A by 3 (A*2)
C11D: 83              ADD A, E        ; A*3
C11E: 5F              LD E, A         ; Store result in E
C11F: DD 19           ADD IX, DE      ; Add offset to IX
; IX = Sample data[position]
C121: DD 7E 01        LD A, (IX+01h)  ; Load instrument parameter
C124: CB 7F           BIT 7, A        ; Test bit 7 (noise flag)
C126: 0E 10           LD C, 10h       ; Load C with noise enable value
C128: C2 2C C1        JP NZ, LBL2     ; Jump if noise enabled
C12B: 4A              LD C, D         ; Clear C (no noise)
C12C:           LBL2:
C12C: CB 77           BIT 6, A        ; Test bit 6 (tone enable flag)
C12E: 06 02           LD B, 02h       ; Load B with tone enable value
C130: C2 34 C1        JP NZ, LBL3     ; Jump if tone enabled
C133: 42              LD B, D         ; Clear B (no tone)
C134:           LBL3:
C134: E6 1F           AND 1Fh         ; Mask volume bits (0-31)
C136: 67              LD H, A         ; Store volume in H
C137: DD 5E 02        LD E, (IX+02h)  ; Load envelope parameter
C13A: DD 7E 00        LD A, (IX+00h)  ; Load base note
C13D: F5              PUSH AF         ; Save AF
C13E: E6 F0           AND 0F0h        ; Mask octave bits
C140: 0F              RRCA            ; Shift octave to lower nibble
C141: 0F              RRCA
C142: 0F              RRCA
C143: 0F              RRCA
C144: 57              LD D, A         ; Store octave in D
C145: F1              POP AF          ; Restore AF
C146: E6 0F           AND 0Fh         ; Mask note bits
C148: 6F              LD L, A         ; Store note in L
C149: DD CB 01 6E     BIT 5, (IX+01h) ; Test bit 5 (effect flag)
C14D: C8              RET Z           ; Return if no effect
C14E: CB E2           SET 4, D        ; Set effect flag in D
C150: C9              RET             ; Return

; Get next pattern
            SUB5:                     ; Get next pattern data
C151: 3A F9 C0        LD A, (VAR15)   ; Load current pattern number
C154: 4F              LD C, A         ; Store in C
C155: 21 D3 C0        LD HL, VAR5     ; Load pattern count address
C158: BE              CP (HL)         ; Compare with total patterns
C159: DA 5E C1        JP C, LBL4      ; Jump if within range
C15C: AF              XOR A           ; Clear A (reset to pattern 0)
C15D: 4F              LD C, A         ; Store 0 in C
C15E:           LBL4:
C15E: 3C              INC A           ; Increment pattern number
C15F: 32 F9 C0        LD (VAR15), A   ; Store next pattern number
C162: 69              LD L, C         ; Pattern number to L
C163: 26 00           LD H, 00h       ; Clear H
C165: 29              ADD HL, HL      ; Multiply by 2 (word index)
C166: ED 5B C9 C0     LD DE, (VAR1)   ; Load pattern table pointer
C16A: 19              ADD HL, DE      ; Calculate pattern address
; HL = Position pointer

; Pattern number
C16B: 4E              LD C, (HL)      ; Load pattern data low byte
C16C: 23              INC HL          ; Increment to high byte

; Transposition
C16D: 7E              LD A, (HL)      ; Load pattern data high byte
C16E: 32 9D C3        LD (SMC6+1), A  ; Store for later use

; Find pattern
C171: 79              LD A, C         ; Pattern number to A
C172: 2A CD C0        LD HL, (VAR3)   ; Load sequence table pointer
C175: 01 07 00        LD BC, 0007h    ; Sequence record size
C178: CD 08 C1        CALL SUB1       ; Find pattern in sequence

C17B: 23              INC HL          ; Move to pattern data pointer

; Set channels pattern data pointers
C17C: CD 0E C1        CALL SUB2       ; Calculate pattern address
C17F: ED 53 D4 C0     LD (VAR6), DE   ; Store pattern pointer
C183: CD 0E C1        CALL SUB2       ; Calculate channel 2 pattern address
C186: ED 53 D6 C0     LD (SMC3+1), DE ; Store channel 2 pattern pointer
C18A: CD 0E C1        CALL SUB2       ; Calculate channel 3 pattern address
C18D: ED 53 D8 C0     LD (VAR7), DE   ; Store channel 3 pattern pointer
C191: C9              RET             ; Return

; Decrement channel note counter
            SUB6:                     ; Decrement pattern delay counter
C192: DD 35 02        DEC (IX+02h)    ; Decrement delay counter
C195: F0              RET P           ; Return if positive or zero
C196: DD 7E FF        LD A, (IX-01h)  ; Load original delay value
C199: DD 77 02        LD (IX+02h), A  ; Reset delay counter
C19C: C9              RET             ; Return


; Playback
            LBL5:                     ; Main playback routine
C19D: 3A D2 C0        LD A, (SMC2)    ; Load tempo counter
C1A0: 3D              DEC A           ; Decrement counter
C1A1: 32 D2 C0        LD (SMC2), A    ; Store updated counter
C1A4: C2 E7 C2        JP NZ, LBL19    ; Jump if not time for new note

; Set tick counter
C1A7: 3A D1 C0        LD A, (VAR4)    ; Load tempo value
C1AA: 32 D2 C0        LD (SMC2), A    ; Reset tempo counter

; A
C1AD: DD 21 DD C0     LD IX, REF4     ; Point to channel 1 data

; Decrement note counter
C1B1: CD 92 C1        CALL SUB6       ; Update channel 1 delay
C1B4: F2 C8 C1        JP P, LBL6      ; Jump if delay not expired

; Check for channel A pattern end
C1B7: 2A D4 C0        LD HL, (VAR6)   ; Load channel 1 pattern pointer
C1BA: 7E              LD A, (HL)      ; Load pattern byte
C1BB: 3C              INC A           ; Test for FFh (end of pattern)
C1BC: CC 51 C1        CALL Z, SUB5    ; If FF, get next pattern

; Process pattern data
C1BF: 2A D4 C0        LD HL, (VAR6)   ; Reload pattern pointer
C1C2: CD F1 C1        CALL SUB7       ; Process pattern data
C1C5: 22 D4 C0        LD (VAR6), HL   ; Store updated pointer

; B
C1C8:           LBL6:
C1C8: DD 21 E7 C0     LD IX, 0C0E7h   ; Point to channel 2 data
C1CC: CD 92 C1        CALL SUB6       ; Update channel 2 delay
C1CF: F2 DB C1        JP P, LBL7      ; Jump if delay not expired

C1D2: 2A D6 C0        LD HL, (SMC3+1) ; Load channel 2 pattern pointer
C1D5: CD F1 C1        CALL SUB7       ; Process pattern data
C1D8: 22 D6 C0        LD (SMC3+1), HL ; Store updated pointer
C1DB:           LBL7:
C1DB: DD 21 F1 C0     LD IX, 0C0F1h   ; Point to channel 3 data
C1DF: CD 92 C1        CALL SUB6       ; Update channel 3 delay
C1E2: F2 E7 C2        JP P, LBL19     ; Jump if delay not expired

C1E5: 2A D8 C0        LD HL, (VAR7)   ; Load channel 3 pattern pointer
C1E8: CD F1 C1        CALL SUB7       ; Process pattern data
C1EB: 22 D8 C0        LD (VAR7), HL   ; Store updated pointer
C1EE: C3 E7 C2        JP LBL19        ; Jump to note processing

; Process pattern data
            SUB7:                     ; Process pattern commands
C1F1: 7E              LD A, (HL)      ; Load pattern byte
C1F2: FE 60           CP 60h          ; Compare with 60h (note range)
C1F4: DA 1F C2        JP C, LBL8      ; Jump if note (00-5F)

C1F7: FE 70           CP 70h          ; Compare with 70h
C1F9: DA 2C C2        JP C, LBL10     ; Jump if instrument change (60-6F)

C1FC: FE 80           CP 80h          ; Compare with 80h
C1FE: DA 4D C2        JP C, LBL14     ; Jump if volume change (70-7F)

C201: CA 44 C2        JP Z, LBL11     ; Jump if 80h (end of pattern)

C204: FE 81           CP 81h          ; Compare with 81h
C206: CA 2A C2        JP Z, LBL9      ; Jump if 81h (rest/note off)

C209: FE 82           CP 82h          ; Compare with 82h
C20B: CA 4A C2        JP Z, LBL13     ; Jump if 82h (arpeggio setup)

C20E: FE 8F           CP 8Fh          ; Compare with 8Fh
C210: DA 69 C2        JP C, LBL16     ; Jump if 83-8Eh (effect commands)

C213: D6 A1           SUB 0A1h        ; Adjust for delay values
C215: DD 77 02        LD (IX+02h), A  ; Set pattern delay
C218: DD 77 FF        LD (IX-01h), A  ; Set initial delay value
C21B: 23              INC HL          ; Move to next byte
C21C: C3 F1 C1        JP SUB7         ; Process next command


; New note
            LBL8:                     ; Process note (00-5F)
C21F: DD 77 01        LD (IX+01h), A  ; Store note number
C222: DD 36 00 00     LD (IX+00h), 00h ; Clear octave
C226: DD 36 07 20     LD (IX+07h), 20h ; Set envelope duration

; No note
C22A:           LBL9:                 ; Rest/note off or continue
C22A: 23              INC HL          ; Move to next pattern byte
C22B: C9              RET             ; Return


; New sample
            LBL10:                    ; Instrument change (60-6F)
C22C: D6 60           SUB 60h         ; Convert to instrument number (0-15)
C22E: E5              PUSH HL         ; Save HL
C22F: 01 63 00        LD BC, 0063h    ; Instrument record size
C232: 2A CF C0        LD HL, (SMC1)   ; Load instrument table base
C235: CD 08 C1        CALL SUB1       ; Find instrument
C238: 23              INC HL          ; Move to instrument data
C239: DD 75 03        LD (IX+03h), L  ; Store instrument pointer low
C23C: DD 74 04        LD (IX+04h), H  ; Store instrument pointer high
C23F: E1              POP HL          ; Restore HL
C240: 23              INC HL          ; Move to next pattern byte
C241: C3 F1 C1        JP SUB7         ; Process next command

; Mute
            LBL11:                    ; End of pattern (80h)
C244: 23              INC HL          ; Move to next byte (should be FF)

; Mute after sample end
C245:           LBL12:                ; Note off/silence
C245: DD 36 07 FF     LD (IX+07h), 0FFh ; Disable envelope
C249: C9              RET             ; Return


; Disable ornament and envelope
            LBL13:                    ; Arpeggio setup (82h)
C24A: AF              XOR A           ; Clear A (arpeggio type)
C24B: 18 02           JR LBL15        ; Jump to common handler

; Set ornament (disable envelope)
            LBL14:                    ; Volume change (70-7F)
C24D: D6 70           SUB 70h         ; Convert to volume (0-15)
C24F:           LBL15:                ; Common handler for volume/arpeggio
C24F: E5              PUSH HL         ; Save HL
C250: 01 21 00        LD BC, 0021h    ; Arpeggio record size
C253: 2A CB C0        LD HL, (VAR2)   ; Load arpeggio table pointer
C256: CD 08 C1        CALL SUB1       ; Find arpeggio data
C259: 23              INC HL          ; Move to arpeggio data
C25A: DD 75 05        LD (IX+05h), L  ; Store arpeggio pointer low
C25D: DD 74 06        LD (IX+06h), H  ; Store arpeggio pointer high
C260: DD 36 FE 00     LD (IX-02h), 00h ; Clear effect flag
C264: E1              POP HL          ; Restore HL
C265: 23              INC HL          ; Move to next pattern byte
C266: C3 F1 C1        JP SUB7         ; Process next command

; Set envelope (disable ornament)
            LBL16:                    ; Effect commands (83-8Eh)
C269: D6 80           SUB 80h         ; Convert to effect number (3-14)
C26B: 32 07 C1        LD (VAR20), A   ; Store effect number
C26E: 23              INC HL          ; Move to effect parameter
C26F: 7E              LD A, (HL)      ; Load effect parameter
C270: 23              INC HL          ; Move to next byte
C271: 32 05 C1        LD (VAR19), A   ; Store effect parameter
C274: DD 36 FE 01     LD (IX-02h), 01h ; Set effect active flag
C278: E5              PUSH HL         ; Save HL
C279: AF              XOR A           ; Clear A
C27A: 01 21 00        LD BC, 0021h    ; Arpeggio record size
C27D: 2A CB C0        LD HL, (VAR2)   ; Load arpeggio table pointer
C280: CD 08 C1        CALL SUB1       ; Find arpeggio data
C283: 23              INC HL          ; Move to arpeggio data
C284: DD 75 05        LD (IX+05h), L  ; Store arpeggio pointer low
C287: DD 74 06        LD (IX+06h), H  ; Store arpeggio pointer high
C28A: E1              POP HL          ; Restore HL
C28B: C3 F1 C1        JP SUB7         ; Process next command


; Update sample position
            SUB8:                     ; Update envelope position
C28E: DD 7E 07        LD A, (IX+07h)  ; Load envelope counter
C291: 3C              INC A           ; Test for FFh (disabled)
C292: C8              RET Z           ; Return if envelope disabled
C293: 3D              DEC A           ; Restore original value
C294: 3D              DEC A           ; Decrement envelope counter
C295: DD 77 07        LD (IX+07h), A  ; Store updated counter

C298: F5              PUSH AF         ; Save counter value
C299: DD 7E 00        LD A, (IX+00h)  ; Load envelope position
C29C: 4F              LD C, A         ; Store in C
C29D: 3C              INC A           ; Increment position
C29E: E6 1F           AND 1Fh         ; Wrap at 32 steps
C2A0: DD 77 00        LD (IX+00h), A  ; Store updated position
C2A3: F1              POP AF          ; Restore counter
C2A4: C0              RET NZ          ; Return if counter not zero

; Repeat count == 0
C2A5: DD 5E 03        LD E, (IX+03h)  ; Load instrument pointer low
C2A8: DD 56 04        LD D, (IX+04h)  ; Load instrument pointer high
C2AB: 21 60 00        LD HL, 0060h    ; Offset to envelope loop point
C2AE: 19              ADD HL, DE      ; Calculate envelope loop address
C2AF: 7E              LD A, (HL)      ; Load loop point
C2B0: 3D              DEC A           ; Test for 0 (no loop)
C2B1: FA 45 C2        JP M, LBL12     ; Jump if no loop (silence note)
C2B4: 4F              LD C, A         ; Store loop point in C
C2B5: 3C              INC A           ; Restore loop point
C2B6: E6 1F           AND 1Fh         ; Mask to 32 steps
C2B8: DD 77 00        LD (IX+00h), A  ; Set new envelope position
C2BB: 23              INC HL          ; Move to loop duration
C2BC: 7E              LD A, (HL)      ; Load loop duration
C2BD: 3C              INC A           ; Adjust duration
C2BE: DD 77 07        LD (IX+07h), A  ; Set new envelope counter
C2C1: C9              RET             ; Return


; Update noise period
            SUB9:                     ; Update channel output
C2C2: 79              LD A, C         ; Check tone enable flag
C2C3: B7              OR A            ; Test if zero
C2C4: C0              RET NZ          ; Return if tone enabled
C2C5: 7C              LD A, H         ; Load frequency high byte
C2C6: 32 00 C1        LD (0C100h), A  ; Store for noise calculation
C2C9: C9              RET             ; Return


; Correctly apply envelope
            SUB10:                    ; Apply effects to channel
C2CA: DD 7E 07        LD A, (IX+07h)  ; Check if envelope active
C2CD: 3C              INC A           ; Test for FFh
C2CE: C8              RET Z           ; Return if envelope disabled
C2CF: DD 7E FE        LD A, (IX-02h)  ; Check effect flag
C2D2: B7              OR A            ; Test if zero
C2D3: C8              RET Z           ; Return if no effect
C2D4: FE 02           CP 02h          ; Check if effect active
C2D6: CA E0 C2        JP Z, LBL17     ; Jump if effect active
C2D9: DD 36 FE 02     LD (IX-02h), 02h ; Set effect active flag
C2DD: C3 E4 C2        JP LBL18        ; Jump to effect application

            LBL17:                    ; Effect active processing
C2E0: AF              XOR A           ; Clear A
C2E1: 32 07 C1        LD (VAR20), A   ; Reset effect number
C2E4:           LBL18:                ; Apply effect to frequency
C2E4: CB E6           SET 4, (HL)     ; Set effect flag in frequency
C2E6: C9              RET             ; Return



; No new note
            LBL19:                    ; Process all channels for output
; A
; Update sample position
C2E7: DD 21 DD C0     LD IX, REF4     ; Point to channel 1 data
C2EB: CD 8E C2        CALL SUB8       ; Update channel 1 envelope
C2EE: 79              LD A, C         ; Get tone enable flag
C2EF: 32 95 C3        LD (0C395h), A  ; Store for later

; Decode channel A sample point
C2F2: DD 2A E0 C0     LD IX, (VAR8)   ; Point to channel 1 instrument
C2F6: CD 19 C1        CALL SUB4       ; Process instrument data
; B = 00h if tone enabled, 02h otherwise
; C = 00h if noise enabled, 10h otherwise
; DE = pitch shift, 0XXXh = negative, 1XXXh = positive
; H = Noise period
; L = Volume

C2F9: 79              LD A, C         ; Get tone/noise flags
C2FA: B0              OR B            ; Combine flags
C2FB: 0F              RRCA            ; Position for output
C2FC: 32 01 C1        LD (0C101h), A  ; Store mixer value
C2FF: DD 21 DD C0     LD IX, REF4     ; Point to channel 1 data
C303: DD 7E 07        LD A, (IX+07h)  ; Check if envelope active
C306: 3C              INC A           ; Test for FFh
C307: CA 13 C3        JP Z, LBL20     ; Jump if envelope disabled
; Update noise period
C30A: CD C2 C2        CALL SUB9       ; Update channel output
; Update tone period
C30D: CD 8B C3        CALL SUB11      ; Calculate frequency with arpeggio
C310: 22 FA C0        LD (VAR16), HL  ; Store channel 1 frequency
C313:           LBL20:                ; Channel 1 processing complete
C313: 21 02 C1        LD HL, REF5     ; Point to channel 1 output
C316: 77              LD (HL), A      ; Store volume output
; Apply envelope
C317: CD CA C2        CALL SUB10      ; Apply effects to channel 1

; B
C31A: DD 21 E7 C0     LD IX, 0C0E7h   ; Point to channel 2 data
C31E: CD 8E C2        CALL SUB8       ; Update channel 2 envelope
C321: DD 7E 07        LD A, (IX+07h)  ; Check if envelope active
C324: 3C              INC A           ; Test for FFh
C325: CA 48 C3        JP Z, LBL21     ; Jump if envelope disabled
C328: 79              LD A, C         ; Get tone enable flag
C329: 32 95 C3        LD (0C395h), A  ; Store for later
C32C: DD 2A EA C0     LD IX, (SMC4+1) ; Point to channel 2 instrument
C330: CD 19 C1        CALL SUB4       ; Process instrument data
C333: 3A 01 C1        LD A, (0C101h)  ; Get current mixer value
C336: B1              OR C            ; Add channel 2 tone flag
C337: B0              OR B            ; Add channel 2 noise flag
C338: 32 01 C1        LD (0C101h), A  ; Store updated mixer
C33B: CD C2 C2        CALL SUB9       ; Update channel output
C33E: DD 21 E7 C0     LD IX, 0C0E7h   ; Point to channel 2 data
C342: CD 8B C3        CALL SUB11      ; Calculate frequency with arpeggio
C345: 22 FC C0        LD (VAR17), HL  ; Store channel 2 frequency
C348:           LBL21:                ; Channel 2 processing complete
C348: 21 03 C1        LD HL, REF6     ; Point to channel 2 output
C34B: 77              LD (HL), A      ; Store volume output
C34C: CD CA C2        CALL SUB10      ; Apply effects to channel 2

; C
C34F: DD 21 F1 C0     LD IX, 0C0F1h   ; Point to channel 3 data
C353: CD 8E C2        CALL SUB8       ; Update channel 3 envelope
C356: DD 7E 07        LD A, (IX+07h)  ; Check if envelope active
C359: 3C              INC A           ; Test for FFh
C35A: CA 81 C3        JP Z, LBL22     ; Jump if envelope disabled
C35D: 79              LD A, C         ; Get tone enable flag
C35E: 32 95 C3        LD (0C395h), A  ; Store for later
C361: DD 2A F4 C0     LD IX, (VAR13)  ; Point to channel 3 instrument
C365: CD 19 C1        CALL SUB4       ; Process instrument data
C368: 3A 01 C1        LD A, (0C101h)  ; Get current mixer value
C36B: CB 01           RLC C           ; Rotate channel 3 tone flag
C36D: CB 00           RLC B           ; Rotate channel 3 noise flag
C36F: B0              OR B            ; Add channel 3 noise flag
C370: B1              OR C            ; Add channel 3 tone flag
C371: 32 01 C1        LD (0C101h), A  ; Store final mixer value
C374: CD C2 C2        CALL SUB9       ; Update channel output
C377: DD 21 F1 C0     LD IX, 0C0F1h   ; Point to channel 3 data
C37B: CD 8B C3        CALL SUB11      ; Calculate frequency with arpeggio
C37E: 22 FE C0        LD (VAR18), HL  ; Store channel 3 frequency
C381:           LBL22:                ; Channel 3 processing complete
C381: 21 04 C1        LD HL, REF7     ; Point to channel 3 output
C384: 77              LD (HL), A      ; Store volume output
C385: CD CA C2        CALL SUB10      ; Apply effects to channel 3
C388: C3 78 C4        JP SUB12        ; Jump to sound chip output

            SUB11:                    ; Calculate frequency with arpeggio
C38B: 7D              LD A, L         ; Save note in A
C38C: F5              PUSH AF         ; Save note on stack
C38D: D5              PUSH DE         ; Save DE
C38E: DD 6E 05        LD L, (IX+05h)  ; Load arpeggio pointer low
C391: DD 66 06        LD H, (IX+06h)  ; Load arpeggio pointer high
C394: 11 0A 00        LD DE, 000Ah    ; Offset to arpeggio table
C397: 19              ADD HL, DE      ; Calculate arpeggio table address
C398: DD 7E 01        LD A, (IX+01h)  ; Load current note
C39B: 86              ADD A, (HL)     ; Add arpeggio offset
C39C:           SMC6:                ; Self-modifying code location
C39C: C6 00           ADD A, 00h      ; Add transpose value
C39E: 87              ADD A, A        ; Multiply by 2 (word index)
C39F: 5F              LD E, A         ; Store index in E
C3A0: 16 00           LD D, 00h       ; Clear D
C3A2: 21 B8 C3        LD HL, REF8     ; Point to frequency table
C3A5: 19              ADD HL, DE      ; Calculate frequency table address
C3A6: 5E              LD E, (HL)      ; Load frequency low byte
C3A7: 23              INC HL          ; Move to high byte
C3A8: 56              LD D, (HL)      ; Load frequency high byte
C3A9: EB              EX DE, HL       ; Frequency in HL
C3AA: D1              POP DE          ; Restore DE
C3AB: F1              POP AF          ; Restore original note
C3AC: CB 62           BIT 4, D        ; Check effect flag
C3AE: 28 04           JR Z, LBL23     ; Jump if no effect
C3B0: CB A2           RES 4, D        ; Clear effect flag
C3B2: 19              ADD HL, DE      ; Add effect offset to frequency
C3B3: C9              RET             ; Return

            LBL23:                    ; No effect - subtract offset
C3B4: A7              AND A           ; Clear carry flag
C3B5: ED 52           SBC HL, DE      ; Subtract effect offset
C3B7: C9              RET             ; Return

            REF8:                     ; Frequency lookup table (96 notes)

        dw      0EF8h,0E10h,0D60h,0C80h,0BD8h,0B28h,0A88h,09F0h
        dw      0960h,08E0h,0858h,07E0h,077Ch,0708h,06B0h,0640h
        dw      05ECh,0594h,0544h,04F8h,04B0h,0470h,042Ch,03F0h
        dw      03BEh,0384h,0358h,0320h,02F6h,02CAh,02A2h,027Ch
        dw      0258h,0238h,0216h,01F8h,01DFh,01C2h,01ACh,0190h
        dw      017Bh,0165h,0151h,013Eh,012Ch,011Ch,010Bh,00FCh
        dw      00EFh,00E1h,00D6h,00C8h,00BDh,00B2h,00A8h,009Fh
        dw      0096h,008Eh,0085h,007Eh,0077h,0070h,006Bh,0064h
        dw      005Eh,0059h,0054h,004Fh,004Bh,0047h,0042h,003Fh
        dw      003Bh,0038h,0035h,0032h,002Fh,002Ch,002Ah,0027h
        dw      0025h,0023h,0021h,001Fh,001Dh,001Ch,001Ah,0019h
        dw      0017h,0016h,0015h,0013h,0012h,0011h,0010h,000Fh

            SUB12:                    ; Output to sound chip
C478: 21 07 C1        LD HL, VAR20    ; Point to noise/enable flags
C47B: AF              XOR A           ; Clear A
C47C: B6              OR (HL)         ; Check if noise/enable active
C47D: 3E 0D           LD A, 0Dh       ; Load register 13 (noise/enable)
C47F: 20 05           JR NZ, LBL28    ; Jump if noise active
C481: D6 03           SUB 03h         ; Use register 10 instead
C483: 2B              DEC HL          ; Adjust pointer
C484: 2B              DEC HL          ; to channel volumes
C485: 2B              DEC HL
C486:           LBL28:
C486: 0E FD           LD C, 0FDh      ; Sound chip register port
C488:           LBL29:                ; Output loop
C488: 06 FF           LD B, 0FFh      ; Set B for register select
C48A: ED 79           OUT (C), A      ; Select sound chip register
C48C: 06 BF           LD B, 0BFh      ; Set B for data output
C48E: ED AB           OUTD            ; Output (HL) and decrement HL
C490: 3D              DEC A           ; Decrement register number
C491: F2 88 C4        JP P, LBL29     ; Loop until all registers output
C494: C9              RET             ; Return

            LBL30:                    ; Sound chip initialization
C495: ED 79           OUT (C), A      ; Output to sound chip
C497: 3C              INC A           ; Increment value
C498: 20 FB           JR NZ, LBL30    ; Loop until 256 bytes output
C49A: 06 FF           LD B, 0FFh      ; Set B for register select
C49C: 3E 07           LD A, 07h       ; Select register 7 (mixer)
C49E: ED 79           OUT (C), A      ; Select mixer register
C4A0: 01 FD BF        LD BC, 0BFFDh   ; Sound chip data port
C4A3: 3E FF           LD A, 0FFh      ; Disable all channels
C4A5: ED 79           OUT (C), A      ; Set mixer (all channels off)
C4A7: C9              RET             ; Return