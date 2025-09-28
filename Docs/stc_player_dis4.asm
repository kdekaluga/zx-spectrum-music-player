; =============================================
; External Address Equates (likely ROM routines)
; =============================================
            EXT1        EQU 16F1h  ; External address 1
            EXT2        EQU 0A202h ; External address 2
            EXT3        EQU 0AC01h ; External address 3
            EXT4        EQU 0EE5Eh ; External address 4
            EXT5        EQU 0F4F3h ; External address 5

; =============================================
; Variable Definitions
; =============================================
                ORG 2700h          ; Set origin to 2700h
2700:           V_2700: DS 2       ; Reserve 2 bytes at 2700h

                ORG 2F00h          ; Set origin to 2F00h
2F00:           V_2F00: DS 1       ; Reserve 1 byte at 2F00h

; =============================================
; Main Player Code Starts at C04Ch
; =============================================
                ORG 0C04Ch         ; Set origin to C04Ch

; =============================================
; Initialization Routines
; =============================================
C04C: 01 FD FF     LD BC, 0FFFDh   ; BC = AY register select port (FFFDh)
C04F: 3E 0C        LD A, 0Ch       ; A = 12 (AY envelope period fine control register)
C051: ED 79        OUT (C), A      ; Select AY register 12

C053: AF           XOR A           ; A = 0 (value to write to AY registers)
C054: 06 BF        LD B, 0BFh      ; B = BFh (high byte of AY data port BFFDh)
C056: C3 95 C4     JP LBL30        ; Jump to silence routine

; Alternate entry point for initialization
C059: 21 A8 C4     LD HL, 0C4A8h   ; HL points to some data/music pointer
C05C: C3 62 C0     JP LBL1         ; Jump to main initialization

; Main playback routine entry point (called from interrupt)
C05F: C3 9D C1     JP LBL5         ; Jump to main playback handler

; =============================================
; Main Initialization Routine (LBL1)
; =============================================
C062:          LBL1:
C062: F3           DI              ; Disable interrupts during setup

; Read song header data
C063: 7E           LD A, (HL)      ; A = first byte (song speed)
C064: 32 D1 C0     LD (VAR4), A    ; Store song speed in VAR4

C067: 22 14 C1     LD (SMC5+1), HL ; Store music data base pointer for SUB3

C06A: 23           INC HL          ; Move to next byte in header
C06B: CD 0E C1     CALL SUB2       ; Read pointer from (HL) -> DE (position list pointer)

C06E: 1A           LD A, (DE)      ; A = length of position list
C06F: 13           INC DE          ; Move to start of position data
C070: 3C           INC A           ; Increment length
C071: 32 D3 C0     LD (VAR5), A    ; Store position count in VAR5

C074: ED 53 C9 C0  LD (VAR1), DE   ; Store position list pointer in VAR1

C078: CD 0E C1     CALL SUB2       ; Read next pointer (ornament data pointer)
C07B: ED 53 CB C0  LD (VAR2), DE   ; Store ornament pointer in VAR2

C07F: D5           PUSH DE         ; Save ornament pointer on stack
C080: CD 0E C1     CALL SUB2       ; Read next pointer (sample data pointer)
C083: ED 53 CD C0  LD (VAR3), DE   ; Store sample pointer in VAR3

; Calculate pattern address table location
C087: 21 1B 00     LD HL, 001Bh    ; HL = offset to pattern table
C08A: CD 13 C1     CALL SUB3       ; Add base address -> DE = pattern table address
C08D: EB           EX DE, HL       ; HL = pattern table address
C08E: 22 CF C0     LD (SMC1+1), HL ; Store for later use

; Initialize channel control blocks (clear to zero)
C091: 21 DA C0     LD HL, REF1     ; HL = start of channel data area
C094: 22 D4 C0     LD (VAR6), HL   ; Store as current pattern pointer for channel 1

C097: 21 DB C0     LD HL, REF2     ; HL = next channel data address
C09A: 11 DC C0     LD DE, REF3     ; DE = destination for clear operation
C09D: 01 2C 00     LD BC, 002Ch    ; BC = 44 bytes to clear (3 channels × ~14 bytes)
C0A0: 70           LD (HL), B      ; Write 0 to first byte (B=0 from BC=002Ch)
C0A1: ED B0        LDIR            ; Clear channel data block (0C0DAh-0C105h)

C0A3: E1           POP HL          ; Restore ornament pointer to HL

; Find ornament 0 (null ornament) in the list
C0A4: 01 21 00     LD BC, 0021h    ; BC = 33 (size of ornament/sample block)
C0A7: AF           XOR A           ; A = 0 (ornament ID 0)
C0A8: CD 08 C1     CALL SUB1       ; Search for ornament 0

; Initialize sample state for all channels to "no sample"
C0AB: 3D           DEC A           ; A = FFh (no sample flag)
C0AC: 32 E4 C0     LD (VAR10), A   ; Set channel 1 sample = no sample
C0AF: 32 EE C0     LD (VAR12), A   ; Set channel 2 sample = no sample
C0B2: 32 F8 C0     LD (0C0F8h), A  ; Set channel 3 sample = no sample

; Initialize tick counter
C0B5: 3E 01        LD A, 01h       ; A = 1
C0B7: 32 D2 C0     LD (SMC2+1), A  ; Set tick counter = 1

; Set initial ornament pointers for all channels to ornament 1
C0BA: 23           INC HL          ; HL points to ornament 1 data
C0BB: 22 E2 C0     LD (VAR9), HL   ; Channel 1 ornament pointer
C0BE: 22 EC C0     LD (VAR11), HL  ; Channel 2 ornament pointer
C0C1: 22 F6 C0     LD (VAR14), HL  ; Channel 3 ornament pointer

C0C4: CD 78 C4     CALL SUB12      ; Initialize AY chip registers

C0C7: FB           EI              ; Enable interrupts
C0C8: C9           RET             ; Return from initialization

; =============================================
; Player Variables and Data Area
; =============================================
C0C9:          VAR1:
C0C9: 77 F1        DW 0F177h       ; Position list pointer
C0CB:          VAR2:
C0CB: 8F F1        DW 0F18Fh       ; Ornament data pointer
C0CD:          VAR3:
C0CD: 13 F2        DW 0F213h       ; Sample data pointer

C0D1:          VAR4:
C0D1:          SMC2:
C0D1: 06           DB 06h          ; Song speed / Tick counter (SMC)
C0D3:          VAR5:
C0D3: 0C           DB 0Ch          ; Number of positions

C0D4:          VAR6:
C0D4: B1 F4        DW 0F4B1h       ; Channel 1 current pattern pointer
C0D6:          SMC3:
C0D6: 35 F5        DW 0F535h       ; Channel 2 current pattern pointer (SMC)
C0D8:          VAR7:
C0D8: 35 F5        DW 0F535h       ; Channel 3 current pattern pointer

C0DA:          REF1:
C0DA: FF           RST 38h         ; Channel 1 control block start
C0DB:          REF2:
C0DB: 00           NOP             ; Channel 2 control block start
C0DC:          REF3:
C0DC: 00           NOP             ; Channel 3 control block start

C0DD:          REF4:
C0DD: 04           INC B           ; Channel 1 data: delay counter
C0DE: 24           INC H           ; Channel 1 data: various flags/state
C0DF: 00           NOP             ; Channel 1 data: current note

C0E0:          VAR8:
C0E0: 88 EF        DW 0EF88h       ; Channel 1 current ornament pointer
C0E2:          VAR9:
C0E2: 90 F1        DW 0F190h       ; Channel 1 ornament data pointer
C0E4:          VAR10:
C0E4: 1C           DB 1Ch          ; Channel 1 current sample number
C0E5: 02           LD (BC), A      ; Channel 1 data: mixer flags?
C0E6: 01 04 1A     LD BC, 1A04h    ; Channel 1 data: envelope/volume

C0E9:          SMC4:
C0E9: 01 4E F0     LD BC, 0F04Eh   ; Channel 2 current ornament pointer (SMC)
C0EC:          VAR11:
C0EC: 90 F1        DW 0F190h       ; Channel 2 ornament data pointer
C0EE:          VAR12:
C0EE: 1C           DB 1Ch          ; Channel 2 current sample number
C0EF: 00           NOP             ; Channel 2 data: mixer flags?
C0F0: 01 0A 35     LD BC, 350Ah    ; Channel 2 data: envelope/volume

C0F3:          00           NOP             ; Padding?
C0F4:          VAR13:
C0F4: B1 F0        DW 0F0B1h       ; Channel 3 current ornament pointer
C0F6:          VAR14:
C0F6: D2 F1        DW 0F1D2h       ; Channel 3 ornament data pointer
C0F8:          1C           DB 1Ch          ; Channel 3 current sample number
C0F9:          VAR15:
C0F9: 05           DB 05h          ; Current position number
C0FA:          VAR16:
C0FA: DF 09        DW 09DFh        ; Channel 1 tone period for AY
C0FC:          VAR17:
C0FC: 58 03        DW 0358h        ; Channel 2 tone period for AY
C0FE:          VAR18:
C0FE: 68 01        DW 0168h        ; Channel 3 tone period for AY

C102:          REF5:
C102: 0B           DEC BC          ; AY mixer value for channels
C103:          REF6:
C103: 1A           LD A, (DE)      ; Temporary storage
C104:          REF7:
C104: 07           RLCA            ; Temporary storage

C105:          VAR19:
C105: D9           DB 0D9h         ; Temporary variable
C106: 00           NOP             ; Padding
C107:          VAR20:
C107: 00           DB 00h          ; Envelope active flag

; =============================================
; Subroutine 1: Linear Search in Block List
; =============================================
C108:          SUB1:
C108: BE           CP (HL)         ; Compare A with block ID at (HL)
C109: C8           RET Z           ; Return if found
C10A: 09           ADD HL, BC      ; Move to next block (add block size)
C10B: C3 08 C1     JP SUB1         ; Continue searching

; =============================================
; Subroutine 2: Read Pointer from Music Data
; =============================================
C10E:          SUB2:
C10E: 5E           LD E, (HL)      ; Read low byte of pointer
C10F: 23           INC HL          ; Move to high byte
C110: 56           LD D, (HL)      ; Read high byte of pointer
C111: 23           INC HL          ; Move to next data
C112: EB           EX DE, HL       ; Pointer now in HL
C113:          SMC5:
C113:          SUB3:
C113: 01 43 EE     LD BC, 0EE43h   ; Base address offset (SMC - changed during init)
C116: 09           ADD HL, BC      ; Convert offset to absolute address
C117: EB           EX DE, HL       ; Result in DE, restore HL
C118: C9           RET             ; Return

; =============================================
; Subroutine 4: Process Ornament Data
; =============================================
C119:          SUB4:
C119: 16 00        LD D, 00h       ; Clear high byte
C11B: 5F           LD E, A         ; DE = A (ornament step index)
C11C: 87           ADD A, A        ; A × 2
C11D: 83           ADD A, E        ; A × 3 (each ornament step is 3 bytes)
C11E: 5F           LD E, A         ; DE = A × 3
C11F: DD 19        ADD IX, DE      ; IX points to current ornament step

C121: DD 7E 01     LD A, (IX+01h)  ; A = ornament step flags
C124: CB 7F        BIT 7, A        ; Test noise enable bit
C126: 0E 10        LD C, 10h       ; C = mixer noise enable mask
C128: C2 2C C1     JP NZ, LBL2     ; Jump if noise enabled
C12B: 4A           LD C, D         ; C = 0 (no noise) if bit 7 not set

C12C:          LBL2:
C12C: CB 77        BIT 6, A        ; Test tone enable bit
C12E: 06 02        LD B, 02h       ; B = mixer tone enable mask
C130: C2 34 C1     JP NZ, LBL3     ; Jump if tone enabled
C133: 42           LD B, D         ; B = 0 (no tone) if bit 6 not set

C134:          LBL3:
C134: E6 1F        AND 1Fh         ; Mask ornament note value (lower 5 bits)
C136: 67           LD H, A         ; H = ornament note value
C137: DD 5E 02     LD E, (IX+02h)  ; E = ornament slide value
C13A: DD 7E 00     LD A, (IX+00h)  ; A = ornament base note
C13D: F5           PUSH AF         ; Save base note
C13E: E6 F0        AND 0F0h        ; Get envelope type from high nibble
C140: 0F           RRCA            ; Rotate to lower nibble
C141: 0F           RRCA
C142: 0F           RRCA
C143: 0F           RRCA
C144: 57           LD D, A         ; D = envelope type
C145: F1           POP AF          ; Restore base note
C146: E6 0F        AND 0Fh         ; Get base note number (lower 4 bits)
C148: 6F           LD L, A         ; L = base note number

C149: DD CB 01 6E  BIT 5, (IX+01h) ; Test envelope enable bit
C14D: C8           RET Z           ; Return if envelope disabled
C14E: CB E2        SET 4, D        ; Set envelope enable flag in D
C150: C9           RET             ; Return

; =============================================
; Subroutine 5: Fetch Pattern Data for Current Position
; =============================================
C151:          SUB5:
C151: 3A F9 C0     LD A, (VAR15)   ; A = current position number
C154: 4F           LD C, A         ; C = position number
C155: 21 D3 C0     LD HL, VAR5     ; HL points to total positions
C158: BE           CP (HL)         ; Compare with max positions
C159: DA 5E C1     JP C, LBL4      ; Jump if position < max
C15C: AF           XOR A           ; Else reset to position 0
C15D: 4F           LD C, A         ; C = 0

C15E:          LBL4:
C15E: 3C           INC A           ; Increment for next time
C15F: 32 F9 C0     LD (VAR15), A   ; Store next position number
C162: 69           LD L, C         ; HL = position number × 2
C163: 26 00        LD H, 00h
C165: 29           ADD HL, HL      ; Each position entry is 2 bytes
C166: ED 5B C9 C0  LD DE, (VAR1)   ; DE = position list pointer
C16A: 19           ADD HL, DE      ; HL points to current position entry
C16B: 4E           LD C, (HL)      ; C = pattern number for channel 1
C16C: 23           INC HL          ; Move to channel 2 data
C16D: 7E           LD A, (HL)      ; A = pattern number for channel 2
C16E: 32 9D C3     LD (SMC6+1), A  ; Store for channel 2 processing

C171: 79           LD A, C         ; A = channel 1 pattern number
C172: 2A CD C0     LD HL, (VAR3)   ; HL = sample data pointer
C175: 01 07 00     LD BC, 0007h    ; BC = 7 (size of pattern table entry?)
C178: CD 08 C1     CALL SUB1       ; Find pattern address for channel 1
C17B: 23           INC HL          ; Move to pattern address
C17C: CD 0E C1     CALL SUB2       ; Read pattern pointer -> DE
C17F: ED 53 D4 C0  LD (VAR6), DE   ; Store as channel 1 pattern pointer
C183: CD 0E C1     CALL SUB2       ; Read channel 2 pattern pointer
C186: ED 53 D6 C0  LD (SMC3+1), DE ; Store as channel 2 pattern pointer
C18A: CD 0E C1     CALL SUB2       ; Read channel 3 pattern pointer
C18D: ED 53 D8 C0  LD (VAR7), DE   ; Store as channel 3 pattern pointer
C191: C9           RET             ; Return

; =============================================
; Subroutine 6: Decrement Channel Delay Counter
; =============================================
C192:          SUB6:
C192: DD 35 02     DEC (IX+02h)    ; Decrement channel delay counter
C195: F0           RET P           ; Return if counter >= 0 (delay not expired)
C196: DD 7E FF     LD A, (IX-01h)  ; Reload delay from previous value
C199: DD 77 02     LD (IX+02h), A  ; Reset delay counter
C19C: C9           RET             ; Return

; =============================================
; Main Playback Routine (LBL5 - Called from Interrupt)
; =============================================
C19D:          LBL5:
C19D: 3A D2 C0     LD A, (SMC2+1)  ; A = current tick counter
C1A0: 3D           DEC A           ; Decrement tick counter
C1A1: 32 D2 C0     LD (SMC2+1), A  ; Store updated counter
C1A4: C2 E7 C2     JP NZ, LBL19    ; Jump if not zero (only process samples/ornaments)

; Tick counter reached zero - process new pattern data
C1A7: 3A D1 C0     LD A, (VAR4)    ; A = song speed
C1AA: 32 D2 C0     LD (SMC2+1), A  ; Reset tick counter

; Process Channel 1 pattern data
C1AD: DD 21 DD C0  LD IX, REF4     ; IX = channel 1 control block
C1B1: CD 92 C1     CALL SUB6       ; Decrement channel 1 delay
C1B4: F2 C8 C1     JP P, LBL6      ; Jump if delay not expired

C1B7: 2A D4 C0     LD HL, (VAR6)   ; HL = channel 1 pattern pointer
C1BA: 7E           LD A, (HL)      ; A = pattern data byte
C1BB: 3C           INC A           ; Test for FFh (end of pattern)
C1BC: CC 51 C1     CALL Z, SUB5    ; If FFh, fetch next pattern data
C1BF: 2A D4 C0     LD HL, (VAR6)   ; HL = channel 1 pattern pointer
C1C2: CD F1 C1     CALL SUB7       ; Process pattern command
C1C5: 22 D4 C0     LD (VAR6), HL   ; Store updated pattern pointer
C1C8:          LBL6:

; Process Channel 2 pattern data
C1C8: DD 21 E7 C0  LD IX, 0C0E7h   ; IX = channel 2 control block
C1CC: CD 92 C1     CALL SUB6       ; Decrement channel 2 delay
C1CF: F2 DB C1     JP P, LBL7      ; Jump if delay not expired

C1D2: 2A D6 C0     LD HL, (SMC3+1) ; HL = channel 2 pattern pointer
C1D5: CD F1 C1     CALL SUB7       ; Process pattern command
C1D8: 22 D6 C0     LD (SMC3+1), HL ; Store updated pattern pointer
C1DB:          LBL7:

; Process Channel 3 pattern data
C1DB: DD 21 F1 C0  LD IX, 0C0F1h   ; IX = channel 3 control block
C1DF: CD 92 C1     CALL SUB6       ; Decrement channel 3 delay
C1E2: F2 E7 C2     JP P, LBL19     ; Jump if delay not expired

C1E5: 2A D8 C0     LD HL, (VAR7)   ; HL = channel 3 pattern pointer
C1E8: CD F1 C1     CALL SUB7       ; Process pattern command
C1EB: 22 D8 C0     LD (VAR7), HL   ; Store updated pattern pointer
C1EE: C3 E7 C2     JP LBL19        ; Jump to sample/ornament processing

; =============================================
; Subroutine 7: Pattern Data Command Interpreter
; =============================================
C1F1:          SUB7:
C1F1: 7E           LD A, (HL)      ; A = pattern command byte
C1F2: FE 60        CP 60h          ; Compare with 60h (note value threshold)
C1F4: DA 1F C2     JP C, LBL8      ; Jump if <60h (it's a note)

C1F7: FE 70        CP 70h          ; Compare with 70h
C1F9: DA 2C C2     JP C, LBL10     ; Jump if 60h-6Fh (ornament command)

C1FC: FE 80        CP 80h          ; Compare with 80h
C1FE: DA 4D C2     JP C, LBL14     ; Jump if 70h-7Fh (sample command)

C201: CA 44 C2     JP Z, LBL11     ; Jump if 80h (sample off command)

C204: FE 81        CP 81h          ; Compare with 81h
C206: CA 2A C2     JP Z, LBL9      ; Jump if 81h (empty event)

C209: FE 82        CP 82h          ; Compare with 82h
C20B: CA 4A C2     JP Z, LBL13     ; Jump if 82h (sample off + new sample)

C20E: FE 8F        CP 8Fh          ; Compare with 8Fh
C210: DA 69 C2     JP C, LBL16     ; Jump if 83h-8Eh (commands with parameters)

C213: D6 A1        SUB 0A1h        ; Subtract A1h (for A1h-BFh range)
C215: DD 77 02     LD (IX+02h), A  ; Set delay counter
C218: DD 77 FF     LD (IX-01h), A  ; Set delay reload value
C21B: 23           INC HL          ; Move to next pattern byte
C21C: C3 F1 C1     JP SUB7         ; Process next command

; Command: Note (00h-5Fh)
C21F:          LBL8:
C21F: DD 77 01     LD (IX+01h), A  ; Store note number
C222: DD 36 00 00  LD (IX+00h), 00h; Reset ornament position
C226: DD 36 07 20  LD (IX+07h), 20h; Set note duration
C22A:          LBL9:
C22A: 23           INC HL          ; Move to next pattern byte
C22B: C9           RET             ; Return

; Command: Set Ornament (60h-6Fh)
C22C:          LBL10:
C22C: D6 60        SUB 60h         ; Convert to ornament number (0-15)
C22E: E5           PUSH HL         ; Save pattern pointer
C22F: 01 63 00     LD BC, 0063h    ; BC = 99 (offset to ornament table?)
C232: 2A CF C0     LD HL, (SMC1+1) ; HL = pattern address table
C235: CD 08 C1     CALL SUB1       ; Find ornament address
C238: 23           INC HL          ; Move to ornament data
C239: DD 75 03     LD (IX+03h), L  ; Store ornament pointer low
C23C: DD 74 04     LD (IX+04h), H  ; Store ornament pointer high
C23F: E1           POP HL          ; Restore pattern pointer
C240: 23           INC HL          ; Move to next pattern byte
C241: C3 F1 C1     JP SUB7         ; Process next command

; Command: Sample Off (80h)
C244:          LBL11:
C244: 23           INC HL          ; Move to next pattern byte
C245:          LBL12:
C245: DD 36 07 FF  LD (IX+07h), 0FFh ; Disable channel (no note)
C249: C9           RET             ; Return

; Command: Sample Off + New Sample (82h)
C24A:          LBL13:
C24A: AF           XOR A           ; A = 0 (sample off)
C24B: 18 02        JR LBL15        ; Jump to sample processing

; Command: Set Sample (70h-7Fh)
C24D:          LBL14:
C24D: D6 70        SUB 70h         ; Convert to sample number (0-15)
C24F:          LBL15:
C24F: E5           PUSH HL         ; Save pattern pointer
C250: 01 21 00     LD BC, 0021h    ; BC = 33 (sample block size)
C253: 2A CB C0     LD HL, (VAR2)   ; HL = sample data pointer
C256: CD 08 C1     CALL SUB1       ; Find sample address
C259: 23           INC HL          ; Move to sample data
C25A: DD 75 05     LD (IX+05h), L  ; Store sample pointer low
C25D: DD 74 06     LD (IX+06h), H  ; Store sample pointer high
C260: DD 36 FE 00  LD (IX-02h), 00h; Reset sample state
C264: E1           POP HL          ; Restore pattern pointer
C265: 23           INC HL          ; Move to next pattern byte
C266: C3 F1 C1     JP SUB7         ; Process next command

; Command: Special Commands with Parameters (83h-8Eh)
C269:          LBL16:
C269: D6 80        SUB 80h         ; Convert to command number (3-14)
C26B: 32 07 C1     LD (VAR20), A   ; Store command number
C26E: 23           INC HL          ; Move to parameter byte
C26F: 7E           LD A, (HL)      ; A = parameter value
C270: 23           INC HL          ; Move to next pattern byte
C271: 32 05 C1     LD (VAR19), A   ; Store parameter
C274: DD 36 FE 01  LD (IX-02h), 01h; Set sample state = active
C278: E5           PUSH HL         ; Save pattern pointer
C279: AF           XOR A           ; A = 0 (find sample 0)
C27A: 01 21 00     LD BC, 0021h    ; BC = 33 (sample block size)
C27D: 2A CB C0     LD HL, (VAR2)   ; HL = sample data pointer
C280: CD 08 C1     CALL SUB1       ; Find sample address
C283: 23           INC HL          ; Move to sample data
C284: DD 75 05     LD (IX+05h), L  ; Store sample pointer low
C287: DD 74 06     LD (IX+06h), H  ; Store sample pointer high
C28A: E1           POP HL          ; Restore pattern pointer
C28B: C3 F1 C1     JP SUB7         ; Process next command

; =============================================
; Subroutine 8: Process Ornament Arpeggio/Glide
; =============================================
C28E:          SUB8:
C28E: DD 7E 07     LD A, (IX+07h)  ; A = note duration counter
C291: 3C           INC A           ; Test for FFh (channel disabled)
C292: C8           RET Z           ; Return if channel disabled

C293: 3D           DEC A           ; Restore counter value
C294: 3D           DEC A           ; Decrement counter twice?
C295: DD 77 07     LD (IX+07h), A  ; Store updated counter
C298: F5           PUSH AF         ; Save counter
C299: DD 7E 00     LD A, (IX+00h)  ; A = current ornament position
C29C: 4F           LD C, A         ; C = ornament position
C29D: 3C           INC A           ; Move to next ornament step
C29E: E6 1F        AND 1Fh         ; Wrap at 32 steps
C2A0: DD 77 00     LD (IX+00h), A  ; Store new ornament position
C2A3: F1           POP AF          ; Restore counter
C2A4: C0           RET NZ          ; Return if counter not expired

; Counter expired - process next ornament step
C2A5: DD 5E 03     LD E, (IX+03h)  ; DE = ornament pointer
C2A8: DD 56 04     LD D, (IX+04h)
C2AB: 21 60 00     LD HL, 0060h    ; HL = offset to ornament data
C2AE: 19           ADD HL, DE      ; HL points to ornament step data
C2AF: 7E           LD A, (HL)      ; A = ornament step value
C2B0: 3D           DEC A           ; Test for FFh (end of ornament)
C2B1: FA 45 C2     JP M, LBL12     ; Jump if FFh (disable channel)
C2B4: 4F           LD C, A         ; C = ornament step value
C2B5: 3C           INC A           ; Restore value
C2B6: E6 1F        AND 1Fh         ; Mask to 5 bits
C2B8: DD 77 00     LD (IX+00h), A  ; Store as new ornament position
C2BB: 23           INC HL          ; Move to duration value
C2BC: 7E           LD A, (HL)      ; A = step duration
C2BD: 3C           INC A           ; Increment duration
C2BE: DD 77 07     LD (IX+07h), A  ; Set new note duration
C2C1: C9           RET             ; Return

; =============================================
; Subroutine 9: Tone Period Calculation Helper
; =============================================
C2C2:          SUB9:
C2C2: 79           LD A, C         ; A = tone period high byte
C2C3: B7           OR A            ; Test if high byte is zero
C2C4: C0           RET NZ          ; Return if not zero (normal case)
C2C5: 7C           LD A, H         ; A = tone period low byte
C2C6: 32 00 C1     LD (0C100h), A  ; Store for special case
C2C9: C9           RET             ; Return

; =============================================
; Subroutine 10: Sample Volume Envelope Processing
; =============================================
C2CA:          SUB10:
C2CA: DD 7E 07     LD A, (IX+07h)  ; A = note duration
C2CD: 3C           INC A           ; Test for FFh (channel disabled)
C2CE: C8           RET Z           ; Return if disabled

C2CF: DD 7E FE     LD A, (IX-02h)  ; A = sample state
C2D2: B7           OR A            ; Test state
C2D3: C8           RET Z           ; Return if state = 0 (inactive)

C2D4: FE 02        CP 02h          ; Compare with state 2 (envelope active)
C2D6: CA E0 C2     JP Z, LBL17     ; Jump if envelope active

C2D9: DD 36 FE 02  LD (IX-02h), 02h; Set state to 2 (envelope active)
C2DD: C3 E4 C2     JP LBL18        ; Jump to set envelope flag

C2E0:          LBL17:
C2E0: AF           XOR A           ; A = 0
C2E1: 32 07 C1     LD (VAR20), A   ; Clear envelope command
C2E4:          LBL18:
C2E4: CB E6        SET 4, (HL)     ; Set envelope enable flag in mixer
C2E6: C9           RET             ; Return

; =============================================
; Main Sound Generation Routine (LBL19)
; =============================================
C2E7:          LBL19:
; Process Channel 1 sound generation
C2E7: DD 21 DD C0  LD IX, REF4     ; IX = channel 1 control block
C2EB: CD 8E C2     CALL SUB8       ; Process ornament for channel 1
C2EE: 79           LD A, C         ; A = resulting note value
C2EF: 32 95 C3     LD (0C395h), A  ; Store for later use

C2F2: DD 2A E0 C0  LD IX, (VAR8)   ; IX = channel 1 ornament data
C2F6: CD 19 C1     CALL SUB4       ; Process ornament step
C2F9: 79           LD A, C         ; A = noise enable flags
C2FA: B0           OR B            ; Combine with tone enable flags
C2FB: 0F           RRCA            ; Rotate to proper position for AY mixer
C2FC: 32 01 C1     LD (0C101h), A  ; Store mixer value

C2FF: DD 21 DD C0  LD IX, REF4     ; IX = channel 1 control block
C303: DD 7E 07     LD A, (IX+07h)  ; Check if channel active
C306: 3C           INC A
C307: CA 13 C3     JP Z, LBL20     ; Jump if channel disabled

C30A: CD C2 C2     CALL SUB9       ; Tone period calculation helper
C30D: CD 8B C3     CALL SUB11      ; Calculate final tone period
C310: 22 FA C0     LD (VAR16), HL  ; Store channel 1 tone period
C313:          LBL20:
C313: 21 02 C1     LD HL, REF5     ; HL points to AY mixer storage
C316: 77           LD (HL), A      ; Store channel 1 mixer flags
C317: CD CA C2     CALL SUB10      ; Process sample volume envelope

; Process Channel 2 sound generation
C31A: DD 21 E7 C0  LD IX, 0C0E7h   ; IX = channel 2 control block
C31E: CD 8E C2     CALL SUB8       ; Process ornament for channel 2
C321: DD 7E 07     LD A, (IX+07h)  ; Check if channel active
C324: 3C           INC A
C325: CA 48 C3     JP Z, LBL21     ; Jump if channel disabled

C328: 79           LD A, C         ; A = resulting note value
C329: 32 95 C3     LD (0C395h), A  ; Store for later use

C32C: DD 2A EA C0  LD IX, (SMC4+1) ; IX = channel 2 ornament data
C330: CD 19 C1     CALL SUB4       ; Process ornament step
C333: 3A 01 C1     LD A, (0C101h)  ; A = current mixer value
C336: B1           OR C            ; Add channel 2 noise enable
C337: B0           OR B            ; Add channel 2 tone enable
C338: 32 01 C1     LD (0C101h), A  ; Store updated mixer value

C33B: CD C2 C2     CALL SUB9       ; Tone period calculation helper
C33E: DD 21 E7 C0  LD IX, 0C0E7h   ; IX = channel 2 control block
C342: CD 8B C3     CALL SUB11      ; Calculate final tone period
C345: 22 FC C0     LD (VAR17), HL  ; Store channel 2 tone period
C348:          LBL21:
C348: 21 03 C1     LD HL, REF6     ; HL points to AY mixer storage
C34B: 77           LD (HL), A      ; Store channel 2 mixer flags
C34C: CD CA C2     CALL SUB10      ; Process sample volume envelope

; Process Channel 3 sound generation
C34F: DD 21 F1 C0  LD IX, 0C0F1h   ; IX = channel 3 control block
C353: CD 8E C2     CALL SUB8       ; Process ornament for channel 3
C356: DD 7E 07     LD A, (IX+07h)  ; Check if channel active
C359: 3C           INC A
C35A: CA 81 C3     JP Z, LBL22     ; Jump if channel disabled

C35D: 79           LD A, C         ; A = resulting note value
C35E: 32 95 C3     LD (0C395h), A  ; Store for later use

C361: DD 2A F4 C0  LD IX, (VAR13)  ; IX = channel 3 ornament data
C365: CD 19 C1     CALL SUB4       ; Process ornament step
C368: 3A 01 C1     LD A, (0C101h)  ; A = current mixer value
C36B: CB 01        RLC C           ; Rotate channel 3 noise enable to position
C36D: CB 00        RLC B           ; Rotate channel 3 tone enable to position
C36F: B0           OR B            ; Add channel 3 tone enable
C370: B1           OR C            ; Add channel 3 noise enable
C371: 32 01 C1     LD (0C101h), A  ; Store final mixer value

C374: CD C2 C2     CALL SUB9       ; Tone period calculation helper
C377: DD 21 F1 C0  LD IX, 0C0F1h   ; IX = channel 3 control block
C37B: CD 8B C3     CALL SUB11      ; Calculate final tone period
C37E: 22 FE C0     LD (VAR18), HL  ; Store channel 3 tone period
C381:          LBL22:
C381: 21 04 C1     LD HL, REF7     ; HL points to AY mixer storage
C384: 77           LD (HL), A      ; Store channel 3 mixer flags
C385: CD CA C2     CALL SUB10      ; Process sample volume envelope

C388: C3 78 C4     JP SUB12        ; Jump to AY chip output routine

; =============================================
; Subroutine 11: Calculate Tone Period
; =============================================
C38B:          SUB11:
C38B: 7D           LD A, L         ; A = base note low byte
C38C: F5           PUSH AF         ; Save base note
C38D: D5           PUSH DE         ; Save DE

C38E: DD 6E 05     LD L, (IX+05h)  ; HL = sample data pointer
C391: DD 66 06     LD H, (IX+06h)
C394: 11 0A 00     LD DE, 000Ah    ; DE = 10 (offset to pitch shift in sample)
C397: 19           ADD HL, DE      ; HL points to sample pitch shift
C398: DD 7E 01     LD A, (IX+01h)  ; A = current note value
C39B: 86           ADD A, (HL)     ; Add sample pitch shift
C39C:          SMC6:
C39C: C6 00        ADD A, 00h      ; Add channel-specific pitch offset (SMC)
C39E: 87           ADD A, A        ; A × 2 (each period entry is 2 bytes)
C39F: 5F           LD E, A         ; DE = index into period table
C3A0: 16 00        LD D, 00h
C3A2: 21 B8 C3     LD HL, REF8     ; HL = tone period lookup table
C3A5: 19           ADD HL, DE      ; HL points to period value
C3A6: 5E           LD E, (HL)      ; DE = period value from table
C3A7: 23           INC HL
C3A8: 56           LD D, (HL)
C3A9: EB           EX DE, HL       ; HL = period value

C3AA: D1           POP DE          ; Restore DE
C3AB: F1           POP AF          ; Restore base note
C3AC: CB 62        BIT 4, D        ; Test envelope slide direction
C3AE: 28 04        JR Z, LBL23     ; Jump if slide down

C3B0: CB A2        RES 4, D        ; Clear direction flag
C3B2: 19           ADD HL, DE      ; Add slide value to period (slide up)
C3B3: C9           RET             ; Return

C3B4:          LBL23:
C3B4: A7           AND A           ; Clear carry flag
C3B5: ED 52        SBC HL, DE      ; Subtract slide value from period (slide down)
C3B7: C9           RET             ; Return

; =============================================
; Tone Period Lookup Table (REF8)
; =============================================
C3B8:          REF8:
; Note periods for AY chip (96 notes, 2 bytes each)
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

; =============================================
; Subroutine 12: Output to AY Chip
; =============================================
C478:          SUB12:
C478: 21 07 C1     LD HL, VAR20    ; HL points to envelope active flag
C47B: AF           XOR A           ; A = 0
C47C: B6           OR (HL)         ; Test if envelope active
C47D: 3E 0D        LD A, 0Dh       ; A = 13 (last AY register to write)
C47F: 20 05        JR NZ, LBL28    ; Jump if envelope active

C481: D6 03        SUB 03h         ; A = 10 (skip envelope registers if not active)
C483: 2B           DEC HL          ; Adjust HL for shorter register set
C484: 2B           DEC HL
C485: 2B           DEC HL
C486:          LBL28:
C486: 0E FD        LD C, 0FDh      ; C = FDh (AY register select port low byte)
C488:          LBL29:
C488: 06 FF        LD B, 0FFh      ; B = FFh (AY register select port high byte)
C48A: ED 79        OUT (C), A      ; Select AY register (A = register number)
C48C: 06 BF        LD B, 0BFh      ; B = BFh (AY data port high byte)
C48E: ED AB        OUTD            ; Output (HL) to AY, decrement HL
C490: 3D           DEC A           ; Decrement register number
C491: F2 88 C4     JP P, LBL29     ; Loop until all registers written (A < 0)
C494: C9           RET             ; Return

; =============================================
; Silence Routine (LBL30)
; =============================================
C495:          LBL30:
C495: ED 79        OUT (C), A      ; Write 0 to AY register (A)
C497: 3C           INC A           ; Next register
C498: 20 FB        JR NZ, LBL30    ; Loop until all 256 registers done (wraps)

; Set AY mixer to silence all channels
C49A: 06 FF        LD B, 0FFh      ; B = FFh (register select port)
C49C: 3E 07        LD A, 07h       ; A = 7 (AY mixer register)
C49E: ED 79        OUT (C), A      ; Select mixer register
C4A0: 01 FD BF     LD BC, 0BFFDh   ; BC = AY data port
C4A3: 3E FF        LD A, 0FFh      ; A = FFh (all channels off)
C4A5: ED 79        OUT (C), A      ; Write to mixer (silence all)
C4A7: C9           RET             ; Return