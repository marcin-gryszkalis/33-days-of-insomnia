;Gus interface
; by Dagoon using Jim Goer's routines
; add dmi? Change all those movs to dec/inc dx
; Dword transfers?

; - = removed
; * = preserves registers
; % = to be removed
; & = to be reduced

; * PlayVoice
; * StopVoice
; * SetVoiceVolume
; * SetVoiceFrequency
; SetVoiceBalance
; * u_Delay
; * u_DelayB
; u_Poke
; u_Peek
; SearchBasePort
;               SetUpAddress
;               CheckULTRA
; - CheckMemory
; RAM2DRAM
; - DRAM2RAM
; & ResetUltraSound


;컴 Play voice 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;

PlayVoice:

;Init
                push    ax bx cx dx

;                mov     dx,cs:u_ActiveVoice
;                mov     al,cs:Voice     ;voice
;                out     dx,al
;                out     dx,al
;                out     dx,al

;                call    StopVoice

        ;Set LoopStart - offset from place in DRAM (repeat)
                mov     ebx,cs:VStart

                mov     dx,cs:u_Command
                mov     al,LoopStartHi
                out     dx,al
                inc     dx ; mov     dx,cs:u_WordIO
                mov     eax,ebx
                shr     eax,7
                and     eax,1fffh
                out     dx,ax

                dec     dx ; mov     dx,cs:u_Command
                mov     al,LoopStartLo
                out     dx,al
                inc     dx ;mov     dx,cs:u_WordIO
                mov     eax,ebx
                and     eax,7fh
                shl     eax,9
                out     dx,ax


        ;Set LoopBegin - place sample in DRAM
                mov     ebx,cs:VBegin

                dec     dx ; mov     dx,cs:u_Command
                mov     al,LoopBeginHi
                out     dx,al
                inc     dx ; mov     dx,cs:u_WordIO
                mov     eax,ebx
                shr     eax,7
                and     eax,1fffh
                out     dx,ax

                dec     dx ; mov     dx,cs:u_Command
                mov     al,LoopBeginLo
                out     dx,al
                inc     dx ; mov     dx,cs:u_WordIO
                mov     eax,ebx
                and     eax,7fh
                shl     eax,9
                out     dx,ax

        ;Set LoopEnd
                mov     ebx,cs:VEnd

                dec     dx ; mov     dx,cs:u_Command
                mov     al,LoopEndHi
                out     dx,al
                inc     dx ; mov     dx,cs:u_WordIO
                mov     eax,ebx
                shr     eax,7
                and     eax,1fffh
                out     dx,ax

                dec     dx ; mov     dx,cs:u_Command
                mov     al,LoopEndLo
                out     dx,al
                inc     dx ; mov     dx,cs:u_WordIO
                mov     eax,ebx
                and     eax,7fh
                shl     eax,9
                out     dx,ax

        ;Set voice mode
                dec     dx ; mov     dx,cs:u_Command
                mov     al,VoiceMode
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,cs:Mode
                out     dx,al

                pop     dx cx bx ax

                ret


;컴 Stop voice 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;

StopVoice:

                push    ax cx dx
        ;Select voice
                mov     dx,cs:u_ActiveVoice
                mov     al,cs:Voice     ;voice
                out     dx,al

        ;Stop voice
                mov     dx,cs:u_Command
                mov     al,R_VoiceMode  ;command #80h
                out     dx,al

                mov     dx,cs:u_ByteIO     ;u_DataHi
                in      ax,dx           ;can be: in al,dx
                mov     cx,ax

                mov     dx,cs:u_Command
                mov     al,VoiceMode    ;command #0
                out     dx,al
                mov     dx,cs:u_ByteIO
                and     cx,11011111b    ;0DFh
                or      cx,00000011b
                mov     ax,cx
                out     dx,ax
                call    u_DelayB
                mov     dx,cs:u_Command
                mov     al,VoiceMode    ;command #0
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     ax,cx
                out     dx,ax
                pop     dx cx ax
                ret


;컴 Set volume 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;
;bx => volume (made up)
SetVoiceVolume:

                push    ax bx dx
                mov     dx,cs:u_ActiveVoice
                mov     al,cs:Voice
                out     dx,al
                mov     dx,cs:u_Command
                mov     al,VoiceVolume
                out     dx,al
                mov     ax,bx ; word ptr cs:[GUSVolume][bx]
                mov     dx,cs:u_WordIO
                out     dx,ax
                pop     dx bx ax
                ret



;컴 Set frequency 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;

SetVoiceFrequency:
                push    ax dx
                push    ax
                mov     dx,cs:u_ActiveVoice
                mov     al,cs:Voice
                out     dx,al
                mov     dx,cs:u_Command
                mov     al,VoiceFreq
                out     dx,al
                mov     dx,cs:u_WordIO
                ;mov     ax,cs:Frequency
                pop     ax
                out     dx,ax
                pop     dx ax

                ret



;컴 Set voice balance 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;

SetVoiceBalance:
                mov     dx,cs:u_ActiveVoice
                mov     al,cs:Voice
                out     dx,al
                mov     dx,cs:u_Command
                mov     al,VoiceBalance
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,cs:Balance
                out     dx,al

                ret



;컴 Delay 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;
;Idle wait for the card

u_Delay         Proc

                push    ax dx
                mov     dx,300h
                Rept 7
                in      al,dx
                EndM
                pop    dx ax

                ret

u_Delay         EndP

u_DelayB         Proc

                push    ax dx
                mov     dx,u_Base
                Rept 14
                in      al,dx
                EndM
                pop    dx ax

                ret

u_DelayB         EndP



;컴 Poke UltraSound Memory 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;
;Write one cell in DRAM memory
;In: BX:CX -> DRAM Address
;    AL -> OutByte

;10000h -> BX=0 , CX=1

u_Poke          Proc

                push    ax
                mov     dx,cs:u_Command
                mov     al,DRAMLo
                out     dx,al

                mov     dx,cs:u_WordIO
                mov     ax,bx
                out     dx,ax

                mov     dx,cs:u_Command
                mov     al,DRAMHi
                out     dx,al

                mov     dx,cs:u_ByteIO
                mov     al,cl
                out     dx,al

                mov     dx,cs:u_DRAMIO
                pop     ax
                out     dx,al

                ret

u_Poke          EndP



;컴 Peek UltraSound Memory 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;
;Returns one cell in DRAM memory
;In: BX:CX -> DRAM Address
;Out: AL -> InByte

u_Peek          Proc

                mov     dx,cs:u_Command
                mov     al,DRAMLo
                out     dx,al

                mov     dx,cs:u_WordIO
                mov     ax,bx
                out     dx,ax

                mov     dx,cs:u_Command
                mov     al,DRAMHi
                out     dx,al

                mov     dx,cs:u_ByteIO
                mov     al,cl
                out     dx,al

                mov     dx,cs:u_DRAMIO
                in      al,dx

                ret

u_Peek          EndP



;컴 Setup addresses 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;
;Sets all required values, i.e. port addresses

SetUpAddress    Proc

                mov     dx,cs:u_Base
                add     dx,6
                mov     cs:u_Status,dx

                mov     dx,cs:u_Base
                add     dx,102h
                mov     cs:u_ActiveVoice,dx

                mov     dx,cs:u_Base
                add     dx,103h
                mov     cs:u_Command,dx

                mov     dx,cs:u_Base
                add     dx,104h
                mov     cs:u_WordIO,dx

                mov     dx,cs:u_Base
                add     dx,105h
                mov     cs:u_ByteIO,dx

                mov     dx,cs:u_Base
                add     dx,107h
                mov     cs:u_DRAMIO,dx

                ret

SetUpAddress    EndP



;컴 Check UltraSound Card 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;
;Tests if UltraSound is installed
;Out: ZeroFlag = 0 -> UltraSound installed
;     ZeroFlag = 1 -> UltraSound not installed

CheckULTRA      Proc

                mov     dx,cs:u_Command
                mov     al,Initialize
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,0
                out     dx,al

                call    u_Delay
                call    u_Delay

                mov     dx,cs:u_Command
                mov     al,Initialize
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,1
                out     dx,al

                mov     bx,0
                mov     cx,0
                mov     al,0AAh
                call    U_Poke

                mov     bx,100h
                mov     cx,0
                mov     al,055h
                call    U_Poke

                mov     bx,0
                mov     cx,0
                call    U_Peek

                push    ax

                mov     dx,cs:u_Command
                mov     al,Initialize
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,0
                out     dx,al

                pop     ax

                cmp     al,0AAh

                ret

CheckULTRA      EndP



;컴 Search UltraSound Baseport 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;
;Out: DX,u_Base -> Baseport

SearchBasePort  Proc

                mov     dx,210h-10h
                mov     cx,6
SBPLP:
                add     dx,10h
                mov     cs:u_Base,dx
                push    cx dx
                call    SetUpAddress
                call    CheckUltra
                pop     dx cx
                loopnz  SBPLP

                ret

SearchBasePort  EndP

;컴 Write data into DRAM 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;
;In: DS:SI -> BASE RAM address of data
;    EBX   -> GUS DRAM address of data
;    CX    -> max 64kB size of data

RAM2DRAM        Proc

R2DLP:
                lodsb
                push    ebx cx
                mov     cx,bx
                shr     ebx,16
                xchg    bx,cx
                call    u_Poke
                pop     cx ebx
                inc     ebx
                dec     cx
                jnz     R2DLP
                ret

RAM2DRAM        EndP


;컴 Reset UltraSound 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;

ResetUltraSound Proc

                mov     dx,cs:u_Command
                mov     al,Initialize
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,0
                out     dx,al

                call    u_DelayB

                mov     dx,cs:u_Command
                mov     al,Initialize           ;command #4Ch
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,1
                out     dx,al

                call    u_DelayB

        ;Clear any pending DMA IRQs
                mov     dx,cs:u_Command
                mov     al,DMAControl
                out     dx,al
                mov     dx,cs:u_ByteIO
                xor     al,al
                out     dx,al

                mov     dx,cs:u_Command
                mov     al,TimerControl
                out     dx,al
                mov     dx,cs:u_ByteIO
                xor     al,al
                out     dx,al

                mov     dx,cs:u_Command
                mov     al,SampleControl
                out     dx,al
                mov     dx,cs:u_ByteIO
                xor     al,al
                out     dx,al

                mov     dx,cs:u_Command
                mov     al,VoicesNumber
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,cs:NumberOfVoices
                dec     al
                or      al,0C0h                 ;it must be ORed with 0C0h
                out     dx,al

                mov     dx,cs:u_Status
                in      al,dx

                mov     dx,cs:u_Command
                mov     al,DMAControl
                out     dx,al
                mov     dx,cs:u_ByteIO
                in      al,dx

                mov     dx,cs:u_Command
                mov     al,SampleControl
                out     dx,al
                mov     dx,cs:u_ByteIO
                in      al,dx

                mov     dx,cs:u_Command
                mov     al,R_IRQStatus          ;command #8Fh
                out     dx,al
                mov     dx,cs:u_ByteIO
                in      al,dx


                mov     cx,32                   ;max available voices
VoiceClearLP:
                mov     dx,cs:u_ActiveVoice
                mov     al,cl
                dec     al
                out     dx,al


        ;Voice off
                mov     dx,cs:u_Command
                mov     al,VoiceMode            ;command #00h
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,11b
                out     dx,al

        ;Ramp off
                mov     dx,cs:u_Command
                mov     al,VolumeMode           ;command #0Dh
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,11b
                out     dx,al

                loop    VoiceClearLP

        ;UnMute
                mov     dx,cs:u_Command
                mov     al,Initialize
                out     dx,al
                mov     dx,cs:u_ByteIO
                mov     al,111b
                out     dx,al
                ret


                mov     dx,cs:u_Command
                mov     al,DMAControl
                out     dx,al
                mov     dx,cs:u_ByteIO
                in      al,dx

                mov     dx,cs:u_Command
                mov     al,SampleControl        ;command #49h
                out     dx,al
                mov     dx,cs:u_ByteIO
                in      al,dx

                mov     dx,cs:u_Command
                mov     al,R_IRQStatus          ;command #8Fh
                out     dx,al
                mov     dx,cs:u_ByteIO
                in      al,dx



                mov     cl,cs:NumberOfVoices
SetRampRateLoop:
                mov     dx,cs:u_ActiveVoice
                mov     al,cs:NumberOfVoices
                sub     al,cl
                out     dx,al

        ;Set ramprate
                mov     dx,cs:u_Command
                mov     al,VolRampRate
                out     dx,al
                mov     al,00111111b
                mov     dx,cs:u_ByteIO
                out     dx,al

        ;Set volume
                mov     dx,cs:u_Command
                mov     al,VoiceVolume
                out     dx,al
                mov     dx,cs:u_WordIO
                xor     ax,ax
                out     dx,ax
                dec     cl
                jnz     SetRampRateLoop

                mov     dx,cs:u_Base
                mov     ax,000b    ;linein on, output on, mic in off
                out     dx,ax

                ret

ResetUltraSound EndP

;컴 Reset UltraSound 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;

;ResetUltraSound2 Proc


;static void gus_reset(void)
;{
;        unsigned char temp;
 ;
;        outp(judas_port + 0xf, 0x5);
;        outp(judas_port, ENABLE_LINE_IN | ENABLE_OUTPUT);
;        outp(judas_port + GF1_IRQ_CTRL, 0x0);
;        outp(judas_port + 0xf, 0x0);
;        outp(judas_port, ENABLE_LINE_IN | ENABLE_OUTPUT);
;        outp(judas_port + GF1_IRQ_CTRL, gus_dmalatch[judas_dma] | 0x80);
;        outp(judas_port, ENABLE_LINE_IN | ENABLE_OUTPUT | SELECT_GF1_REG);
;        outp(judas_port + GF1_IRQ_CTRL, gus_irqlatch[judas_irq]);
;        outp(judas_port, ENABLE_LINE_IN | ENABLE_OUTPUT);
;        outp(judas_port + GF1_IRQ_CTRL, gus_dmalatch[judas_dma] | 0x80);
;        outp(judas_port, ENABLE_LINE_IN | ENABLE_OUTPUT | SELECT_GF1_REG);
;        outp(judas_port + GF1_IRQ_CTRL, gus_irqlatch[judas_irq]);
;        outp(judas_port + GF1_PAGE, 0x0);
;        outp(judas_port, ENABLE_LINE_IN | ENABLE_GF1_IRQ);
;        outp(judas_port + GF1_REG_SELECT, DMA_CONTROL);
;        outp(judas_port + GF1_DATA_HI, 0x0);
;        outp(judas_port + GF1_REG_SELECT, TIMER_CONTROL);
;        outp(judas_port + GF1_DATA_HI, 0x0);
;        outp(judas_port + GF1_REG_SELECT, SAMPLE_CONTROL);
;        outp(judas_port + GF1_DATA_HI, 0x0);
;        outp(judas_port + GF1_REG_SELECT, SET_VOICES);
;        outp(judas_port + GF1_DATA_HI, 13 | 0xc0);
;        temp = inp(judas_port + GF1_IRQ_STAT);
;        outp(judas_port + GF1_REG_SELECT, DMA_CONTROL);
;        temp = inp(judas_port + GF1_DATA_HI);
;        outp(judas_port + GF1_REG_SELECT, SAMPLE_CONTROL);
;        temp = inp(judas_port + GF1_DATA_HI);
;        outp(judas_port + GF1_REG_SELECT, GET_IRQV);
;        temp = inp(judas_port + GF1_DATA_HI);
;        for (temp = 0; temp < 32; temp++)
;        {
;                outp(judas_port + GF1_PAGE, temp);
;                outp(judas_port + GF1_REG_SELECT, SET_CONTROL);
;                outp(judas_port + GF1_DATA_HI, VOICE_STOPPED | STOP_VOICE);
;                gus_delay();
;                outp(judas_port + GF1_DATA_HI, VOICE_STOPPED | STOP_VOICE);
;                outp(judas_port + GF1_REG_SELECT, SET_VOLUME_CONTROL);
;                outp(judas_port + GF1_DATA_HI, VOLUME_STOPPED | STOP_VOLUME);
;                gus_delay();
;                outp(judas_port + GF1_DATA_HI, VOLUME_STOPPED | STOP_VOLUME);
;                outp(judas_port + GF1_REG_SELECT, SET_VOLUME);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;                outp(judas_port + GF1_REG_SELECT, SET_START_HIGH);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;                outp(judas_port + GF1_REG_SELECT, SET_START_LOW);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;                outp(judas_port + GF1_REG_SELECT, SET_END_HIGH);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;                outp(judas_port + GF1_REG_SELECT, SET_END_LOW);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;                outp(judas_port + GF1_REG_SELECT, SET_ACC_HIGH);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;                outp(judas_port + GF1_REG_SELECT, SET_ACC_LOW);
;                outpw(judas_port + GF1_DATA_LOW, 0x0);
;        }
;        temp = inp(judas_port + GF1_IRQ_STAT);
;        outp(judas_port + GF1_REG_SELECT, DMA_CONTROL);
;        temp = inp(judas_port + GF1_DATA_HI);
;        outp(judas_port + GF1_REG_SELECT, SAMPLE_CONTROL);
;        temp = inp(judas_port + GF1_DATA_HI);
;        outp(judas_port + GF1_REG_SELECT, GET_IRQV);
;        temp = inp(judas_port + GF1_DATA_HI);
;        outp(judas_port + GF1_REG_SELECT, MASTER_RESET);
;        outp(judas_port + GF1_DATA_HI, GF1_MASTER_RESET | GF1_OUTPUT_ENABLE | GF1_MASTER_IRQ);
;}
;ResetUltraSound2 EndP


;컴 UltraSound Commands 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;

;Write
VoiceMode       Equ 0h
VoiceFreq       Equ 1h
LoopStartHi     Equ 2h
LoopStartLo     Equ 3h
LoopEndHi       Equ 4h
LoopEndLo       Equ 5h
VolRampRate     Equ 6h
VolRampStart    Equ 7h
VolRampEnd      Equ 8h
VoiceVolume     Equ 9h
LoopBeginHi     Equ 0Ah ;place of sample in DRAM
LoopBeginLo     Equ 0Bh
VoiceBalance    Equ 0Ch
VolumeMode      Equ 0Dh
VoicesNumber    Equ 0Eh
DMAControl      Equ 41h
DRAMLo          Equ 43h
DRAMHi          Equ 44h
TimerControl    Equ 45h
TimerSpeed      Equ 46h
SamplingFreq    Equ 48h
SampleControl   Equ 49h
Initialize      Equ 4Ch ;very important

;Read
;it is added 80h to basic commands,
;f.example. Write BeginLocationsLo of sample is 0A
;           Read BeginLocationsLo of sample is 0A+80=8A
R_VoiceMode     Equ 80h
R_VoiceFreq     Equ 81h
R_LoopStartLo   Equ 82h
R_LoopStartHi   Equ 83h
R_LoopEndLo     Equ 84h
R_LoopEndHi     Equ 85h
R_VolRampRate   Equ 86h
R_VolRampStart  Equ 87h
R_VolRampEnd    Equ 88h
R_Volume        Equ 89h
R_LoopBeginLo   Equ 8Ah
R_LoopBeginHi   Equ 8Bh
R_VoiceBalance  Equ 8Ch
R_VolumeMode    Equ 8Dh
R_NumberVoices  Equ 8Eh
R_IRQStatus     Equ 8Fh


;컴 Divisors values 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�;

Voice14         Equ 43 ;for 14 active voices
Voice15         Equ 40
Voice16         Equ 37
Voice17         Equ 35
Voice18         Equ 33
Voice19         Equ 31
Voice20         Equ 30
Voice21         Equ 28
Voice22         Equ 27
Voice23         Equ 26
Voice24         Equ 25
Voice25         Equ 24
Voice26         Equ 23
Voice27         Equ 22
Voice28         Equ 21
Voice29         Equ 20
Voice30         Equ 20
Voice31         Equ 19
Voice32         Equ 18


;컴 Log Volume 0..64 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴;

u_vol00 Equ 00000h
u_vol04 Equ 0BC00h
u_vol08 Equ 0CD80h
u_vol10 Equ 0DE40h
u_vol18 Equ 0E6A0h
u_vol20 Equ 0EEA0h
u_vol28 Equ 0F2E0h
u_vol30 Equ 0F6E0h
u_vol38 Equ 0FAF0h
u_vol40 Equ 0FDD0h

u_Base          dw 0    ;base
u_Status        dw 0    ;base+6
u_ActiveVoice   dw 0    ;base+102h
u_Command       dw 0    ;base+103h
u_WordIO        dw 0    ;base+104h
u_ByteIO        dw 0    ;base+105h
u_DRAMIO        dw 0    ;base+107h

u_Divisor       dw Voice14
NumberOfVoices  db 14

;current voice - information
Voice           db ?
VStart          dd ?
VBegin          dd ?
VEnd            dd ?
Mode            db ?
Balance         db ?
