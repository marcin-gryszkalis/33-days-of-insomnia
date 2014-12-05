; 33 days of insomnia, 4K intro
; (c) 1998 Marcin Gryszkalis aka dagoon of cryogen

.486p
.487
                Assume cs:_Code, ds:_Code, es:_Code

_Code           Segment Para    Public  'Code'  Use16

                org     100h

Start:          mov     ax,cs
                mov     ds,ax
                mov     es,ax

                mov     dx,offset Introt
                mov     ah,9
                int     21h

                mov     dx,offset setGUSt
                mov     ah,9
                int     21h

                call    CheckULTRA
                jnz     GUSok

                mov     dx,offset GUSfailt
                mov     ah,9
                int     21h
                mov     ah,4Ch
                int     21h

GUSok:          call    SearchBasePort
                mov     NumberOfVoices,14
                mov     u_Divisor,Voice14
                call    ResetUltraSound

                xor     ax,ax
                xor     bx,bx
                xor     cx,cx

; setup voices
l_setb:         mov     Voice,cl         ;0..31
                mov     bx,cx
                mov     bl,tr_pann[bx]
                mov     Balance,bl
                call    SetVoiceBalance


ls1:            mov     bx,cx
                shl     bx,1
                mov     ax,tr_vol[bx]           ;0..64 = #volume
;                shr     ax,mastervol_shr
;                mov     bx,mastervol_mul
;                mul     bx    ; m/n (master vol level)
                mov     bx,ax
                call    SetVoiceVolume

                mov     dx,offset dott
                mov     ah,9
                int     21h

                inc     cx
                cmp     cx,channels
                jne     l_setb

                mov     dx,offset GUSOKt
                mov     ah,9
                int     21h

; --- create samples
; 1. melody smp

                mov     di,Smp_addr
                mov     ax,60h;7fh
                mov     cx,20h
                rep stosb
                mov     ax,0b0h;80h
                mov     cx,20h
                rep stosb

; 2. drum sample

                mov     di,Smp_addr+2
                mov     bx,10h  ; divisor

                mov     cx,1   ; i counter (1..1000h = smp len)
crs0:           mov     ax,01000h  ; l (ax) = ( (1000h - i (cx) )/10h (bx)
                sub     ax,cx
                mov     bx,10h
                div     bl     ; ah=rem, al=quotient
;                and     ax,ffh
                mov     var_l,al ; l now ok

                call    mrrandom
                and     ax,00ffh

                cmp     cx,180h    ; first 180h bytes = 0 of ff (accent)
                jnb     crs5
                mov     var_l,0feh ; can't be ff (causes 80s only
                                   ;when un/sig'ed)
                jmp     crs6

crs5:           cmp     al,var_l  ; if (ax<=l) (random<=l (bound))
                jbe     crs1    ; then crs1

crs6:           mov     bx,ax   ; this creates sustain
                and     bx,1    ; if 0 bit set in bx (50-50 chance)
                jz      crs2   ; then crs2
                xor     ax,ax   ; else ax=0
                jmp     crs1
crs2:           mov     al,var_l    ;or ax=l
                and     ax,00ffh

crs1:           add     ax,128 ; convert sig/unsig (+128 z przeniesieniem)
                add     al,ah
                stosb

                inc     cx
                cmp     cx,1000h
                jne     crs0

; 3. Bass drum [sfires]
                mov      di,Smp_addr+4
                finit
                mov       dword ptr [counter],1
                fldpi
                fadd      st(0),st
                fsub      dword ptr [_2pi_d_360]
                fstp      dword ptr [_2pi]

GLup01:
                fild      dword ptr [counter]
                fmul      dword ptr [_025]
                fadd      dword ptr [_09]
                fmul      dword ptr [_t1]
                fdivr     dword ptr [_2pi]
                fstp      dword ptr [delta]

                fldz
                ;fstp      dword ptr [d]

GLup02:         ;fld       dword ptr [d]
                fadd      dword ptr [delta]
                fst       dword ptr [d]

                fsin
                fld1
                fadd

                fild      dword ptr [counter]
                fld1
                fsub
                fadd      st(0),st
                fsubr     dword ptr [_127]
                fmulp     st(1),st
                fistp     dword ptr [samp]
                mov       ax, word ptr [samp]
                add     ax,128 ; convert sig/unsig (+128 z przeniesieniem)
                add     al,ah
                stosb
                fld       dword ptr [d]
                fcom      dword ptr [_2pi]  ; until <2pi (nie <=) ma byc
                fnstsw    ax
;                sahf
;                jb        GLup02
                shr       ah,1
                jc        GLup02
                inc       dword ptr [counter]
                cmp       dword ptr [counter],64
                jle       GLup01

; 1b (4). melody smp

                mov     di,Smp_addr+6
                finit
cr4_2:          mov     cx,32
                fld     dword ptr [_0]
                fst     dword ptr [_4p]
cr4_1:          fsin
                fmul    dword ptr [_a0]
                fistp   dword ptr [samp]
                mov     ax, word ptr [samp]
;                add     ax,128 ; convert sig/unsig (+128 z przeniesieniem)
;                add     al,ah
                stosb
                fld     dword ptr [_4p]
                fadd    dword ptr [_2pi_d_32]
                fst     dword ptr [_4p]
                loop    cr4_1
                fistp   dword ptr [samp] ; clean up the stack
                fld     dword ptr [_a0]
                fsub    dword ptr [_0_5]
                fstp    dword ptr [_a0]
                inc     counter2
                cmp     counter2,128
                jb      cr4_2

; --- upload samples

x1:             mov     bx,Smp_gp ; these are word ptrs
                mov     cx,Smp_size
                mov     si,Smp_addr
                call    RAM2DRAM

                mov     bx,Smp_gp+2
                mov     cx,Smp_size+2
                mov     si,Smp_addr+2
                call    RAM2DRAM

                mov     bx,Smp_gp+4
                mov     cx,Smp_size+4
                mov     si,Smp_addr+4
                call    RAM2DRAM

                mov     bx,Smp_gp+6
                mov     cx,Smp_size+6
                mov     si,Smp_addr+6
                call    RAM2DRAM

; prepare font bitmapes

                mov      si,offset cfont
                mov      di,offset bfont

                xor      ax,ax
                mov      cx,32*16 ; 32 letters * 16 lines

font_cr0:       lodsb

                mov      bl,al  ; save font byte
                mov      dx,128

                push     cx
                mov      cx,8

font_cr1:       mov      al,bl
                and      ax,dx
                shl      ax,1
                shr      ax,cl

                cmp      ax,1
                jne      font_cr2
                shl      ax,8 ; text_color! = 255 here
                dec      ax

font_cr2:       stosb
                shr      dx,1
                loop     font_cr1

                pop     cx
                loop    font_cr0

; create rcos, rsin
                finit

                fld     dword ptr [_0]
                fst     dword ptr [_4p] ; current angle
                mov     di,offset rcos
                mov     cx,360

cr_rcos:        fcos
                fmul    dword ptr [_120]
                fadd    dword ptr [_120]
                fistp    dword ptr [samp]
                mov     ax, word ptr [samp]
                stosb

                fld     dword ptr [_4p]
                fadd    dword ptr [_2pi_d_360]
                fst     dword ptr [_4p]

                loop    cr_rcos

                fistp   dword ptr [samp] ; clean up the stack

                fld     dword ptr [_0]
                fst     dword ptr [_4p] ; current angle
                mov     di,offset rsin
                mov     cx,360

cr_rsin:        fsin
                fmul    dword ptr [_100]
                fadd    dword ptr [_100]
                fistp    dword ptr [samp]
                mov     ax, word ptr [samp]
                stosb

                fld     dword ptr [_4p]
                fadd    dword ptr [_2pi_d_360]
                fst     dword ptr [_4p]

                loop    cr_rsin

                fistp   dword ptr [samp] ; clean up the stack

; ON gfx mode!
                mov     ax,13h
                int     10h

; set up palette
                mov     ax,0
                mov     di,offset palette
                mov     cx,256
cr_pal1:        stosb
                stosb
                stosb
                inc     ax
                and     ax,00111111b
                loop    cr_pal1
                call    makepal

; START!

                call    inittimer
; ----------------
;    MAIN LOOP
; ----------------

                mov     line_color,127

kloop:          cmp     end_of_song,1
                je      end0

kl1:
                cmp     gretxcount,gretx_no
                jne     kl2
                mov     gretx_p,offset gretx
                mov     gretxcount,0

kl2:
                call    fx4 ; lines
                call    fx5 ; lines 2

                cmp     c_patterns,0
                jne     kl3
                call    fx1 ; 'dagoon presents'

kl3:            cmp     c_patterns,1
                jne     kl4
                call    fx2 ; '33 days...'

kl4:            cmp     c_patterns,2
                jb      kl5
                call    fx3 ; greetings

kl5:
klend:          mov     ax,Scr1Seg
                mov     fs,ax
                call    blurcopy

                in      al, 060h ; check key
                cmp     al, 1
                jne     kloop


; finish it...

end0:           call    deinittimer

                call    ResetUltraSound

                mov     ax,03h
                int     10h

                mov     dx,offset quittxt
                mov     ah,9
                int     21h

endabs:         mov     ax,04c00h
                int     21h


; PROCies

inittimer:      cli
                xor     ax,ax
                mov     fs,ax
                mov     eax,fs:[8*4]
                mov     oldint8,eax
                mov     ax,cs
                shl     eax,16
                mov     ax,OFFSET intti8
                mov     fs:[8*4],eax

; bx=K where K=1193181/F(Hz?)
; eg. K=1193181/100 = 2E9B for 100Hz :) check later...

                mov     bx, timer_freq
                mov     al,036h    ; counter 0
                out     43h, al
                mov     al,bl
                out     40h,al
                mov     al,bh
                out     40h,al
                sti
                ret

deinittimer:
                xor     ax,ax
                mov     fs,ax
                mov     eax,oldint8
it1:            cli
                mov     fs:[8*4],eax
                mov     al,036h
                out     43h,al
                xor     ax,ax
                out     40h,al
                out     40h,al
                sti
                ret

;wait for Vretrace
VR              PROC
                push    eax edx
                mov     dx,03dah
@1v:            in      al,dx
                test    al,8
                jz      @1v
@2v:            in      al,dx
                test    al,8
                jnz     @2v
                pop     edx eax
                ret
VR              ENDP

fx1             PROC
                call    mrrandom
                and     ax,7 ; = <0,7>
                add     ax,80
                mov     pt_row,ax

                call    mrrandom
                and     ax,7
                add     ax,((320-(17*8))/2)-3
                mov     pt_col,ax

                mov     bx,offset mx2
                mov     cx,17
                call    puttext


                call    mrrandom
                and     ax,7
                add     ax,100
                mov     pt_row,ax

                call    mrrandom
                and     ax,7
                add     ax,((320-(8*8))/2)-3
                mov     pt_col,ax

                mov     bx,offset mx3
                mov     cx,8
                call    puttext
                ret
fx1             ENDP

fx2             PROC
                call    mrrandom
                and     ax,7 ; = <0,7>
                add     ax,80
                mov     pt_row,ax

                call    mrrandom
                and     ax,7
                add     ax,((320-(7*8))/2)-3
                mov     pt_col,ax

                mov     bx,offset mx4
                mov     cx,7
                call    puttext


                call    mrrandom
                and     ax,7
                add     ax,100
                mov     pt_row,ax

                call    mrrandom
                and     ax,7
                add     ax,((320-(11*8))/2)-3
                mov     pt_col,ax

                mov     bx,offset mx5
                mov     cx,11
                call    puttext
                ret
fx2             ENDP

fx3             PROC
                mov     pt_row,184
                mov     pt_col,314-gretx_len*8
                mov     bx,gretx_p
                mov     cx,gretx_len
                call    puttext
                ret
fx3             ENDP

fx4             PROC
                xor     cx,cx
                xor     dx,dx

                mov     ax,Scr1Seg ; 0a000h
                mov     es,ax

                mov     bx,alpha1
                mov     cl,rcos[bx] ;rcos <0,240>
                add     cx,40 ; 240 + 2x = 320 -> x=20
lt1:            mov     dl,rsin[bx] ;rsin <0,200>
;                add     dx,20 ; 160/2 + x = 200/2 -> x=20
lt2:            mov     ax,160
                mov     bx,100
                call    BresenhamLine
                ret
fx4             ENDP

fx5             PROC
                xor     cx,cx
                xor     dx,dx

                mov     ax,Scr1Seg ; 0a000h
                mov     es,ax

                call    mrrandom
                cmp     ax,198
                jna     fx5_1
                shr     ax,1
fx5_1:          mov     bx,ax
;                add     ax,1
                mov     dx,ax

                mov     ax,40
                mov     cx,280
                call    BresenhamLine
                ret
fx5             ENDP


; puts text
; in: ds:bx  - text
;     cx     - text length in letters
;     pt_col - col
;     pt_row - row

puttext         PROC

ftf1:           xor     ax,ax
                mov     al,byte ptr ds:[bx]
                mov     si,ax ; now si = ascii code
                sub     si,40h ; now A = 1 etc.
                shl     si,7  ; *8 (kolumn) *16 (wierszy) => *128
                add     si,offset bfont

                mov     ax,Scr1Seg; 0a000h
                mov     es,ax

                mov     ax,pt_row
                mov     dx,320
                mul     dx
                mov     di,ax

                add     di,pt_col

                push    cx

                mov     cx,16
ftf:            push    cx
                mov     cx,8
ftf3:           lodsb
                cmp     al,0
                je      ftf4
                stosb
                jmp     ftf2
ftf4:           inc     di
ftf2:           loop    ftf3

                pop     cx
                add     di,320-8 ; nast.wiersz
                loop    ftf

                inc     bx ; next letter
                add     pt_col,8 ; next cols

                pop     cx
                loop    ftf1
                ret
puttext         ENDP

pt_col          dw 0
pt_row          dw 0

playmusic       PROC
                cmp     end_of_song,1 ; no loop mode now :)
                je      pl_end

                xor     ecx,ecx

                mov     si,tr_note_p

pl0:            ; every track in line loop
                xor     eax,eax
                xor     ebx,ebx

                lodsw  ;read note (from ds:si = ds:tr_note_p); si++

                mov     bl,c_trax
                mov     Voice,bl ; voice no = track no (uses by gus routines)

                mov     di,patt_p
                cmp     byte ptr patt[di+bx],1   ; check pattern table
                jne     pl2                ; skip this track

                cmp     ax,nnn ; note_skip
                je      pl_nxttr

                cmp     ax,n__ ; note_cut
                jne     pl1

pl2:            call    StopVoice
                jmp     pl_nxttr

pl1:            call    SetVoiceFrequency

                mov     cl,smp_mode[bx]
                mov     Mode,cl

                mov     bl,tr_ins[bx] ; instr for this track
                xor     bh,bh
                shl     bl,1    ;now bx=instr_no*2 (word indexing)
                mov     cx,Smp_gp[bx]
                mov     VStart,ecx
                mov     VBegin,ecx
                mov     cx,Smp_size[bx]
                add     cx,Smp_gp[bx]
                mov     VEnd,ecx
                call    PlayVoice

pl_nxttr:
                inc     c_trax  ; next track in line
                cmp     c_trax,channels  ; check for the end of line
                jne     pl0 ; play next track (all tracks in 1 interrupt)

                ; flash the border!
                xor     ebx,ebx
                mov     bl, c_line
                mov     ax, 1001h ; set border color function
                mov     bh, flash[bx]
                int     10h

                ; e-o-line (last track) reached
                mov     c_trax,0 ; back to first track
                add     tr_note_p,channels*2 ; but in next pattern line
                inc     c_line

                mov     al,c_line ;= <0,31>
                and     al,11b
                xor     al,10b
                jnz     ppp1
                inc     gretxcount
                add     gretx_p,gretx_len
ppp1:
                cmp     c_line,patt_len ; check for the end of pattern
                jne     pl_end

                ; e-o-p reached
                mov     tr_note_p,offset tr_note ;set note_pointer to s-o-pat
                mov     c_line,0 ; back to first line
                add     patt_p,channels  ; but in next pattern
                inc     c_patterns  ; yaeh, in next pattern
                cmp     c_patterns,song_len  ; check for the end of song
                jne     pl_end

                ; e-o-song reached
                mov     patt_p,0 ; just for no loop mode...
                inc     end_of_song ; set end_of_song flag
pl_end:         ret
playmusic       ENDP

; MAIN TIMER INTERRUPT

intti8  PROC FAR

                pusha
; setup alpha1
                add     alpha1,2

                call    mrrandom
                and     ax,15
                cmp     ax,1
                jne     al0

                call    mrrandom
                and     ax,0ffh
                add     alpha1,ax

al0:            cmp     alpha1,360
                jb      al1
                sub     alpha1,360

al1:            inc     msxcount

                cmp     msxcount,pl_speed
                jb      al2
                mov     msxcount,0
                call    playmusic

al2:            mov  al,20h ; needed for timer work
                out  20h,al
                popa
                iret
intti8  ENDP


;Random: returns random value, written by Mrock/Hellcore

mrrandom        PROC
                push    bx
                mov     bx,cs:[randseed2]
                mov     ax,cs:[randseed]
                add     bx,3132h
                add     ax,0f1adh
                rol     ax,2
                mov     cs:[randseed],ax
                ror     bx,1
                add     bx,ax
                mov     cs:[randseed2],bx
                add     ax,bx
                pop     bx
                ret
mrrandom        ENDP

randseed        dw      0abcdh
randseed2       dw      1234h

; copy to have motion blur :)
; fs - source segment
; es - destination segment

blurcopy        PROC
                mov     di,0
                mov     si,0
                mov     ax,0a000h
                mov     es,ax

                mov     cx,320*200

blc0:           mov     al,fs:[si]
                cmp     al,0
                jne     blc1
                mov     bl,es:[di]
                mov     al,bl
                and     bl,11000000b ; bl keeps pal quarter (0-3)
                and     al,00111111b
                shr     al,1         ; and we shift only 6 bytes
                or      al,bl        ; and restore quarter

blc1:           stosb
                inc     si ; !!!! for mov ax,fs:[]
                loop    blc0

; cleanup buffer
                xor     eax,eax
                mov     di,ax
                mov     cx,320*200/4
                mov     dx,fs
                mov     es,dx
                rep     stosd

                ret
blurcopy        ENDP

makepal         PROC
                xor     bx,bx
                mov     si,offset palette

makepal1:       mov     dx,03c8h
                mov     al,bl
                out     dx,al
                inc     dx
                mov     cx,3
                rep     outsb  ; dx!
                inc     bx
                cmp     bx,100h
                jne     makepal1
                ret

makepal         ENDP

;----------------------
Include davga.inc
Include dagus_i.inc
Include dagus_n.inc

alpha1          dw      0

oldint8         dd      0
timer_freq      Equ     1193181/70
pl_speed        Equ     8 ; play once every x interrupts (so now ? times/s)
msxcount        db      0


introt          db      'GUS micro replayer by dagoon/cryogen',13,10,'$'
setGUSt         db      'GUS Setup$'
GUSfailt        db      'No GUS found, sorry...',13,10,'$'
GUSOKt          db      'OK',13,10,'$'
dott            db      '.$'
LFt             db      13,10,'$'

; @ = ' '
; ^ = '3'
; _ = '9'
; [ = '2'
; ] = '7'

mx2     db 'DAGOON@OF@CRYOGEN' ; 17
mx3     db 'PRESENTS'          ; 8
mx4     db '^^@DAYS'           ; 7
mx5     db 'OF@INSOMNIA'       ; 11

 gretx_p dw offset gretx - 16*gretx_len ; 16 were skipped during1st and 2nd
                                        ; screens!
gretx_no equ 26
gretx_len equ 9
gretxcount db 0
gretx   db '@@@@@@DSP'
        db '@@@@@@FMC'
        db '@@@@@@IPC'
        db '@@@@@@TRB'
        db '@@@@@FUSE'
        db '@@@@@MIST'
        db '@@@@PULSE'
        db '@@@DEFECT'
        db '@@@DRAGON'
        db '@@@ENENZI'
        db '@@@LOGRUS'
        db '@@@POISON'
        db '@@@SIXTY_'
        db '@@@TUBE[]'
        db '@@@VISION'
        db '@@LIBERTY'
        db '@@OUTSIDE'
        db '@@SHADOWS'
        db '@@SUSPEND'
        db '@HELLCORE'
        db '@INTERROR'
        db '@KOROZONE'
        db '@NORFERIN'
        db '@THE@GRID'
        db 'BRYGADARR'
        db 'HYPNOTIZE'


quittxt db '   ‹‹   ‹‹  ',13,10
        db '     €    €   DAYS OF INSOMNIA',13,10
        db '   ﬂﬂ‹  ﬂﬂ‹   code, music, gfx and design by DAGOON oF CRY0GEN',13,10
        db '   ‹‹ﬂ  ‹‹ﬂ   4KB intro compo entry for ???????? party',13,10,'$'


var_l           db      0
;var_k           db      0

c_line          db 0
c_trax          db 0

c_patterns      db 0

patt_p          dw 0
tr_note_p       dw offset tr_note

patt_len        Equ 20h ; in lines
song_len        Equ 14h ; in patterns

end_of_song     db 0 ; boolean

channels        Equ 12
mastervol_shr   Equ 2 ; 3/4, sound is distorted when vol=100%
mastervol_mul   Equ 3
;mastervol_shr   Equ 1 ; 3/4, sound is distorted when vol=100%
;mastervol_mul   Equ 2

; instrument no. is constant for each track
tr_ins  db 0,0,3,0,0,0,1,2,3,3,0,0

        ;  l l c h h a d d c c b a
patt    db 0,0,0,0,0,0,0,0,0,0,1,0
        db 0,0,1,0,0,0,0,1,1,1,1,0
        db 0,0,1,1,1,0,1,1,1,1,1,0
        db 1,1,1,1,1,0,1,1,1,1,1,0

        db 1,1,1,1,1,1,1,1,1,1,1,0
        db 1,1,1,1,1,1,1,1,1,1,1,0
        db 1,1,1,1,1,1,1,1,1,1,1,0
        db 1,1,1,1,1,1,1,1,1,1,1,0

        db 1,1,1,1,1,0,1,1,1,1,1,1
        db 1,1,1,1,1,0,1,1,1,1,1,1
        db 1,1,1,1,1,0,1,1,1,1,1,1
        db 0,0,1,0,0,0,0,0,1,1,1,1

        db 1,1,1,1,1,1,1,1,1,1,1,0
        db 1,1,1,1,1,1,1,1,1,1,1,0
        db 1,1,1,1,1,1,1,1,1,1,1,0
        db 1,1,1,1,1,1,1,1,1,1,1,0

        db 1,1,1,1,1,1,0,0,1,1,1,0
        db 1,1,0,1,1,1,0,0,0,0,0,0
        db 1,1,0,1,1,0,0,0,0,0,0,0
        db 1,1,0,0,0,0,0,0,0,0,0,0

; volume
tr_vol          dw u_vol20, u_vol10, u_vol30, u_vol08
                dw u_vol04, u_vol18, u_vol18, u_vol30
                dw u_vol30, u_vol30, u_vol28, u_vol18

; panning
tr_pann         db 1,15,10,14,2,6,8,8,9,8,7,6

; mode: 0 - normal, 8 - loop

                ;  l l c h h a d d c c b a
Smp_mode        db 8,8,0,8,8,8,0,0,0,0,8,8


               ;   led  led+ ch1  hil  hil+ add  dr1  dr2  ch2  ch3  bass add
tr_note         label word
                dw c_4, nnn, n__, c_7, a@6, nnn, n__, g_5, n__, n__, c_3, c_5 ;00
                dw nnn, c_4, n__, n__, c_7, nnn, n__, n__, n__, n__, n__, nnn
                dw n__, nnn, c_5, c_7, n__, g_5, c_7, n__, d@5, g_5, c_4, d@5
                dw g_4, n__, nnn, g_7, c_7, nnn, n__, n__, nnn, nnn, c_4, nnn
                dw nnn, g_4, n__, n__, g_7, nnn, c_5, g_5, n__, n__, c_3, g_5
                dw n__, nnn, n__, c_7, n__, nnn, n__, n__, n__, n__, n__, nnn
                dw f@4, n__, c_5, f@7, c_7, f@5, c_7, n__, d@5, g_5, c_4, c_5
                dw nnn, f@4, nnn, n__, f@7, nnn, n__, n__, nnn, nnn, c_4, nnn
                dw n__, nnn, n__, c_7, n__, nnn, n__, g_5, n__, n__, c_3, d@5 ;08
                dw n__, n__, n__, g_7, c_7, nnn, n__, n__, n__, n__, n__, nnn
                dw c_5, n__, c_5, n__, g_7, g_5, c_7, n__, d@5, g_5, c_4, g_5
                dw nnn, c_5, nnn, c_7, n__, nnn, n__, n__, nnn, nnn, c_4, nnn
                dw g@4, nnn, n__, g@7, c_7, nnn, c_5, g_5, n__, n__, c_3, c_5
                dw g_4, g@4, n__, n__, g@7, nnn, n__, n__, n__, n__, n__, nnn
                dw g@4, g_4, c_5, c_7, n__, g@5, c_7, g_5, d@5, g_5, c_4, d@5
                dw nnn, g@4, nnn, f_7, c_7, nnn, c_5, n__, nnn, nnn, c_4, nnn
                dw d@4, nnn, n__, a@6, f_7, nnn, n__, g_5, n__, n__, a@3, a@4 ;10
                dw nnn, d@4, n__, n__, a@6, nnn, n__, n__, n__, n__, n__, nnn
                dw n__, nnn, a@4, a@6, n__, d@5, c_7, n__, d_5, f_5, a@3, d_5
                dw g_4, n__, nnn, f_7, a@6, nnn, n__, n__, nnn, nnn, a@3, nnn
                dw nnn, g_4, n__, n__, f_7, nnn, c_5, g_5, n__, n__, a@2, f_5
                dw n__, nnn, n__, a@6, n__, nnn, n__, n__, n__, n__, n__, nnn
                dw a@4, n__, a@4, d@7, a@6, d_5, c_7, n__, d_5, f_5, a@3, a@4
                dw nnn, a@4, nnn, n__, d@7, nnn, n__, n__, nnn, nnn, a@3, nnn
                dw n__, nnn, n__, c_7, n__, nnn, n__, g_5, n__, n__, d@3, d@5 ;18
                dw n__, n__, n__, g_7, c_7, nnn, n__, n__, n__, n__, n__, nnn
                dw d@5, n__, d@5, n__, g_7, a@4, c_7, n__, g_5, a@5, d@4, g_5
                dw nnn, d@5, nnn, c_7, n__, nnn, n__, n__, nnn, nnn, d@4, nnn
                dw a@4, nnn, n__, g@7, c_7, nnn, c_5, g_5, n__, n__, d_3, a@5
                dw a_4, a@4, n__, n__, g@7, nnn, n__, n__, n__, n__, n__, nnn
                dw a@4, a_4, d@5, c_7, n__, a@5, c_5, n__, g_5, a@5, d_4, d@5
                dw nnn, a@4, nnn, a@6, c_7, nnn, c_5, g_5, nnn, nnn, d_4, nnn


Flash           db 63,32,0,0,63,32,0,0,63,32,0,0,63,32,0,0
                db 63,32,0,0,63,32,0,0,63,32,0,0,63,32,0,0

Smp_addr  dw offset S1
          dw offset S2
          dw offset S3
          dw offset S4

Smp_gp dw 0h ;pos in gus dram
       dw 100h
       dw 2000h
       dw 6000h

Smp_size  dw 040h
          dw 1000h
          dw 2800h
          dw 1000h

; bassdr gen.
_FNSTSW  EQU  0dfh,0e0h

counter2  db   0
bassdrum  dw   ?

align 4
counter   dd   0
_0        dd   0.0
_1        dd   1.0
_0_5      dd   0.5
_127      dd   127.0
_100      dd   100.0
_120      dd   120.0
_80       dd   80.0
_09       dd   0.9
_025      dd   0.25
_t1       dd   20.0
_a0       dd   100.0 ;127.0
_2pi_d_32 dd 0.0981746875
_2pi_d_360 dd 0.017452777777777778
d         dd   ?
_2pi      dd   ?
_4p       dd   0.0
delta     dd   ?
samp      dd   ?

; font data

cfont           label byte
Include c.inc

bfont           Label   Byte
                db 32*8*16 dup (?)

palette   db      3*256 dup(?)

; r*cos, r*sin for r=? :)
rcos      db    360 dup (?)
rsin      db    360 dup (?)

; sample data

;smd_id          db 'smp'

align 2
S1              Label   Word
                db 40h dup (?)

align 2
S2              Label   Word
                db 1002h dup (?)

align 2
S3              Label   Word
                db 4000h dup (?)

align 2
S4              Label   Word
                db 2000h dup (?)

_Code           EndS

SEGMENT         Scr1Seg
LABEL           scrb1   byte
                db      320*200 dup(?)
ENDS

SEGMENT         StackSeg Stack
                db      04000h dup(?)
;idd             db 'END'
ENDS
                End     Start
