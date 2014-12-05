; mode 13h setpixel macro
; ax = x
; bx = y
; es = dest segment

lsetpixel    Macro

        push    bx
        push    cx

        xchg    bh,bl
        mov    cx,bx
        shr    bx,1
        shr    bx,1
        add    bx,cx
        add    bx,ax
        push   ax
        mov    al,line_color
        mov    byte ptr es:[bx],al
        pop    ax

        pop    cx
        pop    bx

        Endm

;    BresenhamLine:
;      ax = x1 start position
;      bx = y1
;      cx = x2 destination position
;      dx = y2


BresenhamLine    Proc    near

        mov    si,1            ; initial y direction
        mov    di,1            ; and x direction
        sub    dx,bx            ; subtract y1 from y2
        jge    storey            ; skip if y2-y1 is nonnegative
        neg    si            ; negative y direction
        neg    dx            ; absolute value of y2-y1
storey:
        mov    deldy,si
        sub    cx,ax            ; subtrack x1
        jge    storex            ; skip if x2-x1 is postive
        neg    di            ; make negative x direction
        neg    cx            ; absolute value of cx
storex:
        mov    deldx,di
        cmp    cx,dx            ; compare dels with delp
        jge    setdiag            ; skip if straight move in x direction
        xor    di,di            ; zap x update
        xchg    cx,dx            ; exchange differences
        jmp    storedelsxy
setdiag:
        xor    si,si            ; zap y update
storedelsxy:
        mov    delsx,di
        mov    delsy,si
        shl    dx,1            ; get delp and * 2
        mov    bp,dx            ; change if straight move
        sub    dx,cx            ; 2*delp-dels
        mov    di,dx            ; initial value
        sub    dx,cx
        mov    si,dx
        inc    cx            ; increment count

drawline:
        lsetpixel

        cmp    di,0            ; determine straight or diagonal
        jge    @@diagonal
@@straight:
        add    ax,delsx        ;update x
        add    bx,delsy        ;update y
        add    di,bp            ;update error term
        loop    drawline
        jmp    short @@3        ; all done
@@diagonal:
        add    ax,deldx        ;update x
        add    bx,deldy        ;update y
        add    di,si            ;update error term
@@2:
        loop    drawline
@@3:

        ret

line_color db 63
delsx        dw    ?
delsy        dw    ?
deldx        dw    ?
deldy        dw    ?

BresenhamLine    Endp
