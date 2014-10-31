; original by T$, optimized 52->32 by Rrrola <rrrola@gmail.com>
; twisted by divip

org 100h          ; assumes: ax=0 bx=0 di=0FFFEh si=0100h

mov  al,13h
int  10h
lds  bx,[bx]      ; bx=20CDh ds=9FFFh
M:
mov  dl,[bx]
add  dl,[bx+di]
dec  bx
add  dl,[bx+si+64]
cmp  [bx],dl
adc  [bx],ah      ; if ([bx] < ah) [bx]++
xchg ax, bx
mov  cx, -27  ; pseudorandom generator: bx = -27*bx-1
imul cx
xchg ax, bx

in   al,60h       ; (2) standard ESC check
dec  al           ; (2)
jnz  M            ; (2)
ret               ; (1)

