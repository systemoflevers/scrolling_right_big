INCLUDE "hardware.inc"
        
SECTION "Memory", ROM0

clearOAM::
        ;; Modifies:
        ;;  hl
        ;;  a
        ld hl, $FE00

.write_loop:
        ld [hl], 0              ; Can't use hli since that needs a?
        inc hl
        ;; Only check the low address byte (l).
        ;; OAM is $FE00 - $FE9F so the high byte (h) is always $FE.
        ld a, l
        cp $A0                  ; 1 past $9F.
        jr nz, .write_loop
        ret

memcpy::
        ;; Args:
        ;;   b: number of bytes
        ;;   hl: destination
        ;;   de: source
        ;; Modifies:
        ;;   a, b, hl, de
        xor a ; set a to 0
        cp b
        ret z

.loop
        ld a, [de]
        inc de
        ld [hli], a
        dec b
        jr nz, .loop
        ret

memcpy_big::
        ;; Uses an r16 for # of bytes to copy.
        ;; Args:
        ;;   bc: number of bytes
        ;;   hl: destination
        ;;   de: source
        ;; Modifies:
        ;;   a, bc, hl, de
        xor a ; set a to 0

        ;; If b is 0 then use memcpy instead.
        ;; Make sure to move c to b.
        cp b
        jr nz, :+
        ld b, c
        jr memcpy
:
        cp c
        jr nz, .loop
        ;; b is non-zero and c is zero

.loop
        ld a, [de]
        inc de
        ld [hli], a
        dec bc
        xor a
        cp b
        jr nz, .loop
        ;; We can use memcpy now.
        ld b, c
        ;; C should never be zero here.
        ;; The only way it could is if B was zero before the "dec bc", which can't happen.
        jr memcpy.loop