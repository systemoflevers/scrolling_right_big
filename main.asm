INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

EntryPoint:
        di ; Disable interrupts. That way we can avoid dealing with them, especially since we didn't talk about them yet :p
        jp Start

REPT $150 - $104
    db 0
ENDR

SECTION "Game code", ROM0

Start:
.waitVBlank
  ld a, [rLY]
  cp 144 ; Check if the LCD is past VBlank
  jr c, .waitVBlank
  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.

  ld bc, TileSet.end - TileSet
  ld hl, _VRAM8000
  ld de, TileSet
  call memcpy_big

  ld c, 0
  ld hl, TileMap
  ld de, _SCRN0
  ld a, 0
  ;;ld a, $1
  ld [vCurrentBankHigh], a
  ld [rROMB1], a
  ld a, $01
  ;;ld a, $FF
  ld [vCurrentBankLow], a
  ld [rROMB0], a


  ;;ld HL, $7E5E
.tile_map_copy
  call copy_column
  inc c
  ld a, 21
  cp c
  jr z, .finish_setup
  ld d, HIGH(_SCRN0)
  ld e, c
  jr .tile_map_copy

.finish_setup
  ld d, h
  ld e, l
  ld hl, vNextColumnAddress
  ld a, d
  ld [hli], a
  ld [hl], e

  ;; Set BG palette.
  ld hl, $FF47
  ld [hl], %11100100


  ;; Disable interupts but set the VBlank IE flag so that I 
  ;; can use halt to wait for VBlank.
  di
  ld a, %00000001
  ld [rIE], a



  xor a
  ld [vNeedToCopy], a

  ld [vLastSCXCopy], a

  ;; turn screen back on
  ld a, %10010001
  ld [rLCDC], a
.mainLoop
  xor a
  ld [rIF], a

  ld a, [vNextColumnAddress]
  ld h, a
  ld a, [vNextColumnAddress + 1]
  ld l, a

  ;; Check if scx has moved into the next tile.
  ;; We can know this when scx ends in 000
  ld a, [rSCX]
  ld e, a
  ld a, [vLastSCXCopy]
  cp e
  jr z, .checkDPad_halt
  ld a, e
  and %00000111
  jr nz, .checkDPad_halt

  ld a, e
  ld [vLastSCXCopy], a
  add (20 << 3)
  srl a
  srl a
  srl a
  ld d, HIGH(_SCRN0)
  ld e, a
  halt
  call copy_column
  ld d, h
  ld e, l

  ;; check if we've reached the end of the ROM bank.
  ;; We know when de == $7FFC
  ld a, $FC
  cp e
  jr nz, .storeColumnAddress
  ld a, $7F
  cp d
  jr nz, .storeColumnAddress
  ;; set de to $4000 so the next column is from the start of the next bank.
  ld d, $40
  ld e, 0

  ld a, [vCurrentBankLow]
  ld l, a
  ld a, [vCurrentBankHigh]
  ld h, a
  inc hl
  ;; check if we overflowed the bank numbers. Max bank is $1FF
  ld a, 2
  cp h
  jr nz, .storeNewBank
  ;; New current bank is $001
  ld hl, $0001
.storeNewBank
  ld a, h
  ld [vCurrentBankHigh], a
  ld [rROMB1], a
  ld a, l
  ld [vCurrentBankLow], a
  ld [rROMB0], a


.storeColumnAddress
  ld hl, vNextColumnAddress
  
  ld a, d

  ld [hli], a
  ld [hl], e
  jr .checkDPad

.checkDPad_halt
  halt
.checkDPad
.checkRiht
  ld a, P1F_GET_DPAD
  ld [rP1], a
  ld a, [rP1]
  ld hl, rSCX
  bit 0, a
  jr nz, .checkLeft
  inc [hl]
.checkLeft
;;  bit 1, a
;;  jr nz, .checkUp
;;  dec [hl]
;;.checkUp
;;  ld hl, rSCY
;;  bit 2, a
;;  jr nz, .checkDown
;;  dec [hl]
;;.checkDown
;;  bit 3, a
;;  jp nz, .mainLoop
;;  inc [hl]
  jp .mainLoop


copy_column::
  ;; Args:
  ;;   de: destination
  ;;   hl: source
  ;; Modifies:
  ;;   a, b, hl, de
  ld b, 18
.copy_loop
  ld a, [hli]
  ld [de], a
  dec b
  ret z ;; b is 0, done copy

  ld a, 32
  add e
  ld e, a
  jr nc, .copy_loop
  inc d
  jr .copy_loop


SECTION "Tile Set", ROM0
TileSet:
  incbin "numbers.gbgfx"
.end

FOR N, 0, $1FF

SECTION "Tile Map {N}", ROMX,BANK[N + 1]
IF N == 0
TileMap:
ENDC
TileMap\@:
  incbin "long_map.tilemap", N * 16380, 16380
.end
ENDR
TileMapEnd:

SECTION "Work RAM", wram0
vNextColumnAddress:
  ds 2
vNeedToCopy:
  ds 1
vLastSCXCopy:
  ds 1
vCurrentBankLow:
  ds 1
vCurrentBankHigh:
  ds 1