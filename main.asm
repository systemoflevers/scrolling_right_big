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

;; Busy loop to poll until VBlank is reached.
.waitVBlank
  ld a, [rLY]
  cp 144 ; Check if the LCD is past VBlank (vblank starts at line 144)
  jr c, .waitVBlank ; c: carry flag means 144 > [rLY], so we haven't reached 144, so loop.

  ;; Turn off the display.
  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.

  ;; Load the tile data from ROM to VRAM at address $8000
  ld bc, TileSet.end - TileSet
  ld hl, _VRAM8000
  ld de, TileSet
  call memcpy_big

  ;; Initialize the ROM bank to $001 and WRAM variables to keep track of it.
  ld a, 0
  ;;ld a, $1 ; for testing
  ld [vCurrentBankHigh], a
  ld [rROMB1], a
  ld a, $01
  ;;ld a, $FF ; for testing
  ld [vCurrentBankLow], a
  ld [rROMB0], a

  ;; Load the first chuck of the tile map from ROM to the _SCRN0 VRAM tile map.
  ld c, 0        ; counter for number of columns copied
  ld hl, TileMap ; source address of tile map data
  ;;ld HL, $7E5E ; for testing
  ld de, _SCRN0  ; destination address of tile map data
.tile_map_copy_loop
  call copy_column
  inc c
  ld a, 21
  cp c
  jr z, .finish_setup ;; copied all the columns, exit the loop
  ;; Since we're copying by column, the start address for the destination of
  ;; the Cth column is _SCRN0 + C. _SCRN0 is $9800 so load $98 into D and
  ;; C into E for DE to have the correct address.
  ld d, HIGH(_SCRN0)
  ld e, c
  jr .tile_map_copy_loop

.finish_setup
  ;; Store the next column's starting source address from HL to RAM at
  ;; vNextColumnAddress.
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



  ;; Initialize variable to keep track of the scX value when we last copied a
  ;; column. This is to make sure we don't repeatedly run the copy code if the
  ;; screen is kept at a position that triggered a copy.
  xor a
  ld [vLastSCXCopy], a

  ;; turn the screen back on with background enabled
  ld a, %10010001
  ld [rLCDC], a

.mainLoop
  ;; clear rIF so that the next halt will wait for a new VBlank. Otherwise halt
  ;; doesn't do anything while rIF and rIE are set.
  xor a
  ld [rIF], a

  ;; See if we need to load in a new column of tile map data.
  ;; Doing this first because it's the more important part to do in VBlank.

  ;; HL = next column address
  ld a, [vNextColumnAddress]
  ld h, a
  ld a, [vNextColumnAddress + 1]
  ld l, a

  ;; Check if scx has moved into the next tile.
  ;; First check if the current screen position is the same as the position
  ;; when we last copied a column.
  ld a, [rSCX]
  ld e, a
  ld a, [vLastSCXCopy]
  cp e
  jr z, .checkDPad_halt ; Same position as last time, no need to copy.
  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, e
  and %00000111
  jr nz, .checkDPad_halt ; nz means scX had a 1 in its lower 3 bits.

  ;; We're going to be copying another column so update vLastSCXCopy now while
  ;; we have the current scX in E.
  ld a, e
  ld [vLastSCXCopy], a
  ;; scX refers to the left edge of the screen but we care about the right so
  ;; add screen width in tiles * tile width in pixels (20 * 8) to get the x 
  add (20 * 8)
  ;; We need the tile map column. For a give X value that's floor(X / 8). That's
  ;; the same as shifting right 3 times (using logical shift)
  srl a
  srl a
  srl a
  ;; The destination address for the column copy is _SCRN0 + A (A has the tile
  ;; map column we're copying to). Since _SCRN0 is $9800, we set D to $98 and E
  ;; to A.
  ld e, a
  ld d, HIGH(_SCRN0)
  ;; DE now has the destination address.

  halt ; Wait for VBlank.
  call copy_column

  ;; Backup the next ROM address to copy from from HL to DE because I need HL.
  ld d, h
  ld e, l

  ;; Check if we've reached the end of the ROM bank.
  ;; We know when de == $7FFC (= $4000 + (floor($4000 / 18) * 18))
  ld a, $FC
  cp e
  jr nz, .storeColumnAddress ; not equal so store the address as is
  ld a, $7F
  cp d
  jr nz, .storeColumnAddress ; not equal so store the address as is
  ;; At the end of the ROM bank so reset the next column address in DE to $4000
  ;; so the next column is from the start of the next bank.
  ld d, $40
  ld e, 0

  ;; Go to the next ROM bank.
  ld a, [vCurrentBankLow]
  ld l, a
  ld a, [vCurrentBankHigh]
  ld h, a
  inc hl
  ;; Check if we overflowed the bank numbers. Max bank is $1FF so an overflow
  ;; would give $200. So we only need to check if H is 2.
  ld a, 2
  cp h
  jr nz, .storeNewBank ;; It's not 2 so store the bank number.
  ;; New current bank is $001
  ld hl, $0001
.storeNewBank
  ;; Store the bank number to RAM and update the the Memory Bank Controller.
  ld a, h
  ld [vCurrentBankHigh], a
  ld [rROMB1], a
  ld a, l
  ld [vCurrentBankLow], a
  ld [rROMB0], a


  ;; Now we can store that column address we put in DE way back when!
.storeColumnAddress
  ld hl, vNextColumnAddress
  ld a, d
  ld [hli], a
  ld [hl], e
  jr .checkDPad

.checkDPad_halt
  halt
.checkDPad
.checkRight
  ld a, P1F_GET_DPAD
  ld [rP1], a
  ld a, [rP1]
  ld hl, rSCX
  bit 0, a
  jp nz, .mainLoop ; Wasn't pressed so loop.
  inc [hl] ; It was pressed so increment [rSCX]
  jp .mainLoop


copy_column::
  ;; Copy a column of 18 tile indexes to a destination tile map column.
  ;; The source tile indexes must be stored sequencially.
  ;; Args:
  ;;   de: destination
  ;;   hl: source
  ;; Modifies:
  ;;   a, b, hl, de

  ;; Copying 18 tiles per column.
  ld b, 18
.copy_loop
  ld a, [hli]
  ld [de], a
  dec b
  ret z ;; b is 0, done copy

  ;; The tile map is stored rows, so the next position to copy to is 32 bytes
  ;; from the last position.
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
  ;; Where to read the next column of tile indexes from.
  ds 2
vLastSCXCopy:
  ;; The value of scX the last time a column was copied. This is to know if the
  ;; column for the current position has already been copied.
  ds 1
vCurrentBankLow:
  ;; Low byte of the current ROM bank.
  ds 1
vCurrentBankHigh:
  ;; High byte of the current ROM bank. Using an MBC5 so this will be 0 or 1.
  ds 1