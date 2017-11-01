unit c64;

//------------------------------------------------------------------------------
//Commodore C-64 GFX files manipulation Delphi class, v1.3
//(c)1994,1995, 2009-2011, 2017 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC
//E-mail: monster@Noniewicz.com
//WWW: http://www.Noniewicz.com
//Licence: BSD 2-Clause License
//------------------------------------------------------------------------------
//History:
//created: somewhere in 1994-1995
//updated: 20091231 ????-????
//updated: 20100101 ????-????
//updated: 20110510 ????-????
//updated: 20171029 1715-2040
//updated: 20171029 2150-2250
//updated: 20171030 2200-2215
//updated: 20171030 2240-2310
//updated: 20171101 1630-2030
//updated: 20171101 2105-2130
//updated: 20171101 2200-2255

{todo:
# MAIN:
- fnt/fntb/logo/mob - colors issue? (proper params)
- ftn/fntb/mob - more/misc (eg get given one, hires v multi)
.- *FLI formats
# NEXT:
- Lazarus friendly (laz demo too, compile+check on Linux)
- separate load and bmp/canvas pack
- more palletes
- bfli seems cut?
# LATER:
- add misc limit checks
- also open source c64pas app
- pack back to C64 formats and write (colormap?)
- ZX analogue?
- C code version (so more portable) ?
- evem more exotic formats?
- see this
  http://fileformats.archiveteam.org/wiki/Commodore_graphics_formats
  http://codebase64.org/doku.php?id=base:c64_grafix_files_specs_list_v0.03
  https://www.c64-wiki.com/wiki/Graphics_Modes
}

{CHANGELOG:
# v1.0
- base stuff, old version in Turbo Pascal, so ancient 
# v1.1
- slightly rewritten for Delphi (then named mob64.pas)
# v1.2
- radical code cleanup
- everything as class
- amica code integrated
- demo app
- misc
# v1.3
- added FLI formats (experimental, unfinished)
- universal loader method (file extension based)
- misc fixes/changes
}

interface

uses Windows, SysUtils, Classes, Graphics, Dialogs;

type
     MOBdata = record
                 cnt: byte;
                 mob: array[1..100, 0..63] of byte;
               end;
     FNTBdata = record
                  cnt: byte;
                  fntb: array[1..255, 0..31] of byte;
                end;
     FNTdata = record
                 cnt: byte;
                 fnt: array[1..255, 0..7] of byte;
               end;
     LOGOdata = record
                  logo: array[0..$2000-$1800-1] of byte;
                  bitmap: array[0..$2800-$2000-1] of byte;
                end;
     KOALAdata = record
                   bitmap: array[0..$7f40-$6000-1] of byte;
                   ink1: array[0..$8328-$7f40-1] of byte;
                   ink2: array[0..$8710-$8328-1] of byte;
                   backGr: byte;
                 end;
     HIRESdata = record
                   bitmap: array[0..$3f40-$2000-1] of byte;
                   ink: array[0..$8328-$7f40-1] of byte;
                 end;
(* unused
     AMICAdata = record
                   len: word;
                   bmp: array[0..40*256-1] of byte;
                 end;
*)
     FLIdata = record //generic, any FLI
                 gfxmem: array[0..16384-1] of byte;
                 chrmem: array[0..7, 0..2048-1] of byte;
                 colmem: array[0..1024-1] of byte;
                 bgcol: array[0..256-1] of byte;
               end;
     IFLIdata = record //IFLI
                  gfxmem1: array[0..8192-1] of byte;
                  gfxmem2: array[0..8192-1] of byte;
                  chrmem1: array[0..8192-1] of byte;
                  chrmem2: array[0..8192-1] of byte;
                  colmem: array[0..1024-1] of byte;
                end;


TC64Loader = procedure(ca: TCanvas) of object;

TAmicaBuff = array[0..32767] of byte;

TC64FileType = (C64_UNKNOWN, C64_KOALA, C64_HIRES, C64_AMICA,
                C64_LOGO, C64_FNT, C64_FNTB, C64_MOB, C64_MBF,
                C64_FLI, C64_AFLI, C64_BFLI, C64_IFLI, C64_FFLI);

TC64 = class(TObject)
private
  f: file of byte;
  color1_: byte;
  FLastError: string;
  function GenericLoader(FileName: string; callback: TC64Loader; ca: TCanvas): integer;

  procedure KOALAshow(koala: KOALAdata; ca: TCanvas);
  procedure HIRESshow(hires: HIRESdata; ca: TCanvas);
  procedure LOGOshow(logo: LOGOdata; ca: TCanvas; color0, color1, color2, color3: byte);
  procedure FNTshow(x0, y0: integer; fnt: FNTdata; ca: TCanvas; cnt: byte);
  procedure FNTBshow(x0, y0: integer; fntb: FNTBdata; ca: TCanvas; cnt: byte; color1: byte);
  procedure hMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte; color1: byte);
  procedure mMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte; color1: byte);
  procedure FLIshow(fli: FLIdata; ca: TCanvas; mode: TC64FileType);
  procedure IFLIshow(ifli: IFLIdata; ca: TCanvas);

  procedure KOALAload(ca: TCanvas);
  procedure HIRESload(ca: TCanvas);
  procedure AMICAload(ca: TCanvas);
  procedure AMICAunpack(i_buff: TAmicaBuff; var o_buff: TAmicaBuff);
  procedure AMICA2KOALA(o_buff: TAmicaBuff; var koala: KOALAdata);
  procedure LOGOload(ca: TCanvas);
  procedure FNTload(ca: TCanvas);
  procedure FNTBload(ca: TCanvas);
  procedure MOBloadHires(ca: TCanvas);
  procedure MOBloadMulticolor(ca: TCanvas);
  procedure FLIload(ca: TCanvas);  
public
  function GetC64Color(index: integer): TColor;
  function ExtMapper(ext: string): TC64FileType;
  function LoadKoalaToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadHiresToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadAmicaToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadLogoToBitmap(FileName: string; ca: TCanvas; color1: byte): integer;
  function LoadFontToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadFont2x2ToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadMobToBitmap(FileName: string; ca: TCanvas; hires: boolean): integer;
  function LoadFliToBitmap(FileName: string; ca: TCanvas): integer;

  function LoadC64ToBitmap(FileName: string; ca: TCanvas): integer;  
published
  property LastError: string read FLastError;
end;


implementation

//VICE pallete ccs64.vpl
//black,white,red,cyan,magenta,green,blue,yellow
//orange,brown,pink,dk.gray,gray,lt.green,lt.blue,lt.gray
const
  _r: array[0..15] of byte = (0,$ff,$e0,$60,$e0,$40,$40,$ff, $e0,$9c,$ff,$54,$88,$a0,$a0,$c0);
  _g: array[0..15] of byte = (0,$ff,$40,$ff,$60,$e0,$40,$ff, $a0,$74,$a0,$54,$88,$ff,$a0,$c0);
  _b: array[0..15] of byte = (0,$ff,$40,$ff,$e0,$40,$e0,$40, $40,$48,$a0,$54,$88,$a0,$ff,$c0);

  pow: array[0..7] of byte = (1, 2, 4, 8, 16, 32, 64, 128);



function TC64.GetC64Color(index: integer): TColor;
begin
  if not (index in [0..15]) then
    result := 0
  else
    result := RGB(_r[index], _g[index], _b[index]);
end;

function TC64.ExtMapper(ext: string): TC64FileType;
var e: string;
begin
  e := uppercase(ext);
  result := C64_UNKNOWN;

  if e = '.KOA' then result := C64_KOALA;
  if (e = '.PIC') or (e = '.ART') or (e = '.OCP') then result := C64_HIRES;
  if (e = '.[B]') or (e = '.AMI') then result := C64_AMICA; //note: '[B]' invented here
  if e = '.GFX' then result := C64_LOGO;  //note: invented here
  if e = '.FNT' then result := C64_FNT;
  if e = '.FNB' then result := C64_FNTB;  //note: invented here
  if e = '.MOB' then result := C64_MOB;   //note: invented here
  if e = '.MBF' then result := C64_MBF;   //note: invented here
  if e = '.FLI' then result := C64_FLI;
  if (e = '.AFLI') or (e = '.AFL') then result := C64_AFLI;
  if e = '.BFLI' then result := C64_BFLI;
  if e = '.FFLI' then result := C64_FFLI;
  if (e = '.IFLI') or (e = '.IFL') then result := C64_IFLI;
end;

function TC64.GenericLoader(FileName: string; callback: TC64Loader; ca: TCanvas): integer;
var err: boolean;
begin
  result := -1;
  FLastError := 'Required parameters not assigned.';
  if not assigned(callback) or not assigned(ca) then exit;

  try
    AssignFile(f, FileName);
    reset(f);
    try
      callback(ca);
      err := false;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        err := true;
      end;
    end;
    CloseFile(f);
    if err then
      raise(Exception.Create(FLastError));
    result := 0;
    FLastError := '';
  except
    on E: Exception do FLastError := E.Message;
  end;
end;

//---

procedure TC64.KOALAshow(koala: KOALAdata; ca: TCanvas);
var x, y, bit, c0, c1, c2, c3, bt, bt1, vl, vl1, vl2: byte;
    c: TColor;
    ndx, ndx2: integer;
begin
  if not assigned(ca) then exit;

  c0 := koala.backGr and $0f; //?
  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      ndx := x+y*40;
      ndx2 := x*8+y*320;
      c1 := (koala.ink1[ndx] and $0f);        //&5c00
      c2 := (koala.ink1[ndx] and $f0) shr 4;
      c3 := (koala.ink2[ndx] and $0f);        //&d800
      for bt := 0 to 7 do
      begin
        bt1 := koala.bitmap[ndx2+bt];
        for bit := 3 downto 0 do
        begin
          vl1 := ((bt1 and pow[bit*2]) div pow[bit*2]);
          vl2 := ((bt1 and pow[bit*2+1]) div pow[bit*2+1]);
          vl := vl1+2*vl2;
          case vl of
            3: c := RGB(_r[c3], _g[c3], _b[c3]);
            2: c := RGB(_r[c1], _g[c1], _b[c1]);
            1: c := RGB(_r[c2], _g[c2], _b[c2]);
            else c := RGB(_r[c0], _g[c0], _b[c0]);
          end;
          ca.Pixels[x*8+7-2*bit, (y*8+bt)] := c;
          ca.Pixels[x*8+8-2*bit, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.HIRESshow(hires: HIRESdata; ca: TCanvas);
var x, y, bit, cc, c1, c2, bt, bt1: byte;
    c: TColor;
begin
  if not assigned(ca) then exit;

  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      cc := hires.ink[x+y*40];
      c1 := (cc and $0f);
      c2 := (cc and $f0) shr 4;
      for bt := 0 to 7 do
      begin
        bt1 := hires.bitmap[x*8+y*320+bt];
        for bit := 7 downto 0 do
        begin
          if (bt1 and pow[bit]) = pow[bit] then
            c := RGB(_r[c2], _g[c2], _b[c2])
          else
            c := RGB(_r[c1], _g[c1], _b[c1]);
          ca.Pixels[x*8+8-bit, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.LOGOshow(logo: LOGOdata; ca: TCanvas; color0, color1, color2, color3: byte);
var x, y, bit, bt1, bt2, vl, vl1, vl2, bt: byte;
    c: TColor;
begin
  if not assigned(ca) then exit;

  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      bt1 := logo.logo[x+y*40];
      for bt := 0 to 7 do
      begin
        bt2 := logo.bitmap[bt1*8+bt];
        for bit := 3 downto 0 do
        begin
          vl1 := ((bt2 and pow[bit*2]) div pow[bit*2]);
          vl2 := ((bt2 and pow[bit*2+1]) div pow[bit*2+1]);
          vl := vl1+2*vl2;
          case vl of
            0: c := RGB(_r[color0], _g[color0], _b[color0]);
            1: c := RGB(_r[color1], _g[color1], _b[color1]);
            2: c := RGB(_r[color2], _g[color2], _b[color2]);
            3: c := RGB(_r[color3], _g[color3], _b[color3]);
          end;
          ca.Pixels[x*8+7-2*bit, (y*8+bt)] := c;
          ca.Pixels[x*8+8-2*bit, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.FNTshow(x0, y0: integer; fnt: FNTdata; ca: TCanvas; cnt: byte);
var y, bit, bt, vl: byte;
    c: TColor;
begin
  if not assigned(ca) then exit;

  for y := 0 to 7 do
  begin
    bt := fnt.fnt[cnt, y];
    for bit := 0 to 7 do
    begin
      vl := bt and pow[bit];
      if vl = 0 then
        c := 0
      else
        c := RGB(255, 255, 255);
      ca.pixels[x0+8-bit, y0+y] := c;
    end;
  end;
end;

procedure TC64.FNTBshow(x0, y0: integer; fntb: FNTBdata; ca: TCanvas; cnt: byte; color1: byte);
var x, y, bit, bt, vl1, vl2, vl, c: byte;
    cl: TColor;
begin
  if not assigned(ca) then exit;

  for y := 0 to 15 do
    for x := 0 to 1 do
    begin
      if y >= 8 then c := 16+y-8 else c := y;
      bt := fntb.fntb[cnt, x*8+c];
      for bit := 3 downto 0 do
      begin
        vl1 := (bt and pow[bit*2]) div pow[bit*2];
        vl2 := (bt and pow[bit*2+1]) div pow[bit*2+1];
        vl := vl1+2*vl2;
        cl := RGB(_r[vl], _g[vl], _b[vl]);
        ca.Pixels[x0+x*8+7-2*bit, y0+y] := cl;
        ca.Pixels[x0+x*8+8-2*bit, y0+y] := cl;
      end;
    end;
end;

procedure TC64.hMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte; color1: byte);
var x, y, bit, bt, vl : byte;
    cl: TColor;
begin
  if not assigned(ca) then exit;

  for y := 0 to 20 do
    for x := 0 to 2 do
    begin
      bt := mob.mob[cnt, x+y*3];
      for bit := 7 downto 0 do
      begin
        vl := (bt and pow[bit]) div pow[bit];
        cl := RGB(_r[vl], _g[vl], _b[vl]);
        ca.Pixels[x0+x*8+7-bit, y0+y] := cl;
      end
    end;
end;

procedure TC64.mMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte; color1: byte);
var x, y, bit, bt, vl1, vl2, vl: byte;
    cl: TColor;
begin
  if not assigned(ca) then exit;

  for y := 0 to 20 do
    for x := 0 to 2 do
    begin
      bt := mob.mob[cnt,x+y*3];
      for bit := 3 downto 0 do
      begin
        vl1 := (bt and pow[bit*2]) div pow[bit*2];
        vl2 := (bt and pow[bit*2+1]) div pow[bit*2+1];
        vl := vl1+2*vl2;
        cl := RGB(_r[vl], _g[vl], _b[vl]);
        ca.Pixels[x0+x*8+7-2*bit, y0+y] := cl;
        ca.Pixels[x0+x*8+8-2*bit, y0+y] := cl;
      end;
    end;
end;

//based on C code from C64Gfx by Pasi 'Albert' Ojala

procedure TC64.FLIshow(fli: FLIdata; ca: TCanvas; mode: TC64FileType);
const bitmask: array[0..3] of byte = ($c0, $30, $0c, $03);
      bitshift: array[0..3] of byte = ($40, $10, $04, $01);
var x, y, ind, pos, bits, ysize, xsize: integer;
    a, b: byte;
begin
  ysize := 200;
  xsize := 160;
  b := 0;

  if (mode = C64_BFLI) then ysize := 400;
  if (mode = C64_AFLI) then xsize := 320;

  for y := 0 to ysize-1 do
  begin
    if (mode = C64_AFLI) then
    begin
      for x := 0 to 320-1 do
      begin
        ind := x div 8 + (y div 8)*40;  //color memory index
        pos := (y mod 8) + (x div 8)*8 + (y div 8)*320;  //gfx memory byte
        bits := 7 - (x mod 8);  //bit numbers
        a := (fli.gfxmem[pos] shr bits) and 1;
        if (x < 24) then
            b := $f
        else
        begin
          if (a <> 0) then
            b := fli.chrmem[y mod 8][ind] div 16
          else
            b := fli.chrmem[y mod 8][ind] mod 16;
        end;
        ca.Pixels[x, y] := RGB(_r[b], _g[b], _b[b]);
      end;
    end
    else
    begin
      for x := 0 to 160-1 do
      begin
        ind := x div 4 + (y div 8)*40;		//color memory index
        pos := (y mod 8)+ (x div 4)*8 + (y div 8)*320;	//gfx memory byte
        bits := (x mod 4);			//bit numbers
        a := (fli.gfxmem[pos] and bitmask[bits]) div bitshift[bits];
        if (mode = C64_FLI) then
        begin
          case a of
            0: b := fli.bgcol[y+6];
            1: if (x < 12) then b := 15 else b := fli.chrmem[y mod 8][ind] div 16;
            2: if (x < 12) then b := 15 else b := fli.chrmem[y mod 8][ind] mod 16;
            3: if (x < 12) then b := 9 else b := fli.colmem[ind] mod 16;
            else b := 0;
          end;
        end
        else if (mode = C64_BFLI) then
        begin
          case a of
            0: b := 0;
            1: if (x < 12) then b := 15 else b := fli.chrmem[y mod 8][ind] div 16;
            2: if (x < 12) then b := 15 else b := fli.chrmem[y mod 8][ind] mod 16;
            3: if (x < 12) then b := 9 else b := fli.colmem[(ind and 1023)]mod 16;
            else b := 0;
          end;
        end;
        ca.Pixels[x, y] := RGB(_r[b], _g[b], _b[b]);
      end;
    end;
  end;
end;

procedure TC64.IFLIshow(ifli: IFLIdata; ca: TCanvas);
const bitmask: array[0..3] of byte = ($c0, $30, $0c, $03);
      bitshift: array[0..3] of byte = ($40, $10, $04, $01);
var x, y, ind, pos, bits, memind, i: integer;
    a0, a1: byte;
    c0, c1: byte;
    buffer: array[0..3*321] of byte;    
begin
  c0 := 0;
  c1 := 0;
  for y := 0 to 200-1 do
  begin
    buffer[0] := 0;
    buffer[1] := 0;
    buffer[2] := 0;
    for x := 0 to 160-1 do
    begin
      ind := x div 4 + (y div 8)*40;  //color memory index
      pos := (y mod 8) + (x div 4)*8 + (y div 8)*320;  //gfx memory byte
      bits := (x mod 4);  //bit numbers
      memind := 1024*(y mod 8) + ind;
	    a0 := (ifli.gfxmem1[pos] and bitmask[bits]) div bitshift[bits];
	    a1 := (ifli.gfxmem2[pos] and bitmask[bits]) div bitshift[bits];
      case a0 of
        0: c0 := 0;
        1: c0 := ifli.chrmem1[memind] div 16;
        2: c0 := ifli.chrmem1[memind] mod 16;
        3: c0 := ifli.colmem[ind] and $0f;
      end;
      case a1 of
        0: c1 := 0;
        1: c1 := ifli.chrmem2[memind] div 16;
        2: c1 := ifli.chrmem2[memind] mod 16;
        3: c1 := ifli.colmem[ind] and $0f;
      end;

	    buffer[6*x+0] := (buffer[6*x+0] + _r[c0]) div 2;
	    buffer[6*x+1] := (buffer[6*x+1] + _g[c0]) div 2;
	    buffer[6*x+2] := (buffer[6*x+2] + _b[c0]) div 2;

	    buffer[6*x+3] := (_r[c1] + _r[c0]) div 2;
	    buffer[6*x+4] := (_g[c1] + _g[c0]) div 2;
	    buffer[6*x+5] := (_b[c1] + _b[c0]) div 2;

	    buffer[6*x+6] := _r[c1];
	    buffer[6*x+7] := _g[c1];
	    buffer[6*x+8] := _b[c1];
    end;
    for x := 0 to 320-1 do
      ca.Pixels[x, y] := RGB(buffer[x*3+0], buffer[x*3+1], buffer[x*3+2]);
  end;
end;

//---

procedure TC64.KOALAload(ca: TCanvas);
var koala: KOALAdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  read(f, none, none);
  for g := 0 to $7f40-$6000-1 do read(f, koala.bitmap[g]);
  for g := 0 to $8328-$7f40-1 do read(f, koala.ink1[g]);
  for g := 0 to $8710-$8328-1 do read(f, koala.ink2[g]);
  read(f, koala.backGr);
  KOALAshow(koala, ca);
end;

procedure TC64.HIRESload(ca: TCanvas);
var HIRES: HIRESdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  {9009 bytes - what's that!?}
  read(f, none, none);
  for g := 0 to $3f40-$2000-1 do read(f, hires.bitmap[g]);
  for g := 0 to $4328-$3f40-1 do read(f, hires.ink[g]);
  HIRESshow(hires, ca);
end;

procedure TC64.AMICAload(ca: TCanvas);
var koala: KOALAdata;
//    AMICA: AMICAdata;
//    g: word;
//    none: byte;
    i_buff, o_buff: TAmicaBuff;
    b, i: integer;
    c: byte;
begin
  if not assigned(ca) then exit;

(*
  amica.len := filesize(f)-2;
  read(f, none, none);
  for g := 0 to amica.len-1 do read(f, amica.bmp[g]);
*)

  b := 0;
  while not eof(f) and (b < 32768) do
  begin
    read(f, c);
    i_buff[b] := c;
    inc(b);
  end;

  for i := 0 to high(o_buff) do o_buff[i] := 0;
  AMICAunpack(i_buff, o_buff);
  AMICA2KOALA(o_buff, koala);
  KOALAshow(koala, ca);
end;

//------------------------------------------------------------------------------
//NOTE: that used to be old amica.pas file
//'AMICA PAINT' C-64 FORMAT SCREEN UNPACKER
//FROM MY ORIGINAL 'SHOWPIX' RESOURCED BY MONSTER/GDC (c)1992
//6502 ASM -> PAS coversion (c)2009 MONSTER/GDC, Noniewicz.com
//this code is kinda lame (labels) because it's direct translation from ASM
//created: 20091230
//updated: 20171029 (nice(r) code, as part of this object)
//------------------------------------------------------------------------------

procedure TC64.AMICAunpack(i_buff: TAmicaBuff; var o_buff: TAmicaBuff);
label unpack, hop, ret2;
var i, x, a: byte;
    _FBC, _FDE: integer;

    procedure SUB1; begin o_buff[_FBC] := a; INC(_FBC); end;
    procedure SUB2; begin a := i_buff[_FDE]; INC(_FDE); end;
begin
  _FBC := 0;
  _FDE := 0+2;
unpack:
  SUB2;
  if a = $c2 then goto hop;
  SUB1;
  goto unpack;
hop:
  SUB2;
  if a = 0 then goto ret2;
  x := a;
  SUB2;
  for i := 1 to x do SUB1;
  goto unpack;
ret2:
end;

procedure TC64.AMICA2KOALA(o_buff: TAmicaBuff; var koala: KOALAdata);
var i: integer;
begin
  for i := 0 to 8000-1 do koala.bitmap[i] := o_buff[i]; //320*200/8 = 8000
  for i := 0 to 1000-1 do koala.ink1[i] := o_buff[8000+i];   
  for i := 0 to 1000-1 do koala.ink2[i] := o_buff[8000+1000+i]; //$D800
  koala.backGr := o_buff[$F710-$c000] and $0f;
end;

//---

procedure TC64.LOGOload(ca: TCanvas);
var logo: LOGOdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  read(f, none, none);
  for g := 0 to $1c00-$1800-1 do
    read(f,logo.logo[g]);
  for g := $1800-$1800 to $2000-$1c00-1 do
    read(f,none);
  for g := $2000-$2000 to $2800-$2000-1 do
    read(f,logo.bitmap[g]);
  LOGOshow(logo, ca, 3, 1, 6, 0); //todo: you know what!
end;

procedure TC64.FNTload(ca: TCanvas);
var fnt: FNTdata;
    g, h: byte;
begin
  if not assigned(ca) then exit;

  for g := 1 to 255 do for h := 0 to 7 do fnt.fnt[g, h] := 0;

  read(f, g, g);
  for g := 1 to 64 do
    for h := 0 to 7 do
      read(f, fnt.fnt[g, h]);
  for h := 1 to 40 do
    FNTshow(h*8-8, 0, fnt, ca, (h));
  for h := 1 to 24 do
    FNTshow(h*8-8, 8, fnt, ca, (40+h));
end;

procedure TC64.FNTBload(ca: TCanvas);
var fntb: FNTBdata;
    g, h, none: byte;
begin
  if not assigned(ca) then exit;

  read(f, none, none);
  for g := 1 to 64 do for h := 0 to 7 do read(f, fntb.fntb[g, h]);
  for g := 1 to 64 do for h := 0 to 7 do read(f, fntb.fntb[g, h+8]);
  for g := 1 to 64 do for h := 0 to 7 do read(f, fntb.fntb[g, h+16]);
  for g := 1 to 64 do for h := 0 to 7 do read(f, fntb.fntb[g, h+24]);

  for h := 1 to 20 do
    FNTBshow(h*16-16, 0, fntb, ca, (h), color1_);
  for h := 1 to 20 do
    FNTBshow(h*16-16, 16, fntb, ca, (20+h), color1_);
  for h := 1 to 20 do
    FNTBshow(h*16-16, 32, fntb, ca, (40+h), color1_);
  for h := 1 to 4 do
    FNTBshow(h*16-16, 48, fntb, ca, (60+h), color1_);
end;

procedure TC64.MOBloadHires(ca: TCanvas);
var mob: MOBdata;
    g: byte;
begin
  if not assigned(ca) then exit;

  mob.cnt := 1;
  read(f, g, g);
  while not eof(f) do
  begin
    for g := 0 to 63 do
    begin
      if not eof(f) then
        read(f, mob.mob[mob.cnt, g])
      else
        mob.mob[mob.cnt, g] := 0;
    end;
    inc(mob.cnt)
  end;
  dec(mob.cnt);

  for g := 1 to 13 do
    if g <= mob.cnt then
      hMOBshow(g*24-24, 0, mob, ca, (g), color1_); //todo: more
end;

procedure TC64.MOBloadMulticolor(ca: TCanvas);
var mob: MOBdata;
    g: byte;
begin
  if not assigned(ca) then exit;

  mob.cnt := 1;
  read(f, g, g);
  while not eof(f) do
  begin
    for g := 0 to 63 do
    begin
      if not eof(f) then
        read(f, mob.mob[mob.cnt, g])
      else
        mob.mob[mob.cnt, g] := 0;
    end;
    inc(mob.cnt)
  end;
  dec(mob.cnt);

  for g := 1 to 13 do
    if g <= mob.cnt then
      hMOBshow(g*24-24, 0, mob, ca, (g), color1_); //todo: more
end;

procedure TC64.FLIload(ca: TCanvas);
var fli: FLIdata;
    ifli: IFLIdata;
    temp: array[0..9] of byte;
    i, j: integer;
begin
  if not assigned(ca) then exit;

  read(f, temp[0], temp[1]);

	if (temp[0] = 0) and (temp[1] = $40) then //AFLI file
  begin
    showmessage('DEBUG: AFLI detected');
    for j := 0 to 7 do
      for i := 0 to 1023 do
        read(f, fli.chrmem[j][i]); //buff 2048 but 1024 loaded
    i := 0;
    while not eof(f) and (i < 16384) do // >= 8000 but max 16384
    begin
      read(f, fli.gfxmem[i]);
      inc(i);
    end;
    //if (i >= 8000) - ok, else file too short
    FLIshow(fli, ca, C64_AFLI);
    exit;
  end;

	if (temp[0] = 0) and ((temp[1] = $3b) or (temp[1] = $3c)) then  //FLI
  begin
    showmessage('DEBUG: FLI detected');  
    if (temp[1] = $3b) then //FLI file with background colors
    begin
      for i := 0 to 255 do read(f, fli.bgcol[i]);
    end;
    if (temp[1] = $3c) then //FLI file without background colors
    begin
      for i := 0 to 255 do fli.bgcol[i] := 0;
    end;
    for i := 0 to 1023 do
      read(f, fli.colmem[i]);
    for j := 0 to 7 do
      for i := 0 to 1023 do
        read(f, fli.chrmem[j][i]); //buff 2048 but 1024 loaded
    for i := 0 to 8000-1 do
      read(f, fli.gfxmem[i]);
    //if (i>=8000) ???
    FLIshow(fli, ca, C64_FLI);
    exit;
  end;

  if (temp[0] = $ff) and (temp[1] = $3b) then //BFLI or FFLI
  begin
    read(f, temp[2]);
    if (temp[2] = ord('b')) then //BFLI file
    begin
      showmessage('DEBUG: BFLI detected');

      for i := 0 to 1023 do read(f, fli.colmem[i]);  //Color mem is strange? why?

      //1st FLI color mem
      for i := 0 to 1000-1 do read(f, fli.chrmem[0][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[1][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[2][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[3][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[4][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[5][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[6][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 1000-1 do read(f, fli.chrmem[7][i]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);

      //1st gfx mem
      for i := 0 to 8000-1 do read(f, fli.gfxmem[i]);
      for i := 0 to 192-1 do read(f, fli.bgcol[i]);

      //2nd FLI color mem
      for i := 0 to 976-1 do read(f, fli.chrmem[0][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[0][i+1000]);
      
      for i := 0 to 976-1 do read(f, fli.chrmem[1][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[1][i+1000]);

      for i := 0 to 976-1 do read(f, fli.chrmem[2][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[2][i+1000]);

      for i := 0 to 976-1 do read(f, fli.chrmem[3][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[3][i+1000]);

      for i := 0 to 976-1 do read(f, fli.chrmem[4][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[4][i+1000]);

      for i := 0 to 976-1 do read(f, fli.chrmem[5][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[5][i+1000]);

      for i := 0 to 976-1 do read(f, fli.chrmem[6][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[6][i+1000]);

      for i := 0 to 976-1 do read(f, fli.chrmem[7][i+1024]);
      for i := 0 to 24-1 do read(f, fli.bgcol[i]);
      for i := 0 to 24-1 do read(f, fli.chrmem[7][i+1000]);

      //2nd gfx mem
      for i := 0 to 7808-1 do read(f, fli.gfxmem[i+8192]);
      for i := 0 to 192-1 do read(f, fli.bgcol[i]);
      for i := 0 to 192-1 do read(f, fli.gfxmem[i+8000]);

      FLIshow(fli, ca, C64_BFLI);
      exit;
    end
    else if (temp[2] = ord('f')) then //FFLI
    begin
      showmessage('DEBUG: FFLI detected');
      raise(Exception.Create('Can not convert FFLI pictures (yet)'));
    end
    else
    begin //unknown
      raise(Exception.Create('Unknown FLI file format ID '+inttostr(temp[2])));
    end;
  end;

(* Gunpaint IFLI format
		Start address = $4000, end = $c341
		$4000 - $6000     FLI screenmaps 1
		$6000 - $7f40     FLI bitmap 1
		$8000 - $8400     Colourmap  ($d800 colours)
		$8400 - $a400     FLI screenmaps 2
		$a400 - $c340     FLI bitmap 2
		$c341             ???   (doesn't seem to be important..)
*)

//IFLI based on: C64 Horizontal 'Interlaced' FLI By Pasi 'Albert' Ojala © 1991-1998

  if (temp[0] = $0) and (temp[1] = $3f) then //IFLI
  begin
    read(f, temp[2]);
    if (temp[2] = ord('I')) then //IFLI file
    begin
      showmessage('DEBUG: IFLI detected');

      for i := 0 to 8192-1 do read(f, ifli.chrmem1[i]);
      for i := 0 to 8192-1 do read(f, ifli.gfxmem1[i]);
      for i := 0 to 1024-1 do read(f, ifli.colmem[i]);
      for i := 0 to 8192-1 do read(f, ifli.chrmem2[i]);
      for i := 0 to 8192-1 do read(f, ifli.gfxmem2[i]); //...
	    //if (i) ok else fprintf(stderr, "Short file!\n");

      IFLIshow(ifli, ca);
    end
    else
      raise(Exception.Create('Unknown IFLI file format ID '+inttostr(temp[2])));
  end;

  raise(Exception.Create('Unknown FLI format (' + inttohex(temp[0], 2) + inttohex(temp[1], 2) + ')'));
end;

//---

function TC64.LoadKoalaToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, KOALAload, ca);
end;

function TC64.LoadHiresToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, HIRESload, ca);
end;

function TC64.LoadAmicaToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, AMICAload, ca);
end;

function TC64.LoadLogoToBitmap(FileName: string; ca: TCanvas; color1: byte): integer;
begin
  color1_ := color1;
  result := GenericLoader(FileName, LOGOload, ca);
end;

function TC64.LoadFontToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, FNTload, ca);
end;

function TC64.LoadFont2x2ToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, FNTBload, ca);
end;

function TC64.LoadMobToBitmap(FileName: string; ca: TCanvas; hires: boolean): integer;
var loader: TC64Loader;
begin
  if hires then
    loader := MOBloadHires 
  else
    loader := MOBloadMulticolor;
  result := GenericLoader(FileName, loader, ca);
end;

function TC64.LoadFliToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, FLIload, ca);
end;

//---

function TC64.LoadC64ToBitmap(FileName: string; ca: TCanvas): integer;
begin
  FLastError := 'Unknown format extension';
  case ExtMapper(ExtractFileExt(FileName)) of
    C64_UNKNOWN: result := -1;
    C64_KOALA: result := LoadKoalaToBitmap(FileName, ca);
    C64_HIRES: result := LoadHiresToBitmap(FileName, ca);
    C64_AMICA: result := LoadAmicaToBitmap(FileName, ca);
    C64_LOGO:  result := LoadLogoToBitmap(FileName, ca, 1);
    C64_FNT:   result := LoadFontToBitmap(FileName, ca);
    C64_FNTB:  result := LoadFont2x2ToBitmap(FileName, ca);
    C64_MOB:   result := LoadMobToBitmap(FileName, ca, false);
    C64_MBF:   result := LoadMobToBitmap(FileName, ca, true);
    C64_FLI,
    C64_AFLI,
    C64_BFLI,
    C64_IFLI,
    C64_FFLI:  result := LoadFliToBitmap(FileName, ca);
    else
      result := -1;
  end;
end;

end.

