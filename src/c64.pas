unit c64;

//------------------------------------------------------------------------------
//Commodore C-64 multicolor & hi-res GFX manipulation Delphi class, v1.2
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

{todo:
# MAIN:
- main demo app - load any file (by ext)
- fnt/logo - colors issue?
- TBitmap -> TCanvas ?
- finish: mob, fntb
- add misc limit checks
# LATER:
- maybe also C version
- also open source c64pas app 
- more (eg. advanced formats)?
- pack back to C64 formats
}

{CHANGELOG:
# v1.0
- base stuff, old version in Turbo Pascal
# v1.1
- slightly rewritten for Delphi (then named mob64.pas)
# v1.2
- radical code cleanup
- everything as class
- amica code integrated
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
     AMICAdata = record
                   len: word;
                   bmp: array[0..40*256-1] of byte;
                 end;


TC64Loader = procedure(bmp: TBitmap) of object;

TAmicaBuff = array[0..32767] of byte;

TC64 = class(TObject)
private
  f: file of byte;
  color1_: byte;
  function GenericLoader(FileName: string; callback: TC64Loader; bmp: TBitmap): integer;

  procedure KOALAshow(koala: KOALAdata; bmp: TBitmap);
  procedure HIRESshow(hires: HIRESdata; bmp: TBitmap);
  procedure LOGOshow(logo: LOGOdata; bmp: TBitmap; color1: byte);
  procedure FNTshow(x0, y0: integer; fnt: FNTdata; bmp: TBitmap; cnt: byte; color1: byte);

  procedure KOALAload(bmp: TBitmap);
  procedure HIRESload(bmp: TBitmap);
  procedure AMICAload(bmp: TBitmap);
  procedure AMICAunpack(i_buff: TAmicaBuff; var o_buff: TAmicaBuff);
  procedure AMICA2KOALA(o_buff: TAmicaBuff; var koala: KOALAdata);
  procedure LOGOload(bmp: TBitmap);
  procedure FNTload(bmp: TBitmap);
public
  function GetC64Color(index: integer): TColor;
  function LoadKoalaToBitmap(FileName: string; bmp: TBitmap): integer;
  function LoadHiresToBitmap(FileName: string; bmp: TBitmap): integer;
  function LoadAmicaToBitmap(FileName: string; bmp: TBitmap): integer;
  function LoadLogoToBitmap(FileName: string; bmp: TBitmap; color1: byte): integer;
  function LoadFontToBitmap(FileName: string; bmp: TBitmap): integer;    
end;

(*
procedure MOBload(name : string; var mob : MOBdata);
procedure FNTBload(name : string; var FNTB : FNTBdata);

procedure FNTBshow(x0,y0 : integer; fntb : FNTBdata; cnt : byte; color1 : byte);
procedure hMOBshow(x0,y0 : integer; mob : MOBdata; cnt : byte; color : byte);
procedure mMOBshow(x0,y0 : integer; mob : MOBdata; cnt : byte; color1 : byte);
*)

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

function TC64.GenericLoader(FileName: string; callback: TC64Loader; bmp: TBitmap): integer;
begin
  result := -1;
  if not assigned(callback) then exit;
  if not assigned(bmp) then exit;
  try
    AssignFile(f, FileName);
    reset(f);
    callback(bmp);
    CloseFile(f);
    result := 0;
  except
//    on E: Exception do showmessage('ERR: '+E.Message+' '+FileName); //todo: fix nice
  end;
end;

//---

procedure TC64.KOALAshow(koala: KOALAdata; bmp: TBitmap);
var x, y, bit, c0, c1, c2, c3, bt, bt1, vl, vl1, vl2: byte;
    c: TColor;
    color, ndx, ndx2: integer;
begin
  if not assigned(bmp) then exit;

  c0 := koala.backGr;
  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      ndx := x+y*40;
      ndx2 := x*8+y*320;
      c1 :=  koala.ink1[ndx] and $0f;        //&5c00
      c2 := (koala.ink1[ndx] and $f0) shr 4;
      c3 :=  koala.ink2[ndx] and $0f;        //&d800
      for bt := 0 to 7 do
      begin
        bt1 := koala.bitmap[ndx2+bt];
        for bit := 3 downto 0 do
        begin
          vl1 := ((bt1 and pow[bit*2]) div pow[bit*2]);
          vl2 := ((bt1 and pow[bit*2+1]) div pow[bit*2+1]);
          vl := vl1+2*vl2;
          case vl of
            3: color := c3;
            2: color := c1;
            1: color := c2;
            else color := c0;
          end;
          c := RGB(_r[color], _g[color], _b[color]);
          bmp.Canvas.Pixels[x*8+7-2*bit, (y*8+bt)] := c;
          bmp.Canvas.Pixels[x*8+8-2*bit, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.HIRESshow(hires: HIRESdata; bmp: TBitmap);
var x, y, bit, cc, c1, c2, bt, bt1, color: byte;
    c: TColor;
begin
  if not assigned(bmp) then exit;

  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      cc := hires.ink[x+y*40];
      c1 := cc and $0f;
      c2 := (cc and $f0) shr 4;
      for bt := 0 to 7 do
      begin
        bt1 := hires.bitmap[x*8+y*320+bt];
        for bit := 7 downto 0 do
        begin
          if (bt1 and pow[bit]) = pow[bit] then
            color := c2
          else
            color := c1;
          c := RGB(_r[color], _g[color], _b[color]);
          bmp.Canvas.Pixels[x*8+8-bit, (y*8+bt)] := c;          
        end;
      end;
    end;
end;

procedure TC64.LOGOshow(logo: LOGOdata; bmp: TBitmap; color1: byte);
var x, y, bit, bt1, bt2, vl, vl1, vl2, bt: byte;
    c: TColor;
begin
  if not assigned(bmp) then exit;

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
          vl := vl1+2*vl2+color1-1;
          c := RGB(_r[vl], _g[vl], _b[vl]);
          bmp.Canvas.Pixels[x*8+7-2*bit, (y*8+bt)] := c;
          bmp.Canvas.Pixels[x*8+8-2*bit, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.FNTshow(x0, y0: integer; fnt: FNTdata; bmp: TBitmap; cnt: byte; color1: byte);
var y, bit, bt, vl: byte;
    c: TColor;
begin
  if not assigned(bmp) then exit;

  for y := 0 to 7 do
  begin
    bt := fnt.fnt[cnt, y];
    for bit := 0 to 7 do
    begin
      vl := (bt and pow[bit]) div pow[bit];
      c := RGB(_r[vl+color1], _g[vl+color1], _b[vl+color1]);
      bmp.Canvas.Pixels[x0+8-bit, y0+y] := c;
    end;
  end;
end;

//---

procedure TC64.KOALAload(bmp: TBitmap);
var koala: KOALAdata;
    g: word;
    none: byte;
begin
  if not assigned(bmp) then exit;
  read(f, none, none);
  for g := 0 to $7f40-$6000-1 do read(f, koala.bitmap[g]);
  for g := 0 to $8328-$7f40-1 do read(f, koala.ink1[g]);
  for g := 0 to $8710-$8328-1 do read(f, koala.ink2[g]);
  read(f, koala.backGr);
  KOALAshow(koala, bmp);
end;

procedure TC64.HIRESload(bmp: TBitmap);
var HIRES: HIRESdata;
    g: word;
    none: byte;
begin
  if not assigned(bmp) then exit;
  {9009 bytes - what's that!?}
  read(f, none, none);
  for g := 0 to $3f40-$2000-1 do read(f, hires.bitmap[g]);
  for g := 0 to $4328-$3f40-1 do read(f, hires.ink[g]);
  HIRESshow(hires, bmp);
end;

procedure TC64.AMICAload(bmp: TBitmap);
var koala: KOALAdata;
//    AMICA: AMICAdata;
//    g: word;
//    none: byte;
    i_buff, o_buff: TAmicaBuff;
    b: integer;
    c: byte;
begin
  if not assigned(bmp) then exit;

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
    b := b + 1;
  end;

  AMICAunpack(i_buff, o_buff);
  AMICA2KOALA(o_buff, koala);
  KOALAshow(koala, bmp);
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
  for i := 0 to 8000-1 do koala.bitmap[i] := o_buff[i];  
  for i := 0 to 1000-1 do koala.ink1[i] := o_buff[8000+i];   
  for i := 0 to 1000-1 do koala.ink2[i] := o_buff[8000+1000+i]; //$D800
  koala.backGr := o_buff[$F710-$c000];
end;

//---

procedure TC64.LOGOload(bmp: TBitmap);
var logo: LOGOdata;
    g: word;
    none: byte;
begin
  if not assigned(bmp) then exit;
  read(f, none, none);
  for g := 0 to $1c00-$1800-1 do
    read(f,logo.logo[g]);
  for g := $1800-$1800 to $2000-$1c00-1 do
    read(f,none);
  for g := $2000-$2000 to $2800-$2000-1 do
    read(f,logo.bitmap[g]);
  LOGOshow(logo, bmp, color1_);
end;

procedure TC64.FNTload(bmp: TBitmap);
var FNT: FNTdata;
    g, h: byte;
begin
  if not assigned(bmp) then exit;

  for g := 1 to 255 do for h := 0 to 7 do fnt.fnt[g, h] := 0;

  read(f,g,g);
  for g := 1 to 64 do
    for h := 0 to 7 do
      read(f, fnt.fnt[g, h]);
  for h := 1 to 40 do
    FNTshow(h*8-8, 0, fnt, bmp, (h), color1_);
  for h := 1 to 24 do    
    FNTshow(h*8-8, 8, fnt, bmp, (40+h), color1_);
end;

//---

function TC64.LoadKoalaToBitmap(FileName: string; bmp: TBitmap): integer;
begin
  result := GenericLoader(FileName, KOALAload, bmp);
end;

function TC64.LoadHiresToBitmap(FileName: string; bmp: TBitmap): integer;
begin
  result := GenericLoader(FileName, HIRESload, bmp);
end;

function TC64.LoadAmicaToBitmap(FileName: string; bmp: TBitmap): integer;
begin
  result := GenericLoader(FileName, AMICAload, bmp);
end;

function TC64.LoadLogoToBitmap(FileName: string; bmp: TBitmap; color1: byte): integer;
begin
  color1_ := color1;
  result := GenericLoader(FileName, LOGOload, bmp);
end;

function TC64.LoadFontToBitmap(FileName: string; bmp: TBitmap): integer;
begin
  result := GenericLoader(FileName, FNTload, bmp);
end;

/////////////////////

(*
procedure MOBload(name : string; var mob : MOBdata);
var f : file of byte;
    g : byte;
begin
  mob.cnt := 1;
  assign(f,name);
  reset(f);
  read(f,g,g);
  while not eof(f) do
    begin
      for g := 0 to 63 do
        read(f,mob.mob[mob.cnt,g]);
      inc(mob.cnt)
    end;
  dec(mob.cnt);
  close(f)
end;

procedure FNTBload(name : string; var FNTB : FNTBdata);
var f : file of byte;
    g,h : byte;
begin
  assign(f,name);
  reset(f);
  read(f,g,g);
  for g := 1 to 64 do
    for h := 0 to 7 do
    read(f,fntb.fntb[g,h]);
  for g := 1 to 64 do
    for h := 0 to 7 do
    read(f,fntb.fntb[g,h+8]);
  for g := 1 to 64 do
    for h := 0 to 7 do
    read(f,fntb.fntb[g,h+16]);
  for g := 1 to 64 do
    for h := 0 to 7 do
    read(f,fntb.fntb[g,h+24]);
  close(f)
end;

//---

procedure hMOBshow(x0,y0 : integer; mob : MOBdata; cnt : byte; color : byte);
var x,y,bit,bt,vl : byte;
    adr : word;
begin
  adr := x0+320*y0;
  for y := 0 to 20 do
    for x := 0 to 2 do
      begin
        bt := mob.mob[cnt,x+y*3];
        for bit := 7 downto 0 do
          begin
            vl := (bt and pow[bit]) div pow[bit];
  {!!!}     if vl = 1 then
              mem[$0a000:adr+y*320+x*8+7-bit] := vl*color
          end
      end
end;

procedure mMOBshow(x0,y0 : integer; mob : MOBdata; cnt : byte; color1 : byte);
var x,y,bit,bt,vl1,vl2,vl : byte;
    adr : word;
begin
  adr := x0+320*y0;
  for y := 0 to 20 do
    for x := 0 to 2 do
      begin
        bt := mob.mob[cnt,x+y*3];
        for bit := 3 downto 0 do
          begin
            vl1 := (bt and pow[bit*2]) div pow[bit*2];
            vl2 := (bt and pow[bit*2+1]) div pow[bit*2+1];
            vl := vl1+2*vl2;
            if vl <> 0 then
             begin
                mem[$0a000:adr+y*320+x*8+7-2*bit] := vl+color1-1;
                mem[$0a000:adr+y*320+x*8+8-2*bit] := vl+color1-1;
              end
          end
      end
end;

procedure FNTBshow(x0,y0 : integer; fntb : FNTBdata; cnt : byte; color1 : byte);
var x,y,bit,bt,vl1,vl2,vl,c : byte;
    adr : word;
begin
  adr := x0+320*y0;
  for y := 0 to 15 do
    for x := 0 to 1 do
      begin
        if y>=8 then c := 16+y-8  else c := y;
        bt := fntb.fntb[cnt,x*8+c];
        for bit := 3 downto 0 do
          begin
            vl1 := (bt and pow[bit*2]) div pow[bit*2];
            vl2 := (bt and pow[bit*2+1]) div pow[bit*2+1];
            vl := vl1+2*vl2;
            if vl <> 0 then
             begin
                mem[$0a000:adr+y*320+x*8+7-2*bit] := vl+color1-1;
                mem[$0a000:adr+y*320+x*8+8-2*bit] := vl+color1-1;
              end
          end
      end

end;
*)

end.
