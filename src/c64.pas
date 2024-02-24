unit c64;

{$ifdef FPC}
{$MODE Delphi}
{$ENDIF}

//------------------------------------------------------------------------------
//Commodore C-64 GFX files manipulation Delphi (7+) / Lazarus class, v1.50
//Crossplatform (Delphi 7+ / Lazarus on Win32, Lazarus on Linux)
//(c)1994, 1995, 2009-2011, 2017, 2024 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC
//WWW: https://www.Noniewicz.com
//Licence: BSD 2-Clause License
//------------------------------------------------------------------------------
//based on my own earlier work (Koala/ArtStudio/Amica/Fonts/Sprites/Logo Painter)
//and this:
//http://codebase64.org/doku.php?id=base:c64_grafix_files_specs_list_v0.03
//(some) FLI code - loosely based on C code from C64Gfx by Pasi 'Albert' Ojala
//------------------------------------------------------------------------------
//Supported formats:
//- Koala Painter 2 (by AUDIO LIGHT) (pc-ext: .koa;.kla;.gg)
//- Wigmore Artist64 (by wigmore) (pc-ext: .a64)
//- Art Studio 1.0-1.1 (by OCP) (pc-ext: .aas;.art;.hpi)
//- Hi-Eddi (by Markt & Technik Verlag) (pc-ext: .hed)
//- Doodle (by OMNI) (pc-ext: .dd;.ddl;.jj) 
//- RunPaint (pc-ext: .rpm)
//- Blazing Paddles (by BAUDEVILLE) (pc-ext: .blp;.pi)
//- Vidcom 64 (by OMEGA) (pc-ext: .vid)
//- Saracen Paint (by IDEA) (pc-ext: .sar)
//- Interpaint Hires (pc-ext: .ip64h;.iph)
//- Image System (Hires) (.ish)
//- Image System (Multicolor) (pc-ext: .ism;.ims)
//- Amica Paint (by OLIVER STILLER/MDG) (pc-ext: .ami)
//- FLI Graph 2.2 (by blackmail) (pc-ext: .bml)
//- AFLI-editor v2.0 (by Topaz Beerline) (pc-ext: .afl)
//- Hires FLI (by Crest) (pc-ext: .hfc) [AFLI]
//- Gunpaint (pc-ext: .gun,.ifl)
//- Funpaint 2 (by Manfred Trenz) (pc-ext: .fp2;.fun)
//- Drazlace (pc-ext: .drl)
//- True Paint (by Fatum) (pc-ext: .mci)
//- some other *FLI formats -- IN PROGRESS 
//- 8x8 and 16x16 font (hires/multicolor) - YET UNFINISHED
//- sprites (hires/multicolor, also font sprited) - YET UNFINISHED
//- Logo Painter v3 (pc-ext: .lp3;.gfx)
//- Paint Magic (pc-ext: .pmg)
//- Advanced Art Studio 2.0 (by OCP) (pc-ext: .ocp;.art)
//- CDU-Paint (pc-ext: .cdu)
//- Rainbow Painter (pc-ext: .rp)
//- Hires-Interlace v1.0 (Feniks) (pc-ext: .hlf)
//- Hires Manager (by Cosmos) (pc-ext: .him)
//- raw single bitmap data (8000 bytes) hires/multi
//------------------------------------------------------------------------------
//History (==~cost):
//created: somewhere in 1994-1995
//updated: 20091231 ????-????
//updated: 20100101 ????-????
//updated: 20110510 ????-????
//---
//updated: 20171029 1715-2040
//updated: 20171029 2150-2250
//updated: 20171030 2200-2215
//updated: 20171030 2240-2310
//updated: 20171101 1630-2030
//updated: 20171101 2105-2130
//updated: 20171101 2200-2255
//updated: 20171104 1920-2000
//updated: 20171105 0020-0050
//updated: 20171105 1210-1315
//updated: 20171105 1330-1340
//updated: 20171105 1455-1520
//updated: 20171105 1550-1630
//updated: 20171105 1830-2055
//updated: 20171111 1220-1500
//updated: 20171111 1605-1800
//updated: 20171111 1935-2050
//updated: 20171111 2130-2150
//updated: 20171112 0100-0110
//updated: 20171112 1155-1325
//updated: 20171112 2045-2115
//updated: 20171112 2200-2330
//updated: 20171113 1000-1045
//updated: 20171113 2255-2350
//updated: 20171114 2250-0000
//updated: 20171115 0000-0035
//updated: 20171115 1905-2025
//updated: 20171115 2140-2220
//updated: 20171118 1245-1315
//updated: 20171118 1405-1415
//updated: 20171118 1425-1540
//updated: 20171118 1615-1820
// = 2140 min = ~35.5h (not counting 1994-1995 and 2009-2011)
//updated: 20171118 1820-1955
//updated: 20171119 1145-1220
//updated: 20171119 1235-1300
//updated: 20171120 2045-2115
//updated: 20171120 2135-2305
//updated: 20171121 2025-2035
//updated: 20171125 1335-1450
//updated: 20171125 1905-1930
//updated: 20171130 0045-0125
//updated: 20171130 0135-0155
//updated: 20171130 2210-2225
//updated: 20171201 1830-1900
//---
//updated: 20240108 1350-1355
//updated: 20240110 2045-2130

//note "." == topic already somewhat in progress, [!] == important, [-] == ignore

{todo:
# MAIN:
- more!

# NEXT:
.- Hires Manager (.him) packed [logo.him] [HIMload]
.- logo - hires v multi - LOGOshow(hires)
.- load 'raw' with start offset param ('search for pic' mode)
- [!] more even more exotic formats
== lookup again https://www.c64-wiki.com/wiki/Graphics_Modes
== see http://archive.li/vVuuk
== lookup view64 / congo / recoil for more formats
== NUFLI - WTF is that?
== there is more: ECI / FLINT / UFLI / SHFLI-XL / EAFLI / SHFLI / SHIFLI / UIFLI
== IFLI from $2000 ?
== AFLI from $6c73 ?
== mcp - Truepaint ?
== Hires-Lace v1.5 (by Oswald/COMA) (pc-ext: .hil)
== ECI Graphic Editor v1.0 (by PROACHER) (pc-ext: .eci) [AFLI]
- [!] separate load and canvas pack (rerender w/o load)
- [!] pack back to C64 formats and write (colormap, dither?)
- [!] recode one c64 fmt to another (m-m, h-h, h*m-*m*h, fli?)
- [!] tabelarize format structures + one call to load them all / LoadFromStream 1st then seek?
# LATER/MAYBE:
- raw data output (24bit RGB .raw)? also as dflt intermediate storage fmt?
- cleanup: commonize FLI code
- cleanup: wrap all FLI data records into one generic?
- fnt/fntb/mob - get given one/range | also output really all
- common file load via LoadFromStream / process later?
- commonize demo code d7/laz
- prep standarized test files (so all have the same pixel base)
- mob - anim?
- add misc limit checks?
- name detected format (return back text) ?
# ISSUES:
- godot.jj - see why differs from congo rendering?
- fun/fp2 - see why differs (cherry, valsary)?
- MCIshow - make sure that it really renders properly
- issue with FLIbug (not really rendered sometimes)?
# FUTURE:
- add deeper byte format detection
- even go xnView / recoil / congo / view64 ?
- C / C++ / C# / JS / Java versions (so more portable) ?
}

{CHANGELOG:
# v1.00
- base stuff, ancient version in Turbo Pascal (koala, amica, hires, sprites, fonts, logo painter)
# v1.10
- slightly rewritten for Delphi (then named mob64.pas)
# v1.20
- radical code cleanup
- everything as class
- amica code integrated
- demo app
- misc
# v1.30
- added FLI formats (experimental, unfinished)
- universal loader method (file extension based)
- misc fixes/changes
# v1.31
- added Hi-Eddi (.hed) format
# v1.32
- added AAS and HPI (untested) support
# v1.33
- added support for FLI Graph 2.2 (.bml)
- added support for Doodle (dd;.ddl)
# v1.34
- added support for Wigmore Artist64 (.a64)
# v1.35
- added support for RunPaint (.rpm)
# v1.36
- added support for Image System (Hires) (.ish)
- added support for Image System (Multicolor) (.ism;.ims)
- misc fixes/updates
# v1.37
- fixed BFLI display (h=400)
- added more palletes: C64S_PAL FRODO_PAL GODOT_PAL PC64_PAL VICE_PAL
- old VICE_PAL was actually CCS64_PAL
- final RGB color - get via one common call
- fnt/fntb/logo/mob - fixed colors issue (can now set proper 4 colors)
- hires/multi mode for font/logo/sprites - param exposed
- added support for Paint Magic (pc-ext: .pmg)
- misc
# v1.38
- crossplatform - Lazarus compatible + Lazarus demo
- added support for Advanced Art Studio 2.0 (pc-ext: .ocp;.art)
- added support for CDU-Paint (pc-ext: .cdu)
- Lazarus - compiled+checked on Linux (Debian 8.7.1)
- fnt/fntb - hires/multi - proper
- MULTICOLORclear, HIRESclear, LOGOclear, FNTclear, FNTBclear MOBclear FLIclear IFLIclear
- misc
# v1.39
- added palletes from HermIRES: C64HQ_PAL, OLDVICE_PAL, VICEDFLT_PAL
- reorganized colormaps
- added support for Rainbow Painter (pc-ext: .rp)
- added support for Doodle RLE packed .jj
- added support for Koala Painter 2 RLE packed .gg
- cleanup: MOBloadHires v MOBloadMulticolor -> one call
# v1.40
- added support for Hires FLI (by Crest) (pc-ext: .hfc) [AFLI]
- cleanup
# v1.41
- added support for Gunpaint (pc-ext: .gun,.ifl)
# v1.42
- added support for FFLI
# v1.43
- added support for Funpaint 2 (by Manfred Trenz) (pc-ext: .fp2;.fun) [packed and not]
- added support for Drazlace (pc-ext: .drl) [packed and not]
- code cleanup: RLE / amica unpack 
- misc minor changes
# v1.44
- added support for True Paint (by Fatum) (pc-ext: .mci)
- misc
# v1.45
- added support for Hires-Interlace v1.0 (Feniks) (pc-ext: .hlf)
# v1.46
- .gfx format was actually already .lp3 Logo Painter format!
- added support for Hires Manager (by Cosmos) (pc-ext: .him) unpacked
- load 'raw' single 8000 bitmap only data - both hires/multi
- code cleanup
- paint pallete to canvas
# v1.47
- added support for Interpaint Hires (pc-ext: .ip64h;.iph)
# v1.48
- fixed (apparently old) error in multicolor rendering - pixels were shifted +1
- added support for Blazing Paddles (by BAUDEVILLE) (pc-ext: .blp;.pi)
- added support for Vidcom 64 (by OMEGA) (pc-ext: .vid)
# v1.49
- added support for Saracen Paint (by IDEA) (pc-ext: .sar)
# v1.50 (202401)
- option to load raw file w/o std C64 2-byte load address (not or all formats)
- some minor changes

# EOFF
}

interface

{$ifdef FPC}
uses LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Dialogs;
{$ELSE}
uses Windows, SysUtils, Classes, Graphics, Dialogs;
{$ENDIF}



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
                  logo: array[0..$2000-$1800-1] of byte;  //8192-6144 = 2048
                  bitmap: array[0..$2800-$2000-1] of byte;
                end;
     MULTIdata = record
                   bitmap: array[0..$7f40-$6000-1] of byte;
                   ink1: array[0..$8328-$7f40-1] of byte;
                   ink2: array[0..$8710-$8328-1] of byte; // -> $d800
                   backGr: byte;
                 end;
     HIRESdata = record
                   bitmap: array[0..$3f40-$2000-1] of byte;
                   ink: array[0..$8328-$7f40-1] of byte;
                 end;
     FLIdata = record //generic, any FLI (except some)
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
                  bg: byte;
                end;
     FFLIdata = record //FFLI
                  gfxmem1: array[0..8192-1] of byte;
                  chrmem1: array[0..8192-1] of byte;
                  chrmem2: array[0..8192-1] of byte;
                  colmem: array[0..1024-1] of byte;
                  bgmem1: array[0..256-1] of byte;
                  bgmem2: array[0..256-1] of byte;
                end;
     FFLI2data = record //IFLI~FFLI
                  gfxmem1: array[0..8192-1] of byte;
                  gfxmem2: array[0..8192-1] of byte;
                  chrmem1: array[0..8192-1] of byte;
                  chrmem2: array[0..8192-1] of byte;
                  colmem: array[0..1024-1] of byte;
                  bgmem: array[0..256-1] of byte;
                end;
     DRAZFLIdata = record //Drazlace FLI 
                 gfxmem1: array[0..8000-1] of byte;
                 gfxmem2: array[0..8000-1] of byte;
                 chrmem: array[0..1024-1] of byte;
                 colmem: array[0..1024-1] of byte;
                 bgcol: byte;
               end;
     MCIFLIdata = record //True Paint FLI 
                 gfxmem1: array[0..8000-1] of byte;
                 gfxmem2: array[0..8000-1] of byte;
                 chrmem1: array[0..1000-1] of byte;
                 chrmem2: array[0..1000-1] of byte;
                 colmem: array[0..1024-1] of byte;
                 bgcol: byte;
               end;
     HIIdata = record // 
                 gfxmem1: array[0..8000-1] of byte;
                 gfxmem2: array[0..8000-1] of byte;
                 chrmem1: array[0..1000-1] of byte;
                 chrmem2: array[0..1000-1] of byte;
               end;



TC64Loader = procedure(ca: TCanvas) of object;

TAmicaBuff = array[0..49151] of byte; //was 32767, now 49151

TC64Pallete = (C64S_PAL, CCS64_PAL, FRODO_PAL, GODOT_PAL, PC64_PAL, VICE_PAL,
               C64HQ_PAL, OLDVICE_PAL, VICEDFLT_PAL);

TC64FileType = (C64_UNKNOWN,
                C64_RAW,
                C64_KOALA, C64_WIGMORE, C64_RUNPAINT, C64_ISM, C64_PAINTMAGIC,
                C64_ADVARTST, C64_CDU, C64_RAINBOW,
                C64_BLP, C64_VID, C64_SAR,
                C64_KOALA_RLE, 
                C64_HIRES, C64_HED, C64_DDL, C64_ISH, C64_IPH,
                C64_DDL_RLE,
                C64_AMICA,
                C64_LOGO, C64_FNT, C64_FNTB, C64_MOB, C64_MBF,
                C64_FLI, C64_AFLI, C64_BFLI, C64_IFLI, C64_FFLI,
                C64_HFC, C64_GUN_IFLI, C64_FUN, C64_DRL, C64_MCI,
                C64_HLF, C64_HIM
                );

TC64 = class(TObject)
private
  f: file of byte;
  FColors4: array [0..3] of byte;
  FLastError: string;
  FPalette: TC64Pallete;
  FAsHires: boolean;
  FSkipHead: boolean;

  function GenericLoader(FileName: string; callback: TC64Loader; ca: TCanvas; mode: TC64FileType): integer;
  function GetVersion: string;
  procedure ReadAndDiscardHead;

  procedure MULTICOLORclear(var data: MULTIdata);
  procedure HIRESclear(var data: HIRESdata);
  procedure LOGOclear(var data: LOGOdata);
  procedure FNTclear(var data: FNTdata);
  procedure FNTBclear(var data: FNTBdata);
  procedure MOBclear(var data: MOBdata);
  procedure FLIclear(var data: FLIdata);
  procedure IFLIclear(var data: IFLIdata);
  procedure FFLIclear(var data: FFLIdata);
  procedure FFLI2clear(var data: FFLI2data);
  procedure DRAZFLIclear(var data: DRAZFLIdata);
  procedure MCIFLIclear(var data: MCIFLIdata);
  procedure HIIclear(var data: HIIdata);

  procedure MULTICOLORshow(data: MULTIdata; ca: TCanvas);
  procedure HIRESshow(data: HIRESdata; ca: TCanvas);
  procedure LOGOshow(data: LOGOdata; ca: TCanvas);
  procedure FNTshow(x0, y0: integer; fnt: FNTdata; ca: TCanvas; cnt: byte);
  procedure FNTBshow(x0, y0: integer; fntb: FNTBdata; ca: TCanvas; cnt: byte);
  procedure hMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte);
  procedure mMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte);
  procedure HIMshow(fli: FLIdata; ca: TCanvas);
  procedure FLIshow(fli: FLIdata; ca: TCanvas; mode: TC64FileType);
  procedure IFLIshow(ifli: IFLIdata; ca: TCanvas);
  procedure FFLIshow(ffli: FFLIdata; ca: TCanvas);
  procedure FFLI2show(ffli2: FFLI2data; ca: TCanvas);
  procedure DRAZFLIshow(drazfli: DRAZFLIdata; ca: TCanvas);
  procedure MCIshow(data: MCIFLIdata; ca: TCanvas);
  procedure HIIshow(data: HIIdata; ca: TCanvas);

  procedure RAWHIRESload(ca: TCanvas);
  procedure RAWMULTIoad(ca: TCanvas);
  procedure KOALAload(ca: TCanvas);
  procedure KOALAload_RLE(ca: TCanvas);
  procedure WIGMOREload(ca: TCanvas);
  procedure RUNPAINTload(ca: TCanvas);
  procedure IMGSYSload(ca: TCanvas);
  procedure PAMAGload(ca: TCanvas);
  procedure ADVARTSTload(ca: TCanvas);
  procedure CDUload(ca: TCanvas);
  procedure RPload(ca: TCanvas);
  procedure BLPload(ca: TCanvas);
  procedure VIDload(ca: TCanvas);
  procedure SARload(ca: TCanvas);

  procedure HIRESload(ca: TCanvas);
  procedure HIRESloadHED(ca: TCanvas);
  procedure HIRESloadDDL(ca: TCanvas);
  procedure HIRESloadDDL_RLE(ca: TCanvas);
  procedure HIRESloadISH(ca: TCanvas);
  procedure HIRESloadIPH(ca: TCanvas);

  procedure AMICAload(ca: TCanvas);
  procedure LOGOload(ca: TCanvas);
  procedure FNTload(ca: TCanvas);
  procedure FNTBload(ca: TCanvas);
  procedure MOBload(ca: TCanvas);
  procedure AFLIload(ca: TCanvas);
  procedure GUNFLIload(ca: TCanvas);
  procedure FUNFLIload(ca: TCanvas);
  procedure DRLload(ca: TCanvas);
  procedure MCIload(ca: TCanvas);
  procedure HIIload(ca: TCanvas);
  procedure HIMload(ca: TCanvas);
  procedure FLIload(ca: TCanvas);
public
  constructor Create;
  function GetC64Color(index: integer): TColor;
  function GetC64Color8(index, rgb: integer): byte;
  function GetC64ColorR(index: integer): byte;
  function GetC64ColorG(index: integer): byte;
  function GetC64ColorB(index: integer): byte;
  procedure Set4Colors(color0, color1, color2, color3: byte);
  procedure PaintPallete(ca: TCanvas; x0, y0, w, h: integer);
  function ExtMapper(ext: string): TC64FileType;

  function LoadRawToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
  function LoadMulticolorToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
  function LoadHiresToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
  function LoadAmicaToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadLogoToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadFontToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadFont2x2ToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadMobToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadFliToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;

  function LoadC64ToBitmap(FileName: string; ca: TCanvas): integer;
  function LoadC64ToBitmapByMode(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
published
  property LastError: string read FLastError;
  property Palette: TC64Pallete read FPalette write FPalette;
  property AsHires: boolean read FAsHires write FAsHires;
  property SkipHead: boolean read FSkipHead write FSkipHead;
  property Version: string read GetVersion;
end;


implementation

const
  VER_MAJOR = 1;
  VER_MINOR = 50;


//0..15 = black,white,red,cyan,magenta(purple),green,blue,yellow
//orange,brown(lt.red),pink,dk.gray,gray,lt.green,lt.blue,lt.gray

//VICE pallete c64s.vpl
  c64s_rgb: array[0..2, 0..15] of byte = (
      ($00,$fc,$a8,$54,$a8,$00,$00,$fc, $a8,$80,$fc,$54,$80,$54,$54,$a8),
      ($00,$fc,$00,$fc,$00,$a8,$00,$fc, $54,$2c,$54,$54,$80,$fc,$54,$a8),
      ($00,$fc,$00,$fc,$a8,$00,$a8,$00, $00,$00,$54,$54,$80,$54,$fc,$a8));

//VICE pallete ccs64.vpl
  ccs64_rgb: array[0..2, 0..15] of byte = (
      ($00,$ff,$e0,$60,$e0,$40,$40,$ff, $e0,$9c,$ff,$54,$88,$a0,$a0,$c0),
      ($00,$ff,$40,$ff,$60,$e0,$40,$ff, $a0,$74,$a0,$54,$88,$ff,$a0,$c0),
      ($00,$ff,$40,$ff,$e0,$40,$e0,$40, $40,$48,$a0,$54,$88,$a0,$ff,$c0));

//VICE pallete frodo.vpl
  frodo_rgb: array[0..2, 0..15] of byte = (
      ($00,$ff,$cc,$00,$ff,$00,$00,$ff, $ff,$88,$ff,$44,$88,$88,$88,$cc),
      ($00,$ff,$00,$ff,$00,$cc,$00,$ff, $88,$44,$88,$44,$88,$ff,$88,$cc),
      ($00,$ff,$00,$cc,$ff,$00,$cc,$00, $00,$00,$88,$44,$88,$88,$ff,$cc));

//VICE pallete godot.vpl
  godot_rgb: array[0..2, 0..15] of byte = (
      ($00,$ff,$88,$aa,$cc,$00,$00,$ee, $dd,$66,$fe,$33,$77,$aa,$00,$bb),
      ($00,$ff,$00,$ff,$44,$cc,$00,$ee, $88,$44,$77,$33,$77,$ff,$88,$bb),
      ($00,$ff,$00,$ee,$cc,$55,$aa,$77, $55,$00,$77,$33,$77,$66,$ff,$bb));

//VICE pallete pc64.vpl
  pc64_rgb: array[0..2, 0..15] of byte = (
      ($21,$ff,$b5,$73,$b5,$21,$21,$ff, $b5,$94,$ff,$73,$94,$73,$73,$b5),
      ($21,$ff,$21,$ff,$21,$b5,$21,$ff, $73,$42,$73,$73,$94,$ff,$73,$b5),
      ($21,$ff,$21,$ff,$b5,$21,$b5,$21, $21,$21,$73,$73,$94,$73,$ff,$b5));

//VICE pallete vice.vpl (pepto.vpl seems the same)
  vice_rgb: array[0..2, 0..15] of byte = (
      ($00,$ff,$68,$70,$6f,$58,$35,$b8, $6f,$43,$9a,$44,$6c,$9a,$6c,$95),
      ($00,$ff,$37,$a4,$3d,$8d,$28,$c7, $4f,$39,$67,$44,$6c,$d2,$5e,$95),
      ($00,$ff,$2b,$b2,$86,$43,$79,$6f, $25,$00,$59,$44,$6c,$84,$b5,$95));

//VICE pallete c64hq.vpl (from HermIRES-1.29)
  c64hq_rgb: array[0..2, 0..15] of byte = (
      ($0a,$ff,$85,$65,$a7,$4d,$1a,$eb, $a9,$44,$d2,$46,$8b,$8e,$4d,$ba),
      ($0a,$f8,$1f,$cd,$3b,$ab,$0c,$e3, $4b,$1e,$80,$46,$8b,$f6,$91,$ba),
      ($0a,$ff,$02,$a8,$9f,$19,$92,$53, $02,$00,$75,$46,$8b,$8e,$d1,$ba));

//VICE pallete older-vice.vpl (from HermIRES-1.29)
  oldvice_rgb: array[0..2, 0..15] of byte = (
      ($0a,$ff,$8f,$a1,$96,$86,$4b,$f3, $9b,$64,$cb,$64,$96,$d6,$93,$c9),
      ($0a,$f8,$53,$d9,$5f,$bc,$40,$fd, $72,$52,$92,$64,$96,$ff,$89,$c9),
      ($0a,$ff,$4b,$e0,$b4,$67,$9e,$a6, $44,$00,$8b,$64,$96,$b8,$e1,$c9));

//VICE pallete vice-default.vpl (from HermIRES-1.29)
  vicedflt_rgb: array[0..2, 0..15] of byte = (
      ($00,$fd,$be,$30,$b4,$1f,$21,$df, $b8,$6a,$fe,$42,$70,$59,$5f,$a4),
      ($00,$fe,$1a,$e6,$1a,$d2,$1b,$f6, $41,$33,$4a,$45,$74,$fe,$53,$a7),
      ($00,$fc,$24,$c6,$e2,$1e,$ae,$0a, $04,$04,$57,$40,$6f,$59,$fe,$a2));

  pow: array[0..7] of byte = (1, 2, 4, 8, 16, 32, 64, 128);

  TOP_BUFF = high(TAmicaBuff);



//RLE: Escape code / 1 byte value / 1 byte count
procedure unpackRLE_V_C(esc: byte; i_buff: TAmicaBuff; i_size: integer; var o_buff: TAmicaBuff; o_size: integer);
var i, x, a: byte;
    _FBC, _FDE: integer;
begin
  _FBC := 0;
  _FDE := 0+2; //skip load addr (2 bytes)
  repeat
    if (_FDE >= i_size) then exit;
    a := i_buff[_FDE]; INC(_FDE);
    if a = esc then
    begin
      if (_FDE >= i_size) then exit;
      x := i_buff[_FDE]; INC(_FDE);
      if (_FDE >= i_size) then exit;
      a := i_buff[_FDE]; INC(_FDE);
      if a = 0 then exit;
      for i := 1 to a do
      begin
        if (_FBC >= o_size) then exit;
        o_buff[_FBC] := x; INC(_FBC);
      end;
    end
    else
    begin
      if (_FBC >= o_size) then exit;
      o_buff[_FBC] := a; INC(_FBC);
    end;
  until false; //can it hung? coz if it can it will
end;

//'AMICA PAINT' UNPACKER / RLE: Escape code / 1 byte count / 1 byte value
procedure AMICAunpack(skip: integer; esc: byte; i_buff: TAmicaBuff; i_size: integer; var o_buff: TAmicaBuff; o_size: integer);
var i, x, a: byte;
    _FBC, _FDE: integer;
begin
  _FBC := 0;
  _FDE := skip;
  repeat
    if (_FDE >= i_size) then exit;  
    a := i_buff[_FDE]; INC(_FDE);
    if a = esc then
    begin
      if (_FDE >= i_size) then exit;
      a := i_buff[_FDE]; INC(_FDE);
      if a = 0 then exit;
      x := a;
      if (_FDE >= i_size) then exit;
      a := i_buff[_FDE]; INC(_FDE);
      for i := 1 to x do
      begin
        if (_FBC >= o_size) then exit;
        o_buff[_FBC] := a; INC(_FBC);
      end;
    end
    else
    begin
      if (_FBC >= o_size) then exit;
      o_buff[_FBC] := a; INC(_FBC);
    end;
  until false; //can it hung? coz if it can it will
end;

//---

constructor TC64.Create;
begin
  inherited;
  FPalette := CCS64_PAL;
  FAsHires := false;
  Set4Colors(0, 1, 15, 11);
  FSkipHead := false;
end;

function TC64.GetVersion: string;
begin
  result := inttostr(VER_MAJOR)+'.'+inttostr(VER_MINOR);
end;

procedure TC64.ReadAndDiscardHead;
var none: byte;
begin
  if not FSkipHead then
  begin
    read(f, none, none);
  end;
end;

function TC64.GetC64Color(index: integer): TColor;
begin
  if not (index in [0..15]) then
    result := 0
  else
    case FPalette of
      C64S_PAL:     result := RGB(c64s_rgb[0, index],     c64s_rgb[1, index],     c64s_rgb[2, index]);
      CCS64_PAL:    result := RGB(ccs64_rgb[0, index],    ccs64_rgb[1, index],    ccs64_rgb[2, index]);
      FRODO_PAL:    result := RGB(frodo_rgb[0, index],    frodo_rgb[1, index],    frodo_rgb[2, index]);
      GODOT_PAL:    result := RGB(godot_rgb[0, index],    godot_rgb[1, index],    godot_rgb[2, index]);
      PC64_PAL:     result := RGB(pc64_rgb[0, index],     pc64_rgb[1, index],     pc64_rgb[2, index]);
      VICE_PAL:     result := RGB(vice_rgb[0, index],     vice_rgb[1, index],     vice_rgb[2, index]);
      C64HQ_PAL:    result := RGB(c64hq_rgb[0, index],    c64hq_rgb[1, index],    c64hq_rgb[2, index]);
      OLDVICE_PAL:  result := RGB(oldvice_rgb[0, index],  oldvice_rgb[1, index],  oldvice_rgb[2, index]);
      VICEDFLT_PAL: result := RGB(vicedflt_rgb[0, index], vicedflt_rgb[1, index], vicedflt_rgb[2, index]);
      else result := 0;
    end;
end;

function TC64.GetC64Color8(index, rgb: integer): byte;
begin
  if not (index in [0..15]) or not (rgb in [0..2]) then
    result := 0
  else
    case FPalette of
      C64S_PAL:     result := c64s_rgb[rgb, index];
      CCS64_PAL:    result := ccs64_rgb[rgb, index];
      FRODO_PAL:    result := frodo_rgb[rgb, index];
      GODOT_PAL:    result := godot_rgb[rgb, index];
      PC64_PAL:     result := pc64_rgb[rgb, index];
      VICE_PAL:     result := vice_rgb[rgb, index];
      C64HQ_PAL:    result := c64hq_rgb[rgb, index];
      OLDVICE_PAL:  result := oldvice_rgb[rgb, index];
      VICEDFLT_PAL: result := vicedflt_rgb[rgb, index];
      else result := 0;
    end;
end;

function TC64.GetC64ColorR(index: integer): byte;
begin
  result := GetC64Color8(index, 0);
end;

function TC64.GetC64ColorG(index: integer): byte;
begin
  result := GetC64Color8(index, 1);
end;

function TC64.GetC64ColorB(index: integer): byte;
begin
  result := GetC64Color8(index, 2);
end;

procedure TC64.Set4Colors(color0, color1, color2, color3: byte);
begin
  FColors4[0] := color0;
  FColors4[1] := color1;
  FColors4[2] := color2;
  FColors4[3] := color3;
end;

procedure TC64.PaintPallete(ca: TCanvas; x0, y0, w, h: integer);
var i: integer;
    r: TRect;
begin
  if not assigned(ca) then exit;

  r.Top := y0;
  r.Bottom := h;
  ca.Brush.Style := bsSolid;

  for i := 0 to 15 do
  begin
    r.Left := x0 + i * w;
    r.Right := r.Left + w;
    ca.Brush.Color := GetC64Color(i);
    ca.FillRect(r);
  end;
end;

//---

function TC64.ExtMapper(ext: string): TC64FileType;
var e: string;
begin
  e := uppercase(ext);
  result := C64_UNKNOWN;

  //raw
  if (e = '.RAW') then result := C64_RAW;

  //Koala Painter 2 (by AUDIO LIGHT) (pc-ext: .koa;.kla;.gg)
  if (e = '.KOA') or (e = '.KLA') then result := C64_KOALA;
  if (e = '.GG') then result := C64_KOALA_RLE;

  //Advanced Art Studio 2.0 (by OCP) (pc-ext: .ocp;.art)
  if (e = '.MPIC') then result := C64_ADVARTST; //note: we use .mpic ext, not .art or other

  //Blazing Paddles (by BAUDEVILLE) (pc-ext: .blp;.pi) 
  if (e = '.BLP') or (e = '.PI') then result := C64_BLP;

  //Vidcom 64 (by OMEGA) (pc-ext: .vid) 
  if (e = '.VID') then result := C64_VID;

  //Saracen Paint (by IDEA) (pc-ext: .sar)
  if (e = '.SAR') then result := C64_SAR;

  //Wigmore Artist64 (by wigmore) (pc-ext: .a64)
  if (e = '.A64') then result := C64_WIGMORE;

  //RunPaint (pc-ext: .rpm)
  if (e = '.RPM') then result := C64_RUNPAINT;

  //Image System (Multicolor) (pc-ext: .ism;.ims)
  if (e = '.ISM') or (e = '.IMS') then result := C64_ISM;

  //Paint Magic (pc-ext: .pmg)
  if (e = '.PMG') then result := C64_PAINTMAGIC;

  //CDU-Paint (pc-ext: .cdu)
  if (e = '.CDU') then result := C64_CDU;

  //Rainbow Paiter (pc-ext: .rp)
  if (e = '.RP') then result := C64_RAINBOW;

  //Art Studio 1.0-1.1 (by OCP) (pc-ext: .aas;.art;.hpi)
  if (e = '.PIC') or (e = '.ART') or (e = '.OCP') or (e = '.AAS') or (e = '.HPI') then
    result := C64_HIRES;

  //Hi-Eddi (by Markt & Technik Verlag) (pc-ext: .hed) 
  if (e = '.HED') then result := C64_HED;

  //Doodle (by OMNI) (pc-ext: .dd;.ddl;.jj)
  if (e = '.DD') or (e = '.DDL') then result := C64_DDL;
  if (e = '.JJ') then result := C64_DDL_RLE;

  //Image System (Hires) (pc-ext: .ish)
  if (e = '.ISH') then result := C64_ISH;

  //Interpaint Hires (pc-ext: .ip64h)
  if (e = '.IPH') or (e = '.IP64H') then result := C64_IPH;

  //Amica Paint
  if (e = '.[B]') or (e = '.AMI') then result := C64_AMICA; //note: '[B]' invented here

  //Logo Painter v3
  if (e = '.LP3') or (e = '.GFX') then result := C64_LOGO;  //note: gfx ext invented here

  //8x8 font (multi or hires)
  if e = '.FNT' then result := C64_FNT;

  //16x16 font (multi or hires)
  if (e = '.FNB') or (e = '.FNTB') then result := C64_FNTB;  //note: ext invented here

  //sprites + sprite fonts hires/multi 
  if e = '.MOB' then result := C64_MOB;   //note: ext invented here
  if e = '.MBF' then result := C64_MBF;   //note: ext invented here

  //FLI Graph 2.2 (by blackmail) (pc-ext: .bml)
  if (e = '.FLI') or (e = '.BML') then result := C64_FLI;

  //AFLI-editor v2.0 (by Topaz Beerline) (pc-ext: .afl)
  if (e = '.AFLI') or (e = '.AFL')  then result := C64_AFLI;

  //Hires FLI (by Crest) (pc-ext: .hfc)
  if (e = '.HFC') then result := C64_HFC;

  //Big FLI
  if (e = '.BFLI') then result := C64_BFLI;

  if (e = '.FFLI') then result := C64_FFLI;

  if (e = '.IFLI') or (e = '.IFL') then result := C64_IFLI;

  //Gunpaint (pc-ext: .gun,.ifl)
  if (e = '.GUN') then result := C64_GUN_IFLI;

  //Funpaint 2 (by Manfred Trenz) (pc-ext: .fp2;.fun)
  if (e = '.FUN') or (e = '.FP2') then result := C64_FUN;

  //Drazlace (pc-ext: .drl) 
  if (e = '.DRL') then result := C64_DRL;

  //True Paint (by Fatum) (pc-ext: .mci)
  if (e = '.MCI') then result := C64_MCI;

  //Hires-Interlace v1.0 (Feniks) (pc-ext: .hlf)
  if (e = '.HLF') then result := C64_HLF;

  //Hires Manager (by Cosmos) (pc-ext: .him)
  if (e = '.HIM') then result := C64_HIM;
end;

function TC64.GenericLoader(FileName: string; callback: TC64Loader; ca: TCanvas; mode: TC64FileType): integer;
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

procedure TC64.MULTICOLORclear(var data: MULTIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.HIRESclear(var data: HIRESdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.LOGOclear(var data: LOGOdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.FNTclear(var data: FNTdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.FNTBclear(var data: FNTBdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.MOBclear(var data: MOBdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.FLIclear(var data: FLIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.IFLIclear(var data: IFLIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.FFLIclear(var data: FFLIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.FFLI2clear(var data: FFLI2data);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.DRAZFLIclear(var data: DRAZFLIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.MCIFLIclear(var data: MCIFLIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

procedure TC64.HIIclear(var data: HIIdata);
begin
  FillChar(data, sizeof(data), #0);
end;

//---

procedure TC64.MULTICOLORshow(data: MULTIdata; ca: TCanvas);
var x, y, bit, c0, c1, c2, c3, bt, bt1, vl, vl1, vl2: byte;
    c: TColor;
    ndx, ndx2: integer;
begin
  if not assigned(ca) then exit;

  c0 := data.backGr and $0f;
  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      ndx := x+y*40;
      ndx2 := x*8+y*320;
      c1 := (data.ink1[ndx] and $0f);        //&5c00
      c2 := (data.ink1[ndx] and $f0) shr 4;
      c3 := (data.ink2[ndx] and $0f);        //&d800
      for bt := 0 to 7 do
      begin
        bt1 := data.bitmap[ndx2+bt];
        for bit := 3 downto 0 do
        begin
          vl1 := ((bt1 and pow[bit*2]) div pow[bit*2]);
          vl2 := ((bt1 and pow[bit*2+1]) div pow[bit*2+1]);
          vl := vl1+2*vl2;
          case vl of
            3: c := GetC64Color(c3);
            2: c := GetC64Color(c1);
            1: c := GetC64Color(c2);
            else c := GetC64Color(c0);
          end;
          //todo: err x - should be -1 (so everywhere)
          ca.Pixels[x*8+7-2*bit-1, (y*8+bt)] := c;
          ca.Pixels[x*8+8-2*bit-1, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.HIRESshow(data: HIRESdata; ca: TCanvas);
var x, y, bit, cc, c1, c2, bt, bt1: byte;
    c: TColor;
begin
  if not assigned(ca) then exit;

  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      cc := data.ink[x+y*40];
      c1 := (cc and $0f);
      c2 := (cc and $f0) shr 4;
      for bt := 0 to 7 do
      begin
        bt1 := data.bitmap[x*8+y*320+bt];
        for bit := 7 downto 0 do
        begin
          if (bt1 and pow[bit]) = pow[bit] then
            c := GetC64Color(c2)
          else
            c := GetC64Color(c1);
          ca.Pixels[x*8+8-bit, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.LOGOshow(data: LOGOdata; ca: TCanvas);
var x, y, bit, bt1, bt2, vl, vl1, vl2, bt: byte;
    c: TColor;
begin
  if not assigned(ca) then exit;

  for x := 0 to 39 do
    for y := 0 to 24 do
    begin
      bt1 := data.logo[x+y*40];
      for bt := 0 to 7 do
      begin
        bt2 := data.bitmap[bt1*8+bt];
        for bit := 3 downto 0 do
        begin
          vl1 := ((bt2 and pow[bit*2]) div pow[bit*2]);
          vl2 := ((bt2 and pow[bit*2+1]) div pow[bit*2+1]);
          vl := vl1+2*vl2;
          //todo: hires too
          case vl of
            0: c := GetC64Color(FColors4[0]);
            1: c := GetC64Color(FColors4[1]);
            2: c := GetC64Color(FColors4[2]);
            3: c := GetC64Color(FColors4[3]);
            else c := GetC64Color(0);
          end;
          ca.Pixels[x*8+7-2*bit-1, (y*8+bt)] := c;
          ca.Pixels[x*8+8-2*bit-1, (y*8+bt)] := c;
        end;
      end;
    end;
end;

procedure TC64.FNTshow(x0, y0: integer; fnt: FNTdata; ca: TCanvas; cnt: byte);
var y, bit, bt, vl, vl1, vl2: byte;
    c: TColor;
begin
  if not assigned(ca) then exit;

  if FAsHires then
  begin
    for y := 0 to 7 do
    begin
      bt := fnt.fnt[cnt, y];
      for bit := 0 to 7 do
      begin
        vl := bt and pow[bit];
        if vl = 0 then
          c := GetC64Color(FColors4[0]) //bg
        else
          c := GetC64Color(FColors4[1]); //fg
        ca.pixels[x0+8-bit, y0+y] := c;
      end;
    end;
  end
  else  //multicolor 
  begin
    for y := 0 to 7 do
    begin
      bt := fnt.fnt[cnt, y];
      for bit := 0 to 3 do
      begin
        vl1 := (bt and pow[bit*2]) div pow[bit*2];
        vl2 := (bt and pow[bit*2+1]) div pow[bit*2+1];
        vl := vl1+2*vl2;
        case vl of
          0: c := GetC64Color(FColors4[0]);
          1: c := GetC64Color(FColors4[1]);
          2: c := GetC64Color(FColors4[2]);
          3: c := GetC64Color(FColors4[3]);
          else c := GetC64Color(0);
        end;
        ca.Pixels[x0+8-2*bit, y0+y] := c;
        ca.Pixels[x0+8+1-2*bit, y0+y] := c;
      end;
    end;
  end
end;

procedure TC64.FNTBshow(x0, y0: integer; fntb: FNTBdata; ca: TCanvas; cnt: byte);
var x, y, bit, bt, vl1, vl2, vl, c: byte;
    cl: TColor;
begin
  if not assigned(ca) then exit;

  if FAsHires then
  begin
    for y := 0 to 15 do
      for x := 0 to 1 do
      begin
        if y >= 8 then c := 16+y-8 else c := y;
        bt := fntb.fntb[cnt, x*8+c];
        for bit := 0 to 7 do
        begin
          vl := bt and pow[bit];
          if vl = 0 then
            cl := GetC64Color(FColors4[0]) //bg
          else
            cl := GetC64Color(FColors4[1]); //fg
          ca.Pixels[x0+x*8+8-bit, y0+y] := cl;
        end;
      end;
  end
  else  //multicolor
  begin
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
          case vl of
            0: cl := GetC64Color(FColors4[0]);
            1: cl := GetC64Color(FColors4[1]);
            2: cl := GetC64Color(FColors4[2]);
            3: cl := GetC64Color(FColors4[3]);
            else cl := GetC64Color(0);
          end;
          ca.Pixels[x0+x*8+7-2*bit, y0+y] := cl;
          ca.Pixels[x0+x*8+8-2*bit, y0+y] := cl;
        end;
      end;
  end;
end;

procedure TC64.hMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte);
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
        if vl = 0 then
          cl := GetC64Color(0)  //background (eg. black)
        else
          cl := GetC64Color(1); //foreground (eg. white)
        ca.Pixels[x0+x*8+7-bit, y0+y] := cl;
      end
    end;
end;

procedure TC64.mMOBshow(x0, y0: integer; mob: MOBdata; ca: TCanvas; cnt: byte);
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
        case vl of
          0: cl := GetC64Color(FColors4[0]);
          1: cl := GetC64Color(FColors4[1]);
          2: cl := GetC64Color(FColors4[2]);
          3: cl := GetC64Color(FColors4[3]);
          else cl := GetC64Color(0);
        end;
        ca.Pixels[x0+x*8+7-2*bit, y0+y] := cl;
        ca.Pixels[x0+x*8+8-2*bit, y0+y] := cl;
      end;
    end;
end;

//FLI - based (somewhat) on C code from C64Gfx by Pasi 'Albert' Ojala

const bitmask: array[0..3] of byte = ($c0, $30, $0c, $03);
      bitshift: array[0..3] of byte = ($40, $10, $04, $01);

procedure TC64.HIMshow(fli: FLIdata; ca: TCanvas);
var x, y, ind, pos, bits: integer;
    a, b: byte;
begin
  if not assigned(ca) then exit;

  for y := 0 to 200-1 do
  begin
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
        ca.Pixels[x, y] := GetC64Color(b);
      end;
    end;
  end;
end;

procedure TC64.FLIshow(fli: FLIdata; ca: TCanvas; mode: TC64FileType);
var x, y, ind, pos, bits, ysize(*, xsize*): integer;
    a, b: byte;
begin
  if not assigned(ca) then exit;

  ysize := 200;
//  xsize := 160;
  b := 0;

  if (mode = C64_BFLI) then ysize := 400;
//  if (mode = C64_AFLI) then xsize := 320;

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
        ca.Pixels[x, y] := GetC64Color(b);
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
        ca.Pixels[x, y] := GetC64Color(b);
      end;
    end;
  end;
end;

procedure TC64.IFLIshow(ifli: IFLIdata; ca: TCanvas);
var x, y, ind, pos, bits, memind: integer;
    a0, a1: byte;
    c0, c1: byte;
    buffer: array[0..3*321] of byte;    
begin
  if not assigned(ca) then exit;
  
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
        0: c0 := ifli.bg; //note: not all formats use this, sometimes it'll be 0
        1: c0 := ifli.chrmem1[memind] div 16;
        2: c0 := ifli.chrmem1[memind] mod 16;
        3: c0 := ifli.colmem[ind] and $0f;
      end;
      case a1 of
        0: c1 := ifli.bg; //note: not all formats use this, sometimes it'll be 0
        1: c1 := ifli.chrmem2[memind] div 16;
        2: c1 := ifli.chrmem2[memind] mod 16;
        3: c1 := ifli.colmem[ind] and $0f;
      end;

	    buffer[6*x+0] := (buffer[6*x+0] + GetC64ColorR(c0)) div 2;
	    buffer[6*x+1] := (buffer[6*x+1] + GetC64ColorG(c0)) div 2;
	    buffer[6*x+2] := (buffer[6*x+2] + GetC64ColorB(c0)) div 2;

	    buffer[6*x+3] := (GetC64ColorR(c1) + GetC64ColorR(c0)) div 2;
	    buffer[6*x+4] := (GetC64ColorG(c1) + GetC64ColorG(c0)) div 2;
	    buffer[6*x+5] := (GetC64ColorB(c1) + GetC64ColorB(c0)) div 2;

	    buffer[6*x+6] := GetC64ColorR(c1);
	    buffer[6*x+7] := GetC64ColorG(c1);
	    buffer[6*x+8] := GetC64ColorB(c1);
    end;
    for x := 0 to 320-1 do
      ca.Pixels[x, y] := RGB(buffer[x*3+0], buffer[x*3+1], buffer[x*3+2]);
  end;
end;

procedure TC64.FFLIshow(ffli: FFLIdata; ca: TCanvas);
var x, y, ind, pos, bits, memind: integer;
    a0, c0, c1: byte;
begin
  if not assigned(ca) then exit;
  
  c0 := 0;
  c1 := 0;
  for y := 0 to 200-1 do   //todo: IFLI -> FFLI (320->160, how to mix?)
  begin
    for x := 0 to 160-1 do
    begin
      ind := x div 4 + (y div 8)*40;  //color memory index
      pos := (y mod 8) + (x div 4)*8 + (y div 8)*320;  //gfx memory byte
      bits := (x mod 4);  //bit numbers
      memind := 1024*(y mod 8) + ind;
	    a0 := (ffli.gfxmem1[pos] and bitmask[bits]) div bitshift[bits];
      case a0 of
        0: c0 := ffli.bgmem1[y];
        1: c0 := ffli.chrmem1[memind] div 16;
        2: c0 := ffli.chrmem1[memind] mod 16;
        3: c0 := ffli.colmem[ind] and $0f;
      end;
      case a0 of
        0: c1 := ffli.bgmem2[y];
        1: c1 := ffli.chrmem2[memind] div 16;
        2: c1 := ffli.chrmem2[memind] mod 16;
        3: c1 := ffli.colmem[ind] and $0f;
      end;
      ca.Pixels[x, y] := RGB(
	                           (GetC64ColorR(c1) + GetC64ColorR(c0)) div 2,
  	                         (GetC64ColorG(c1) + GetC64ColorG(c0)) div 2,
  	                         (GetC64ColorB(c1) + GetC64ColorB(c0)) div 2
      );
    end;
  end;
end;

procedure TC64.FFLI2show(ffli2: FFLI2data; ca: TCanvas); //FFLI~IFLI
var x, y, ind, pos, bits, memind: integer;
    a0, a1: byte;
    c0, c1: byte;
    buffer: array[0..3*321] of byte;
begin
  if not assigned(ca) then exit;
  
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
	    a0 := (ffli2.gfxmem1[pos] and bitmask[bits]) div bitshift[bits];
	    a1 := (ffli2.gfxmem2[pos] and bitmask[bits]) div bitshift[bits];
      case a0 of
        0: c0 := ffli2.bgmem[y];
        1: c0 := ffli2.chrmem1[memind] div 16;
        2: c0 := ffli2.chrmem1[memind] mod 16;
        3: c0 := ffli2.colmem[ind] and $0f;
      end;
      case a1 of
        0: c1 := ffli2.bgmem[y];
        1: c1 := ffli2.chrmem2[memind] div 16;
        2: c1 := ffli2.chrmem2[memind] mod 16;
        3: c1 := ffli2.colmem[ind] and $0f;
      end;

	    buffer[6*x+0] := (buffer[6*x+0] + GetC64ColorR(c0)) div 2;
	    buffer[6*x+1] := (buffer[6*x+1] + GetC64ColorG(c0)) div 2;
	    buffer[6*x+2] := (buffer[6*x+2] + GetC64ColorB(c0)) div 2;

	    buffer[6*x+3] := (GetC64ColorR(c1) + GetC64ColorR(c0)) div 2;
	    buffer[6*x+4] := (GetC64ColorG(c1) + GetC64ColorG(c0)) div 2;
	    buffer[6*x+5] := (GetC64ColorB(c1) + GetC64ColorB(c0)) div 2;

	    buffer[6*x+6] := GetC64ColorR(c1);
	    buffer[6*x+7] := GetC64ColorG(c1);
	    buffer[6*x+8] := GetC64ColorB(c1);
    end;
    for x := 0 to 320-1 do
      ca.Pixels[x, y] := RGB(buffer[x*3+0], buffer[x*3+1], buffer[x*3+2]);
  end;
end;

procedure TC64.DRAZFLIshow(drazfli: DRAZFLIdata; ca: TCanvas);
var x, y, ind, pos, bits, memind: integer;
    a0, a1: byte;
    c0, c1: byte;
    buffer: array[0..3*321] of byte;
begin
  if not assigned(ca) then exit;

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
      memind := ind;
	    a0 := (drazfli.gfxmem1[pos] and bitmask[bits]) div bitshift[bits];
	    a1 := (drazfli.gfxmem2[pos] and bitmask[bits]) div bitshift[bits];
      case a0 of
        0: c0 := drazfli.bgcol;
        1: c0 := drazfli.chrmem[memind] div 16;
        2: c0 := drazfli.chrmem[memind] mod 16;
        3: c0 := drazfli.colmem[ind] and $0f;
      end;
      case a1 of
        0: c1 := drazfli.bgcol;
        1: c1 := drazfli.chrmem[memind] div 16;
        2: c1 := drazfli.chrmem[memind] mod 16;
        3: c1 := drazfli.colmem[ind] and $0f;
      end;

	    buffer[6*x+0] := (buffer[6*x+0] + GetC64ColorR(c0)) div 2;
	    buffer[6*x+1] := (buffer[6*x+1] + GetC64ColorG(c0)) div 2;
	    buffer[6*x+2] := (buffer[6*x+2] + GetC64ColorB(c0)) div 2;

	    buffer[6*x+3] := (GetC64ColorR(c1) + GetC64ColorR(c0)) div 2;
	    buffer[6*x+4] := (GetC64ColorG(c1) + GetC64ColorG(c0)) div 2;
	    buffer[6*x+5] := (GetC64ColorB(c1) + GetC64ColorB(c0)) div 2;

	    buffer[6*x+6] := GetC64ColorR(c1);
	    buffer[6*x+7] := GetC64ColorG(c1);
	    buffer[6*x+8] := GetC64ColorB(c1);
    end;
    for x := 0 to 320-1 do
      ca.Pixels[x, y] := RGB(buffer[x*3+0], buffer[x*3+1], buffer[x*3+2]);
  end;
end;

procedure TC64.MCIshow(data: MCIFLIdata; ca: TCanvas);
var x, y, ind, pos, bits, memind: integer;
    a0, a1: byte;
    c0, c1: byte;
    buffer: array[0..3*321] of byte;
begin
  if not assigned(ca) then exit;

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
      memind := ind;
	    a0 := (data.gfxmem1[pos] and bitmask[bits]) div bitshift[bits];
	    a1 := (data.gfxmem2[pos] and bitmask[bits]) div bitshift[bits];
      case a0 of
        0: c0 := data.bgcol;
        1: c0 := data.chrmem1[memind] div 16;
        2: c0 := data.chrmem1[memind] mod 16;
        3: c0 := data.colmem[ind] and $0f;
      end;
      case a1 of
        0: c1 := data.bgcol;
        1: c1 := data.chrmem2[memind] div 16;
        2: c1 := data.chrmem2[memind] mod 16;
        3: c1 := data.colmem[ind] and $0f;
      end;

	    buffer[6*x+0] := (buffer[6*x+0] + GetC64ColorR(c0)) div 2;
	    buffer[6*x+1] := (buffer[6*x+1] + GetC64ColorG(c0)) div 2;
	    buffer[6*x+2] := (buffer[6*x+2] + GetC64ColorB(c0)) div 2;

	    buffer[6*x+3] := (GetC64ColorR(c1) + GetC64ColorR(c0)) div 2;
	    buffer[6*x+4] := (GetC64ColorG(c1) + GetC64ColorG(c0)) div 2;
	    buffer[6*x+5] := (GetC64ColorB(c1) + GetC64ColorB(c0)) div 2;

      buffer[6*x+6] := GetC64ColorR(c1);
      buffer[6*x+7] := GetC64ColorG(c1);
      buffer[6*x+8] := GetC64ColorB(c1);
    end;
    for x := 0 to 320-1 do
      ca.Pixels[x, y] := RGB(buffer[x*3+0], buffer[x*3+1], buffer[x*3+2]);
  end;
end;

procedure TC64.HIIshow(data: HIIdata; ca: TCanvas);
var x, y, pos, bits, memind, p: integer;
    a0, a1: byte;
    c0, c1: byte;
begin
  if not assigned(ca) then exit;

  for y := 0 to 200-1 do
  begin
    for x := 0 to 320-1 do
    begin
      memind := x div 8 + (y div 8)*40;  //color memory index
      pos := (y mod 8) + (x div 8)*8 + (y div 8)*320;  //gfx memory byte
      bits := (x mod 8);  //bit numbers
      p := pow[7-bits];
	    a0 := (data.gfxmem1[pos] and p) div p;
	    a1 := (data.gfxmem2[pos] and p) div p;
      if a0 <> 0 then
        c0 := data.chrmem1[memind] div 16
      else
        c0 := data.chrmem1[memind] mod 16;
      if a1 <> 0 then
        c1 := data.chrmem2[memind] div 16
      else
        c1 := data.chrmem2[memind] mod 16;

      ca.Pixels[x, y] := RGB(
          (GetC64ColorR(c1) + GetC64ColorR(c0)) div 2,
          (GetC64ColorG(c1) + GetC64ColorG(c0)) div 2,
          (GetC64ColorB(c1) + GetC64ColorB(c0)) div 2
        );
    end;
  end;
end;

//---

procedure TC64.RAWHIRESload(ca: TCanvas);
var data: HIRESdata;
    g: word;
    c: byte;
begin
  if not assigned(ca) then exit;
  HIRESclear(data);
  c := (FColors4[0] and $0f) + ((FColors4[1] and $0f) shl 4);
  ReadAndDiscardHead;
  for g := 0 to 8000-1 do read(f, data.bitmap[g]);
  for g := 0 to 1000-1 do data.ink[g] := c;
  HIRESshow(data, ca);
end;

procedure TC64.RAWMULTIoad(ca: TCanvas);
var data: MULTIdata;
    g: word;
    c: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  c := (FColors4[1] and $0f) + ((FColors4[2] and $0f) shl 4);
  ReadAndDiscardHead;
  for g := 0 to $7f40-$6000-1 do read(f, data.bitmap[g]);
  //note: not yet sure if ink1/ink2 not reversed
  for g := 0 to $8328-$7f40-1 do data.ink1[g] := c;
  for g := 0 to $8710-$8328-1 do data.ink2[g] := FColors4[3] and $0f;
  data.backGr := FColors4[0];
  MULTICOLORshow(data, ca);
end;

procedure TC64.KOALAload(ca: TCanvas);
var data: MULTIdata;
    g: word;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  ReadAndDiscardHead;
  for g := 0 to 7999 do read(f, data.bitmap[g]); //$7f40-$6000-1
  for g := 0 to 999 do read(f, data.ink1[g]); //$8328-$7f40-1
  for g := 0 to 999 do read(f, data.ink2[g]); //$8710-$8328-1
  read(f, data.backGr);
  MULTICOLORshow(data, ca);
end;

procedure TC64.KOALAload_RLE(ca: TCanvas);
var data: MULTIdata;
    i_buff, o_buff: TAmicaBuff;
    i, g: word;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  g := 0;
  while not eof(f) and (g < TOP_BUFF+1) do
  begin
    read(f, i_buff[g]);
    inc(g);
  end;
  for i := 0 to TOP_BUFF do o_buff[i] := 0;
  unpackRLE_V_C($FE, i_buff, g, o_buff, sizeof(TAmicaBuff));

  for g := 0 to 7999 do data.bitmap[g] := o_buff[g];  
  for g := 0 to 999 do data.ink1[g] := o_buff[8000+g];
  for g := 0 to 999 do data.ink2[g] := o_buff[8000+1000+g];
  data.backGr := o_buff[8000+1000+1000];

  MULTICOLORshow(data, ca);
end;

(*
Wigmore Artist64 (by wigmore) (pc-ext: .a64)
load address: $4000 - $67FF
$4000 - $5F3F 	Bitmap
$6000 - $63E7 	Screen RAM
$6400 - $67E7 	Color RAM
$67FF 	Background
*)
procedure TC64.WIGMOREload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);  
  ReadAndDiscardHead;
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to $6000-$5F3F-1-1 do read(f, none); //?
  for g := 0 to 999 do read(f, data.ink1[g]);
  for g := 0 to $6400-$63E7-1-1 do read(f, none); //?
  for g := 0 to 999 do read(f, data.ink2[g]);
  read(f, data.backGr); //todo: chk this one
  MULTICOLORshow(data, ca);
end;

(*
RunPaint (pc-ext: .rpm)
load address: $6000 - $8710
$6000 - $7F3F 	Bitmap
$7F40 - $8327 	Screen RAM
$8328 - $870F 	Color RAM
$8710 	Background
*)
procedure TC64.RUNPAINTload(ca: TCanvas);
var data: MULTIdata;
    g: word;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);  
  ReadAndDiscardHead;
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 999 do read(f, data.ink1[g]);
  for g := 0 to 999 do read(f, data.ink2[g]);
  read(f, data.backGr);
  MULTICOLORshow(data, ca);
end;

(*
Image System (Multicolor) (pc-ext: .ism;.ims)
load address: $3C00 - $63E8
$3C00 - $3FE7 	Color RAM
$4000 - $5F3F 	Bitmap
$5FFF 	Background
$6000 - $63E7 	Screen RAM
*)
procedure TC64.IMGSYSload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);  
  ReadAndDiscardHead;
  for g := 0 to 999 do read(f, data.ink2[g]);
  for g := 0 to 23 do read(f, none); //?
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 191-1 do read(f, none); //10218-2-1000-1000-24-8000-1 = 191
  read(f, data.backGr);
  for g := 0 to 999 do read(f, data.ink1[g]);
  MULTICOLORshow(data, ca);
end;

(*
Paint Magic (pc-ext: .pmg)
load address: $3F8E - $63FF
$3F8E - $3FFF 	Display Routine
$4000 - $5F3F 	Bitmap
$5F40 	Background
$5F43 	Color RAM Byte
$5F44 	Border
$6000 - $63E7 	Screen RAM
*)
procedure TC64.PAMAGload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    c, none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);  
  ReadAndDiscardHead;
  for g := 0 to $3FFF-$3F8E-1+(1) do read(f, none); //display routine
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  read(f, data.backGr);
  read(f, none, none); //skip
  read(f, c); //Color RAM Byte
  read(f, none); //border - ignore
  for g := 0 to 999 do data.ink2[g] := c;
  for g := 0 to $6000-$5F45-1 do read(f, none); //skip
  for g := 0 to 999 do read(f, data.ink1[g]);
  MULTICOLORshow(data, ca);
end;

(*
Advanced Art Studio 2.0 (by OCP) (pc-ext: .ocp;.art) -- Frontpic.art / gfartist.art / MONALISA.ART / PRODIGY.ART / SIANO.ART
load address: $2000 - $471F
$2000 - $3F3F 	Bitmap
$3F40 - $4327 	Screen RAM
$4328 	Border
$4329 	Background
$4338 - $471F 	Color RAM
*)
procedure TC64.ADVARTSTload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);  
  ReadAndDiscardHead;
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 999 do read(f, data.ink1[g]);
  read(f, none);
  read(f, data.backGr);
  for g := 0 to $4338-$432A-1 do read(f, none); //skip
  for g := 0 to 999 do read(f, data.ink2[g]);
  MULTICOLORshow(data, ca);
end;

(*
CDU-Paint (pc-ext: .cdu)
load address: $7EEF - $a711
$7FEF - $7FFF 	Display routine
$8000 - $9F3F 	Bitmap
$9F40 - $A328 	Screen RAM (???)
$A328 - $A710 	Color RAM (???)
$A710 	Background (???)
*)
procedure TC64.CDUload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  ReadAndDiscardHead;
  for g := 0 to 272 do read(f, none); //skip display routine
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 999 do read(f, data.ink1[g]);
  for g := 0 to 999 do read(f, data.ink2[g]);
  read(f, data.backGr);
  MULTICOLORshow(data, ca);
end;

procedure TC64.RPload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  ReadAndDiscardHead;
  for g := 0 to 999 do read(f, data.ink1[g]);
  for g := 0 to 23 do read(f, none); //skip
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 191 do read(f, none); //skip
  for g := 0 to 999 do read(f, data.ink2[g]);
  data.backGr := 0; //?
  MULTICOLORshow(data, ca);
end;

(*
Blazing Paddles (by BAUDEVILLE) (pc-ext: .blp;.pi)
load address: $A000 - $C7FF
$A000 - $BF3F 	Bitmap
$BF80 	Background
$C000 - $C3E7 	Screen RAM
$C400 - $C7E7 	Color RAM
*)
procedure TC64.BLPload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  ReadAndDiscardHead;
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 63 do read(f, none); //skip
  read(f, data.backGr);
  for g := 0 to 127-1 do read(f, none); //skip
  for g := 0 to 999 do read(f, data.ink1[g]);
  for g := 0 to 23 do read(f, none); //skip
  for g := 0 to 999 do read(f, data.ink2[g]);
  MULTICOLORshow(data, ca);
end;

(*
Vidcom 64 (by OMEGA) (pc-ext: .vid) 
load address: $5800 - $7F3F
$5800 - $5BE7 	Color RAM
$5C00 - $5FE7 	Screen RAM
$5FE8 	Background
$6000 - $7F3F 	Bitmap
*)
procedure TC64.VIDload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  ReadAndDiscardHead;
  for g := 0 to 999 do read(f, data.ink2[g]);
  for g := 0 to 23 do read(f, none); //skip
  for g := 0 to 999 do read(f, data.ink1[g]);
  read(f, data.backGr);
  read(f, data.backGr); //apparently this one
  for g := 0 to 23-1-1 do read(f, none); //skip
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  MULTICOLORshow(data, ca);
end;

(*
Saracen Paint (by IDEA) (pc-ext: .sar)
load address: $7800 - $9FE7
$7800 - $7BE7 	Screen RAM
$7BF0 	Background
$7C00 - $9B3F 	Bitmap
$9C00 - $9FE7 	Color RAM
*)
procedure TC64.SARload(ca: TCanvas);
var data: MULTIdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  MULTICOLORclear(data);
  ReadAndDiscardHead;
  for g := 0 to 999 do read(f, data.ink1[g]);
  for g := 0 to 7-1 do read(f, none); //skip
  read(f, data.backGr); //todo: why differs from congo? my seems proper
  for g := 0 to 15-1+1 do read(f, none); //skip
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 191 do read(f, none); //skip  
  for g := 0 to 999 do read(f, data.ink2[g]);
  MULTICOLORshow(data, ca);
end;

procedure TC64.HIRESload(ca: TCanvas);
var data: HIRESdata;
    g: word;
begin
  if not assigned(ca) then exit;
  HIRESclear(data);
  {9009 bytes - what's that!?}
  ReadAndDiscardHead;
  for g := 0 to $3f40-$2000-1 do read(f, data.bitmap[g]);
  for g := 0 to $4328-$3f40-1 do read(f, data.ink[g]);
  HIRESshow(data, ca);
end;

procedure TC64.HIRESloadHED(ca: TCanvas);
var data: HIRESdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  HIRESclear(data);  
  ReadAndDiscardHead;
  for g := 0 to $3f3f-$2000-1 do read(f, data.bitmap[g]);
  for g := 0 to $4000-$3f3f-1 do read(f, none); //skip
  for g := 0 to $43e7-$4000-1 do read(f, data.ink[g]);
  HIRESshow(data, ca);
end;

procedure TC64.HIRESloadDDL(ca: TCanvas);
var data: HIRESdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  HIRESclear(data);
//9218-1000-8000 = 218
  read(f, none, none);
  for g := 0 to 999 do read(f, data.ink[g]);
  for g := 0 to 7+8+8 do read(f, none); //but why???
  for g := 0 to $7f3f-$6000 do read(f, data.bitmap[g]);
  HIRESshow(data, ca);
end;

procedure TC64.HIRESloadDDL_RLE(ca: TCanvas);
var data: HIRESdata;
    i_buff, o_buff: TAmicaBuff;
    i, g: word;
begin
  if not assigned(ca) then exit;
  HIRESclear(data);
  g := 0;
  while not eof(f) and (g < TOP_BUFF+1) do
  begin
    read(f, i_buff[g]);
    inc(g);
  end;
  for i := 0 to TOP_BUFF do o_buff[i] := 0;
  unpackRLE_V_C($FE, i_buff, g, o_buff, sizeof(TAmicaBuff));
  for g := 0 to 999 do data.ink[g] := o_buff[g];
  for g := 0 to 7999 do data.bitmap[g] := o_buff[1000+(7+8+8+1)+g];
  HIRESshow(data, ca);
end;

(*
Image System (Hires) (pc-ext: .ish)
load address: $4000 - $63e7
$4000 - $5f3f 	Bitmap
$6000 - $63e7 	Screen RAM
*)
procedure TC64.HIRESloadISH(ca: TCanvas);
var data: HIRESdata;
    g: word;
begin
  if not assigned(ca) then exit;
  HIRESclear(data);
  ReadAndDiscardHead;
  for g := 0 to 7999 do read(f, data.bitmap[g]);
  for g := 0 to 999 do read(f, data.ink[g]);
  HIRESshow(data, ca);
end;

(*
Interpaint Hires (pc-ext: .ip64h)
load address: $4000 - $6327
$4000 - $5f3f 	Bitmap
$5f40 - $6327 	Screen RAM
*)
procedure TC64.HIRESloadIPH(ca: TCanvas);
begin
  HIRESloadISH(ca); //the same
end;

procedure TC64.AMICAload(ca: TCanvas);
var koala: MULTIdata;
    i_buff, o_buff: TAmicaBuff;
    b, i: integer;
begin
  if not assigned(ca) then exit;

  b := 0;
  while not eof(f) and (b < TOP_BUFF+1) do
  begin
    read(f, i_buff[b]);
    inc(b);
  end;

  for i := 0 to TOP_BUFF do o_buff[i] := 0;
  AMICAunpack(2, $c2, i_buff, b, o_buff, sizeof(TAmicaBuff));

  for i := 0 to 8000-1 do koala.bitmap[i] := o_buff[i]; //320*200/8 = 8000
  for i := 0 to 1000-1 do koala.ink1[i] := o_buff[8000+i];
  for i := 0 to 1000-1 do koala.ink2[i] := o_buff[8000+1000+i]; //$D800
  koala.backGr := o_buff[$F710-$c000] and $0f;

  MULTICOLORshow(koala, ca);
end;

procedure TC64.LOGOload(ca: TCanvas);
var data: LOGOdata;
    g: word;
    none: byte;
begin
  if not assigned(ca) then exit;
  LOGOclear(data);
  ReadAndDiscardHead;
  for g := 0 to $1c00-$1800-1 do read(f, data.logo[g]);
  for g := $1800-$1800 to $2000-$1c00-1 do read(f, none);
  for g := $2000-$2000 to $2800-$2000-1 do read(f, data.bitmap[g]);
  LOGOshow(data, ca);
end;

procedure TC64.FNTload(ca: TCanvas);
var data: FNTdata;
    g, h: byte;
begin
  if not assigned(ca) then exit;

  FNTclear(data);
  ReadAndDiscardHead;
  for g := 1 to 64 do
    for h := 0 to 7 do
      read(f, data.fnt[g, h]);

  for h := 1 to 40 do
    FNTshow(h*8-8, 0, data, ca, (h));
  for h := 1 to 24 do
    FNTshow(h*8-8, 8, data, ca, (40+h));
end;

procedure TC64.FNTBload(ca: TCanvas);
var data: FNTBdata;
    g, h: byte;
begin
  if not assigned(ca) then exit;

  FNTBclear(data);
  ReadAndDiscardHead;
  for g := 1 to 64 do for h := 0 to 7 do read(f, data.fntb[g, h]);
  for g := 1 to 64 do for h := 0 to 7 do read(f, data.fntb[g, h+8]);
  for g := 1 to 64 do for h := 0 to 7 do read(f, data.fntb[g, h+16]);
  for g := 1 to 64 do for h := 0 to 7 do read(f, data.fntb[g, h+24]);

  for h := 1 to 20 do
    FNTBshow(h*16-16, 0, data, ca, (h));
  for h := 1 to 20 do
    FNTBshow(h*16-16, 16, data, ca, (20+h));
  for h := 1 to 20 do
    FNTBshow(h*16-16, 32, data, ca, (40+h));
  for h := 1 to 4 do
    FNTBshow(h*16-16, 48, data, ca, (60+h));
end;

procedure TC64.MOBload(ca: TCanvas);
var mob: MOBdata;
    g: byte;
begin
  if not assigned(ca) then exit;

  MOBclear(mob);

  mob.cnt := 1;
  ReadAndDiscardHead;
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

  if FAsHires then
  begin
    for g := 1 to 13 do
      if g <= mob.cnt then
        hMOBshow(g*24-24, 0, mob, ca, (g)); //todo: more
  end
  else
  begin
    for g := 1 to 13 do
      if g <= mob.cnt then
        mMOBshow(g*24-24, 0, mob, ca, (g)); //todo: more
  end;
end;

(*
Hires FLI (by Crest) (pc-ext: .hfc) [AFLI]
load address: $4000 - $7fff
$4000 - $5f3f 	Bitmap
$6000 - $7fe7 	Screen RAMs
*)
procedure TC64.AFLIload(ca: TCanvas);
var fli: FLIdata;
    none: byte;
    i, j: integer;
begin
  if not assigned(ca) then exit;

  FLIclear(fli);
  ReadAndDiscardHead;

  for i := 0 to 7999 do
    read(f, fli.gfxmem[i]);
  //16386 - 8000 - 2 = 8384 / 8*1024 = 8192 / 8384-8192 = 192
  for j := 0 to 191 do read(f, none);
  for j := 0 to 7 do
    for i := 0 to 1023 do
      read(f, fli.chrmem[j][i]);

  FLIshow(fli, ca, C64_AFLI);
end;

(*
Gunpaint (pc-ext: .gun,.ifl) 
load address: $4000 - $c340
$4000 - $5fe7 	Screen RAMs 1
$43e8 - $43f7 	header db 'GUNPAINT (JZ) '
$6000 - $7f3f 	Bitmap 1
$7f4f - $7fff 	177 x $d021 colors
$8000 - $83e7 	Color RAM
$8400 - $a3e7 	Screen RAMs 2
$87e8 - $87fb 	20 x $d021 colors
$a400 - $c33f 	Bitmap 2
$c340 	???
*)
(* Gunpaint IFLI format - alternative to the above
Start address = $4000, end = $c341
$4000 - $6000     FLI screenmaps 1
$6000 - $7f40     FLI bitmap 1
$8000 - $8400     Colourmap  ($d800 colours)
$8400 - $a400     FLI screenmaps 2
$a400 - $c340     FLI bitmap 2
$c341             ???   (doesn't seem to be important..)
*)
procedure TC64.GUNFLIload(ca: TCanvas);
var ifli: IFLIdata;
    none: byte;
    i: integer;
begin
  if not assigned(ca) then exit;

  IFLIclear(ifli);
  ReadAndDiscardHead;
  //33603 total

  for i := 0 to 8192-1 do read(f, ifli.chrmem1[i]);  //$4000 - $5fe7 = Screen RAMs 1
  for i := 0 to 8000-1 do read(f, ifli.gfxmem1[i]);  //$6000 - $7f3f = Bitmap 1
  for i := 0 to 15-1 do read(f, none);               //?
  for i := 0 to 177-1 do read(f, none);              //$7f4f - $7fff = 177 x $d021 colors
  for i := 0 to 1024-1 do read(f, ifli.colmem[i]);   //$8000 - $83e7 = Color RAM
  for i := 0 to 8192-1 do read(f, ifli.chrmem2[i]);  //$8400 - $a3e7 = Screen RAMs 2
  for i := 0 to 8000-1 do read(f, ifli.gfxmem2[i]);  //$a400 - $c33f = Bitmap 27999+1

  IFLIshow(ifli, ca); //C64_GUN_IFLI
end;

(*
Funpaint 2 (by Manfred Trenz) (pc-ext: .fp2;.fun)
load address: $3ff0 - $c38b
$3ff0 - $3fff 	'FUNPAINT (MT) ' NB: space
$3ffe 	Flag: packed if non-zero
$3fff 	Escape byte for unpacking 
$4000 - $5fe7 	Screen RAMs 1
$6000 - $7f3f 	Bitmap 1
$7f48 - $7fab 	100 x $d021 color
$8000 - $83e7 	Color RAM
$83e8 - $a3e7 	Screen RAMs 2
$a3e8 - $c327 	Bitmap 2
$c328 - $c38b 	100 x $d021 color
RLE sequences are ESCAPE,COUNT,BYTE
*)
procedure TC64.FUNFLIload(ca: TCanvas);
var ffli2: FFLI2data;
    i_buff, o_buff: TAmicaBuff;
    none, flag, esc: byte;
    i: integer;
begin
  if not assigned(ca) then exit;

  FFLI2clear(ffli2);
  ReadAndDiscardHead;
  //total max: 33694

  for i := 0 to 14-1 do read(f, none); //skip header
  read(f, flag);
  read(f, esc);

  if flag <> 0 then  //packed
  begin
    for i := 0 to TOP_BUFF do o_buff[i] := 0;
    i := 0;
    while not eof(f) and (i < TOP_BUFF+1) do
    begin
      read(f, i_buff[i]);
      inc(i);
    end;
    AMICAunpack(0, esc, i_buff, i, o_buff, sizeof(TAmicaBuff));
    for i := 0 to 8192-1 do ffli2.chrmem1[i] := o_buff[i];
    for i := 0 to 8008-1 do ffli2.gfxmem1[i] := o_buff[8192+i];
    for i := 0 to 100-1 do ffli2.bgmem[i] := o_buff[8192+8008+i];
    for i := 0 to 1000-1 do ffli2.colmem[i] := o_buff[8192+8008+100+84+i];
    for i := 0 to 8192-1 do ffli2.chrmem2[i] := o_buff[8192+8008+100+84+1000+i];
    for i := 0 to 8000-1 do ffli2.gfxmem2[i] := o_buff[8192+8008+100+84+1000+8192+i];
    for i := 0 to 100-1 do ffli2.bgmem[i+100] := o_buff[8192+8008+100+84+1000+8192+8000+i];
  end
  else
  begin
    for i := 0 to 8192-1 do read(f, ffli2.chrmem1[i]);
    for i := 0 to 8008-1 do read(f, ffli2.gfxmem1[i]);
    for i := 0 to 100-1 do read(f, ffli2.bgmem[i]);
    for i := 0 to 84-1 do read(f, none);
    for i := 0 to 1000-1 do read(f, ffli2.colmem[i]);
    for i := 0 to 8192-1 do read(f, ffli2.chrmem2[i]);
    for i := 0 to 8000-1 do read(f, ffli2.gfxmem2[i]);
    for i := 0 to 100-1 do read(f, ffli2.bgmem[i+100]);
  end;

  FFLI2show(ffli2, ca); //C64_FUN
end;

(*
Drazlace (pc-ext: .drl)
"Crippled" Multicolor Interlaced - 2 x Multicolor Bitmap, shared Screen RAM and Color RAM
load address: $5800 - $9f3f
$5800 - $5be7 	Color RAM
$5c00 - $5fe7 	Screen RAM
$6000 - $7f3f 	Bitmap 1
$7f40 	Background
$7f42 	$d016 flag
$8000 - $9f3f 	Bitmap 2
*)
procedure TC64.DRLload(ca: TCanvas);
var drazfli: DRAZFLIdata;
    i_buff, o_buff: TAmicaBuff;
    none, esc: byte;
    tmp: array[0..12] of byte;
    i: integer;
    hd: string;
begin
  if not assigned(ca) then exit;

  DRAZFLIclear(drazfli);
  ReadAndDiscardHead;
  //18239 total max

  //packed file starts with header 'DRAZLACE! 1.0' and escape byte. RLE: ESCAPE,COUNT,BYTE
  hd := '';
  for i := 0 to 12 do begin read(f, tmp[i]); hd := hd + chr(tmp[i]); end;

  if hd = 'DRAZLACE! 1.0' then //packed
  begin
    read(f, esc);
    for i := 0 to TOP_BUFF do o_buff[i] := 0;
    i := 0;
    while not eof(f) and (i < TOP_BUFF+1) do
    begin
      read(f, i_buff[i]);
      inc(i);
    end;
    AMICAunpack(0, esc, i_buff, i, o_buff, sizeof(TAmicaBuff));
    for i := 0 to 1024-1 do drazfli.colmem[i] := o_buff[i];
    for i := 0 to 1024-1 do drazfli.chrmem[i] := o_buff[1024+i];
    for i := 0 to 8000-1 do drazfli.gfxmem1[i] := o_buff[1024+1024+i];
    drazfli.bgcol := o_buff[1024+1024+8000+0];
    for i := 0 to 8000-1 do drazfli.gfxmem2[i] := o_buff[1024+1024+8000+1+1+1+189+i];
  end
  else
  begin
    seek(f, 0);
    read(f, none, none);
    for i := 0 to 1024-1 do read(f, drazfli.colmem[i]);   //Color RAM
    for i := 0 to 1024-1 do read(f, drazfli.chrmem[i]);  //Screen RAMs
    for i := 0 to 8000-1 do read(f, drazfli.gfxmem1[i]);  //Bitmap 1
    read(f, drazfli.bgcol); //bg
    read(f, none);
    read(f, none); //$d016 flag - what to do with it? ignore for now
    for i := 0 to 189-1 do read(f, none);
    for i := 0 to 8000-1 do read(f, drazfli.gfxmem2[i]);  //Bitmap 2
  end;

  DRAZFLIshow(drazfli, ca); //C64_DRL
end;

(*
True Paint (by Fatum) (pc-ext: .mci)
load address: $9C00 - $E7E7
$9C00 - $9FE7 	Screen RAM 1
$9FE8 	Background
$A000 - $BF3F 	Bitmap 1
$C000 - $DF3F 	Bitmap 2
$E000 - $E3E7 	Screen RAM 2
$E400 - $E7E7 	Color RAM
*)
procedure TC64.MCIload(ca: TCanvas);
var data: MCIFLIdata;
    none: byte;
    i: integer;
begin
  if not assigned(ca) then exit;

  MCIFLIclear(data);
  ReadAndDiscardHead;

  for i := 0 to 1000-1 do read(f, data.chrmem1[i]);  //Screen RAM
  read(f, data.bgcol); //bg
  for i := 1 to 8+16-1 do read(f, none);
  for i := 0 to 8000-1 do read(f, data.gfxmem1[i]);  //Bitmap 1
  for i := 1 to 8192-8000 do read(f, none);
  for i := 0 to 8000-1 do read(f, data.gfxmem2[i]);  //Bitmap 2
  for i := 1 to 8192-8000 do read(f, none);
  for i := 0 to 1000-1 do read(f, data.chrmem2[i]);
  for i := 1 to 1024-1000 do read(f, none);
  for i := 0 to 1000-1 do read(f, data.colmem[i]);   //Color RAM

  MCIshow(data, ca); //C64_MCI
end;
                                               
(*
Hires-Interlace v1.0 (Feniks) (pc-ext: .hlf) 
load address: $4000 - $7f3f - or rather $2000 ?
$2000 - $3f3f 	Bitmap 1
$4400 - $47e7 	Screen RAM 1
$4800 - $4be7 	Screen RAM 2
$6000 - $7f3f 	Bitmap 2
*)
procedure TC64.HIIload(ca: TCanvas);
var data: HIIdata;
    none: byte;
    i: integer;
begin
  if not assigned(ca) then exit;

  HIIclear(data);
  ReadAndDiscardHead;

  for i := 0 to 8000-1 do read(f, data.gfxmem1[i]);
  for i := 0 to 1216-1 do read(f, none);
  for i := 0 to 1000-1 do read(f, data.chrmem1[i]);
  for i := 0 to 24-1 do read(f, none);
  for i := 0 to 1000-1 do read(f, data.chrmem2[i]);
  for i := 0 to 5144-1 do read(f, none);
  for i := 0 to 8000-1 do read(f, data.gfxmem2[i]);

  HIIshow(data, ca); //C64_HLF
end;

(*
Hires Manager (by Cosmos) (pc-ext: .him)
Unpacked format:
load address: $4000 - $7ffe
$4000 - $5f3f 	Bitmap
$4001 	format marker, $ff = unpacked
$6000 - $7fe7 	Screen RAMs
Packed format:
load address: $4000
$4000 - $4001 	Pointer to end of packed data
$4002 - $4003 	Pointer to end of unpacked data + 1 ($7ff2)
---
Data packed backwards 
$00 = escape byte, RLE sequences: $00,COUNT,BYTE, 
and literal sequences are COUNT+1,DATA,DATA… 
Literal sequences are buggy and overlap with the next sequence
---
Hires Manager images are only 192 lines tall, 
and the packed format only saves $4140 - $5f3f of the bitmap
*)
procedure TC64.HIMload(ca: TCanvas);
var data: FLIdata;
    none, fmt: byte;
    i, j, m: integer;
begin
  if not assigned(ca) then exit;

  FLIclear(data);
  ReadAndDiscardHead;

  read(f, none, fmt);
  seek(f, 0);
  if fmt <> $ff then raise(Exception.Create('Packed Hires Manager not supported yet!'));
  read(f, none, none);

  for i := 0 to 8192-1 do read(f, data.gfxmem[i]);
  for j := 0 to 7 do  //8168 = screen RAMs total
  begin
    m := 1024;
    if j = 7 then m := 1000; //what a mess! 
    for i := 0 to m-1 do
      read(f, data.chrmem[j][i]);
  end;

  HIMshow(data, ca);
end;

procedure TC64.FLIload(ca: TCanvas);
var fli: FLIdata;
    ifli: IFLIdata;
    ffli: FFLIdata;
    temp: array[0..9] of byte;
    none: byte;
    i, j: integer;
begin
  if not assigned(ca) then exit;

  FLIclear(fli);
  IFLIclear(ifli);
  FFLIclear(ffli);

  read(f, temp[0], temp[1]);

	if ((temp[0] = 0) and (temp[1] = $40)) then //AFLI file
  begin
//    showmessage('DEBUG: AFLI detected');
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
//    showmessage('DEBUG: FLI detected');  
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

  if (temp[0] = $ff) and ((temp[1] = $3a) or (temp[1] = $3b)) then //BFLI or FFLI
  begin
    read(f, temp[2]);
    if (temp[2] = ord('b')) then //BFLI file
    begin
//      showmessage('DEBUG: BFLI detected');

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
//      showmessage('DEBUG: FFLI detected');

(*
	Offset	Address	Size	Used	Description
	------	-------	----	----	-----------
	0		          2        2	Load address ($3aff)
	2	    $3aff	  1        1	'f' FFLI identification
	3	    $3b00	256	     200	Background colors for each line
	259	  $3c00	1024	  1000	Color memory
	1283	$4000	8192	8x1000	FLI color memories
	9475	$6000	8192	  8000	The bitmap (multicolor mode)
	17667	$8000	8192	8x1000	FLI color memories
	25859	$a000	  256	   200	Background colors for each line
	26115	$a100
*)

      for i := 0 to 256-1 do read(f, ffli.bgmem1[i]);
      for i := 0 to 1024-1 do read(f, ffli.colmem[i]);
      for i := 0 to 8192-1 do read(f, ffli.chrmem1[i]);
      for i := 0 to 8192-1 do read(f, ffli.gfxmem1[i]);
      for i := 0 to 8192-1 do read(f, ffli.chrmem2[i]);
      for i := 0 to 256-1 do read(f, ffli.bgmem2[i]);
      FFLIshow(ffli, ca);
      exit;
    end
    else
    begin //unknown
      raise(Exception.Create('Unknown FLI file format ID '+inttostr(temp[2])));
    end;
  end;

//IFLI based on: C64 Horizontal 'Interlaced' FLI By Pasi 'Albert' Ojala © 1991-1998

//note: gunpaint - separate code

  if ((temp[0] = $0) and (temp[1] = $3f)) then //IFLI
  begin
    read(f, temp[2]);
    if (temp[2] = ord('I')) then //IFLI file
    begin
//      showmessage('DEBUG: IFLI detected');

      for i := 0 to 8192-1 do read(f, ifli.chrmem1[i]);
      for i := 0 to 8192-1 do read(f, ifli.gfxmem1[i]);
      for i := 0 to 1024-1 do read(f, ifli.colmem[i]);
      for i := 0 to 8192-1 do read(f, ifli.chrmem2[i]);
      for i := 0 to 8192-1 do read(f, ifli.gfxmem2[i]);
      IFLIshow(ifli, ca);
    end
    else
      raise(Exception.Create('Unknown IFLI file format ID '+inttostr(temp[2])));
  end;

  raise(Exception.Create('Unknown FLI format (' + inttohex(temp[0], 2) + inttohex(temp[1], 2) + ')'));
end;

//---

function TC64.LoadRawToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
begin
  if AsHires then
    result := GenericLoader(FileName, RAWHIRESload, ca, mode)
  else
    result := GenericLoader(FileName, RAWMULTIoad, ca, mode);
end;

function TC64.LoadMulticolorToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
begin
  case mode of
    C64_KOALA:      result := GenericLoader(FileName, KOALAload, ca, mode);
    C64_WIGMORE:    result := GenericLoader(FileName, WIGMOREload, ca, mode);
    C64_RUNPAINT:   result := GenericLoader(FileName, RUNPAINTload, ca, mode);
    C64_ISM:        result := GenericLoader(FileName, IMGSYSload, ca, mode);
    C64_PAINTMAGIC: result := GenericLoader(FileName, PAMAGload, ca, mode);
    C64_ADVARTST:   result := GenericLoader(FileName, ADVARTSTload, ca, mode);
    C64_CDU:        result := GenericLoader(FileName, CDUload, ca, mode);
    C64_RAINBOW:    result := GenericLoader(FileName, RPload, ca, mode);
    C64_KOALA_RLE:  result := GenericLoader(FileName, KOALAload_RLE, ca, mode);
    C64_BLP:        result := GenericLoader(FileName, BLPload, ca, mode);
    C64_VID:        result := GenericLoader(FileName, VIDload, ca, mode);
    C64_SAR:        result := GenericLoader(FileName, SARload, ca, mode);
    else result := -1;
  end;
end;

function TC64.LoadHiresToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
begin
  case mode of
    C64_HIRES:   result := GenericLoader(FileName, HIRESload, ca, mode);
    C64_HED:     result := GenericLoader(FileName, HIRESloadHED, ca, mode);
    C64_DDL:     result := GenericLoader(FileName, HIRESloadDDL, ca, mode);
    C64_ISH:     result := GenericLoader(FileName, HIRESloadISH, ca, mode);
    C64_DDL_RLE: result := GenericLoader(FileName, HIRESloadDDL_RLE, ca, mode);
    C64_IPH:     result := GenericLoader(FileName, HIRESloadIPH, ca, mode);
    else result := -1;
  end;
end;

function TC64.LoadAmicaToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, AMICAload, ca, C64_AMICA);
end;

function TC64.LoadLogoToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, LOGOload, ca, C64_LOGO);
end;

function TC64.LoadFontToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, FNTload, ca, C64_FNT);
end;

function TC64.LoadFont2x2ToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, FNTBload, ca, C64_FNTB);
end;

function TC64.LoadMobToBitmap(FileName: string; ca: TCanvas): integer;
begin
  result := GenericLoader(FileName, MOBload, ca, C64_MOB);
end;

function TC64.LoadFliToBitmap(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
begin
  case mode of
    C64_HFC:      result := GenericLoader(FileName, AFLIload, ca, C64_HFC);
    C64_GUN_IFLI: result := GenericLoader(FileName, GUNFLIload, ca, C64_GUN_IFLI);
    C64_FUN:      result := GenericLoader(FileName, FUNFLIload, ca, C64_FUN);
    C64_DRL:      result := GenericLoader(FileName, DRLload, ca, C64_DRL);
    C64_MCI:      result := GenericLoader(FileName, MCIload, ca, C64_MCI);
    C64_HLF:      result := GenericLoader(FileName, HIIload, ca, C64_HLF);
    C64_HIM:      result := GenericLoader(FileName, HIMload, ca, C64_HIM);
    else          result := GenericLoader(FileName, FLIload, ca, C64_FLI);
  end;
end;

//---

function TC64.LoadC64ToBitmap(FileName: string; ca: TCanvas): integer;
var mode: TC64FileType;
begin
  mode := ExtMapper(ExtractFileExt(FileName));
  result := LoadC64ToBitmapByMode(FileName, ca, mode);
end;

function TC64.LoadC64ToBitmapByMode(FileName: string; ca: TCanvas; mode: TC64FileType): integer;
begin
  FLastError := 'Unknown format extension';
  case mode of
    C64_KOALA,
    C64_KOALA_RLE,
    C64_WIGMORE,
    C64_RUNPAINT,
    C64_ISM,
    C64_PAINTMAGIC,
    C64_ADVARTST,
    C64_CDU,
    C64_RAINBOW,
    C64_BLP,
    C64_VID,
    C64_SAR:      result := LoadMulticolorToBitmap(FileName, ca, mode);
    C64_AMICA:    result := LoadAmicaToBitmap(FileName, ca);
    C64_HIRES,
    C64_HED,
    C64_DDL,
    C64_DDL_RLE,
    C64_ISH,
    C64_IPH:      result := LoadHiresToBitmap(FileName, ca, mode);
    C64_LOGO:     result := LoadLogoToBitmap(FileName, ca);
    C64_FNT:      result := LoadFontToBitmap(FileName, ca);
    C64_FNTB:     result := LoadFont2x2ToBitmap(FileName, ca);
    C64_MOB,      
    C64_MBF:      result := LoadMobToBitmap(FileName, ca);
    C64_FLI,
    C64_AFLI,
    C64_BFLI,
    C64_IFLI,
    C64_FFLI,
    C64_HFC,
    C64_GUN_IFLI,
    C64_FUN,
    C64_DRL,
    C64_MCI,
    C64_HLF,    
    C64_HIM:      result := LoadFliToBitmap(FileName, ca, mode);
    C64_RAW:      result := LoadRawToBitmap(FileName, ca, mode);
    else
      result := -1;
  end;
end;

end.

