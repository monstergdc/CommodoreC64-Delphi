unit c64demoMainUnit;

//TC64 Delphi class example / demo, v1.0
//(c)2017 Noniewicz.com
//created: 20171029

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, c64, StdCtrls, Buttons, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Label1: TLabel;
    BitBtn6: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnAbout: TBitBtn;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure BitBtnAboutClick(Sender: TObject);
  private
    c64: TC64;
  public
  end;

var
  Form1: TForm1;
  folder: string;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  c64 := TC64.Create;
  Image1.Picture.Bitmap.Width := 320;
  Image1.Picture.Bitmap.Height := 200;
  Image1.Picture.Bitmap.PixelFormat := pf24bit;
  folder := ExtractFilePath(Application.ExeName)+'..\c64-sampledata\';
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  c64.Free;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  c64.LoadKoalaToBitmap(folder+'PIC_GDC.koa', Image1.Picture.Bitmap);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  c64.LoadHiresToBitmap(folder+'ZX-FLORD.PIC', Image1.Picture.Bitmap);
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  c64.LoadAmicaToBitmap(folder+'[b]stormlord', Image1.Picture.Bitmap);
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  c64.LoadLogoToBitmap(folder+'MYLOGOV2.GFX', Image1.Picture.Bitmap, 1);
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image1.Picture.Bitmap.Canvas.FillRect(RECT(0, 0, 320, 200));
  c64.LoadFontToBitmap(folder+'IRON-PL.FNT', Image1.Picture.Bitmap);
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
begin
(*
  MOBload(path+'rozne.mob',mob1);
  MOBload(path+'swiss.mbf',mob2);
  MOBload(path+'tfight.mob',mob3);
  FNTBload(path+'leonardo.fnb',fntb);

  for n := 1 to 19 do
    FNTBshow(n*16-15,150,fntb,n,7);
  for n := 1 to 12 {mob1.cnt} do
    mMOBshow(n*25-20,30,mob1,n,1);
  for n := 1 to 12 {mob2.cnt} do
    mMOBshow(n*25-20,60,mob2,n,4);
  for n := 1 to 12 {mob3.cnt} do
    hMOBshow(n*25-20,90,mob3,n,15);

  n := 1; m := 1;
  repeat
    hCLS(160,120);
    hMOBshow(160,120,mob3,n,15);
    hCLS(201,120);
    mMOBshow(200,120,mob1,no[m],1);

    delay(20);
    inc(n);
    inc(m);
    if n>10 then n := 1;
    if m>8 then m := 1
  until keypressed;
*)

//
end;

procedure TForm1.BitBtnSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  try
    Image1.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
  except
    on E: Exception do showmessage('Error: '+E.Message);
  end;
end;

procedure TForm1.BitBtnAboutClick(Sender: TObject);
begin
  showmessage('Commodore C-64 images and GFX data viewer/converter Delphi class (TC64) DEMO.'#13#10+
              '(c)1994-2017 Noniewicz.com'#13#10+
              '*FREEWARE*'#13#10+
              'Reads AMICA PAINT, KOALA, HIRES, FONT and MOB (sprite) files.'#13#10+
              'Saves only BMP for now.'#13#10+
              #13#10+
              'Further development possible but not granted.'#13#10+
              #13#10+
              'Greetings to all old friends from demoscene!'#13#10
              );
end;

end.

