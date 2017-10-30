unit c64demoMainUnit;

//TC64 Delphi class example / demo, v1.0
//(c)2017 Noniewicz.com
//created: 20171029

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  c64, jpeg;

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
    BitBtn7: TBitBtn;
    BitBtnLoad: TBitBtn;
    OpenDialog1: TOpenDialog;
    BitBtn8: TBitBtn;
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
    procedure BitBtnLoadClick(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
  private
    c64: TC64;
    procedure ClearImage;
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
  OpenDialog1.InitialDir := ExtractFilePath(folder);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  c64.Free;
end;

procedure TForm1.ClearImage;
begin
  Image1.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image1.Picture.Bitmap.Canvas.FillRect(RECT(0, 0, 320, 200));
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  c64.LoadKoalaToBitmap(folder+'PIC_GDC.koa', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  c64.LoadHiresToBitmap(folder+'ZX-FLORD.PIC', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  c64.LoadAmicaToBitmap(folder+'[b]stormlord.[b]', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  c64.LoadLogoToBitmap(folder+'MYLOGOV2.GFX', Image1.Picture.Bitmap.Canvas, 1);
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
  ClearImage;
  c64.LoadFontToBitmap(folder+'IRON-PL.FNT', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
begin
  ClearImage;
  c64.LoadFont2x2ToBitmap(folder+'LEONARDO.FNB', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtn7Click(Sender: TObject);
begin
  ClearImage;
  c64.LoadMobToBitmap(folder+'ROZNE.MOB', Image1.Picture.Bitmap.Canvas, false);
end;

procedure TForm1.BitBtn8Click(Sender: TObject);
begin
  ClearImage;
  c64.LoadMobToBitmap(folder+'SWISS.MBF', Image1.Picture.Bitmap.Canvas, false);
end;

procedure TForm1.BitBtnSaveClick(Sender: TObject);
var jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;

  SaveDialog1.Filter := 'BMP (*.bmp)|*.bmp|JPG (*.jpg)|*.jpg';
  if SaveDialog1.Execute then
  try
    if SaveDialog1.FilterIndex = 2 then //jpg
    begin
      jpg.Assign(Image1.Picture.Bitmap);
      jpg.CompressionQuality := 100;
      SaveDialog1.FileName := ChangeFileExt(SaveDialog1.FileName, '.jpg');
      jpg.SaveToFile(SaveDialog1.FileName);
    end
    else
    begin
      SaveDialog1.FileName := ChangeFileExt(SaveDialog1.FileName, '.bmp');
      Image1.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
    end;
    MessageDlg('Image saved to '+SaveDialog1.FileName, mtInformation, [mbOK], 0);
  except
    on E: Exception do
      MessageDlg('Error saving image: '+E.Message, mtError, [mbOK], 0);
  end;

  jpg.Free;  
end;

procedure TForm1.BitBtnAboutClick(Sender: TObject);
begin
  showmessage('Commodore C-64 images and GFX data viewer/converter Delphi class (TC64) DEMO.'#13#10+
              '(c)1994-2017 Noniewicz.com'#13#10+
              '*FREEWARE*'#13#10+
              'Reads AMICA PAINT, KOALA, HIRES, FONT and MOB (sprite) files.'#13#10+
              'Saves only BMP/JPG for now.'#13#10+
              #13#10+
              'Further development possible but not granted.'#13#10+
              #13#10+
              'Greetings to all old friends from demoscene!'#13#10
              );
end;

procedure TForm1.BitBtnLoadClick(Sender: TObject);
var fn, ext: string;
begin
  if OpenDialog1.Execute then
  begin
    ClearImage;
    fn := OpenDialog1.FileName;
    ext := uppercase(ExtractFileExt(fn));
    if ext = '.KOA' then
      c64.LoadKoalaToBitmap(fn, Image1.Picture.Bitmap.Canvas);
    if ext = '.PIC' then
      c64.LoadHiresToBitmap(fn, Image1.Picture.Bitmap.Canvas);
    if ext = '.[B]' then
      c64.LoadAmicaToBitmap(fn, Image1.Picture.Bitmap.Canvas);
    if ext = '.GFX' then
      c64.LoadLogoToBitmap(fn, Image1.Picture.Bitmap.Canvas, 1);
    if ext = '.FNT' then
      c64.LoadFontToBitmap(fn, Image1.Picture.Bitmap.Canvas);
    if ext = '.FNB' then
      c64.LoadFont2x2ToBitmap(fn, Image1.Picture.Bitmap.Canvas);
    if ext = '.MOB' then
      c64.LoadMobToBitmap(fn, Image1.Picture.Bitmap.Canvas, false);
    if ext = '.MBF' then
      c64.LoadMobToBitmap(fn, Image1.Picture.Bitmap.Canvas, true);
  end;
end;

end.

