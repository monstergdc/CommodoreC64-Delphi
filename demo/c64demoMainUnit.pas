unit c64demoMainUnit;

//TC64 Delphi/Lazarus class example / demo Delphi 7, v1.0
//(c)2017 Noniewicz.com
//created: 20171029
//updated: 20171101, 05, 11, 12, 13, 15, 18, 20, 25

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  c64, jpeg;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    BitBtnSave: TBitBtn;
    BitBtnAbout: TBitBtn;
    SaveDialog1: TSaveDialog;
    BitBtnLoad: TBitBtn;
    OpenDialog1: TOpenDialog;
    GBQuick: TGroupBox;
    BitBtnKOALA: TBitBtn;
    BitBtnHIRES: TBitBtn;
    BitBtnAMICA: TBitBtn;
    BitBtnLOGO: TBitBtn;
    BitBtnFONT: TBitBtn;
    BitBtnFNTB: TBitBtn;
    BitBtnMOBM: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtnBFLI: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtnFLI: TBitBtn;
    BitBtnAFLI: TBitBtn;
    BitBtnHIEDDI: TBitBtn;
    BitBtnDDL: TBitBtn;
    RGPal: TRadioGroup;
    GroupBox2: TGroupBox;
    ComboBoxC0: TComboBox;
    Label2: TLabel;
    ComboBoxC1: TComboBox;
    Label3: TLabel;
    ComboBoxC2: TComboBox;
    Label4: TLabel;
    ComboBoxC3: TComboBox;
    Label5: TLabel;
    BitBtnMOBH: TBitBtn;
    cbHires: TCheckBox;
    ImagePal: TImage;
    BitBtnADVART: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtnKOALAClick(Sender: TObject);
    procedure BitBtnHIRESClick(Sender: TObject);
    procedure BitBtnAMICAClick(Sender: TObject);
    procedure BitBtnLOGOClick(Sender: TObject);
    procedure BitBtnFONTClick(Sender: TObject);
    procedure BitBtnFNTBClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure BitBtnAboutClick(Sender: TObject);
    procedure BitBtnLoadClick(Sender: TObject);
    procedure BitBtnMOBMClick(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtnBFLIClick(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtnFLIClick(Sender: TObject);
    procedure BitBtnAFLIClick(Sender: TObject);
    procedure BitBtnHIEDDIClick(Sender: TObject);
    procedure BitBtnDDLClick(Sender: TObject);
    procedure RGPalClick(Sender: TObject);
    procedure BitBtnMOBHClick(Sender: TObject);
    procedure BitBtnADVARTClick(Sender: TObject);
  private
    c64: TC64;
    procedure ClearImage(bfli: boolean = false);
    procedure Set320x200;
    procedure Set320x400;
  public
  end;

var
  Form1: TForm1;
  folder: string;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
  c64 := TC64.Create;
  ClearImage(false);
  folder := ExtractFilePath(Application.ExeName)+'..\c64-sampledata\';
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog1.InitialDir := ExtractFilePath(folder);

  ImagePal.Picture.Bitmap.Width := ImagePal.Width;
  ImagePal.Picture.Bitmap.Height := ImagePal.Height;
  RGPal.ItemIndex := 0;

  ComboBoxC0.Items.Clear;
  for i := 0 to 15 do
    ComboBoxC0.Items.Add(inttostr(i));
  ComboBoxC1.Items.Text := ComboBoxC0.Items.Text;
  ComboBoxC2.Items.Text := ComboBoxC0.Items.Text;
  ComboBoxC3.Items.Text := ComboBoxC0.Items.Text;
  ComboBoxC0.ItemIndex := 0;
  ComboBoxC1.ItemIndex := 0;
  ComboBoxC2.ItemIndex := 0;
  ComboBoxC3.ItemIndex := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  c64.Free;
end;

procedure TForm1.ClearImage(bfli: boolean = false);
begin
  if bfli then Set320x400 else Set320x200;
  Image1.Picture.Bitmap.PixelFormat := pf24bit;
  Image1.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Image1.Picture.Bitmap.Canvas.FillRect(RECT(0, 0, Image1.Picture.Bitmap.Width, Image1.Picture.Bitmap.Height));
end;

procedure TForm1.Set320x200;
begin
  Image1.Width := 320;
  Image1.Height := 200;
  Image1.Picture.Bitmap.Width := 320;
  Image1.Picture.Bitmap.Height := 200;
end;

procedure TForm1.Set320x400;
begin
  Image1.Width := 320;
  Image1.Height := 400;
  Image1.Picture.Bitmap.Width := 320;
  Image1.Picture.Bitmap.Height := 400;
end;

procedure TForm1.BitBtnKOALAClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadMulticolorToBitmap(folder+'PIC_GDC.koa', Image1.Picture.Bitmap.Canvas, C64_KOALA);
end;

procedure TForm1.BitBtnHIRESClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadHiresToBitmap(folder+'ZX-FLORD.PIC', Image1.Picture.Bitmap.Canvas, C64_HIRES);
end;

procedure TForm1.BitBtnAMICAClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadAmicaToBitmap(folder+'[b]stormlord.[b]', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtnLOGOClick(Sender: TObject);
begin
  ClearImage;
  ComboBoxC0.ItemIndex := 3;
  ComboBoxC1.ItemIndex := 1;
  ComboBoxC2.ItemIndex := 6;
  ComboBoxC3.ItemIndex := 0;
  c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
  c64.AsHires := false;
  c64.LoadLogoToBitmap(folder+'MYLOGOV2.GFX', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtnFONTClick(Sender: TObject);
begin
  ClearImage;
  ComboBoxC0.ItemIndex := 0;
  ComboBoxC1.ItemIndex := 1;
  ComboBoxC2.ItemIndex := 0;
  ComboBoxC3.ItemIndex := 0;
  c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
  c64.AsHires := true;
  c64.LoadFontToBitmap(folder+'IRON-PL.FNT', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtnFNTBClick(Sender: TObject);
begin
  ClearImage;
  ComboBoxC0.ItemIndex := 0;
  ComboBoxC1.ItemIndex := 1;
  ComboBoxC2.ItemIndex := 15;
  ComboBoxC3.ItemIndex := 2;
  c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
  c64.AsHires := false;
  c64.LoadFont2x2ToBitmap(folder+'LEONARDO.FNB', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtnMOBMClick(Sender: TObject);
begin
  ClearImage;
  ComboBoxC0.ItemIndex := 0;
  ComboBoxC1.ItemIndex := 6;
  ComboBoxC2.ItemIndex := 14;
  ComboBoxC3.ItemIndex := 1;
  c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
  c64.AsHires := false;
  c64.LoadMobToBitmap(folder+'ROZNE.MOB', Image1.Picture.Bitmap.Canvas);
end;

procedure TForm1.BitBtn8Click(Sender: TObject);
begin
  ClearImage;
  ComboBoxC0.ItemIndex := 0;
  ComboBoxC1.ItemIndex := 6;
  ComboBoxC2.ItemIndex := 14;
  ComboBoxC3.ItemIndex := 1;
  c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
  c64.AsHires := false;
  c64.LoadMobToBitmap(folder+'SWISS.MBF', Image1.Picture.Bitmap.Canvas);
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
  showmessage('Commodore C-64 images and GFX data viewer/converter Delphi/Lazarus class (TC64) DEMO.'#13#10+
              'Component version: '+c64.Version+#13#10+
              '(c)1994-2017 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC'#13#10+
              '*FREEWARE*'#13#10#13#10+
              'Reads various C-64 graphics files.'#13#10+
              'Saves only BMP/JPG for now.'#13#10+
              #13#10+
              'Further development possible but not granted.'#13#10+
              #13#10+
              'Greetings to all old friends from demoscene!'#13#10
              );
end;


procedure TForm1.BitBtnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ClearImage(c64.ExtMapper(ExtractFileExt(OpenDialog1.FileName)) = C64_BFLI);
    c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
    c64.AsHires := cbHires.Checked;
    if c64.LoadC64ToBitmap(OpenDialog1.FileName, Image1.Picture.Bitmap.Canvas) <> 0 then
      showmessage('ERROR: '+c64.LastError);
  end;
end;

procedure TForm1.BitBtn9Click(Sender: TObject);
begin
  ClearImage;
  c64.LoadFliToBitmap(folder+'intel.ifli', Image1.Picture.Bitmap.Canvas, C64_IFLI);
  showmessage('DEBUG: '+c64.LastError);
end;

procedure TForm1.BitBtnBFLIClick(Sender: TObject);
begin
  ClearImage(true);
  c64.LoadFliToBitmap(folder+'kira.bfli', Image1.Picture.Bitmap.Canvas, C64_BFLI);
end;

procedure TForm1.BitBtn11Click(Sender: TObject);
begin
  ClearImage;
  c64.LoadFliToBitmap(folder+'parrot_3aff.ffli', Image1.Picture.Bitmap.Canvas, C64_FFLI);
end;

procedure TForm1.BitBtnFLIClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadFliToBitmap(folder+'cpu.fli', Image1.Picture.Bitmap.Canvas, C64_FLI);
end;

procedure TForm1.BitBtnAFLIClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadFliToBitmap(folder+'logo.afl', Image1.Picture.Bitmap.Canvas, C64_AFLI);
end;

procedure TForm1.BitBtnHIEDDIClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadHiresToBitmap(folder+'Hii-Eddi.hed', Image1.Picture.Bitmap.Canvas, C64_HED);
end;

procedure TForm1.BitBtnDDLClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadHiresToBitmap(folder+'midear.dd', Image1.Picture.Bitmap.Canvas, C64_DDL);
end;

procedure TForm1.BitBtnADVARTClick(Sender: TObject);
begin
  ClearImage;
  c64.LoadMulticolorToBitmap(folder+'demo-meyes.mpic', Image1.Picture.Bitmap.Canvas, C64_ADVARTST);
end;

procedure TForm1.RGPalClick(Sender: TObject);
begin
  case RGPal.ItemIndex of
    0: c64.Palette := C64S_PAL;
    1: c64.Palette := CCS64_PAL;
    2: c64.Palette := FRODO_PAL;
    3: c64.Palette := GODOT_PAL;
    4: c64.Palette := PC64_PAL;
    5: c64.Palette := VICE_PAL;
    6: c64.Palette := C64HQ_PAL;
    7: c64.Palette := OLDVICE_PAL;
    8: c64.Palette := VICEDFLT_PAL;
  end;

  c64.PaintPallete(ImagePal.Picture.Bitmap.Canvas, 0, 0, 16, ImagePal.Picture.Bitmap.Height);
end;

procedure TForm1.BitBtnMOBHClick(Sender: TObject);
begin
  ClearImage;
  ComboBoxC0.ItemIndex := 0;
  ComboBoxC1.ItemIndex := 1;
  ComboBoxC2.ItemIndex := 0;
  ComboBoxC3.ItemIndex := 0;
  c64.Set4Colors(ComboBoxC0.ItemIndex, ComboBoxC1.ItemIndex, ComboBoxC2.ItemIndex, ComboBoxC3.ItemIndex);
  c64.AsHires := true;
  c64.LoadMobToBitmap(folder+'TFIGHT.MOB', Image1.Picture.Bitmap.Canvas);
end;


end.

