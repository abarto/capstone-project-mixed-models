unit UnitFormBitacoraDepuracion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls, ActnList, ImgList, ToolWin,
  UaMatriz, UaVector;

type
  TFormBitacoraDepuracion = class(TForm)
    ActionLimpiarMemo: TAction;
    ActionLimpiarTodo: TAction;
    ActionOcultar: TAction;
    ActionListBitacoraDepuracion: TActionList;
    ImageListBitacoraDepuracion: TImageList;
    MemoBitacoraDepuracion: TMemo;
    PageControlBitacoraDepuracion: TPageControl;
    TabSheetMemoBitacoraDepuracion: TTabSheet;
    ToolBarBitacoraDepuracion: TToolBar;
    ToolButtonLimpiarMemo: TToolButton;
    ToolButtonLimpiarTodo: TToolButton;
    ToolButtonOcultar: TToolButton;
    ToolButtonSeparador1: TToolButton;
    ToolButton1: TToolButton;
    CheckBoxBitacoraDepuracionAjustarLineas: TCheckBox;
    ActionAjustarLineas: TAction;

    procedure ActionLimpiarMemoExecute(Sender: TObject);
    procedure ActionLimpiarTodoExecute(Sender: TObject);
    procedure ActionOcultarExecute(Sender: TObject);
    procedure ActionOcultarUpdate(Sender: TObject);
    procedure ActionAjustarLineasExecute(Sender: TObject);
    procedure ActionAjustarLineasUpdate(Sender: TObject);
  public
    procedure ImprimirString(const AString: String);
    procedure VerMatriz(const AString: String; const Matriz: TUaMatriz; const ComoTexto: Boolean = false);
    procedure VerVector(const AString: String; const Vector: TUaVector; const ComoTexto: Boolean = false);
  end;

var
  FormBitacoraDepuracion: TFormBitacoraDepuracion;

implementation

{$R *.dfm}

procedure TFormBitacoraDepuracion.ActionLimpiarMemoExecute(Sender: TObject);
begin
  MemoBitacoraDepuracion.Clear;
end;

procedure TFormBitacoraDepuracion.ActionLimpiarTodoExecute(Sender: TObject);
var
  I, J: Integer; // Indices
  T: TTabSheet;  // TabSheet auxiliar
begin
  // Limpiar el contenido del Memo
  MemoBitacoraDepuracion.Clear;

  // Ocultar el PageControl
  PageControlBitacoraDepuracion.Hide;

  // Eliminar cada TabSheet adicional del PageControl
  J := PageControlBitacoraDepuracion.PageCount;
  I := 1;
  while (I < J) do
  begin
    T := PageControlBitacoraDepuracion.FindNextPage(TabSheetMemoBitacoraDepuracion, true, false);
    T.Parent := nil;
    FreeAndNil(T);
    Inc(I);
  end;

  // Mostrar el PageControl
  PageControlBitacoraDepuracion.Show;
end;

procedure TFormBitacoraDepuracion.ActionOcultarExecute(Sender: TObject);
begin
  Hide;
end;

procedure TFormBitacoraDepuracion.ActionOcultarUpdate(Sender: TObject);
begin
  ActionOcultar.Checked := Showing;
end;

procedure TFormBitacoraDepuracion.ImprimirString(const AString: String);
begin
  // Imprimir el string y mostrar el TabSheet correspondiente
  MemoBitacoraDepuracion.Lines.Add(AString);
  PageControlBitacoraDepuracion.ActivePage := TabSheetMemoBitacoraDepuracion;
end;

procedure TFormBitacoraDepuracion.VerMatriz(const AString: String; const Matriz: TUaMatriz; const ComoTexto: Boolean);
var
  I, J: Integer;
  TabSheet: TTabSheet;
  StringGrid: TStringGrid;
begin
  if ComoTexto then
  begin
    MemoBitacoraDepuracion.Lines.Add(AString + ': ' + Matriz.Texto);
    PageControlBitacoraDepuracion.ActivePage := TabSheetMemoBitacoraDepuracion;
  end
  else
  begin
    // Construir e inicializar el TabSheet
    TabSheet := TTabSheet.Create(PageControlBitacoraDepuracion);
    TabSheet.PageControl := PageControlBitacoraDepuracion;
    TabSheet.Caption := 'StringGrid: ' + AString;

    // Contruir e inicializar el StringGrid
    StringGrid := TStringGrid.Create(TabSheet);
    StringGrid.Parent := TabSheet;
    StringGrid.Align := alClient;

    // Mostrar el contenido de la matriz
    StringGrid.ColCount := Matriz.CantidadColumnas + 1;
    StringGrid.RowCount := Matriz.CantidadFilas + 1;

    for I := 1 to StringGrid.ColCount do
      StringGrid.Cells [I, 0] := IntToStr(I);

    for I := 1 to StringGrid.RowCount do
      StringGrid.Cells [0, I] := IntToStr(I);

    for I := 1 to Matriz.CantidadFilas do
      for J := 1 to Matriz.CantidadColumnas do
        StringGrid.Cells [J, I] := FloatToStr(Matriz [I, J]);
  end;
end;

procedure TFormBitacoraDepuracion.VerVector(const AString: String; const Vector: TUaVector; const ComoTexto: Boolean);
var
  I: Integer;
  TabSheet: TTabSheet;
  StringGrid: TStringGrid;
begin
  if ComoTexto then
  begin
    MemoBitacoraDepuracion.Lines.Add(AString + ': ' + Vector.Texto);
    PageControlBitacoraDepuracion.ActivePage := TabSheetMemoBitacoraDepuracion;
  end
  else
  begin
    // Construir e inicializar el TabSheet
    TabSheet := TTabSheet.Create(PageControlBitacoraDepuracion);
    TabSheet.PageControl := PageControlBitacoraDepuracion;
    TabSheet.Caption := 'StringGrid: ' + AString;

    // Contruir e inicializar el StringGrid
    StringGrid := TStringGrid.Create(TabSheet);
    StringGrid.Parent := TabSheet;
    StringGrid.Align := alClient;

    // Mostrar el contenido de la matriz
    StringGrid.ColCount := 2;
    StringGrid.RowCount := Vector.Dimension + 1;

    StringGrid.Cells [0, 0] := 'Elemento';
    StringGrid.Cells [1, 0] := 'Valor';

    for I := 1 to Vector.Dimension do
      StringGrid.Cells [1, I] := FloatToStr(Vector [I]);
  end;
end;

procedure TFormBitacoraDepuracion.ActionAjustarLineasExecute(
  Sender: TObject);
begin
  MemoBitacoraDepuracion.WordWrap := not MemoBitacoraDepuracion.WordWrap;
end;

procedure TFormBitacoraDepuracion.ActionAjustarLineasUpdate(
  Sender: TObject);
begin
  ActionAjustarLineas.Checked := MemoBitacoraDepuracion.WordWrap;
end;

end.


