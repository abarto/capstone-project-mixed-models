{**
@abstract(TForm de presentacion de valores del modelo)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(September 13, 2004)
Este modulo contiene al TForm utilizado para presentar en forma de tabla los
valores del modelo.
}

unit UnitFormTabla;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
  UnitConfiguracionDatos;

type
  {** TForm para presentar los valores del modelo en forma de tabla.
      @abstract(TForm para presentar los valores del modelo.) }
  TFormTabla = class(TForm)
    StringGridTabla: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    FDatos: TConfiguracionDatos;
  public
    constructor Create(AOwner: TComponent; ADatos: TConfiguracionDatos);
  end;

(*
var
  FormTabla: TFormTabla;
*)

implementation

{$R *.dfm}

procedure TFormTabla.FormCreate(Sender: TObject);
var
  I, J: Integer;
begin
  Caption := 'Tabla: ' + FDatos.Etiqueta;

  StringGridTabla.ColCount := FDatos.Valores.CantidadColumnas + 1;
  StringGridTabla.RowCount := FDatos.Valores.CantidadFilas + 1;

  StringGridTabla.Cells [0, 0] := 'Obs./Col.';
  for I := 0 to FDatos.Columnas.Count - 1 do
  begin
    StringGridTabla.Rows [0].Strings [I + 1] := FDatos.Columnas [I].Texto;
  end;

  for I := 0 to FDatos.Valores.CantidadFilas - 1 do
  begin
    StringGridTabla.Cells [0, I + 1] := '#' + IntToStr(I);
    for J := 0 to FDatos.Valores.CantidadColumnas - 1 do
      StringGridTabla.Cells [J + 1, I + 1] := FDatos.Valores [I, J];
  end;
end; // TFormTabla.FormCreate

constructor TFormTabla.Create(AOwner: TComponent; ADatos: TConfiguracionDatos);
begin
  inherited Create(AOwner);
  FDatos := ADatos;
end; // TFormTabla.Create

end. // UnitFormTabla
