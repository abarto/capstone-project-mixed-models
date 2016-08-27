{**
@abstract(Configuracion de la fuente de datos del caso.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(September 11, 2004)
Este modulo contiene la clase que almacena la configuracion de la fuente de
datos del caso. Esto incluye la matriz de los valores con las observaciones que
seran utilizadas para ajustar el modelo.
}

unit UnitConfiguracionDatos;

interface

uses
  Classes, SysUtils,
  UnitColumnaDatos, UnitMatrizValores;

type
  {** Configuracion de la fuente de datos. Incluye la matriz con los valores
      de las observaciones del caso.
      @abstract(Configuracion de la fuente de datos.) }
  TConfiguracionDatos = class
  private
    {** Lista de descriptores de columna de datos. }
    FColumnas: TListaColumnasDatos;
    {** Etiqueta descriptiva de los datos. }
    FEtiqueta: String;
    {** Matriz de valores con las observaciones del modelo. }
    FValores: TMatrizValores;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AEtiqueta Descripcion de la fuente de datos.)
        @param(AColumnas Lista de descriptores de columnas de datos.)
        @param(AValores Matriz con las observaciones del modelo.) }
    constructor Create(AEtiqueta: String; AColumnas: TListaColumnasDatos; AValores: TMatrizValores);  overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Valida el contenido de la configuracion de la fuente de datos.
        @param(BitacoraValidacion Registro de posibles errores y advertencias
        producidas durante el proceso de validacion.)
        @returns(Devuelve @true si los datos son validos, o @false en caso
        contrario.) }
    function Validar(var BitacoraValidacion: TStrings): Boolean;
    {** Etiqueta descriptiva de los datos. }
    property Etiqueta: String read FEtiqueta write FEtiqueta;
    {** Descriptores de las columnas de datos. }
    property Columnas: TListaColumnasDatos read FColumnas write FColumnas;
    {** Matriz de valores con las observaciones. }
    property Valores: TMatrizValores read FValores write FValores;
  end;

implementation

// -----------------------------------------------------------------------------
// TConfiguracionDatos
// -----------------------------------------------------------------------------

constructor TConfiguracionDatos.Create;
begin { TConfiguracionDatos.Create }
  Create('', TListaColumnasDatos.Create, TMatrizValores.Create);
end { TConfiguracionDatos.Create };

constructor TConfiguracionDatos.Create(AEtiqueta: String; AColumnas: TListaColumnasDatos; AValores: TMatrizValores);
begin { TConfiguracionDatos.Create }
  Assert(Assigned(AColumnas), 'TConfiguracionDatos.Create: Assigned(AColumnas)');
  Assert(Assigned(AValores), 'TConfiguracionDatos.Create: Assigned(AValores)');

  FEtiqueta := AEtiqueta;
  FColumnas := AColumnas;
  FValores := AValores;
end { TConfiguracionDatos.Create };

destructor TConfiguracionDatos.Destroy;
begin { TConfiguracionDatos.Destroy }
  try
    FColumnas.Clear;
  finally
    FreeAndNil(FColumnas);
    FreeAndNil(FValores);
    inherited Destroy;
  end;
end { TConfiguracionDatos.Destroy };

function TConfiguracionDatos.Validar(var BitacoraValidacion: TStrings): Boolean;
var
  B, Listo: Boolean;
  I, J: Integer;
begin { TConfiguracionDatos.Validar }
  Assert(Assigned(FColumnas), 'TConfiguracionDatos.Validar: Assigned(FColumnas)');
  Assert(Assigned(FValores), 'TConfiguracionDatos.Validar: Assigned(FValores)');
  Assert(Assigned(BitacoraValidacion), 'TConfiguracionDatos.Validar: Assigned(BitacoraValidacion)');

  B := true;

  // Columnas

  // Lista de definiciones de columnas vacia
  if FColumnas.Count > 0 then
  begin
    // Definicion de columna duplicada o inconsistente
    I := 0;
    Listo := false;
    while (I < FColumnas.Count - 1) and (not Listo) do
    begin
      J := I + 1;
      while (J < FColumnas.Count) and (not Listo) do
      begin
        if FColumnas [I].Nombre = FColumnas [J].Nombre then
        begin
          BitacoraValidacion.Add('Definici�n de columna duplicada o inconsistente: #' + IntToStr(I) + ' = ' + FColumnas [I].Texto + ', #' + IntToStr(J) + ' = ' + FColumnas [J].Texto + '.');
          B := false;
          Listo := true;
        end;

        Inc(J);
      end;

      Inc(I);
    end;
  end
  else
  begin
    BitacoraValidacion.Add('Lista de definiciones de columnas vac�a.');
    B := false;
  end;

  // Valores

  // Matriz de valores vacia
  if FValores.CantidadFilas = 0 then
  begin
    BitacoraValidacion.Add('Matriz de valores vac�a.');
    B := false;
  end;

  Result := B;
end { TConfiguracionDatos.Validar };

end { UnitConfiguracionDatos }. 
