{**
@abstract(Estructuras de varianza/covarianza.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de estructuras de varianza y covarianza.
}

unit UnitEstructura;

interface

uses
  Classes;

type
  {** Definicion de estructura generica, sin orden.
      @abstract(Definicion de estructura.) }
  TEstructura = class
  protected
    {** Nombre de la estructura. }
    FNombre: String;
    {** Metodo de lectura de la propiedad @link(Texto). Devuelve la
        representacion de cadena de caracteres de la estructura.
        @returns(Representacion de cadena de caracteres de la estructura.) }
    function GetTexto: String; virtual;
  public
    {** Constructor.
        @param(ANombre Nombre de la estructura.) }
    constructor Create(ANombre: String);
    {** Valida la estructura.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(@true si la estructura es valida, o @false en caso
        contrario.) }
    function Validar(var Bitacora: TStrings): Boolean; virtual;
    {** Nombre de la estructura. }
    property Nombre: String read FNombre write FNombre;
    {** Representacion de cadenadde caracteres de la estructura. }
    property Texto: String read GetTexto;
  end { TEstructura };

  {** Definicion de estructura con parametro de orden.
      @abstract(Definicion de estructura con parametro de orden.) }
  TEstructuraOrden = class(TEstructura)
  protected
    {** Orden de la estructura. }
    FOrden: Integer;
    {** Ver @link(TEstructura.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la estructura.)
        @param(AOrden Parametro de orden.) }
    constructor Create(ANombre: String; AOrden: Integer);
    {** Ver @link(TEstructura.Validar). }
    function Validar(var Bitacora: TStrings): Boolean; override;
    {** Orden de la estructura. }
    property Orden: Integer read FOrden write FOrden;
  end { TEstructuraOrden };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TEstructura
// -----------------------------------------------------------------------------

function TEstructura.GetTexto: String;
begin { TEstructura.GetTexto }
  Result := FNombre;
end { TEstructura.GetTexto };

constructor TEstructura.Create(ANombre: String);
begin { TEstructura.Create }
  Assert(ANombre <> '', 'TEstructura.Create: ''''');

  FNombre := ANombre;
end { TEstructura.Create };

function TEstructura.Validar(var Bitacora: TStrings): Boolean;
begin { TEstructura.Validar }
  Assert(Assigned(Bitacora), 'TEstructura.Validar: Assigned(Bitacora)');
  Result := true;
end { TEstructura.Validar };

// -----------------------------------------------------------------------------
// TEstructuraOrden
// -----------------------------------------------------------------------------

function TEstructuraOrden.GetTexto: String;
begin { TEstructuraOrden.GetTexto }
  Result := FNombre + '[' + IntToStr(FOrden) + ']';
end { TEstructuraOrden.GetTexto };

constructor TEstructuraOrden.Create(ANombre: String; AOrden: Integer);
begin { TEstructuraOrden.Create }
  inherited Create(ANombre);
  FOrden := AOrden;
end { TEstructuraOrden.Create };

function TEstructuraOrden.Validar(var Bitacora: TStrings): Boolean;
begin { TEstructuraOrden.Validar }
  Assert(Assigned(Bitacora), 'TEstructuraOrden.Validar: Assigned(Bitacora)');

  Result := true;

  if FOrden <= 0 then
  begin
    Bitacora.Add('Numero de orden de estructura invalido: ' + IntToStr(FOrden));
    Result := false;
  end;
end { TEstructuraOrden.Validar };

end { UnitEstructura }.  
