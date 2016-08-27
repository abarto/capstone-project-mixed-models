{**
@abstract(Arreglo dinamico de cadenas de caracteres y estructuras derivadas.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de arreglos dinamicos de cadenas de
caracteres y estructuras de datos mas complejas construidas a partir de estos.
La principal utilidad de estas estructuras es almacenar niveles de variables.
}

unit UnitArregloStrings;

interface

uses
  Classes;

type
  {** Arreglo dinamico de cadenas de caracteres. Es una clase envoltura de un
      array of String. Se utiliza una clase envolvente para facilitar la
      manipulacion de los arreglos dinamicos y proveer de una interfaz
      consistente.
      @abstract(Arreglo dinamico de cadenas de caracteres.) }
  TArregloStrings = class
  private
    {** Arreglo dinamico de cadenas de caracteres propiamente dicho. }
    FStrings: array of String;

    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico.
        @returns(El mayor indice del arreglo dinamico.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico.
        @returns(El menor indice del arreglo dinamico.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension de arreglo dinamico.
        @returns(Dimension (o longitud) del arreglo dinamico.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(Strings). Devuelve la cadena de
        caracteres almacenada bajo un indice.
        @param(I Indice de la cadena a accerder.)
        @returns(La cadena almacenada bajo el indice.) }
    function GetString(I: Integer): String;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo.
        @returns(Representacion textual del arreglo.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FStrings).
        @param(ADimension Nueva dimension del arreglo dinamico.) }
    procedure SetDimension(ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Strings). Actualiza el valor
        de la cadena de caracteres almacenada bajo un indice.
        @param(I Indice de la cadena a actualizar.)
        @param(AString Nuevo valor para de la cadena de caracteres bajo el
        indice.) }
    procedure SetString(I: Integer; AString: String);
  public
    {** Constructor. Esta version del constructor genera una copia del contenido
        del parametro.
        @param(Arreglo Arreglo a copiar.) }
    constructor Create(Arreglo: TArregloStrings); overload;
    {** Constructor. Esta version del constructor genera una arreglo de strings
        de la dimension inicial indicada.
        @param(ADimension Dimension inicial del arreglo.) }
    constructor Create(ADimension: Integer); overload;
    {** Constructor. Esta version del constructor crea la instancia a partir de
        una lista de cadenas de caracteres.
        @param(Strings Lista de cadena de caracteres de la cual extraer las
        entradas al arreglo.) }
    constructor Create(Strings: TStrings); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Genera una copia por valor del arreglo.
        @returns(Copia por valor del arreglo.) }
    function Clonar: TArregloStrings;
    {** Indica si arreglo y el parametro coinciden en dimension y contenido.
        @param(Arreglo Arreglo a comparar.)
        @returns(@true en caso que los arreglos coincidan en dimension y
        contenido, @false en caso contrario.) }
    function IgualA(Arreglo: TArregloStrings): Boolean;
    {** Busca una cadena de caracteres dentro del arreglo comparando por
        dimension y contenido.)
        @param(AString Cadena de caracteres a buscar.)
        @returns(El indice de la primera cadena identica al parametro en caso de
        encontrar una, o -1 en caso contrario.).}
    function IndiceDe(AString: String): Integer;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension (longitud) del arreglo. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Cadenas de caracteres almacenadas. }
    property Strings [I: Integer]: String read GetString write SetString; default;
    {** Representacion textual del arreglo. }
    property Texto: String read GetTexto;
  end { TArregloStrings };

  {** Arreglo dinamico de @link(TArregloStrings). Es una clase envoltura de
      un array of TArregloStrings Funciona como una matriz, pero en cada fila
      es posible tener una cantidad diferente de columnas. Se utiliza una clase
      envolvente para facilitar la manipulacion de los arreglos y proveer de una
      interfaz consistente.
      @abstract(Arreglo dinamico de @link(TArregloStrings).) }
  TArregloArreglosStrings = class
  private
    { Arreglo dinamico de arreglos de cadenas de caracteres. }
    FArreglosStrings: array of TArregloStrings;
    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico.
        @returns(El mayor indice del arreglo dinamico.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Arreglos). Devuelve el arreglo
        asociado a un indice.
        @param(I Indice del arreglo que deseamos acceder.)
        @returns(Arreglo bajo el indice.) }
    function GetArreglo(I: Integer): TArregloStrings;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico.
        @returns(El menor indice del arreglo dinamico.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension de arreglo dinamico.
        @returns(Dimension (o longitud) del arreglo dinamico.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(Strings). Devuelve una cadena de
        caracteres dentro de los arreglos de cadenas almacenados.
        @param(I Indice del arreglo de cadenas de caracteres.)
        @param(J Indice de la cadena de caracteres.)
        @returns(Cadena de caracteres bajo el indice I en el arreglo bajo el
        indice J.) }
    function GetString(I, J: Integer): String;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo de arreglos de
        String.
        @returns(Representacion textual del arreglo.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Arreglos). Actualiza el valor
        de un arreglo almacenado.
        @param(I Indice del arreglo a actualizar.)
        @param(Arreglo Nuevo valor para el arreglo.) }
    procedure SetArreglo(I: Integer; Arreglo: TArregloStrings);
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FArreglosStrings).
        @param(ADimension Nueva dimension del arreglo.) }
    procedure SetDimension(ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Strings). Actualiza el valor
        de una cadena de caracteres en alguno de los arreglos almacenados.
        @param(I Indice del arreglo que contiene la cadena de caracteres.)
        @param(J Indice de la cadena de caracteres dentro del arreglo.)
        @param(AString Cadena de caracteres a almacenar.) }
    procedure SetString(I, J: Integer; AString: String);
  public
    {** Esta version del constructor dimensiona el arreglo dinamico e inicializa
        cada instancia contenida a @nil.
        @param(ADimension Dimension inicial del arreglo dinamico.) }
    constructor Create(ADimension: Integer);
    {** Destructor. }
    destructor Destroy; override;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Arreglos de cadenas de caracteres almacenados. }
    property Arreglos [I: Integer]: TArregloStrings read GetArreglo write SetArreglo;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension del arreglo dinamico. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Cadenas de caracteres almacenadas. }
    property Strings [I, J: Integer]: String read GetString write SetString; default;
    {** Representacion textual del arreglo. }
    property Texto: String read GetTexto;
  end { TArregloArreglosStrings };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TArregloStrings
// -----------------------------------------------------------------------------

function TArregloStrings.GetAlto: Integer;
begin {  TArregloStrings.GetAlto }
  Result := High(FStrings);
end {  TArregloStrings.GetAlto };

function TArregloStrings.GetBajo: Integer;
begin { TArregloStrings.GetBajo }
  Result := Low(FStrings);
end { TArregloStrings.GetBajo };

function TArregloStrings.GetDimension: Integer;
begin { TArregloStrings.GetDimension }
  Result := Length(FStrings);
end { TArregloStrings.GetDimension };

function TArregloStrings.GetString(I: Integer): String;
begin { TArregloStrings.GetString }
  Assert((I >= Low(FStrings)) and (I <= High(FStrings)), 'TArregloStrings.GetString: (I >= Low(FStrings)) and (I <= High(FStrings))');

  Result := FStrings [I];
end { TArregloStrings.GetString };

function TArregloStrings.GetTexto: String;
var
  I: Integer;
begin { TArregloStrings.GetTexto }
  Result := '[';
  if Length(FStrings) > 0 then
  begin
    for I := Low(FStrings) to High(FStrings) - 1 do
      Result := Result + FStrings [I] + ', ';

    Result := Result + FStrings [High(FStrings)];
  end;
  Result := Result + ']';
end { TArregloStrings.GetTexto };

procedure TArregloStrings.SetDimension(ADimension: Integer);
begin { TArregloStrings.SetDimension }
  Assert(ADimension > 0, 'TArregloStrings.SetDimension: ADimension > 0');

  SetLength(FStrings, ADimension);
end { TArregloStrings.SetDimension };

procedure TArregloStrings.SetString(I: Integer; AString: String);
begin { TArregloStrings.SetString }
  Assert((I >= Low(FStrings)) and (I <= High(FStrings)), 'TArregloStrings.SetString: (I >= Low(FStrings)) and (I <= High(FStrings))');

  FStrings [I] := AString;
end { TArregloStrings.SetString };

constructor TArregloStrings.Create(ADimension: Integer);
begin { TArregloStrings.Create }
  // Assert(ADimension > 0, 'TArregloStrings.Create: ADimension > 0');

  SetLength(FStrings, ADimension);
end { TArregloStrings.Create };

constructor TArregloStrings.Create(Arreglo: TArregloStrings);
begin { TArregloStrings.Create }
  Assert(Assigned(Arreglo), 'TArregloStrings.Create: Assigned(Arreglo)');

  FStrings := Copy(Arreglo.FStrings, 0, Length(Arreglo.FStrings));
end { TArregloStrings.Create };

constructor TArregloStrings.Create(Strings: TStrings);
var
  I: Integer;
begin { TArregloStrings.Create }
  Assert(Assigned(Strings), 'TArregloStrings.Create: Assigned(Strings)');

  SetLength(FStrings, Strings.Count);

  for I := 0 to Strings.Count - 1 do
    FStrings [I] := Strings [I];
end { TArregloStrings.Create };

destructor TArregloStrings.Destroy;
begin { TArregloStrings.Destroy }
  FStrings := nil;
  inherited Destroy;
end { TArregloStrings.Destroy };

function TArregloStrings.Clonar: TArregloStrings;
begin { TArregloStrings.Clonar }
  Result := TArregloStrings.Create(Self);
end { TArregloStrings.Clonar };

function TArregloStrings.IgualA(Arreglo: TArregloStrings): Boolean;
var
  I: Integer;
begin { TArregloStrings.IgualA }
  Assert(Assigned(Arreglo), 'TArregloStrings.IgualA: Assigned(Arreglo)');

  if Length(FStrings) = Length(Arreglo.FStrings) then
  begin
    I := Low(FStrings);
    Result := true;
    while (I <= High(FStrings)) and Result do
    begin
      Result := Result and (FStrings [I] = Arreglo.FStrings [I]);
      Inc(I);
    end;
  end
  else
    Result := false;
end { TArregloStrings.IgualA };

function TArregloStrings.IndiceDe(AString: String): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TArregloStrings.IndiceDe }
  I := Low(FStrings);
  Result := -1;
  Listo := false;
  while (I <= High(FStrings)) and (not Listo) do
  begin
    if AString = FStrings [I] then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TArregloStrings.IndiceDe };

// -----------------------------------------------------------------------------
// TArregloArreglosStrings
// -----------------------------------------------------------------------------

function TArregloArreglosStrings.GetAlto: Integer;
begin { TArregloArreglosStrings.GetAlto }
  Result := High(FArreglosStrings);
end { TArregloArreglosStrings.GetAlto };

function TArregloArreglosStrings.GetArreglo(I: Integer): TArregloStrings;
begin { TArregloArreglosStrings.GetArreglo }
  Assert((I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings)), 'TArregloArreglosStrings.GetArreglo: (I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings))');

  Result := FArreglosStrings [I];
end { TArregloArreglosStrings.GetArreglo };

function TArregloArreglosStrings.GetBajo: Integer;
begin { TArregloArreglosStrings.GetBajo }
  Result := Low(FArreglosStrings);
end { TArregloArreglosStrings.GetBajo };

function TArregloArreglosStrings.GetDimension: Integer;
begin { TArregloArreglosStrings.GetDimension }
  Result := Length(FArreglosStrings);
end { TArregloArreglosStrings.GetDimension };

function TArregloArreglosStrings.GetString(I, J: Integer): String;
begin { TArregloArreglosStrings.GetString }
  Assert((I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings)), 'TArregloArreglosStrings.GetString: (I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings))');

  Result := FArreglosStrings [I] [J];
end { TArregloArreglosStrings.GetString };

function TArregloArreglosStrings.GetTexto: String;
var
  I: Integer;
begin { TArregloArreglosStrings.GetTexto }
  Result := '[';
  if Length(FArreglosStrings) > 0 then
  begin
    for I := Low(FArreglosStrings) to High(FArreglosStrings) - 1 do
      Result := Result + FArreglosStrings [I].Texto + ', ';

    Result := Result + FArreglosStrings [High(FArreglosStrings)].Texto;
  end;
  Result := Result + ']';
end { TArregloArreglosStrings.GetTexto };

procedure TArregloArreglosStrings.SetArreglo(I: Integer; Arreglo: TArregloStrings);
begin { TArregloArreglosStrings.SetArreglo }
  Assert((I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings)), 'TArregloArreglosStrings.SetArreglo: (I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings))');
  Assert(Assigned(Arreglo), 'TArregloArreglosStrings.SetArreglo: Assigned(Arreglo)');

  FArreglosStrings [I] := Arreglo;
end { TArregloArreglosStrings.SetArreglo };

procedure TArregloArreglosStrings.SetDimension(ADimension: Integer);
var
  I, DimensionAnterior: Integer;
begin { TArregloArreglosStrings.SetDimension }
  Assert(ADimension > 0, 'TArregloArreglosStrings.SetDimension');

  DimensionAnterior := Length(FArreglosStrings);

  if DimensionAnterior > ADimension then
  begin
    for I := DimensionAnterior to High(FArreglosStrings) do
      FreeAndNil(FArreglosStrings [I]);
    SetLength(FArreglosStrings, ADimension);
  end
  else if DimensionAnterior < ADimension then
  begin
    SetLength(FArreglosStrings, ADimension);

    for I := DimensionAnterior to High(FArreglosStrings) do
      FArreglosStrings [I] := nil;
  end;
end { TArregloArreglosStrings.SetDimension };

procedure TArregloArreglosStrings.SetString(I, J: Integer; AString: String);
begin { TArregloArreglosStrings.SetString }
  Assert((I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings)), 'TArregloArreglosStrings.SetString: (I >= Low(FArreglosStrings)) and (I <= High(FArreglosStrings))');

  FArreglosStrings [I] [J] := AString;
end { TArregloArreglosStrings.SetString };

constructor TArregloArreglosStrings.Create(ADimension: Integer);
var
  I: Integer;
begin { TArregloArreglosStrings.Create }
  Assert(ADimension > 0, 'TArregloArreglosStrings.Create: ADimension > 0');

  SetLength(FArreglosStrings, ADimension);

  for I := Low(FArreglosStrings) to High(FArreglosStrings) do
    FArreglosStrings [I] := nil;
end { TArregloArreglosStrings.Create };

destructor TArregloArreglosStrings.Destroy;
var
  I: Integer;
begin { TArregloArreglosStrings.Destroy }
  try
    for I := Low(FArreglosStrings) to High(FArreglosStrings) do
      FreeAndNil(FArreglosStrings [I]);
    FArreglosStrings := nil;
  finally
    inherited Destroy;
  end;
end { TArregloArreglosStrings.Destroy };

end { UnitArregloStrings }. 
