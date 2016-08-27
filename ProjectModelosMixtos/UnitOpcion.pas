{**
@abstract(Opciones con distintos parametros para modelos mixtos y procedimientos
de simulacion de los mismos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 22, 2005)
Este modulo contiene la definicion de opciones con distintos parametros,
utilizadas para describir propiedades de componentes de la configuracion de
modelos mixtos y procedimientos de simulacion de los mismos.
}
unit UnitOpcion;

interface

uses
  Contnrs,
  UnitEstructura, UnitEfecto;

type
  {** Opcion basica para desciribir alguna caracteristica de distintos elementos
      que componen las configuraciones de modelos y procedimiento. Se hereda
      esta clase para poder asignar diferentes parametros a la opcion.
      @abstract(Opcion basica.) }
  TOpcion = class
  protected
    {** Nombre de la opcion o propiedad. }
    FNombre: String;
    {** Metodo de lectura de la propiedad @link(Texto). Devuelve una
        representacion de cadena de caracteres de la opcion.
        @returns(Representacion de cadena de caracteres de la opcion.) }
    function GetTexto: String; virtual;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.) }
    constructor Create(ANombre: String);
    {** Nombre de la opcion. }
    property Nombre: String read FNombre write FNombre;
    {** Representacion textual de la opcion. }
    property Texto: String read GetTexto;
  end { TOpcion };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      booleanos.
      @abstract(Opcion de parametro Boolean.) }
  TOpcionParametroBoolean = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: Boolean;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: Boolean);
    {** Parametro de la opcion o propiedad. }
    property Parametro: Boolean read FParametro write FParametro;
  end { TOpcionParametroBoolean };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      de valores enteros.
      @abstract(Opcion de parametro Integer.) }
  TOpcionParametroInteger = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: Integer;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: Integer);
    {** Parametro de la opcion o propiedad. }
    property Parametro: Integer read FParametro write FParametro;
  end { TOpcionParametroInteger };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      de valores reales.
      @abstract(Opcion de parametro Real.) }
  TOpcionParametroReal = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: Real;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: Real);
    {** Parametro de la opcion o propiedad. }
    property Parametro: Real read FParametro write FParametro;
  end { TOpcionParametroReal };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      de cadenas de caracteres.
      @abstract(Opcion de parametro String.) }
  TOpcionParametroString = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: String;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: String);
    {** Parametro de la opcion o propiedad. }
    property Parametro: String read FParametro write FParametro;
  end { TOpcionParametroString };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      de objetos genericos.
      @abstract(Opcion de parametro TObject.) }
  TOpcionParametroTObject = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: TObject;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: TObject);
    {** Parametro de la opcion o propiedad. }
    property Parametro: TObject read FParametro write FParametro;
  end { TOpcionParametroTObject };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      de identificadores de estructuras (Ver @link(UnitEstructura.TEstructura)).
      @abstract(Opcion de parametro TEstructura.) }
  TOpcionParametroTEstructura = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: TEstructura;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: TEstructura);
    {** Parametro de la opcion o propiedad. }
    property Parametro: TEstructura read FParametro write FParametro;
  end { TOpcionParametroTEstructura };

  {** Ver @link(TOpcion). Especializacion de las opciones para parametros
      efecto (Ver @link(UnitEfecto.TEfecto)).
      @abstract(Opcion de parametro TEfecto.) }
  TOpcionParametroTEfecto = class(TOpcion)
  protected
    {** Parametro de la opcion o propiedad. }
    FParametro: TEfecto;
    {** Ver @link(TOpcion.GetTexto.) }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la opcion o propiedad.)
        @param(AParametro Parametro de la opcion.) }
    constructor Create(ANombre: String; AParametro: TEfecto);
    {** Parametro de la opcion o propiedad. }
    property Parametro: TEfecto read FParametro write FParametro;
  end { TOpcionParametroTEfecto };

  {** Lista de @link(TOpcion). Es una clase envoltura de TObjectList. Se
      utiliza esta arquitectura para proveer cheque de tipos de las instancias
      almacenadas y aprovechar la funcionalidad de TObjectList.
      @abstract(Lista de @link(TOpcion).) }
  TListaOpciones = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad @link(Opciones). Devuelve el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(Index: Integer): TOpcion;
    {** Metodo de escritura de la propiedad @link(Opciones). Actualiza el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(Index: Integer; Item: TOpcion);
  public
    {** Destructor. }
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(Item: TOpcion): Integer;
    {** Busca un elemento dentro de la lista por nombre y asigna el indice
        encontrado al parametro. }
    function BuscarAsignar(var Indice: Integer; const Nombre: String): Boolean;
    {** Busca un elemento dentro de la lista.
        @param(Nombre Nombre de la opcion a buscar.)
        @returns(Devuelve el indice de la opcion en caso de encontrarlo, o -1 en
        caso contrario.) }
    function IndexOf(Nombre: String): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(Index: Integer; Item: TOpcion);
    {** Opciones almacenadas. }
    property Opciones [Index: Integer]: TOpcion read GetItem write SetItem; default;
  end { TListaOpciones };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TOpcion
// -----------------------------------------------------------------------------

constructor TOpcion.Create(ANombre: String);
begin { TOpcion.Create }
  Assert(ANombre <> '', 'TOpcion.Create: ANombre <> ''''');
  FNombre := ANombre;
end { TOpcion.Create };

function TOpcion.GetTexto: String;
begin { TOpcion.GetTexto }
  Result := FNombre;
end { TOpcion.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroBoolean
// -----------------------------------------------------------------------------

constructor TOpcionParametroBoolean.Create(ANombre: String; AParametro: Boolean);
begin { TOpcionParametroBoolean.Create }
  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroBoolean.Create };

function TOpcionParametroBoolean.GetTexto: String;
begin { TOpcionParametroBoolean.GetTexto }
  if FParametro then
    Result := FNombre + ' [Si]'
  else
    Result := FNombre + ' [No]';
end { TOpcionParametroBoolean.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroInteger
// -----------------------------------------------------------------------------

constructor TOpcionParametroInteger.Create(ANombre: String; AParametro: Integer);
begin { TOpcionParametroInteger.Create }
  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroInteger.Create };

function TOpcionParametroInteger.GetTexto: String;
begin { TOpcionParametroInteger.GetTexto }
  Result := FNombre + ' [' + IntToStr(FParametro) + ']';
end { TOpcionParametroInteger.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroReal
// -----------------------------------------------------------------------------

constructor TOpcionParametroReal.Create(ANombre: String; AParametro: Real);
begin { TOpcionParametroReal.Create }
  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroReal.Create };

function TOpcionParametroReal.GetTexto: String;
begin { TOpcionParametroReal.GetTexto }
  Result := FNombre + ' [' + FloatToStr(FParametro) + ']';
end { TOpcionParametroReal.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroString
// -----------------------------------------------------------------------------

constructor TOpcionParametroString.Create(ANombre: String; AParametro: String);
begin { TOpcionParametroString.Create }
  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroString.Create };

function TOpcionParametroString.GetTexto: String;
begin { TOpcionParametroString.GetTexto }
  Result := FNombre + ' [' + FParametro + ']';
end { TOpcionParametroString.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroTObject
// -----------------------------------------------------------------------------

constructor TOpcionParametroTObject.Create(ANombre: String; AParametro: TObject);
begin { TOpcionParametroTObject.Create }
  Assert(Assigned(AParametro), 'TOpcionParametroTObject.Create: Assigned(AParametro)');

  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroTObject.Create };

function TOpcionParametroTObject.GetTexto: String;
begin { TOpcionParametroTObject.GetTexto }
  Result := FNombre + ' [...]'
end { TOpcionParametroTObject.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroTEstructura
// -----------------------------------------------------------------------------

constructor TOpcionParametroTEstructura.Create(ANombre: String; AParametro: TEstructura);
begin { TOpcionParametroTEstructura.Create }
  Assert(Assigned(AParametro), 'TOpcionParametroTEstructura.Create: Assigned(AParametro)');

  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroTEstructura.Create };

function TOpcionParametroTEstructura.GetTexto: String;
begin { TOpcionParametroTEstructura.GetTexto }
  Result := FNombre + ' [' + FParametro.Texto + ']'
end { TOpcionParametroTEstructura.GetTexto };

// -----------------------------------------------------------------------------
// TOpcionParametroTEfecto
// -----------------------------------------------------------------------------

constructor TOpcionParametroTEfecto.Create(ANombre: String; AParametro: TEfecto);
begin { TOpcionParametroTEfecto.Create }
  Assert(Assigned(AParametro), 'TOpcionParametroTEfecto.Create: Assigned(AParametro)');

  inherited Create(ANombre);
  FParametro := AParametro;
end { TOpcionParametroTEfecto.Create };

function TOpcionParametroTEfecto.GetTexto: String;
begin { TOpcionParametroTEfecto.GetTexto }
  Result := FNombre + ' [' + FParametro.Texto + ']'
end { TOpcionParametroTEfecto.GetTexto };

// -----------------------------------------------------------------------------
// TListaOpciones
// -----------------------------------------------------------------------------

function TListaOpciones.GetItem(Index: Integer): TOpcion;
begin { TListaOpciones.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaOpciones.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TOpcion;
end { TListaOpciones.GetItem };

procedure TListaOpciones.SetItem(Index: Integer; Item: TOpcion);
begin { TListaOpciones.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaOpciones.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaOpciones.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaOpciones.SetItem };

destructor TListaOpciones.Destroy;
begin { TListaOpciones.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaOpciones.Destroy };

function TListaOpciones.Add(Item: TOpcion): Integer;
begin { TListaOpciones.Add }
  Assert(Assigned(Item), 'TListaOpciones.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaOpciones.Add };

function TListaOpciones.BuscarAsignar(var Indice: Integer; const Nombre: String): Boolean;
begin { TListaOpciones.BuscarAsignar }
  Indice := IndexOf(Nombre);
  Result := Indice <> -1;
end { TListaOpciones.BuscarAsignar };

function TListaOpciones.IndexOf(Nombre: String): Integer;
var
  I: Integer;
begin { TListaOpciones.IndexOf }
  Result := -1;
  I := 0;

  while (I < Count) and (Result = -1) do
  begin
    if (GetItem(I) as TOpcion).Nombre = Nombre then
      Result := I;
    Inc(I);
  end;
end { TListaOpciones.IndexOf };

procedure TListaOpciones.Insert(Index: Integer; Item: TOpcion);
begin { TListaOpciones.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaOpciones.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaOpciones.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaOpciones.Insert };

end { UnitOpcion }.
