{**
@abstract(Descriptor del tipo de columna de datos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de descriptores del tipo de columnas de las
fuentes de datos del procedimiento.
}
unit UnitColumnaDatos;

interface

uses
  Contnrs;

type
  {** Descriptor (generico o abstracto) del tipo de columna en la fuente de
      datos.
      @abstract(Descriptor de columna de datos generica.) }
  TColumnaDatos = class
  protected
    {** Nombre de la columna }
    FNombre: String;

    {** Metodo de lectura de la propiedad Texto. Genera una representacion de
        cadena de caracteres del descriptor de columna. Este metodo debe ser
        implementado por cada subclase para especializar la representacion de
        acuerdo al tipo particular.
        @returns(Representacion de cadena de caracteres del descriptor.) }
    function GetTexto: String; virtual; abstract;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.) }
    constructor Create (ANombre: String);
    {** Compara el descriptor referido con el parametro. Se utiliza solo el
        nombre de la columna para distinguir descriptores.
        @param(Columna Descriptor de columna a comparar.)
        @returns(Devuelve @true si los descriptores son iguales, y @false en
        caso constrario.) }
    function IgualA(Columna: TColumnaDatos): Boolean;
    {** Nombre de la columna. }
    property Nombre: String read FNombre write FNombre;
    {** Representacion en cadena de caracteres del descriptor de columna. }
    property Texto: String read GetTexto;
  end { TColumnaDatos };

  {** Descriptor de columna de numeros enteros.
      @abstract(Descriptor de columna de numeros enteros.) }
  TColumnaDatosEnteros = class(TColumnaDatos)
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
  end { TColumnaDatosEnteros };

  {** Descriptor de columna de numeros reales.
      @abstract(Descriptor de columna de numeros reales.) }
  TColumnaDatosReales = class(TColumnaDatos)
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
  end { TColumnaDatosReales };

  {** Descriptor de columna de valores categoricos. Puede tratarse de cualquier
      tipo de datos, pero los mismos solo son interpretados como cadenas de
      caracteres.
      @abstract(Descriptor de columna de valores categoricos.) }
  TColumnaDatosCategoricos = class(TColumnaDatos)
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
  end { TColumnaDatosCategoricos };

  {** Lista de descriptores de columnas de datos. Es una clase envoltura de
      TObjectList. Se elige esta arquitectura para permitir el chequeo de tipos
      de los elementos de la lista y aprovechar los servicios prestados por
      TObjectList.
      @abstract(Lista de @link(TColumnaDatos).) }
  TListaColumnasDatos = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad Columnas. Devuelve el descriptor de
        columnas asociado a un indice respecto del orden en que fueron agregados
        los elementos a la lista. El orden de esta operacion depende de la
        cantidad de elementos de la misma, por lo que deberia utilizarse
        esporadicamente.
        @param(Indice Indice del descriptor de columna que deseamos accerder.)
        @returns(Descriptor de columna asociado al indice especificado.) }
    function GetItem(Index: Integer): TColumnaDatos;
    {** Metodo de escritura de la propiedad Columnas. Actualiza el valor del
        descriptor de columna almacenado bajo un indice. El orden de esta
        operacion depende de la cantidad de elementos de la misma, por lo que
        deberia utilizarse esporadicamente.
        @param(Indice Indice del descriptor de columna que deseamos actualizar.)
        @param(Item Nuevo valor del descriptor de columna.) }
    procedure SetItem(Index: Integer; Item: TColumnaDatos);
  public
    destructor Destroy; override;
    {** Agrega un descriptor de columna al final de la lista.
        @param(Item Descriptor de columna a agregar.)
        @returns(Indice del descriptor de columna agregado.) }
    function Add(Item: TColumnaDatos): Integer;
    {** Busca el primer descriptor de columna en la lista cuyo nombre coincida
        con el parametro.
        @param(Nombre Nombre de la columna a buscar.)
        @returns(El indice de la primer columna cuyo nombre coincida con el
        parametro, o - 1 en caso de no encontrar tal elemento.) }
    function IndexOf(Nombre: String): Integer; overload;
    {** Inserta un descriptor de columna bajo un indice particular desplazando
        el resto de los elementos.
        @param(Index Indice bajo el cual almacenar el descriptor de columna.)
        @param(Item Descriptor de columna a insertar.) }
    procedure Insert(Index: Integer; Item: TColumnaDatos);
    {** Descriptores de columnas almacenados. }
    property Columnas [Index: Integer]: TColumnaDatos read GetItem  write SetItem; default;
  end { TListaColumnasDatos };

implementation

// -----------------------------------------------------------------------------
// TColumnaDatos
// -----------------------------------------------------------------------------

function TColumnaDatos.IgualA(Columna: TColumnaDatos): Boolean;
begin { TColumnaDatos.IgualA }
  Result := FNombre = Columna.FNombre;
end { TColumnaDatos.IgualA };

constructor TColumnaDatos.Create(ANombre: String);
begin { TColumnaDatos.Create }
  Assert(ANombre <> '', 'TColumnaDatos.Create: ANombre <> '''''); 

  FNombre := ANombre;
end { TColumnaDatos.Create };

// -----------------------------------------------------------------------------
// TColumnaDatosEnteros
// -----------------------------------------------------------------------------

function TColumnaDatosEnteros.GetTexto: String;
begin { TColumnaDatosEnteros.GetTexto }
  Result := FNombre + ':Enteros';
end { TColumnaDatosEnteros.GetTexto };

// -----------------------------------------------------------------------------
// TColumnaReales
// -----------------------------------------------------------------------------

function TColumnaDatosReales.GetTexto: String;
begin { TColumnaDatosReales.GetTexto }
  Result := FNombre + ':Reales';
end { TColumnaDatosReales.GetTexto };

// -----------------------------------------------------------------------------
// TColumnaDatosCategoricos
// -----------------------------------------------------------------------------

function TColumnaDatosCategoricos.GetTexto: String;
begin { TColumnaDatosCategoricos.GetTexto }
  Result := FNombre + ':Categoricos';
end { TColumnaDatosCategoricos.GetTexto };

// -----------------------------------------------------------------------------
// TListaColumnaDatos
// -----------------------------------------------------------------------------

function TListaColumnasDatos.GetItem(Index: Integer): TColumnaDatos;
begin { TListaColumnasDatos.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaColumnasDatos.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TColumnaDatos;
end { TListaColumnasDatos.GetItem };

procedure TListaColumnasDatos.SetItem(Index: Integer; Item: TColumnaDatos);
begin { TListaColumnasDatos.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaColumnasDatos.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaColumnasDatos.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaColumnasDatos.SetItem };

destructor TListaColumnasDatos.Destroy;
begin { TListaColumnasDatos.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaColumnasDatos.Destroy };

function TListaColumnasDatos.Add(Item: TColumnaDatos): Integer;
begin { TListaColumnasDatos.Add }
  Assert(Assigned(Item), 'TListaColumnasDatos.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaColumnasDatos.Add };

function TListaColumnasDatos.IndexOf(Nombre: String): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TListaColumnasDatos.IndexOf }
  I := 0;
  Result := -1;
  Listo := false;

  while (I < Count) and (not Listo) do
  begin
    if (GetItem(I) as TColumnaDatos).Nombre = Nombre then
    begin
      Listo := true;
      Result := I;
    end;

    Inc(I);
  end;
end { TListaColumnasDatos.IndexOf };

procedure TListaColumnasDatos.Insert(Index: Integer; Item: TColumnaDatos);
begin { TListaColumnasDatos.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaColumnasDatos.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaColumnasDatos.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaColumnasDatos.Insert };

end { UnitColumnaDatos }.
