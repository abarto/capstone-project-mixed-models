{**
@abstract(Arreglo dinamico de enteros y estructuras derivadas.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de arreglos dinamicos de enteros y
estructuras de datos mas complejas construidas a partir de estos. La principal
utilidad de estas estructuras es almacenar indices a otras estructuras.
}
unit UnitArregloEnteros;

interface

uses
  Contnrs;

type
  {** Arreglo dinamico de enteros. Es una clase envoltura de un array of
      Integer. Se utiliza una clase envolvente para facilitar la manipulacion de
      los arreglos dinamicos y proveer de una interfaz consistente.
      @abstract(Arreglo dinamico de numeros enteros.) }
  TArregloEnteros = class
  private
    {** El arreglo de enteros propiamente dicho. }
    FEnteros: array of Integer;
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
    {** Metodo de lectura de la propiedad @link(Enteros). Devuelve el valor de
        un entero almacenado bajo un indice.
        @param(I Indice del entero que deseamos acceder.)
        @returns(Valor del entero bajo el indice.) }
    function GetEntero(I: Integer): Integer;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo.
        @returns(Representacion textual del arreglo.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FEnteros).
        @param(ADimension Nueva dimension del arreglo dinamico.) }
    procedure SetDimension(ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Enteros). Actualiza el valor
        del entero almacenado bajo un indice.
        @param(I Indice del entero a actualizar.)
        @param(Entero Nuevo valor para el entero bajo el indice.) }
    procedure SetEntero(I: Integer; Entero: Integer);
  public
    {** Constructor. Crea una instancia con la dimension indicada.
        @param(ADimension Dimension inicial del arreglo.) }
    constructor Create(ADimension: Integer);  overload;
    {** Constructor. Esta version del constructor crea una nueva instancia
        copiando el contenido del parametro.
        @param(Arreglo Arreglo del cual se desea obtener una instancia
        identica.) }
    constructor Create(Arreglo: TArregloEnteros);  overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Genera una copia por valor del arreglo.
        @returns(Copia por valor del arreglo.) }
    function Clonar: TArregloEnteros;
    {** Indica si arreglo y el parametro coinciden en dimension y contenido.
        @param(Arreglo Arreglo a comparar.)
        @returns(@true en caso que los arreglos coincidan en dimension y
        contenido, @false en caso contrario.) }
    function IgualA(Arreglo: TArregloEnteros): Boolean;
    {** Busca un entero dentro del arreglo.
        @param(Entero Entero a buscar en el arreglo.)
        @returns(Indice asociado al entero o -1 si tal no fuese encontrado.) }
    function IndiceDe(Entero: Integer): Integer;
    {** Calcula el indice del mayor entero del arreglo.
        @returns(Indice asociado al mayor entero almacenado.) }
    function Mayor: Integer;
    {** Calcula el indice del menor entero del arreglo.
        @returns(Indice asociado al menor entero almacenado.) }
    function Menor: Integer;
    {** Decrementa el valor de un entero asociado a un indice.
        @param(I Indice del valor entero a actualizar.)
        @param(Entero Valor a restar al entero.) }
    procedure Decrementar(I: Integer; Entero: Integer = 1);
    {** Incrementa el valor de un entero asociado a un indice.
        @param(I Indice del valor entero a actualizar.)
        @param(Entero Valor a sumar al entero.) }
    procedure Incrementar(I: Integer; Entero: Integer = 1);
    {** Intercambia los enteros asociados a dos indices en el arreglo.
        @param(I Indice del primer entero.)
        @param(J Indice del segundo entero.) }
    procedure Intercambiar(I, J: Integer);
    {** Inicializa todos los valores del arreglo a un entero dado.
        @param(Entero Nuevo valor para todas las entradas del arreglo.) }
    procedure PonerA(Entero: Integer); overload;
    {** Inicializa todos los valores del arreglo tomando los valores de un
        arreglo dado.
        @param(Arreglo Arreglo que contiene los nuevos valores.) }
    procedure PonerA(Arreglo: TArregloEnteros); overload;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension (o longitud) del arreglo. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Enteros almacenados en el arreglo. }
    property Enteros [I: Integer]: Integer read GetEntero write SetEntero; default;
    {** Representacion textual del arreglo. }
    property Texto: String read GetTexto;
  end { TArregloEnteros };

  {** Lista de @link(TArregloEnteros). Es una clase envoltura de TObjectList. Se
      utiliza esta arquitectura para proveer cheque de tipos de las instancias
      almacenadas y aprovechar la funcionalidad de TObjectList.
      @abstract(Lista de @link(TArregloEnteros).) }
  TListaArreglosEnteros = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad @link(Arreglos). Devuelve el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(Index: Integer): TArregloEnteros;
    {** Metodo de escritura de la propiedad @link(Arreglos). Actualiza el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(Index: Integer; Item: TArregloEnteros);
  public
    {** Destructor. }
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(Item: TArregloEnteros): Integer;
    {** Busca un elemento dentro de la lista comparando dimension y contenido.
        (Ver @link(TArregloEnteros.IgualA)).
        @param(Arreglo Arreglo a buscar.)
        @returns(Devuelve el indice del arreglo en caso de encontrarlo, o -1 en
        caso contrario.) }
    function IndexOf(Arreglo: TArregloEnteros): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(Index: Integer; Item: TArregloEnteros);
    {** Arreglos de enteros almacenados. }
    property Arreglos [Index: Integer]: TArregloEnteros read GetItem write SetItem; default;
  end { TListaArreglosEnteros };

  {** Arreglo dinamico de @link(TArregloEnteros). Es una clase envoltura de
      un array of TArregloEnteros. Funciona como una matriz, pero en cada fila
      es posible tener una cantidad diferente de columnas. Se utiliza una clase
      envolvente para facilitar la manipulacion de los arreglos y proveer de una
      interfaz consistente.
      @abstract(Arreglo dinamico de @link(TArregloEnteros).) }
  TArregloArreglosEnteros = class
  private
    {** Arreglo dinamico de arreglos dinamicos de enteros. }
    FArreglosEnteros: array of TArregloEnteros;
    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico.
        @returns(El mayor indice del arreglo dinamico.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Arreglos). Devuelve el arreglo
        asociado a un indice.
        @param(I Indice del arreglo que deseamos acceder.)
        @returns(Arreglo bajo el indice.) }
    function GetArreglo(I: Integer): TArregloEnteros;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico.
        @returns(El menor indice del arreglo dinamico.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension de arreglo dinamico.
        @returns(Dimension (o longitud) del arreglo dinamico.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(Enteros). Devuelve un entero
        dentro de los arreglos de enteros almacenados.
        @param(I Indice del arreglo de enteros.)
        @param(J Indice del entero.)
        @returns(Entero bajo el indice I en el arreglo bajo el indice J.) }
    function GetEntero(I, J: Integer): Integer;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo de arreglos de
        enteros.
        @returns(Representacion textual del arreglo.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Arreglos). Actualiza el valor
        de un arreglo almacenado.
        @param(I Indice del arreglo a actualizar.)
        @param(Arreglo Nuevo valor para el arreglo.) }
    procedure SetArreglo(I: Integer; Arreglo: TArregloEnteros);
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FArreglosEnteros).
        @param(ADimension Nueva dimension del arreglo.) }
    procedure SetDimension(ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Enteros). Actualiza el valor
        de un entero en alguno de los arreglos almacenados.
        @param(I Indice del arreglo que contiene al entero.)
        @param(J Indice del entero dentro del arreglo.)
        @param(Entero Valor del entero a almacenar.) }
    procedure SetEntero(I, J: Integer; Entero: Integer);
  public
    {** Constructor. Esta version del constructor dimensiona el arreglo
        dinamico e inicializa cada elemento a @nil.
        @param(ADimension Dimension inicial del arreglo dinamico.) }
    constructor Create(ADimension: Integer); overload;
    {** Constructor. Esta version del constructor dimensiona el arreglo
        dinamico e inicializa cada instancia contenida a arreglos de una
        dimension particular.
        @param(ADimension Dimension inicial del arreglo dinamico.)
        @param(DimensionArreglos Dimension inicial de los arreglos
        almacenados.) }
    constructor Create(ADimension, DimensionArreglos: Integer); overload;
    {** Constructor. Esta version del constructor genera la instancia a partir
        del contenido de una lista de arreglos de enteros.
        @param(Lista Lista de arreglos de enteros de la cual extraer los
        arreglos.) }
    constructor Create(Lista: TListaArreglosEnteros); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Busca un arreglo de enteros dentro de los almacenados. Las comparaciones
        se hacen por contenido (ver @link(TArregloEnteros.IgualA)) en lugar de
        utilizar la referencia.
        @param(Arreglo Arreglo a buscar entre los almacenados.)
        @returns(Indice del arreglo identico al parametro en caso de
        encontrarlo, o -1 en caso contrario.)}
    function IndiceDe(Arreglo: TArregloEnteros): Integer;
    {** Decrementa el valor de un entero en un arreglo asociado a un indice.
        @param(I Indice del arreglo de enteros actualizar.)
        @param(J Indice del entero a actualizar.)
        @param(Entero Valor a restar al entero.) }
    procedure Decrementar(I, J: Integer; Entero: Integer = 1);
    {** Incrementa el valor de un entero en un arreglo asociado a un indice.
        @param(I Indice del arreglo de enteros actualizar.)
        @param(J Indice del entero a actualizar.)
        @param(Entero Valor a sumar al entero.) }
    procedure Incrementar(I, J: Integer; Entero: Integer = 1);
    {** Intercambia los arreglos asociados a dos indices.
        @param(I Indice del primer arreglo.)
        @param(J Indice del segundo arreglo.) }
    procedure Intercambiar(I, J: Integer);
    {** Inicializa todos los valores de los arreglos a un entero dado.
        @param(Entero Nuevo valor para todas las entradas de los arreglos.) }
    procedure PonerA(Entero: Integer);
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Arreglos de enteros almacenados. }
    property Arreglos [I: Integer]: TArregloEnteros read GetArreglo write SetArreglo;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension del arreglo dinamico. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Enteros contenidos en elos arreglos almacenados. }
    property Enteros [I, J: Integer]: Integer read GetEntero write SetEntero; default;
    {** Representacion textual del arreglo. }
    property Texto: String read GetTexto;
  end { TArregloArreglosEnteros };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TArregloEnteros
// -----------------------------------------------------------------------------

function TArregloEnteros.GetAlto: Integer;
begin { TArregloEnteros.GetAlto }
  Result := High(FEnteros);
end { TArregloEnteros.GetAlto };

function TArregloEnteros.GetBajo: Integer;
begin { TArregloEnteros.GetBajo }
  Result := Low(FEnteros);
end { TArregloEnteros.GetBajo };

function TArregloEnteros.GetDimension: Integer;
begin { TArregloEnteros.GetDimension }
  Result := Length(FEnteros);
end { TArregloEnteros.GetDimension };

function TArregloEnteros.GetEntero(I: Integer): Integer;
begin { TArregloEnteros.GetEntero }
  Assert((I >= Low(FEnteros)) and (I <= High(FEnteros)), 'TArregloEnteros.GetEntero: (I >= Low(FEnteros)) and (I <= High(FEnteros))');

  Result := FEnteros [I];
end { TArregloEnteros.GetEntero };

function TArregloEnteros.GetTexto: String;
var
  I: Integer;
begin { TArregloEnteros.GetTexto }
  Result := '[';
  if Length(FEnteros) > 0 then
  begin
    for I := Low(FEnteros) to High(FEnteros) - 1 do
      Result := Result + IntToStr(FEnteros [I]) + ', ';

    Result := Result + IntToStr(FEnteros [High(FEnteros)]);
  end;
  Result := Result + ']';
end { TArregloEnteros.GetTexto };

procedure TArregloEnteros.SetDimension(ADimension: Integer);
var
  I: Integer;
  DimensionAnterior: Integer;
begin { TArregloEnteros.SetDimension }
  Assert(ADimension > 0, 'TArregloEnteros.SetDimension: ADimension > 0');

  DimensionAnterior := Length(FEnteros);

  if ADimension <> DimensionAnterior then
  begin
    SetLength(FEnteros, ADimension);

    if DimensionAnterior < Length(FEnteros) then
    begin
      for I := DimensionAnterior to High(FEnteros) do
        FEnteros [I] := 0;
    end;
  end;
end { TArregloEnteros.SetDimension };

procedure TArregloEnteros.SetEntero(I: Integer; Entero: Integer);
begin { TArregloEnteros.SetEntero }
  Assert((I >= Low(FEnteros)) and (I <= High(FEnteros)), 'TArregloEnteros.SetEntero: (I >= Low(FEnteros)) and (I <= High(FEnteros))');

  FEnteros [I] := Entero;
end { TArregloEnteros.SetEntero };

constructor TArregloEnteros.Create(ADimension: Integer);
var
  I: Integer;
begin { TArregloEnteros.Create }
  Assert(ADimension >= 0, 'TArregloEnteros.Create: ADimension > 0');

  SetLength(FEnteros, ADimension);

  for I := Low(FEnteros) to High(FEnteros) do
    FEnteros [I] := 0;
end { TArregloEnteros.Create };

constructor TArregloEnteros.Create(Arreglo: TArregloEnteros);
begin { TArregloEnteros.Create }
  Assert(Assigned(Arreglo), 'TArregloEnteros.Create: Assigned(Arreglo)');

  FEnteros := Copy(Arreglo.FEnteros, 0, Length(Arreglo.FEnteros));
end { TArregloEnteros.Create };

destructor TArregloEnteros.Destroy;
begin { TArregloEnteros.Destroy }
  FEnteros := nil;
  inherited Destroy;
end { TArregloEnteros.Destroy };

function TArregloEnteros.Clonar: TArregloEnteros;
begin { TArregloEnteros.Clonar }
  Result := TArregloEnteros.Create(Self);
end { TArregloEnteros.Clonar };

function TArregloEnteros.IgualA(Arreglo: TArregloEnteros): Boolean;
var
  I: Integer;
begin { TArregloEnteros.IgualA }
  Assert(Assigned(Arreglo), 'TArregloEnteros.IgualA: Assigned(Arreglo)');

  if Length(FEnteros) = Length(Arreglo.FEnteros) then
  begin
    I := Low(FEnteros);
    Result := true;
    while (I <= High(FEnteros)) and Result do
    begin
      Result := Result and (FEnteros [I] = Arreglo.FEnteros [I]);
      Inc(I);
    end;
  end
  else
    Result := false;
end { TArregloEnteros.IgualA };

function TArregloEnteros.IndiceDe(Entero: Integer): Integer;
var
  Listo: Boolean;
  I: Integer;
begin { TArregloEnteros.IndiceDe }
  I := Low(FEnteros);
  Result := -1;
  Listo := false;
  while (I <= High(FEnteros)) and (not Listo) do
  begin
    if FEnteros [I] = Entero then
    begin
      Listo := true;
      Result := I;
    end;

    Inc(I);
  end;
end { TArregloEnteros.IndiceDe };

function TArregloEnteros.Mayor: Integer;
var
  I, Mayor: Integer;
begin { TArregloEnteros.Mayor }
  Assert(Length(FEnteros) > 0, 'TArregloEnteros.Mayor: Length(FEnteros) > 0'); 

  Mayor := FEnteros [Low(FEnteros)];
  Result := Low(FEnteros);

  for I := Low(FEnteros) + 1 to High(FEnteros) do
  begin
    if FEnteros [I] > Mayor then
    begin
      Mayor := FEnteros [I];
      Result := I;
    end;
  end;
end { TArregloEnteros.Mayor };

function TArregloEnteros.Menor: Integer;
var
  I, Menor: Integer;
begin { TArregloEnteros.Menor }
  Assert(Length(FEnteros) > 0, 'TArregloEnteros.Menor: Length(FEnteros) > 0');

  Menor := FEnteros [Low(FEnteros)];
  Result := Low(FEnteros);

  for I := Low(FEnteros) + 1 to High(FEnteros) do
  begin
    if FEnteros [I] < Menor then
    begin
      Menor := FEnteros [I];
      Result := I;
    end;
  end;
end { TArregloEnteros.Menor };

procedure TArregloEnteros.Decrementar(I: Integer; Entero: Integer);
begin { TArregloEnteros.Decrementar }
  Assert((I >= Low(FEnteros)) and (I <= High(FEnteros)), 'TArregloEnteros.Decrementar: (I >= Low(FEnteros)) and (I <= High(FEnteros))');

  Dec(FEnteros [I], Entero);
end { TArregloEnteros.Decrementar };

procedure TArregloEnteros.Incrementar(I: Integer; Entero: Integer);
begin { TArregloEnteros.Incrementar }
  Assert((I >= Low(FEnteros)) and (I <= High(FEnteros)), 'TArregloEnteros.Incrementar: (I >= Low(FEnteros)) and (I <= High(FEnteros))');

  Inc(FEnteros [I], Entero);
end { TArregloEnteros.Incrementar };

procedure TArregloEnteros.Intercambiar(I, J: Integer);
var
  Entero: Integer;
begin { TArregloEnteros.Intercambiar }
  Assert((I >= Low(FEnteros)) and (I <= High(FEnteros)), 'TArregloEnteros.Intercambiar: (I >= Low(FEnteros)) and (I <= High(FEnteros))');
  Assert((J >= Low(FEnteros)) and (J <= High(FEnteros)), 'TArregloEnteros.Intercambiar: (J >= Low(FEnteros)) and (J <= High(FEnteros))');

  if I <> J then
  begin
    Entero := FEnteros [I];
    FEnteros [I] := FEnteros [J];
    FEnteros [J] := Entero;
  end;
end { TArregloEnteros.Intercambiar };

procedure TArregloEnteros.PonerA(Entero: Integer);
var
  I: Integer;
begin { TArregloEnteros.PonerA }
  for I := Low(FEnteros) to High(FEnteros) do
    FEnteros [I] := Entero;
end { TArregloEnteros.PonerA };

procedure TArregloEnteros.PonerA(Arreglo: TArregloEnteros);
var
  I: Integer;
begin { TArregloEnteros.PonerA }
  Assert(Assigned(Arreglo), 'TArregloEnteros.PonerA: Assigned(Arreglo)');

  I := Low(FEnteros);
  while (I <= High(FEnteros)) and (I <= High(Arreglo.FEnteros)) do
  begin
    FEnteros [I] := Arreglo.FEnteros [I];
    Inc(I);
  end;
end { TArregloEnteros.PonerA };

// -----------------------------------------------------------------------------
// TListaArreglosEnteros
// -----------------------------------------------------------------------------

function TListaArreglosEnteros.GetItem(Index: Integer): TArregloEnteros;
begin { TListaArreglosEnteros.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaArreglosEnteros.GetItem: (Index >= 0) and (Index < Count)');
   
  Result := inherited GetItem(Index) as TArregloEnteros;
end { TListaArreglosEnteros.GetItem };

procedure TListaArreglosEnteros.SetItem(Index: Integer; Item: TArregloEnteros);
begin { TListaArreglosEnteros.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaArreglosEnteros.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaArreglosEnteros.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaArreglosEnteros.SetItem };

destructor TListaArreglosEnteros.Destroy;
begin { TListaArreglosEnteros.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaArreglosEnteros.Destroy };

function TListaArreglosEnteros.Add(Item: TArregloEnteros): Integer;
begin { TListaArreglosEnteros.Add }
  Assert(Assigned(Item), 'TListaArreglosEnteros.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaArreglosEnteros.Add };

function TListaArreglosEnteros.IndexOf(Arreglo: TArregloEnteros): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TListaArreglosEnteros.IndexOf }
  Assert(Assigned(Arreglo), 'TListaArreglosEnteros.IndexOf: Assigned(Arreglo)');

  I := 0;
  Result := -1;
  Listo := false;
  while (I < Count) and (not Listo) do
  begin
    if Arreglo.IgualA(inherited GetItem(I) as TArregloEnteros) then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TListaArreglosEnteros.IndexOf };

procedure TListaArreglosEnteros.Insert(Index: Integer; Item: TArregloEnteros);
begin { TListaArreglosEnteros.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaArreglosEnteros.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaArreglosEnteros.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaArreglosEnteros.Insert };

// -----------------------------------------------------------------------------
// TArregloArreglosEnteros
// -----------------------------------------------------------------------------

function TArregloArreglosEnteros.GetAlto: Integer;
begin { TArregloArreglosEnteros.GetAlto }
  Result := High(FArreglosEnteros);
end { TArregloArreglosEnteros.GetAlto };

function TArregloArreglosEnteros.GetBajo: Integer;
begin { TArregloArreglosEnteros.GetBajo }
  Result := Low(FArreglosEnteros);
end { TArregloArreglosEnteros.GetBajo };

function TArregloArreglosEnteros.GetArreglo(I: Integer): TArregloEnteros;
begin { TArregloArreglosEnteros.GetArreglo }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.GetArreglo: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');

  Result := FArreglosEnteros [I];
end { TArregloArreglosEnteros.GetArreglo };

function TArregloArreglosEnteros.GetDimension: Integer;
begin { TArregloArreglosEnteros.GetDimension }
  Result := Length(FArreglosEnteros);
end; { TArregloArreglosEnteros.GetDimension }

function TArregloArreglosEnteros.GetEntero(I, J: Integer): Integer;
begin { TArregloArreglosEnteros.GetEntero }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.GetEntero: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');

  Result := FArreglosEnteros [I] [J];
end { TArregloArreglosEnteros.GetEntero };

function TArregloArreglosEnteros.GetTexto: String;
var
  I: Integer;
begin { TArregloArreglosEnteros.GetTexto }
  Result := '[';
  if Length(FArreglosEnteros) > 0 then
  begin
    for I := Low(FArreglosEnteros) to High(FArreglosEnteros) - 1 do
      Result := Result + FArreglosEnteros [I].Texto + ', ';

    Result := Result + FArreglosEnteros [High(FArreglosEnteros)].Texto;
  end;
  Result := Result + ']';
end { TArregloArreglosEnteros.GetTexto };

procedure TArregloArreglosEnteros.SetArreglo(I: Integer; Arreglo: TArregloEnteros);
begin { TArregloArreglosEnteros.SetArreglo }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.SetArreglo: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');
  Assert(Assigned(Arreglo), 'TArregloArreglosEnteros.SetArreglo: Assigned(Arreglo)');

  FreeAndNil(FArreglosEnteros [I]);
  FArreglosEnteros [I] := TArregloEnteros.Create(Arreglo);
end { TArregloArreglosEnteros.SetArreglo };

procedure TArregloArreglosEnteros.SetDimension(ADimension: Integer);
var
  I, DimensionAnterior: Integer;
begin { ArregloArreglosEnteros.SetDimension }
  Assert(Dimension > 0, 'TArregloArreglosEnteros.SetDimension: Dimension > 0');

  DimensionAnterior := Length(FArreglosEnteros);

  if DimensionAnterior > ADimension then
  begin
    for I := DimensionAnterior to High(FArreglosEnteros) do
      FreeAndNil(FArreglosEnteros [I]);
    SetLength(FArreglosEnteros, ADimension);
  end
  else if DimensionAnterior < ADimension then
  begin
    SetLength(FArreglosEnteros, ADimension);

    for I := DimensionAnterior to High(FArreglosEnteros) do
      FArreglosEnteros [I] := nil;
  end;
end { ArregloArreglosEnteros.SetDimension };

procedure TArregloArreglosEnteros.SetEntero(I, J: Integer; Entero: Integer);
begin { TArregloArreglosEnteros.SetEntero }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.SetEntero: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');

  FArreglosEnteros [I] [J] := Entero;
end { TArregloArreglosEnteros.SetEntero };

constructor TArregloArreglosEnteros.Create(ADimension: Integer);
var
  I: Integer;
begin { TArregloArreglosEnteros.Create }
  Assert(ADimension > 0, 'TArregloArreglosEnteros.Create: ADimension > 0');

  SetLength(FArreglosEnteros, ADimension);

  for I := Low(FArreglosEnteros) to High(FArreglosEnteros) do
    FArreglosEnteros [I] := nil;
end { TArregloArreglosEnteros.Create };

constructor TArregloArreglosEnteros.Create(ADimension, DimensionArreglos: Integer);
var
  I: Integer;
begin { TArregloArreglosEnteros.Create }
  Assert(ADimension > 0, 'TArregloArreglosEnteros.Create: ADimension > 0');

  SetLength(FArreglosEnteros, ADimension);

  for I := Low(FArreglosEnteros) to High(FArreglosEnteros) do
    FArreglosEnteros [I] := TArregloEnteros.Create(DimensionArreglos);
end { TArregloArreglosEnteros.Create };

constructor TArregloArreglosEnteros.Create(Lista: TListaArreglosEnteros);
var
  I: Integer;
begin { TArregloArreglosEnteros.Create }
  Assert(Assigned(Lista) and (Lista.Count > 0), 'TArregloArreglosEnteros.Create: Assigned(Lista) and (Lista.Count > 0)');

  SetLength(FArreglosEnteros, Lista.Count);

  for I := 0 to Lista.Count - 1 do
    FArreglosEnteros [I] := Lista [I];
end { TArregloArreglosEnteros.Create };

destructor TArregloArreglosEnteros.Destroy;
var
  I: Integer;
begin { TArregloArreglosEnteros.Destroy }
  try
    for I := Low(FArreglosEnteros) to High(FArreglosEnteros) do
      FreeAndNil(FArreglosEnteros [I]);
    FArreglosEnteros := nil;
  finally
    inherited Destroy;
  end;
end { TArregloArreglosEnteros.Destroy };

function TArregloArreglosEnteros.IndiceDe(Arreglo: TArregloEnteros): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TArregloArreglosEnteros.IndiceDe }
  Assert(Assigned(Arreglo), 'TArregloArreglosEnteros.IndiceDe: Assigned(Arreglo)');

  Result := -1;
  I := Low(FArreglosEnteros);
  Listo := false;
  while (I <= High(FArreglosEnteros)) and (not Listo) do
  begin
    if Arreglo.IgualA(FArreglosEnteros [I]) then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TArregloArreglosEnteros.IndiceDe };

procedure TArregloArreglosEnteros.Intercambiar(I, J: Integer);
var
  Arreglo: TArregloEnteros;
begin { TArregloArreglosEnteros.Intercambiar }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.Intercambiar: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');
  Assert((J >= Low(FArreglosEnteros)) and (J <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.Intercambiar: (J >= Low(FArreglosEnteros)) and (J <= High(FArreglosEnteros))');

  if I <> J then
  begin
    Arreglo := FArreglosEnteros [I];
    FArreglosEnteros [I] := FArreglosEnteros [J];
    FArreglosEnteros [J] := Arreglo;
  end;
end { TArregloArreglosEnteros.Intercambiar };

procedure TArregloArreglosEnteros.Decrementar(I, J: Integer; Entero: Integer);
begin { TArregloArreglosEnteros.Decrementar }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.Decrementar: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');

  FArreglosEnteros [I].Incrementar(J, Entero);
end { TArregloArreglosEnteros.Decrementar };

procedure TArregloArreglosEnteros.Incrementar(I, J: Integer; Entero: Integer);
begin { TArregloArreglosEnteros.Incrementar }
  Assert((I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros)), 'TArregloArreglosEnteros.Incrementar: (I >= Low(FArreglosEnteros)) and (I <= High(FArreglosEnteros))');

  FArreglosEnteros [I].Incrementar(J, Entero);
end { TArregloArreglosEnteros.Incrementar };

procedure TArregloArreglosEnteros.PonerA(Entero: Integer);
var
  I: Integer;
begin { TArregloArreglosEnteros.PonerA }
  for I := Low(FArreglosEnteros) to High(FArreglosEnteros) do
    FArreglosEnteros [I].PonerA(Entero);
end { TArregloArreglosEnteros.PonerA };

end { UnitArregloEnteros }. 
