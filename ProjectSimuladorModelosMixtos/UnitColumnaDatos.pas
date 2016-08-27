{**
@abstract(Descriptor del tipo de columna de datos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(March 7, 2005)
@lastmod(March 9, 2005)
Este modulo contiene la definicion de descriptores del tipo de columnas del
simulador de datos de modelos mixtos.
}
unit UnitColumnaDatos;

interface

uses
  Classes, Contnrs, SysUtils,
  UaVector, UnitArregloEnteros, UnitArregloStrings, UnitCalculoSimbolico;

type
  {** Descriptor (generico o abstracto) del tipo de columna en la fuente de
      datos.
      @abstract(Descriptor de columna de datos.) }
  TColumnaDatos = class
  protected
    {** Nombre de la columna }
    FNombre: String;

    {** Metodo de lectura de la propiedad Texto. Genera una representacion de
        cadena de caracteres del descriptor de columna. Este metodo debe ser
        implementado por cada subclase para especializar la representacion de
        acuerdo al tipo particular.
        @returns(Representacion como cadena de caracteres del descriptor.) }
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
  private
    {** Expresion generadora de datos. }
    FExpresion: TCsExpresionEntera;
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(AExpresion Expresion entera generadora de datos.) }
    constructor Create(const ANombre: String; const AExpresion: TCsExpresionEntera);
    {** Expresion generadora de datos. }
    property Expresion: TCsExpresionEntera read FExpresion write FExpresion;
  end { TColumnaDatosEnteros };

  {** Descriptor de columna de numeros reales.
      @abstract(Descriptor de columna de numeros reales.) }
  TColumnaDatosReales = class(TColumnaDatos)
  private
    {** Expresion generadora de datos. }
    FExpresion: TCsExpresion;
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(AExpresion Expresion real generadora de datos.) }
    constructor Create(const ANombre: String; const AExpresion: TCsExpresion);
    {** Expresion generadora de datos. }
    property Expresion: TCsExpresion read FExpresion write FExpresion;
  end { TColumnaDatosReales };

  {** Descriptor de columna de valores categoricos. Puede tratarse de cualquier
      tipo de datos, pero los mismos solo son interpretados como cadenas de
      caracteres.
      @abstract(Descriptor de columna de valores categoricos.) }
  TColumnaDatosCategoricos = class(TColumnaDatos)
  private
    {** Niveles de la columna. }
    FNiveles: TArregloStrings;
    {** Cantidad de observaciones por nivel. }
    FCantidadObservacionesNivel: TArregloEnteros;
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(ANiveles Lista de identificadores de posibles niveles.)
        @param(ACantidadObservacionesNivel Arreglo con la cantidad de
        observaciones por nivel a generar.) }
    constructor Create(const ANombre: String; const ANiveles: TStrings; const ACantidadObservacionesNivel: TArregloEnteros); overload;
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(ANiveles Arreglo de identificadores de posibles niveles.)
        @param(ACantidadObservacionesNivel Arreglo con la cantidad de
        observaciones por nivel a generar.) }
    constructor Create(const ANombre: String; const ANiveles: TArregloStrings; const ACantidadObservacionesNivel: TArregloEnteros); overload;
    {** Niveles de la columna. }
    property Niveles: TArregloStrings read FNiveles write FNiveles;
    {** Cantidad de observaciones generadas por nivel. }
    property CantidadObservacionesNivel: TArregloEnteros read FCantidadObservacionesNivel write FCantidadObservacionesNivel;
  end { TColumnaDatosCategoricos };

  {** Descritor de columna de datos cuyos valores surgen de un vector
      suministrado por el usuario.
      @abstract(Columna de datos cuyos valores surgen de un vector.) }
  TColumnaVectorDatos = class(TColumnaDatos)
  protected
    {** Metodo de lectura de la propiedad @link(Valores). Obtiene el valor,
    como cadena de caracteres, de un dato almacenado en el vector de datos de
    la columna.) }
    function GetValor(const Indice: Integer): String; virtual; abstract;
  public
    {** Valor (como cadena de caracteres) de una entrada en el vector de
    datos. }
    property Valores [const Indice: Integer]: String read GetValor;
  end { TColumnaVectorDatos };

  {** Descriptor de columna de numeros reales con parametro de vector de
      valores.
      @abstract(Descriptor de columna de numeros reales con parametro de vector
      de valores.) }
  TColumnaVectorDatosReales = class(TColumnaVectorDatos)
  private
    {** Vector de datos. }
    FDatos: TUaVector;
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TColumnaVectorDatos.GetValor). }
    function GetValor(const Indice: Integer): String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(ADatos Vector conteniendo los datos.) }
    constructor Create(const ANombre: String; const ADatos: TUaVector);
    {** Vector de datos. }
    property Datos: TUaVector read FDatos write FDatos;
  end { TColumnaDatosReales };

  {** Descriptor de columna de numeros enteros con parametro de vector de
      valores.
      @abstract(Descriptor de columna de numeros enteros con parametro de
      vector de valores.) }
  TColumnaVectorDatosEnteros = class(TColumnaVectorDatos)
  private
    {** Vector de datos. }
    FDatos: TArregloEnteros;
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TColumnaVectorDatos.GetValor). }
    function GetValor(const Indice: Integer): String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(ADatos Vector conteniendo los datos.) }
    constructor Create(const ANombre: String; const ADatos: TArregloEnteros);
    {** Vector de datos. }
    property Datos: TArregloEnteros read FDatos write FDatos;
  end { TColumnaVectorDatosEnteros };

  {** Descriptor de columna de valores categoricos con parametro de vector de
      valores.
      @abstract(Descriptor de columna de valores categoricos con parametro de
      vector de valores.) }
  TColumnaVectorDatosCategoricos = class(TColumnaVectorDatos)
  private
    {** Vector de datos. }
    FDatos: TArregloStrings;
  protected
    {** Ver @link(TColumnaDatos.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TColumnaVectorDatos.GetValor). }
    function GetValor(const Indice: Integer): String; override;
  public
    {** Constructor.
        @param(ANombre Nombre de la columna.)
        @param(ADatos Vector conteniendo los datos.) }
    constructor Create(const ANombre: String; const ADatos: TArregloStrings);
    {** Vector de datos. }
    property Datos: TArregloStrings read FDatos write FDatos;
  end { TColumnaVectorDatosCategoricos };

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
  end;

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
  Result := FNombre + ':Enteros(' + FExpresion.Texto + ')';
end { TColumnaDatosEnteros.GetTexto };

constructor TColumnaDatosEnteros.Create(const ANombre: String; const AExpresion: TCsExpresionEntera);
begin { TColumnaDatosEnteros.Create }
  inherited Create(ANombre);
  
  Assert(Assigned(AExpresion), 'TColumnaDatosEnteros.Create: Assigned(AExpresion)');
  FExpresion := AExpresion;
end { TColumnaDatosEnteros.Create };

// -----------------------------------------------------------------------------
// TColumnaReales
// -----------------------------------------------------------------------------

function TColumnaDatosReales.GetTexto: String;
begin { TColumnaDatosReales.GetTexto }
  Result := FNombre + ':Reales(' + FExpresion.Texto + ')';
end { TColumnaDatosReales.GetTexto };

constructor TColumnaDatosReales.Create(const ANombre: String; const AExpresion: TCsExpresion);
begin { TColumnaDatosReales.Create }
  inherited Create(ANombre);
  
  Assert(Assigned(AExpresion), 'TColumnaDatosReales.Create: Assigned(AExpresion)');
  FExpresion := AExpresion;
end { TColumnaDatosReales.Create };

// -----------------------------------------------------------------------------
// TColumnaDatosCategoricos
// -----------------------------------------------------------------------------

function TColumnaDatosCategoricos.GetTexto: String;
var
  I: Integer;
begin { TColumnaDatosCategoricos.GetTexto }
  Result := FNombre + ':Categoricos ' + FNiveles.Texto + ' ' + FCantidadObservacionesNivel.Texto;
end { TColumnaDatosCategoricos.GetTexto };

constructor TColumnaDatosCategoricos.Create(const ANombre: String; const ANiveles: TStrings; const ACantidadObservacionesNivel: TArregloEnteros);
begin { TColumnaDatosCategoricos.Create }
  inherited Create(ANombre);
  Create(ANombre, TArregloStrings.Create(ANiveles), ACantidadObservacionesNivel);
end { TColumnaDatosCategoricos.Create };

constructor TColumnaDatosCategoricos.Create(const ANombre: String; const ANiveles: TArregloStrings; const ACantidadObservacionesNivel: TArregloEnteros);
begin { TColumnaDatosCategoricos.Create }
  inherited Create(ANombre);

  Assert(Assigned(ANiveles), 'TColumnaDatosCategoricos.Create: Assigned(ANiveles)');
  Assert(ANiveles.Dimension > 0, 'TColumnaDatosCategoricos.Create: ANiveles.Dimension > 0');

  FNiveles := ANiveles;

  if not Assigned(ACantidadObservacionesNivel) then
  begin
    FCantidadObservacionesNivel := TArregloEnteros.Create(FNiveles.Dimension);
    FCantidadObservacionesNivel.PonerA(1);
  end
  else if ACantidadObservacionesNivel.Dimension = 0 then
  begin
    FCantidadObservacionesNivel.Dimension := FNiveles.Dimension;
    FCantidadObservacionesNivel.PonerA(1);
  end
  else
    FCantidadObservacionesNivel := ACantidadObservacionesNivel;
end { TColumnaDatosCategoricos.Create };

// -----------------------------------------------------------------------------
// TColumnaVectorDatosReales
// -----------------------------------------------------------------------------

function TColumnaVectorDatosReales.GetTexto: String;
begin { TColumnaVectorDatosReales.GetTexto }
  Result := FNombre + ':Reales<' + FDatos.Texto + '>';
end { TColumnaVectorDatosReales.GetTexto };

function TColumnaVectorDatosReales.GetValor(const Indice: Integer): String;
begin { TColumnaVectorDatosReales.GetValor }
  Result := FloatToStr(FDatos [Abs(Indice) mod FDatos.Dimension]); 
end { TColumnaVectorDatosReales.GetValor };

constructor TColumnaVectorDatosReales.Create(const ANombre: String; const ADatos: TUaVector);
begin { TColumnaVectorDatosReales.Create }
  inherited Create(ANombre);

  Assert(Assigned(ADatos), 'TColumnaVectorDatosReales.Create: Assigned(ADatos)');
  Assert(ADatos.Dimension > 0, 'TColumnaVectorDatosReales.Create: ADatos.Dimension > 0');

  FDatos := ADatos;
end { TColumnaVectorDatosReales.Create };

// -----------------------------------------------------------------------------
// TColumnaVectorDatosEnteros
// -----------------------------------------------------------------------------

function TColumnaVectorDatosEnteros.GetTexto: String;
begin { TColumnaVectorDatosEnteros.GetTexto }
  Result := FNombre + ':Enteros<' + FDatos.Texto + '>';
end { TColumnaVectorDatosEnteros.GetTexto };

function TColumnaVectorDatosEnteros.GetValor(const Indice: Integer): String;
begin { TColumnaVectorDatosEnteros.GetValor }
  Result := IntToStr(FDatos [Abs(Indice) mod FDatos.Dimension]);
end { TColumnaVectorDatosEnteros.GetValor };

constructor TColumnaVectorDatosEnteros.Create(const ANombre: String; const ADatos: TArregloEnteros);
begin { TColumnaVectorDatosEnteros.Create }
  inherited Create(ANombre);

  Assert(Assigned(ADatos), 'TColumnaVectorDatosEnteros.Create: Assigned(ADatos)');
  Assert(ADatos.Dimension > 0, 'TColumnaVectorDatosEnteros.Create: ADatos.Dimension > 0');

  FDatos := ADatos;
end { TColumnaVectorDatosEnteros.Create };

// -----------------------------------------------------------------------------
// TColumnaVectorDatosCategoricos
// -----------------------------------------------------------------------------

function TColumnaVectorDatosCategoricos.GetTexto: String;
begin { TColumnaVectorDatosCategoricos.GetTexto }
  Result := FNombre + ':Categoricos<' + FDatos.Texto + '>';
end { TColumnaVectorDatosCategoricos.GetTexto };

function TColumnaVectorDatosCategoricos.GetValor(const Indice: Integer): String;
begin { TColumnaVectorDatosCategoricos.GetValor }
  Result := FDatos [Abs(Indice) mod FDatos.Dimension];
end { TColumnaVectorDatosCategoricos.GetValor };

constructor TColumnaVectorDatosCategoricos.Create(const ANombre: String; const ADatos: TArregloStrings);
begin { TColumnaVectorDatosCategoricos.Create }
  inherited Create(ANombre);

  Assert(Assigned(ADatos), 'TColumnaVectorDatosCategoricos.Create: Assigned(ADatos)');
  Assert(ADatos.Dimension > 0, 'TColumnaVectorDatosCategoricos.Create: ADatos.Dimension > 0');

  FDatos := ADatos;
end { TColumnaVectorDatosCategoricos.Create };

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
