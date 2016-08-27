{**
@abstract(Matriz de valores con los datos de modelos mixtos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 13, 2005)
Este modulo contiene la definicion de las matrices de valores utilizados como
fuentes de datos para los modelos mixtos.
}
unit UnitMatrizValores;

interface

uses
  Classes;

const
  {** Cantidad inicial de filas de la matriz. }
  CCantidadInicialFilas: Integer = 16;
  {** Cantidad inicial de columnas de la matriz. }
  CCantidadInicialColumnas: Integer = 4;
  {** Cantidad de filas sin datos a arregar durante un acceso fuera de rango. }
  CIncrementoCantidadFilas: Integer = 32;
  {** Cantidad de columnas sin datos a arregar durante un acceso fuera de
      rango. }
  CIncrementoCantidadColumnas: Integer = 1;

type
  {** Matriz de valores (cadenas de caracteres) utilizada como fuente de datos
      para los modelos mixtos. Es basicamente un arreglo de arreglos dinamicos
      de cadenas de catacteres con la posibilidad de redimensionamiento
      automatico al producirse un acceso fuera de rango. Esta caracteristica
      es util durante el proceso de lectura del archivo de configuracion.
      @abstract(Matriz de datos de modelos mixtos.) }
  TMatrizValores = class
  private
    {** Cantidad de columnas del arreglo dinamico. }
    FCantidadColumnas: Integer;
    {** Cantidad de columnas (con datos) de la matriz. }
    FCantidadColumnasConDatos: Integer;
    {** Cantidad de filas del arreglo dinamico. }
    FCantidadFilas: Integer;
    {** Cantidad de filas (con datos) de la matriz. }
    FCantidadFilasConDatos: Integer;
    {** La matriz propiamente dicha. Un arreglo dinamico de arreglos dinamicos
        de cadenas de caracteres. }
    FValores: array of array of String;

    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico de filas. Utilizado para hacer recorridos por filas
        de la matriz.
        @returns(El menor indice del arreglo dinamico de filas.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico de filas. Utilizado para hacer recorridos por filas
        de la matriz.
        @returns(El mayor indice del arreglo dinamico de filas.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(CantidadColumnas). Devuelve la
        cantidad de columnas con datos de la matriz.
        @returns(Cantidad de columnas con datos de la matriz.) }
    function GetCantidadColumnas: Integer;
    {** Metodo de lectura de la propiedad @link(CantidadFilas). Devuelve la
        cantidad de Filas con datos de la matriz.
        @returns(Cantidad de filas con datos de la matriz.) }
    function GetCantidadFilas: Integer;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres de la matriz.
        @returns(Representacion textual de la matriz.) }
    function GetTexto: String;
    {** Metodo de lectura de la propiedad @link(TextoEnFilas). Devuelve una
        representacion como lista de cadena de caracteres (una por fila) de la
        matriz de valores.
        @returns(Representacion textual de la matriz de valores.) }
    function GetTextoEnFilas: TStrings;
    {** Metodo de lectura de la propiedad @link(Valores). Devuelve la cadena
        de caracteres almacenada bajo un indice de fila y columna.
        @param(I Indice del arreglo de filas.)
        @param(J Indice del string.)
        @returns(Cadena de caracteres almacenada bajo el indice de fila I e
        indice de columna J.) }
    function GetValores(I, J: Integer): String;
    {** Metodo de escritura de la propiedad @link(CantidadColumnas). Actualiza
        la cantidad de columnas con datos de la matriz y redimensiona el arreglo
        dinamico de ser necesario.
        @param(ACantidadColumnas Nueva cantidad de columnas con datos de la
        matriz.) }
    procedure SetCantidadColumnas(ACantidadColumnas: Integer);
    {** Metodo de escritura de la propiedad @link(CantidadFilas). Actualiza la
        cantidad de filas con datos de la matriz y redimensiona el arreglo
        dinamico de ser necesario.
        @param(ACantidadFilas Nueva cantidad de filas con datos de la
        matriz.) }
    procedure SetCantidadFilas(ACantidadFilas: Integer);
    {** Metodo de escritura de la propiedad @link(Valores). Actualiza la cadena
        de caracteres almacenada bajo un indice de fila y columna.
        @param(I Indice del arreglo de filas.)
        @param(J Indice del string.)
        @param(Valor Nuevo valor de la cadena de caracteres bajo el indice de
        fila I e indice de columna J.) }
    procedure SetValores(I, J: Integer; Valor: String);
  public
    {** Constructor. La cantidad inicial de filas y columnas se extrae de las
        constantes CCantidadInicialFilas y CCantidadInicialColumnas
        respectivamente. }
    constructor Create; overload;
    {** Constructor.
        @param(ACantidadFilas Cantidad inicial de filas de la matriz.)
        @param(ACantidadColumnas Cantidad inicial de columnas de la matriz.) }
    constructor Create(ACantidadFilas, ACantidadColumnas: Integer); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Menor indice de fila de la matriz. }
    property Bajo: Integer read GetBajo;
    {** Mayor indice de fila de la matriz. }
    property Alto: Integer read GetAlto;
    {** Cadenas de caracteres almacenadas en la matriz. }
    property Valores [I, J: Integer]: String read GetValores write SetValores; default;
    {** Cantidad de filas de la matriz. }
    property CantidadFilas: Integer read GetCantidadFilas write SetCantidadFilas;
    {** Cantidad de columnas de la matriz. }
    property CantidadColumnas: Integer read GetCantidadColumnas write SetCantidadColumnas;
    {** Representacion de cadena de caracteres de la matriz. }
    property Texto: String read GetTexto;
    {** Representacion como lista de cadenas de caracteres (una por fila) de la
        matriz. }
    property TextoEnFilas: TStrings read GetTextoEnFilas;
  end { TMatrizValores };

implementation

// -----------------------------------------------------------------------------
// TMatrizValores
// -----------------------------------------------------------------------------

function TMatrizValores.GetBajo: Integer;
begin { TMatrizValores.GetBajo }
  Result := Low(FValores);
end { TMatrizValores.GetBajo };

function TMatrizValores.GetAlto: Integer;
begin { TMatrizValores.GetAlto }
  Result := FCantidadFilasConDatos - 1;
end { TMatrizValores.GetAlto };

function TMatrizValores.GetCantidadColumnas: Integer;
begin { TMatrizValores.GetCantidadColumnas }
  Result := FCantidadColumnasConDatos;
end { TMatrizValores.GetCantidadColumnas };

function TMatrizValores.GetCantidadFilas: Integer;
begin { TMatrizValores.GetCantidadFilas }
  Result := FCantidadFilasConDatos;
end { TMatrizValores.GetCantidadFilas };

function TMatrizValores.GetTexto: String;
var
  I, J: Integer;
begin { TMatrizValores.GetTexto }
  Result := '[';
  if (FCantidadFilasConDatos > 0) and
     (FCantidadColumnasConDatos > 0) then
  begin
    for I := 0 to FCantidadFilasConDatos - 2 do
    begin
      Result := Result + '[';
      for J := 0 to FCantidadColumnasConDatos - 2 do
        Result := Result + FValores [I, J] + ', ';
      J := FCantidadColumnasConDatos - 1;
      Result := Result + FValores [I, J] + '], ';
    end;
    I := FCantidadFilasConDatos - 1;
    Result := Result + '[';
    for J := 0 to FCantidadColumnasConDatos - 2 do
      Result := Result + FValores [I, J] + ', ';
    J := FCantidadColumnasConDatos - 1;
    Result := Result + FValores [I, J] + ']';
  end;
  Result := Result + ']';
end { TMatrizValores.GetTexto };

function TMatrizValores.GetTextoEnFilas: TStrings;
var
  I, J: Integer;
  String1: String;
begin { TMatrizValores.GetTextoEnFilas }
  Result := TStringList.Create;

  if (FCantidadFilasConDatos > 0) and
     (FCantidadColumnasConDatos > 0) then
  begin
    for I := 0 to FCantidadFilasConDatos - 1 do
    begin
      String1 := '[';
      for J := 0 to FCantidadColumnasConDatos - 2 do
        String1 := String1 + FValores [I, J] + ', ';
      String1 := String1 + FValores [I, FCantidadColumnasConDatos - 1] + ']';

      Result.Add(String1);
    end;
  end;
end { TMatrizValores.GetTextoEnFilas };

function TMatrizValores.GetValores(I, J: Integer): String;
begin { TMatrizValores.GetValores }
  Assert((I >= Low(FValores)) and (I < FCantidadFilasConDatos), 'TMatrizValores.GetValores: (I >= Low(FValores)) and (I < FCantidadFilasConDatos)');
  Assert((J >= Low(FValores [I])) and (J < FCantidadColumnasConDatos), 'TMatrizValores.GetValores: (J >= Low(FValores [I])) and (J < FCantidadColumnasConDatos)');

  Result := FValores [I, J];
end { TMatrizValores.GetValores };

procedure TMatrizValores.SetCantidadColumnas(ACantidadColumnas: Integer);
begin { TMatrizValores.SetCantidadColumnas }
  if FCantidadColumnasConDatos <> ACantidadColumnas then
  begin
    SetLength(FValores, FCantidadFilas, ACantidadColumnas);
    FCantidadColumnas := ACantidadColumnas;
    if FCantidadColumnas < FCantidadColumnasConDatos then
      FCantidadColumnasConDatos := FCantidadColumnas;
  end;
end { TMatrizValores.SetCantidadColumnas };

procedure TMatrizValores.SetCantidadFilas(ACantidadFilas: Integer);
begin { TMatrizValores.SetCantidadFilas }
  if FCantidadFilasConDatos <> ACantidadFilas then
  begin
    SetLength(FValores, ACantidadFilas, FCantidadColumnas);
    FCantidadFilas := ACantidadFilas;
    if FCantidadFilas < FCantidadFilasConDatos then
      FCantidadFilasConDatos := FCantidadFilas;
  end;
end { TMatrizValores.SetCantidadFilas };

procedure TMatrizValores.SetValores(I, J: Integer; Valor: String);
begin { TMatrizValores.SetValores }
  if I >= FCantidadFilasConDatos then
  begin
    if I >= FCantidadFilas then
    begin
      if I >= FCantidadFilas + CIncrementoCantidadFilas then
      begin
        SetLength(FValores, I + 1, FCantidadColumnas);
        FCantidadFilas := I + 1;
        FCantidadFilasConDatos := I + 1;
      end
      else
      begin
        SetLength(FValores, FCantidadFilas + CIncrementoCantidadFilas, FCantidadColumnas);
        FCantidadFilas := FCantidadFilas + CIncrementoCantidadFilas;
        FCantidadFilasConDatos := I + 1;
      end;
    end
    else
    begin
      FCantidadFilasConDatos := I + 1;
    end;
  end;

  if J >= FCantidadColumnasConDatos then
  begin
    if J >= FCantidadColumnas then
    begin
      if J >= FCantidadColumnas + CIncrementoCantidadColumnas then
      begin
        SetLength(FValores, FCantidadFilas, J + 1);
        FCantidadColumnas := J + 1;
        FCantidadColumnasConDatos := J + 1;
      end
      else
      begin
        SetLength(FValores, FCantidadFilas, FCantidadColumnas + CIncrementoCantidadColumnas);
        FCantidadColumnas := FCantidadColumnas + CIncrementoCantidadColumnas;
        FCantidadColumnasConDatos := J + 1;
      end;
    end
    else
    begin
      FCantidadColumnasConDatos := J + 1;
    end;
  end;

  FValores [I, J] := Valor;
end { TMatrizValores.SetValores };

constructor TMatrizValores.Create;
begin { TMatrizValores.Create }
  Create(CCantidadInicialFilas, CCantidadInicialColumnas);
end { TMatrizValores.Create };

constructor TMatrizValores.Create(ACantidadFilas, ACantidadColumnas: Integer);
begin { TMatrizValores.Create }
  SetLength(FValores, ACantidadFilas, ACantidadColumnas);
  FCantidadFilas := ACantidadFilas;
  FCantidadFilasConDatos := ACantidadFilas;
  FCantidadColumnas := ACantidadColumnas;
end { TMatrizValores.Create };

destructor TMatrizValores.Destroy;
begin { TMatrizValores.Destroy }
  FValores := nil;
  inherited Destroy;
end { TMatrizValores.Destroy };

end { UnitMatrizValores }.
