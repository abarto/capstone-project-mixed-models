{**
@abstract(Matriz de numeros reales y metodos asociados.)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 29, 2004)
@lastmod(March 22, 2005)
Este modulo contiene provee matrices de numeros reales y subrutinas asociadas.
}
unit UaMatriz;

interface

uses
  Classes,
  UaComun, UaConstantes, UaMiscelanea, UaVector;

const
  {** Constante de dimension (cantidad de elementos) maxima de matrices. }
  CUaDimensionMaximaMatrices: Integer = MaxInt div 16;

type
  {** Matriz de numeros reales (Ver @link(UaComun.TUaReal).)
      @abstract(Matriz de numeros reales.) }
  TUaMatriz = class
  private
    {** Cantidad de columnas de la matriz. }
    FCantidadColumnas: Integer;
    {** Cantidad de columnas de la matriz. }
    FCantidadFilas: Integer;
    {** Arreglo que contiene los elementos de la matriz. }
    FDatos: PUaArregloArregloReal;
    {** Metodo de lectura de la propiedad @link(Columna). Devuelve un vector
        conteniendo los elementos de la columna indicada.
        @param(Indice Indice de la columna a acceder.)
        @returns(Vector conteniendo la columna indicada.) }
    function GetColumna(const Indice: Integer): TUaVector;
    {** Metodo de lectura de la propiedad @link(Diagonal). Devuelve un vector
        conteniendo los elementos de la diagonal de la matriz.
        @returns(Vector conteniendo la diagonal.) }
    function GetDiagonal: TUaVector;
    {** Metodo de lectura de la propiedad @link(Elemento). Devuelve el numero
        real almacenado bajo un indice de fila y columna.
        @param(IndiceFila Indice de la fila que contiene al elemento.)
        @param(IndiceColumna Indice de la columna que contiene al elemento.)
        @returns(Numero real bajo la fila y columnas indicadas.) }
    function GetElemento(const IndiceFila, IndiceColumna: Integer): TUaReal;
    {** Metodo de lectura de la propiedad @link(Fila). Devuelve un vector
        conteniendo los elementos de la fila indicada.
        @param(Indice Indice de la fila a acceder.)
        @returns(Vector conteniendo la fila indicada.) }
    function GetFila(const Indice: Integer): TUaVector;
    {** Metodo de lectura de la propiedad @link(Texto). Devuelve una
        representacion de cadena de caracteres de la matriz.
        @returns(Representacion textual de la matriz.) }
    function GetTexto: String;
    {** Metodo de lectura de la propiedad @link(TextoEnFilas). Devuelve una
        representacion como lista de cadena de caracteres (una por fila) de la
        matriz.
        @returns(Representacion textual de la matriz.) }
    function GetTextoEnFilas: TStrings;
    {** Metodo de escritura de la propiedad @link(CantidadColumnas).
        Redimensiona la matriz incrementando o disminuyendo la cantidad de
        columnas de acuerdo a la cantidad indicada.
        @param(ACantidadColumnas Nueva cantidad de columnas deseada.) }
    procedure SetCantidadColumnas(const ACantidadColumnas: Integer);
    {** Metodo de escritura de la propiedad @link(CantidadFilas). Redimensiona
        la matriz incrementando o disminuyendo la cantidad de filas de
        acuerdo a la cantidad indicada.
        @param(ACantidadFilas Nueva cantidad de filas deseada.) }
    procedure SetCantidadFilas(const ACantidadFilas: Integer);
    {** Metodo de escritura de la propiedad @link(Columna). Asigna los
        elementos de una columna con los elementos de un vector. Solo pueden
        asignarse vectores cuya dimension corresponda a la cantidad de filas de
        la matriz.
        @param(Indice Indice de la columna a actualizar.)
        @param(Columna Vector que contiene los nuevos elementos de la
        columna.) }
    procedure SetColumna(const Indice: Integer; const Columna: TUaVector);
    {** Metodo de escritura de la propiedad @link(Diagonal). Asigna los
        elementos de la diagonal con los elementos de un vector. Solo pueden
        asignarse vectores cuya dimension corresponda al minimo entre la
        cantidad de filas y columnas de la matriz.
        @param(Vector Vector que contiene los nuevos elementos de la
        diagonal.) }
    procedure SetDiagonal(const Vector: TUaVector);
    {** Metodo de escritura de la propiedad @link(Elemento). Asigna un nuevo
        valor a un elemento almacenado bajo un indice de fila y columna.
        @param(IndiceFila Indice de la fila que contiene al elemento.)
        @param(IndiceColumna Indice de la columna que contiene al elemento.)
        @param(AReal Nuevo valor para el elemento.) }
    procedure SetElemento(const IndiceFila, IndiceColumna: Integer; const AReal: TUaReal);
    {** Metodo de escritura de la propiedad @link(Fila). Asigna los elementos
        de una fila con los elementos de un vector. Solo pueden asignarse
        vectores cuya dimension corresponda a la cantidad de columnas de la
        matriz.
        @param(Indice Indice de la fila a actualizar.)
        @param(Fila Vector que contiene los nuevos elementos de la fila.) }
    procedure SetFila(const Indice: Integer; const Fila: TUaVector);
    {** Verifica si el parametro es un indice de columnas valido. Si el indice
        no es valido, se genera una @link(EUaErrorMatriz).
        @param(Indice Indice de columna a verificar.)
        @param(Llamador Identificador utilizado para marcar el metodo que
        origina la verificacion.) }
    procedure VerificarRangoColumna(const Indice: Integer; const Llamador: String = 'TUaMatriz.VerificarRangoColumna');
    {** Verifica si el parametro es un indice de filas valido. Si el indice
        no es valido, se genera una @link(EUaErrorMatriz).
        @param(Indice Indice de fila a verificar.)
        @param(Llamador Identificador utilizado para marcar el metodo que
        origina la verificacion.) }
    procedure VerificarRangoFila(const Indice: Integer; const Llamador: String = 'TUaMatriz.VerificarRangoFila');
    {** Verifica si los indices de fila y columna ingresados son validos. Si el
        par de indices no es valido, se genera una @link(EUaErrorMatriz).
        @param(IndiceFila Indice de fila a verificar.)
        @param(IndiceColumna Indice de columna a verificar.)
        @param(Llamador Identificador utilizado para marcar el metodo que
        origina la verificacion.) }
    procedure VerificarRangos(const IndiceFila, IndiceColumna: Integer; const Llamador: String = 'TUaMatriz.VerificarRangos');
  public
    {** Asigna los elementos de la matriz de acuerdo a los elementos del
        parametro. Solo pueden asignarse matrices de igual dimension.
        @param(Matriz Matriz de la cual extraer los nuevos valores para los
        elementos.)
        @returns(Referencia a Self.) }
    function Asignar(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Asigna un numero real a todos los elementos de la matriz.
        @param(AReal Nuevo valor para los elementos de la matriz.)
        @returns(Referencia a Self.) }
    function Asignar(const AReal: TUaReal): TUaMatriz; overload;
    {** Asigna ceros a todos los elemenentos de la matriz.
        @returns(Referencia a Self.) }
    function AsignarCero: TUaMatriz;
    {** Asigna un valor a todos los elementos de una columna.
        @param(Indice Indice de la columna que contiene los valores a asignar.)
        @param(AReal Nuevo valor para los elementos de la columna.)
        @returns(Referencia a Self.) }
    function AsignarColumna(const Indice: Integer; const AReal: TUaReal): TUaMatriz;
    {** Asigna un valor a todos los elementos de una fila.
        @param(Indice Indice de la fila que contiene los valores a asignar.)
        @param(AReal Nuevo valor para los elementos de la fila.)
        @returns(Referencia a Self.) }
    function AsignarFila(const Indice: Integer; const AReal: TUaReal): TUaMatriz;
    {** Asigna a la matriz como la matriz identidad con las dimensiones
        correspondientes.
        @returns(Referencia a Self.) }
    function AsignarIdentidad: TUaMatriz;
    {** Asigna los elementos de la matriz referenciada desde un indice de fila
        y columna con los elementos de una matriz.
        @param(IndiceFila Indice de la fila donde comenzar a asignar.)
        @param(IndiceColumna Indice de la columna donde comenzar a asignar.)
        @param(Matriz Matriz que contiene los elementos a asignar.)
        @returns(Referencia a Self.) }
    function AsignarSubMatriz(const IndiceFila, IndiceColumna: Integer; const Matriz: TUaMatriz): TUaMatriz;
    {** Genera una copia por valor de la matriz.
        @returns(Copia de la matriz.) }
    function Copia: TUaMatriz;
    {** Copia la matriz parametro en la matriz referenciada destruyendo el
        contenido anterior. Se redimensiona la matriz de ser necesario. }
    function Copiar(const Matriz: TUaMatriz): TUaMatriz;
    {** Constructor.
        @param(ACantidadFilas Cantidad inicial de filas.)
        @param(ACantidadColumnas Cantidad inicial de columnas.) }
    constructor Create(const ACantidadFilas, ACantidadColumnas: Integer); overload;
    {** Constructor.
        @param(ACantidadFilas Cantidad inicial de filas.)
        @param(ACantidadColumnas Cantidad inicial de columnas.)
        @param(AReal Valor inicial para los elementos de la matriz.) }
    constructor Create(const ACantidadFilas, ACantidadColumnas: Integer; const AReal: TUaReal); overload;
    {** Constructor. Esta version del constructor genera instancias de la
        matriz como vectores, o sea, matrices de dimension Nx1 o 1xN, de
        acuerdo a los parametros.
        @param(Dimension Dimension inicial de la matriz.)
        @param(Transpuesta Indica si se debe generar una matriz de dimension
        Dimension x 1 o 1 x Dimension.) }
    constructor Create(const Dimension: Integer; const Transpuesta: Boolean = true); overload;
    {** Construcor. Pero con la posibilidad de asignar un valor inicial a los
        elementos de la matriz.
        @param(Dimension Dimension inicial de la matriz.)
        @param(AReal Valor inicial de los elementos de la matriz.)
        @param(Transpuesta Indica si se debe generar una matriz de dimension
        Dimension x 1 o 1 x Dimension.) }
    constructor Create(const Dimension: Integer; const AReal: TUaReal; const Transpuesta: Boolean = true); overload;
    {** Constructor. Esta version del constructor genera un instancia que es
        una copia por valor del parametro.
        @param(Matriz Matriz a copiar.) }
    constructor Create(const Matriz: TUaMatriz); overload;
    {** Constructor. Esta version del constructor transforma un vector en una
        matriz de las dimensiones correspondientes. Se puede indicar si el
        vector parametro debe tomarse como vector fila o vector columnas.
        @param(Vector Vector de base a copiar.)
        @param(Tanspuesta Indica si el vector debe tomarse como vector columna o
        fila.) }
    constructor Create(const Vector: TUaVector; const Transpuesta: Boolean = true); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Calcula el determinante de la matriz.
        @returns(Determinante de la matriz.) }
    function Determinante: TUaReal;
    {** Almacena la diagonal principal de la matriz en el vector parametro.
        @param(Vector Vector donde deseamos almacenar la diagonal principal de
        la matriz.)
        @returns(Referencia a Self.) }
    function DiagonalEn(var Vector: TUaVector): TUaMatriz;
    {** Intercambia dos columnas de la matriz.
        @param(IndicePrimeraColumna Indice de la primera columna.)
        @param(IndiceSegundaColumna Indice de la segunda columna.)
        @returns(Referencia a Self.) }
    function IntercambiarColumnas(const IndicePrimeraColumna, IndiceSegundaColumna: Integer): TUaMatriz;
    {** Intercambia dos elementos de la matriz.
        @param(IndiceFilaPrimerElemento Indice de fila del primer elemento.)
        @param(IndiceColumnaPrimerElemento Indice de columna del primer elemento.)
        @param(IndiceFilaSegundaElemento Indice de fila del segundo elemento.)
        @param(IndiceColumnaSegundaElemento Indice de columna del segundo elemento.)
        @returns(Referencia a Self.) }
    function IntercambiarElementos(const IndiceFilaPrimerElemento, IndiceColumnaPrimerElemento, IndiceFilaSegundoElemento, IndiceColumnaSegundoElemento: Integer): TUaMatriz;
    {** Intercambia dos filas de la matriz.
        @param(IndicePrimeraFila Indice de la primera fila.)
        @param(IndiceSegundaFila Indice de la segunda fila.)
        @returns(Referencia a Self.) }
    function IntercambiarFilas(const IndicePrimeraFila, IndiceSegundaFila: Integer): TUaMatriz;
    {** Basado en Numerical Recipes in C - 2nd. Edition.
        Calcula la inversa de la matriz.
        @returns(Inversa de la matriz.) }
    function Inversa: TUaMatriz;
    {** Basado en Numerical Recipes in C - 2nd. Edition.
        Invierte la matriz.
        @returns(Referencia a Self.) }
    function Invertir: TUaMatriz;
    {** Multiplica a la matriz por un escalar.
        @param(AReal Escalar a multiplicar a la matriz.)
        @returns(Referencia a Self.) }
    function Multiplicar(const AReal: TUaReal): TUaMatriz; overload;
    {** Multiplica a derecha de la matriz referenciada por la matriz parametro.
        Solo pueden multiplicarse matrices cuya cantidad de filas coincida con
        la cantidad de columnas de la matriz referenciada.
        @param(Matriz Matriz a multiplicar.)
        @returns(Referencia a Self.) }
    function Multiplicar(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Calcula la norma de la matriz.
        @returns(Norma de la matriz.) }
    function Norma: TUaReal;
    {** Multiplica a izquierda de la matriz referenciada por la matriz parametro.
        Solo pueden multiplicarse matrices cuya cantidad de columnas coincida con
        la cantidad de filas de la matriz referenciada.
        @param(Matriz Matriz a multiplicar.)
        @returns(Referencia a Self.) }
    function PreMultiplicar(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Calcula el producto a izquierda de la matriz referenciada por la matriz
        parametro. Solo pueden multiplicarse matrices cuya cantidad de columnas
        coincida con la cantidad de filasde la matriz referenciada.
        @param(Producto a izquierda de la matriz referenciada con el parametro.)
        @returns(Referencia a Self.) }
    function PreProducto(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Calcula el producto de la matriz por un escalar.
        @param(AReal Escalar a multiplicar a la matriz.)
        @returns(Producto de la matriz por el parametro.) }
    function Producto(const AReal: TUaReal): TUaMatriz; overload;
    {** Calcula el producto a derecha de la matriz referenciada por la matriz
        parametro. Solo pueden multiplicarse matrices cuya cantidad de filas
        coincida con la cantidad de columnas de la matriz referenciada.
        @param(Matriz Matriz a multiplicar.)
        @returns(Producto a derecha de la matriz referenciada con el
        parametro.) }
    function Producto(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Calcula el rango de la matriz utilizando su descomposicion de valores
        signulares (Ver @link(UaMiscelanea.svdcmp)).
        @returns(El rango de la matriz.) }
    function Rango: TUaEntero;
    {** Redimensiona, de ser necesario, la matriz a las dimensiones indicadas.
        @param(ACantidadFilas Nueva cantidad de filas de la matriz.)
        @param(ACantidadColumnas Nueva cantidad de columnas de la matriz.) }
    procedure Redimensionar(const ACantidadFilas, ACantidadColumnas: Integer);
    {** Calcula la matriz resultante de restar a todos los elementos de la
        matriz un escalar.
        @param(AReal Escalar a restar a la matriz.)
        @returns(Resta de un escalar a la matriz.) }
    function Resta(const AReal: TUaReal): TUaMatriz; overload;
    {** Calcula la resta de la matriz referenciada con la matriz parametro.
        Solo pueden restarse matrices de igual dimension.
        @param(Matriz Matriz a restar.)
        @returns(Resta de la matriz parametro a la referenciada.) }
    function Resta(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Resta a todos los elementos de la matriz un escalar.
        @param(AReal Escalar a restar a la matriz.)
        @returns(Referencia a Self.) }
    function Restar(const AReal: TUaReal): TUaMatriz; overload;
    {** Resta a la matriz referenciada la matriz parametro. Solo
        pueden restarse matrices de igual dimension.
        @param(Matriz Matriz a restar.)
        @returns(Referencia a Self.) }
    function Restar(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Construye una submatriz con los elementos que se encuentran entre los
        indices parametro.
        @param(IndiceFilaInical Indice de fila inicial.)
        @param(IndiceColumnaInicial Indice de columna inicial.)
        @param(IndiceFilaFinal Indice de fila final.)
        @param(IndiceColumnaFinal Indice de columna final.)
        @returns(Submatriz de la matriz referenciada.) }
    function SubMatriz(const IndiceFilaInicial, IndiceColumnaInicial, IndiceFilaFinal, IndiceColumnaFinal: Integer): TUaMatriz;
    {** Construye una submatriz con los elementos que se encuentran entre los
        indices parametro y almacena el resultado el la matriz parametro.
        @param(Matriz Matriz que almacenara la submatriz.)
        @param(IndiceFilaInical Indice de fila inicial.)
        @param(IndiceColumnaInicial Indice de columna inicial.)
        @param(IndiceFilaFinal Indice de fila final.)
        @param(IndiceColumnaFinal Indice de columna final.)
        @returns(Referencia a Self.) }
    function SubMatrizEn(var Matriz: TUaMatriz; const IndiceFilaInicial, IndiceColumnaInicial, IndiceFilaFinal, IndiceColumnaFinal: Integer; const IndiceFilaDestino: Integer = 1; const IndiceColumnaDestino: Integer = 1): TUaMatriz;
    {** Calcula la matriz resultante de sumar a todos los elementos de la
        matriz un escalar.
        @param(AReal Escalar a sumar.)
        @returns(Suma de la matriz referenciada con el escalar.) }
    function Suma(const AReal: TUaReal): TUaMatriz; overload;
    {** Calcula la suma de la matriz referenciada con la matriz parametro.
        Solo pueden sumarse matrices de igual dimension.
        @param(Matriz Matriz a sumar.)
        @returns(Suma de la matriz parametro a la referenciada.) }
    function Suma(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Suma a todos los elementos de la matriz un escalar.
        @param(AReal Escalar a sumar.)
        @returns(Referencia a Self.) }
    function Sumar(const AReal: TUaReal): TUaMatriz; overload;
    {** Suma a la matriz referenciada la matriz parametro. Solo
        pueden sumarse matrices de igual dimension.
        @param(Matriz Matriz a sumar.)
        @returns(Referencia a Self.) }
    function Sumar(const Matriz: TUaMatriz): TUaMatriz; overload;
    {** Transpone la matriz.
        @returns(Referencia a Self.) }
    function Transponer: TUaMatriz;
    {** Calcula la matriz transpuesta.
        @returns(Matriz transpuesta.) }
    function Transpuesta: TUaMatriz;
    {** Calcula la traza de la matriz.
        @returns(Traza de la matriz.) }
    function Traza: TUaReal;
    {** Cantidad de filas de columnas de la matriz. }
    property CantidadColumnas: Integer read FCantidadColumnas write SetCantidadColumnas;
    {** Cantidad de filas de filas de la matriz. }
    property CantidadFilas: Integer read FCantidadFilas write SetCantidadFilas;
    {** Arreglo con las columnas (como vectores) de la matriz. }
    property Columna [const Indice: Integer]: TUaVector read GetColumna write SetColumna;
    {** Puntero al contenedor de los elementos. }
    property Datos:  PUaArregloArregloReal read FDatos;
    {** Diagonal (como vector) de la matriz. }
    property Diagonal: TUaVector read GetDiagonal write SetDiagonal;
    {** Arreglo bidimensional con los elementos de la matriz. }
    property Elemento [const IndiceFila, IndiceColumna: Integer]: TUaReal read GetElemento write SetElemento; default;
    {** Arreglo con las filas (como vectores) de la matriz. }
    property Fila [const Indice: Integer]: TUaVector read GetFila write SetFila;
    {** Representacion como cadena de caracteres de la matriz. }
    property Texto: String read GetTexto;
    {** Representacion como lista de cadenas de caracteres (una por fila) de la
    matriz .}
    property TextoEnFilas: TStrings read GetTextoEnFilas;
  end;

  {** Excepcion relacionada a errores en operaciones de matrices.
      @abstract(Excepcion relacionada a operaciones de matrices.) }
  EUaErrorMatriz = class(EUaError);

{** Construye una matriz identidad de las dimensiones especificadas.
    @param(CantidadFilas Cantidad de filas de la matriz.)
    @param(CantidadColumnas Cantidad de columnas de la matriz.)
    @returns(Matriz identidad con las dimensiones indicadas.) }
function UaMatrizIdentidad(const CantidadFilas, CantidadColumnas: Integer): TUaMatriz; overload;
{** Construye una matriz identidad cuadrada, con la cantidad de filas
    especificadas.
    @param(CantidadFilas Cantidad de filas de la matriz.)
    @returns(Matriz indentidad cuadrada con la cantidad de filas indicadas.) }
function UaMatrizIdentidad(const CantidadFilas: Integer): TUaMatriz; overload;

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TUaMatriz
// -----------------------------------------------------------------------------

function TUaMatriz.GetColumna(const Indice: Integer): TUaVector;
var
  I: Integer;
begin { TUaMatriz.GetColumna }
  VerificarRangoColumna(Indice, 'TUaMatriz.GetColumna');

  Result := TUaVector.Create(FCantidadFilas);
  for I := 1 to FCantidadFilas do
    Result.Datos^ [I] := FDatos^ [I]^ [Indice];
end { TUaMatriz.GetColumna };

function TUaMatriz.GetDiagonal: TUaVector;
var
  I: Integer;
begin { TUaMatriz.GetDiagonal }
  if FCantidadFilas = FCantidadColumnas then
  begin
    Result := TUaVector.Create(FCantidadFilas);
    for I := 1 to FCantidadFilas do
      Result.Datos^ [I] := FDatos^ [I]^ [I];
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Diagonal: La matriz no es cuadrada.');
end { TUaMatriz.GetDiagonal };

function TUaMatriz.GetElemento(const IndiceFila, IndiceColumna: Integer): TUaReal;
begin { TUaMatriz.GetElemento }
  VerificarRangos(IndiceFila, IndiceColumna, 'TUaMatriz.GetElemento');
  Result := FDatos^ [IndiceFila]^ [IndiceColumna];
end { TUaMatriz.GetElemento }; 

function TUaMatriz.GetFila(const Indice: Integer): TUaVector;
var
  I: Integer;
begin { TUaMatriz.GetFila }
  VerificarRangoFila(Indice, 'TUaMatriz.GetFila');

  Result := TUaVector.Create(FCantidadColumnas);
  for I := 1 to FCantidadColumnas do
    Result.Datos^ [I] := FDatos^ [Indice]^ [I];
end; { TUaMatriz.GetFila }

function TUaMatriz.GetTexto: String;
var
  I, J: Integer;
begin { TUaMatriz.GetTexto }
  Result := '[';
  for I := 1 to FCantidadFilas  - 1 do
  begin
    Result := Result + '[';
    for J := 1 to FCantidadColumnas - 1 do
      Result := Result + FloatToStr(FDatos^ [I]^ [J]) + ', ';
    J := FCantidadColumnas;
    Result := Result + FloatToStr(FDatos^ [I]^ [J]) + '], ';
  end;
  I := FCantidadFilas;
  Result := Result + '[';
  for J := 1 to FCantidadColumnas - 1 do
    Result := Result + FloatToStr(FDatos^ [I]^ [J]) + ', ';
  J := FCantidadColumnas;
  Result := Result + FloatToStr(FDatos^ [I]^ [J]) + ']]';
end { TUaMatriz.GetTexto };

function TUaMatriz.GetTextoEnFilas: TStrings;
var
  S: String;
  I, J: Integer;
begin { TUaMatriz.GetTextoEnFilas }
  Result := TStringList.Create;
  for I := 1 to FCantidadFilas do
  begin
    S := '[';
    for J := 1 to FCantidadColumnas - 1 do
      S := S + FloatToStr(FDatos^ [I]^ [J]) + ', ';
    J := FCantidadColumnas;
    S := S + FloatToStr(FDatos^ [I]^ [J]) + ']';
    Result.Add(S);
  end;
end { TUaMatriz.GetTextoEnFilas };


procedure TUaMatriz.SetCantidadColumnas(const ACantidadColumnas: Integer);
begin { TUaMatriz.SetCantidadColumnas }
  Redimensionar(FCantidadFilas, ACantidadColumnas);
end { TUaMatriz.SetCantidadColumnas };

procedure TUaMatriz.SetCantidadFilas(const ACantidadFilas: Integer);
begin { TUaMatriz.SetCantidadFilas }
  Redimensionar(ACantidadFilas, FCantidadColumnas);
end { TUaMatriz.SetCantidadFilas };

procedure TUaMatriz.SetColumna(const Indice: Integer; const Columna: TUaVector);
var
  I: Integer;
begin { TUaMatriz.SetColumna }
  Assert(Assigned(Columna), 'TUaMatriz.SetColumna: Assigned(Columna)');

  VerificarRangoColumna(Indice, 'TUaMatriz.SetColumna');

  if FCantidadFilas = Columna.Dimension then
  begin
    for I := 1 to FCantidadFilas do
      FDatos^ [I]^ [Indice] := Columna.Datos^ [I];
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.SetColumna: La dimension del vector no coincide con la cantidad de filas de la matriz.');
end { TUaMatriz.SetColumna }; 

procedure TUaMatriz.SetDiagonal(const Vector: TUaVector);
var
  I: Integer;
begin { TUaMatriz.SetDiagonal }
  Assert(Assigned(Vector), 'TUaMatriz.SetDiagonal: Assigned(Vector)');

  if FCantidadFilas = FCantidadColumnas then
  begin
    if Vector.Dimension = FCantidadFilas then
    begin
      for I := 1 to FCantidadFilas do
        FDatos^ [I] ^[I] := Vector.Datos^ [I];
    end
    else
      raise EUaErrorMatriz.Create('TUaMatriz.SetDiagonal: La dimension del vector no coincide con la cantidad de filas (o columnas) de la matriz.');
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.SetDiagonal: La matriz no es cuadrada.');

end { TUaMatriz.SetDiagonal };

procedure TUaMatriz.SetElemento(const IndiceFila, IndiceColumna: Integer; const AReal: TUaReal);
begin { TUaMatriz.SetElemento }
  VerificarRangos(IndiceFila, IndiceColumna, 'TUaMatriz.SetElemento');
  FDatos^ [IndiceFila]^ [IndiceColumna] := AReal;
end { TUaMatriz.SetElemento };

procedure TUaMatriz.SetFila(const Indice: Integer; const Fila: TUaVector);
var
  I: Integer;
begin { TUaMatriz.SetFila }
  Assert(Assigned(Fila), 'TUaMatriz.SetFila: Assigned(Fila)');

  VerificarRangoFila(Indice, 'TUaMatriz.SetFila');

  if FCantidadColumnas = Fila.Dimension then
  begin
    for I := 1 to FCantidadColumnas do
      FDatos^ [Indice]^ [I] := Fila.Datos^ [I];
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.SetFila: La dimension del vector no coincide con la cantidad de columnas de la matriz.');
end { TUaMatriz.SetFila };

procedure TUaMatriz.VerificarRangoColumna(const Indice: Integer; const Llamador: String = 'TUaMatriz.VerificarRangoColumna');
begin { TUaMatriz.VerificarRangoColumna }
  if (Indice < 0) or (Indice > FCantidadColumnas) then
    raise EUaErrorMatriz.Create(Llamador + ': Indice de columna fuera de rango (' + IntToStr(Indice) + ').');
end { TUaMatriz.VerificarRangoColumna };

procedure TUaMatriz.VerificarRangoFila(const Indice: Integer; const Llamador: String = 'TUaMatriz.VerificarRangoFila');
begin { TUaMatriz.VerificarRangoFila }
  if (Indice < 0) or (Indice > FCantidadFilas) then
    raise EUaErrorMatriz.Create(Llamador + ': Indice de fila fuera de rango (' + IntToStr(Indice) + ').');
end { TUaMatriz.VerificarRangoFila };

procedure TUaMatriz.VerificarRangos(const IndiceFila, IndiceColumna: Integer; const Llamador: String = 'TUaMatriz.VerificarRangos');
begin { TUaMatriz.VerificarRangos }
  if (IndiceFila < 0) or (IndiceFila > FCantidadFilas) then
    raise EUaErrorMatriz.Create(Llamador + ': Indice de fila fuera de rango (' + IntToStr(IndiceFila) + ').');
  if (IndiceColumna < 0) or (IndiceColumna > FCantidadColumnas) then
    raise EUaErrorMatriz.Create(Llamador + ': Indice de columna fuera de rango (' + IntToStr(IndiceColumna) + ').');
end { TUaMatriz.VerificarRangos };

function TUaMatriz.Asignar(const AReal: TUaReal): TUaMatriz;
var
  I, J: Integer;
  ADatos: PUaArregloReal;
begin { TUaMatriz.Asignar }
  for I := 1 to FCantidadFilas do
  begin
    ADatos := FDatos^ [I];
    for J := 1 to FCantidadColumnas do
      ADatos^ [J] := AReal;
  end;

  Result := Self;
end { TUaMatriz.Asignar };

function TUaMatriz.Asignar(const Matriz: TUaMatriz): TUaMatriz;
begin { TUaMatriz.Asignar }
  Assert(Assigned(Matriz), 'TUaMatriz.Asignar: Assigned(Matriz)');

  if (FCantidadFilas = Matriz.FCantidadFilas) and (FCantidadColumnas = Matriz.FCantidadColumnas) then
    UaCopiarPUaArregloArregloReal(Matriz.FDatos, FDatos, FCantidadFilas, FCantidadColumnas)
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Asignar: Las dimensiones de las matrices no coinciden.');

  Result := Self;
end { TUaMatriz.Asignar };

function TUaMatriz.AsignarCero: TUaMatriz;
begin { TUaMatriz.AsignarCero }
  Asignar(0.0);

  Result := Self;
end { TUaMatriz.AsignarCero };

function TUaMatriz.AsignarColumna(const Indice: Integer; const AReal: TUaReal): TUaMatriz;
var
  I: Integer;
begin { TUaMatriz.AsignarColumna }
  VerificarRangoColumna(Indice);

  for I := 1 to FCantidadFilas do
    FDatos^ [I]^ [Indice] := AReal;

  Result := Self;
end { TUaMatriz.AsignarColumna };

function TUaMatriz.AsignarFila(const Indice: Integer; const AReal: TUaReal): TUaMatriz;
var
  I: Integer;
begin { TUaMatriz.AsignarFila }
  VerificarRangoFila(Indice);

  for I := 1 to FCantidadColumnas do
    FDatos^ [Indice]^ [I] := AReal;

  Result := Self;
end { TUaMatriz.AsignarFila };

function TUaMatriz.AsignarIdentidad: TUaMatriz;
var
  I: Integer;
begin { TUaMatriz.AsignarIdentidad }
  if FCantidadFilas = FCantidadColumnas then
  begin
    AsignarCero;
    for I := 1 to FCantidadFilas do
      FDatos^ [I]^ [I] := 1.0;
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.SetDiagonal: La matriz no es cuadrada.');

  Result := Self;
end { TUaMatriz.AsignarIdentidad };

function TUaMatriz.AsignarSubMatriz(const IndiceFila, IndiceColumna: Integer; const Matriz: TUaMatriz): TUaMatriz;
var
  I, J: Integer;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.AsignarSubMatriz }
  Assert(Assigned(Matriz), 'TUaMatriz.AsignarSubMatriz: Assigned(Matriz)');

  VerificarRangoFila(IndiceFila);
  VerificarRangoFila(IndiceFila + Matriz.FCantidadFilas - 1);
  VerificarRangoColumna(IndiceColumna);
  VerificarRangoColumna(IndiceColumna + Matriz.FCantidadColumnas - 1);

  for I := 1 to Matriz.FCantidadFilas do
  begin
    ADatos1 := FDatos^ [IndiceFila + I - 1];
    ADatos2 := Matriz.FDatos^ [I];
    for J := 1 to Matriz.FCantidadColumnas do
      ADatos1^ [IndiceColumna + J - 1] := ADatos2^ [J];
  end;

  Result := Self;
end { TUaMatriz.AsignarSubMatriz };

function TUaMatriz.Copia: TUaMatriz;
begin { TUaMatriz.Copia }
  Result := TUaMatriz.Create(Self);
end { TUaMatriz.Copia };

function TUaMatriz.Copiar(const Matriz: TUaMatriz): TUaMatriz;
begin { TUaMatriz.Copiar }
  Assert(Assigned(Matriz), 'TUaMatriz.Copiar: Assigned(Matriz)');

  if (FCantidadFilas <> Matriz.CantidadFilas) or (FCantidadColumnas <> Matriz.FCantidadColumnas) then
    Redimensionar(Matriz.FCantidadFilas, Matriz.FCantidadColumnas);
  Asignar(Matriz);

  Result := Self;
end { TUaMatriz.Copiar };

constructor TUaMatriz.Create(const ACantidadFilas, ACantidadColumnas: Integer);
begin { TUaMatriz.Create }
  if (ACantidadFilas > 0) and (ACantidadColumnas > 0) and (ACantidadFilas <= CUaDimensionMaximaMatrices) and (ACantidadColumnas <= CUaDimensionMaximaMatrices) then
  begin
    try
      FDatos := UaAsignarPUaArregloArregloReal(ACantidadFilas, ACantidadColumnas);

      FCantidadFilas := ACantidadFilas;
      FCantidadColumnas := ACantidadColumnas;
    except
      on E: Exception do
        raise EUaErrorMatriz.Create('UaMatriz.Create: Error instanciando la matriz (' + E.Message + ').');
    end;
  end
  else
    raise EUaErrorMatriz.Create('UaMatriz.Create: Dimensiones erroneas o fuera de rango (' + IntToStr(ACantidadFilas) + ', IntToStr(ACantidadColumnas)).');
end { TUaMatriz.Create };

constructor TUaMatriz.Create(const ACantidadFilas, ACantidadColumnas: Integer; const AReal: TUaReal);
begin { TUaMatriz.Create }
  Create(ACantidadFilas, ACantidadColumnas);
  Asignar(AReal);
end { TUaMatriz.Create };

constructor TUaMatriz.Create(const Dimension: Integer; const Transpuesta: Boolean = true);
begin { TUaMatriz.Create }
  if Transpuesta then
    Create(Dimension, 1)
  else
    Create(1, Dimension);
end { TUaMatriz.Create };

constructor TUaMatriz.Create(const Dimension: Integer; const AReal: TUaReal; const Transpuesta: Boolean = true);
begin { TUaMatriz.Create }
  Create(Dimension, Transpuesta);
  Asignar(AReal);
end { TUaMatriz.Create };

constructor TUaMatriz.Create(const Vector: TUaVector; const Transpuesta: Boolean = true);
begin { TUaMatriz.Create }
  Assert(Assigned(Vector), 'TUaMatriz.Create: Assigned(Vector)');

  if Transpuesta then
  begin
    Create(Vector.Dimension, 1);
    SetColumna(1, Vector);
  end
  else
  begin
    Create(1, Vector.Dimension);
    SetFila(1, Vector);
  end;
end { TUaMatriz.Create };

constructor TUaMatriz.Create(const Matriz: TUaMatriz);
begin { TUaMatriz.Create }
  Assert(Assigned(Matriz), 'TUaMatriz.Create: Assigned(Matriz)');

  Create(Matriz.FCantidadFilas, Matriz.FCantidadColumnas);
  Asignar(Matriz);
end { TUaMatriz.Create };

destructor TUaMatriz.Destroy;
begin { TUaMatriz.Destroy }
  try
    UaLiberarPUaArregloArregloReal(FDatos, FCantidadFilas, FCantidadColumnas);
  finally
    inherited Destroy;
  end;
end { TUaMatriz.Destroy };

function TUaMatriz.Determinante: TUaReal;
var
  I: Integer;
  a: PUaArregloArregloReal;
  d: TUaReal;
  indx: PUaArregloEntero;
begin { TUaMatriz.Determinante }
  if FCantidadFilas = FCantidadColumnas then
  begin
    a := UaAsignarPUaArregloArregloReal(FCantidadFilas, FCantidadColumnas);
    indx := UaAsignarPUaArregloEntero(FCantidadFilas);

    UaCopiarPUaArregloArregloReal(FDatos, a, FCantidadFilas, FCantidadColumnas);

    ludcmp(a, FCantidadFilas, indx, d);
    for I := 1 to FCantidadFilas do
      d := d * a^ [I]^ [I];

    UaLiberarPUaArregloEntero(indx, FCantidadFilas);
    UaLiberarPUaArregloArregloReal(a, FCantidadFilas, FCantidadColumnas);
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Determinante: La matriz no es cuadrada.');

  Result := d;
end { TUaMatriz.Determinante };

function TUaMatriz.DiagonalEn(var Vector: TUaVector): TUaMatriz;
var
  I: Integer;
begin { TUaMatriz.DiagonalEn }
  Assert(Assigned(Vector), 'TUaMatriz.DiagonalEn: Assigned(Vector)');
  Assert(FCantidadFilas = FCantidadColumnas, 'TUaMatriz.DiagonalEn: FCantidadFilas = FCantidadColumnas');
  Assert(Vector.Dimension = FCantidadColumnas, 'TUaMatriz.DiagonalEn: Vector.Dimension = FCantidadColumnas');

  for I := 1 to FCantidadColumnas do
    // Vector [I] := FDatos^ [I]^ [I];
    Vector.Datos^ [I] := FDatos^ [I]^ [I];

  Result := Self;
end { TUaMatriz.DiagonalEn };

function TUaMatriz.IntercambiarColumnas(const IndicePrimeraColumna, IndiceSegundaColumna: Integer): TUaMatriz;
var
  I: Integer;
  R: TUaReal;
begin { TUaMatriz.IntercambiarColumnas }
  VerificarRangoColumna(IndicePrimeraColumna, 'TUaMatriz.IntercambiarColumnas');
  VerificarRangoColumna(IndiceSegundaColumna, 'TUaMatriz.IntercambiarColumnas');

  if IndicePrimeraColumna <> IndiceSegundaColumna then
  begin
    for I := 1 to FCantidadFilas do
    begin
      R := FDatos^ [I]^ [IndiceSegundaColumna];
      FDatos^ [I]^ [IndiceSegundaColumna] := FDatos^ [I]^ [IndicePrimeraColumna];
      FDatos^ [I]^ [IndicePrimeraColumna] := R;
    end;
  end;

  Result := Self;
end { TUaMatriz.IntercambiarColumnas };

function TUaMatriz.IntercambiarElementos(const IndiceFilaPrimerElemento, IndiceColumnaPrimerElemento, IndiceFilaSegundoElemento, IndiceColumnaSegundoElemento: Integer): TUaMatriz;
var
  R: TUaReal;
begin { TUaMatriz.IntercambiarElementos }
  VerificarRangos(IndiceFilaPrimerElemento, IndiceColumnaPrimerElemento, 'TUaMatriz.IntercambiarElementos');
  VerificarRangos(IndiceFilaSegundoElemento, IndiceColumnaSegundoElemento, 'TUaMatriz.IntercambiarElementos');

  if (IndiceFilaPrimerElemento <> IndiceFilaSegundoElemento) or (IndiceColumnaPrimerElemento <> IndiceColumnaSegundoElemento) then
  begin
    R := FDatos^ [IndiceFilaSegundoElemento]^ [IndiceColumnaSegundoElemento];
    FDatos^ [IndiceFilaSegundoElemento]^ [IndiceColumnaSegundoElemento] := FDatos^ [IndiceFilaPrimerElemento]^ [IndiceColumnaPrimerElemento];
    FDatos^ [IndiceFilaPrimerElemento]^ [IndiceColumnaPrimerElemento] := R;
  end;

  Result := Self;
end { TUaMatriz.IntercambiarElementos };

function TUaMatriz.IntercambiarFilas(const IndicePrimeraFila, IndiceSegundaFila: Integer): TUaMatriz;
var
  (* I: Integer; *)
  Fila: PUaArregloReal;
  (* R: TUaReal; *)
begin { TUaMatriz.IntercambiarFilas }
  VerificarRangoFila(IndicePrimeraFila, 'TUaMatriz.IntercambiarFilas');
  VerificarRangoFila(IndiceSegundaFila, 'TUaMatriz.IntercambiarFilas');

  if IndicePrimeraFila <> IndiceSegundaFila then
  begin
    Fila := FDatos^ [IndiceSegundaFila];
    FDatos^ [IndiceSegundaFila] := FDatos^ [IndiceSegundaFila];
    FDatos^ [IndicePrimeraFila] := Fila;
    (*
    for I := 1 to FCantidadColumnas do
    begin
      R := FDatos^ [IndiceSegundaFila]^ [I];
      FDatos^ [IndiceSegundaFila]^ [I] := FDatos^ [IndicePrimeraFila]^ [I];
      FDatos^ [IndicePrimeraFila]^ [I] := R;
    end;
    *)
  end;

  Result := Self;
end { TUaMatriz.IntercambiarFilas };

function TUaMatriz.Inversa: TUaMatriz;
var
  I, J: Integer;
  a: PUaArregloArregloReal;
  col: PUaArregloReal;
  indx: PUaArregloEntero;
  d: TUaReal;
begin { TUaMatriz.Inversa }
  if FCantidadFilas = FCantidadColumnas then
  begin
    Result := TUaMatriz.Create(FCantidadFilas, FCantidadColumnas);

    a := UaAsignarPUaArregloArregloReal(FCantidadFilas, FCantidadColumnas);
    UaCopiarPUaArregloArregloReal(FDatos, a, FCantidadFilas, FCantidadColumnas);

    col := UaAsignarPUaArregloReal(FCantidadFilas);
    indx := UaAsignarPUaArregloEntero(FCantidadFilas);

    ludcmp(a, FCantidadFilas, indx, d);
    for J := 1 to FCantidadColumnas do
    begin
      for I := 1 to FCantidadFilas do
        col [I] := 0.0;
      col [J] := 1.0;

      lubksb(a, FCantidadFilas, indx, col);
      for I := 1 to FCantidadFilas do
        Result.FDatos^ [I]^ [J] := col [I];
    end;

    UaLiberarPUaArregloEntero(indx, FCantidadFilas);
    UaLiberarPUaArregloReal(col, FCantidadFilas);
    UaLiberarPUaArregloArregloReal(a, FCantidadFilas, FCantidadColumnas);
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Inversa: La matriz no es cuadrada.');
end { TUaMatriz.Inversa };

function TUaMatriz.Invertir: TUaMatriz;
var
  I, J: Integer;
  y: PUaArregloArregloReal;
  col: PUaArregloReal;
  indx: PUaArregloEntero;
  d: TUaReal;
begin { TUaMatriz.Invertir }
  if FCantidadFilas = FCantidadColumnas then
  begin
    y := UaAsignarPUaArregloArregloReal(FCantidadFilas, FCantidadColumnas);
    col := UaAsignarPUaArregloReal(FCantidadFilas);
    indx := UaAsignarPUaArregloEntero(FCantidadFilas);

    ludcmp(FDatos, FCantidadFilas, indx, d);
    for J := 1 to FCantidadColumnas do
    begin
      for I := 1 to FCantidadFilas do
        col [I] := 0.0;
      col [J] := 1.0;

      lubksb(FDatos, FCantidadFilas, indx, col);
      for I := 1 to FCantidadFilas do
        y^ [I]^ [J] := col [I];
    end;

    UaLiberarPUaArregloEntero(indx, FCantidadFilas);
    UaLiberarPUaArregloReal(col, FCantidadFilas);

    UaLiberarPUaArregloArregloReal(FDatos, FCantidadFilas, FCantidadColumnas);
    FDatos := y;
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Invertir: La matriz no es cuadrada.');

  Result := Self;
end { TUaMatriz.Invertir };

function TUaMatriz.Multiplicar(const AReal: TUaReal): TUaMatriz;
var
  I, J: Integer;
  ADatos: PUaArregloReal;
begin { TUaMatriz.Multiplicar }
  for I := 1 to FCantidadFilas do
  begin
    ADatos := FDatos^ [I];
    for J := 1 to FCantidadColumnas do
      ADatos^ [J] := ADatos^ [J] * AReal;
  end;

  Result := Self;
end { TUaMatriz.Multiplicar };

function TUaMatriz.Multiplicar(const Matriz: TUaMatriz): TUaMatriz;
var
  I, J, K: Integer;
  ADatos: PUaArregloArregloReal;
  ADatos1, ADatos2: PUaArregloReal;
  Suma: TUaReal;
begin { TUaMatriz.Multiplicar }
  Assert(Assigned(Matriz), 'TUaMatriz.Multiplicar: Assigned(Matriz)');

  if FCantidadColumnas = Matriz.FCantidadFilas then
  begin
    if FCantidadFilas * Matriz.FCantidadColumnas <= CUaDimensionMaximaMatrices then
    begin
      ADatos := UaAsignarPUaArregloArregloReal(FCantidadFilas, Matriz.FCantidadColumnas);

      for I := 1 to FCantidadFilas do
      begin
        ADatos1 :=  FDatos^ [I];
        ADatos2 :=  ADatos^ [I];

        for J := 1 to Matriz.FCantidadColumnas do
        begin
          Suma := 0.0;
          for K := 1 to FCantidadColumnas do
            Suma := Suma + (ADatos1^ [K]) * (Matriz.FDatos^ [K]^ [J]);

          ADatos2^ [J] := Suma;
        end;
      end;

      UaLiberarPUaArregloArregloReal(FDatos, FCantidadFilas, FCantidadColumnas);
      FDatos := ADatos;
      FCantidadColumnas := Matriz.FCantidadColumnas;
    end
    else
      raise EUaErrorMatriz.Create('TUaMatriz.Multiplicar: Las dimensiones de la matriz producto exceden los limites permitidos.');
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Multiplicar: La cantidad de columnas (' + IntToStr(FCantidadColumnas) + ') de la matriz difiere de la cantidad de filas del operando (' + IntToStr(Matriz.FCantidadFilas) + ').');

  Result := Self;
end { TUaMatriz.Multiplicar };

function TUaMatriz.Norma: TUaReal;
var
  I, J: Integer;
  ADatos: PUaArregloReal;
begin { TUaMatriz.Norma }
  Result := 0.0;
  for I := 1 to FCantidadFilas do
  begin
    ADatos := FDatos^ [I];
    for J := 1 to FCantidadColumnas do
      Result := Result + Sqrt(ADatos^ [J]);
  end;
end { TUaMatriz.Norma };

function TUaMatriz.PreMultiplicar(const Matriz: TUaMatriz): TUaMatriz;
var
  I, J, K: Integer;
  ADatos: PUaArregloArregloReal;
  ADatos1, ADatos2: PUaArregloReal;
  Suma: TUaReal;
begin { TUaMatriz.PreMultiplicar }
  Assert(Assigned(Matriz), 'TUaMatriz.PreMultiplicar: Assigned(Matriz)');

  if FCantidadFilas = Matriz.FCantidadColumnas then
  begin
    if Matriz.FCantidadFilas * FCantidadColumnas <= CUaDimensionMaximaMatrices then
    begin
      ADatos := UaAsignarPUaArregloArregloReal(Matriz.FCantidadFilas, FCantidadColumnas);
      for I := 1 to Matriz.FCantidadFilas do
      begin
        ADatos1 := Matriz.FDatos^ [I];
        ADatos2 := ADatos^ [I];

        for J := 1 to FCantidadColumnas do
        begin
          Suma := 0.0;
          for K := 1 to Matriz.FCantidadColumnas do
            Suma := Suma + ADatos1^ [K] * FDatos^ [K]^ [J];

          ADatos2 ^[J] := Suma;
        end;
      end;

      UaLiberarPUaArregloArregloReal(FDatos, FCantidadFilas, FCantidadColumnas);
      FDatos := ADatos;
      FCantidadFilas := Matriz.FCantidadFilas;
    end
    else
      raise EUaErrorMatriz.Create('TUaMatriz.PreMultiplicar: Las dimensiones de la matriz producto exceden los limites permitidos.');
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.PreMultiplicar: La cantidad de filas de la matriz (' + IntToStr(FCantidadFilas) + ') difiere de la cantidad de columnas del operando (' + IntToStr(Matriz.FCantidadColumnas) + ').');

  Result := Self;
end { TUaMatriz.PreMultiplicar };

function TUaMatriz.PreProducto(const Matriz: TUaMatriz): TUaMatriz;
var
  I, J, K: Integer;
  Suma: TUaReal;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.PreProducto }
  Assert(Assigned(Matriz), 'TUaMatriz.PreProducto: Assigned(Matriz)');

  if FCantidadFilas = Matriz.FCantidadColumnas then
  begin
    if Matriz.FCantidadFilas * FCantidadColumnas <= CUaDimensionMaximaMatrices then
    begin
      Result := TUaMatriz.Create(Matriz.FCantidadFilas, FCantidadColumnas);

      for I := 1 to Matriz.FCantidadFilas do
      begin
        ADatos1 := Matriz.FDatos^ [I];
        ADatos2 := Result.FDatos^ [I];

        for J := 1 to FCantidadColumnas do
        begin
          Suma := 0.0;
          for K := 1 to Matriz.FCantidadColumnas do
            Suma := Suma + ADatos1^ [K] * FDatos^ [K]^ [J];

          ADatos2^ [J] := Suma;
        end;
      end;
    end
    else
      raise EUaErrorMatriz.Create('TUaMatriz.PreProducto: Las dimensiones de la matriz producto exceden los limites permitidos.');
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.PreProducto: La cantidad de filas de la matriz (' + IntToStr(FCantidadFilas) + ') difiere de la cantidad de columnas del operando (' + IntToStr(Matriz.FCantidadColumnas) + ').');
end { TUaMatriz.PreProducto };

function TUaMatriz.Producto(const AReal: TUaReal): TUaMatriz;
begin { TUaMatriz.Producto }
  Result := TUaMatriz.Create(Self);
  Result.Multiplicar(AReal);
end { TUaMatriz.Producto };

function TUaMatriz.Producto(const Matriz: TUaMatriz): TUaMatriz;
var
  I, J, K: Integer;
  Suma: TUaReal;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.Producto }
  Assert(Assigned(Matriz), 'TUaMatriz.Producto: Assigned(Matriz)');

  if FCantidadColumnas = Matriz.FCantidadFilas then
  begin
    if FCantidadFilas * Matriz.FCantidadColumnas <= CUaDimensionMaximaMatrices then
    begin
      Result := TUaMatriz.Create(FCantidadFilas, Matriz.FCantidadColumnas);

      for I := 1 to FCantidadFilas do
      begin
        ADatos1 := FDatos^ [I];
        ADatos2 := Result.FDatos^ [I];

        for J := 1 to Matriz.FCantidadColumnas do
        begin
          Suma := 0.0;
          for K := 1 to FCantidadColumnas do
            Suma := Suma + ADatos1^ [K] * Matriz.FDatos^ [K]^ [J];

          ADatos2^ [J] := Suma;
        end;
      end;
    end
    else
      raise EUaErrorMatriz.Create('TUaMatriz.Producto: Las dimensiones de la matriz producto exceden los limites permitidos.');
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Producto: La cantidad de columnas de la matriz (' + IntToStr(FCantidadColumnas) + ') difiere de la cantidad de filas del operando (' + IntToStr(Matriz.FCantidadFilas) + ').');
end { TUaMatriz.Producto };

function TUaMatriz.Rango: TUaEntero;
var
  I, ACantidadFilas: Integer;
  a, v: PUaArregloArregloReal;
  w: PUaArregloReal;
  Tolerancia: TUaReal;
begin { TUaMatriz.Rango }
  if FCantidadFilas < FCantidadColumnas then
    ACantidadFilas := FCantidadColumnas
  else
    ACantidadFilas := FCantidadFilas;

  // Notar que las filas adicionales con ceros son agregadas automaticamente
  a := UaAsignarPUaArregloArregloReal(ACantidadFilas, FCantidadColumnas);
  // UaCopiarPUaArregloArregloReal(FDatos, a, ACantidadFilas, FCantidadColumnas);
  UaCopiarPUaArregloArregloReal(FDatos, a, FCantidadFilas, FCantidadColumnas);

  v := UaAsignarPUaArregloArregloReal(FCantidadColumnas, FCantidadColumnas);
  w := UaAsignarPUaArregloReal(FCantidadColumnas);

  svdcmp(a, ACantidadFilas, FCantidadColumnas, w, v);

  Tolerancia := UaMaximo(ACantidadFilas, FCantidadColumnas) * w^ [1] * UaEpsilon;

  Result := 0;
  I := 1;
  while (I <= FCantidadColumnas) do
  begin
    if (w^ [I] > Tolerancia) then
      Inc(Result);

    Inc(I);
  end;

  UaLiberarPUaArregloReal(w, FCantidadColumnas);
  UaLiberarPUaArregloArregloReal(a, ACantidadFilas, FCantidadColumnas);
  UaLiberarPUaArregloArregloReal(v, FCantidadColumnas, FCantidadColumnas);
end { TUaMatriz.Rango };

procedure TUaMatriz.Redimensionar(const ACantidadFilas, ACantidadColumnas: Integer);
var
  K, L: Integer;
  ADatos: PUaArregloArregloReal;
begin { TUaMatriz.Redimensionar }
  if (ACantidadFilas > 0) and (ACantidadColumnas > 0) and (ACantidadFilas * ACantidadColumnas <= CUaDimensionMaximaMatrices) then
  begin
    try
      ADatos := UaAsignarPUaArregloArregloReal(ACantidadFilas, ACantidadColumnas);

      K := UaMinimo(FCantidadFilas, ACantidadFilas);
      L := UaMinimo(FCantidadColumnas, ACantidadColumnas);

      UaCopiarPUaArregloArregloReal(FDatos, ADatos, K, L);

      (*
      for I := 1 to K do
        for J := 1 to L do
          ADatos^ [I]^ [J] := FDatos^ [I]^ [J];
      *)

      UaLiberarPUaArregloArregloReal(FDatos, FCantidadFilas, FCantidadColumnas);
      FDatos := ADatos;
      FCantidadFilas := ACantidadFilas;
      FCantidadColumnas := ACantidadColumnas;
    except
      on E: Exception do
        raise EUaErrorMatriz.Create('UaMatriz.Redimensionar: Error redimensionando la matriz (' + E.Message + ').');
    end;
  end
  else
    raise EUaErrorMatriz.Create('UaMatriz.Redimensionar: Dimensiones erroneas o fuera de rango (' + IntToStr(ACantidadFilas) + ', IntToStr(ACantidadColumnas)).');
end { TUaMatriz.Redimensionar };

function TUaMatriz.Resta(const AReal: TUaReal): TUaMatriz;
begin { TUaMatriz.Resta }
  Result := TUaMatriz.Create(Self);
  Result.Restar(AReal);
end { TUaMatriz.Resta };

function TUaMatriz.Resta(const Matriz: TUaMatriz): TUaMatriz;
begin { TUaMatriz.Resta }
  Assert(Assigned(Matriz), 'TUaMatriz.Resta: Assigned(Matriz)');

  Result := TUaMatriz.Create(Self);
  Result.Restar(Matriz);
end { TUaMatriz.Resta };

function TUaMatriz.Restar(const AReal: TUaReal): TUaMatriz;
var
  I, J: Integer;
  ADatos: PUaArregloReal;
begin { TUaMatriz.Restar }
  for I := 1 to FCantidadColumnas do
  begin
    ADatos := FDatos^ [I];
    for J := 1 to FCantidadColumnas do
      ADatos^ [J] := ADatos^ [J] - AReal;
  end;

  Result := Self;
end { TUaMatriz.Restar };

function TUaMatriz.Restar(const Matriz: TUaMatriz): TUaMatriz;
var
  I, J: Integer;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.Restar }
  Assert(Assigned(Matriz), 'TUaMatriz.Restar: Assigned(Matriz)');

  if (FCantidadFilas = Matriz.FCantidadFilas) and (FCantidadColumnas = Matriz.FCantidadColumnas) then
  begin
    for I := 1 to FCantidadFilas do
    begin
      ADatos1 := FDatos^ [I];
      ADatos2 := Matriz.FDatos^ [I];

      for J := 1 to FCantidadColumnas do
        ADatos1^ [J] := ADatos1^ [J] - ADatos2^ [J];
    end;
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Restar: La dimensiones de las matrices son diferentes.');

  Result := Self;
end { TUaMatriz.Restar };

function TUaMatriz.SubMatriz(const IndiceFilaInicial, IndiceColumnaInicial, IndiceFilaFinal, IndiceColumnaFinal: Integer): TUaMatriz;
var
  I, J: Integer;
  ACantidadFilas, ACantidadColumnas: Integer;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.SubMatriz }
  VerificarRangoFila(IndiceFilaInicial, 'TUaMatriz.SubMatriz');
  VerificarRangoFila(IndiceFilaFinal, 'TUaMatriz.SubMatriz');
  VerificarRangoColumna(IndiceColumnaInicial, 'TUaMatriz.SubMatriz');
  VerificarRangoColumna(IndiceColumnaInicial, 'TUaMatriz.SubMatriz');

  if (IndiceFilaFinal >= IndiceFilaInicial) and (IndiceColumnaFinal >= IndiceColumnaInicial) then
  begin
    ACantidadFilas := IndiceFilaFinal - IndiceFilaInicial + 1;
    ACantidadColumnas := IndiceColumnaFinal - IndiceColumnaInicial + 1;

    Result := TUaMatriz.Create(ACantidadFilas, ACantidadColumnas);
    for I := 1 to ACantidadFilas do
    begin
      ADatos1 := Result.FDatos^ [I];
      ADatos2 := FDatos^ [IndiceFilaInicial + I - 1];
      for J := 1 to ACantidadColumnas do
        ADatos1^ [J] := ADatos2^ [IndiceColumnaInicial + J - 1];
    end;
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.SubMatriz: Indices de filas o columnas erroneos.');
end { TUaMatriz.SubMatriz };

function TUaMatriz.SubMatrizEn(var Matriz: TUaMatriz; const IndiceFilaInicial, IndiceColumnaInicial, IndiceFilaFinal, IndiceColumnaFinal: Integer; const IndiceFilaDestino: Integer; const IndiceColumnaDestino: Integer): TUaMatriz;
var
  I, J: Integer;
  ACantidadFilas, ACantidadColumnas: Integer;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.SubMatrizEn }
  Assert(Assigned(Matriz), 'TUaMatriz.SubMatrizEn: Assigned(Matriz)');

  VerificarRangoFila(IndiceFilaInicial, 'TUaMatriz.SubMatrizEn');
  VerificarRangoFila(IndiceFilaFinal, 'TUaMatriz.SubMatrizEn');
  VerificarRangoColumna(IndiceColumnaInicial, 'TUaMatriz.SubMatrizEn');
  VerificarRangoColumna(IndiceColumnaFinal, 'TUaMatriz.SubMatrizEn');

  if (IndiceFilaFinal >= IndiceFilaInicial) and (IndiceColumnaFinal >= IndiceColumnaInicial) then
  begin
    ACantidadFilas := IndiceFilaFinal - IndiceFilaInicial + 1;
    ACantidadColumnas := IndiceColumnaFinal - IndiceColumnaInicial + 1;

    Matriz.VerificarRangos(IndiceFilaDestino, IndiceColumnaDestino, 'TUaMatriz.SubMatrizEn');
    Matriz.VerificarRangos(IndiceFilaDestino + ACantidadFilas - 1, IndiceColumnaDestino + ACantidadColumnas - 1, 'TUaMatriz.SubMatrizEn');

    for I := 1 to ACantidadFilas do
    begin
      ADatos1 := Matriz.FDatos^ [I + IndiceFilaDestino - 1];
      ADatos2 := FDatos^ [IndiceFilaInicial + I - 1];
      for J := 1 to ACantidadColumnas do
        ADatos1^ [J + IndiceColumnaDestino - 1] := ADatos2^ [IndiceColumnaInicial + J - 1];
    end;
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.SubMatrizEn: Indices de filas o columnas erroneos.');

  Result := Self;
end { TUaMatriz.SubMatrizEn };

function TUaMatriz.Suma(const AReal: TUaReal): TUaMatriz;
begin { TUaMatriz.Suma }
  Result := TUaMatriz.Create(Self);
  Result.Sumar(AReal);
end { TUaMatriz.Suma };

function TUaMatriz.Suma(const Matriz: TUaMatriz): TUaMatriz;
begin { TUaMatriz.Suma }
  Assert(Assigned(Matriz), 'TUaMatriz.Suma: Assigned(Matriz)');

  Result := TUaMatriz.Create(Self);
  Result.Sumar(Matriz);
end { TUaMatriz.Suma };

function TUaMatriz.Sumar(const AReal: TUaReal): TUaMatriz;
var
  I, J: Integer;
  ADatos1: PUaArregloReal;
begin { TUaMatriz.Sumar }
  for I := 1 to FCantidadFilas do
  begin
    ADatos1 := FDatos^ [I];
    for J := 1 to FCantidadColumnas do
      ADatos1^ [J] := ADatos1^ [J] + AReal;
  end;

  Result := Self;
end { TUaMatriz.Sumar };

function TUaMatriz.Sumar(const Matriz: TUaMatriz): TUaMatriz;
var
  I, J: Integer;
  ADatos1, ADatos2: PUaArregloReal;
begin { TUaMatriz.Sumar }
  Assert(Assigned(Matriz), 'TUaMatriz.Sumar: Assigned(Matriz)');

  if (FCantidadFilas = Matriz.FCantidadFilas) and (FCantidadColumnas = Matriz.FCantidadColumnas) then
  begin
    for I := 1 to FCantidadFilas do
    begin
      ADatos1 := FDatos^ [I];
      ADatos2 := Matriz.FDatos^ [I];
      for J := 1 to FCantidadColumnas do
        ADatos1^ [J] := ADatos1^ [J] + ADatos2^ [J];
    end;
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Sumar: Las dimensiones de las matrices son diferentes.');

  Result := Self;
end { TUaMatriz.Sumar };

function TUaMatriz.Transponer: TUaMatriz;
var
  I, J: Integer;
  ADatos: PUaArregloArregloReal;
begin { TUaMatriz.Transponer }
  ADatos := UaAsignarPUaArregloArregloReal(FCantidadColumnas, FCantidadFilas);

  for I := 1 to FCantidadFilas do
    for J := 1 to FCantidadColumnas do
      ADatos^ [J]^ [I] := FDatos^ [I]^ [J];

  UaLiberarPUaArregloArregloReal(FDatos, FCantidadFilas, FCantidadColumnas);
  FDatos := ADatos;
  I := FCantidadFilas;
  FCantidadFilas := FCantidadColumnas;
  FCantidadColumnas := I;

  Result := Self;
end { TUaMatriz.Transponer };

function TUaMatriz.Transpuesta: TUaMatriz;
var
  I, J: Integer;
begin { TUaMatriz.Transpuesta }
  Result := TUaMatriz.Create(FCantidadColumnas, FCantidadFilas);

  for I := 1 to FCantidadFilas do
    for J := 1 to FCantidadColumnas do
      Result.FDatos^ [J]^ [I] := FDatos^ [I]^ [J];
end { TUaMatriz.Transpuesta };

function TUaMatriz.Traza: TUaReal;
var
  I: Integer;
begin { TUaMatriz.Traza }
  if FCantidadFilas = FCantidadColumnas then
  begin
    Result := 0.0;
    for I := 1 to FCantidadFilas do
      Result := Result + FDatos^ [I]^ [I];
  end
  else
    raise EUaErrorMatriz.Create('TUaMatriz.Traza: La matriz no es cuadrada.');
end { TUaMatriz.Traza };

// -----------------------------------------------------------------------------
// Procedimientos y funciones de UaMatriz
// -----------------------------------------------------------------------------

function UaMatrizIdentidad(const CantidadFilas: Integer): TUaMatriz;
begin { MatrizIdentidad }
  Result := TUaMatriz.Create(CantidadFilas, CantidadFilas);
  Result.AsignarIdentidad;
end { MatrizIdentidad };

function UaMatrizIdentidad(const CantidadFilas, CantidadColumnas: Integer): TUaMatriz;
begin { MatrizIdentidad }
  Result := TUaMatriz.Create(CantidadFilas, CantidadColumnas);
  Result.AsignarIdentidad;
end { MatrizIdentidad };

end { UaMatriz }.
