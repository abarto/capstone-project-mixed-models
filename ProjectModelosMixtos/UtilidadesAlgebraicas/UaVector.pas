{**
@abstract(Vector de numeros reales y metodos asociados.)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 29, 2004)
@lastmod(December 2, 2004)
Este modulo provee vectores de numeros reales y subrutinas asociadas.
}
unit UaVector;

interface

uses
  UaComun;

const
  {** Dimension (cantidad de elementos) maxima de vectores. }
  CUaDimensionMaximaVectores: Integer = MaxInt div 16;

type
  {** Vector de numeros reales (Ver @link(UaComun.TUaReal).)
      @abstract(Vector de numeros reales.) }
  TUaVector = class
  private
    {** Dimension (cantidad de elementos) del vector. }
    FDimension: Integer;
    {** Arreglo que contiene los elementos del vector. }
    FDatos: PUaArregloReal;
    {** Metodo de lectura de la propiedad @link(Elemento). Devuelve el numero real
        almacenado bajo un indice.
        @param(Indice Indice del elemento que deseamos acceder.)
        @returns(El numero real almacenado bajo Indice.) }
    function GetElemento(const Indice: Integer): TUaReal;
    {** Metodo de lectura de la propiedad @link(Texto). Genera la
        representacion como cadena de caracteres del vector.
        @returns(Representacion como cadena de caracteres del vector.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        vector al valor solicitado por el usuario, de ser posible.
        @param(ADimension Nueva dimension del vector.) }
    procedure SetDimension(const ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Elemento). Actualiza el numero
        real almacenado bajo un indice.
        @param(Indice Indice del elemento que deseamos actualizar.)
        @param(AReal Nuevo valor del numero real.) }
    procedure SetElemento(const Indice: Integer; const AReal: TUaReal);
    {** Verifica que el valor de un indice este dentro de los rangos
        permitidos. Esto es, que el indice sea mayor o igual que uno y menor
        que la dimension del arreglo. Si el indice esta fuera de los limites, se
        eleva una @link(EUaErrorVector).
        @param(Indice Indice a verificar.)
        @param(Llamador Identificador para indicar el metodo donde se produce
        la excepcion.) }
    procedure VerificarRangos(const Indice: Integer; const Llamador: String = 'TUaVector.VerificarRangos');
  public
    {** Asigna un valor a todos los elementos del vector.
        @param(AReal Valor a asignar a los elementos del vector.)
        @returns(Referencia a Self.) }
    function Asignar(const AReal: TUaReal): TUaVector; overload;
    {** Asigna valores a los elementos del vector de acuerdo a los valores
        almacenados en el vector parametro. Solo pueden asignarse vectores de
        igual dimension.
        @param(Vector Vector que contiene los valores a asignar.)
        @returns(Referencia a Self.) }
    function Asignar(const Vector: TUaVector): TUaVector; overload;
    {** Asigna ceros a todos los elementos del vector.
        @returns(Referencia a Self.) }
    function AsignarCero: TUaVector;
    {** Genera una copia por valor del vector.
        @returns(Copia por valor del vector.) }
    function Copia: TUaVector;
    {** Copia el contenido del parametro en el vector referenciado. Se
        redimensiona el vector de ser necesario.
        @param(Vector Vector del cual se desea copiar el contenido.)
        @returns(Referencia a Self.) }
    function Copiar(const Vector: TUaVector): TUaVector;
    {** Constructor.
        @param(ADimension Dimension inicial del vector.) }
    constructor Create(const ADimension: Integer); overload;
    {** Constructor.
        @param(ADimension Dimension inicial del vector.)
        @param(AReal Valor inicial de los elementos del vector.) }
    constructor Create(const ADimension: Integer; const AReal: TUaReal); overload;
    {** Constructor. Esta version una copia por valor del parametro.
        @param(Vector Vector del cual se desea una copia.) }
    constructor Create(const Vector: TUaVector); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Divide los elementos del vector por un numero real.
        @param(AReal Valor divisor de los elementos del vector.)
        @returns(Referencia a Self.) }
    function Dividir(const AReal: TUaReal): TUaVector; overload;
    {** Divide al vector referenciado por el vector parametro. Solo pueden
        restarse vectores de igual dimension.
        @param(Vector Vector que divide al vector referenciado.)
        @returns(Referencia a Self.) }
    function Dividir(const Vector: TUaVector): TUaVector; overload;
    {** Calcula el vector resultante de dividir los elementos del vector
        por un numero real.
        @param(AReal Valor divisor de los elementos del vector.)
        @returns(Vector resultante de la division.) }
    function Division(const AReal: TUaReal): TUaVector; overload;
    {** Calcula la division del vector referenciado por el vector parametro.
        Solo pueden dividirse vectores de igual dimension.
        @param(Vector Vector a dividir.)
        @returns(Vector resultante de la division.) }
    function Division(const Vector: TUaVector): TUaVector; overload;
    {** Calcula la norma1 del vector.
        @returns(La norma1 del vector.) }
    function Norma1: TUaReal;
    {** Calcula la norma2 del vector.
        @returns(La norma2 del vector.) }
    function Norma2: TUaReal;
    {** Calcula la norma infinito del vector.
        @returns(La norma infinito del vector.) }
    function NormaInfinito: TUaReal;
    {** Devuelve un vector con los primeros elementos del vector referenciado.
        @param(CantidadElementos Cantidad de elementos que se desean tomar.)
        @returns(Vector con los primeros elementos del vector.) }
    function Prefijo(const CantidadElementos: Integer): TUaVector;
    {** Calcula el vector resultado de multiplicar a cada elemento del vector
        referenciado por un valor real.
        @param(AReal Valor a multiplicar.)
        @returns(Vector resultante de la multiplicacion.) }
    function Producto(const AReal: TUaReal): TUaVector; overload;
    {** Calcula el producto del vector referenciado por el vector parametro.
        Solo pueden multiplicarse vectores de igual dimension.
        @param(Vector Vector a multiplicar.)
        @returns(Producto entre el vector referenciado y el parametro.) }
    function Producto(const Vector: TUaVector): TUaVector; overload;
    {** Calcula el producto punto entre el vector referenciado y el parametro.
        @param(Vector Vector a multiplicar.)
        @returns(Producto punto entre el vector referenciado y el parametro.) }
    function ProductoPunto(const Vector: TUaVector): TUaReal;
    {** Calcula la productoria de los elementos del vector.
        @returns(La productoria de los elementos del vector.) }
    function Productoria: TUaReal;
    {** Calcula el vector resultante de restar a los elementos del vector
        referenciado un numero real.
        @param(AReal Valor a restar a los elementos del vector.)
        @returns(Vector resultante de la resta.) }
    function Resta(const AReal: TUaReal): TUaVector; overload;
    {** Calcula la resta del vector referenciado con el vector parametro. Solo
        pueden restarse vectores de igual dimension.
        @param(Vector Vector a restar.)
        @returns(Vector resultante de la resta.) }
    function Resta(const Vector: TUaVector): TUaVector; overload;
    {** Resta un numero real a los elementos del vector.
        @param(AReal Valor a restar a los elementos del vector.)
        @returns(Referencia a Self.) }
    function Restar(const AReal: TUaReal): TUaVector; overload;
    {** Resta al vector referenciado el vector parametro. Solo pueden restarse
        vectores de igual dimension.
        @param(Vector Vector a restar al vector referenciado.)
        @returns(Referencia a Self.) }
    function Restar(const Vector: TUaVector): TUaVector; overload;
    {** Devuelve un vector con los elementos contenidos entre dos indices.
        @param(IndiceInicio Indice inicial del segmento.)
        @param(IndiceFin Indice final del segmento.)
        @returns(Segmento del vector contenido entre los dos indices.) }
    function Segmento(const IndiceInicio, IndiceFin: Integer): TUaVector;
    {** Calcula la sumatoria de los elementos del vector.
        @returns(La sumatoria de los elementos del vector.) }
    function Sumatoria: TUaReal;
    {** Calcula el vector resultante de sumar un numero real a los elementos
        del vector referenciado.
        @param(AReal Valor a sumar a los elementos del vector.)
        @returns(Vector resultante de la suma.) }
    function Suma(const AReal: TUaReal): TUaVector; overload;
    {** Calcula la suma del ector parametro con el vector referenciado. Solo
        pueden sumarse vectores de igual dimension.
        @param(Vector Vector a sumar.)
        @returns(Vector resultante de la suma.) }
    function Suma(const Vector: TUaVector): TUaVector; overload;
    {** Suma un numero real a los elementos del vector.
        @param(AReal Valor a sumar a los elementos del vector.)
        @returns(Referencia a Self.) }
    function Sumar(const AReal: TUaReal): TUaVector; overload;
    {** Suma al vector referenciado el vector parametro. Solo pueden sumarse
        vectores de igual dimension.
        @param(Vector Vector a sumar al vector referenciado.)
        @returns(Referencia a Self.) }
    function Sumar(const Vector: TUaVector): TUaVector; overload;
    {** Multiplica los elementos del vector por un numero real.
        @param(AReal Valor a multiplicar a los elementos del vector.)
        @returns(Referencia a Self.) }
    function Multiplicar(const AReal: TUaReal): TUaVector; overload;
    {** Multiplica el vector referenciado por el parametro. Solo pueden
        multiplicarse elementos de igual dimension.
        @param(Vector Vector a multiplicar.)
        @returns(Referencia a Self.) }
    function Multiplicar(const Vector: TUaVector): TUaVector; overload;
    {** Norma del vector. }
    property Norma: TUaReal read Norma2;
    {** Puntero al contenedor de datos. }
    property Datos: PUaArregloReal read FDatos;
    {** Arreglo de elementos del vector. }
    property Elemento [const Indice: Integer]: TUaReal read GetElemento write SetElemento; default;
    {** Dimension (cantidad de elementos) del vector. }
    property Dimension: Integer read FDimension write SetDimension;
    {** Representacion como cadena de caracteres del vector. }
    property Texto: String read GetTexto;
  end;

  {** Excepcion relacionada a errores en operaciones de vectores.
      @abstract(Excepcion relacionada a operaciones de vectores.) }
  EUaErrorVector = class(EUaError);

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TUaVector
// -----------------------------------------------------------------------------

function TUaVector.GetElemento(const Indice: Integer): TUaReal;
begin { TUaVector.GetElemento }
  VerificarRangos(Indice, 'TUaVector.GetElemento');
  Result := FDatos^ [Indice];
end { TUaVector.GetElemento };

function TUaVector.GetTexto: String;
var
  I: Integer;
begin { TUaVector.GetTexto }
  Result := '[';
  for I := 1 to FDimension - 1 do
    Result := Result + FloatToStr(FDatos^ [I]) + ', ';
  Result := Result + FloatToStr(FDatos^ [FDimension]) + ']';
end { TUaVector.GetTexto };

procedure TUaVector.SetDimension(const ADimension: Integer);
var
  ADatos: PUaArregloReal;
  Count: LongInt;
begin { TUaVector.SetDimension }
  if (ADimension > 0) and (Dimension <= CUaDimensionMaximaVectores) then
  begin
    try
      if FDimension <> ADimension then
      begin
        if ADimension < FDimension then
          Count := ADimension * SizeOf(TUaReal)
        else
          Count := FDimension * SizeOf(TUaReal);

        ADatos := UaAsignarPUaArregloReal(ADimension * SizeOf(TUaReal));

        Move(FDatos^, ADatos^, Count);

        UaLiberarPUaArregloReal(FDatos, FDimension);

        FDatos := ADatos;
        FDimension := ADimension;
      end;
    except
      on E: Exception do
        raise EUaErrorVector.Create('TUaVector.SetDimension: Error redimensionando el vector (' + E.Message + ').');
    end;
  end
  else
    raise EUaErrorVector.Create('UaVector.Create: Dimension erronea (' + IntToStr(ADimension) + ').');
end { TUaVector.SetDimension };

procedure TUaVector.SetElemento(const Indice: Integer; const AReal: TUaReal);
begin { TUaVector.SetElemento }
  VerificarRangos(Indice, 'TUaVector.SetElemento');
  FDatos^ [Indice] := AReal;
end { TUaVector.SetElemento };

procedure TUaVector.VerificarRangos(const Indice: Integer; const Llamador: String);
begin { TUaVector.VerificarRangos }
  if Indice < 1 then
    raise EUaErrorVector.Create(Llamador + ': Indice fuera de rango (' + IntToStr(Indice) + ').');
  if Indice > FDimension then
    raise EUaErrorVector.Create(Llamador + ': Indice fuera de rango (' + IntToStr(Indice) + ').');
end { TUaVector.VerificarRangos };

function TUaVector.Asignar(const AReal: TUaReal): TUaVector;
var
  I: Integer;
begin { TUaVector.Asignar }
  for I := 1 to FDimension do
    FDatos^ [I] := AReal;

  Result := Self;
end { TUaVector.Asignar };

function TUaVector.Asignar(const Vector: TUaVector): TUaVector;
begin { TUaVector.Asignar }
  Assert(Assigned(Vector), 'TUaVector.Asignar: Assigned(Vector)');

  if FDimension = Vector.FDimension then
    Move(Vector.FDatos^, FDatos^, FDimension * SizeOf(TUaReal))
  else
    raise EUaErrorVector.Create('TUaVector.Asignar: Las dimensiones de los vectores son diferentes.');

  Result := Self;
end { TUaVector.Asignar };

function TUaVector.AsignarCero: TUaVector;
begin { TUaVector.AsignarCero }
  Asignar(0.0);

  Result := Self;
end { TUaVector.AsignarCero };

function TUaVector.Copia: TUaVector;
begin { TUaVector.Copia }
  Result := TUaVector.Create(Self);
end { TUaVector.Copia };

function TUaVector.Copiar(const Vector: TUaVector): TUaVector;
begin { TUaVector.Copiar }
  Assert(Assigned(Vector), 'TUaVector.Copiar: Assigned(Vector)');

  SetDimension(Vector.FDimension);
  Asignar(Vector);

  Result := Self;
end { TUaVector.Copiar };

constructor TUaVector.Create(const ADimension: Integer);
begin { TUaVector.Create }
  if (ADimension > 0) and (Dimension <= CUaDimensionMaximaVectores) then
  begin
    try
      FDatos := UaAsignarPUaArregloReal(ADimension * SizeOf(TUaReal));
      FDimension := ADimension;
    except
      on E: Exception do
        raise EUaErrorVector.Create('TUaVector.Create: Error instanciando el vector (' + E.Message + ').');
    end;
  end
  else
    raise EUaErrorVector.Create('UaVector.Create: Dimension erronea (' + IntToStr(ADimension) + ').');
end { TUaVector.Create };

constructor TUaVector.Create(const ADimension: Integer; const AReal: TUaReal);
begin { TUaVector.Create }
  Create(ADimension);
  Asignar(AReal);
end { TUaVector.Create };

constructor TUaVector.Create(const Vector: TUaVector);
begin {  TUaVector.Create }
  Assert(Assigned(Vector), 'TUaVector.Create: Assigned(Vector)');

  Create(Vector.FDimension);
  Asignar(Vector);
end {  TUaVector.Create };

destructor TUaVector.Destroy;
begin { TUaVector.Destroy }
  try
    UaLiberarPUaArregloReal(FDatos, FDimension);
  finally
    inherited Destroy;
  end;
end { TUaVector.Destroy };

function TUaVector.Dividir(const AReal: TUaReal): TUaVector;
var
  I: Integer;
begin { TUaVector.Dividir }
  if AReal <> 0.0 then
  begin
    for I := 1 to FDimension do
     FDatos^ [I] := FDatos^ [I] * AReal;
  end
  else
    raise EUaErrorVector.Create('TUaVector.Dividir: Division por cero.');

  Result := Self;
end { TUaVector.Dividir };

function TUaVector.Dividir(const Vector: TUaVector): TUaVector;
var
  I: Integer;
begin { TUaVector.Dividir }
  Assert(Assigned(Vector), 'TUaVector.Dividir: Assigned(Vector)');

  if FDimension = Vector.FDimension then
  begin
    try
      for I := 1 to FDimension do
        FDatos^ [I] := FDatos^ [I] / Vector.FDatos^ [I];
    except
      on E: EDivByZero do
        raise EUaErrorVector.Create('TUaVector.Dividir: Division por cero.');
    end;
  end
  else
    raise EUaErrorVector.Create('TUaVector.Dividir: Las dimensiones de los vectores son diferentes.');

  Result := Self;
end { TUaVector.Dividir };

function TUaVector.Division(const AReal: TUaReal): TUaVector;
begin { TUaVector.Division }
  Result := TUaVector.Create(Self);
  Result.Dividir(AReal);
end { TUaVector.Division };

function TUaVector.Division(const Vector: TUaVector): TUaVector;
begin { TUaVector.Division }
  Assert(Assigned(Vector), 'TUaVector.Division: Assigned(Vector)');

  Result := TUaVector.Create(Self);
  Result.Dividir(Vector);
end { TUaVector.Division };

function TUaVector.Multiplicar(const AReal: TUaReal): TUaVector;
var
  I: Integer;
begin { TUaVector.Multiplicar }
  for I := 1 to FDimension do
    FDatos^ [I] := FDatos^ [I] * AReal;

  Result := Self;
end { TUaVector.Multiplicar };

function TUaVector.Multiplicar(const Vector: TUaVector): TUaVector;
var
  I: Integer;
begin { TUaVector.Multiplicar }
  Assert(Assigned(Vector), 'TUaVector.Multiplicar: Assigned(Vector)');

  if FDimension = Vector.FDimension then
  begin
    for I := 1 to FDimension do
      FDatos^ [I] := FDatos^ [I] * Vector.FDatos^ [I];
  end
  else
    raise EUaErrorVector.Create('TUaVector.Multiplicar: Las dimensiones de los vectores son diferentes.');

  Result := Self;
end { TUaVector.Multiplicar };

function TUaVector.Norma1: TUaReal;
var
  I: Integer;
begin { TUaVector.Norma1 }
  Result := 0.0;
  for I := 1 to FDimension do
    Result := Result + Abs(FDatos^ [I]);
end { TUaVector.Norma1 };
  
function TUaVector.Norma2: TUaReal;
var
  I: Integer;
  S: TUaReal;
begin { TUaVector.Norma2 }
  S := 0.0;
  for I := 1 to FDimension do
    S := S + Sqr(Abs(FDatos^ [I]));
  Result := Sqrt(S);
end { TUaVector.Norma2 };
  
function TUaVector.NormaInfinito: TUaReal;
var
  I: Integer;
  M: TUaReal;
begin { TUaVector.NormaInfinito }
  M := Abs(FDatos^ [1]);
  for I := 2 to FDimension do
  begin
    if Abs(FDatos^ [I]) > M then
      M := Abs(FDatos^ [I]);
  end;
  Result := M;
end { TUaVector.NormaInfinito };

function TUaVector.Prefijo(const CantidadElementos: Integer): TUaVector;
begin { TUaVector.Prefijo }
  if (CantidadElementos > 0) or (CantidadElementos <= FDimension) then
  begin
    Result := TUaVector.Create(CantidadElementos);
    Move(FDatos^, Result.FDatos^, CantidadElementos * SizeOf(TUaReal));
  end
  else
    raise EUaErrorVector.Create('TUaVector.Prefijo: Cantidad de elementos invalida o mayor a la dimension del vector (' + IntToStr(CantidadElementos) + ').');
end { TUaVector.Prefijo };

function TUaVector.Producto(const AReal: TUaReal): TUaVector;
begin { TUaVector.Producto }
  Result := TUaVector.Create(Self);
  Result.Multiplicar(AReal);
end { TUaVector.Producto };

function TUaVector.Producto(const Vector: TUaVector): TUaVector;
begin { TUaVector.Producto }
  Assert(Assigned(Vector), 'TUaVector.Producto: Assigned(Vector)');

  Result := TUaVector.Create(Self);
  Result.Multiplicar(Vector);
end { TUaVector.Producto };

function TUaVector.ProductoPunto(const Vector: TUaVector): TUaReal;
var
  I: Integer;
begin { TUaVector.ProductoPunto }
  Assert(Assigned(Vector), 'TUaVector.ProductoPunto: Assigned(Vector)');

  if FDimension = Vector.FDimension then
  begin
    Result := 0.0;
    for I := 1 to FDimension do
      Result := Result + FDatos^ [I] * Vector.FDatos^ [I];
  end
  else
    raise EUaErrorVector.Create('TUaVector.ProductoPunto: Las dimensiones de los vectores son diferentes.');
end { TUaVector.ProductoPunto };

function TUaVector.Productoria: TUaReal;
var
  I: Integer;
begin { TUaVector.Productoria }
  Result := 1.0;
  for I := 1 to FDimension do
    Result := Result * FDatos^ [I];
end { TUaVector.Productoria };

function TUaVector.Resta(const AReal: TUaReal): TUaVector;
begin { TUaVector.Resta }
  Result := TUaVector.Create(Self);
  Result.Restar(AReal);
end { TUaVector.Resta };

function TUaVector.Resta(const Vector: TUaVector): TUaVector;
begin { TUaVector.Resta }
  Assert(Assigned(Vector), 'TUaVector.Resta: Assigned(Vector)');

  Result := TUaVector.Create(Self);
  Result.Restar(Vector);
end { TUaVector.Resta };

function TUaVector.Restar(const AReal: TUaReal): TUaVector;
var
  I: Integer;
begin { TUaVector.Restar }
  for I := 1 to FDimension do
    FDatos^ [I] := FDatos^ [I] - AReal;

  Result := Self;
end { TUaVector.Restar };

function TUaVector.Restar(const Vector: TUaVector): TUaVector;
var
  I: Integer;
begin { TUaVector.Restar }
  Assert(Assigned(Vector), 'TUaVector.Restar: Assigned(Vector)');

  if FDimension = Vector.FDimension then
  begin
    for I := 1 to FDimension do
      FDatos^ [I] := FDatos^ [I] - Vector.FDatos^ [I];
  end
  else
    raise EUaErrorVector.Create('TUaVector.Restar: Las dimensiones de los vectores son diferentes.');

  Result := Self;
end { TUaVector.Restar };

function TUaVector.Segmento(const IndiceInicio, IndiceFin: Integer): TUaVector;
var
  I: Integer;
  Vector: TUaVector;
begin { TUaVector.Segmento }
  if IndiceInicio < IndiceFin then
  begin
    if (IndiceInicio > 0) and (IndiceInicio <= FDimension) then
    begin
      if (IndiceFin > 0) and (IndiceFin <= FDimension) then
      begin
        Vector := TUaVector.Create(IndiceFin - IndiceInicio);
        for I := IndiceInicio to IndiceFin do
          Vector.FDatos^ [I - IndiceInicio] := FDatos^ [I];
      end
      else
        raise EUaErrorVector.Create('TUaVector.Segmento: El indice de fin de segmento esta fuera de rango (' + IntToStr(IndiceFin) + ').');
    end
    else
      raise EUaErrorVector.Create('TUaVector.Segmento: El indice de inicio de segmento esta fuera de rango (' + IntToStr(IndiceInicio) + ').');
  end
  else
    raise EUaErrorVector.Create('TUaVector.Segmento: El indice de inicio (' + IntToStr(IndiceInicio) + ') es mayor al indice de fin de segmento (' + IntToStr(IndiceFin) + ').');

  Result := Vector;
end { TUaVector.Segmento };

function TUaVector.Suma(const AReal: TUaReal): TUaVector;
begin { TUaVector.Suma }
  Result := TUaVector.Create(Self);
  Result.Sumar(AReal);
end { TUaVector.Suma };

function TUaVector.Suma(const Vector: TUaVector): TUaVector;
begin { TUaVector.Suma }
  Assert(Assigned(Vector), 'TUaVector.Suma: Assigned(Vector)');

  Result := TUaVector.Create(Self);
  Result.Sumar(Vector);
end { TUaVector.Suma };

function TUaVector.Sumar(const AReal: TUaReal): TUaVector;
var
  I: Integer;
begin { TUaVector.Sumar }
  for I := 1 to FDimension do
    FDatos^ [I] := FDatos^ [I] + AReal;

  Result := Self;
end { TUaVector.Sumar };

function TUaVector.Sumar(const Vector: TUaVector): TUaVector;
var
  I: Integer;
begin { TUaVector.Sumar }
  Assert(Assigned(Vector), 'TUaVector.Sumar: Assigned(Vector)');

  if FDimension = Vector.FDimension then
  begin
    for I := 1 to FDimension do
      FDatos^ [I] := FDatos^ [I] + Vector.FDatos^ [I];
  end
  else
    raise EUaErrorVector.Create('TUaVector.Sumar: Las dimensiones de los vectores son diferentes.');

  Result := Self;
end { TUaVector.Sumar };

function TUaVector.Sumatoria: TUaReal;
var
  I: Integer;
begin { TUaVector.Sumatoria }
  Result := 0.0;
  for I := 1 to FDimension do
    Result := Result + FDatos^ [I];
end { TUaVector.Sumatoria };

end { UaVector}.
