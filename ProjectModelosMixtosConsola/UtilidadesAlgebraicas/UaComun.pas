{**
@abstract(Definiciones comunes de Utilidades Algebraicas.)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 29, 2004)
@lastmod(December 2, 2004)
Este modulo contiene definiciones de tipos y constantes utlizados por el resto
de las unidades de Utilidades Algebraicas.
}
unit UaComun;

interface

uses
  SysUtils;

type
  {** Tipo elemental de la libreria Utilidades Algebraicas. Es el componente
      basico de las estructuras complejas como vectores y matrices.
      @abstract(Tipo elemental de Utilidades Algebraicas.) }
  TUaReal = Double;

  {** Tipo entero de la libreria Utilidades Algebraicas. }
  TUaEntero = Integer;

  {** Arreglo de @link(TUaReal). Se utiliza como contenedor para vectores.
      @abstract(Arreglo de @link(TUaReal). Contenedor para otras estructuras.) }
  TUaArregloReal = array [1 .. MaxInt div 16] of TUaReal;

  {** Puntero a @link(TUaArregloReal). Se utiliza para la asignacion dinamica
      de memoria de los contenedores de vectores.
      @abstract(Puntero a @link(TUaArregloReal). Utilizado para la asignacion
      dinamica de contenedores de vectores.) }
  PUaArregloReal = ^TUaArregloReal;

  {** Arreglo de @link(TUaArregloReal). Se utiliza como contenedor para
      matrices. Las matrices se almacenan como arreglos de filas.
      @abstract(Puntero a @link(TUaArregloReal). Utilizado para el
      almacenamiento de matrices.) }
  TUaArregloArregloReal = array [1 .. MaxInt div 16] of PUaArregloReal;

  {** Puntero a @link(TUaArregloArregloReal). Se utiliza para la asignacion
      dinamica de memoria de los contenedores de matrices.
      @abstract(Puntero a @link(TUaArregloArregloReal). Utilizado para la
      asignacion dinamica de contenedores.) }
  PUaArregloArregloReal = ^TUaArregloArregloReal;

  {** Arreglo de @link(TUaEntero). En general es utilizado como contenedor para
      vectores de indices.
      @abstract(Arreglo de @link(TUaEntero).) }
  TUaArregloEntero = array [1 .. MaxInt div 16] of TUaEntero;

  {** Puntero a @link(TUaArregloEntero). Se utiliza para la asignacion dinamica
      de memoria de los contenedores.
      @abstract(Puntero a @link(TUaArregloEntero). Utilizado para la asignacion
      dinamica de contenedores.) }
  PUaArregloEntero = ^TUaArregloEntero;

  {** Excepcion basica de Unidades Algebraicas. Todas las otras excepciones son
      descendientes de esta.
      @abstract(Excepcion basica de Unidades Algebraicas.) }
  EUaError = class(Exception);

  {** Asigna una porcion de memoria para un arreglo de @link(TUaReal).
      @param(CantidadElementos Cantidad de elementos del arreglo dinamico.)
      @returns(Puntero a una posicion de memoria conteniendo un arreglo de
      @link(TUaReal) de CantidadElementos elementos.) }
  function UaAsignarPUaArregloReal(const CantidadElementos: Integer): PUaArregloReal;
  {** Libera la memoria asignada a un arreglo de @link(TUaReal).
      @param(Arreglo Puntero al arreglo a liberar.)
      @param(CantidadElementos Cantidad de elementos del arreglo.) }
  procedure UaLiberarPUaArregloReal(var Arreglo: PUaArregloReal; const CantidadElementos: Integer);
  {** Copia una cierta cantidad de elementos de un arreglo de @link(TUaReal) a
      otro.
      @param(ArregloOrigen Puntero al arreglo de origen.)
      @param(ArregloDestino Puntero al arreglo de destino.)
      @param(CantidadElementos Cantidad de elementos a copiar.) }
  procedure UaCopiarPUaArregloReal(const ArregloOrigen: PUaArregloReal; var ArregloDestino: PUaArregloReal; const CantidadElementos: Integer);
  {** Asigna una porcion de memoria para un arreglo de arreglos de
      @link(TUaReal).
      @param(CantidadArreglos Cantidad de arreglos.)
      @param(CantidadElementosArreglo Cantidad de @link(TUaReal) por arreglo.)
      @returns(Puntero a una posicion de memoria conteniendo un arreglo de
      CantidadArreglos de arreglos de @link(TUaReal) de CantidadElementoArreglo
      elementos.) }
  function UaAsignarPUaArregloArregloReal(const CantidadArreglos, CantidadElementosArreglo: Integer): PUaArregloArregloReal;
  {** Libera la memoria asignada a un arreglo de arreglos de @link(TUaReal).
      @param(Arreglo Puntero al arreglo a liberar.)
      @param(CantidadArreglos Cantidad de arreglos de @link(TUaReal) en
      Arreglo.)
      @param(CantidadElementosArreglo Cantidad de elementos por cada arreglo
      almacenado.) }
  procedure UaLiberarPUaArregloArregloReal(var Arreglo: PUaArregloArregloReal; const CantidadArreglos, CantidadElementosArreglo: Integer);
  {** Copia una cierta cantidad de elementos de un arreglo de arreglos de
      @link(TUaReal) a otro.
      @param(ArregloOrigen Puntero al arreglo de origen.)
      @param(ArregloDestino Puntero al arreglo de destino.)
      @param(CantidadArreglos Cantidad de arreglos a copiar.)
      @param(CantidadElementosArreglo Cantidad de elementos a de cada arreglo
      copiar.) }
  procedure UaCopiarPUaArregloArregloReal(const ArregloOrigen: PUaArregloArregloReal; var ArregloDestino: PUaArregloArregloReal; const CantidadArreglos, CantidadElementosArreglo: Integer);
  {** Asigna una porcion de memoria para un arreglo de @link(TUaEntero).
      @param(CantidadElementos Cantidad de elementos del arreglo dinamico.)
      @returns(Puntero a una posicion de memoria conteniendo un arreglo de
      @link(TUaEntero) de CantidadElementos elementos.) }
  function UaAsignarPUaArregloEntero(const CantidadElementos: Integer): PUaArregloEntero;
  {** Libera la memoria asignada a un arreglo de @link(TUaEntero).
      @param(Arreglo Puntero al arreglo a liberar.)
      @param(CantidadElementos Cantidad de elementos del arreglo.) }
  procedure UaLiberarPUaArregloEntero(var Arreglo: PUaArregloEntero; const CantidadElementos: Integer);

  {** Calcula el minimo entre dos enteros.
      @param(I Entero a comparar.)
      @param(J Entero a comparar.)
      @returns(El minimo entre I y J.) }
  function UaMinimo(const I, J: TUaEntero): TUaEntero; overload;

  {** Calcula el maximo entre dos enteros.
      @param(I Entero a comparar.)
      @param(J Entero a comparar.)
      @returns(El maximo entre I y J.) }
  function UaMaximo(const I, J: TUaEntero): TUaEntero; overload;

  {** Calcula el minimo entre dos reales.
      @param(X Real a comparar.)
      @param(Y Real a comparar.)
      @returns(El minimo entre I y J.) }
  function UaMinimo(const X, Y: TUaReal): TUaReal; overload;

  {** Calcula el maximo entre dos reales.
      @param(X Real a comparar.)
      @param(Y Real a comparar.)
      @returns(El maximo entre I y J.) }
  function UaMaximo(const X, Y: TUaReal): TUaReal; overload;

  {** Indica el signo de un numero real.
      @param(X Numero real.)
      @returns(1 si X es positivo, -1 si X es negativo y 0 si X es cero.) }
  function UaSigno(const X: TUaReal): TUaReal; overload;

  {** Calcula el signo de un numero real por su valor absoluto, en funcion del
      signo de otro numero real. 
      @param(X Numero real.)
      @param(Y Numero real.)
      @returns(Abs(X) si Y es positivo, -Abs(X) si Y es negativo y 0 si Y es
      cero.) }
  function UaSigno(const X, Y: TUaReal): TUaReal; overload;

  {** Basado en Numerical Recipes in C - 2nd. Edition.
      Calcula Sqrt(X^2 + Y^2) ebitando problemas de representacion de numeros
      reales.
      @param(X Numero real.)
      @param(Y Numero real.)
      @returns(Sqrt(X^2 + Y^2).) }
  function UaPitagoras(const X, Y: TUaReal): TUaReal;

implementation

function UaAsignarPUaArregloReal(const CantidadElementos: Integer): PUaArregloReal;
begin { UaAsignarPUaArregloReal }
  try
    Result := AllocMem(CantidadElementos * SizeOf(TUaReal));
  except
    on E: Exception do
      raise EUaError.Create('UaAsignarPUaArregloReal: ' + E.Message);
  end;
end { UaAsignarPUaArregloReal };

procedure UaLiberarPUaArregloReal(var Arreglo: PUaArregloReal; const CantidadElementos: Integer);
begin { UaLiberarPUaArregloReal }
  try
    if Assigned(Arreglo) then
    begin
      FreeMem(Arreglo, CantidadElementos * SizeOf(TUaReal));
      Arreglo := nil;
    end;
  except
    on E: Exception do
      raise EUaError.Create('UaLiberarPUaArregloReal: ' + E.Message);
  end;
end { UaLiberarPUaArregloReal };

procedure UaCopiarPUaArregloReal(const ArregloOrigen: PUaArregloReal; var ArregloDestino: PUaArregloReal; const CantidadElementos: Integer);
begin { UaCopiarPUaArregloReal }
  try
    Move(ArregloOrigen^, ArregloDestino^, CantidadElementos * SizeOf(TUaReal));
  except
    on E: Exception do
      raise EUaError.Create('UaCopiarPUaArregloReal: ' + E.Message);
  end;
end { UaCopiarPUaArregloReal };

function UaAsignarPUaArregloArregloReal(const CantidadArreglos, CantidadElementosArreglo: Integer): PUaArregloArregloReal;
var
  I: Integer;
begin { UaAsignarPUaArregloArregloReal }
  try
    Result := AllocMem(CantidadArreglos * SizeOf(PUaArregloReal));
    for I := 1 to CantidadArreglos do
      Result^ [I] := AllocMem(CantidadElementosArreglo * SizeOf(TUaReal));
  except
    on E: Exception do
      raise EUaError.Create('UaAsignarPUaArregloArregloReal: ' + E.Message);
  end;
end { UaAsignarPUaArregloArregloReal };

procedure UaLiberarPUaArregloArregloReal(var Arreglo: PUaArregloArregloReal; const CantidadArreglos, CantidadElementosArreglo: Integer);
var
  I: Integer;
begin { LiberarPUaArregloArregloReal }
  try
    if Assigned(Arreglo) then
    begin
      for I := 1 to CantidadArreglos do
      begin
        Assert(Assigned(Arreglo^ [I]), 'UaLiberarPUaArregloArregloReal: Assigned(Arreglo^ [I])');
        FreeMem(Arreglo^ [I], CantidadElementosArreglo * SizeOf(TUaReal));
      end;
      FreeMem(Arreglo, CantidadArreglos * SizeOf(PUaArregloReal));
      Arreglo := nil;
    end;
  except
    on E: Exception do
      raise EUaError.Create('UaLiberarPUaArregloArregloReal: ' + E.Message);
  end;
end { LiberarPUaArregloArregloReal };

procedure UaCopiarPUaArregloArregloReal(const ArregloOrigen: PUaArregloArregloReal; var ArregloDestino: PUaArregloArregloReal; const CantidadArreglos, CantidadElementosArreglo: Integer);
var
  I: Integer;
begin { UaCopiarPUaArregloArregloReal }
  Assert(Assigned(ArregloOrigen), 'UaCopiarPUaArregloArregloReal: Assigned(ArregloOrigen)');
  Assert(Assigned(ArregloDestino), 'UaCopiarPUaArregloArregloReal: Assigned(ArregloDestino');

  try
    for I := 1 to CantidadArreglos do
      Move(ArregloOrigen^ [I]^, ArregloDestino^ [I]^, CantidadElementosArreglo * SizeOf(TUaReal));
  except
    on E: Exception do
      raise EUaError.Create('UaCopiarPUaArregloArregloReal: ' + E.Message);
  end;
end { UaCopiarPUaArregloArregloReal };

function UaAsignarPUaArregloEntero(const CantidadElementos: Integer): PUaArregloEntero;
begin { UaAsignarPUaArregloEntero }
  try
    Result := AllocMem(CantidadElementos * SizeOf(TUaEntero));
  except
    on E: Exception do
      raise EUaError.Create('UaAsignarPUaArregloEntero: ' + E.Message);
  end;
end { UaAsignarPUaArregloEntero };

procedure UaLiberarPUaArregloEntero(var Arreglo: PUaArregloEntero; const CantidadElementos: Integer);
begin { UaLiberarPUaArregloEntero }
  try
    if Assigned(Arreglo) then
    begin
      FreeMem(Arreglo, CantidadElementos * SizeOf(TUaEntero));
      Arreglo := nil;
    end;
  except
    on E: Exception do
      raise EUaError.Create('UaLiberarPUaArregloEntero: ' + E.Message);
  end;
end { UaLiberarPUaArregloEntero };

function UaMinimo(const I, J: TUaEntero): TUaEntero;
begin { UaMinimo }
  if I < J then
    Result := I
  else
    Result := J;
end { UaMinimo };

function UaMaximo(const I, J: TUaEntero): TUaEntero;
begin { UaMaximo }
  if I > J then
    Result := I
  else
    Result := J;
end { UaMaximo };

function UaMinimo(const X, Y: TUaReal): TUaReal;
begin { UaMinimo }
  if X < Y then
    Result := X
  else
    Result := Y;
end { UaMinimo };

function UaMaximo(const X, Y: TUaReal): TUaReal;
begin { UaMaximo }
  if X > Y then
    Result := X
  else
    Result := Y;
end { UaMaximo };

function UaSigno(const X: TUaReal): TUaReal;
begin { UaSigno }
  if X < 0 then
    Result := -1
  else if X > 0 then
    Result := 1
  else
    Result := 0;
end { UaSigno };

function UaSigno(const X, Y: TUaReal): TUaReal;
begin { UaSigno }
  if Y >= 0.0 then
    Result := Abs(X)
  else
    Result := -Abs(X);
end { UaSigno };

function UaPitagoras(const X, Y: TUaReal): TUaReal;
var
   at,bt: real;
begin { UaPitagoras }
  at := Abs(X);
  bt := Abs(Y);
  if at > bt then
    Result := at * Sqrt(1.0 + Sqr(bt / at))
  else
  begin
    if bt = 0.0 then
      Result := 0.0
    else
      Result := bt * Sqrt(1.0 + Sqr(at / bt));
  end;
end { UaPitagoras };

end { UaComun }.
