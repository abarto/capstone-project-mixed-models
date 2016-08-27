{**
@abstract(Subrutinas algebraicas.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de subrutinas algebraicas basadas en los
diferentes metodos necesarios para el ajuste de los modelos, pero que son
demasiado especificos para su inclusion en la libreria de Utilidades
Algebraicas.
}
unit UnitSubrutinasAlgebraicas;

interface

uses
  SysUtils,
  UaComun, UaConstantes, UaMatriz, UaMiscelanea, UaVector;

{** Verifica si una matriz es positiva definida intentando construir la
    descomposicion de Sholesky de la misma.
    @param(Matriz Matriz que deseamos verificar.)
    @param(Tolerancia Tolerancia de la descomposicion de Cholesky.)
    @returns(@true si la matriz es positiva definida, o @false en caso
    contrario.) }
function EsPositivaDefinida(const Matriz: TUaMatriz; const Tolerancia: TUaReal): Boolean;
{** Verifica si una matriz es positiva semi definida intentando construir la
    descomposicion de Sholesky de la misma.
    @param(Matriz Matriz que deseamos verificar.)
    @param(Tolerancia Tolerancia de la descomposicion de Cholesky.)
    @returns(@true si la matriz es positiva definida, o @false en caso
    contrario.) }
function EsPositivaSemiDefinida(const Matriz: TUaMatriz; const Tolerancia: TUaReal): Boolean;
{** Basado en codigo de SAS Technical Report R-106 "The Sweep Operator: Its
    importante in Statistical Computing". Aplica el operador SWEEP a una columna
    de una matriz.
    @param(Matriz Matriz a la cual aplicar el operador SWEEP.)
    @param(K Columna de Matriz a la cual aplicar el operador SWEEP.)
    @param(Tolerancia Tolerancia a perturbaciones a utilizar durante la
    aplicacion del operador.) }
procedure G2SweepColumna(var Matriz: TUaMatriz; const K: Integer; const Tolerancia: TUaReal); overload;
{** Basado en codigo de SAS Technical Report R-106 "The Sweep Operator: Its
    importante in Statistical Computing". Calcula la inversa generalizada de
    una matriz definida positiva.
    @param(Matriz Matriz de la cual se desea conocer su inversa generalizada.)
    @param(Tolerancia Tolerancia a perturbaciones a utilizar durante la
    aplicacion del operador G2SWEEP.)
    @returns(Inversa generalizada de Matriz.) }
function InversaGeneralizadaG2Sweep(const Matriz: TUaMatriz; const Tolerancia: TUaReal): TUaMatriz;
{** Basado en algoritmos de Matrix Computations 3rd. Edition.
    Calcula la matriz triangular inferior L de la descomposicion de Cholesky
    de una matriz definida positiva. Simultaneamente puede utilizarse para
    detectar si una matriz es positiva definida.
    @param(Matriz Matriz positiva definida de la cual se desea conocer su
    descomposicion de Cholesky.)
    @param(Descomposicion Descomposicion triangular inferior de Cholesky del
    parametro Matriz. Si la matriz no es positiva definida el valor devuelto es
    @nil.)
    @param(Tolerancia Tolerancia de la descomposicion.)
    @returns(@true si la matriz es positiva semi-definida, o @false en caso
    contrario.) }
function DescomposicionCholesky(const Matriz: TUaMatriz; var Descomposicion: TUaMatriz; const Tolerancia: TUaReal): Boolean;
{** Basado en algoritmos de Matrix Computations 3rd. Edition.
    Calcula la matriz triangular inferior L de la descomposicion de Cholesky
    de una matriz positiva semi-definida.  Simultaneamente puede utilizarse para
    detectar si una matriz es positiva semi-definida.
    @param(Matriz Matriz positiva semi-definida de la cual se desea conocer su
    descomposicion de Cholesky.)
    @param(Descomposicion Descomposicion triangular inferior de Cholesky del
    parametro Matriz. Si la matriz no es positiva definida el valor devuelto es
    @nil.)
    @param(Tolerancia Tolerancia de la descomposicion.)
    @returns(@true si la matriz es positiva semi-definida, o @false en caso
    contrario.) }
function DescomposicionCholeskyPositivaSemiDefinida(const Matriz: TUaMatriz; var Descomposicion: TUaMatriz; const Tolerancia: TUaReal): Boolean;
{** Basado en Numerical Recipes in C - 2nd. Edition

    Resuelve el sistema de acuaciones lineales A.x = b. La matriz A debe ser una
    matriz cuadrada y b debe tener igual dimension que la cantidad de filas de
    A. No destruye ni A, ni b.
    @param(A Matriz del lado izquierdo de la ecuacion.)
    @param(b Vector con el lado derecho de la ecuacion.)
    @returns(Vector x con la solucion de A.x = b.) }
function ResolverSistemaEcuacionesLineales(const A: TUaMatriz; const b: TUaVector): TUaVector;
{** Basado en Numerical Recipes in C - 2nd. Edition

    Resuelve el sistema de acuaciones lineales A.x = b. La matriz A debe ser una
    matriz cuadrada y b debe tener igual dimension que la cantidad de filas de
    A. Este procedimiento destruye A y b.

    @param(A Matriz del lado izquierdo de la ecuacion.)
    @param(b Vector con el lado derecho de la ecuacion y solucion de la
    ecuacion luego de la ejecucion.) }
procedure ResolverSistemaEcuacionesLinealesEn(var A: TUaMatriz; var b: TUaVector);

implementation

function EsPositivaDefinida(const Matriz: TUaMatriz; const Tolerancia: TUaReal): Boolean;
var
  M: TUaMatriz;
begin { EsPositivaDefinida }
  Assert(Assigned(Matriz), 'EsPositivaDefinida: Assigned(Matriz)');
  M := nil;
  Result := DescomposicionCholesky(Matriz, M, Tolerancia);
  FreeAndNil(M);
end { EsPositivaDefinida };

function EsPositivaSemiDefinida(const Matriz: TUaMatriz; const Tolerancia: TUaReal): Boolean;
var
  M: TUaMatriz;
begin { EsPositivaSemiDefinida }
  Assert(Assigned(Matriz), 'EsPositivaSemiDefinida: Assigned(Matriz)');
  M := nil;
  Result := DescomposicionCholeskyPositivaSemiDefinida(Matriz, M, Tolerancia);
  FreeAndNil(M);
end { EsPositivaSemiDefinida };

procedure G2SweepColumna(var Matriz: TUaMatriz; const K: Integer; const Tolerancia: TUaReal); overload;
var
  B, D: TUaReal;
  I, J: Integer;
  MatrizDatos: PUaArregloArregloReal;
begin { G2SweepColumna }
  Assert(Assigned(Matriz), 'G2SweepColumna: Assigned(Matriz)');
  Assert((K >= 1) and (K <= Matriz.CantidadColumnas), 'G2SweepColumna: (K >= 1) and (K <= Matriz.CantidadColumnas)');

  MatrizDatos := Matriz.Datos;

  // Sea D = Matriz [K, K]. Si D es menor que la tolerancia a perturbaciones,
  // poner a cero la K-esimna fila y columna y terminar. De otro modo,
  // continuar aplicando Sweep a la K-esima columna.
  D := MatrizDatos^ [K]^ [K];
  if (D = 0.0) or (D < Tolerancia) then
  begin
    // Poner a cero la K-esima fila y columna.
    Matriz.AsignarFila(K, 0.0);
    Matriz.AsignarColumna(K, 0.0);
  end
  else
  begin
    // Aplicar Sweep a la K-esima columna.

    // Dividir la K-esima fila por D.
    for I := 1 to Matriz.CantidadColumnas do
      MatrizDatos^ [K]^ [I] := MatrizDatos^ [K]^ [I] / D;

    // Para cada fila I distinta de K, sea B = Matriz [I, K], restar b veces
    // la fila K de la fila I y hacer Matriz [I, K] = -B / D.
    for I := 1 to Matriz.CantidadFilas do
    begin
      if I <> K then
      begin
        B := MatrizDatos^ [I]^ [K];
        for J := 1 to Matriz.CantidadColumnas do
          MatrizDatos^ [I]^ [J] := MatrizDatos^ [I]^ [J] - B * MatrizDatos^ [K]^ [J];
        MatrizDatos^ [I]^ [K] := - B / D;
      end
    end;

    // Poner Matriz [K, K] = 1 / D.
    MatrizDatos^ [K]^ [K] := 1 / D;
  end;
end { G2SweepColumna };

function InversaGeneralizadaG2Sweep(const Matriz: TUaMatriz; const Tolerancia: TUaReal): TUaMatriz;
var
  R: TUaMatriz;
  RDatos: PUaArregloArregloReal;
  Diagonal: TUaVector;
  B, D: TUaReal;
  I, J, K: Integer;
begin { InversaGeneralizadaG2Sweep }
  Assert(Assigned(Matriz), 'InversaGeneralizadaG2Sweep: Assigned(Matriz)');

  R := TUaMatriz.Create(Matriz);
  RDatos := R.Datos;

  // Construir vector con la diagonal principal de la matriz para utilizar sus
  // elementos como factores de la tolerancia.
  Diagonal := Matriz.Diagonal;

  // Aplicar G2SWEEP a cada columna de R
  for K := 1 to R.CantidadColumnas do
  begin
    // Sea D = Matriz [K, K]. Si D es menor que la tolerancia a perturbaciones,
    // poner a cero la K-esimna fila y columna y terminar. De otro modo,
    // continuar aplicando Sweep a la K-esima columna.
    D := RDatos^ [K]^ [K];
    if D < Diagonal [K] * Tolerancia then
    begin
      // Poner a cero la K-esima fila y columna.
      R.AsignarFila(K, 0.0);
      R.AsignarColumna(K, 0.0);
    end
    else
    begin
      // Aplicar Sweep a la K-esima columna.

      // Dividir la K-esima fila por D.
      for I := 1 to R.CantidadColumnas do
        RDatos^ [K]^ [I] := RDatos^ [K]^ [I] / D;

      // Para cada fila I distinta de K, sea B = Matriz [I, K], restar b veces
      // la fila K de la fila I y hacer Matriz [I, K] = -B / D.
      for I := 1 to R.CantidadFilas do
      begin
        if I <> K then
        begin
          B := RDatos^ [I]^ [K];
          for J := 1 to Matriz.CantidadColumnas do
            RDatos^ [I]^ [J] := RDatos^ [I]^ [J] - B * RDatos^ [K]^ [J];
          RDatos^ [I]^ [K] := - B / D;
        end
      end;

      // Poner Matriz [K, K] = 1 / D.
      RDatos^ [K]^ [K] := 1 / D;
    end;
  end;

  // Liberar diagonal
  FreeAndNil(Diagonal);

  Result := R;
end { InversaGeneralizadaG2Sweep };

function DescomposicionCholesky(const Matriz: TUaMatriz; var Descomposicion: TUaMatriz; const Tolerancia: TUaReal): Boolean;
var
  I, J, K: Integer;
  X: TUaReal;
  MatrizDatos, DescomposicionDatos: PUaArregloArregloReal;
  V: TUaVector;
  VDatos: PUaArregloReal;
begin { DescomposicionCholesky }
  Assert(Assigned(Matriz), 'DescomposicionCholesky: Assigned(Matriz)');
  Assert(not Assigned(Descomposicion), 'DescomposicionCholesky: not Assigned(Descomposicion)');

  Descomposicion := TUaMatriz.Create(Matriz.CantidadFilas, Matriz.CantidadColumnas);
  V := TUaVector.Create(Matriz.CantidadColumnas);

  DescomposicionDatos := Descomposicion.Datos;
  MatrizDatos := Matriz.Datos;
  VDatos := V.Datos;

  Result := True;
  I := 1;
  while (I <= Matriz.CantidadColumnas) and Result do
  begin
//    if MatrizDatos^ [I]^ [I] > 0 then
    if MatrizDatos^ [I]^ [I] > MatrizDatos^ [I]^ [I] * Tolerancia then
    begin
      for J := I to Matriz.CantidadColumnas do
        VDatos^ [J] := MatrizDatos^ [J]^ [I];

      for J := 1 to I - 1 do
      begin
        for K := I to Matriz.CantidadColumnas do
          VDatos^ [K] := VDatos^ [K] - DescomposicionDatos^ [I]^ [J] * DescomposicionDatos^ [K]^ [J];
      end;

      if VDatos^ [I] > 0 then
      begin
        X := Sqrt(VDatos^ [I]);

        if X > MatrizDatos^ [I]^ [I] * Tolerancia then
        begin
          for J := I to Matriz.CantidadColumnas do
            DescomposicionDatos^ [J]^ [I] := VDatos^ [J] / X;
        end
        else
          Result := False;
      end
      else
        Result := False;

      Inc(I);
    end
    else
      Result := False;
  end;

  FreeAndNil(V);

  // Liberar la descomposicion si la matriz no es positiva definida
  if not Result then
    FreeAndNil(Descomposicion);
end { DescomposicionCholesky };

function DescomposicionCholeskyPositivaSemiDefinida(const Matriz: TUaMatriz; var Descomposicion: TUaMatriz; const Tolerancia: TUaReal): Boolean;
var
  I, J, K: Integer;
  X: TUaReal;
  MatrizDatos, DescomposicionDatos: PUaArregloArregloReal;
  V: TUaVector;
  VDatos: PUaArregloReal;
begin { DescomposicionCholeskyPositivaSemiDefinida }
  Assert(Assigned(Matriz), 'DescomposicionCholeskyPositivaSemiDefinida: Assigned(Matriz)');
  Assert(not Assigned(Descomposicion), 'DescomposicionCholeskyPositivaSemiDefinida: not Assigned(Descomposicion)');

  Descomposicion := TUaMatriz.Create(Matriz.CantidadFilas, Matriz.CantidadColumnas);
  V := TUaVector.Create(Matriz.CantidadColumnas);

  DescomposicionDatos := Descomposicion.Datos;
  MatrizDatos := Matriz.Datos;
  VDatos := V.Datos;

  Result := True;
  I := 1;
  while (I <= Matriz.CantidadColumnas) and Result do
  begin
//    if MatrizDatos^ [I]^ [I] > 0 then
    if MatrizDatos^ [I]^ [I] > MatrizDatos^ [I]^ [I] * Tolerancia then
    begin
      for J := I to Matriz.CantidadColumnas do
        VDatos^ [J] := MatrizDatos^ [J]^ [I];

      for J := 1 to I - 1 do
      begin
        for K := I to Matriz.CantidadColumnas do
          VDatos^ [K] := VDatos^ [K] - DescomposicionDatos^ [I]^ [J] * DescomposicionDatos^ [K]^ [J];
      end;

      if VDatos^ [I] > 0 then
      begin
        X := Sqrt(VDatos^ [I]);

        if X > MatrizDatos^ [I]^ [I] * Tolerancia then
        begin
          for J := I to Matriz.CantidadColumnas do
            DescomposicionDatos^ [J]^ [I] := VDatos^ [J] / X;
        end
        else
          Result := False;
      end
      else
        Result := False;
    end;

    Inc(I);
  end;

  FreeAndNil(V);

  // Liberar la descomposicion si la matriz no es positiva semi-definida
  if not Result then
    FreeAndNil(Descomposicion);
end { DescomposicionCholeskyPositivaSemiDefinida };

function ResolverSistemaEcuacionesLineales(const A: TUaMatriz; const b: TUaVector): TUaVector;
var
  Vector: TUaVector;
  A1: PUaArregloArregloReal;
  b1, ADatos: PUaArregloReal;
  indx: PUaArregloEntero;
  d: TUaReal;
begin { ResolverSistemaEcuacionesLineales }
  Assert(Assigned(A), 'ResolverSistemaEcuacionesLineales: Assigned(A)');
  Assert(Assigned(b), 'ResolverSistemaEcuacionesLineales: Assigned(b)');

  if A.CantidadFilas = A.CantidadColumnas then
  begin
    A1 := UaAsignarPUaArregloArregloReal(A.CantidadFilas, A.CantidadColumnas);
    b1 := UaAsignarPUaArregloReal(A.CantidadFilas);
    indx := UaAsignarPUaArregloEntero(A.CantidadFilas);

    UaCopiarPUaArregloArregloReal(A.Datos, A1, A.CantidadFilas, A.CantidadColumnas);
    UaCopiarPUaArregloReal(b.Datos, b1, A.CantidadFilas);

    ludcmp(A1, A.CantidadFilas, indx, d);
    lubksb(A1, A.CantidadFilas, indx, b1);

    UaLiberarPUaArregloArregloReal(A1, A.CantidadFilas, A.CantidadColumnas);
    UaLiberarPUaArregloEntero(indx, A.CantidadFilas);

    Vector := TUaVector.Create(A.CantidadFilas);
    ADatos := Vector.Datos;
    UaCopiarPUaArregloReal(b1, ADatos, A.CantidadFilas);
  end
  else
    raise EUaError.Create('ResolverSistemaEcuacionesLineales: Solo puede llamarse esta subrutina para sistemas de igual cantidad de incognitas y ecuaciones.');

  Result := Vector;
end { ResolverSistemaEcuacionesLineales };

procedure ResolverSistemaEcuacionesLinealesEn(var A: TUaMatriz; var b: TUaVector);
var
  ADatos: PUaArregloArregloReal;
  BDatos: PUaArregloReal;
  indx: PUaArregloEntero;
  d: TUaReal;
begin { ResolverSistemaEcuacionesLinealesEn }
  Assert(Assigned(A), 'ResolverSistemaEcuacionesLinealesEn: Assigned(A)');
  Assert(Assigned(b), 'ResolverSistemaEcuacionesLinealesEn: Assigned(b)');

  if A.CantidadFilas = A.CantidadColumnas then
  begin
    indx := UaAsignarPUaArregloEntero(A.CantidadFilas);
    ADatos := A.Datos;
    BDatos := b.Datos;
    ludcmp(ADatos, A.CantidadFilas, indx, d);
    lubksb(ADatos, A.CantidadFilas, indx, BDatos);
    UaLiberarPUaArregloEntero(indx, A.CantidadFilas);
  end
  else
    raise EUaError.Create('ResolverSistemaEcuacionesLinealesEn: Solo puede llamarse esta subrutina para sistemas de igual cantidad de incognitas y ecuaciones.');
end { ResolverSistemaEcuacionesLinealesEn };

end { UnitSubrutinasAlgebraicas }.
