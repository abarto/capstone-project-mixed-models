{**
@abstract(Subrutinas asociadas a estadisticos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(March 12, 2005)
@lastmod(March 12, 2005)
Este modulo contiene constantes, funciones y procedimientos utilizados
para el calculo de estadisticos.
}

unit UnitSubrutinasEstadisticas;

interface

uses
  Math,
  UaComun;

const
  {** Constante cof (originalmente un arreglo estico local) de la funcion
      gammln. (Ver Numerical Recipes in C. 2nd Edition.) }
  gammln_cof: array [0..5] of TUaReal = (
    76.18009172947146,
    -86.50532032941677,
    24.01409824083091,
    -1.231739572450155,
    0.1208650973866179e-2,
    -0.5395239384953e-5 );
  {** Constante MAXIT de la function betacf. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  betacf_MAXIT: Integer = 100;
  {** Constante EPS de la function betacf. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  betacf_EPS: TUaReal = 3.0e-7;
  {** Constante FPMIN de la function betacf. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  betacf_FPMIN: TUaReal = 1.0e-30;
  {** Constante ITMAX de la funcion gser. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  gser_ITMAX: Integer = 100;
  {** Constante EPS de la funcion gser. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  gser_EPS: TUaReal = 3.0E-7;
  {** Constante ITMAX de la funcion gcf. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  gcf_ITMAX: Integer = 100;
  {** Constante EPS de la funcion gcf. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  gcf_EPS: TUaReal = 3.0E-7;
  {** Constante FPMIN de la funcion gcf. (Ver Numerical Recipes in C. 2nd
      Edition.) }
  gcf_FPMIN: TUaReal = 1.0E-30;

{** Basado en codigo de Numerical Recipes in C - 2nd Edition.
    Returns the value ln[Gamma(xx)] for xx > 0. }
function gammln(const xx: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition.
    Returns the incomplete beta function I_x(a,b). }
function betai(const a, b, x: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition.
    Used by betai: Evaluates continued fraction for incomplete beta function by
    modified Lentz's method. }
function betacf(const a, b, x: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition. Returns the
    incomplete gamma function P(a,x) evaluated by its serires representation as
    gamser. Also returns Ln (Gamma(a)) as gln. }
procedure gser(var gamser: TUaReal; const a: TUaReal; const x: TUaReal; var gln: TUaReal);
{** Basado en codigo de Numerical Recipes in C - 2nd Edition. Returns the
    incomplete gamma function Q(a,x) evaluated by its continued fraction
    representation as gammcf. Also returns Ln(Gamma(a)) as gln. }
procedure gcf(var gammcf: TUaReal; const a: TUaReal; const x: TUaReal; var gln: TUaReal);
{** Basado en codigo de Numerical Recipes in C - 2nd Edition. Returns the
    incomplete gamma function P(a,x). }
function gammp(const a, x: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition. Returns the
    incomplete gamma function Q(a,x) = 1 P(a,x). }
function gammq(const a, x: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition (Ver ecuacion
    6.4.11). Calcula la funcion de probabilidad de la distribucion F.
    @param(F Un estadistico observado).
    @param(nu1 Grados de libertad de la primera muestra.)
    @param(nu2 Grados de libertad de la segunda muestra.)
    @returns(Q(F|nu1, nu2).) }
function FuncionDistribucionProbabilidadF(F, nu1, nu2: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition (Ver ecuacion
    6.4.9). Calcula la funcion de probabilidad de la distribucion de Student.
    @param(t Un estadistico observado).
    @param(nu Grados de libertad.)
    @returns(A(t|nu).) }
function FuncionDistribucionProbabilidadStudent(t, nu: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition. Calcula la
    probabilidad que el valor de chi-cuadrado observado sea menor que el valor
    de Chi^2 (dado un valor de grados de libertad).
    @param(x2 Valor de chi-cuadrado observado).
    @param(nu1 Grados de libertad.)
    @returns(P(x2|nu).) }
function FuncionDistribucionProbabilidadChiCuadradoP(x2, nu: TUaReal): TUaReal;
{** Basado en codigo de Numerical Recipes in C - 2nd Edition. Complemento de
    @link(FuncionDistribucionProbabilidadChiCuadradoP). Calcula la probabilidad
    que el valor de chi-cuadrado observado sea mayor que el valor de Chi^2
    (dado un valor de grados de libertad).
    @param(x2 Valor de chi-cuadrado observado).
    @param(nu1 Grados de libertad.)
    @returns(Q(x2|nu).) }
function FuncionDistribucionProbabilidadChiCuadradoQ(x2, nu: TUaReal): TUaReal;

implementation

function gammln(const xx: TUaReal): TUaReal;
var
  x, y, tmp, ser: TUaReal;
  j: Integer;
begin { gammln }
  Assert(xx > 0, 'gammln: xx > 0');

  y := xx;
  x := xx;
  tmp := x + 5.5;
  tmp := tmp - (x + 0.5) * ln(tmp);
  for j := 0 to 5 do
  begin
    y := y + 1.0;
    ser := ser + gammln_cof [j] / y;
  end;
  Result := -tmp + ln(2.506628274631005 * ser / x);
end { gammln };

function betai(const a, b, x: TUaReal): TUaReal;
var
  bt: TUaReal;
begin { betai }
  if (x < 0.0) or (x > 1.0) then
    raise EUaError.Create('betai: Valor de x invalido.');

  if (x = 0.0) or (x = 1.0) then
    bt := 0.0
  else
    bt := exp(gammln(a + b) - gammln(a) - gammln(b) + a * ln(x)+ b * ln(1.0 - x));

  if (x < (a + 1.0) / (a + b + 2.0)) then
    Result := bt * betacf(a,b,x) / a
  else
    Result := 1.0 - bt * betacf(b,a,1.0 - x) / b;
end { betai };

function betacf(const a, b, x: TUaReal): TUaReal;
var
  m, m2: Integer;
  aa, c, d, del, h, qab, qam, qap: TUaReal;
begin { betacf }
  qab := a + b;
  qap := a + 1.0;
  qam := a - 1.0;
  c := 1.0;
  d := 1.0 - qab * x / qap;
  if (Abs(d) < betacf_FPMIN) then
    d := betacf_FPMIN;
  for m := 1 to betacf_MAXIT do
  begin
    m2 := 2 * m;
    aa := m * (b - m) * x / ((qam + m2) * (a + m2));
    d := 1.0 + aa * d;
    if (Abs(d) < betacf_FPMIN) then
      d := betacf_FPMIN;
    c := 1.0 + aa / d;
    if (Abs(c) < betacf_FPMIN) then
      c := betacf_FPMIN;
    d := 1.0 / d;
    del := d * c;
    h := h * del;
    if (Abs(del - 1.0) < betacf_EPS) then
      Break;
  end;

  if (m > betacf_MAXIT) then
    raise EUaError.Create('betcf: a o b muy grandes, o MAXIT muy pequeno.');
end { betacf };

procedure gser(var gamser: TUaReal; const a: TUaReal; const x: TUaReal; var gln: TUaReal);
var
  n: Integer;
  sum, del, ap: TUaReal;
begin { gser }
  gln := gammln(a);
  if (x <= 0.0) then
  begin
    gamser := 0.0;
    raise EUaError.Create('gser: x menor o igual que cero en la rutina gser.');
  end
  else
  begin
    ap := a;
    sum := 1.0 / a;
    del := sum;
    for n := 1 to gser_ITMAX do
    begin
      ap := ap + 1.0;
      del := del * (x / ap);
      sum := sum + del;
      if (Abs(del) < Abs(sum) * gser_EPS) then
      begin
        gamser := sum * Exp(-x + a * Ln(X) - gln);
        Exit;
      end;
    end;
    raise EUaError.Create('gser: a muy grande, o ITMAX muy chico en la rutina gser.');
  end;
end { gser };

procedure gcf(var gammcf: TUaReal; const a: TUaReal; const x: TUaReal; var gln: TUaReal);
var
  i: Integer;
  an, b, c, d, del, h: TUaReal;
begin { gcf }
  gln := gammln(a);
  b := x + 1.0 - a;
  c := 1.0 / gcf_FPMIN;
  d := 1.0 / b;
  h := d;
  for i := 1 to gcf_ITMAX do
  begin
    an := -i * (i - a);
    b := b + 2.0;
    d := an * d + b;
    if (Abs(d) < gcf_FPMIN) then
      d := gcf_FPMIN;
    c := b + an / c;
    if (Abs(c) < gcf_FPMIN) then
      d := 1.0 / d;
    del := d * c;
    h := h * del;
    if (Abs(del - 1.0) < gcf_EPS) then
      Break;
  end;
  // Se invirtio el orden de estas ultimas lineas de codigo dado que nerror no
  // detiene el algoritmo y raise si y es necesario darle un valor a gammcf.
  gammcf := Exp(-x + a * Ln(x) - gln) * h;
  if (i > gcf_ITMAX) then
    raise EUaError.Create('gcf: a muy grande, o ITMAX muy chico en la rutina gcf.');
end { gcf };

function gammp(const a, x: TUaReal): TUaReal;
var
  gamser, gammcf, gln: TUaReal;
begin { gammp }
  // Notar que la logica de las siguientes instrucciones difieren del codigo
  // C de Numerical Recipes, pero el resultado final es el mismo.

  if (x < 0.0) or (a <= 0.0) then
    raise EUaError.Create('gammp: argumentos invalidos en la rutina gammp.');

  if (x < (a + 1.0)) then
  begin
    gser(gamser, a, x, gln);
    Result := gamser;
  end
  else
  begin
    gcf(gammcf, a, x, gln);
    Result := 1.0 - gammcf;
  end;
end { gammp };

function gammq(const a, x: TUaReal): TUaReal;
var
  gamser, gammcf, gln: TUaReal;
begin { gammq }
  // Notar que la logica de las siguientes instrucciones difieren del codigo
  // C de Numerical Recipes, pero el resultado final es el mismo.

  if (x < 0.0) or (a <= 0.0) then
    raise EUaError.Create('gammq: argumentos invalidos en la rutina gammq.');

  if (x < (a + 1.0)) then
  begin
    gser(gamser, a, x, gln);
    Result := 1.0 - gamser;
  end
  else
  begin
    gcf(gammcf, a, x, gln);
    Result := gammcf;
  end;
end { gammq };

function FuncionDistribucionProbabilidadStudent(t, nu: TUaReal): TUaReal;
var
  x: TUaReal;
begin { FuncionDistribucionProbabilidadStudent }
  x := nu / (nu + t * t);

  Result := 1.0 - betai(nu / 2.0, 0.5, x);
end { FuncionDistribucionProbabilidadStudent };

function FuncionDistribucionProbabilidadF(F, nu1, nu2: TUaReal): TUaReal;
var
  x: TUaReal;
begin { FuncionDistribucionProbabilidadF }
  x := nu2 / (nu2 + Power(nu1, F));

  Result := betai(nu2 / 2.0, nu1 / 2.0, x);
end { FuncionDistribucionProbabilidadF };

function FuncionDistribucionProbabilidadChiCuadradoP(x2, nu: TUaReal): TUaReal;
begin { FuncionDistribucionProbabilidadChiCuadradoP }
  Result := gammp(nu / 2.0, x2 / 2.0);
end { FuncionDistribucionProbabilidadChiCuadradoP };

function FuncionDistribucionProbabilidadChiCuadradoQ(x2, nu: TUaReal): TUaReal;
begin { FuncionDistribucionProbabilidadChiCuadradoQ }
  Result := gammq(nu / 2.0, x2 / 2.0);
end { FuncionDistribucionProbabilidadChiCuadradoQ };

end { UnitSubrutinasEstadisticas }.
