{**
@abstract(Unit de generadores de numeros pseudo-aleatorios y variables
aleatorias.)
@author(Agustin Barto <abarto@gmail.com>)
@created(March 2, 2005)
@lastmod(March 2, 2005)
Este modulo contiene definiciones de clase, metodos y constantes utilizados
para evaluar expresiones simbolicas reales y booleanas.
}
unit UaGeneradorNumerosAleatorios;

{$Q-}
{$R+}
{$HINTS OFF}
{$WARNINGS OFF}

interface

uses
  UaComun;

type
  {** Generador de numeros aleatorios basicos. Indica la interfaz a cumplir por
      las distintas implementaciones de generadores de numeros aleatorios. Todos
      las implementaciones generan una observaciones de una variable
      uniformemente distribuida en [0, 1]. 
      @abstract(Generador de numeros aleatorios basico.) }
  TUaGeneradorNumerosAleatorios = class
  protected
    {** Metodo de lectura de la propiedad @link(Siguiente). Genera el siguiente
        valor aleatorio real entre 0 y 1.
        @returns(Numero real pseudo-aleatorio entre 0 y 1.) }
    function GetSiguiente: TUaReal; virtual; abstract;
  public
    {** Siguiente valor aleatorio real entre 0 y 1. }
    property Siguiente: TUaReal read GetSiguiente;
  end { TUaGeneradorNumerosAleatorios };

  {** Implementacion de generador de numeros aleatorios utilizando el metodo de
      Mersenne Twiser. Ver
      http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
      @abstract(Generador de numeros aleatorios basados en el metodo de
      Mersenne Twister.) }
  TUaMersenneTwister = class(TUaGeneradorNumerosAleatorios)
  private
    // Variables y metodos internos al metodo de mersenne twiser. Basado en
    // mt19937ar.c.

    MT19937AR_mti: Smallint;
    MT19937AR_mt: array of Cardinal;

    procedure MT19937AR_init_genrand(const s: Cardinal);
    procedure MT19937AR_init_by_array(const init_key: array of Cardinal; const key_length: Integer);
    function MT19937AR_genrand_int32: Cardinal;
    function MT19937AR_genrand_int31: Longint;
    function MT19937AR_genrand_real1: Double;
    function MT19937AR_genrand_real2: Double;
    function MT19937AR_genrand_real3: Double;
    function MT19937AR_genrand_res53: Double;
  protected
    {** Ver @link(TUaGeneradorNumerosAleatorios.GetSiguiente). }
    function GetSiguiente: TUaReal; override;
  public
    {** Constructor. Crea una instancia del generador con una semilla por
        defecto (constante). }
    constructor Create; overload;
    {** Constructor. Crea una instancia del generador utilizando una semilla
        especificada.
        @param(Semilla Semilla del algoritmo.) }
    constructor Create(const Semilla: Cardinal); overload;
  end { TUaMersenneTwister };

  {** Generador basico de observaciones de una variable aleatoria. Define las
      propiedades basicas de los generadores de observaciones de variables de
      diferentes distribuciones.}
  TUaVariableAleatoria = class
  protected
    {** Generador de numeros aleatorios utilizado para generar las
        observaciones de la variable aleatoria. }
    FGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios;
    {** Metodo de lectura de la propiedad @link(Siguiente). Genera la siguiente
        observacion de la variable con la distrubucion y propiedades (media,
        varianza, etc.) adecuadas.
        @returns(Observacion de la variable aleatoria.) }
    function GetSiguiente: TUaReal; virtual; abstract;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del generador de observaciones.
        @returns(Representacion textual del generado.) }
    function GetTexto: String; virtual; abstract;
  public
    {** Constructor. Inicializa el generador con el metodo de Mersenne
        Twister. }
    constructor Create; overload;
    {** Constructor.
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilizar para generar las observaciones.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios); overload;
    {** Siguiente observacion de la variable. }
    property Siguiente: TUaReal read GetSiguiente;
    {** Generador de numeros aleatorios utilizado para generar las
        observaciones de la variable aleatoria. }
    property GeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios read FGeneradorNumerosAleatorios write FGeneradorNumerosAleatorios;
    {** Representacion de cadena de caracteres del generador de
    obseravaciones. }
    property Texto: String read GetTexto;
  end { TUaVariableAleatoria };

  {** Generador de observaciones de una variable aleatoria con distrubucion
      uniforme. Es posible indicar cota inferior y superior del intervalo de
      generacion.
      @abstract(Generador de observaciones de una variable aleatoria con
      distribucion uniforme.) }
  TUaVariableDistribucionUniforme = class(TUaVariableAleatoria)
  private
    {** Cota inferior del intervalo de posibles valores generados. }
    FCotaInferior: TUaReal;
    {** Cota superior del intervalo de posibles valores generados. }
    FCotaSuperior: TUaReal;
  protected
    {** Ver @link(TUaVariableAleatoria.GetSiguiente). }
    function GetSiguiente: TUaReal; override;
    {** Ver @link(TUaVariableAleatoria.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor. Inicializa el generador de numeros aleatorios con el metodo
        de Mersenne Twister e inicializa el intervalo de valores generados a
        [0, 1]. }
    constructor Create; overload;
    {** Constructor. Inicializa el generador de numeros aleatorios de acuerdo al
        parametro e inicializa el intervalo de valores generados a [0, 1].
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilzar.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios); overload;
    {** Constructor. Inicializa el generador de numeros aleatorios con el metodo
        de Mersenne Twister e inicializa el intervalo de valores generados a
        [ACotaInferior, ACotaSuperior].
        @param(ACotaInferior Cota inferior del intervalo de valores generados.)
        @param(ACotaSuperior Cota superior del intervalo de valores generados.) }
    constructor Create(const ACotaInferior, ACotaSuperior: TUaReal); overload;
    {** Constructor. Inicializa el generador de numeros aleatorios de acuerdo al
        parametro e inicializa el intervalo de valores generados a
        [ACotaInferior, ACotaSuperior].
        @param(ACotaInferior Cota inferior del intervalo de valores generados.)
        @param(ACotaSuperior Cota superior del intervalo de valores generados.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios; const ACotaInferior, ACotaSuperior: TUaReal); overload;
    {** Cota inferior del intervalo de posibles valores generados. }
    property CotaInferior: TUaReal read FCotaInferior;
    {** Cota superior del intervalo de posibles valores generados. }
    property CotaSuperior: TUaReal read FCotaSuperior;
  end { TUaVariableDistribucionUniforme };

  {** Generador de observaciones de una variable aleatoria con distrubucion
      normal. Es posible indicar la media y la varianza de los valores
      generados.
      @abstract(Generador de observaciones de una variable aleatoria con
      distribucion normal (Gaussiana).) }
  TUaVariableDistribucionNormal = class(TUaVariableAleatoria)
  private
    {** Media. }
    FMedia: TUaReal;
    {** Varianza. }
    FVarianza: TUaReal;
  protected
    {** Ver @link(TUaVariableAleatoria.GetSiguiente). }
    function GetSiguiente: TUaReal; override;
    {** Ver @link(TUaVariableAleatoria.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor. Inicializa el generador de numeros aleatorios con el metodo
        de Mersenne Twister e inicializa la media y la varianza a 0 y 1
        respectivamente. }
    constructor Create; overload;
    {** Constructor. Inicializa el generador de numeros aleatorios de acuerdo al
        parametro e inicializa la media y la varianza a 0 y 1 respectivamente.
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilzar.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios); overload;
    {** Constructor. Inicializa el generador de numeros aleatorios con el metodo
        de Mersenne Twister e inicializa la media y la varianza de acuerdo a los
        parametros
        @param(AMedia Media de los valores generados.)
        @param(AVarianza Varianza de los valores generados.) }
    constructor Create(const AMedia, AVarianza: TUaReal); overload;
    {** Constructor. Inicializa el generador de numeros aleatorios de acuerdo al
        parametro e inicializa la media y la varianza de acuerdo a los
        parametros
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilzar.)
        @param(AMedia Media de los valores generados.)
        @param(AVarianza Varianza de los valores generados.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios; const AMedia, AVarianza: TUaReal); overload;
    {** Generador de la siguiente observacion mediante el metodo de limite
        central.
        @returns(Observacion de la variable normal con la media y la varianza
        estipuladas.) }
    function SiguienteMetodoLimiteCentral(const N: Smallint): TUaReal;
    {** Generador de la siguiente observacion mediante el metodo de Box-Muller.
        @returns(Observacion de la variable normal con la media y la varianza
        estipuladas.) }
    function SiguienteMetodoBoxMuller: TUaReal;
    {** Media. }
    property Media: TUaReal read FMedia;
    {** Varianza. }
    property Varianza: TUaReal read FVarianza;
  end { TUaVariableDistribucionNormal };

  {** Generador de observaciones de una variable aleatoria con distrubucion
      exponencial.
      @abstract(Generador de observaciones de una variable aleatoria con
      distribucion exponencial.) }
  TUaVariableDistribucionExponencial = class(TUaVariableAleatoria)
  private
    {** Media. }
    FMedia: TUaReal;
  protected
    {** Ver @link(TUaVariableAleatoria.GetSiguiente). }
    function GetSiguiente: TUaReal; override;
    {** Ver @link(TUaVariableAleatoria.GetTexto). }
    function GetTexto: String; override;
  public
    {** Constructor. Inicializa el generador de numeros aleatorios con el metodo
        de Mersenne Twister e inicializa la media en 1. }
    constructor Create; overload;
    {** Constructor. Inicializa el generador de numeros aleatorios de acuerdo al
        parametro e inicializa la media en 1.
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilzar.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios); overload;
    {** Constructor. Inicializa el generador de numeros aleatorios con el metodo
        de Mersenne Twister y la media de acuerdo al parametro.
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilzar.)
        @param(AMedia Media de los valores generados.) }
    constructor Create(const AMedia: TUaReal); overload;
    {** Constructor. Inicializa el generador de numeros aleatorios y la media
        de acuerdo a los parametros.
        @param(AGeneradorNumerosAleatorios Generador de numeros
        aleatorios a utilzar.)
        @param(AMedia Media de los valores generados.) }
    constructor Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios; const AMedia: TUaReal); overload;
    {** Media. }
    property Media: TUaReal read FMedia;
  end { TUaVariableDistribucionExponencial };

implementation

uses
  Math, SysUtils;

// -----------------------------------------------------------------------------
// TUaMersenneTwister
// -----------------------------------------------------------------------------

const
  MT19937AR_N = 624;
  MT19937AR_M = 397;
  MT19937AR_MATRIX_A = $9908B0DF;
  MT19937AR_UPPER_MASK = $80000000;
  MT19937AR_LOWER_MASK = $7FFFFFFF;
  MT19937AR_mag01 : array [0..1] of Cardinal = (0, MT19937AR_MATRIX_A);

procedure TUaMersenneTwister.MT19937AR_init_genrand(const s: Cardinal);
begin { UaMersenneTwister.MT19937AR_init_genrand }
  MT19937AR_mt [0] := s and $FFFFFFFF;

  MT19937AR_mti := 1;
  while (MT19937AR_mti < MT19937AR_N) do
  begin
    MT19937AR_mt [MT19937AR_mti] := (1812433253 * (MT19937AR_mt [MT19937AR_mti - 1] xor (MT19937AR_mt [MT19937AR_mti-1] shr 30)) + MT19937AR_mti);
    MT19937AR_mt [MT19937AR_mti] := MT19937AR_mt [MT19937AR_mti] and $FFFFFFFF;
    Inc(MT19937AR_mti);
  end;
end { UaMersenneTwister.MT19937AR_init_genrand };

procedure TUaMersenneTwister.MT19937AR_init_by_array(const init_key: array of Cardinal; const key_length: Integer);
var
  i, j, k: Smallint;
begin { TUaMersenneTwister.MT19937AR_init_by_array }
  MT19937AR_init_genrand(19650218);

  i := 1;
  j := 0;
  k := key_length;
  if (MT19937AR_N > k) then
    k := MT19937AR_N;

  while (k > 0) do
  begin
    MT19937AR_mt [i] := (MT19937AR_mt [i] xor ((MT19937AR_mt [i-1] xor (MT19937AR_mt [i-1] shr 30)) * 1664525)) + init_key [j] + j;
    MT19937AR_mt [i] := MT19937AR_mt[i] and $FFFFFFFF;
    Inc(i);
    Inc(j);
    if (i >= MT19937AR_N) then
    begin
      MT19937AR_mt [0] := MT19937AR_mt [MT19937AR_N-1];
      i := 1;
    end;

    if (j >= key_length) then
      j := 0;

    Dec(k);
  end;

  k := MT19937AR_N - 1;
  while (k > 0) do
  begin
    MT19937AR_mt [i] :=(MT19937AR_mt [i] xor ((MT19937AR_mt [i-1] xor (MT19937AR_mt [i-1] shr 30)) * 1566083941)) - i;
    MT19937AR_mt [i] := MT19937AR_mt [i] and $FFFFFFFF;
    Inc(I);

    if (i >= MT19937AR_N) then
    begin
      MT19937AR_mt [0] := MT19937AR_mt [MT19937AR_N-1];
      i := 1;
    end;

    Dec(k);
  end;

  MT19937AR_mt [0] := $80000000;
end { TUaMersenneTwister.MT19937AR_init_by_array };

function TUaMersenneTwister.MT19937AR_genrand_int32: Cardinal;
var
  y: Cardinal;
  kk: Integer;
begin { TUaMersenneTwister.MT19937AR_genrand_int32 }
  if (MT19937AR_mti >= MT19937AR_N)  then
  begin
    if (MT19937AR_mti = (MT19937AR_N + 1)) then
      MT19937AR_init_genrand(5489);

    for kk := 0 to MT19937AR_N - MT19937AR_M - 1 do
    begin
      y := (MT19937AR_mt [kk] and MT19937AR_UPPER_MASK) or (MT19937AR_mt [kk+1] and MT19937AR_LOWER_MASK);
      MT19937AR_mt [kk] := MT19937AR_mt [kk+MT19937AR_M] xor (y shr 1) xor MT19937AR_mag01 [y and 1];
    end;

    while (kk < MT19937AR_N - 1) do
    begin
      y := (MT19937AR_mt [kk] and MT19937AR_UPPER_MASK) or (MT19937AR_mt [kk+1] and MT19937AR_LOWER_MASK);
      MT19937AR_mt[kk] := MT19937AR_mt [kk+(MT19937AR_M-MT19937AR_N)] xor (y shr 1) xor MT19937AR_mag01 [y and 1];
      Inc(kk);
    end;

    y := (MT19937AR_mt [MT19937AR_N-1] and MT19937AR_UPPER_MASK) or (MT19937AR_mt [0] and MT19937AR_LOWER_MASK);
    MT19937AR_mt[MT19937AR_N-1] := MT19937AR_mt [MT19937AR_M-1] xor (y shr 1) xor MT19937AR_mag01[y and 1];
    MT19937AR_mti := 0;
  end;

  y := MT19937AR_mt[MT19937AR_mti];
  Inc(MT19937AR_mti);
  y := y xor (y shr 11);
  y := y xor ((y shl 7) and $9d2c5680);
  y := y xor ((y shl 15) and $efc60000);
  y := y xor (y shr 18);

  Result := y;
end { TUaMersenneTwister.MT19937AR_genrand_int32 };

function TUaMersenneTwister.MT19937AR_genrand_int31: Longint;
begin { TUaMersenneTwister.MT19937AR_genrand_int31 }
  Result := MT19937AR_genrand_int32 shr 1;
end { TUaMersenneTwister.MT19937AR_genrand_int31 };

function TUaMersenneTwister.MT19937AR_genrand_real1: Double;
begin { TUaMersenneTwister.MT19937AR_genrand_real1 }
  Result := MT19937AR_genrand_int32 * (1.0 / 4294967295.0);
end { TUaMersenneTwister.MT19937AR_genrand_real1 };

function TUaMersenneTwister.MT19937AR_genrand_real2: Double;
begin { TUaMersenneTwister.MT19937AR_genrand_real2 }
  Result := MT19937AR_genrand_int32 * (1.0 / 4294967296.0);
end { TUaMersenneTwister.MT19937AR_genrand_real2 };

function TUaMersenneTwister.MT19937AR_genrand_real3: Double;
begin { TUaMersenneTwister.MT19937AR_genrand_real3 }
  Result := (MT19937AR_genrand_int32 + 0.5) * (1.0 / 4294967296.0);
end { TUaMersenneTwister.MT19937AR_genrand_real3 };

function TUaMersenneTwister.MT19937AR_genrand_res53: Double;
var
  a,b: Cardinal;
begin { TUaMersenneTwister.MT19937AR_genrand_res53 }
  a := MT19937AR_genrand_int32 shr 5;
  b := MT19937AR_genrand_int32 shr 6;
  Result := (a * 67108864.0 + b) * (1.0 / 9007199254740992.0);
end { TUaMersenneTwister.MT19937AR_genrand_res53 };

constructor TUaMersenneTwister.Create;
begin { TUaMersenneTwister.Create }
  SetLength(MT19937AR_mt, MT19937AR_N);
  MT19937AR_mti := MT19937AR_N + 1;
  MT19937AR_init_genrand(19650218);
end { TUaMersenneTwister.Create };

constructor TUaMersenneTwister.Create(const Semilla: Cardinal);
begin { TUaMersenneTwister.Create }
  SetLength(MT19937AR_mt, MT19937AR_N);
  MT19937AR_mti := MT19937AR_N + 1;
  MT19937AR_init_genrand(Semilla);
end { TUaMersenneTwister.Create };

function TUaMersenneTwister.GetSiguiente: TUaReal;
begin { TUaMersenneTwister.GetSiguiente }
  Result := MT19937AR_genrand_real1;
end { TUaMersenneTwister.GetSiguiente };

// -----------------------------------------------------------------------------
// TUaVariableAleatoria
// -----------------------------------------------------------------------------

constructor TUaVariableAleatoria.Create;
begin { TUaVariableAleatoria.Create }
  FGeneradorNumerosAleatorios := TUaMersenneTwister.Create(Floor(Now));
end { TUaVariableAleatoria.Create };

constructor TUaVariableAleatoria.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios);
begin { TUaVariableAleatoria.Create }
  Assert(Assigned(AGeneradorNumerosAleatorios), 'TUaVariableAleatoria.Create: Assigned(AGeneradorNumerosAleatorios)');

  FGeneradorNumerosAleatorios := AGeneradorNumerosAleatorios;
end { TUaVariableAleatoria.Create };

// -----------------------------------------------------------------------------
// TUaVariableDistribucionUniforme
// -----------------------------------------------------------------------------

function TUaVariableDistribucionUniforme.GetSiguiente: TUaReal;
begin { TUaVariableDistribucionUniforme.GetSiguiente }
  Result := (FCotaSuperior - FCotaInferior) * FGeneradorNumerosAleatorios.Siguiente + FCotaInferior;
end { TUaVariableDistribucionUniforme.GetSiguiente };

function TUaVariableDistribucionUniforme.GetTexto: String;
begin { TUaVariableDistribucionUniforme.GetTexto }
  Result := 'U(' + FloatToStr(FCotaInferior) + ', ' + FloatToStr(FCotaSuperior) + ')';
end { TUaVariableDistribucionUniforme.GetTexto };


constructor TUaVariableDistribucionUniforme.Create;
begin { TUaVariableDistribucionUniforme.Create }
  inherited Create;

  FCotaInferior := 0;
  FCotaSuperior := 1;
end { TUaVariableDistribucionUniforme.Create };

constructor TUaVariableDistribucionUniforme.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios);
begin { TUaVariableDistribucionUniforme.Create }
  inherited Create(AGeneradorNumerosAleatorios);

  FCotaInferior := 0;
  FCotaSuperior := 1;
end { TUaVariableDistribucionUniforme.Create };

constructor TUaVariableDistribucionUniforme.Create(const ACotaInferior, ACotaSuperior: TUaReal);
begin { TUaVariableDistribucionUniforme.Create }
  inherited Create;

  Assert(ACotaInferior < ACotaSuperior, 'TUaVariableDistribucionUniforme.Create: ACotaInferior < ACotaSuperior');

  FCotaInferior := ACotaInferior;
  FCotaSuperior := ACotaSuperior;
end { TUaVariableDistribucionUniforme.Create };

constructor TUaVariableDistribucionUniforme.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios; const ACotaInferior, ACotaSuperior: TUaReal);
begin { TUaVariableDistribucionUniforme.Create }
  inherited Create(AGeneradorNumerosAleatorios);

  Assert(ACotaInferior < ACotaSuperior, 'TUaVariableDistribucionUniforme.Create: ACotaInferior < ACotaSuperior');

  FCotaInferior := ACotaInferior;
  FCotaSuperior := ACotaSuperior;
end { TUaVariableDistribucionUniforme.Create };

// -----------------------------------------------------------------------------
// TUaVariableDistribucionNormal
// -----------------------------------------------------------------------------

function TUaVariableDistribucionNormal.GetSiguiente: TUaReal;
begin { TUaVariableDistribucionNormal.GetSiguiente }
  Result := SiguienteMetodoLimiteCentral(12);
end { TUaVariableDistribucionNormal.GetSiguiente };

function TUaVariableDistribucionNormal.GetTexto: String;
begin { TUaVariableDistribucionNormal.GetTexto }
  Result := 'N(' + FloatToStr(FMedia) + ', ' + FloatToStr(FVarianza) + ')';
end { TUaVariableDistribucionNormal.GetTexto };

function TUaVariableDistribucionNormal.SiguienteMetodoLimiteCentral(const N: Smallint): TUaReal;
var
  I: Integer;
  X: TUaReal;
begin { TUaVariableDistribucionNormal.SiguienteMetodoLimiteCentral }
  Assert(N > 0,'TUaVariableDistribucionNormal.SiguienteMetodoLimiteCentral: N > 0');

  X := 0.0;
  for I := 1 to N do
    X := X + FGeneradorNumerosAleatorios.Siguiente;
  X := X - N / 2.0;
  if N <> 12 then
    X := X * Sqrt(12 / N);

  Result := FMedia + Sqrt(FVarianza) * X;
end { TUaVariableDistribucionNormal.SiguienteMetodoLimiteCentral };

function TUaVariableDistribucionNormal.SiguienteMetodoBoxMuller: TUaReal;
var
  V1, V2, S: TUaReal;
begin { TUaVariableDistribucionNormal.SiguienteMetodoBoxMuller }
  repeat
    V1 := 2.0 * FGeneradorNumerosAleatorios.Siguiente - 1.0;
    V2 := 2.0 * FGeneradorNumerosAleatorios.Siguiente - 1.0;
    S := V1 * V1 + V2 * V2;
  until S < 1;

  Result := FMedia + Sqrt(FVarianza) * (Sqrt(-2.0 * Ln(S) / S) * V1);
end { TUaVariableDistribucionNormal.SiguienteMetodoBoxMuller };

constructor TUaVariableDistribucionNormal.Create;
begin { TUaVariableDistribucionNorma }
  inherited Create;

  FMedia := 0;
  FVarianza := 1;
end { TUaVariableDistribucionNorma };

constructor TUaVariableDistribucionNormal.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios);
begin { TUaVariableDistribucionNormal.Create }
  inherited Create(AGeneradorNumerosAleatorios);

  FMedia := 0;
  FVarianza := 1;
end { TUaVariableDistribucionNormal.Create };

constructor TUaVariableDistribucionNormal.Create(const AMedia, AVarianza: TUaReal);
begin { TUaVariableDistribucionNormal.Create }
  inherited Create;

  Assert(AVarianza > 0, 'TUaVariableDistribucionNormal.Create: AVarianza > 0');

  FMedia := AMedia;
  FVarianza := AVarianza;
end { TUaVariableDistribucionNormal.Create };

constructor TUaVariableDistribucionNormal.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios; const AMedia, AVarianza: TUaReal);
begin { TUaVariableDistribucionNormal.Create }
  inherited Create(AGeneradorNumerosAleatorios);

  Assert(AVarianza > 0, 'TUaVariableDistribucionNormal.Create: AVarianza > 0');

  FMedia := AMedia;
  FVarianza := AVarianza;
end { TUaVariableDistribucionNormal.Create };

// -----------------------------------------------------------------------------
// TUaVariableDistribucionExponencial
// -----------------------------------------------------------------------------

function TUaVariableDistribucionExponencial.GetSiguiente: TUaReal;
begin { TUaVariableDistribucionExponencial.GetSiguiente }
  Result := -FMedia * Ln(FGeneradorNumerosAleatorios.Siguiente);
end { TUaVariableDistribucionExponencial.GetSiguiente };

function TUaVariableDistribucionExponencial.GetTexto: String;
begin { TUaVariableDistribucionExponencial.GetTexto }
  Result := 'E(' + FloatToStr(FMedia) + ')';
end { TUaVariableDistribucionExponencial.GetTexto };

constructor TUaVariableDistribucionExponencial.Create;
begin { TUaVariableDistribucionExponencial.Create }
  inherited Create;

  FMedia := 1.0;
end { TUaVariableDistribucionExponencial.Create };

constructor TUaVariableDistribucionExponencial.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios);
begin { TUaVariableDistribucionExponencial.Create }
  inherited Create(AGeneradorNumerosAleatorios);

  FMedia := 1.0;
end { TUaVariableDistribucionExponencial.Create };

constructor TUaVariableDistribucionExponencial.Create(const AMedia: TUaReal);
begin { TUaVariableDistribucionExponencial.Create }
  inherited Create;

  FMedia := AMedia;
end { TUaVariableDistribucionExponencial.Create };

constructor TUaVariableDistribucionExponencial.Create(const AGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios; const AMedia: TUaReal);
begin { TUaVariableDistribucionExponencial.Create }
  inherited Create(AGeneradorNumerosAleatorios);

  FMedia := AMedia;
end { TUaVariableDistribucionExponencial.Create };

end { UaGeneradorNumerosAleatorios }.
