{**
@abstract(Definiciones de constantes utilizadas en Utilidades Algebraicas.)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 29, 2004)
@lastmod(December 23, 2004)
Este modulo contiene definiciones de constantes utlizados por el resto
de las unidades de Utilidades Algebraicas.
}

unit UaConstantes;

interface

uses
  UaComun;

const
  {** Constante de tolerancia de la operacion de descomposicion LU. }
  CUaTinyDescomposicionLU: TUaReal = 10E-20;
  {** Cantidad de iteraciones para decomposicion en valores singulares. }
  CUaCantidadIteracionesSVD: Integer = 50;

  CUaRaizCub2: TUaReal    = 1.2599210498948731647672106072782;  // Raiz cubica de 2
  CUaRaizCub3: TUaReal    = 1.4422495703074083823216383107801;  // Raiz cubica de 2
  CUaRaizCub10: TUaReal   = 2.1544346900318837217592935665194;  // Raiz cubica de 10
  CUaRaizCub100: TUaReal  = 4.6415888336127788924100763509194;  // Raiz cubica de 100
  CUaRaizCubPi: TUaReal   = 1.4645918875615232630201425272638;  // Raiz cubica de Pi
  CUaPi: TUaReal          = 3.1415926535897932384626433832795;  // Pi
  CUaPiSobre2: TUaReal    = 1.5707963267948966192313216916398;  // Pi / 2
  CUaPiSobre3: TUaReal    = 1.0471975511965977461542144610932;  // Pi / 3
  CUaPiSobre4: TUaReal    = 0.78539816339744830961566084581988; // Pi / 4
  CUaRaizCua2: TUaReal    = 1.4142135623730950488016887242097;  // Raiz cuadrada de 2
  CUaRaizCua3: TUaReal    = 1.7320508075688772935274463415059;  // Raiz cuadrada de 3
  CUaRaizCua5: TUaReal    = 2.2360679774997896964091736687313;  // Raiz cuadrada de 5
  CUaRaizCua10: TUaReal   = 3.1622776601683793319988935444327;  // Raiz cuadrada de 10
  CUaRaizCuaPi: TUaReal   = 1.7724538509055160272981674833411;  // Raiz cuadrada de Pi
  CUaRaizCua2Pi: TUaReal  = 2.506628274631000502415765284811;   // Raiz cuadrada de 2 * Pi
  CUaDosPi: TUaReal       = 6.283185307179586476925286766559;   // 2 * Pi
  CUaTresPi: TUaReal      = 9.4247779607693797153879301498385;  // 3 * Pi
  CUaLn2: TUaReal         = 0.69314718055994530941723212145818; // Log(2)
  CUaLn10: TUaReal        = 2.3025850929940456840179914546844;  // Log(10)
  CUaLnPi: TUaReal        = 1.1447298858494001741434273513531;  // Log(Pi)
  CUaLog2: TUaReal        = 0.30102999566398119521373889472449; // Log10(2)
  CUaLog3: TUaReal        = 0.47712125471966243729502790325512; // Log10(3)
  CUaLogPi: TUaReal       = 0.4971498726941338543512682882909;  // Log10(Pi)
  CUaLogE: TUaReal        = 0.43429448190325182765112891891661; // Log10(E)
  CUaE: TUaReal           = 2.7182818284590452353602874713527;  // E
  CUaLn2PiSobre2: TUaReal = 0.91893853320467274178032973640562; // Log(2 * Pi) / 2
  CUaInv2Pi: TUaReal      = 0.159154943091895;                  // 0.5 / Pi
  CUaDosALa63: TUaReal    = 9223372036854775808.0;              // 2^63

var
  {** Tolerancia de la operacion de descomposicion LU. }
  UaTinyDescomposicionLU: TUaReal;

  {** Minimo numero real R tal que R + R = R }
  UaEpsilon: TUaReal;

implementation

procedure UaCalcularEpsilon;
var
  X, Y: TUaReal;
begin { UaCalcularEpsilon }
  X := 1.0;
  UaEpsilon := X;
  repeat
    UaEpsilon := 0.5 * UaEpsilon;
    Y := X + UaEpsilon;
  until X = Y;
  UaEpsilon := 2.0 * UaEpsilon;
end { UaCalcularEpsilon };

initialization
begin { initialization }
  UaCalcularEpsilon;
  UaTinyDescomposicionLU := CUaTinyDescomposicionLU;
end  { initialization };

end { UaConstantes }.
