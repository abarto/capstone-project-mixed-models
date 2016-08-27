{**
@abstract(Constantes y tipos basicos del procedimiento.)
@author(Agustin Barto <abarto@gmail.com>)
@created(February 08, 2003)
@lastmod(March 13, 2005)
Este modulo contiene la definicion de constantes utilizadas durante el
procedimiento.
}
unit UnitConstantesProcedimiento;

interface

uses
  UnitParametros;

const
  {** Valor inicial para los parametros. }
  CValorInicialParametros: TValorParametro = 1.0;
  {** Cantidad maxima de observaciones generadas. }
  CCantidadObservaciones: Integer = 100;

implementation

end.
