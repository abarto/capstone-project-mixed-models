{**
@abstract(Constantes y tipos basicos del procedimiento.)
@author(Agustin Barto <abarto@gmail.com>)
@created(February 08, 2003)
@lastmod(February 08, 2005)
Este modulo contiene la definicion de constantes utilizadas durante el
procedimiento. Las mismas tienen un valor por defecto y variables asociadas que
permiten su modificacion durante el proceso de inicializacion. Ademas estan
definidos en el modulo tipos basicos utilizados por los procedimientos.
}
unit UnitConstantesProcedimiento;

interface

uses
  Classes,
  UaComun, UaConstantes, UaMatriz, UaVector, UnitParametros;

type
  {** Funcion de metodo de ajuste del modelo. }
  TMetodoAjusteModelo = function (var Parametros: TUaVector; var Bitacora: TStrings): Boolean of object;

  {** Criterio de convergencia. }
  TCriterioConvergencia = (
    {** Convergencia absoluta de parametros. }
    ConvergenciaParametrosAbsoluta,
    {** Convergencia relativa de parametros. }
    ConvergenciaParametrosRelativa,
    {** Convergencia absoluta del valor de la funcion objetivo. }
    ConvergenciaVerosimilitudAbsoluta,
    {** Convergencia relativa del valor de la funcion objetivo. }
    ConvergenciaVerosimilitudRelativa,
    {** Convergencia absoluta del gradiente de la funcion objetivo. }
    ConvergenciaGradienteAbsoluta,
    {** Convergencia relativa del gradiente de la funcion objetivo. }
    ConvergenciaGradienteRelativa,
    {** Convergencia absoluta del hessiano de la funcion objetivo. }
    ConvergenciaHessianoAbsoluta,
    {** Convergencia relativa del hessiano de la funcion objetivo. }
    ConvergenciaHessianoRelativa
  );

const
  {** Valor inicial para los parametros. }
  CValorInicialParametros: TValorParametro = 1.0;
  {** Cota inferior para los parametros. }
  CCotaInferiorParametros: TValorParametro = 1.0E-12;
  {** Indica si se debe verificar la cota inferior por defect. }
  CVerificarCotaInferiorParametros: Boolean = false;
  {** Cantidad maxima de evaluaciones de funcion de verosimilitud. }
  CCantidadMaximaEvaluacionesFuncionObjetivo: Integer = 150;
  {** Cantidad maxima de iteraciones del algoritmo de ajuste. }
  CCantidadMaximaIteraciones: Integer = 50;
  {** Cantidad de pasos iniciales de Scoring. }
  CCantidadPasosScoring: Integer = 1;
  {** Criterio de convergencia. }
  CCriterioConvergencia: TCriterioConvergencia = ConvergenciaHessianoRelativa;
  {** Tolerancia de convergencia. }
  CToleranciaConvergencia: TUaReal = 1E-8;
  {** Valor inicial factor de ridging. }
  CValorInicialFactorRidging: TUaReal = 0.1;
  {** Factor de incremento del factor de ridging. }
  CFactorIncrementoFactorRidging: TUaReal = 2.0;
  {** Factor de decremento del factor de ridging. }
  CFactorDecrementoFactorRidging: TUaReal = 5.0;
  {** Valor minimo factor ridging. }
  CValorMinimoFactorRidging: TUaReal = 1E-12;
  {** Valor maximo factor ridging. }
  CValorMaximoFactorRidging: TUaReal = 1E12;

implementation

end.
