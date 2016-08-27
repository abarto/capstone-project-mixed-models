{**
@abstract(Configuracion de procedimiento de simulacion.)
@author(Agustin Barto <abarto@gmail.com>)
@created(July 1, 2005)
@lastmod(July 1, 2005)
Este modulo contiene la clase que almacena la configuracion del procedimiento
del procedimiento de simulacion.
}

unit UnitConfiguracionProcedimiento;

interface

uses
  Classes, Contnrs, SysUtils,
  UnitOpcion;

type
  {** Configuracion del procedimiento de simulacion. Esta clase contiene las
      opciones que controlan el procedimiento de simulacion y la salida
      generada por el mismo.
      @abstract(Configuracion del procedimiento de simulacion.) }
  TConfiguracionProcedimiento = class
  private
    {** Opciones del procedimiento. }
    FOpciones: TListaOpciones;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AOpciones Lista de opciones del procedimiento.) }
    constructor Create(AOpciones: TListaOpciones);  overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Opciones del procedimiento. }
    property Opciones: TListaOpciones read FOpciones write FOpciones;
  end { TConfiguracionProcedimiento };

implementation

// -----------------------------------------------------------------------------
// TConfiguracionProcedimiento
// -----------------------------------------------------------------------------

constructor TConfiguracionProcedimiento.Create;
begin { TConfiguracionProcedimiento.Create }
  Create(TListaOpciones.Create);
end { TConfiguracionProcedimiento.Create };

constructor TConfiguracionProcedimiento.Create(AOpciones: TListaOpciones);
begin { TConfiguracionProcedimiento.Create }
  Assert(Assigned(AOpciones), 'TConfiguracionProcedimiento.Create: Assigned(AOpciones)');

  FOpciones := AOpciones;
end { TConfiguracionProcedimiento.Create };

destructor TConfiguracionProcedimiento.Destroy;
begin { TConfiguracionProcedimiento.Destroy }
  try
    FOpciones.Clear;
  finally
    FreeAndNil(FOpciones);

    inherited Destroy;
  end;
end { TConfiguracionProcedimiento.Destroy };

end { UnitConfiguracionProcedimiento }.
