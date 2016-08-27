{**
@abstract(Thread envoltura del objeto de procedimiento.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 19, 2005)
Este modulo contiene la definicion del thread que envuelve al objeto de
procedimiento. El thread es necesario para mantener una interfaz con buena
velocidad de respuesta y desacoplada del procedimiento, pero el objeto de
procedimiento podria utilizarse directamente.
}

{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}
{$WARN UNSAFE_CAST ON}

unit UnitThreadProcedimiento;

interface

uses
  Classes, SysUtils,
  UnitConfiguracionProcedimiento, UnitProcedimiento;

type
  {** Thread envoltura del objeto de procedimiento. Es el encargado de
      inicializar y ejecutar el procedimiento a partir de una configuracion. Se
      utiliza para mantener una interfaz responsiva y desacoplada del proceso
      de ajuste.
      @abstract(Thread envoltura del objeto de procedimiento.) }
  TThreadProcedimiento = class(TThread)
  private
    {** Indica si el procedimiento fue inicializado exitosamente. }
    FInicializacionExitosa: Boolean;
    {** Indica si el ajuste fue exitoso. }
    FEjecucionExitosa: Boolean;
    {** Indica si el postprocesamiento fue exitoso. }
    FPostProcesamientoExitoso: Boolean;
    {** Evento de notificacion de fin de inicializacion del procedimiento. }
    FOnFinInicializacion: TNotifyEvent;
    {** Evento de notificacion de fin de ejecucion del procedimiento. }
    FOnFinEjecucion: TNotifyEvent;
    {** Objeto de procedimiento. }
    FProcedimiento: TProcedimiento;
    {** Duracion del proceso de inicializacion. }
    FDuracionInicializacion: TDateTime;
    {** Duracion del proceso de iteracion. }
    FDuracionIteracion: TDateTime;
    {** Lista de errores y advertencias generadas durante la ejecucion. }
    FBitacoraEjecucion: TStrings;
    {** Lista de errores y advertencias generadas durante la inicializacion. }
    FBitacoraInicializacion: TStrings;
    {** Bitacora de iteracion. Solo valida para metodos iterativos. }
    FBitacoraIteracion: TStrings;
    {** Bitacora de postprocesamiento. }
    FBitacoraPostprocesamiento: TStrings;

    {** Procedimiento de manejo del evento de fin de ejecucion. }
    procedure NotificarFinEjecucion;
    {** Procedimiento de manejo del evento de fin de inicializacion. }
    procedure NotificarFinInicializacion;
  protected
    {** Procedimiento de ejecucion del thread. }
    procedure Execute; override;
  public
    {** Constructor.
        @param(CreateSuspended Indica si el thread debe crearse suspendido o
        si se debe ejecutar @link(Execute) luego de instanciarlo.)
        @param(ConfiguracionProcedimiento Configuracion del procedimiento de
        ajuste.)
        @param(AOnFinInicializacion Metodo de manejo del evento de fin de
        inicializacion.)
        @param(AOnFinEjecucion Metodo de manejo del evento de fin de
        ejecucion.) }
    constructor Create(const CreateSuspended: Boolean; ConfiguracionProcedimiento: TConfiguracionProcedimiento; const AOnFinInicializacion, AOnFinEjecucion: TNotifyEvent);
    {** Destructor. }
    destructor Destroy; override;
    {** Duracion del proceso de inicializacion. }
    property DuracionInicializacion: TDateTime read FDuracionInicializacion;
    {** Duracion del proceso de iteracion. }
    property DuracionIteracion: TDateTime read FDuracionIteracion;
    {** Indica si la ejecucion finalizo exitosamente. }
    property EjecucionExitosa: Boolean read FEjecucionExitosa;
    {** Indica si la inicializacion finalizo exitosamente. }
    property InicializacionExitosa: Boolean read FInicializacionExitosa;
    {** Indica si el postprocesamiento finalizo exitosamente. }
    property PostProcesamientoExitoso:Boolean read FPostProcesamientoExitoso;
    {** Evento de notificacion de fin de inicializacion del procedimiento. }
    property OnFinInicializacion: TNotifyEvent read FOnFinInicializacion write FOnFinInicializacion;
    {** Evento de notificacion de fin de ejecucion del procedimiento. }
    property OnFinEjecucion: TNotifyEvent read FOnFinEjecucion write FOnFinEjecucion;
    {** Procedimiento de ajuste envuelto. }
    property Procedimiento: TProcedimiento read FProcedimiento;
    {** Lista de mensajes de error o advertencia generados durante la ejecucion
        del procedimiento. }
    property BitacoraEjecucion: TStrings read FBitacoraEjecucion;
    {** Lista de mensajes de error o advertencia generados durante la
        inicializacion del procedimiento. }
    property BitacoraInicializacion: TStrings read FBitacoraInicializacion;
    {** Bitacora de iteracion. Solo valida para metodos iterativos. }
    property BitacoraIteracion: TStrings read FBitacoraIteracion;
    {** Bitacora de postprocesamiento. }
    property BitacoraPostprocesamiento: TStrings read FBitacoraPostprocesamiento;
  end { TThreadProcedimiento };

implementation

// -----------------------------------------------------------------------------
// TInformacionParametro
// -----------------------------------------------------------------------------

constructor TThreadProcedimiento.Create(const CreateSuspended: Boolean; ConfiguracionProcedimiento: TConfiguracionProcedimiento; const AOnFinInicializacion, AOnFinEjecucion: TNotifyEvent);
begin { TThreadProcedimiento.Create }
  inherited Create(CreateSuspended);

  Assert(Assigned(ConfiguracionProcedimiento), 'TThreadProcedimiento.Create: Assigned(ConfiguracionProcedimiento)');

  // Priority := tpLower;
  FreeOnTerminate := True;

  FProcedimiento := TProcedimiento.Create(ConfiguracionProcedimiento);

  FOnFinInicializacion := AOnFinInicializacion;
  FOnFinEjecucion := AOnFinEjecucion;

  FInicializacionExitosa := False;
  FEjecucionExitosa := False;

  FBitacoraInicializacion := TStringList.Create;
  FBitacoraEjecucion := TStringList.Create;
  FBitacoraIteracion := TStringList.Create;
  FBitacoraPostprocesamiento := TStringList.Create;
  FDuracionInicializacion := 0;
  FDuracionIteracion := 0;
end { TThreadProcedimiento.Create };

destructor TThreadProcedimiento.Destroy;
begin { TThreadProcedimiento.Destroy }
  try
    FBitacoraInicializacion.Clear;
    FBitacoraEjecucion.Clear;
    FBitacoraIteracion.Clear;
    FreeAndNil(FBitacoraIteracion);
    FreeAndNil(FBitacoraInicializacion);
    FreeAndNil(FBitacoraEjecucion);
    FreeAndNil(FBitacoraPostProcesamiento);
  finally
    inherited Destroy;
  end;
end { TThreadProcedimiento.Destroy };

procedure TThreadProcedimiento.Execute;
var
  InicioInicializacion, InicioIteracion: TDateTime;
begin { TThreadProcedimiento.Execute }
  try
    InicioInicializacion := Now;
    FInicializacionExitosa := FProcedimiento.Inicializar(FBitacoraInicializacion);
    FDuracionInicializacion := Now - InicioInicializacion;
  except on E: Exception do
    begin
      FInicializacionExitosa := False;
      FBitacoraInicializacion.Add(E.Message);
    end
  end;
  Synchronize(NotificarFinInicializacion);

  if FInicializacionExitosa then
  begin
    try
      InicioIteracion := Now;
      FEjecucionExitosa := FProcedimiento.Ejecutar(FBitacoraEjecucion, FBitacoraIteracion);
      FDuracionIteracion := Now - InicioIteracion;

      try
        if FEjecucionExitosa then
          FPostProcesamientoExitoso := FProcedimiento.PostProcesar(FBitacoraPostprocesamiento);
      except on E: Exception do
        begin
          FPostProcesamientoExitoso := False;
          FBitacoraEjecucion.Add(E.Message);
        end;
      end;
    except on E: Exception do
      begin
        FEjecucionExitosa := False;
        FBitacoraEjecucion.Add(E.Message);
      end;
    end;

    Synchronize(NotificarFinEjecucion);
  end;
end { TThreadProcedimiento.Execute };

procedure TThreadProcedimiento.NotificarFinEjecucion;
begin { TThreadProcedimiento.NotificarFinEjecucion }
  if Assigned(FOnFinEjecucion) then
    FOnFinEjecucion(Self);
end { TThreadProcedimiento.NotificarFinEjecucion };

procedure TThreadProcedimiento.NotificarFinInicializacion;
begin { TThreadProcedimiento.NotificarFinInicializacion }
  if Assigned(FOnFinInicializacion) then
    FOnFinInicializacion(Self);
end { TThreadProcedimiento.NotificarFinInicializacion };

end { UnitThreadProcedimiento }.
