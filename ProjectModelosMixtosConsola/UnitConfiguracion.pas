{**
@abstract(Configuracion del caso.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(September 24, 2004)
Este modulo contiene la clase que almacena la configuracion del caso de estudio.
}

unit UnitConfiguracion;

interface

uses
  Classes, SysUtils,
  UnitParserConfiguracion, UnitConfiguracionDatos, UnitConfiguracionModelo,
  UnitConfiguracionProcedimiento;

type
  {** Configuracion del caso de estudio.
      @abstract(Configuracion del caso de estudio.) }
  TConfiguracion = class
  private
    {** Configuracion de la fuente de datos. }
    FDatos: TConfiguracionDatos;
    {** Fuente de los datos de configuracion. }
    FFuente: TMemoryStream;
    {** Configuracion del modelo. }
    FModelo: TConfiguracionModelo;
    {** Nombre del archivo de configuracion. }
    FNombreArchivoConfiguracion: TFileName;
    {** Parser del archivo de configuracion. }
    FParserConfiguracion: TParserConfiguracion;
    {** Configuracion del procedimiento de ajuste. }
    FProcedimiento: TConfiguracionProcedimiento;
    {** Indica si la configuracion fue validada mediante @link(Validar). }
    FValida: Boolean;
  public
    {** Constructor.
        @param(ANombreArchivoConfiguracion Nombre del archivo que contiene la
        informacion de configuracion.) }
    constructor Create(ANombreArchivoConfiguracion: TFileName); overload;
    {** Constructor.
        @param(Stream Flujo de datos que contiene la informacion de
        configuracion.) }
    constructor Create(Stream: TStream); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Parsea la fuente de configuracion.
        @returns(Devuelve @true si el parseo fue exitoso, o @false en caso
        contrario.) }
    function Parsear: Boolean;
    {** Valida la configuracion.
        @param(BitacoraValidacionDatos Registro de posibles errores y
        advertencias producidas durante el proceso de validacion de datos.)
        @param(BitacoraValidacionModelo Registro de posibles errores y
        advertencias producidas durante el proceso de validacion del modelo.)
        @param(BitacoraValidacionProcedimiento Registro de posibles errores y
        advertencias producidas durante el proceso de validacion del
        procedimiento.)
        @returns(Devuelve @true si la configuracion es valida, o @false en caso
        contrario.) }
    function Validar(var BitacoraValidacionDatos: TStrings; var BitacoraValidacionModelo: TStrings; var BitacoraValidacionProcedimiento: TStrings): Boolean;

    {** Configuracion de la fuente de datos. }
    property Datos: TConfiguracionDatos read FDatos;
    {** Configuracion del modelo. }
    property Modelo: TConfiguracionModelo read FModelo;
    {** Parser de la configracion. }
    property Parser: TParserConfiguracion read FParserConfiguracion;
    {** Configuracion del procedimiento. }
    property Procedimiento: TConfiguracionProcedimiento Read FProcedimiento;
    {** Nombre del archivo de configuracion. }
    property NombreArchivoConfiguracion: TFileName read FNombreArchivoConfiguracion write FNombreArchivoConfiguracion;
    {** La configuracion es valida o no. }
    property Valida: Boolean read FValida;
  end;

implementation

// -----------------------------------------------------------------------------
// TConfiguracion
// -----------------------------------------------------------------------------

constructor TConfiguracion.Create(ANombreArchivoConfiguracion: TFileName);
begin { TConfiguracion.Create }
  try
    // Asignar nombre de archivo de configuracion
    FNombreArchivoConfiguracion := ANombreArchivoConfiguracion;

    // Construir stream fuente a partir del archivo
    FFuente := TMemoryStream.Create;
    FFuente.LoadFromFile(ANombreArchivoConfiguracion);

    // Instanciar el parser de los archivos de configuracion y asignar la fuente
    FParserConfiguracion := TParserConfiguracion.Create(FFuente);

    FValida := false;
  except
    on E: Exception do
    begin
      // Liberar los objetos utilizados
      FreeAndNil(FParserConfiguracion);
      FreeAndNil(FFuente);
      raise E;
    end;
  end;
end { TConfiguracion.Create };

constructor TConfiguracion.Create(Stream: TStream);
begin { TConfiguracion.Create }
  Assert(Assigned(Stream), 'TConfiguracion.Create: Assigned(Stream)');

  try
    // No hay archivo de configuracion
    FNombreArchivoConfiguracion := '';

    // Construir stream a partir del parametro
    FFuente := TMemoryStream.Create;
    FFuente.LoadFromStream(Stream);

    // Instanciar el parser
    FParserConfiguracion := TParserConfiguracion.Create(FFuente);

    FValida := false;
  except
    on E: Exception do
    begin
      // Liberar los objetos utilizados
      FreeAndNil(FParserConfiguracion);
      FreeAndNil(FFuente);
      raise E;
    end;
  end;
end { TConfiguracion.Create };

destructor TConfiguracion.Destroy;
begin { TConfiguracion.Destroy }
  // Liberar los campos
  try
    FFuente.Clear;
  finally
    FreeAndNil(FParserConfiguracion);
    FreeAndNil(FProcedimiento);
    inherited Destroy;
  end;
end { TConfiguracion.Destroy };

function TConfiguracion.Parsear: Boolean;
begin { TConfiguracion.Parsear }
  // Verificar el resultado del parseo
  if FParserConfiguracion.Parsear then
  begin
    // Parseo exitoso

    // Liberar campos utilizados
    FreeAndNil(FProcedimiento);
    FreeAndNil(FModelo);
    FreeAndNil(FDatos);

    // Extraer la informacion del parser
    FDatos := FParserConfiguracion.Datos;
    FModelo := FParserConfiguracion.Modelo;
    FProcedimiento := FParserConfiguracion.Procedimiento;

    FValida := false;
    Result := true;
  end
  else
  begin
    // Parseo erroneo

    FValida := false;
    Result := false;
  end;
end { TConfiguracion.Parsear };

function TConfiguracion.Validar(var BitacoraValidacionDatos: TStrings; var BitacoraValidacionModelo: TStrings; var BitacoraValidacionProcedimiento: TStrings): Boolean;
begin { TConfiguracion.Validar }
  Assert(Assigned(BitacoraValidacionDatos), 'TConfiguracion.Validar: Assigned(BitacoraValidacionDatos)');
  Assert(Assigned(BitacoraValidacionModelo), 'TConfiguracion.Validar: Assigned(BitacoraValidacionModelo)');
  Assert(Assigned(BitacoraValidacionProcedimiento), 'TConfiguracion.Validar: Assigned(BitacoraValidacionProcedimiento)');

  // Validar cada componente de configuracion

  Result := FDatos.Validar(BitacoraValidacionDatos) and
            FModelo.Validar(BitacoraValidacionModelo) and
            FProcedimiento.Validar(BitacoraValidacionProcedimiento);

  FValida := Result;
end { TConfiguracion.Validar };

end { UnitConfiguracion }.
