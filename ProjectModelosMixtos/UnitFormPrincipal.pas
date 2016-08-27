{**
@abstract(TForm principal de la aplicacion)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(February 7, 2005)
Este modulo contiene al TForm principal de la aplicacion.
}
unit UnitFormPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, CommCtrl, ExtCtrls, StdCtrls, Menus, ToolWin, ImgList,
  ActnList, Grids,
  UaVector, UaMatriz, UnitConfiguracion, UnitThreadProcedimiento;

type
  {** TForm principal de la aplicacion.
      @abstract(TForm principal.) }
  TFormPrincipal = class(TForm)
    MenuItemArchivoGuardarConfiguracion: TMenuItem;
    MenuItemArchivoGuardarConfiguracionComo: TMenuItem;
    ActionArchivoAbrirConfiguracion: TAction;
    ActionArchivoSalir: TAction;
    ActionEstadisticasAnalisisVarianza: TAction;
    ActionFuenteAjustarLineas: TAction;
    ActionFuenteGuardar: TAction;
    ActionFuenteGuardarComo: TAction;
    ActionFuenteParsear: TAction;
    ActionArchivoMostrarBitacoraDepuracion: TAction;
    ActionSalidaAjustarLineas: TAction;
    ActionSalidaGuardarComo: TAction;
    ActionSalidaLimpiar: TAction;
    ActionListPrincipal: TActionList;
    CheckBoxFuenteAjustarLineas: TCheckBox;
    CheckBoxSalidaAjustarLineas: TCheckBox;
    ImageListPrincipal: TImageList;
    LabelConfiguracionUltimaConfiguracion: TLabel;
    MainMenuPrincipal: TMainMenu;
    MenuItemArchivo: TMenuItem;
    MenuItemArchivoAbrirConfiguracion: TMenuItem;
    MenuItemArchivoSalir: TMenuItem;
    MenuItemAyuda: TMenuItem;
    MenuItemEstadisticas: TMenuItem;
    MenuItemEstadisticasAnalisisVarianza: TMenuItem;
    N1: TMenuItem;
    PageControlPrincipal: TPageControl;
    RichEditFuente: TRichEdit;
    RichEditSalida: TRichEdit;
    OpenDialogPrincipal: TOpenDialog;
    SaveDialogPrincipal: TSaveDialog;
    TabSheetConfiguracion: TTabSheet;
    TabSheetFuente: TTabSheet;
    TabSheetSalida: TTabSheet;
    ToolBarConfiguracion: TToolBar;
    ToolBarFuente: TToolBar;
    ToolBarSalida: TToolBar;
    ToolBarPrincipal: TToolBar;
    ToolButtonArchivoAbrirConfiguracion: TToolButton;
    ToolButtonArchivoSalir: TToolButton;
    ToolButtonEstadisticasAnalisisVarianza: TToolButton;
    ToolButtonFuenteGuardar: TToolButton;
    ToolButtonFuenteGuardarComo: TToolButton;
    ToolButtonFuenteParsear: TToolButton;
    ToolButtonMostrarBitacoraDepuracion: TToolButton;
    ToolButtonSalidaLimpiar: TToolButton;
    ToolButtonSalidaGuardarComo: TToolButton;
    ToolButtonSeparador1: TToolButton;
    ToolButtonSeparador2: TToolButton;
    ToolButtonSeparador3: TToolButton;
    ToolButtonSeparador4: TToolButton;
    TreeViewConfiguracion: TTreeView;
    StatusBarPrincipal: TStatusBar;
    TimerPrincipal: TTimer;
    ProgressBarPrincipal: TProgressBar;
    procedure ActionArchivoAbrirConfiguracionExecute(Sender: TObject);
    procedure ActionArchivoMostrarBitacoraDepuracionUpdate(Sender: TObject);
    procedure ActionArchivoMostrarBitacoraDepuracionExecute(Sender: TObject);
    procedure ActionArchivoSalirExecute(Sender: TObject);
    procedure ActionEstadisticasAnalisisVarianzaExecute(Sender: TObject);
    procedure ActionEstadisticasAnalisisVarianzaUpdate(Sender: TObject);
    procedure ActionFuenteAjustarLineasExecute(Sender: TObject);
    procedure ActionFuenteAjustarLineasUpdate(Sender: TObject);
    procedure ActionFuenteGuardarExecute(Sender: TObject);
    procedure ActionFuenteGuardarUpdate(Sender: TObject);
    procedure ActionFuenteGuardarComoExecute(Sender: TObject);
    procedure ActionFuenteParsearExecute(Sender: TObject);
    procedure ActionSalidaLimpiarExecute(Sender: TObject);
    procedure ActionSalidaGuardarComoExecute(Sender: TObject);
    procedure ActionSalidaAjustarLineasExecute(Sender: TObject);
    procedure ActionSalidaAjustarLineasUpdate(Sender: TObject);
    procedure TreeViewConfiguracionDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerPrincipalTimer(Sender: TObject);
    procedure HabilitarProgressBarPrincipal;
    procedure DeshabilitarProgressBarPrincipal;
  private
    FTreeNodeTabla: TTreeNode;
    FNombreArchivoConfiguracionActual: TFileName;
    FConfiguracion: TConfiguracion;
    FThreadProcedimiento: TThreadProcedimiento;
    FOnFinEjecucionThreadProcedimiento: TNotifyEvent;
    FOnFinInicializacionThreadProcedimiento: TNotifyEvent;

    procedure ActualizarTreeViewConfiguracion;
    procedure ImprimirStringAdvertenciaRichEditSalida(AString: String);
    procedure ImprimirStringErrorRichEditSalida(AString: String);
    procedure ImprimirStringRichEditSalida(AString: String);
    procedure ImprimirStringSinFormatoRichEditSalida(AString: String);
    procedure ImprimirStringsRichEditSalida(Strings: TStrings);
    procedure ImprimirStringsConVinetasRichEditSalida(Strings: TStrings);
    procedure IrAlFinalRichEditSalida;
    procedure LimpiarTreeViewConfiguracion;
    procedure DoFinEjecucionThreadProcedimiento(Sender: TObject); dynamic;
    procedure DoFinInicializacionThreadProcedimiento(Sender: TObject); dynamic;
    procedure SalidaVerMatriz(const AString: String; const Matriz: TUaMatriz);
    procedure SalidaVerVector(const AString: String; const Vector: TUaVector);
  published
    property OnFinEjecucionThreadProcedimiento: TNotifyEvent read FOnFinEjecucionThreadProcedimiento write FOnFinEjecucionThreadProcedimiento;
    property OnFinInicializacionThreadProcedimiento: TNotifyEvent read FOnFinInicializacionThreadProcedimiento write FOnFinInicializacionThreadProcedimiento;
  end { TFormPrincipal };

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  UnitFormBitacoraDepuracion, UnitFormTabla,
  UnitArregloStrings, UnitEfecto, UnitOpcion, UnitProcedimiento, UaComun;

{$R *.dfm}

// -----------------------------------------------------------------------------
// TFormPrincipal
// -----------------------------------------------------------------------------

procedure TFormPrincipal.ActionArchivoAbrirConfiguracionExecute(Sender: TObject);
var
  Configuracion: TConfiguracion;
  NombreArchivoConfiguracionAnterior: TFileName;
  Stream: TStream;
  Strings, BitacoraInicializacion, BitacoraValidacionDatos, BitacoraValidacionModelo, BitacoraValidacionProcedimiento: TStrings;
begin { TFormPrincipal.ActionArchivoAbrirConfiguracionExecute }
  // Almacenar el nombre de archivo de la configuracion actual
  NombreArchivoConfiguracionAnterior := FNombreArchivoConfiguracionActual;

  try
    if OpenDialogPrincipal.Execute then
    begin
      // Cargar las lineas del archivo de datos
      Strings := TStringList.Create;
      Strings.LoadFromFile(OpenDialogPrincipal.FileName);

      // Crear un stream en memoria a partir de las lineas para contruir la configuracion
      Stream := TMemoryStream.Create;
      Strings.SaveToStream(Stream);

      // Construir la configuracion parseando el stream
      Configuracion := TConfiguracion.Create(Stream);
      FreeAndNil(Stream);

      // Cargar las lineas a la interfaz
      RichEditFuente.Lines := Strings;

      // Liberar las lineas
      FreeAndNil(Strings);

      // Actualizar los nombres de archivo correspondientes
      Configuracion.NombreArchivoConfiguracion := OpenDialogPrincipal.FileName;
      FNombreArchivoConfiguracionActual := OpenDialogPrincipal.FileName;

      ImprimirStringRichEditSalida('Parseando archivo de configuraci�n...');

      // Parsear la fuente de configuracion
      if Configuracion.Parsear then
      begin
        // Parseo exitoso

        ImprimirStringRichEditSalida('Validando configuraci�n...');

        // Inicializar la configuracion para poder validarla

        BitacoraInicializacion := TStringList.Create;

        if Configuracion.Modelo.Inicializar(BitacoraInicializacion) then
        begin
          // Verificar si se produjeron advertencias durante la inicializacion del modelo

          if BitacoraInicializacion.Count > 0 then
          begin
            ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la inicializacion del modelo:');
            ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
          end;

          // Validar la configuracion

          BitacoraValidacionDatos := TStringList.Create;
          BitacoraValidacionModelo := TStringList.Create;
          BitacoraValidacionProcedimiento := TStringList.Create;

          if Configuracion.Validar(BitacoraValidacionDatos, BitacoraValidacionModelo, BitacoraValidacionProcedimiento) then
          begin
            // Configuracion valida

            // Liberar configuracion anterior y asignar la nueva
            FreeAndNil(FConfiguracion);
            FConfiguracion := Configuracion;

            // Actualizar componentes visuales
            LimpiarTreeViewConfiguracion;
            ActualizarTreeViewConfiguracion;
            TabSheetConfiguracion.TabVisible := true;
            ActionEstadisticasAnalisisVarianza.Enabled := true;

            // Imprimir advertencias
            if BitacoraValidacionDatos.Count > 0 then
            begin
              ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la validaci�n de datos:');
              ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionDatos);
            end;

            if BitacoraValidacionModelo.Count > 0 then
            begin
              ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la validaci�n de modelo:');
              ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionModelo);
            end;

            if BitacoraValidacionProcedimiento.Count > 0 then
            begin
              ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la validaci�n de procedimiento:');
              ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionProcedimiento);
            end;

            ImprimirStringRichEditSalida('Configuracion exitosa.');
          end
          else
          begin
            // Configuracion invalida, imprimir los errores

            if BitacoraValidacionDatos.Count > 0 then
            begin
              ImprimirStringErrorRichEditSalida('Errores de datos:');
              ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionDatos);
            end;

            if BitacoraValidacionModelo.Count > 0 then
            begin
              ImprimirStringErrorRichEditSalida('Errores de modelo:');
              ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionModelo);
            end;

            if BitacoraValidacionProcedimiento.Count > 0 then
            begin
              ImprimirStringErrorRichEditSalida('Errores de procedimiento:');
              ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionProcedimiento);
            end;

            ImprimirStringErrorRichEditSalida('Configuracion fallida.');

            // Liberar la configuracion parseada
            FreeAndNil(Configuracion);
          end;

          // Liberar bitacoras
          FreeAndNil(BitacoraValidacionDatos);
          FreeAndNil(BitacoraValidacionModelo);
          FreeAndNil(BitacoraValidacionProcedimiento);
        end
        else
        begin
          // Se produjeron errores durante la inicializacion

          ImprimirStringErrorRichEditSalida('Errores generados durante la inicializacion del modelo:');
          ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
          ImprimirStringErrorRichEditSalida('Configuracion fallida.');
        end;

        // Liberar bitacoras
        FreeAndNil(BitacoraInicializacion);
      end
      else
      begin
        // Parseo fallido, imprimir errores
        ImprimirStringErrorRichEditSalida('Se generaron errores durante el parseo de la configuraci�n:');
        ImprimirStringSinFormatoRichEditSalida('Error de sintaxis: Linea #' + IntToStr(Configuracion.Parser.ErrorSintaxis.IndiceLinea) + ', Columna #' + IntToStr(Configuracion.Parser.ErrorSintaxis.IndiceColumna) + ' "' + Configuracion.Parser.ErrorSintaxis.Contexto + '".');
        ImprimirStringErrorRichEditSalida('Configuracion fallida.');

        // Liberar los errores y la configuracion erronea
        FreeAndNil(Configuracion);
      end;

      // Actualizar interfaz
      IrAlFinalRichEditSalida;
      RichEditFuente.Modified := false;
    end;
  except
    on E: Exception do
    begin
      // Se produjo una excepcion durante el proceso de lectura de la configuracion

      ImprimirStringErrorRichEditSalida('Se produjo un error cargando la configuraci�n: ' + E.Message + '.');
      ImprimirStringErrorRichEditSalida('Configuracion fallida.');
      IrAlFinalRichEditSalida;

      // Reinicializar nombre de configuracion anterior
      FNombreArchivoConfiguracionActual := NombreArchivoConfiguracionAnterior;

      // Liberar los objetos utilizados
      FreeAndNil(Stream);
      FreeAndNil(Strings);
      FreeAndNil(BitacoraValidacionDatos);
      FreeAndNil(BitacoraValidacionModelo);
      FreeAndNil(BitacoraValidacionProcedimiento);
      FreeAndNil(Configuracion);
    end;
  end;
end { TFormPrincipal.ActionArchivoAbrirConfiguracionExecute };

procedure TFormPrincipal.ActionArchivoMostrarBitacoraDepuracionUpdate(Sender: TObject);
begin { TFormPrincipal.ActionArchivoMostrarBitacoraDepuracionUpdate }
  // Alternar el estado del boton de la bitacora de depuracion
  ActionArchivoMostrarBitacoraDepuracion.Checked := FormBitacoraDepuracion.Showing;
end { TFormPrincipal.ActionArchivoMostrarBitacoraDepuracionUpdate };

procedure TFormPrincipal.ActionArchivoMostrarBitacoraDepuracionExecute(Sender: TObject);
begin { TFormPrincipal.ActionArchivoMostrarBitacoraDepuracionExecute }
  // Mostrar u ocultar la bitacora de depuracion
  if FormBitacoraDepuracion.Showing then
    FormBitacoraDepuracion.Hide
  else
    FormBitacoraDepuracion.Show;
end { TFormPrincipal.ActionArchivoMostrarBitacoraDepuracionExecute };

procedure TFormPrincipal.ActionArchivoSalirExecute(Sender: TObject);
begin { TFormPrincipal.ActionArchivoSalirExecute }
  if RichEditFuente.Modified then
  begin
    // La fuente fue modificada, confirmar salida
    if MessageDlg('La salida fue modificada. Desea salir y perder las modificaciones?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Close;
  end
  else
    Close;
end { TFormPrincipal.ActionArchivoSalirExecute };

procedure TFormPrincipal.ActionEstadisticasAnalisisVarianzaExecute(Sender: TObject);
begin { TFormPrincipal.ActionEstadisticasAnalisisVarianzaExecute }
  // Inicializar el procedmiento

  ImprimirStringRichEditSalida('Instanciando procedimiento...');
  FThreadProcedimiento := TThreadProcedimiento.Create(True, FConfiguracion.Procedimiento, FOnFinInicializacionThreadProcedimiento, FOnFinEjecucionThreadProcedimiento);
  ImprimirStringRichEditSalida('Inicializando procedimiento...');
  ActionEstadisticasAnalisisVarianza.Enabled := False;
  Invalidate;
  FThreadProcedimiento.Resume;
end { TFormPrincipal.ActionEstadisticasAnalisisVarianzaExecute };

procedure TFormPrincipal.ActionEstadisticasAnalisisVarianzaUpdate(Sender: TObject);
begin { TFormPrincipal.ActionEstadisticasAnalisisVarianzaUpdate }
  // Perimitir el analisis solo si la configuracion existe y fue validada
  ActionEstadisticasAnalisisVarianza.Enabled := Assigned(FConfiguracion) and FConfiguracion.Valida
end { TFormPrincipal.ActionEstadisticasAnalisisVarianzaUpdate };

procedure TFormPrincipal.ActionFuenteAjustarLineasExecute(Sender: TObject);
begin { TFormPrincipal.ActionFuenteAjustarLineasExecute }
  RichEditFuente.WordWrap := not RichEditFuente.WordWrap;
end { TFormPrincipal.ActionFuenteAjustarLineasExecute };

procedure TFormPrincipal.ActionFuenteAjustarLineasUpdate(Sender: TObject);
begin { TFormPrincipal.ActionFuenteAjustarLineasUpdate }
  ActionFuenteAjustarLineas.Checked := RichEditFuente.WordWrap;
end { TFormPrincipal.ActionFuenteAjustarLineasUpdate };

procedure TFormPrincipal.ActionFuenteGuardarExecute(Sender: TObject);
var
  NombreArchivoConfiguracionAnterior: TFileName; // Nombre de archivo de la configuracion en memoria
begin { TFormPrincipal.ActionFuenteGuardarExecute }
  // Guardar la fuente

  // Elegir el filtro para archivos de texto plano
  SaveDialogPrincipal.FilterIndex := 1;

  // Almacenar el nombre de archivo actual por precaucion
  NombreArchivoConfiguracionAnterior := FNombreArchivoConfiguracionActual;

  try
    if FNombreArchivoConfiguracionActual = '' then
    begin
      // La configuracion no fue editada desde un archivo existente

      if SaveDialogPrincipal.Execute then
      begin
        // Si la configuracion fue creada, almacenar el nombre elegido
        if Assigned(FConfiguracion) then
          FConfiguracion.NombreArchivoConfiguracion := SaveDialogPrincipal.FileName;

        // Almacenar el nombre elegido en el form
        FNombreArchivoConfiguracionActual := SaveDialogPrincipal.FileName;

        // Grabar el archivo y actualizar la interfaz
        RichEditFuente.Lines.SaveToFile(FNombreArchivoConfiguracionActual);
        ImprimirStringRichEditSalida('Fuente guardada exitosamente en ' + FNombreArchivoConfiguracionActual + '.');
        RichEditFuente.Modified := false;
      end;
    end
    else
    begin
      // La fuente fue editada desde un archivo existente

      // Grabar el archivo y actualizar la interfaz
      RichEditFuente.Lines.SaveToFile(FNombreArchivoConfiguracionActual);
      ImprimirStringRichEditSalida('Fuente guardada exitosamente en ' + FNombreArchivoConfiguracionActual + '.');
      RichEditFuente.Modified := false;
    end;
  except
    on E: Exception do
    begin
      // Se produjo algun error grabando la fuente

      ImprimirStringErrorRichEditSalida('Se produjo un error al guardar la fuente: ' + E.Message);
      IrAlFinalRichEditSalida;

      // Reinicializar nombre de configuracion anterior
      FNombreArchivoConfiguracionActual := NombreArchivoConfiguracionAnterior;
      if Assigned(FConfiguracion) then
        FConfiguracion.NombreArchivoConfiguracion := NombreArchivoConfiguracionAnterior;
    end;
  end;
end { TFormPrincipal.ActionFuenteGuardarExecute };

procedure TFormPrincipal.ActionFuenteGuardarUpdate(Sender: TObject);
begin { TFormPrincipal.ActionFuenteGuardarUpdate }
  // Solo permitir guardar si la fuente fue modificada
  ActionFuenteGuardar.Enabled := RichEditFuente.Modified;
end { TFormPrincipal.ActionFuenteGuardarUpdate };

procedure TFormPrincipal.ActionFuenteGuardarComoExecute(Sender: TObject);
var
  NombreArchivoConfiguracionAnterior: TFileName;
begin { TFormPrincipal.ActionFuenteGuardarComoExecute }
  // Guardar la fuente consultando al usuario por el nombre de archivo

  // Elegir el filtro para archivos de texto plano
  SaveDialogPrincipal.FilterIndex := 1;

  // Almacenar el nombre de archivo actual por precaucion
  NombreArchivoConfiguracionAnterior := FNombreArchivoConfiguracionActual;

  try
    if SaveDialogPrincipal.Execute then
    begin
      // Si la configuracion fue creada, almacenar el nombre elegido
      if Assigned(FConfiguracion) then
        FConfiguracion.NombreArchivoConfiguracion := SaveDialogPrincipal.FileName;

      // Almacenar el nombre elegido en el form
      FNombreArchivoConfiguracionActual := SaveDialogPrincipal.FileName;

      // Grabar el archivo y actualizar la interfaz
      RichEditFuente.Lines.SaveToFile(FConfiguracion.NombreArchivoConfiguracion);
      ImprimirStringRichEditSalida('Fuente guardada exitosamente en ' + FConfiguracion.NombreArchivoConfiguracion + '.');
      RichEditFuente.Modified := false;
    end;
  except
    on E: Exception do
    begin
      // Se produjo algun error grabando la fuente

      ImprimirStringErrorRichEditSalida('Se produjo un error al guardar la fuente: ' + E.Message);
      IrAlFinalRichEditSalida;

      // Reinicializar nombre de configuracion anterior
      FNombreArchivoConfiguracionActual := NombreArchivoConfiguracionAnterior;
      if Assigned(FConfiguracion) then
        FConfiguracion.NombreArchivoConfiguracion := NombreArchivoConfiguracionAnterior;
    end;
  end;
end { TFormPrincipal.ActionFuenteGuardarComoExecute };

procedure TFormPrincipal.ActionFuenteParsearExecute(Sender: TObject);
var
  Configuracion: TConfiguracion;
  NombreArchivoConfiguracionAnterior: TFileName;
  Stream: TStream;
  Strings, BitacoraInicializacion, BitacoraValidacionDatos, BitacoraValidacionModelo, BitacoraValidacionProcedimiento: TStrings;
begin { TFormPrincipal.ActionFuenteParsearExecute }
  try
    // Crear el stream a partir de la fuente actual
    Stream := TMemoryStream.Create;
    RichEditFuente.Lines.SaveToStream(Stream);

    // Crear la nueva configuracion desde el stream
    Configuracion := TConfiguracion.Create(Stream);
    FreeAndNil(Stream);

    ImprimirStringRichEditSalida('Parseando archivo de configuraci�n...');

    // Parsear el archivo de configuracion
    if Configuracion.Parsear then
    begin
      // Parseo exitoso

      ImprimirStringRichEditSalida('Validando configuracion...');

      // Inicializar la configuracion para validar

      BitacoraInicializacion := TStringList.Create;

      if Configuracion.Modelo.Inicializar(BitacoraInicializacion) then
      begin
        // Verificar si se produjeron advertencias durante la inicializacion del modelo
        if BitacoraInicializacion.Count > 0 then
        begin
          ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la inicializacion del modelo:');
          ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
        end;

        // Validar la configuracion

        BitacoraValidacionDatos := TStringList.Create;
        BitacoraValidacionModelo := TStringList.Create;
        BitacoraValidacionProcedimiento := TStringList.Create;

        if Configuracion.Validar(BitacoraValidacionDatos, BitacoraValidacionModelo, BitacoraValidacionProcedimiento) then
        begin
          // Configuracion valida

          // Asignar el nombre de archivo de la configuracion anterior a la nueva
          Configuracion.NombreArchivoConfiguracion := FNombreArchivoConfiguracionActual;

          // Liberar la configuracion actual y asignar la nueva
          FreeAndNil(FConfiguracion);
          FConfiguracion := Configuracion;

          // Actualizar interfaz
          LimpiarTreeViewConfiguracion;
          ActualizarTreeViewConfiguracion;
          TabSheetConfiguracion.TabVisible := true;
          ActionEstadisticasAnalisisVarianza.Enabled := true;

          // Imprimir advertencias
          if BitacoraValidacionDatos.Count > 0 then
          begin
            ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la validaci�n de datos:');
            ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionDatos);
          end;

          if BitacoraValidacionModelo.Count > 0 then
          begin
            ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la validaci�n de modelo:');
            ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionModelo);
          end;

          if BitacoraValidacionProcedimiento.Count > 0 then
          begin
            ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas en la validaci�n de procedimiento:');
            ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionProcedimiento);
          end;

          ImprimirStringRichEditSalida('Configuracion exitosa.');
        end
        else
        begin
          // Configuracion invalida, imprimir errores de validacion
          ImprimirStringErrorRichEditSalida('Se produjeron errores durante la validaci�n de la configuraci�n:');

          if BitacoraValidacionDatos.Count > 0 then
          begin
            ImprimirStringErrorRichEditSalida('Errores en los datos:');
            ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionDatos);
          end;

          if BitacoraValidacionModelo.Count > 0 then
          begin
            ImprimirStringErrorRichEditSalida('Errores de modelo');
            ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionModelo);
          end;

          if BitacoraValidacionProcedimiento.Count > 0 then
          begin
            ImprimirStringErrorRichEditSalida('Errores de procedimiento');
            ImprimirStringsConVinetasRichEditSalida(BitacoraValidacionProcedimiento);
          end;

          ImprimirStringErrorRichEditSalida('Configuracion fallida.');

          // Liberar configuracion invalida parseada
          FreeAndNil(Configuracion);
        end;

        // Liberar bitacoras
        FreeAndNil(BitacoraValidacionDatos);
        FreeAndNil(BitacoraValidacionModelo);
        FreeAndNil(BitacoraValidacionProcedimiento);
      end
      else
      begin
        // Se produjeron errores durante la inicializacion

        ImprimirStringErrorRichEditSalida('Errores durante la inicializacion del modelo:');
        ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
        ImprimirStringErrorRichEditSalida('Configuracion fallida.');
      end;

      // Liberar bitacoras
      FreeAndNil(BitacoraInicializacion);
    end
    else
    begin
      // Parseo erroneo, imprimir errores del parser
      ImprimirStringErrorRichEditSalida('Se produjeron errores durante el parseo de la configuraci�n:');
      ImprimirStringSinFormatoRichEditSalida('Error de sintaxis: Linea #' + IntToStr(Configuracion.Parser.ErrorSintaxis.IndiceLinea) + ', Columna #' + IntToStr(Configuracion.Parser.ErrorSintaxis.IndiceColumna) + ' "' + Configuracion.Parser.ErrorSintaxis.Contexto + '".');
      ImprimirStringErrorRichEditSalida('Configuracion fallida.');

      // Liberar errores y configuracion erronea
      FreeAndNil(Configuracion);
    end;

    // Actualizar interfaz
    IrAlFinalRichEditSalida;
  except
    on E: Exception do
    begin
      // Se produjo un error parseando la configuracion

      ImprimirStringErrorRichEditSalida('Se produjo un error cargando la configuraci�n: ' + E.Message + '.');
      ImprimirStringErrorRichEditSalida('Configuracion fallida.');
      IrAlFinalRichEditSalida;

      // Reinicializar el nombre de archivo de configuracion anterior
      FNombreArchivoConfiguracionActual := NombreArchivoConfiguracionAnterior;

      // Liberar objetos utilizados
      FreeAndNil(Stream);
      FreeAndNil(Strings);
      FreeAndNil(BitacoraValidacionDatos);
      FreeAndNil(BitacoraValidacionModelo);
      FreeAndNil(BitacoraValidacionProcedimiento);
      FreeAndNil(Configuracion);
    end;
  end;
end { TFormPrincipal.ActionFuenteParsearExecute };

procedure TFormPrincipal.ActionSalidaLimpiarExecute(Sender: TObject);
var
  TabSheet: TTabSheet;
  Listo: Boolean;
begin { TFormPrincipal.ActionSalidaLimpiarExecute }
  // Ocultar el PageControl
  PageControlPrincipal.Hide;

  RichEditSalida.Clear;

  // Eliminar cada TabSheet adicional del PageControl
  Listo := False;
  while (not Listo) do
  begin
    TabSheet := PageControlPrincipal.FindNextPage(TabSheetSalida, true, false);
    if TabSheet = TabSheetFuente then
      Listo := True
    else
    begin
      TabSheet.Parent := nil;
      FreeAndNil(TabSheet);
    end;
  end;

  PageControlPrincipal.ActivePage := TabSheetSalida;

  // Mostrar el PageControl
  PageControlPrincipal.Show;
end { TFormPrincipal.ActionSalidaLimpiarExecute };

procedure TFormPrincipal.ActionSalidaGuardarComoExecute(Sender: TObject);
begin { TFormPrincipal.ActionSalidaGuardarComoExecute }
  // Guardar la salida

  // Asignar un nombre por defecto a la salida
  SaveDialogPrincipal.FileName := 'salida.txt';

  try
    if SaveDialogPrincipal.Execute then
    begin
      // Grabar la salida y actualizar la interfaz
      RichEditSalida.Lines.SaveToFile(SaveDialogPrincipal.FileName);
      RichEditFuente.Modified := false;
    end
  except
    on E: Exception do
    begin
      // Se produjo un error grabando la salida

      ImprimirStringErrorRichEditSalida('Se produjo un error al guardar la salida: ' + E.Message);
    end;
  end;
end { TFormPrincipal.ActionSalidaGuardarComoExecute };

procedure TFormPrincipal.ActionSalidaAjustarLineasExecute(Sender: TObject);
begin { TFormPrincipal.ActionSalidaAjustarLineasExecute }
  RichEditSalida.WordWrap := not RichEditSalida.WordWrap;
end { TFormPrincipal.ActionSalidaAjustarLineasExecute };

procedure TFormPrincipal.ActionSalidaAjustarLineasUpdate(Sender: TObject);
begin { TFormPrincipal.ActionSalidaAjustarLineasUpdate }
  ActionSalidaAjustarLineas.Checked := RichEditSalida.WordWrap;
end { TFormPrincipal.ActionSalidaAjustarLineasUpdate };

procedure TFormPrincipal.TreeViewConfiguracionDblClick(Sender: TObject);
var
  FormTabla: TFormTabla; // Form con la tabla de los datos de la fuente
begin { TFormPrincipal.TreeViewConfiguracionDblClick }
  // Verificar si se hizo doble click en el nodo de los datos
  if TreeViewConfiguracion.Selected = FTreeNodeTabla then
  begin
    // Crear un form con la tabla de datos y mostrarla
    FormTabla := TFormTabla.Create(Self, FConfiguracion.Datos);
    FormTabla.ShowModal;

    // Liberar el form
    FreeAndNil(FormTabla);
  end;
end { TFormPrincipal.TreeViewConfiguracionDblClick };

procedure TFormPrincipal.ActualizarTreeViewConfiguracion;
var
  I, J: Integer;                  // Indices
  NodoConfiguracion: TTreeNode;   // Nodo raiz
  NodoDatos: TTreeNode;           // Nodo de datos
  NodoModelo: TTreeNode;          // Nodo de modelos
  NodoProcedimiento: TTreeNode;   // Nodo de procedimiento
  Nodo1, Nodo2, Nodo3: TTreeNode; // Nodos auxiliares
begin { TFormPrincipal.ActualizarTreeViewConfiguracion }
  // Presentar la informacion de configuracion en el TreeView correspondiente

  // Limpiar el contenido actual del TreeView
  TreeViewConfiguracion.Items.Clear;

  // Construir nodo principal
  NodoConfiguracion := TreeViewConfiguracion.Items.AddFirst(nil, 'Configuraci�n');

  // Construir sub-arbol de datos
  if FConfiguracion.Datos.Etiqueta = '' then
    NodoDatos := TreeViewConfiguracion.Items.AddChild(NodoConfiguracion, 'Datos')
  else
    NodoDatos := TreeViewConfiguracion.Items.AddChild(NodoConfiguracion, 'Datos (' + FConfiguracion.Datos.Etiqueta + ')');

  Nodo1 := TreeViewConfiguracion.Items.AddChild(NodoDatos, 'Columnas');
  for I := 0 to FConfiguracion.Datos.Columnas.Count - 1 do
    TreeViewConfiguracion.Items.AddChild(Nodo1, FConfiguracion.Datos.Columnas [I].Texto);

  Nodo1 := TreeViewConfiguracion.Items.AddChild(NodoDatos, 'Valores');
  TreeViewConfiguracion.Items.AddChild(Nodo1, 'Cantidad Filas: ' + IntToStr(FConfiguracion.Datos.Valores.CantidadFilas));
  TreeViewConfiguracion.Items.AddChild(Nodo1, 'Cantidad Columnas: ' + IntToStr(FConfiguracion.Datos.Valores.CantidadColumnas));
  FTreeNodeTabla := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Tabla (Doble click para visualizar tabla de valores)');

  // Construir sub-arbol de modelo
  if FConfiguracion.Modelo.Etiqueta = '' then
    NodoModelo := TreeViewConfiguracion.Items.AddChild(NodoConfiguracion, 'Modelo')
  else
    NodoModelo := TreeViewConfiguracion.Items.AddChild(NodoConfiguracion, 'Modelo (' + FConfiguracion.Modelo.Etiqueta + ')');

  TreeViewConfiguracion.Items.AddChild(NodoModelo, 'Variable Dependiente: ' + FConfiguracion.Modelo.VariableDependiente);

  Nodo1 := TreeViewConfiguracion.Items.AddChild(NodoModelo, 'Variables de Clasificaci�n');
  for I := 0 to FConfiguracion.Modelo.VariablesClasificacion.Count - 1 do
    TreeViewConfiguracion.Items.AddChild(Nodo1, FConfiguracion.Modelo.VariablesClasificacion [I]);

  Nodo1 := TreeViewConfiguracion.Items.AddChild(NodoModelo, 'Efectos Fijos');
  Nodo2 := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Efectos');
  for I := 0 to FConfiguracion.Modelo.EfectosFijos.Efectos.Count - 1 do
  begin
    TreeViewConfiguracion.Items.AddChild(Nodo2, FConfiguracion.Modelo.EfectosFijos.Efectos [I].Texto);
  end;

  if FConfiguracion.Modelo.EfectosFijos.Opciones.Count > 0 then
  begin
    Nodo2 := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Opciones');
    for I := 0 to FConfiguracion.Modelo.EfectosFijos.Opciones.Count - 1 do
      TreeViewConfiguracion.Items.AddChild(Nodo2, FConfiguracion.Modelo.EfectosFijos.Opciones [I].Texto);
  end;

  Nodo1 := TreeViewConfiguracion.Items.AddChild(NodoModelo, 'Efectos Aleatorios');
  for I := 0 to FConfiguracion.Modelo.GruposEfectosAleatorios.Count - 1 do
  begin
    Nodo2 := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Grupo #' + IntToStr(I));
    Nodo3 := TreeViewConfiguracion.Items.AddChild(Nodo2, 'Efectos');
    for J := 0 to FConfiguracion.Modelo.GruposEfectosAleatorios [I].Efectos.Count - 1 do
    begin
      TreeViewConfiguracion.Items.AddChild(Nodo3, FConfiguracion.Modelo.GruposEfectosAleatorios [I].Efectos [J].Texto);
    end;

    if FConfiguracion.Modelo.GruposEfectosAleatorios [I].Opciones.Count > 0 then
    begin
      Nodo3 := TreeViewConfiguracion.Items.AddChild(Nodo2, 'Opciones');
      for J := 0 to FConfiguracion.Modelo.GruposEfectosAleatorios [I].Opciones.Count - 1 do
        TreeViewConfiguracion.Items.AddChild(Nodo3, FConfiguracion.Modelo.GruposEfectosAleatorios [I].Opciones [J].Texto);
    end;
  end;

  if FConfiguracion.Modelo.EfectosAleatorios.Efectos.Count > 0 then
  begin
    Nodo2 := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Efectos');
    for I := 0 to FConfiguracion.Modelo.EfectosAleatorios.Efectos.Count - 1 do
      TreeViewConfiguracion.Items.AddChild(Nodo2, FConfiguracion.Modelo.EfectosAleatorios.Efectos [I].Texto);
  end;

  if FConfiguracion.Modelo.EfectosAleatorios.Opciones.Count > 0 then
  begin
    Nodo2 := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Opciones');
    for I := 0 to FConfiguracion.Modelo.EfectosAleatorios.Opciones.Count - 1 do
      TreeViewConfiguracion.Items.AddChild(Nodo2, FConfiguracion.Modelo.EfectosAleatorios.Opciones [I].Texto);
  end;

  if FConfiguracion.Modelo.Error.Count > 0 then
  begin
    Nodo2 := TreeViewConfiguracion.Items.AddChild(Nodo1, 'Error');
    for I := 0 to FConfiguracion.Modelo.Error.Count - 1 do
      TreeViewConfiguracion.Items.AddChild(Nodo2, FConfiguracion.Modelo.Error [I].Texto);
  end;

  if FConfiguracion.Modelo.Opciones.Count > 0 then
  begin
    Nodo1 := TreeViewConfiguracion.Items.AddChild(NodoModelo, 'Opciones');
    for I := 0 to FConfiguracion.Modelo.Opciones.Count - 1 do
      TreeViewConfiguracion.Items.AddChild(Nodo1, FConfiguracion.Modelo.Opciones [I].Texto);
  end;

  // Construir sub-arbol de procedimiento
  NodoProcedimiento := TreeViewConfiguracion.Items.AddChild(NodoConfiguracion, 'Procedimiento');

  if FConfiguracion.Procedimiento.Opciones.Count > 0 then
  begin
    for I := 0 to FConfiguracion.Procedimiento.Opciones.Count - 1 do
      TreeViewConfiguracion.Items.AddChild(NodoProcedimiento, FConfiguracion.Procedimiento.Opciones [I].Texto);
  end;

  NodoConfiguracion.Expand(false);
end { TFormPrincipal.ActualizarTreeViewConfiguracion };

procedure TFormPrincipal.ImprimirStringAdvertenciaRichEditSalida(AString: String);
begin { TFormPrincipal.ImprimirStringAdvertenciaRichEditSalida }
  RichEditSalida.Lines.Add(TimeToStr(Now) + ' ADVERTENCIA> ' + AString);
  StatusBarPrincipal.Panels [0].Text := AString;
end { TFormPrincipal.ImprimirStringAdvertenciaRichEditSalida };

procedure TFormPrincipal.ImprimirStringErrorRichEditSalida(AString: String);
begin { TFormPrincipal.ImprimirStringErrorRichEditSalida }
  RichEditSalida.Lines.Add(TimeToStr(Now) + ' ERROR> ' + AString);
  StatusBarPrincipal.Panels [0].Text := AString;
end { TFormPrincipal.ImprimirStringErrorRichEditSalida };

procedure TFormPrincipal.ImprimirStringRichEditSalida(AString: String);
begin { TFormPrincipal.ImprimirStringRichEditSalida }
  RichEditSalida.Lines.Add(TimeToStr(Now) + '> ' + AString);
  StatusBarPrincipal.Panels [0].Text := AString;
end { TFormPrincipal.ImprimirStringRichEditSalida };

procedure TFormPrincipal.ImprimirStringSinFormatoRichEditSalida(AString: String);
begin { TFormPrincipal.ImprimirStringSinFormatoRichEditSalida }
  RichEditSalida.Lines.Add(AString);
end { TFormPrincipal.ImprimirStringSinFormatoRichEditSalida };

procedure TFormPrincipal.ImprimirStringsRichEditSalida(Strings: TStrings);
begin { TFormPrincipal.ImprimirStringsRichEditSalida }
  RichEditSalida.Lines.AddStrings(Strings);
end { TFormPrincipal.ImprimirStringsRichEditSalida };

procedure TFormPrincipal.ImprimirStringsConVinetasRichEditSalida(Strings: TStrings);
var
  I: Integer;
begin { TFormPrincipal.ImprimirStringsConVinetasRichEditSalida }
  for I := 0 to Strings.Count - 1 do
    RichEditSalida.Lines.Add('  * ' + Strings [I]);
end { TFormPrincipal.ImprimirStringsConVinetasRichEditSalida };

procedure TFormPrincipal.IrAlFinalRichEditSalida;
begin { TFormPrincipal.IrAlFinalRichEditSalida }
  RichEditSalida.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end { TFormPrincipal.IrAlFinalRichEditSalida };

procedure TFormPrincipal.LimpiarTreeViewConfiguracion;
begin { TFormPrincipal.LimpiarTreeViewConfiguracion }
  TreeViewConfiguracion.Items.Clear;
end { TFormPrincipal.LimpiarTreeViewConfiguracion };

procedure TFormPrincipal.FormShow(Sender: TObject);
var
  Rect: TRect;
begin { FormPrincipal.FormShow }
  StatusBarPrincipal.Perform(SB_GETRECT, 1, Integer(@Rect));
  ProgressBarPrincipal.Parent := StatusBarPrincipal;
  ProgressBarPrincipal.Top := Rect.Top;
  ProgressBarPrincipal.Left := Rect.Left;
  ProgressBarPrincipal.Width := Rect.Right - Rect.Left;
  ProgressBarPrincipal.Height := Rect.Bottom - Rect.Top;
end { FormPrincipal.FormShow };

procedure TFormPrincipal.TimerPrincipalTimer(Sender: TObject);
begin { TFormPrincipal.TimerPrincipalTimer }
  ProgressBarPrincipal.StepIt;
end { TFormPrincipal.TimerPrincipalTimer };

procedure TFormPrincipal.HabilitarProgressBarPrincipal;
begin { TFormPrincipal.HabilitarProgressBarPrincipal }
  ProgressBarPrincipal.Position := ProgressBarPrincipal.Min;
  TimerPrincipal.Enabled := true;
end { TFormPrincipal.HabilitarProgressBarPrincipal };

procedure TFormPrincipal.DeshabilitarProgressBarPrincipal;
begin { TFormPrincipal.DeshabilitarProgressBarPrincipal }
  TimerPrincipal.Enabled := false;
  ProgressBarPrincipal.Position := ProgressBarPrincipal.Min;
end { TFormPrincipal.DeshabilitarProgressBarPrincipal };

procedure TFormPrincipal.DoFinEjecucionThreadProcedimiento;
var
  I, IndiceOpcionSalida, IndiceGrupo, IndiceEfecto, IndiceNivel, IndiceParametro, IndiceVariable, IndiceEstimador: Integer;
  AnchoCampo1, AnchoCampo2, AnchoCampo3, AnchoCampo4: Integer;
  CantidadVariablesClasificacion, CantidadColumnasZ: Integer;
  Procedimiento: TProcedimiento;
  String1, String2: String;
  Strings: TStrings;
  OpcionesSalida: TArregloStrings;
  EscribirArchivoSalida, TieneUnidadExperimental: Boolean;
  NombreArchivoSalida: TFileName;
  ArchivoSalida: TFileStream;
begin { TFormPrincipal.DoFinEjecucionThreadProcedimiento }
  DeshabilitarProgressBarPrincipal;

  Strings := TStringList.Create;

  Procedimiento := FThreadProcedimiento.Procedimiento;

  // Obtener o construir arreglo de componentes de salida
  if Procedimiento.Opciones.BuscarAsignar(I, 'salida') then
  begin
    // Se configuro un arreglo de componentes de salida

    OpcionesSalida := (Procedimiento.Opciones [I] as TOpcionParametroTObject).Parametro as TArregloStrings;
  end
  else
  begin
    // No se configuro un arreglo de componentes de salida, contruir
    // uno por defecto.

    OpcionesSalida := TArregloStrings.Create(7);
    OpcionesSalida [0] := 'informacion_modelo';
    OpcionesSalida [1] := 'dimensiones';
    OpcionesSalida [2] := 'estructura_media_solucion';
    OpcionesSalida [3] := 'efectos_aleatorios_solucion';
    OpcionesSalida [4] := 'efectos_aleatorios_estimacion';
    OpcionesSalida [5] := 'informacion_ajuste';
    OpcionesSalida [6] := 'estadisticas_iteracion';
  end;

  // Verificar si se desea generar un archivo de salida directamente.
  if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_salida') then
  begin
    EscribirArchivoSalida := True;
    NombreArchivoSalida := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
    ArchivoSalida := TFileStream.Create(NombreArchivoSalida, fmCreate	or fmOpenWrite);
  end
  else
    EscribirArchivoSalida := False;

  if (FThreadProcedimiento.EjecucionExitosa) and
     (FThreadProcedimiento.PostProcesamientoExitoso) then
  begin
    ImprimirStringRichEditSalida('Fin de ejecucion. Generando salida...');
    Invalidate;

    // Generar salida

    for IndiceOpcionSalida := OpcionesSalida.Bajo to OpcionesSalida.Alto do
    begin
      Strings.Clear;

      if OpcionesSalida [IndiceOpcionSalida] = 'informacion_modelo' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Informacion de modelo:');
        Strings.Add('');
        Strings.Add(Format('%-22s %s', ['Modelo:', Procedimiento.Modelo.Etiqueta]));
        Strings.Add(Format('%-22s %s', ['Conjunto de Datos:', Procedimiento.Modelo.Datos.Etiqueta]));
        Strings.Add(Format('%-22s %s', ['Variable Dependiente:', Procedimiento.Modelo.VariableDependiente]));

        if Procedimiento.Modelo.VariablesClasificacion.Count > 0 then
        begin
          AnchoCampo1 := 0;
          for IndiceVariable := 0 to Procedimiento.Modelo.VariablesClasificacion.Count - 1 do
          begin
            if AnchoCampo1 < Length(Procedimiento.Modelo.VariablesClasificacion [IndiceVariable]) then
              AnchoCampo1 := Length(Procedimiento.Modelo.VariablesClasificacion [IndiceVariable]);
          end;

          Strings.Add('');
          Strings.Add('Variables de Clasificacion:');
          Strings.Add('');
          Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10s %s', ['Clase', 'Niveles', 'Valores']));
          Strings.Add('------------------------------------------------------------');

          for IndiceVariable := 0 to Procedimiento.Modelo.VariablesClasificacion.Count - 1 do
            Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10d %s', [Procedimiento.Modelo.VariablesClasificacion [IndiceVariable], Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [IndiceVariable].Dimension, Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [IndiceVariable].Texto]));
        end;

        if Procedimiento.VariablesClasificacionAuxiliares.Count > 0 then
        begin
          Strings.Add('');
          Strings.Add('Variables de Clasificacion Auxiliares:');
          Strings.Add('');

          AnchoCampo1 := 0;
          for IndiceVariable := 0 to Procedimiento.VariablesClasificacionAuxiliares.Count - 1 do
          begin
            if AnchoCampo1 < Length(Procedimiento.VariablesClasificacionAuxiliares [IndiceVariable]) then
              AnchoCampo1 := Length(Procedimiento.VariablesClasificacionAuxiliares [IndiceVariable]);
          end;

          CantidadVariablesClasificacion := Procedimiento.Modelo.VariablesClasificacion.Count;

          Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10s %s', ['Clase', 'Niveles', 'Valores']));
          Strings.Add('------------------------------------------------------------');

          for IndiceVariable := 0 to Procedimiento.Modelo.VariablesClasificacion.Count - 1 do
            Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10d %s', [Procedimiento.VariablesClasificacionAuxiliares [IndiceVariable], Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [CantidadVariablesClasificacion + IndiceVariable].Dimension, Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [CantidadVariablesClasificacion + IndiceVariable].Texto]));
        end;
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'dimensiones' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Dimensiones:');
        Strings.Add('');
        Strings.Add(Format('%-30s %4d', ['Parametros de Covarianza:', Procedimiento.CantidadParametrosCovarianza]));
        Strings.Add(Format('%-30s %4d', ['Cantidad Columnas X:', Procedimiento.X.CantidadColumnas]));
        if Procedimiento.Modelo.TieneEfectosAleatorios or Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
          CantidadColumnasZ := Procedimiento.Z.CantidadColumnas
        else
          CantidadColumnasZ := 0;
        Strings.Add(Format('%-30s %4d', ['Cantidad Columnas Z:', CantidadColumnasZ]));
        Strings.Add(Format('%-30s %4d', ['Cantidad Observaciones:', Procedimiento.CantidadObservaciones]));

        if (Procedimiento.Modelo.TieneEfectosAleatorios or Procedimiento.Modelo.TieneEfectosAleatoriosGrupos) then
        begin
          Strings.Add('Dimensiones por grupo de efectos aleatorios:');

          for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
          begin
            Strings.Add(Format('%-s %d:', ['  Dimensiones Grupo ', IndiceGrupo]));
            Strings.Add(Format('%-45s %4d', ['    Cantidad Parametros:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Dimension]));
            Strings.Add(Format('%-45s %4d', ['    Cant. Columnas de Z del Grupo:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].CantidadColumnasMatrizDiseno]));
            Strings.Add(Format('%-45s', ['    Cant. Columnas de Z por Efecto:']));

            for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
            begin
              if not (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto) then
                Strings.Add(Format('%-s: %d (%d columnas nulas)', ['      ' + Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNivelesInexistentes]));
            end;

            if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
            begin
              Strings.Add(Format('%-50s %4d', ['    Unid. Experimentales:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles - Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionUnidadExperimental.CantidadNivelesInexistentes]));
              Strings.Add(Format('%-50s %4d', ['    Cant. Max. Observ. por Unid. Experimental:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones]));
            end;
          end;
        end;

        if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
        begin
          Strings.Add('Dimensiones del vector error:');
          Strings.Add(Format('%-45s %4d', ['  Unid. Experimentales:', Procedimiento.InformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles - Procedimiento.InformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNivelesInexistentes]));
          Strings.Add(Format('%-45s %4d', ['  Cant. Max. Observ. por Unid. Experimental:', Procedimiento.InformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones]));
        end;
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'estructura_media_solucion' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Solucion para la estructura de media:');
        Strings.Add('');

        // Calcular la longitud maxima del nombre de un efecto
        AnchoCampo1 := 0;
        for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
        begin
          if  AnchoCampo1 < Length(Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto) then
             AnchoCampo1 := Length(Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto);
        end;

        // Verificar si el nombre de efecto mas largo es menor que "efecto".
        if AnchoCampo1 < 6 then
          AnchoCampo1 := 6;

        // Calcular la longitud maxima del identificador de un nivel, y buscar
        // la cantidad maxima de variables de clasificacion en un efecto fijo.
        AnchoCampo2 := 0;
        CantidadVariablesClasificacion := 0;
        for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
        begin
          if (not (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
             (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
          begin
            if CantidadVariablesClasificacion < Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension then
              CantidadVariablesClasificacion := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension;

            for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Alto do
            begin
              for IndiceNivel := Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Bajo to Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Alto do
              begin
                if AnchoCampo2 < Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]) then
                  AnchoCampo2 := Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]);
              end;
            end;
          end;
        end;

        // Verificar si la lista mas larga de niveles de las variables de
        // clasificacion es menor que "Niveles".
        if AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1) < 8 then
          AnchoCampo3 := 8
        else
          AnchoCampo3 := AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1);

        Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) + 's %-15s %-15s %-15s', ['Efecto', 'Niveles', 'Estimador', 'Error Estandar', 'Valor-t']));
        Strings.Add('----------------------------------------------------------------------------------------------------');
        Strings.Add('');

        IndiceEstimador := 1;
        for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
        begin
          String2 := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto;

          if (not (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
             (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
          begin

            for IndiceNivel := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
            begin
              String1 := '';

              for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto - 1 do
                String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's ', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable], Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, IndiceVariable]]]);
              String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto], Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto]]]);

              if Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [IndiceEstimador] = 0.0 then
                Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15s %-15s %-15s', [String2, String1, '0.0', '-', '-']))
              else
                Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f %-15.4f', [String2, String1, Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ValorTObservadoEstimadoresParametrosEfectosFijos [IndiceEstimador]]));

              Inc(IndiceEstimador);
            end;
          end
          else
          begin
            Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f %-15.4f', [String2, '', Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ValorTObservadoEstimadoresParametrosEfectosFijos [IndiceEstimador]]));
            Inc(IndiceEstimador);
          end;
        end;
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'efectos_aleatorios_estimacion' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Estimadores de parametros de covarianza:');
        Strings.Add('');

        // Calcular la longitudes maximas de nombres
        AnchoCampo1 := 0;
        AnchoCampo2 := 0;
        TieneUnidadExperimental := False;
        if Procedimiento.Modelo.TieneEfectosAleatorios or
           Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
        begin
          for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
          begin
            if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
            begin
              TieneUnidadExperimental := True;
              if AnchoCampo2 < Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto) then
                AnchoCampo2 := Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto);
            end;

            for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
            begin
              if AnchoCampo1 < Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro]) then
                AnchoCampo1 := Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro]);
            end;
          end;
        end;

        AnchoCampo3 := 0;
        AnchoCampo4 := 0;
        if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
        begin
          if AnchoCampo4 < Length(Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto) then
            AnchoCampo4 := Length(Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto);
        end;
        for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
        begin
          if AnchoCampo3 < Length(Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro]) then
            AnchoCampo3 := Length(Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro]);
        end;

        // Verificar si el ancho del segundo campo es menor que la longitud de
        // "Unidad Experimental"
        if AnchoCampo2 < Length('Unidad Experimental') then
          AnchoCampo2 := Length('Unidad Experimental');
        if AnchoCampo4 < Length('Unidad Experimental') then
          AnchoCampo4 := Length('Unidad Experimental');

        // Verificar si el hessiano es positivo semi-definido, de no serlo, no
        // es posible calcular el error estandar de los estimadores de
        // parametros de covarianza.
        if Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.HessianoPositivoDefinido then
        begin
          // El hessiano es positivo semidefinido

          IndiceEstimador := 1;

          // Parametros de efectos aleatorios

          if Procedimiento.Modelo.TieneEfectosAleatorios or
             Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
          begin
            Strings.Add('Parametros de efectos aleatorios:');
            Strings.Add('');

            // Verificar si algun grupo de efectos aleatorios define una unidad
            // experimental
            if TieneUnidadExperimental then
            begin
              Strings.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
              Strings.Add('----------------------------------------------------------------------------------------------------');

              for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
              begin
                if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
                  String1 := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto
                else
                  String1 := '';

                for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                begin
                  Strings.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20.4f %-20.4f', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
                  Inc(IndiceEstimador);
                end;
              end;
            end
            else
            begin
              Strings.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
              Strings.Add('----------------------------------------------------------------------------------------------------');

              for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
              begin
                for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                begin
                  Strings.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-20.4f %-20.4f %-20.4f', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
                  Inc(IndiceEstimador);
                end;
              end;
            end;

            Strings.Add('');
          end;

          Strings.Add('Parametros del error:');
          Strings.Add('');

          // Verificar si se definio unidad experimental para el error
          if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
          begin
            Strings.Add(Format('%-' + IntToStr(AnchoCampo3) + 's %-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
            Strings.Add('----------------------------------------------------------------------------------------------------');

            String1 := Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto;

            for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
            begin
              Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20.4f %-20.4f', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
              Inc(IndiceEstimador);
            end;
          end
          else
          begin
            Strings.Add(Format('%-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
            Strings.Add('----------------------------------------------------------------------------------------------------');

            for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
            begin
              Strings.Add(Format('%-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20.4f %-20.4f', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
              Inc(IndiceEstimador);
            end;
          end;
        end
        else
        begin
          // El hessiano no es positivo semidefinido

          IndiceEstimador := 1;

          // Parametros de efectos aleatorios

          if Procedimiento.Modelo.TieneEfectosAleatorios or
             Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
          begin
            Strings.Add('Parametros de efectos aleatorios:');
            Strings.Add('');

            // Verificar si algun grupo de efectos aleatorios define una unidad
            // experimental
            if TieneUnidadExperimental then
            begin
              Strings.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
              Strings.Add('----------------------------------------------------------------------------------------------------');

              for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
              begin
                if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
                  String1 := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto
                else
                  String1 := '';

                for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                begin
                  Strings.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20s %-20s', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
                  Inc(IndiceEstimador);
                end;
              end;
            end
            else
            begin
              Strings.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
              Strings.Add('----------------------------------------------------------------------------------------------------');

              for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
              begin
                for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                begin
                  Strings.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-20.4f %-20s %-20s', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
                  Inc(IndiceEstimador);
                end;
              end;
            end;

            Strings.Add('');
          end;

          Strings.Add('Parametros del error:');
          Strings.Add('');

          // Verificar si se definio unidad experimental para el error
          if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
          begin
            Strings.Add(Format('%-' + IntToStr(AnchoCampo3) + 's %-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
            Strings.Add('----------------------------------------------------------------------------------------------------');

            String1 := Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto;

            for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
            begin
              Strings.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20s %-20s', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
              Inc(IndiceEstimador);
            end;
          end
          else
          begin
            Strings.Add(Format('%-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
            Strings.Add('----------------------------------------------------------------------------------------------------');

            for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
            begin
              Strings.Add(Format('%-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20s %-20s', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
              Inc(IndiceEstimador);
            end;
          end;
        end
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'efectos_aleatorios_solucion' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Solucion para los efectos aleatorios:');
        Strings.Add('');

        // Generar la solucion para la estructura de covarianza si el modelo
        // posee efectos aleatorios.
        if Procedimiento.Modelo.TieneEfectosAleatorios or
           Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
        begin
          // Calcular la longitud maxima del nombre de un efecto
          AnchoCampo1 := 0;
          for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
          begin
            for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
            begin
              if  AnchoCampo1 < Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto) then
                 AnchoCampo1 := Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto);
            end;
          end;

          // Verificar si el nombre de efecto mas largo es menor que "efecto".
          if AnchoCampo1 < 6 then
            AnchoCampo1 := 6;

          // Calcular la longitud maxima del identificador de un nivel, y buscar
          // la cantidad maxima de variables de clasificacion en un efecto
          // aleatorio.
          AnchoCampo2 := 0;
          CantidadVariablesClasificacion := 0;
          for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
          begin
            for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
            begin
              if (not (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
                 (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
              begin
                if CantidadVariablesClasificacion < Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension then
                  CantidadVariablesClasificacion := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension;

                for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Alto do
                begin
                  for IndiceNivel := Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Bajo to Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Alto do
                  begin
                    if AnchoCampo2 < Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]) then
                      AnchoCampo2 := Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]);
                  end;
                end;
              end;
            end;
          end;

          // Verificar si la lista mas larga de niveles de las variables de
          // clasificacion es menor que "Niveles".
          if AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1) < 8 then
            AnchoCampo3 := 8
          else
            AnchoCampo3 := AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1);

          Strings.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) + 's %-15s %-15s', ['Grupo', 'Efecto', 'Niveles', 'Estimador', 'Error Estandar']));
          Strings.Add('----------------------------------------------------------------------------------------------------');

          IndiceEstimador := 1;
          for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
          begin
            for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
            begin
              String2 := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto;

              if (not (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
                 (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
              begin
                for IndiceNivel := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
                begin
                  String1 := '';

                  for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto - 1 do
                    String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's ', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable], Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, IndiceVariable]]]);
                  String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto], Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto]]]);

                  Strings.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f', [IndiceGrupo, String2, String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.PredictoresEfectosAleatorios [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios [IndiceEstimador]]));
                  Inc(IndiceEstimador);
                end;
              end
              else
              begin
                Strings.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f', [IndiceGrupo, String2, '', Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.PredictoresEfectosAleatorios [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios [IndiceEstimador]]));
                Inc(IndiceEstimador);
              end;
            end;
          end;
        end;
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'bitacora_iteracion' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Bitacora de iteracion:');
        Strings.Add('');
        Strings.AddStrings(FThreadProcedimiento.BitacoraIteracion);
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'informacion_ajuste' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Informacion de ajuste:');
        Strings.Add('');
        Strings.Add(Format('%-45s %.2f', ['Verosimilitud Logaritmica:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmica]));
        Strings.Add(Format('%-45s %.2f', ['-2.0 * Verosimilitud Logaritmica:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmica]));
        Strings.Add(Format('%-45s %.2f', ['Verosimilitud Logaritmica Residual:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmicaResidual]));
        Strings.Add(Format('%-45s %.2f', ['-2.0 * Verosimilitud Logaritmica Residual:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmicaResidual]));
        Strings.Add(Format('%-45s %.2f', ['AIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.AIC]));
        Strings.Add(Format('%-45s %.2f', ['AICC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.AICC]));
        Strings.Add(Format('%-45s %.2f', ['HQIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.HQIC]));
        Strings.Add(Format('%-45s %.2f', ['BIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.BIC]));
        Strings.Add(Format('%-45s %.2f', ['CAIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.CAIC]));
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_x' then
        SalidaVerMatriz('Salida: X', Procedimiento.X)
      else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_z' then
      begin
        if Procedimiento.Modelo.TieneEfectosAleatorios or
           Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
          SalidaVerMatriz('Salida: Z', Procedimiento.Z);
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_g' then
      begin
        if Procedimiento.Modelo.TieneEfectosAleatorios or
          Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
        SalidaVerMatriz('Salida: G', Procedimiento.G);
      end
      else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_r' then
        SalidaVerMatriz('Salida: R', Procedimiento.R)
      else if OpcionesSalida [IndiceOpcionSalida] = 'estadisticas_iteracion' then
      begin
        Strings.Add('');
        Strings.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Estadisticas de iteracion:');
        Strings.Add('');
        Strings.Add(Format('%-50s %8d', ['Cantidad de Iteraciones:', Procedimiento.CantidadIteraciones]));
        Strings.Add(Format('%-50s %8d', ['Cantidad de Iteraciones de Bucle Interno:', Procedimiento.CantidadIteracionesBucleInterno]));
        Strings.Add(Format('%-50s %8d', ['Cantidad de Evaluaciones de la Funcion Objetivo:', Procedimiento.CantidadEvaluacionesFuncionObjetivo]));
        Strings.Add(Format('%-50s %8s', ['Duracion de Etapa de Inicializacion:', FormatDateTime('hh:mm:ss', FThreadProcedimiento.DuracionInicializacion)]));
        Strings.Add(Format('%-50s %8s', ['Duracion de Etapa de Iteracion:', FormatDateTime('hh:mm:ss', FThreadProcedimiento.DuracionIteracion)]));
      end;

      ImprimirStringsRichEditSalida(Strings);
      if EscribirArchivoSalida then
        Strings.SaveToStream(ArchivoSalida);
    end;

    ImprimirStringRichEditSalida('Ejecucion exitosa.');
  end
  else
  begin
    ImprimirStringRichEditSalida('Bitacora de ejecucion:');
    ImprimirStringsConVinetasRichEditSalida(FThreadProcedimiento.BitacoraEjecucion);
    ImprimirStringRichEditSalida('Bitacora de iteracion:');
    ImprimirStringsRichEditSalida(FThreadProcedimiento.BitacoraIteracion);
    ImprimirStringErrorRichEditSalida('Ejecucion fallida.');

    if EscribirArchivoSalida then
    begin
      FThreadProcedimiento.BitacoraEjecucion.SaveToStream(ArchivoSalida);
      FThreadProcedimiento.BitacoraIteracion.SaveToStream(ArchivoSalida);
    end;
  end;

  FreeAndNil(Strings);
  if EscribirArchivoSalida then
    FreeAndNil(ArchivoSalida);

  ActionEstadisticasAnalisisVarianza.Enabled := True;
end { TFormPrincipal.DoFinEjecucionThreadProcedimiento };

procedure TFormPrincipal.DoFinInicializacionThreadProcedimiento;
var
  I: Integer;
  Procedimiento: TProcedimiento;
  EscribirArchivoSalida: Boolean;
  NombreArchivoSalida: TFileName;
  ArchivoSalida: TFileStream;
begin { TFormPrincipal.DoFinInicializacionThreadProcedimiento }
  DeshabilitarProgressBarPrincipal;

  Procedimiento := FThreadProcedimiento.Procedimiento;

  if FThreadProcedimiento.InicializacionExitosa then
  begin
    if FThreadProcedimiento.BitacoraInicializacion.Count > 0 then
    begin
      ImprimirStringAdvertenciaRichEditSalida('Advertencias generadas durante la inicializacion del procedimiento:');
      ImprimirStringsConVinetasRichEditSalida(FThreadProcedimiento.BitacoraInicializacion);
    end;

    ImprimirStringRichEditSalida('Inicializaci�n exitosa. Continua el resto del procedimiento...');
    Invalidate;
    HabilitarProgressBarPrincipal;
  end
  else
  begin
    // Verificar si se desea generar un archivo de salida directamente.
    if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_salida') then
    begin
      EscribirArchivoSalida := True;
      NombreArchivoSalida := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
      ArchivoSalida := TFileStream.Create(NombreArchivoSalida, fmCreate	or fmOpenWrite);
    end
    else
      EscribirArchivoSalida := False;

    ImprimirStringErrorRichEditSalida('Se produjeron errores durante la inicializacion del procedimiento:');
    ImprimirStringsConVinetasRichEditSalida(FThreadProcedimiento.BitacoraInicializacion);
    if EscribirArchivoSalida then
      FThreadProcedimiento.BitacoraInicializacion.SaveToStream(ArchivoSalida);
    ImprimirStringErrorRichEditSalida('Inicializacion fallida.');
    ActionEstadisticasAnalisisVarianza.Enabled := True;

    FreeAndNil(ArchivoSalida);
  end;
end { TFormPrincipal.DoFinInicializacionThreadProcedimiento };

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin { TFormPrincipal.FormCreate }
  FOnFinInicializacionThreadProcedimiento := DoFinInicializacionThreadProcedimiento;
  FOnFinEjecucionThreadProcedimiento := DoFinEjecucionThreadProcedimiento;
  ProgressBarPrincipal.Step := (ProgressBarPrincipal.Max - ProgressBarPrincipal.Min) div 4;
end { TFormPrincipal.FormCreate };

procedure TFormPrincipal.SalidaVerMatriz(const AString: String; const Matriz: TUaMatriz);
var
  I, J: Integer;
  TabSheet: TTabSheet;
  StringGrid: TStringGrid;
begin { TFormPrincipal.SalidaVerMatriz }
  // Construir e inicializar el TabSheet
  TabSheet := TTabSheet.Create(PageControlPrincipal);
  TabSheet.PageControl := PageControlPrincipal;
  TabSheet.Caption := AString;

  // Contruir e inicializar el StringGrid
  StringGrid := TStringGrid.Create(TabSheet);
  StringGrid.Parent := TabSheet;
  StringGrid.Align := alClient;

  // Mostrar el contenido de la matriz
  StringGrid.ColCount := Matriz.CantidadColumnas + 1;
  StringGrid.RowCount := Matriz.CantidadFilas + 1;

  for I := 1 to StringGrid.ColCount do
    StringGrid.Cells [I, 0] := IntToStr(I);

  for I := 1 to StringGrid.RowCount do
    StringGrid.Cells [0, I] := IntToStr(I);

  for I := 1 to Matriz.CantidadFilas do
    for J := 1 to Matriz.CantidadColumnas do
      StringGrid.Cells [J, I] := FloatToStr(Matriz [I, J]);
end { TFormPrincipal.SalidaVerMatriz };

procedure TFormPrincipal.SalidaVerVector(const AString: String; const Vector: TUaVector);
var
  I: Integer;
  TabSheet: TTabSheet;
  StringGrid: TStringGrid;
begin { TFormPrincipal.SalidaVerVector }
  // Construir e inicializar el TabSheet
  TabSheet := TTabSheet.Create(PageControlPrincipal);
  TabSheet.PageControl := PageControlPrincipal;
  TabSheet.Caption := AString;

  // Contruir e inicializar el StringGrid
  StringGrid := TStringGrid.Create(TabSheet);
  StringGrid.Parent := TabSheet;
  StringGrid.Align := alClient;

  // Mostrar el contenido de la matriz
  StringGrid.ColCount := 2;
  StringGrid.RowCount := Vector.Dimension + 1;

  StringGrid.Cells [0, 0] := 'Elemento';
  StringGrid.Cells [1, 0] := 'Valor';

  for I := 1 to Vector.Dimension do
    StringGrid.Cells [1, I] := FloatToStr(Vector [I]);
end { TFormPrincipal.SalidaVerVector };

end { UnitFormPrincipal }.
