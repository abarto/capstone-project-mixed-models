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
  UnitConfiguracion, UnitOpcion, UnitProcedimiento, UaMatriz, UaVector;

type
  {** TForm principal de la aplicacion.
      @abstract(TForm principal.) }
  TFormPrincipal = class(TForm)
    PageControlConfiguracion: TPageControl;
    TabSheetFuente: TTabSheet;
    RichEditFuente: TRichEdit;
    ToolBarFuente: TToolBar;
    ToolButtonFuenteParsear: TToolButton;
    ToolButtonFuenteGuardar: TToolButton;
    ToolButtonFuenteGuardarComo: TToolButton;
    ToolButtonSeparador3: TToolButton;
    CheckBoxFuenteAjustarLineas: TCheckBox;
    TabSheetSalida: TTabSheet;
    RichEditSalida: TRichEdit;
    ToolBarSalida: TToolBar;
    ToolButtonSalidaLimpiar: TToolButton;
    ToolButtonSalidaGuardarComo: TToolButton;
    ToolButtonSeparador4: TToolButton;
    CheckBoxSalidaAjustarLineas: TCheckBox;
    MenuItemArchivoGuardarConfiguracion: TMenuItem;
    MenuItemArchivoGuardarConfiguracionComo: TMenuItem;
    ToolButton3: TToolButton;
    ActionModeloSimular: TAction;
    ToolButtonSeparador5: TToolButton;
    LabelFuenteEstadoConfiguracion: TLabel;
    MenuItemModelo: TMenuItem;
    MenuItemModeloSimular: TMenuItem;

    ActionArchivoAbrirConfiguracion: TAction;
    ActionArchivoSalir: TAction;
    ActionFuenteAjustarLineas: TAction;
    ActionFuenteGuardar: TAction;
    ActionFuenteGuardarComo: TAction;
    ActionFuenteParsear: TAction;
    ActionSalidaAjustarLineas: TAction;
    ActionSalidaGuardarComo: TAction;
    ActionSalidaLimpiar: TAction;

    ActionListPrincipal: TActionList;

    ImageListPrincipal: TImageList;

    MainMenuPrincipal: TMainMenu;

    MenuItemArchivo: TMenuItem;
    MenuItemArchivoAbrirConfiguracion: TMenuItem;
    MenuItemArchivoSalir: TMenuItem;
    MenuItemAyuda: TMenuItem;
    N1: TMenuItem;

    OpenDialogPrincipal: TOpenDialog;
    SaveDialogPrincipal: TSaveDialog;
    ToolBarPrincipal: TToolBar;
    ToolButtonArchivoAbrirConfiguracion: TToolButton;
    ToolButtonArchivoSalir: TToolButton;
    ToolButtonSeparador1: TToolButton;
    StatusBarPrincipal: TStatusBar;

    procedure ActionArchivoAbrirConfiguracionExecute(Sender: TObject);
    procedure ActionArchivoSalirExecute(Sender: TObject);
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
    procedure ActionModeloSimularUpdate(Sender: TObject);
    procedure ActionModeloSimularExecute(Sender: TObject);
    procedure SalidaVerMatriz(const AString: String; const Matriz: TUaMatriz);
  private
    FConfiguracion: TConfiguracion;
    FNombreArchivoConfiguracionActual: TFileName;

    procedure ImprimirStringAdvertenciaRichEditSalida(AString: String);
    procedure ImprimirStringErrorRichEditSalida(AString: String);
    procedure ImprimirStringRichEditSalida(AString: String);
    procedure ImprimirStringSinFormatoRichEditSalida(AString: String);
    procedure ImprimirStringsRichEditSalida(Strings: TStrings);
    procedure ImprimirStringsConVinetasRichEditSalida(Strings: TStrings);
    procedure ImprimirLabelFuenteEstadoConfiguracion(const Mensaje: String; const Color: TColor = clDefault);
    procedure LimpiarLabelFuenteEstadoConfiguracion;
    procedure IrAlFinalRichEditSalida;
  end { TFormPrincipal };

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------
// TFormPrincipal
// -----------------------------------------------------------------------------

procedure TFormPrincipal.ActionArchivoAbrirConfiguracionExecute(Sender: TObject);
var
  Configuracion: TConfiguracion;
  NombreArchivoConfiguracionAnterior: TFileName;
  Stream: TStream;
  Strings, BitacoraInicializacion, BitacoraValidacionDatos, BitacoraValidacionModelo: TStrings;
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

          if Configuracion.Validar(BitacoraValidacionDatos, BitacoraValidacionModelo) then
          begin
            // Configuracion valida

            // Liberar configuracion anterior y asignar la nueva
            FreeAndNil(FConfiguracion);
            FConfiguracion := Configuracion;

            // Actualizar componentes visuales
            ImprimirLabelFuenteEstadoConfiguracion('Configuracion Exitosa', clGreen);

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

            ImprimirStringRichEditSalida('Configuracion exitosa.');
          end
          else
          begin
            // Configuracion invalida, imprimir los errores

            ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

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

            ImprimirStringErrorRichEditSalida('Configuracion fallida.');

            // Liberar la configuracion parseada
            FreeAndNil(Configuracion);
          end;

          // Liberar bitacoras
          FreeAndNil(BitacoraValidacionDatos);
          FreeAndNil(BitacoraValidacionModelo);
        end
        else
        begin
          // Se produjeron errores durante la inicializacion

          ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

          ImprimirStringErrorRichEditSalida('Errores generados durante la inicializacion del modelo:');
          ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
          ImprimirStringErrorRichEditSalida('Configuracion fallida.');
        end;

        // Liberar bitacoras
        FreeAndNil(BitacoraInicializacion);
      end
      else
      begin
        ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

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

      ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

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
      FreeAndNil(Configuracion);
    end;
  end;
end { TFormPrincipal.ActionArchivoAbrirConfiguracionExecute };

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
      LimpiarLabelFuenteEstadoConfiguracion;
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
  // ActionFuenteGuardar.Enabled := RichEditFuente.Modified;
  if RichEditFuente.Modified then
  begin
    ActionFuenteGuardar.Enabled := True;
    ImprimirLabelFuenteEstadoConfiguracion('Configuracion Modificada', clBlue);
  end;
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
      LimpiarLabelFuenteEstadoConfiguracion;
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
  Strings, BitacoraInicializacion, BitacoraValidacionDatos, BitacoraValidacionModelo: TStrings;
begin { TFormPrincipal.ActionFuenteParsearExecute }
  try
    RichEditFuente.Modified := False;

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

        if Configuracion.Validar(BitacoraValidacionDatos, BitacoraValidacionModelo) then
        begin
          // Configuracion valida

          // Asignar el nombre de archivo de la configuracion anterior a la nueva
          Configuracion.NombreArchivoConfiguracion := FNombreArchivoConfiguracionActual;

          // Liberar la configuracion actual y asignar la nueva
          FreeAndNil(FConfiguracion);
          FConfiguracion := Configuracion;

          // Actualizar interfaz
          ImprimirLabelFuenteEstadoConfiguracion('Configuracion Exitosa', clGreen);

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

          ImprimirStringErrorRichEditSalida('Configuracion fallida.');
          ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

          // Liberar configuracion invalida parseada
          FreeAndNil(Configuracion);
        end;

        // Liberar bitacoras
        FreeAndNil(BitacoraValidacionDatos);
        FreeAndNil(BitacoraValidacionModelo);
      end
      else
      begin
        // Se produjeron errores durante la inicializacion

        ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

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

      ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

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

      ImprimirLabelFuenteEstadoConfiguracion('Configuracion Fallida', clRed);

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
      FreeAndNil(Configuracion);
    end;
  end;
end { TFormPrincipal.ActionFuenteParsearExecute };

procedure TFormPrincipal.ActionSalidaLimpiarExecute(Sender: TObject);
begin { TFormPrincipal.ActionSalidaLimpiarExecute }
  RichEditSalida.Clear;
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

procedure TFormPrincipal.ImprimirLabelFuenteEstadoConfiguracion(const Mensaje: String; const Color: TColor = clDefault);
begin { TFormPrincipal.ImprimirLabelFuenteEstadoConfiguracion }
  LabelFuenteEstadoConfiguracion.Font.Color := Color;
  LabelFuenteEstadoConfiguracion.Caption := Mensaje;
end { TFormPrincipal.ImprimirLabelFuenteEstadoConfiguracion };

procedure TFormPrincipal.LimpiarLabelFuenteEstadoConfiguracion;
begin { TFormPrincipal.LimpiarLabelFuenteEstadoConfiguracion }
  LabelFuenteEstadoConfiguracion.Font.Color := clDefault;
  LabelFuenteEstadoConfiguracion.Caption := '';
end { TFormPrincipal.LimpiarLabelFuenteEstadoConfiguracion };

procedure TFormPrincipal.ActionModeloSimularUpdate(Sender: TObject);
begin { TFormPrincipal.ActionModeloSimularUpdate }
  // Perimitir la simulacion solo si la configuracion existe y fue validada
  ActionModeloSimular.Enabled := Assigned(FConfiguracion) and FConfiguracion.Valida
end { TFormPrincipal.ActionModeloSimularUpdate };

procedure TFormPrincipal.ActionModeloSimularExecute(Sender: TObject);
var
  I, IndiceEjecucion, CantidadEjecuciones: Integer;
  Continuar, EscribirResumenVerosimilitud, EscribirResumenBeta, EscribirResumenTheta: Boolean;
  Procedimiento: TProcedimiento;
  BitacoraInicializacion, BitacoraEjecucion: TStrings;
  PrefijoArchivoSalida, NombreArchivoResumenVerosimilitud, NombreArchivoResumenBeta, NombreArchivoResumenTheta: TFileName;
  ArchivoResumenVerosimilitud, ArchivoResumenBeta, ArchivoResumenTheta: TextFile;
  VerosimilitudEjecucion, VerosimilitudResidualEjecucion: TUaVector;
  String1: String;
begin { TFormPrincipal.ActionModeloSimularExecute }
  BitacoraInicializacion := TStringList.Create;
  BitacoraEjecucion := TStringList.Create;

  // Inicializar el procedmiento
  ImprimirStringRichEditSalida('Instanciando procedimiento...');
  Procedimiento := TProcedimiento.Create(FConfiguracion);
  ImprimirStringRichEditSalida('Inicializando procedimiento...');
  ActionModeloSimular.Enabled := False;

  // Inicializar cantidad de ejecuciones
  if FConfiguracion.Procedimiento.Opciones.BuscarAsignar(I, 'cantidad_ejecuciones') then
  begin
    CantidadEjecuciones := (FConfiguracion.Procedimiento.Opciones [I] as TOpcionParametroInteger).Parametro;
    if CantidadEjecuciones <= 0 then
      CantidadEjecuciones := 1;
  end
  else
    CantidadEjecuciones := 1;

  // Inicializar prefijos de nombres de archivos de salida
  if FConfiguracion.Procedimiento.Opciones.BuscarAsignar(I, 'archivo_salida') then
    PrefijoArchivoSalida := (FConfiguracion.Procedimiento.Opciones [I] as TOpcionParametroString).Parametro
  else
    PrefijoArchivoSalida := 'ValoresSimulados' + FloatToStr(Now) + '-';

  // Inicializar los archivos de resumen

  if FConfiguracion.Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_verosimilitud') then
  begin
    NombreArchivoResumenVerosimilitud := (FConfiguracion.Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
    EscribirResumenVerosimilitud := True;
    AssignFile(ArchivoResumenVerosimilitud, NombreArchivoResumenVerosimilitud);
    if FileExists(NombreArchivoResumenVerosimilitud) then
      Append(ArchivoResumenVerosimilitud)
    else
      ReWrite(ArchivoResumenVerosimilitud);
    VerosimilitudEjecucion := TUaVector.Create(CantidadEjecuciones);
    VerosimilitudResidualEjecucion := TUaVector.Create(CantidadEjecuciones);
  end
  else
  begin
    NombreArchivoResumenVerosimilitud := '';
    EscribirResumenVerosimilitud := False;
    VerosimilitudEjecucion := nil;
    VerosimilitudResidualEjecucion := nil;
  end;

  if FConfiguracion.Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_beta') then
  begin
    NombreArchivoResumenBeta := (FConfiguracion.Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
    EscribirResumenBeta := True;
    AssignFile(ArchivoResumenBeta, NombreArchivoResumenBeta);
    if FileExists(NombreArchivoResumenBeta) then
      Append(ArchivoResumenBeta)
    else
      ReWrite(ArchivoResumenBeta);
  end
  else
  begin
    NombreArchivoResumenBeta := '';
    EscribirResumenBeta := False;
  end;

  if FConfiguracion.Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_theta') then
  begin
    NombreArchivoResumenTheta := (FConfiguracion.Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
    EscribirResumenTheta := True;
    AssignFile(ArchivoResumenTheta, NombreArchivoResumenTheta);
    if FileExists(NombreArchivoResumenTheta) then
      Append(ArchivoResumenTheta)
    else
      ReWrite(ArchivoResumenTheta);
  end
  else
  begin
    NombreArchivoResumenTheta := '';
    EscribirResumenTheta := False;
  end;

  // Iterar
  Continuar := True;
  IndiceEjecucion := 1;
  while (IndiceEjecucion <= CantidadEjecuciones) and Continuar do
  begin
    // Generar nuevo nombre de archvio de salida
    Procedimiento.ArchivoSalida := PrefijoArchivoSalida + IntToStr(IndiceEjecucion) + '.txt';

    BitacoraInicializacion.Clear;
    BitacoraEjecucion.Clear;

    if Procedimiento.Inicializar(BitacoraInicializacion) then
    begin
      // Inicializacion exitosa

      if (BitacoraInicializacion.Count > 0) then
      begin
        ImprimirStringRichEditSalida('Advertencias generadas durante la inicializacion del procedimiento:');
        ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
      end;
      ImprimirStringRichEditSalida('Inicializacion exitosa.');

      if Procedimiento.Ejecutar(BitacoraEjecucion) then
      begin
        // Ejecucion exitosa

        if (BitacoraEjecucion.Count > 0) then
        begin
          ImprimirStringRichEditSalida('Advertencias generadas durante la inicializacion del procedimiento:');
          ImprimirStringsConVinetasRichEditSalida(BitacoraEjecucion);
        end;
        ImprimirStringRichEditSalida('Salida generada:');
        ImprimirStringsRichEditSalida(Procedimiento.Salida);
        ImprimirStringRichEditSalida('Ejecucion exitosa.');

        // Grabar salida

        try
          Procedimiento.Salida.SaveToFile(Procedimiento.ArchivoSalida);
          ImprimirStringRichEditSalida('Salida guardada exitosamente en ' + Procedimiento.ArchivoSalida + '.');
        except
          on E: Exception do
          begin
            ImprimirStringErrorRichEditSalida('Se produjo un error al guardar los datos generados: ' + E.Message);
            ImprimirStringErrorRichEditSalida('Ejecucion fallida.');
          end;
        end;

        // Almacenar datos de resumen
        if EscribirResumenVerosimilitud then
        begin
          VerosimilitudEjecucion [IndiceEjecucion] := Procedimiento.VerosimilitudLogaritmica;
          VerosimilitudResidualEjecucion [IndiceEjecucion] := Procedimiento.VerosimilitudLogaritmicaResidual;
        end;
      end
      else
      begin
        // Ejecucion fallida

        ImprimirStringErrorRichEditSalida('Se generaron errores durante la ejecucion del procedimiento:');
        ImprimirStringsConVinetasRichEditSalida(BitacoraEjecucion);
        ImprimirStringErrorRichEditSalida('Ejecucion fallida.');
        Continuar := False;
      end;
    end
    else
    begin
      // Inicializacion fallida

      ImprimirStringErrorRichEditSalida('Se generaron errores durante la inicializacion del procedimiento:');
      ImprimirStringsConVinetasRichEditSalida(BitacoraInicializacion);
      ImprimirStringErrorRichEditSalida('Inicializacion fallida.');
      Continuar := False;
    end;

    Inc(IndiceEjecucion);
  end;

  // Grabar valores de resumenes

  if EscribirResumenVerosimilitud then
  begin
    String1 := '';

    for I := 1 to CantidadEjecuciones do
    begin
      String1 := FloatToStr(VerosimilitudEjecucion [I]) + ' ' + FloatToStr(VerosimilitudResidualEjecucion [I]);
      WriteLn(ArchivoResumenVerosimilitud, String1);
    end;

    Flush(ArchivoResumenVerosimilitud);
    CloseFile(ArchivoResumenVerosimilitud);

    FreeAndNil(VerosimilitudEjecucion);
    FreeAndNil(VerosimilitudResidualEjecucion);
  end;

  if EscribirResumenBeta then
  begin
    String1 := '';
    for I := 1 to Procedimiento.Beta.CantidadFilas do
      String1 := String1 + FloatToStr(Procedimiento.XBeta [I, 1]) + ' ';

    for I := 1 to CantidadEjecuciones do
      WriteLn(ArchivoResumenBeta, String1);

    Flush(ArchivoResumenBeta);
    CloseFile(ArchivoResumenBeta);
  end;

  if EscribirResumenTheta then
  begin
    String1 := '';
    for I := 1 to Procedimiento.ValoresParametrosEfectosAleatorios.Dimension do
      String1 := String1 + FloatToStr(Procedimiento.ValoresParametrosEfectosAleatorios [I]) + ' ';

    for I := 1 to CantidadEjecuciones do
      WriteLn(ArchivoResumenTheta, String1);

    Flush(ArchivoResumenTheta);
    CloseFile(ArchivoResumenTheta);
  end;

  ActionModeloSimular.Enabled := True;
  FreeAndNil(Procedimiento);
  FreeAndNil(BitacoraInicializacion);
  FreeAndNil(BitacoraEjecucion);
end { TFormPrincipal.ActionModeloSimularExecute };

procedure TFormPrincipal.SalidaVerMatriz(const AString: String; const Matriz: TUaMatriz);
var
  I, J: Integer;
  TabSheet: TTabSheet;
  StringGrid: TStringGrid;
begin { TFormPrincipal.SalidaVerMatriz }
  // Construir e inicializar el TabSheet
  TabSheet := TTabSheet.Create(PageControlConfiguracion);
  TabSheet.PageControl := PageControlConfiguracion;
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

end { UnitFormPrincipal }.

