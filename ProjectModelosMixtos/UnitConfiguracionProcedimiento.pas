{**
@abstract(Configuracion del procedimiento.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(September 24, 2004)
Este modulo contiene la clase que almacena la informacion de configuracion sobre
procedimientos de ajuste de modelos mixtos.
}

unit UnitConfiguracionProcedimiento;

interface

uses
  Classes, SysUtils,
  UnitConfiguracionModelo, UnitOpcion;

type
  {** Configuracion sobre algun procedimiento relacionado a un modelo mixto. Es
      lo suficientemente abstracta como para permitir configurar cualquier clase
      de procedimiento de ajuste.
      @abstract(Configuracion sobre algun procedimiento involucrando modelos
      mixtos.) }
  TConfiguracionProcedimiento = class
  private
    {** Modelo mixto sobre el cual se desea llevar a cabo el procedimiento. }
    FModelo: TConfiguracionModelo;
    {** Configuracion propiamente dicha. Es una lista de
       @link(UnitOpcion.TOpcion) lo que permite representar una gran variedad de
       opciones de configuracion. }
    FOpciones: TListaOpciones;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AOpciones Opciones de configuracion del procedimiento.) }
    constructor Create(AOpciones: TListaOpciones);  overload;
    {** Constructor.
        @param(AModelo Modelo mixto sobre el cual se desea llevar a cabo el
        procedimiento.)
        @param(AOpciones Opciones de configuracion del procedimiento.) }
    constructor Create(AModelo: TConfiguracionModelo; AOpciones: TListaOpciones);  overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Valida la informacion de configuracion en si y en relacion al modelo
        sobre el cual se desea llevar a cabo el procedimiento.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la configuracion es valida o @false en caso
        contrario.) }
    function Validar(var Bitacora: TStrings): Boolean;
    {** Modelo mixto sobre el cual se desea llevar a cabo el procedimiento. }
    property Modelo: TConfiguracionModelo read FModelo write FModelo;
    {** Lista de opciones de configuracion. }
    property Opciones: TListaOpciones read FOpciones write FOpciones;
  end;

implementation

uses
  UnitParametros;

// -----------------------------------------------------------------------------
// TConfiguracionProcedimiento
// -----------------------------------------------------------------------------

constructor TConfiguracionProcedimiento.Create;
begin { TConfiguracionProcedimiento.Create }
  Create(nil, TListaOpciones.Create);
end { TConfiguracionProcedimiento.Create };

constructor TConfiguracionProcedimiento.Create(AOpciones: TListaOpciones);
begin { TConfiguracionProcedimiento.Create }
  Create(nil, AOpciones);
end { TConfiguracionProcedimiento.Create };

constructor TConfiguracionProcedimiento.Create(AModelo: TConfiguracionModelo; AOpciones: TListaOpciones);
begin { TConfiguracionProcedimiento.Create }
  FModelo := AModelo;
  FOpciones := AOpciones;
end { TConfiguracionProcedimiento.Create };

destructor TConfiguracionProcedimiento.Destroy;
begin { TConfiguracionProcedimiento.Destroy }
  try
    FreeAndNil(FOpciones);
  finally
    inherited Destroy;
  end;
end { TConfiguracionProcedimiento.Destroy };

function TConfiguracionProcedimiento.Validar(var Bitacora: TStrings): Boolean;
var
  I, J, K, L: Integer;
  Listo: Boolean;
  Opcion: TOpcion;
  ListaInformacionParametroIndice: TListaInformacionParametroIndice;
  S: String;
begin { TConfiguracionProcedimiento.Validar }
  Assert(Assigned(FModelo), 'TConfiguracionProcedimiento.Validar: Assigned(FModelo)');
  Assert(Assigned(FOpciones), 'TConfiguracionProcedimiento.Validar: Assigned(FOpciones)');
  Assert(Assigned(Bitacora), 'TConfiguracionProcedimiento.Validar: Assigned(Bitacora)');

  Result := True;
  Listo := False;
  I := 0;
  while (I <= FOpciones.Count - 1) and (not Listo) do
  begin
    Opcion := FOpciones [I];

    if (Opcion.Nombre = 'fuente_valores_iniciales_parametros') or
       (Opcion.Nombre = 'metodo') then
    begin
      if (Opcion as TOpcionParametroString).Parametro = 'mivque0' then
      begin
        // Verificar que sea posible utilizar mivque0

        // Verificar que las estructuras de covarianzas posean deriviadas
        // segundas nulas.

        if FModelo.TieneEfectosAleatorios then
        begin
          if FModelo.EfectosAleatorios.Opciones.BuscarAsignar(J, 'estructura') then
          begin
            S := (FModelo.EfectosAleatorios.Opciones [J] as TOpcionParametroTEstructura).Parametro.Nombre;
            if (S = 'factor_analytic') or
               (S = 'factor_analytic_sin_diagonal') or
               (S = 'factor_analytic_diagonal_escalar') or
               (S = 'autoregresiva') then
            begin
              // No es posible aplicar mivque0
              Bitacora.Add('Se eligio mivque0 como metodo o fuente de valores iniciales, pero una estructura de covarianza posee derivada segunda no nula.');
              Result := false;
              Listo := true;
            end;
          end;
        end;

        if FModelo.TieneEfectosAleatoriosGrupos then
        begin
          J := 0;
          while (J <= FModelo.GruposEfectosAleatorios.Count - 1) and (not Listo) do
          begin
            if FModelo.GruposEfectosAleatorios [J].Opciones.BuscarAsignar(K, 'estructura') then
            begin
              S := (FModelo.GruposEfectosAleatorios [J].Opciones [K] as TOpcionParametroTEstructura).Parametro.Nombre;
              if (S = 'factor_analytic') or
                 (S = 'factor_analytic_sin_diagonal') or
                 (S = 'factor_analytic_diagonal_escalar') or
                 (S = 'autoregresiva') then
              begin
                // No es posible aplicar mivque0
                Bitacora.Add('Se eligio mivque0 como metodo o fuente de valores iniciales, pero una estructura de covarianza posee derivada segunda no nula.');
                Result := false;
                Listo := true;
              end;
            end;

            Inc(J);
          end;
        end;

        J := FModelo.Error.IndexOf('estructura');
        if J <> -1 then
        begin
          S := (FModelo.Error [J] as TOpcionParametroTEstructura).Parametro.Nombre;
          if (S = 'factor_analytic') or
             (S = 'factor_analytic_sin_diagonal') or
             (S = 'factor_analytic_diagonal_escalar') or
             (S = 'autoregresiva') then
          begin
            // No es posible aplicar mivque0
            Bitacora.Add('Se eligio mivque0 como metodo o fuente de valores iniciales, pero una estructura de covarianza posee derivada segunda no nula.');
            Result := false;
            Listo := true;
          end;
        end;
      end
      else if (Opcion as TOpcionParametroString).Parametro = 'grilla_busqueda' then
      begin
        // Verificar que sea posible utilizar el metodo de grilla de busqueda

        // Verificar que todos los parametros posean cotas inferiores,
        // superiores y paso de busqueda.

        if (not FOpciones.BuscarAsignar(J, 'cota_inferior_parametros')) or
           (not FOpciones.BuscarAsignar(J, 'cota_superior_parametros')) or
           (not FOpciones.BuscarAsignar(J, 'paso_grilla_parametros')) then
        begin
          if FModelo.TieneEfectosAleatorios then
          begin
            if FModelo.EfectosAleatorios.Opciones.BuscarAsignar(J, 'parametros') then
            begin
              ListaInformacionParametroIndice := (FModelo.EfectosAleatorios.Opciones [J] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

              K := 0;
              while (K <= ListaInformacionParametroIndice.Count - 1) and (not Listo) do
              begin
                with ListaInformacionParametroIndice [K] do
                begin
                  if (not TieneCotaInferior) or
                     (not TieneCotaSuperior) or
                     (not TienePaso) then
                  begin
                    Bitacora.Add('Se eligio grilla de busqueda como metodo o fuente de valores iniciales, pero uno de los parametros de efectos aleatorios (' + IntToStr(K) + ') no posee cota inferior, superior o paso de busqueda.');
                    Result := false;
                    Listo := true;
                  end;
                end;

                Inc(K);
              end;
            end
            else
            begin
              Bitacora.Add('Se eligio grilla de busqueda como metodo o fuente de valores iniciales, pero no se definieron cota inferior, superior o paso de busqueda para los parametros de efectos aleatorios.');
              Result := false;
              Listo := true;
            end;
          end;

          if FModelo.TieneEfectosAleatoriosGrupos then
          begin
            J := 0;
            while (J <= FModelo.GruposEfectosAleatorios.Count - 1) and (not Listo) do
            begin
              if FModelo.GruposEfectosAleatorios [J].Opciones.BuscarAsignar(K, 'parametros') then
              begin
                ListaInformacionParametroIndice := (FModelo.GruposEfectosAleatorios [J].Opciones [K] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

                L := 0;
                while (L <= ListaInformacionParametroIndice.Count - 1) and (not Listo) do
                begin
                  with ListaInformacionParametroIndice [L] do
                  begin
                    if (not TieneCotaInferior) or
                       (not TieneCotaSuperior) or
                       (not TienePaso) then
                    begin
                      Bitacora.Add('Se eligio grilla de busqueda como metodo o fuente de valores iniciales, pero uno de los parametros del grupo #' + IntToStr(J) + ' de efectos aleatorios (' + IntToStr(L) + ') no posee cota inferior, superior o paso de busqueda.');
                      Result := false;
                      Listo := true;
                    end;
                  end;

                  Inc(L);
                end;
              end
              else
              begin
                Bitacora.Add('Se eligio grilla de busqueda como metodo o fuente de valores iniciales, pero no se definieron cota inferior, superior o paso de busqueda para los parametros del grupo efectos aleatorios #' + IntToStr(J) + '.');
                Result := false;
                Listo := true;
              end;

              Inc(J);
            end;
          end;

          if FModelo.Error.BuscarAsignar(J, 'parametros') then
          begin
            ListaInformacionParametroIndice := (FModelo.Error [J] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

            K := 0;
            while (K <= ListaInformacionParametroIndice.Count - 1) and (not Listo) do
            begin
              with ListaInformacionParametroIndice [K] do
              begin
                if (not TieneCotaInferior) or
                   (not TieneCotaSuperior) or
                   (not TienePaso) then
                begin
                  Bitacora.Add('Se eligio grilla de busqueda como metodo o fuente de valores iniciales, pero uno de los parametros del error (' + IntToStr(K) + ') no posee cota inferior, superior o paso de busqueda.');
                  Result := false;
                  Listo := true;
                end;
              end;

              Inc(K);
            end;
          end;
        end;
      end;
    end
    else if Opcion.Nombre = 'cantidad_pasos_scoring' then
    begin
      if (Opcion as TOpcionParametroInteger).Parametro < 0 then
      begin
        // Valor ilegal de cantidad de pasos de Scoring
        Bitacora.Add('Valor ilegal de cantidad de pasos iniciales utilizando Scoring.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'cantidad_maxima_iteraciones' then
    begin
      if (Opcion as TOpcionParametroInteger).Parametro <= 0 then
      begin
        // Valor ilegal de cantidad maxima de iteraciones
        Bitacora.Add('Valor ilegal de cantidad maxima de iteraciones del algoritmo de ajuste.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'cantidad_maxima_evaluaciones_funcion_objetivo' then
    begin
      if (Opcion as TOpcionParametroInteger).Parametro <= 0 then
      begin
        // Valor ilegal de cantidad maxima de evaluaciones de la funcion de verosimilitud
        Bitacora.Add('Valor ilegal de cantidad maxima de evaluaciones de la funcion objetivo del algoritmo de ajuste.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'tolerancia_convergencia' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de tolerancia de convergencia
        Bitacora.Add('Valor ilegal de tolerancia de convergencia.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'tolerancia_singularidad' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de tolerancia de singularidad
        Bitacora.Add('Valor ilegal de tolerancia de singularidades.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'tolerancia_cholesky' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de tolerancia de la descomposicion de Cholesky
        Bitacora.Add('Valor ilegal de tolerancia de descomposicion de Cholesky.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'valor_inicial_factor_ridging' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de valor inicial de factor de ridging
        Bitacora.Add('Valor ilegal de valor inicial de factor de ridging.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'factor_incremento_factor_ridging' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de factor de incremento de factor de ridging
        Bitacora.Add('Valor ilegal de factor de incremento de factor de ridging.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'factor_decremento_factor_ridging' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de factor de decremento de factor de ridging
        Bitacora.Add('Valor ilegal de factor de decremento de factor de ridging.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'valor_minimo_factor_ridging' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de valor minimo del factor de ridging
        Bitacora.Add('Valor ilegal de valor minimo de factor de ridging.');
        Result := false;
        Listo := true;
      end;
    end
    else if Opcion.Nombre = 'valor_maximo_factor_ridging' then
    begin
      if (Opcion as TOpcionParametroReal).Parametro <= 0 then
      begin
        // Valor ilegal de valor maximo del factor de ridging
        Bitacora.Add('Valor ilegal de valor maximo de factor de ridging.');
        Result := false;
        Listo := true;
      end;
    end;

    Inc(I);
  end;
end { TConfiguracionProcedimiento.Validar };

end { UnitConfiguracionProcedimiento }.
