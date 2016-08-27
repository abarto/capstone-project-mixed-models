{**
@abstract(Asociacion de un TStream a un TextFile.)
@author(Peter Below)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 9, 2004)
@lastmod(December 9, 2004)
Este modulo contiene la definicion del procedimiento AssignStream utilizado
para asignar un TextFile a un TStream, permitiendo utilizar el contenido
provisto por este ultimo mediante las funciones estandar de entrada/salida.
Basado en StreamIO de Peter Below.
}

unit StreamIO;

interface

uses
  Classes;

{** Asocia un TextFile a un TStream para perimitir el acceso a este ultimo
    mediante las subrutinas estandar de entrada y salida como Read, Write,
    ReadLn, etc.
    @author(P. Below)
    @param(F Archivo a asociar al stream.)
    @param(S Stream a acceder.) }
procedure AssignStream(var F: TextFile; S: TStream);

implementation

uses
  SysUtils;

{** Obtiene la referencia al stream almacenada en el area de datos del usuario
    de TTextRec.
    @author(P. Below)
    @param(F El TextFile a operar.)
    @returns(Referencia al stream.) }
function GetDevStream(var F: TTextRec): TStream;
begin
  Move(F.Userdata, Result, Sizeof(Result));
  Assert(Assigned(Result));
end { GetDevStream };

{** Utilizada por Read, ReadLn, etc. para llenar las memorias intermedias de
    TextFile desde el dispositivo.
    @author(P. Below)
    @param(F El TextFile a operar.)
    @returns(0, o sea, no ocurrio ningun error.) }
function DevIn(var F: TTextRec): Integer;
begin
  Result := 0;
  with F do
  begin
    BufEnd := GetDevStream(F).Read(BufPtr^, BufSize);
    BufPos := 0;
  end;
end { DevIn };

{** Metodo muleta, no realiza ninguna tarea.
    @author(P. Below)
    @param(F El TextFile a operar.)
    @returns(0, o sea, no ocurrio ningun error.) }
function DevFlushIn(var F: TTextRec): Integer;
begin
  Result := 0;
end { DevFlushIn };

{** Escribe el contenido de las memorias intermedias del TextFile en el stream.
    Utilizado por Write y WriteLn cuando se llena la memoria intermedia.
    Tambien es llamada por Flush.
    @author(P. Below)
    @param(F El TextFile a operar.)
    @returns(0, o sea, no ocurrio ningun error.)
    @raises(EStreamError Si la escritura fallase por alguna razon.) }
function DevOut(var F: TTextRec): Integer;
begin
  Result := 0;
  with F do
  begin
    if BufPos > 0 then
    begin
      GetDevStream(F).WriteBuffer(BufPtr^, BufPos);
      BufPos := 0;
    end;
  end;
end { DevOut };

{** Metodo muleta, no realiza ninguna tarea. Llamado por CloseFile.
    @author(P. Below)
    @param(F El TextFile a operar.)
    @returns(0, o sea, no ocurrio ningun error.) }
function DevClose(var F: TTextRec): Integer;
begin
  Result := 0;
end { DevClose };

{** Llamado por Reset, ReWrite o Append para preparar al TextFile para las
    subsiguientes operaciones de entrada y salida.
    @author(P. Below)
    @param(F El TextFile a operar.)
    @returns(0, o sea, no ocurrio ningun error.) }
function DevOpen(var F: TTextRec): Integer;
begin
  Result := 0;
  with F do
  begin
    case Mode of
      fmInput:
        begin
          InOutFunc := @DevIn;
          FlushFunc := @DevFlushIn;
          BufPos := 0;
          BufEnd := 0;
          GetDevStream(F).Position := 0;
        end;
      fmOutput:
        begin
          InOutFunc := @DevOut;
          FlushFunc := @DevOut;
          BufPos := 0;
          BufEnd := 0;
          GetDevStream(F).Position := 0;
        end;
      fmInOut:
        begin
          Mode := fmOutput;
          DevOpen(F);
          GetDevStream(F).Seek(0, soFromEnd);
        end;
    end;
  end;
end { DevOpen };

procedure AssignStream(var F: Textfile; S: TStream);
begin
  Assert(Assigned(S));
  with TTextRec(F) do
  begin
    Mode := fmClosed;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @DevOpen;
    CloseFunc := @DevClose;
    Name[0] := #0;
    Move(S, Userdata, Sizeof(S));
  end;
end { AssignStream };

end { StreamIO }.
