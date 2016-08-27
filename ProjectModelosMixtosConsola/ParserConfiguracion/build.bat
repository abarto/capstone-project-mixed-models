@echo off
del ParserConfiguracionLex.pas
del UnitParserConfiguracion.*
plex.exe -v ParserConfiguracionLex.l
pyacc.exe -v ParserConfiguracion.y UnitParserConfiguracion.pas