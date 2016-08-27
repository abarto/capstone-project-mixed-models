@echo off
del *.dcu
dcc32 -$H- -$R+ -$Q+ -M lexlib.pas
dcc32 -$H- -$R+ -$Q+ -M yacclib.pas
rem dcc32 -$H- -$R+ -$Q+ -P -cc -H -M