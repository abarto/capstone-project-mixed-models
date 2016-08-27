@echo off
del *.dcu
rem dcc32 -$H- -$R+ -$Q+ -P -cc -H -M
dcc32 -$H- -$R+ -$Q+ -M lexlib.pas
dcc32 -$H- -$R+ -$Q+ -M yacclib.pas