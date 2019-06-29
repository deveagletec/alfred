@ECHO OFF

@SET BDS=C:\Program Files (x86)\Embarcadero\Studio\16.0
IF %processor_architecture%==x86 @SET BDS=C:\Program Files\Embarcadero\Studio\16.0

@SET BDSCOMMONDIR=C:\Users\Public\Documents\Embarcadero\Studio\16.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\
@SET FrameworkVersion=v3.5
@SET PATH=%FrameworkDir%%FrameworkVersion%;%PATH%

@SET SourceDir=%1
@SET ProjectFile=%2
@SET Modo=%3
@SET BinDir=%SourceDir%\bin\%Modo%

msbuild /target:Build /p:config=%Modo% %ProjectFile% /property:DCC_UseMSBuildExternally=true /property:DCC_DcuOutput=%BinDir% /property:DCC_ObjOutput=%BinDir% /property:DCC_HppOutput=%BinDir% /property:DCC_BplOutput=%BinDir% /property:DCC_DcpOutput=%BinDir%