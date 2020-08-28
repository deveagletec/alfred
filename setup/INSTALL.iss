#define MyAppName "Alfred"
#define MyAppVersion "1.0"
#define MyAppExeName "Alfred.exe"
#define MyAppBuild "..\build\Win32\Debug"

[Setup]
AppId={{2E66D7BB-609C-4573-B908-C349B92348B4}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName}{#MyAppVersion}
DefaultDirName={autopf}\{#MyAppName}
DisableProgramGroupPage=yes
UsedUserAreasWarning=no
LicenseFile=C:\development\Projects\alfred\LICENSE
OutputDir=output
OutputBaseFilename=Installer_{#MyAppName}_{#MyAppVersion}
Compression=lzma
SolidCompression=yes
WizardStyle=modern
AlwaysRestart=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"

[Tasks]
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 6.1; Check: not IsAdminInstallMode

[Dirs]
Name: {app}\templates; Permissions: users-full

[Files]
Source: "{#MyAppBuild}\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "resources\Alfred.conf"; DestDir: "{app}"; Flags: ignoreversion onlyifdoesntexist
Source: "..\resources\templates\default\*.pas"; DestDir: "{app}\templates\default"; Flags: ignoreversion onlyifdoesntexist

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchicon

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: expandsz; ValueName: "Alfred"; ValueData: "{autopf}\{#MyAppName}"; Flags: createvalueifdoesntexist uninsdeletekeyifempty uninsdeletevalue; Permissions: users-full

