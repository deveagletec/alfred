unit Eagle.Alfred.Exceptions;

interface
uses
  System.SysUtils;

type

  EAlfredException = class(Exception);

  EAlfredFileNotFoundException = class(EAlfredException);

  EAlfredPackageInvalidException = class(EAlfredException);

  EAlfredCreateFileException = class(EAlfredException);

  EAlfredCreateDirException = class(EAlfredException);

  ECommandGroupNotFoundException = class(EAlfredException);

  ECommandNotFound = class(EAlfredException);

implementation

end.
