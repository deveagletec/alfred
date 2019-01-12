unit Eagle.Alfred.Exceptions;

interface
uses
  System.SysUtils;

type

  EAlfredException = class(Exception);

  EFileNotFoundException = class(EAlfredException);

  EFileAlwaysExistsException = class(EAlfredException);

  EPackageNotFoundException = class(EAlfredException);

  EPackageInvalidException = class(EAlfredException);

  EAlfredCreateFileException = class(EAlfredException);

  EAlfredCreateDirException = class(EAlfredException);

  ECommandGroupNotFoundException = class(EAlfredException);

  ECommandNotFound = class(EAlfredException);

implementation

end.
