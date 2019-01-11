unit Eagle.Alfred.Core.Exceptions;

interface
uses
  System.SysUtils;

type

  EAlfredException = class(Exception);

  EAlfredFileNotFoundException = class(EAlfredException);

  EAlfredPackageInvalidException = class(EAlfredException);

  EAlfredCreateDirException = class(EAlfredException);

  EAlfredCreateFileException = class(EAlfredException);

  EDataBaseException = class(EAlfredException);
  
  EFileNotFoundException = class(EAlfredException);

  EFileAlwaysExistsException = class(EAlfredException);

  EPackageNotFoundException = class(EAlfredException);

  EPackageInvalidException = class(EAlfredException);

  EAlfredCreateFileException = class(EAlfredException);

  EAlfredCreateDirException = class(EAlfredException);

  ECommandGroupNotFoundException = class(EAlfredException);

  ECommandNotFound = class(EAlfredException);

  EJSONReadException = class(EAlfredException);


implementation

end.