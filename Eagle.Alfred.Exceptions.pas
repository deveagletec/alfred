unit Eagle.Alfred.Exceptions;

interface
uses
  System.SysUtils;

type

  EAlfredException = class(Exception);

  EAlfredFileNotFoundException = class(EAlfredException);

  EAlfredPackageInvalidException = class(EAlfredException);

implementation

end.
