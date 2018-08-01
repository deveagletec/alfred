unit Eagle.Alfred.Exceptions;

interface
uses
  System.SysUtils;

type

  EAlfredException = class(Exception);

  EAlfredFileNotFoundException = class(EAlfredException);

implementation

end.
