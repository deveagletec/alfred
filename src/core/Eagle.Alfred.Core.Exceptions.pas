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

  EAlfredDeleteFileException = class(EAlfredException);

  EDataBaseException = class(EAlfredException);

  EFileNotFoundException = class(EAlfredException);

  EFileAlwaysExistsException = class(EAlfredException);

  EPackageNotFoundException = class(EAlfredException);

  EPackageInvalidException = class(EAlfredException);

  ECommandGroupNotFoundException = class(EAlfredException);

  ECommandNotFound = class(EAlfredException);

  EJSONReadException = class(EAlfredException);

  ERequiredParameterException = class(EAlfredException);

  EMigrationsNotFoundException = class(EAlfredException);

  ENoExecutedMigrationFoundException = class(EAlfredException);

  EDependencyDefinitionException = class(EAlfredException);

  ERepositoryTypeInvalidException = class(EAlfredException);

  EDownloadException = class(EAlfredException);

  EUninstallException = class(EAlfredException);

implementation

end.
