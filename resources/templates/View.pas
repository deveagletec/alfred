unit Eagle.ERP.{ModuleName}.View.{ModelName}View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Mask, MyEdit, Vcl.Menus,

  Spring.Container,

  DSharp.Bindings.VCLControls,

  Eagle.ERP.Common.Bind.Attributes,

  Eagle.ERP.Common.View.Inseptors.VCLControls.MyEdit,
  Eagle.ERP.Common.View.NavigationActions,
  Eagle.ERP.Common.View.ActionsCrud,
  Eagle.ERP.Common.View.CrudNavigateBar;

type
  [ViewModel('{ModelName}ViewModel')]
  T{ModelName}View = class(TForm)
    [DataBinding('OnCloseQuery', 'OnCloseQuery')]
    [DataBinding('StatusBarText', 'StatusBar.Panels[0].Text')]
    StatusBar: TStatusBar;
    PageControl1: TPageControl;
    [DataBinding('Editable', 'TabGeral.Enabled')]
    TabGeral: TTabSheet;
    [AutoBinding('CrudActions')]
    CrudActions: TCrudNavigateBar;
    lb_{ModelName}_id: TLabel;
    lb_{ModelName}_nome: TLabel;
    [DataBinding('Codigo', '{ModelName}_id.Text')]
    {ModelName}_id: TMyEdit;
    [DataBinding('Nome', '{ModelName}_nome.Text', True)]
    {ModelName}_nome: TMyEdit;
    [DataBinding('IsInativo', '{ModelName}_inativo.Checked')]
    {ModelName}_inativo: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

GlobalContainer.RegisterType<T{ModelName}View>('{ModelName}View');

end.
