unit Eagle.ERP.{ModuleName}.View.{ModelName}View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Mask, MyEdit, Vcl.Menus,

  Spring.Container,

  DSharp.Bindings.VCLControls,

  Eagle.Core.Bind.Attributes,

  Eagle.Core.View.Inseptors.VCLControls.MyEdit,
  Eagle.ERP.Common.View.NavigationActions,
  Eagle.ERP.Common.View.ActionsCrud,
  Eagle.ERP.Common.View.CrudNavigateBar;

type
  [ViewModel('{ModelName}ViewModel')]
  [Secured('permissionName')]
  T{ModelName}View = class(TForm)
    [DataBinding('OnCloseQuery', 'OnCloseQuery')]
    [DataBinding('StatusBarText', 'StatusBar.Panels[0].Text')]
    StatusBar: TStatusBar;
    PageControl1: TPageControl;
    [DataBinding('Editable', 'TabGeral.Enabled')]
    TabGeral: TTabSheet;
    [AutoBinding('CrudActions')]
    CrudActions: TCrudNavigateBar;
    LB{ModelName}_id: TLabel;
    LB{ModelName}_nome: TLabel;
    [DataBinding('Codigo', 'ME{ModelName}_id.Text')]
    ME{ModelName}_id: TMyEdit;
    [DataBinding('Nome', 'ME{ModelName}_nome.Text', True)]
    ME{ModelName}_nome: TMyEdit;
    [DataBinding('IsInativo', 'CB{ModelName}_inativo.Checked')]
    CB{ModelName}_inativo: TCheckBox;

    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure T{ModelName}View.FormKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;

  if (Key = #13) then
  begin
    Key := #0;
    keybd_event(VK_TAB, 0, 0, 0);
  end;

end;

initialization

GlobalContainer.RegisterType<T{ModelName}View>('{ModelName}View');

end.
