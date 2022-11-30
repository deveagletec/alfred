object {ModelName}View: T{ModelName}View
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Form'
  ClientHeight = 537
  ClientWidth = 794
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Size = 10
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = CrudActions.pmMenu
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 778
    Height = 467
    ActivePage = TabGeral
    TabOrder = 0
    object TabGeral: TTabSheet
      Caption = 'Geral'
      object LB{ModelName}_id: TLabel
        Left = 77
        Top = 19
        Width = 53
        Height = 16
        Alignment = taRightJustify
        Caption = 'C'#243'digo:'
      end
      object LB{ModelName}_nome: TLabel
        Left = 74
        Top = 58
        Width = 56
        Height = 16
        Alignment = taRightJustify
        Caption = '* Nome:'
      end
      object ME{ModelName}_id: TMyEdit
        Left = 138
        Top = 16
        Width = 76
        Height = 24
        Alignment = taCenter
        CharCase = ecUpperCase
        Enabled = False
        ReadOnly = True
        TabOrder = 0
        Text = '0'
        CheckChars = '0123456789E-'
        DisplayFormat = dfInteger
        MaxValue = 2147483647.000000000000000000
        FieldMapping = 'entity_id'
      end
      object ME{ModelName}_nome: TMyEdit
        Left = 138
        Top = 54
        Width = 557
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 10
        TabOrder = 1
        Text = ''
        MaxValue = 2147483647.000000000000000000
        FieldMapping = 'entity_nome'
      end
      object CB{ModelName}_inativo: TCheckBox
        Left = 629
        Top = 20
        Width = 66
        Height = 17
        Margins.Right = 0
        Caption = 'Inativo'
        TabOrder = 2
      end
    end
  end
  inline CrudActions: TCrudNavigateBar
    Left = 8
    Top = 485
    Width = 778
    Height = 25
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 485
    inherited Actions: TActionsCrud
      ExplicitLeft = 382
    end
    inherited BtnMenu: TButton
      ExplicitLeft = 0
      ExplicitTop = 0
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 518
    Width = 794
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 50
      end>
  end
end
