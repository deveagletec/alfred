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
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = CrudActions.pmMenu
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 778
    Height = 467
    ActivePage = TabGeral
    TabOrder = 0
    object TabGeral: TTabSheet
      Caption = 'Geral'
      object lb_{ModelName}_id: TLabel
        Left = 19
        Top = 19
        Width = 37
        Height = 13
        Alignment = taRightJustify
        Caption = 'C'#243'digo:'
      end
      object lb_{ModelName}_nome: TLabel
        Left = 16
        Top = 57
        Width = 40
        Height = 13
        Alignment = taRightJustify
        Caption = '* Nome:'
      end
      object {ModelName}_id: TMyEdit
        Left = 62
        Top = 16
        Width = 73
        Height = 21
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
      object {ModelName}_nome: TMyEdit
        Left = 62
        Top = 54
        Width = 609
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 10
        TabOrder = 1
        Text = ''
        MaxValue = 2147483647.000000000000000000
        FieldMapping = 'entity_nome'
      end
      object {ModelName}_inativo: TCheckBox
        Left = 62
        Top = 90
        Width = 97
        Height = 17
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
