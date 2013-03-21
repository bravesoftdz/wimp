object WMain: TWMain
  Left = 0
  Top = 0
  AutoSize = True
  Caption = 'Where is My Packet?'
  ClientHeight = 312
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 493
    Height = 65
    TabOrder = 0
    object ledMulticastGroup: TLabeledEdit
      Left = 112
      Top = 8
      Width = 121
      Height = 21
      EditLabel.Width = 74
      EditLabel.Height = 13
      EditLabel.Caption = 'Multicast Group'
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object ledMulticastPort: TLabeledEdit
      Left = 112
      Top = 35
      Width = 121
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Port'
      LabelPosition = lpLeft
      TabOrder = 1
    end
    object bbtnStartStop: TBitBtn
      Left = 336
      Top = 8
      Width = 137
      Height = 48
      Caption = 'Start'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = bbtnStartStopClick
    end
  end
  object sgStats: TStringGrid
    Left = 0
    Top = 71
    Width = 493
    Height = 130
    DefaultColWidth = 48
    DefaultRowHeight = 18
    RowCount = 2
    FixedRows = 0
    TabOrder = 1
  end
  object lbLog: TListBox
    Left = 0
    Top = 207
    Width = 493
    Height = 105
    ItemHeight = 13
    TabOrder = 2
  end
end
