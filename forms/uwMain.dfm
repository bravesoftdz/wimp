object WMain: TWMain
  Left = 0
  Top = 0
  Caption = 'Where is My Packet?'
  ClientHeight = 432
  ClientWidth = 613
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 613
    Height = 65
    Align = alTop
    TabOrder = 0
    DesignSize = (
      613
      65)
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
      Left = 456
      Top = 8
      Width = 137
      Height = 48
      Anchors = [akTop, akRight]
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
    Top = 65
    Width = 613
    Height = 223
    Align = alClient
    DefaultRowHeight = 18
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
    TabOrder = 1
  end
  object lbLog: TListBox
    Left = 0
    Top = 288
    Width = 613
    Height = 144
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object timerUpdateView: TTimer
    OnTimer = timerUpdateViewTimer
    Left = 776
    Top = 432
  end
end
