object WMain: TWMain
  Left = 0
  Top = 0
  Caption = 'Where is My Packet? v.0.2'
  ClientHeight = 374
  ClientWidth = 507
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
    Width = 507
    Height = 65
    Align = alTop
    TabOrder = 0
    DesignSize = (
      507
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
      Left = 350
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
  object pcMainPageControl: TPageControl
    Left = 0
    Top = 65
    Width = 507
    Height = 309
    ActivePage = tsGraphBandwidth
    Align = alClient
    TabOrder = 1
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object lbLog: TListBox
        Left = 0
        Top = 0
        Width = 499
        Height = 281
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsStats: TTabSheet
      Caption = 'Statistic'
      object sgStats: TStringGrid
        Left = 0
        Top = 0
        Width = 499
        Height = 281
        Align = alClient
        DefaultColWidth = 80
        DefaultRowHeight = 18
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
        TabOrder = 0
      end
    end
    object tsGraphBandwidth: TTabSheet
      Caption = 'Network Graph'
      ImageIndex = 2
      object tcGraphBandwidth: TChart
        Left = 0
        Top = 0
        Width = 499
        Height = 281
        Cursor = crCross
        AllowPanning = pmNone
        Legend.Visible = False
        ScrollMouseButton = mbLeft
        Title.Text.Strings = (
          'Bandwidth (Packets/sec)')
        Title.Visible = False
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.Title.Caption = 'Packets / sec'
        Panning.MouseWheel = pmwNone
        RightAxis.Automatic = False
        RightAxis.AutomaticMinimum = False
        RightAxis.Title.Caption = 'KBytes / sec'
        View3D = False
        Zoom.Allow = False
        Zoom.MouseButton = mbRight
        Align = alClient
        Color = clWindow
        TabOrder = 0
        ColorPaletteIndex = 13
        object tcsBandwidth: TLineSeries
          Cursor = crCross
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          ShowInLegend = False
          Title = 'Bytes/sec'
          VertAxis = aRightAxis
          Brush.BackColor = clDefault
          LinePen.Color = 10708548
          LinePen.Width = 2
          Pointer.Brush.Gradient.EndColor = 10708548
          Pointer.Gradient.EndColor = 10708548
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          TreatNulls = tnIgnore
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
          object TSmoothingFunction
            CalcByValue = False
            Period = 1.000000000000000000
            Factor = 8
          end
        end
        object tcsPackets: TLineSeries
          Cursor = crCross
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          ShowInLegend = False
          Title = 'Packets/sec'
          Brush.BackColor = clDefault
          LinePen.Color = 65408
          LinePen.Width = 2
          Pointer.Brush.Gradient.EndColor = 10708548
          Pointer.Gradient.EndColor = 10708548
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          TreatNulls = tnIgnore
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
          object TSmoothingFunction
            CalcByValue = False
            Period = 1.000000000000000000
            Factor = 8
          end
        end
      end
    end
  end
  object timerUpdateView: TTimer
    OnTimer = timerUpdateViewTimer
    Left = 776
    Top = 432
  end
end
