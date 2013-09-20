object WMain: TWMain
  Left = 0
  Top = 0
  Caption = 'Where is My Packet? v.0.3.10'
  ClientHeight = 444
  ClientWidth = 629
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
    Width = 629
    Height = 65
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 507
    DesignSize = (
      629
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
      NumbersOnly = True
      TabOrder = 1
    end
    object bbtnStartStop: TBitBtn
      Left = 472
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
      ExplicitLeft = 350
    end
  end
  object pcMainPageControl: TPageControl
    Left = 0
    Top = 65
    Width = 629
    Height = 379
    ActivePage = tsGraphBandwidth
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 507
    ExplicitHeight = 309
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      ExplicitWidth = 499
      ExplicitHeight = 281
      object lbLog: TListBox
        Left = 0
        Top = 0
        Width = 621
        Height = 351
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 499
        ExplicitHeight = 281
      end
    end
    object tsStats: TTabSheet
      Caption = 'Statistic'
      ExplicitWidth = 499
      ExplicitHeight = 281
      object sgStats: TStringGrid
        Left = 0
        Top = 0
        Width = 621
        Height = 351
        Align = alClient
        DefaultColWidth = 80
        DefaultRowHeight = 18
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
        TabOrder = 0
        ExplicitWidth = 499
        ExplicitHeight = 281
      end
    end
    object tsGraphBandwidth: TTabSheet
      Caption = 'Network Graph'
      ImageIndex = 2
      ExplicitWidth = 499
      ExplicitHeight = 281
      object tcGraphBandwidth: TChart
        Left = 0
        Top = 0
        Width = 621
        Height = 351
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
        ExplicitWidth = 499
        ExplicitHeight = 241
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
    object tsGraphErrors: TTabSheet
      Caption = 'Errors Graph'
      ImageIndex = 3
      ExplicitWidth = 499
      ExplicitHeight = 281
      object tcErrorsGraph: TChart
        Left = 0
        Top = 0
        Width = 621
        Height = 351
        AllowPanning = pmNone
        Legend.Visible = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.DateTimeFormat = 'dd.mm hh:nn'
        BottomAxis.ExactDateTime = False
        BottomAxis.Increment = 0.003472222222222222
        BottomAxis.LabelsAngle = 90
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.ExactDateTime = False
        LeftAxis.Increment = 1.000000000000000000
        LeftAxis.Maximum = 100.000000000000000000
        Panning.MouseWheel = pmwNone
        View3D = False
        Zoom.Allow = False
        Align = alClient
        Color = clWindow
        TabOrder = 0
        ExplicitWidth = 499
        ExplicitHeight = 281
        PrintMargins = (
          15
          22
          15
          22)
        ColorPaletteIndex = 13
        object tcsErrorsCount: TBarSeries
          BarBrush.Gradient.EndColor = 2152289
          BarBrush.Gradient.MidColor = clRed
          BarBrush.Gradient.StartColor = 10485760
          Marks.Angle = 90
          Marks.Arrow.Visible = False
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = False
          Marks.Callout.Distance = 3
          Marks.Callout.Length = 0
          Marks.BackColor = clWhite
          Marks.Color = clWhite
          Marks.Emboss.Color = 8487297
          Marks.Font.Height = -8
          Marks.Font.Name = 'Tahoma'
          Marks.Margins.Left = 3
          Marks.Margins.Top = 3
          Marks.Margins.Right = 6
          Marks.Margins.Bottom = 3
          Marks.Margins.Units = maPixels
          Marks.Shadow.Color = 8684676
          Marks.Visible = False
          Title = 'Errors'
          Emboss.Color = 8487297
          Gradient.EndColor = 2152289
          Gradient.MidColor = clRed
          Gradient.StartColor = 10485760
          MarksLocation = mlStart
          MarksOnBar = True
          Shadow.Color = 8487297
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
          Data = {
            000600000000000000008076400000000000B870400000000000B06340000000
            00000054400000000000404F400000000000404040}
        end
      end
    end
  end
  object timerUpdateView: TTimer
    OnTimer = timerUpdateViewTimer
    Left = 776
    Top = 432
  end
  object timerCheckStream: TTimer
    Interval = 300
    OnTimer = timerCheckStreamTimer
    Left = 776
    Top = 480
  end
end
