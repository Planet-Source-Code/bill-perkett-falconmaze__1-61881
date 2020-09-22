VERSION 5.00
Begin VB.Form MapLdr 
   Caption         =   "MapLdr"
   ClientHeight    =   6585
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form5"
   ScaleHeight     =   6585
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command3 
      Caption         =   "Save"
      Height          =   495
      Left            =   360
      TabIndex        =   5
      Top             =   6000
      Width           =   2775
   End
   Begin VB.FileListBox File1 
      BackColor       =   &H80000006&
      ForeColor       =   &H0000FFFF&
      Height          =   4380
      Left            =   0
      TabIndex        =   4
      Top             =   360
      Width           =   2775
   End
   Begin VB.DirListBox Dir1 
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FFFF&
      Height          =   4140
      Left            =   0
      TabIndex        =   2
      Top             =   360
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   0
      TabIndex        =   1
      Top             =   5520
      Width           =   3375
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Load"
      Height          =   375
      Left            =   0
      TabIndex        =   0
      Top             =   5040
      Visible         =   0   'False
      Width           =   3375
   End
   Begin VB.Label Label1 
      BackColor       =   &H00000000&
      Caption         =   "Double-click a game folder to Load"
      ForeColor       =   &H0000FF00&
      Height          =   375
      Left            =   0
      TabIndex        =   3
      Top             =   0
      Width           =   3255
   End
End
Attribute VB_Name = "MapLdr"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command2_Click()
On Error GoTo dangit



Form2.Show

Open Dir1.Path & "\MAPNAME.nfm" For Input As #1
Dim yyd
Input #1, yyd
Close #1
Form2.TxtTitle.Text = yyd

Open Dir1.Path & "\MAPLIVES.nfm" For Input As #1
Dim yy3
Input #1, yy3
Close #1
Form2.ll.Text = yy3


'orm2.Show
Me.Hide
Dim n
For n = 0 To 120
Dim parser
Open Dir1.Path & "\tile" & n & ".ffm" For Input As #1
Input #1, parser
Close #1
Form2.tile(n).Tag = parser
If Form2.tile(n).Tag = "dirt" Then Form2.tile(n).Picture = Form2.dropper(0).Picture
If Form2.tile(n).Tag = "door" Then Form2.tile(n).Picture = Form2.dropper(7).Picture
If Form2.tile(n).Tag = "wall" Then Form2.tile(n).Picture = Form2.dropper(1).Picture
If Form2.tile(n).Tag = "lava" Then Form2.tile(n).Picture = Form2.dropper(6).Picture
If Form2.tile(n).Tag = "hole" Then Form2.tile(n).Picture = Form2.dropper(5).Picture
If Form2.tile(n).Tag = "spikes" Then Form2.tile(n).Picture = Form2.dropper(4).Picture
If Form2.tile(n).Tag = "start" Then Form2.tile(n).Picture = Form2.d2.Picture
If Form2.tile(n).Tag = "random" Then Form2.tile(n).Picture = Form2.dropper(2).Picture
Open Dir1.Path & "\mazetime.ttm" For Input As #1
Dim ty
Input #1, ty
Form2.tol.Text = ty
Close #1


Next n
Exit Sub
dangit:
MsgBox "Invalid Maze file.", vbCritical, "ACK error 001"
ldr.Show
Unload Form2
Exit Sub
End Sub

Private Sub Command3_Click()
'    Dim Y
'    On Error Resume Next
'    Y = InputBox("Whats the map's name???", "EXPORT", "NONAME")
'    If Y = "" Then Exit Sub
'
'    'MkDir App.Path & "\mazes\" & Y
'
'    Open App.Path & "\mazes\" & Y & ".nfm" For Output As #1
'    Dim dyta
'    dyta = Y
'    Print #1, Y
'    'Close #1
'
'    'Open App.Path & "\mazes\" & Y & "\" & "MAPLIVES.nfm" For Output As #1
'    Dim dyta2
'    dyta2 = ll.Text
'    Print #1, dyta2
'    'Close #1
'
'
'    Dim e
'    For e = 0 To 120
'    'Open App.Path & "\mazes\" & Y & "\tile" & e & ".ffm" For Output As #1
'    Dim nfo
'    nfo = tile(e).Tag
'    Print #1, nfo
'    'Close #1
'    Next e
'
'    'Open App.Path & "\mazes\" & Y & "\mazetime.ttm" For Output As #1
'    Dim gh
'    gh = tol.Text
'    Print #1, gh
'    Close #1
'    MsgBox "Save Successful!", vbOKOnly, "YAY!"
End Sub

Private Sub Dir1_Change()
  File1.Path = Dir1.Path
End Sub

Private Sub File1_Click()
On Error GoTo dangit



    Form2.Show

    Open Dir1.Path & "\" & File1.FileName For Input As #1
'    Dim yyd
'    Input #1, yyd
'    'Close #1
'    'Form2.mn = yyd
'
'   ' Open Dir1.Path & "\MAPLIVES.nfm" For Input As #1
'    Dim yy3
'    Input #1, yy3
'    'Close #1
'   ' 'Form2.lvs = yy3
'
'
'    Form2.Show
'    Me.Hide
'    Dim n
'    Dim parser As String
'    For n = 0 To 120
'    'Open Dir1.Path & "\tile" & n & ".ffm" For Input As #1
'    Input #1, parser
'   ' Close #1
'    Form2.tile(n).Tag = parser
'    If Form2.tile(n).Tag = "dirt" Then Form2.tile(n).Picture = Form2.dropper(0).Picture
'    If Form2.tile(n).Tag = "door" Then Form2.tile(n).Picture = Form2.dropper(7).Picture
'    If Form2.tile(n).Tag = "wall" Then Form2.tile(n).Picture = Form2.dropper(1).Picture
'    If Form2.tile(n).Tag = "lava" Then Form2.tile(n).Picture = Form2.dropper(6).Picture
'    If Form2.tile(n).Tag = "hole" Then Form2.tile(n).Picture = Form2.dropper(5).Picture
'    If Form2.tile(n).Tag = "spikes" Then Form2.tile(n).Picture = Form2.dropper(4).Picture
'    If Form2.tile(n).Tag = "start" Then Form2.tile(n).Picture = Form2.d2.Picture
'    Next n
'    'Open Dir1.Path & "\mazetime.ttm" For Input As #1
'    Dim ty
'    Input #1, ty
'    'Form2.tm.Caption = ty
'    Close #1
'
'
'    'Next n
'    Exit Sub
'dangit:
'    MsgBox "Invalid Maze file.", vbCritical, "ACK error 001"
'    ldr.Show
'    Unload Form2
'    Exit Sub
    Dim yyd
    Input #1, yyd
    Form2.TxtTitle.Text = yyd
    
    Dim yy3
    Input #1, yy3
    Form2.ll.Text = yy3
    
    
    'orm2.Show
    Me.Hide
    Dim n
    Dim parser As String
    For n = 0 To 120
    Input #1, parser
    Form2.tile(n).Tag = parser
    If Form2.tile(n).Tag = "dirt" Then Form2.tile(n).Picture = Form2.dropper(0).Picture
    If Form2.tile(n).Tag = "door" Then Form2.tile(n).Picture = Form2.dropper(7).Picture
    If Form2.tile(n).Tag = "wall" Then Form2.tile(n).Picture = Form2.dropper(1).Picture
    If Form2.tile(n).Tag = "lava" Then Form2.tile(n).Picture = Form2.dropper(6).Picture
    If Form2.tile(n).Tag = "hole" Then Form2.tile(n).Picture = Form2.dropper(5).Picture
    If Form2.tile(n).Tag = "spikes" Then Form2.tile(n).Picture = Form2.dropper(4).Picture
    If Form2.tile(n).Tag = "start" Then Form2.tile(n).Picture = Form2.d2.Picture
    If Form2.tile(n).Tag = "random" Then Form2.tile(n).Picture = Form2.dropper(2).Picture
    If Form2.tile(n).Tag = "key" Then Form2.tile(n).Picture = Form2.dropper(3).Picture
    If Form2.tile(n).Tag = "lock" Then Form2.tile(n).Picture = Form2.dropper(8).Picture
    If Form2.tile(n).Tag = "boots" Then Form2.tile(n).Picture = Form2.dropper(9).Picture
    If Form2.tile(n).Tag = "bomb" Then Form2.tile(n).Picture = Form2.dropper(10).Picture
    Next n
    
    Dim ty
    Input #1, ty
    Form2.tol.Text = ty
    Input #1, iBombMax
    Form2.MaxBombs.Text = iBombMax
    Close #1
    Exit Sub
dangit:
    MsgBox "Invalid Maze file.", vbCritical, "ACK error 001"
    ldr.Show
    Unload Form2
    Exit Sub
End Sub

Private Sub Form_Load()
  Dir1.Path = App.Path & "\Mazes"
End Sub
