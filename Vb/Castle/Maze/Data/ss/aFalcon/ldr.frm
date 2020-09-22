VERSION 5.00
Begin VB.Form ldr 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Load a Maze"
   ClientHeight    =   6930
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   7800
   ControlBox      =   0   'False
   LinkTopic       =   "Form5"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6930
   ScaleWidth      =   7800
   StartUpPosition =   2  'CenterScreen
   Begin VB.FileListBox File1 
      BackColor       =   &H80000006&
      ForeColor       =   &H0000FFFF&
      Height          =   4185
      Left            =   120
      TabIndex        =   7
      Top             =   360
      Width           =   4095
   End
   Begin VB.OptionButton Option1 
      BackColor       =   &H00000000&
      Caption         =   "FlashLight Mode"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   5520
      Width           =   3375
   End
   Begin VB.OptionButton o2 
      BackColor       =   &H00000000&
      Caption         =   "ThunderBolt Mode"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   120
      TabIndex        =   5
      Top             =   5160
      Width           =   3375
   End
   Begin VB.OptionButton o1 
      BackColor       =   &H00000000&
      Caption         =   "One Square Mode (standard)"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   4800
      Value           =   -1  'True
      Width           =   3375
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Load"
      Height          =   375
      Left            =   120
      TabIndex        =   3
      Top             =   6000
      Width           =   3375
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   6480
      Width           =   3375
   End
   Begin VB.DirListBox Dir1 
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FFFF&
      Height          =   4140
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Visible         =   0   'False
      Width           =   3375
   End
   Begin VB.Label Label1 
      BackColor       =   &H00000000&
      Caption         =   "Double-click a game folder to play"
      ForeColor       =   &H0000FF00&
      Height          =   375
      Left            =   360
      TabIndex        =   1
      Top             =   120
      Width           =   3135
   End
End
Attribute VB_Name = "ldr"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Unload Me
Form3.Show
End Sub

Private Sub Command2_Click()
On Error GoTo dangit
 
iOldLoc = -1
iDoorcnt = 0

iOldStartLoc = -1
iStartcnt = 0
Open Dir1.Path & "\MAPNAME.nfm" For Input As #1
Dim yyd
Input #1, yyd
Close #1
Form1.mn = yyd

Open Dir1.Path & "\MAPLIVES.nfm" For Input As #1
Dim yy3
Input #1, yy3
Close #1
Form1.lvs = yy3


Form1.Show
Me.Hide
Dim n
For n = 0 To 120
Dim parser
Open Dir1.Path & "\tile" & n & ".ffm" For Input As #1
Input #1, parser
Close #1
If parser = "random" Then
  iDoorcnt = iDoorcnt + 1
  iDoorSave(iDoorcnt) = n
  parser = "dirt"
End If
If parser = "start" Then
  iStartcnt = iStartcnt + 1
  iStartSave(iStartcnt) = n
End If
Form1.tile(n).Tag = parser

Open Dir1.Path & "\mazetime.ttm" For Input As #1
Dim ty
Input #1, ty
Form1.tm.Caption = ty
Input #1, iBombMax
Close #1


Next n
'
' Assign Random Door
'
If iDoorcnt > 0 Then
  Call RanDoor
End If
If iStartcnt > 1 Then
  Call RanStart
End If
Exit Sub
dangit:
MsgBox "Invalid Maze file.", vbCritical, "ACK error 001"
ldr.Show
Unload Form1
Exit Sub
End Sub

Private Sub Dir1_Change()
  File1.Path = Dir1.Path
End Sub

Private Sub File1_Click()
On Error GoTo dangit
 
iOldLoc = -1
iDoorcnt = 0

iOldStartLoc = -1
iStartcnt = 0

iOldKeyLoc = -1
iKeycnt = 0

iOldLockLoc = -1
iLockcnt = 0

iOldBootLoc = -1
iBootcnt = 0
iBombcnt = 0
Open Dir1.Path & "\" & File1.FileName For Input As #1
Dim yyd
Input #1, yyd
Form1.mn = yyd

Dim yy3
Input #1, yy3
Form1.lvs = yy3
iMyLife = yy3

Form1.Show
Me.Hide
Dim n
Dim parser As String
For n = 0 To 120
Input #1, parser
If parser = "random" Then
  iDoorcnt = iDoorcnt + 1
  iDoorSave(iDoorcnt) = n
  parser = "dirt"
End If
If parser = "start" Then
  iStartcnt = iStartcnt + 1
  iStartSave(iStartcnt) = n
End If
If parser = "key" Then
  iKeycnt = iKeycnt + 1
  iKeySave(iKeycnt) = n
End If
If parser = "lock" Then
  iLockcnt = iLockcnt + 1
  iLockSave(iLockcnt) = n
End If
If parser = "boots" Then
  iBootcnt = iBootcnt + 1
  iBootSave(iBootcnt) = n
End If
If parser = "bomb" Then
  iBombcnt = iBombcnt + 1
  iBombSave(iBombcnt) = n
End If
Form1.tile(n).Tag = parser
Next n
Dim ty
Input #1, ty
Form1.tm.Caption = ty
iMyTime = ty
Input #1, iBombMax
Close #1



'
' Assign Random Door
'
If iDoorcnt > 0 Then
  Call RanDoor
End If

If iStartcnt > 1 Then
  Call RanStart
End If
If iKeycnt > 1 Then
  Call RanKey
End If
If iLockcnt > 1 Then
  Call RanLock
End If

Exit Sub
dangit:
MsgBox "Invalid Maze file.", vbCritical, "ACK error 001"
ldr.Show
Unload Form1
Exit Sub
End Sub

Private Sub Form_Load()
  Dir1.Path = App.Path & "\Mazes"
End Sub

Private Sub o1_Click()
On Error Resume Next
Form1.q1.Enabled = False
Form1.q2.Enabled = False
Form1.q3.Enabled = False

End Sub

Private Sub o2_Click()
On Error Resume Next
Form1.q1.Enabled = False
Form1.q2.Enabled = True
Form1.q3.Enabled = False

End Sub

Private Sub Option1_Click()
On Error Resume Next
Form1.q1.Enabled = False
Form1.q2.Enabled = False
Form1.q3.Enabled = True

End Sub

Private Sub Option2_Click()

End Sub
