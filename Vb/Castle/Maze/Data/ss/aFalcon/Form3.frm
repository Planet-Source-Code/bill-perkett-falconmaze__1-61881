VERSION 5.00
Begin VB.Form Form3 
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "FalconMaze- start"
   ClientHeight    =   7800
   ClientLeft      =   45
   ClientTop       =   315
   ClientWidth     =   6360
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7800
   ScaleWidth      =   6360
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton d2 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Start Pos"
      Height          =   975
      Left            =   2400
      Picture         =   "Form3.frx":0000
      Style           =   1  'Graphical
      TabIndex        =   11
      Top             =   3240
      Width           =   975
   End
   Begin VB.CommandButton Command3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Quit"
      Height          =   615
      Left            =   120
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   7080
      Width           =   6135
   End
   Begin VB.CommandButton Command2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Make a maze"
      Height          =   615
      Left            =   120
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   6360
      Width           =   6135
   End
   Begin VB.CommandButton Command1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Play a maze"
      Height          =   615
      Left            =   120
      Style           =   1  'Graphical
      TabIndex        =   0
      Top             =   5640
      Width           =   6135
   End
   Begin VB.Label Label13 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Bomb"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   5040
      TabIndex        =   16
      Top             =   2880
      Width           =   405
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   10
      Left            =   4920
      Picture         =   "Form3.frx":1302
      Tag             =   "bomb"
      Top             =   2040
      Width           =   660
   End
   Begin VB.Label Label12 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Metal Boots"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   360
      TabIndex        =   15
      Top             =   2880
      Width           =   840
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   9
      Left            =   480
      Picture         =   "Form3.frx":2604
      Tag             =   "lock"
      Top             =   2040
      Width           =   660
   End
   Begin VB.Label Label11 
      BackStyle       =   0  'Transparent
      Caption         =   "Locked         Door"
      ForeColor       =   &H8000000E&
      Height          =   675
      Left            =   5640
      TabIndex        =   14
      Top             =   1560
      Width           =   1170
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   8
      Left            =   5640
      Picture         =   "Form3.frx":3906
      Tag             =   "lock"
      Top             =   840
      Width           =   660
   End
   Begin VB.Label Label10 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Key"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   4920
      TabIndex        =   13
      Top             =   1680
      Width           =   270
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   3
      Left            =   4680
      Picture         =   "Form3.frx":4C08
      Tag             =   "key"
      Top             =   840
      Width           =   660
   End
   Begin VB.Label Label9 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "The start will be ONE of the these squares"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   1560
      TabIndex        =   12
      Top             =   4320
      Width           =   2985
   End
   Begin VB.Label Label8 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "The exit will be ONE of the these squares"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   1560
      TabIndex        =   10
      Top             =   2880
      Width           =   2925
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   2
      Left            =   2520
      Picture         =   "Form3.frx":5F0A
      Tag             =   "random"
      Top             =   2040
      Width           =   660
   End
   Begin VB.Label Label7 
      BackColor       =   &H00000000&
      Caption         =   "Avoid the lava, spikes, bombs and holes as you find your way through the maze. "
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1200
      Left            =   120
      TabIndex        =   9
      Top             =   4560
      Width           =   5910
   End
   Begin VB.Label Label6 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Wall"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   2880
      TabIndex        =   8
      Top             =   1680
      Width           =   315
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Spikes"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   1920
      TabIndex        =   7
      Top             =   1680
      Width           =   480
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Hole"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   1200
      TabIndex        =   6
      Top             =   1680
      Width           =   330
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Lava"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   240
      TabIndex        =   5
      Top             =   1680
      Width           =   360
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Exit"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   3960
      TabIndex        =   4
      Top             =   1680
      Width           =   255
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   4
      Left            =   1860
      Picture         =   "Form3.frx":720C
      Tag             =   "spikes"
      Top             =   840
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   7
      Left            =   3720
      Picture         =   "Form3.frx":850E
      Tag             =   "door"
      Top             =   840
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   1
      Left            =   2730
      Picture         =   "Form3.frx":9810
      Tag             =   "wall"
      Top             =   840
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   5
      Left            =   990
      Picture         =   "Form3.frx":AB12
      Tag             =   "hole"
      Top             =   840
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   6
      Left            =   120
      Picture         =   "Form3.frx":BE14
      Tag             =   "lava"
      Top             =   840
      Width           =   660
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Falcon Maze"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   360
      Left            =   2280
      TabIndex        =   3
      Top             =   120
      Width           =   1815
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
ldr.Show
Me.Hide
End Sub

Private Sub Command2_Click()
Form2.Show
Me.Hide
End Sub

Private Sub Command3_Click()
Unload Me
End
End Sub

Private Sub Form_Load()
  Randomize
End Sub
