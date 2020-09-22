VERSION 5.00
Begin VB.Form Form2 
   BackColor       =   &H00404040&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "FalconMaze Editor"
   ClientHeight    =   9270
   ClientLeft      =   45
   ClientTop       =   315
   ClientWidth     =   9255
   ControlBox      =   0   'False
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9270
   ScaleWidth      =   9255
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox MaxBombs 
      BackColor       =   &H80000006&
      ForeColor       =   &H0000FFFF&
      Height          =   285
      Left            =   1920
      TabIndex        =   25
      Text            =   "0"
      Top             =   8040
      Width           =   1695
   End
   Begin VB.TextBox TxtTitle 
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FFFF&
      Height          =   285
      Left            =   1920
      TabIndex        =   19
      Text            =   "NoName"
      Top             =   8520
      Width           =   1575
   End
   Begin VB.CommandButton Command3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Load"
      Height          =   255
      Left            =   7200
      Style           =   1  'Graphical
      TabIndex        =   14
      Top             =   8280
      Width           =   975
   End
   Begin VB.TextBox ll 
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FFFF&
      Height          =   285
      Left            =   1920
      TabIndex        =   8
      Text            =   "3"
      Top             =   7680
      Width           =   1575
   End
   Begin VB.TextBox tol 
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FFFF&
      Height          =   285
      Left            =   1920
      TabIndex        =   5
      Text            =   "60"
      Top             =   7200
      Width           =   1575
   End
   Begin VB.PictureBox Picture2 
      BackColor       =   &H00808080&
      Height          =   9135
      Left            =   7080
      ScaleHeight     =   9075
      ScaleWidth      =   1755
      TabIndex        =   1
      Top             =   0
      Width           =   1815
      Begin VB.CommandButton Command2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Quit"
         Height          =   255
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   4
         Top             =   8640
         Width           =   975
      End
      Begin VB.CommandButton Command1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Save"
         Height          =   255
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   7920
         Width           =   975
      End
      Begin VB.CommandButton d2 
         BackColor       =   &H00E0E0E0&
         Caption         =   "Start Pos"
         Height          =   975
         Left            =   120
         Picture         =   "Form2.frx":0000
         Style           =   1  'Graphical
         TabIndex        =   2
         Top             =   6840
         Width           =   975
      End
      Begin VB.Label Label16 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Metal Boots"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   360
         TabIndex        =   24
         Top             =   6480
         Width           =   840
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   10
         Left            =   960
         Picture         =   "Form2.frx":1302
         Tag             =   "bomb"
         Top             =   4680
         Width           =   660
      End
      Begin VB.Label Label15 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Bomb"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   1080
         TabIndex        =   23
         Top             =   5400
         Width           =   405
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   9
         Left            =   360
         Picture         =   "Form2.frx":2604
         Tag             =   "boots"
         Top             =   5760
         Width           =   660
      End
      Begin VB.Label Label14 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Key"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   1080
         TabIndex        =   22
         Top             =   4200
         Width           =   270
      End
      Begin VB.Label Label13 
         BackStyle       =   0  'Transparent
         Caption         =   "Locked Door"
         ForeColor       =   &H8000000E&
         Height          =   255
         Left            =   0
         TabIndex        =   21
         Top             =   5400
         Width           =   975
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   8
         Left            =   120
         Picture         =   "Form2.frx":3906
         Tag             =   "lock"
         Top             =   4680
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   3
         Left            =   960
         Picture         =   "Form2.frx":4C08
         Tag             =   "key"
         Top             =   3360
         Width           =   660
      End
      Begin VB.Label Label12 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Sand"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   240
         TabIndex        =   20
         Top             =   720
         Width           =   375
      End
      Begin VB.Label Label10 
         BackStyle       =   0  'Transparent
         Caption         =   "Random Exit"
         ForeColor       =   &H8000000E&
         Height          =   255
         Left            =   0
         TabIndex        =   17
         Top             =   4200
         Width           =   975
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   2
         Left            =   120
         Picture         =   "Form2.frx":5F0A
         Tag             =   "random"
         Top             =   3360
         Width           =   660
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Wall"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   240
         TabIndex        =   16
         Top             =   1800
         Width           =   315
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Hole"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   240
         TabIndex        =   13
         Top             =   3000
         Width           =   330
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Lava"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   1200
         TabIndex        =   11
         Top             =   1800
         Width           =   360
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Exit"
         ForeColor       =   &H8000000E&
         Height          =   195
         Left            =   1200
         TabIndex        =   10
         Top             =   720
         Width           =   255
      End
      Begin VB.Label Label2 
         BackStyle       =   0  'Transparent
         Caption         =   "Spikes"
         ForeColor       =   &H8000000E&
         Height          =   255
         Left            =   1080
         TabIndex        =   7
         Top             =   3000
         Width           =   495
      End
      Begin VB.Image brs3 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Left            =   240
         Picture         =   "Form2.frx":720C
         Tag             =   "dirt"
         Top             =   6960
         Visible         =   0   'False
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   7
         Left            =   960
         Picture         =   "Form2.frx":850E
         Tag             =   "door"
         Top             =   0
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   6
         Left            =   960
         Picture         =   "Form2.frx":9810
         Tag             =   "lava"
         Top             =   960
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   5
         Left            =   120
         Picture         =   "Form2.frx":AB12
         Tag             =   "hole"
         Top             =   2160
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   4
         Left            =   960
         Picture         =   "Form2.frx":BE14
         Tag             =   "spikes"
         Top             =   2160
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   1
         Left            =   120
         Picture         =   "Form2.frx":D116
         Tag             =   "wall"
         Top             =   960
         Width           =   660
      End
      Begin VB.Image dropper 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   0
         Left            =   120
         Picture         =   "Form2.frx":E418
         Tag             =   "dirt"
         Top             =   0
         Width           =   660
      End
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H00808080&
      Height          =   6975
      Left            =   0
      ScaleHeight     =   6915
      ScaleWidth      =   6915
      TabIndex        =   0
      Top             =   0
      Width           =   6975
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   0
         Left            =   120
         Picture         =   "Form2.frx":F71A
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   1
         Left            =   720
         Picture         =   "Form2.frx":10A1C
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   2
         Left            =   1320
         Picture         =   "Form2.frx":11D1E
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   3
         Left            =   1920
         Picture         =   "Form2.frx":13020
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   4
         Left            =   2520
         Picture         =   "Form2.frx":14322
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   5
         Left            =   3120
         Picture         =   "Form2.frx":15624
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   6
         Left            =   3720
         Picture         =   "Form2.frx":16926
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   7
         Left            =   4320
         Picture         =   "Form2.frx":17C28
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   8
         Left            =   4920
         Picture         =   "Form2.frx":18F2A
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   9
         Left            =   5520
         Picture         =   "Form2.frx":1A22C
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   10
         Left            =   6120
         Picture         =   "Form2.frx":1B52E
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   11
         Left            =   120
         Picture         =   "Form2.frx":1C830
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   12
         Left            =   720
         Picture         =   "Form2.frx":1DB32
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   13
         Left            =   1320
         Picture         =   "Form2.frx":1EE34
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   14
         Left            =   1920
         Picture         =   "Form2.frx":20136
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   15
         Left            =   2520
         Picture         =   "Form2.frx":21438
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   16
         Left            =   3120
         Picture         =   "Form2.frx":2273A
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   17
         Left            =   3720
         Picture         =   "Form2.frx":23A3C
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   18
         Left            =   4320
         Picture         =   "Form2.frx":24D3E
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   19
         Left            =   4920
         Picture         =   "Form2.frx":26040
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   20
         Left            =   5520
         Picture         =   "Form2.frx":27342
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   21
         Left            =   6120
         Picture         =   "Form2.frx":28644
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   22
         Left            =   120
         Picture         =   "Form2.frx":29946
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   23
         Left            =   720
         Picture         =   "Form2.frx":2AC48
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   24
         Left            =   1320
         Picture         =   "Form2.frx":2BF4A
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   25
         Left            =   1920
         Picture         =   "Form2.frx":2D24C
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   26
         Left            =   2520
         Picture         =   "Form2.frx":2E54E
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   27
         Left            =   3120
         Picture         =   "Form2.frx":2F850
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   28
         Left            =   3720
         Picture         =   "Form2.frx":30B52
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   29
         Left            =   4320
         Picture         =   "Form2.frx":31E54
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   30
         Left            =   4920
         Picture         =   "Form2.frx":33156
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   31
         Left            =   5520
         Picture         =   "Form2.frx":34458
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   32
         Left            =   6120
         Picture         =   "Form2.frx":3575A
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   33
         Left            =   120
         Picture         =   "Form2.frx":36A5C
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   34
         Left            =   720
         Picture         =   "Form2.frx":37D5E
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   35
         Left            =   1320
         Picture         =   "Form2.frx":39060
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   36
         Left            =   1920
         Picture         =   "Form2.frx":3A362
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   37
         Left            =   2520
         Picture         =   "Form2.frx":3B664
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   38
         Left            =   3120
         Picture         =   "Form2.frx":3C966
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   39
         Left            =   3720
         Picture         =   "Form2.frx":3DC68
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   40
         Left            =   4320
         Picture         =   "Form2.frx":3EF6A
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   41
         Left            =   4920
         Picture         =   "Form2.frx":4026C
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   42
         Left            =   5520
         Picture         =   "Form2.frx":4156E
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   43
         Left            =   6120
         Picture         =   "Form2.frx":42870
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   44
         Left            =   120
         Picture         =   "Form2.frx":43B72
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   45
         Left            =   720
         Picture         =   "Form2.frx":44E74
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   46
         Left            =   1320
         Picture         =   "Form2.frx":46176
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   47
         Left            =   1920
         Picture         =   "Form2.frx":47478
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   48
         Left            =   2520
         Picture         =   "Form2.frx":4877A
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   49
         Left            =   3120
         Picture         =   "Form2.frx":49A7C
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   50
         Left            =   3720
         Picture         =   "Form2.frx":4AD7E
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   51
         Left            =   4320
         Picture         =   "Form2.frx":4C080
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   52
         Left            =   4920
         Picture         =   "Form2.frx":4D382
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   53
         Left            =   5520
         Picture         =   "Form2.frx":4E684
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   54
         Left            =   6120
         Picture         =   "Form2.frx":4F986
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   55
         Left            =   120
         Picture         =   "Form2.frx":50C88
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   56
         Left            =   720
         Picture         =   "Form2.frx":51F8A
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   57
         Left            =   1320
         Picture         =   "Form2.frx":5328C
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   58
         Left            =   1920
         Picture         =   "Form2.frx":5458E
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   59
         Left            =   2520
         Picture         =   "Form2.frx":55890
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   60
         Left            =   3120
         Picture         =   "Form2.frx":56B92
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   61
         Left            =   3720
         Picture         =   "Form2.frx":57E94
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   62
         Left            =   4320
         Picture         =   "Form2.frx":59196
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   63
         Left            =   4920
         Picture         =   "Form2.frx":5A498
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   64
         Left            =   5520
         Picture         =   "Form2.frx":5B79A
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   65
         Left            =   6120
         Picture         =   "Form2.frx":5CA9C
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   66
         Left            =   120
         Picture         =   "Form2.frx":5DD9E
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   67
         Left            =   720
         Picture         =   "Form2.frx":5F0A0
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   68
         Left            =   1320
         Picture         =   "Form2.frx":603A2
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   69
         Left            =   1920
         Picture         =   "Form2.frx":616A4
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   70
         Left            =   2520
         Picture         =   "Form2.frx":629A6
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   71
         Left            =   3120
         Picture         =   "Form2.frx":63CA8
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   72
         Left            =   3720
         Picture         =   "Form2.frx":64FAA
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   73
         Left            =   4320
         Picture         =   "Form2.frx":662AC
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   74
         Left            =   4920
         Picture         =   "Form2.frx":675AE
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   75
         Left            =   5520
         Picture         =   "Form2.frx":688B0
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   76
         Left            =   6120
         Picture         =   "Form2.frx":69BB2
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   77
         Left            =   120
         Picture         =   "Form2.frx":6AEB4
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   78
         Left            =   720
         Picture         =   "Form2.frx":6C1B6
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   79
         Left            =   1320
         Picture         =   "Form2.frx":6D4B8
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   80
         Left            =   1920
         Picture         =   "Form2.frx":6E7BA
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   81
         Left            =   2520
         Picture         =   "Form2.frx":6FABC
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   82
         Left            =   3120
         Picture         =   "Form2.frx":70DBE
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   83
         Left            =   3720
         Picture         =   "Form2.frx":720C0
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   84
         Left            =   4320
         Picture         =   "Form2.frx":733C2
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   85
         Left            =   4920
         Picture         =   "Form2.frx":746C4
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   86
         Left            =   5520
         Picture         =   "Form2.frx":759C6
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   87
         Left            =   6120
         Picture         =   "Form2.frx":76CC8
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   88
         Left            =   120
         Picture         =   "Form2.frx":77FCA
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   89
         Left            =   720
         Picture         =   "Form2.frx":792CC
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   90
         Left            =   1320
         Picture         =   "Form2.frx":7A5CE
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   91
         Left            =   1920
         Picture         =   "Form2.frx":7B8D0
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   92
         Left            =   2520
         Picture         =   "Form2.frx":7CBD2
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   93
         Left            =   3120
         Picture         =   "Form2.frx":7DED4
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   94
         Left            =   3720
         Picture         =   "Form2.frx":7F1D6
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   95
         Left            =   4320
         Picture         =   "Form2.frx":804D8
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   96
         Left            =   4920
         Picture         =   "Form2.frx":817DA
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   97
         Left            =   5520
         Picture         =   "Form2.frx":82ADC
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   98
         Left            =   6120
         Picture         =   "Form2.frx":83DDE
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   99
         Left            =   120
         Picture         =   "Form2.frx":850E0
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   100
         Left            =   720
         Picture         =   "Form2.frx":863E2
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   101
         Left            =   1320
         Picture         =   "Form2.frx":876E4
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   102
         Left            =   1920
         Picture         =   "Form2.frx":889E6
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   103
         Left            =   2520
         Picture         =   "Form2.frx":89CE8
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   104
         Left            =   3120
         Picture         =   "Form2.frx":8AFEA
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   105
         Left            =   3720
         Picture         =   "Form2.frx":8C2EC
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   106
         Left            =   4320
         Picture         =   "Form2.frx":8D5EE
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   107
         Left            =   4920
         Picture         =   "Form2.frx":8E8F0
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   108
         Left            =   5520
         Picture         =   "Form2.frx":8FBF2
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   109
         Left            =   6120
         Picture         =   "Form2.frx":90EF4
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   110
         Left            =   120
         Picture         =   "Form2.frx":921F6
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   111
         Left            =   720
         Picture         =   "Form2.frx":934F8
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   112
         Left            =   1320
         Picture         =   "Form2.frx":947FA
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   113
         Left            =   1920
         Picture         =   "Form2.frx":95AFC
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   114
         Left            =   2520
         Picture         =   "Form2.frx":96DFE
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   115
         Left            =   3120
         Picture         =   "Form2.frx":98100
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   116
         Left            =   3720
         Picture         =   "Form2.frx":99402
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   117
         Left            =   4320
         Picture         =   "Form2.frx":9A704
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   118
         Left            =   4920
         Picture         =   "Form2.frx":9BA06
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   119
         Left            =   5520
         Picture         =   "Form2.frx":9CD08
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   120
         Left            =   6120
         Picture         =   "Form2.frx":9E00A
         Top             =   6120
         Width           =   660
      End
   End
   Begin VB.Label Label17 
      BackStyle       =   0  'Transparent
      Caption         =   "MaxBombs"
      ForeColor       =   &H0000FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   26
      Top             =   8040
      Width           =   975
   End
   Begin VB.Label Label11 
      BackStyle       =   0  'Transparent
      Caption         =   "Title"
      ForeColor       =   &H0000FFFF&
      Height          =   255
      Left            =   240
      TabIndex        =   18
      Top             =   8520
      Width           =   975
   End
   Begin VB.Label Label8 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Current Tile"
      ForeColor       =   &H0000FFFF&
      Height          =   195
      Left            =   4680
      TabIndex        =   15
      Top             =   7200
      Width           =   810
   End
   Begin VB.Image Brs 
      BorderStyle     =   1  'Fixed Single
      Height          =   855
      Left            =   4560
      Top             =   7440
      Width           =   1095
   End
   Begin VB.Label Label6 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Oil"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   7920
      TabIndex        =   12
      Top             =   4320
      Width           =   180
   End
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   "Level Lives"
      ForeColor       =   &H0000FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   7680
      Width           =   2535
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Level Time (seconds)"
      ForeColor       =   &H0000FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   7200
      Width           =   2535
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub brs_DragDrop(Source As Control, X As Single, Y As Single)
On Error Resume Next
Brs.Picture = Source.Picture
Brs.Tag = Source.Tag
End Sub

Private Sub Command1_Click()
'Dim Y
'On Error Resume Next
'Y = InputBox("Whats the map's name???", "EXPORT", "NONAME")
'If Y = "" Then Exit Sub
'
'
'
'
'
'MkDir App.Path & "\mazes\" & Y
'
'Open App.Path & "\mazes\" & Y & "\" & "MAPNAME.nfm" For Output As #1
'Dim dyta
'dyta = Y
'Print #1, Y
'Close #1
'
'Open App.Path & "\mazes\" & Y & "\" & "MAPLIVES.nfm" For Output As #1
'Dim dyta2
'dyta2 = ll.Text
'Print #1, dyta2
'Close #1
'
'
'Dim e
'For e = 0 To 120
'Open App.Path & "\mazes\" & Y & "\tile" & e & ".ffm" For Output As #1
'Dim nfo
'nfo = tile(e).Tag
'Print #1, nfo
'Close #1
'Next e
'
'Open App.Path & "\mazes\" & Y & "\mazetime.ttm" For Output As #1
'Dim gh
'gh = tol.Text
'Print #1, gh
'Close #1
'MsgBox "Successful Export!", vbOKOnly, "YAY!"
Dim Y
    On Error Resume Next
    Y = InputBox("Whats the map's name???", "EXPORT", TxtTitle.Text)
    If Y = "" Then Exit Sub
    
    'MkDir App.Path & "\mazes\" & Y
    
    Open App.Path & "\mazes\" & Y & ".nfm" For Output As #1
    Dim dyta
    dyta = Y
    Print #1, Y
    'Close #1
    
    'Open App.Path & "\mazes\" & Y & "\" & "MAPLIVES.nfm" For Output As #1
    Dim dyta2
    dyta2 = ll.Text
    Print #1, dyta2
    'Close #1
    
    
    Dim e
    For e = 0 To 120
    'Open App.Path & "\mazes\" & Y & "\tile" & e & ".ffm" For Output As #1
    Dim nfo
    nfo = tile(e).Tag
    Print #1, nfo
    'Close #1
    Next e
    
    'Open App.Path & "\mazes\" & Y & "\mazetime.ttm" For Output As #1
    Dim gh
    gh = tol.Text
    Print #1, gh
    gh = MaxBombs.Text
    Print #1, gh
    Close #1
    MsgBox "Save Successful!", vbOKOnly, "YAY!"
End Sub

Private Sub Command2_Click()
Unload Me
Form3.Show
End Sub

Private Sub Command3_Click()
  MapLdr.Show
   Me.Hide
End Sub

Private Sub d2_Click()
'Me.Hide
'Form4.Show 1, Form2
 On Error Resume Next
 Brs.Picture = Form2.d2.Picture
 Brs.Tag = "start"
End Sub

Private Sub dropper_Click(Index As Integer)
On Error Resume Next
Brs.Picture = dropper(Index).Picture
Brs.Tag = dropper(Index).Tag
End Sub

Private Sub Form_Load()
Dim i
For i = 0 To 120
tile(i).Picture = dropper(0).Picture
tile(i).Tag = "dirt"
Next i

tile(110).Picture = d2.Picture
tile(110).Tag = "start"
Brs.Picture = dropper(0).Picture
End Sub

Private Sub tile_DragDrop(Index As Integer, Source As Control, X As Single, Y As Single)
On Error Resume Next
If tile(Index).Tag = "start" Then
MsgBox "You cannot draw over the start position!", vbCritical, "ACK!"
Exit Sub
End If

tile(Index).Picture = Source.Picture
tile(Index).Tag = Source.Tag



End Sub

Private Sub tile_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)

If Button = vbLeftButton Then
tile(Index).Picture = Brs.Picture
tile(Index).Tag = Brs.Tag
End If


If Button = vbRightButton Then
Brs.Picture = tile(Index).Picture
Brs.Tag = tile(Index).Tag
End If




End Sub







