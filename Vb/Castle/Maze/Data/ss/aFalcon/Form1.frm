VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "FalconMaze"
   ClientHeight    =   7545
   ClientLeft      =   45
   ClientTop       =   315
   ClientWidth     =   8745
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7545
   ScaleWidth      =   8745
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer TimeBomb 
      Enabled         =   0   'False
      Interval        =   500
      Left            =   7200
      Top             =   5640
   End
   Begin VB.CommandButton CmdPlay 
      Caption         =   "Play Again    level may change"
      Height          =   615
      Left            =   7320
      TabIndex        =   28
      Top             =   2880
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.CommandButton Command8 
      Caption         =   "Command8"
      Height          =   375
      Left            =   7680
      TabIndex        =   27
      Top             =   7080
      Visible         =   0   'False
      Width           =   615
   End
   Begin VB.Timer TimeDone 
      Enabled         =   0   'False
      Interval        =   1800
      Left            =   7560
      Top             =   4920
   End
   Begin VB.CommandButton Command7 
      Caption         =   "Command7"
      Height          =   375
      Left            =   7680
      TabIndex        =   26
      Top             =   6600
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.Timer nrt 
      Enabled         =   0   'False
      Interval        =   70
      Left            =   8040
      Top             =   5640
   End
   Begin VB.Timer q2 
      Enabled         =   0   'False
      Interval        =   3000
      Left            =   8040
      Top             =   5160
   End
   Begin VB.Timer q3 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   7560
      Top             =   5640
   End
   Begin VB.Timer q1 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   7560
      Top             =   5520
   End
   Begin VB.CommandButton rs 
      Caption         =   "Restart Level   no changes"
      Height          =   615
      Left            =   7320
      TabIndex        =   20
      Top             =   2160
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Timer doom 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   8280
      Top             =   6000
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   8160
      Top             =   4440
   End
   Begin VB.Timer hurtimer 
      Interval        =   1
      Left            =   8280
      Top             =   1560
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Begin"
      Height          =   615
      Left            =   7320
      TabIndex        =   7
      Top             =   3600
      Width           =   1335
   End
   Begin VB.CommandButton Command4 
      BackColor       =   &H008080FF&
      Caption         =   ">"
      Enabled         =   0   'False
      Height          =   495
      Left            =   8160
      Style           =   1  'Graphical
      TabIndex        =   5
      Top             =   960
      Width           =   495
   End
   Begin VB.CommandButton Command3 
      BackColor       =   &H008080FF&
      Caption         =   "<"
      Enabled         =   0   'False
      Height          =   495
      Left            =   7200
      Style           =   1  'Graphical
      TabIndex        =   4
      Top             =   960
      Width           =   495
   End
   Begin VB.CommandButton Command2 
      BackColor       =   &H008080FF&
      Caption         =   "v"
      Enabled         =   0   'False
      Height          =   495
      Left            =   7680
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   1440
      Width           =   495
   End
   Begin VB.CommandButton Command1 
      BackColor       =   &H008080FF&
      Caption         =   "^"
      Enabled         =   0   'False
      Height          =   495
      Left            =   7680
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   480
      Width           =   495
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H00808080&
      Height          =   6975
      Left            =   120
      ScaleHeight     =   6915
      ScaleWidth      =   6915
      TabIndex        =   0
      Top             =   480
      Width           =   6975
      Begin VB.PictureBox cover 
         BackColor       =   &H00000000&
         Height          =   6975
         Left            =   0
         ScaleHeight     =   6915
         ScaleWidth      =   6915
         TabIndex        =   8
         Top             =   0
         Width           =   6975
         Begin VB.CommandButton Command6 
            Caption         =   "Try Again"
            Height          =   495
            Left            =   2520
            TabIndex        =   21
            Top             =   4920
            Visible         =   0   'False
            Width           =   1815
         End
         Begin VB.CommandButton sm 
            Caption         =   "Show entire maze"
            Height          =   495
            Left            =   2520
            TabIndex        =   1
            Top             =   4320
            Visible         =   0   'False
            Width           =   1815
         End
         Begin VB.Label dd 
            BackStyle       =   0  'Transparent
            Caption         =   "YOU LOST!"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   18
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H000000FF&
            Height          =   495
            Left            =   2640
            TabIndex        =   25
            Top             =   1320
            Visible         =   0   'False
            Width           =   1935
         End
         Begin VB.Label tu 
            BackStyle       =   0  'Transparent
            Caption         =   "TIME UP!"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   18
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H000000FF&
            Height          =   495
            Left            =   2640
            TabIndex        =   19
            Top             =   1920
            Visible         =   0   'False
            Width           =   1575
         End
         Begin VB.Label go 
            BackStyle       =   0  'Transparent
            Caption         =   "YOU WIN"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   18
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H000000FF&
            Height          =   495
            Left            =   2640
            TabIndex        =   10
            Top             =   2400
            Visible         =   0   'False
            Width           =   2175
         End
         Begin VB.Label Label2 
            BackStyle       =   0  'Transparent
            Caption         =   "START"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   18
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0000FF00&
            Height          =   495
            Left            =   2760
            TabIndex        =   9
            Top             =   3120
            Width           =   1335
         End
      End
      Begin VB.Image sens 
         BorderStyle     =   1  'Fixed Single
         Height          =   1335
         Left            =   1800
         Top             =   -1200
         Visible         =   0   'False
         Width           =   1335
      End
      Begin VB.Image u 
         BorderStyle     =   1  'Fixed Single
         Height          =   375
         Left            =   -120
         Top             =   -240
         Visible         =   0   'False
         Width           =   375
      End
      Begin VB.Image dude 
         Height          =   300
         Left            =   6840
         Picture         =   "Form1.frx":34CA
         Stretch         =   -1  'True
         Top             =   240
         Width           =   330
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   120
         Left            =   6120
         Picture         =   "Form1.frx":6434
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   119
         Left            =   5520
         Picture         =   "Form1.frx":7736
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   118
         Left            =   4920
         Picture         =   "Form1.frx":8A38
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   117
         Left            =   4320
         Picture         =   "Form1.frx":9D3A
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   116
         Left            =   3720
         Picture         =   "Form1.frx":B03C
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   115
         Left            =   3120
         Picture         =   "Form1.frx":C33E
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   114
         Left            =   2520
         Picture         =   "Form1.frx":D640
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   113
         Left            =   1920
         Picture         =   "Form1.frx":E942
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   112
         Left            =   1320
         Picture         =   "Form1.frx":FC44
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   111
         Left            =   720
         Picture         =   "Form1.frx":10F46
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   110
         Left            =   120
         Picture         =   "Form1.frx":12248
         Top             =   6120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   109
         Left            =   6120
         Picture         =   "Form1.frx":1354A
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   108
         Left            =   5520
         Picture         =   "Form1.frx":1484C
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   107
         Left            =   4920
         Picture         =   "Form1.frx":15B4E
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   106
         Left            =   4320
         Picture         =   "Form1.frx":16E50
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   105
         Left            =   3720
         Picture         =   "Form1.frx":18152
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   104
         Left            =   3120
         Picture         =   "Form1.frx":19454
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   103
         Left            =   2520
         Picture         =   "Form1.frx":1A756
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   102
         Left            =   1920
         Picture         =   "Form1.frx":1BA58
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   101
         Left            =   1320
         Picture         =   "Form1.frx":1CD5A
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   100
         Left            =   720
         Picture         =   "Form1.frx":1E05C
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   99
         Left            =   120
         Picture         =   "Form1.frx":1F35E
         Top             =   5520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   98
         Left            =   6120
         Picture         =   "Form1.frx":20660
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   97
         Left            =   5520
         Picture         =   "Form1.frx":21962
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   96
         Left            =   4920
         Picture         =   "Form1.frx":22C64
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   95
         Left            =   4320
         Picture         =   "Form1.frx":23F66
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   94
         Left            =   3720
         Picture         =   "Form1.frx":25268
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   93
         Left            =   3120
         Picture         =   "Form1.frx":2656A
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   92
         Left            =   2520
         Picture         =   "Form1.frx":2786C
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   91
         Left            =   1920
         Picture         =   "Form1.frx":28B6E
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   90
         Left            =   1320
         Picture         =   "Form1.frx":29E70
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   89
         Left            =   720
         Picture         =   "Form1.frx":2B172
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   88
         Left            =   120
         Picture         =   "Form1.frx":2C474
         Top             =   4920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   87
         Left            =   6120
         Picture         =   "Form1.frx":2D776
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   86
         Left            =   5520
         Picture         =   "Form1.frx":2EA78
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   85
         Left            =   4920
         Picture         =   "Form1.frx":2FD7A
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   84
         Left            =   4320
         Picture         =   "Form1.frx":3107C
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   83
         Left            =   3720
         Picture         =   "Form1.frx":3237E
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   82
         Left            =   3120
         Picture         =   "Form1.frx":33680
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   81
         Left            =   2520
         Picture         =   "Form1.frx":34982
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   80
         Left            =   1920
         Picture         =   "Form1.frx":35C84
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   79
         Left            =   1320
         Picture         =   "Form1.frx":36F86
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   78
         Left            =   720
         Picture         =   "Form1.frx":38288
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   77
         Left            =   120
         Picture         =   "Form1.frx":3958A
         Top             =   4320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   76
         Left            =   6120
         Picture         =   "Form1.frx":3A88C
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   75
         Left            =   5520
         Picture         =   "Form1.frx":3BB8E
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   74
         Left            =   4920
         Picture         =   "Form1.frx":3CE90
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   73
         Left            =   4320
         Picture         =   "Form1.frx":3E192
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   72
         Left            =   3720
         Picture         =   "Form1.frx":3F494
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   71
         Left            =   3120
         Picture         =   "Form1.frx":40796
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   70
         Left            =   2520
         Picture         =   "Form1.frx":41A98
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   69
         Left            =   1920
         Picture         =   "Form1.frx":42D9A
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   68
         Left            =   1320
         Picture         =   "Form1.frx":4409C
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   67
         Left            =   720
         Picture         =   "Form1.frx":4539E
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   66
         Left            =   120
         Picture         =   "Form1.frx":466A0
         Top             =   3720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   65
         Left            =   6120
         Picture         =   "Form1.frx":479A2
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   64
         Left            =   5520
         Picture         =   "Form1.frx":48CA4
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   63
         Left            =   4920
         Picture         =   "Form1.frx":49FA6
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   62
         Left            =   4320
         Picture         =   "Form1.frx":4B2A8
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   61
         Left            =   3720
         Picture         =   "Form1.frx":4C5AA
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   60
         Left            =   3120
         Picture         =   "Form1.frx":4D8AC
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   59
         Left            =   2520
         Picture         =   "Form1.frx":4EBAE
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   58
         Left            =   1920
         Picture         =   "Form1.frx":4FEB0
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   57
         Left            =   1320
         Picture         =   "Form1.frx":511B2
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   56
         Left            =   720
         Picture         =   "Form1.frx":524B4
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   55
         Left            =   120
         Picture         =   "Form1.frx":537B6
         Top             =   3120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   54
         Left            =   6120
         Picture         =   "Form1.frx":54AB8
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   53
         Left            =   5520
         Picture         =   "Form1.frx":55DBA
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   52
         Left            =   4920
         Picture         =   "Form1.frx":570BC
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   51
         Left            =   4320
         Picture         =   "Form1.frx":583BE
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   50
         Left            =   3720
         Picture         =   "Form1.frx":596C0
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   49
         Left            =   3120
         Picture         =   "Form1.frx":5A9C2
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   48
         Left            =   2520
         Picture         =   "Form1.frx":5BCC4
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   47
         Left            =   1920
         Picture         =   "Form1.frx":5CFC6
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   46
         Left            =   1320
         Picture         =   "Form1.frx":5E2C8
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   45
         Left            =   720
         Picture         =   "Form1.frx":5F5CA
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   44
         Left            =   120
         Picture         =   "Form1.frx":608CC
         Top             =   2520
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   43
         Left            =   6120
         Picture         =   "Form1.frx":61BCE
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   42
         Left            =   5520
         Picture         =   "Form1.frx":62ED0
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   41
         Left            =   4920
         Picture         =   "Form1.frx":641D2
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   40
         Left            =   4320
         Picture         =   "Form1.frx":654D4
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   39
         Left            =   3720
         Picture         =   "Form1.frx":667D6
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   38
         Left            =   3120
         Picture         =   "Form1.frx":67AD8
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   37
         Left            =   2520
         Picture         =   "Form1.frx":68DDA
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   36
         Left            =   1920
         Picture         =   "Form1.frx":6A0DC
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   35
         Left            =   1320
         Picture         =   "Form1.frx":6B3DE
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   34
         Left            =   720
         Picture         =   "Form1.frx":6C6E0
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   33
         Left            =   120
         Picture         =   "Form1.frx":6D9E2
         Top             =   1920
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   32
         Left            =   6120
         Picture         =   "Form1.frx":6ECE4
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   31
         Left            =   5520
         Picture         =   "Form1.frx":6FFE6
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   30
         Left            =   4920
         Picture         =   "Form1.frx":712E8
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   29
         Left            =   4320
         Picture         =   "Form1.frx":725EA
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   28
         Left            =   3720
         Picture         =   "Form1.frx":738EC
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   27
         Left            =   3120
         Picture         =   "Form1.frx":74BEE
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   26
         Left            =   2520
         Picture         =   "Form1.frx":75EF0
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   25
         Left            =   1920
         Picture         =   "Form1.frx":771F2
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   24
         Left            =   1320
         Picture         =   "Form1.frx":784F4
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   23
         Left            =   720
         Picture         =   "Form1.frx":797F6
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   22
         Left            =   120
         Picture         =   "Form1.frx":7AAF8
         Top             =   1320
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   21
         Left            =   6120
         Picture         =   "Form1.frx":7BDFA
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   20
         Left            =   5520
         Picture         =   "Form1.frx":7D0FC
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   19
         Left            =   4920
         Picture         =   "Form1.frx":7E3FE
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   18
         Left            =   4320
         Picture         =   "Form1.frx":7F700
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   17
         Left            =   3720
         Picture         =   "Form1.frx":80A02
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   16
         Left            =   3120
         Picture         =   "Form1.frx":81D04
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   15
         Left            =   2520
         Picture         =   "Form1.frx":83006
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   14
         Left            =   1920
         Picture         =   "Form1.frx":84308
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   13
         Left            =   1320
         Picture         =   "Form1.frx":8560A
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   12
         Left            =   720
         Picture         =   "Form1.frx":8690C
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   11
         Left            =   120
         Picture         =   "Form1.frx":87C0E
         Top             =   720
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   10
         Left            =   6120
         Picture         =   "Form1.frx":88F10
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   9
         Left            =   5520
         Picture         =   "Form1.frx":8A212
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   8
         Left            =   4920
         Picture         =   "Form1.frx":8B514
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   7
         Left            =   4320
         Picture         =   "Form1.frx":8C816
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   6
         Left            =   3720
         Picture         =   "Form1.frx":8DB18
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   5
         Left            =   3120
         Picture         =   "Form1.frx":8EE1A
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   4
         Left            =   2520
         Picture         =   "Form1.frx":9011C
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   3
         Left            =   1920
         Picture         =   "Form1.frx":9141E
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   2
         Left            =   1320
         Picture         =   "Form1.frx":92720
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   1
         Left            =   720
         Picture         =   "Form1.frx":93A22
         Top             =   120
         Width           =   660
      End
      Begin VB.Image tile 
         BorderStyle     =   1  'Fixed Single
         Height          =   660
         Index           =   0
         Left            =   120
         Picture         =   "Form1.frx":94D24
         Top             =   120
         Width           =   660
      End
   End
   Begin VB.Shape ShapeR 
      BackColor       =   &H000000FF&
      BackStyle       =   1  'Opaque
      Height          =   375
      Left            =   7200
      Top             =   6960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shapeo 
      BackColor       =   &H000080FF&
      BackStyle       =   1  'Opaque
      Height          =   735
      Left            =   7200
      Top             =   6600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape Shapey 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      Height          =   1095
      Left            =   7200
      Top             =   6240
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Image Boots 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Left            =   7560
      Picture         =   "Form1.frx":96026
      Top             =   6480
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.Image Key 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Left            =   7560
      Picture         =   "Form1.frx":97328
      Top             =   5640
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.Label Label9 
      BackColor       =   &H00000000&
      Caption         =   "Lives:"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7320
      TabIndex        =   24
      Top             =   4920
      Width           =   1215
   End
   Begin VB.Label lvs 
      BackColor       =   &H00000000&
      Caption         =   "Infinity"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7320
      TabIndex        =   23
      Top             =   5160
      Width           =   1215
   End
   Begin VB.Label mn 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      BorderStyle     =   1  'Fixed Single
      Caption         =   "No Name Maze"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   375
      Left            =   120
      TabIndex        =   22
      Top             =   120
      Width           =   6975
   End
   Begin VB.Label tm 
      BackColor       =   &H00000000&
      Caption         =   "0000"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7320
      TabIndex        =   18
      Top             =   4560
      Width           =   1215
   End
   Begin VB.Label Label7 
      BackColor       =   &H00000000&
      Caption         =   "Time:"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7320
      TabIndex        =   17
      Top             =   4320
      Width           =   1215
   End
   Begin VB.Label Label6 
      BackColor       =   &H00000000&
      Caption         =   "Hole"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   8880
      TabIndex        =   16
      Top             =   7800
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Label Label5 
      BackColor       =   &H00000000&
      Caption         =   "Door"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   8880
      TabIndex        =   15
      Top             =   8160
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Label Label4 
      BackColor       =   &H00000000&
      Caption         =   "Spikes"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   8880
      TabIndex        =   14
      Top             =   7440
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Label Label3 
      BackColor       =   &H00000000&
      Caption         =   "Lava"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   8880
      TabIndex        =   13
      Top             =   7080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Shape sdanger 
      BackColor       =   &H0000FF00&
      BackStyle       =   1  'Opaque
      Height          =   255
      Left            =   8520
      Top             =   7440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape hdanger 
      BackColor       =   &H0000FF00&
      BackStyle       =   1  'Opaque
      Height          =   255
      Left            =   8520
      Top             =   7800
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape ddanger 
      BackColor       =   &H0000FF00&
      BackStyle       =   1  'Opaque
      Height          =   255
      Left            =   8520
      Top             =   8160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Shape ldanger 
      BackColor       =   &H0000FF00&
      BackStyle       =   1  'Opaque
      Height          =   255
      Left            =   8280
      Top             =   7080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label t 
      BackColor       =   &H00000000&
      Caption         =   "0"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   8520
      TabIndex        =   12
      Top             =   7440
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Label l 
      BackColor       =   &H00000000&
      Caption         =   "0"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   8640
      TabIndex        =   11
      Top             =   7080
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Image ddd 
      Height          =   600
      Left            =   8400
      Picture         =   "Form1.frx":9862A
      Top             =   7680
      Width           =   600
   End
   Begin VB.Label Label1 
      BackColor       =   &H00000000&
      Caption         =   "Movement"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7560
      TabIndex        =   6
      Top             =   120
      Width           =   1335
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Function Collided(imgA As Image, imgB As Image) As Integer
On Error Resume Next
Dim a As tRect
Dim B As tRect
Dim ResultRect As tRect

    a.Left = imgA.Left
    a.Top = imgA.Top
    B.Left = imgB.Left
    B.Top = imgB.Top
 
     a.Right = a.Left + imgA.Width - 1
    a.Bottom = a.Top + imgA.Height - 1

    B.Right = B.Left + imgB.Width - 1
    B.Bottom = B.Top + imgB.Height - 1

    Collided = IntersectRect(ResultRect, a, B)
End Function

Sub gostart()
On Error Resume Next
If didit = True Then Exit Sub
Dim sp
For sp = 0 To 120
'go to start position
If tile(sp).Tag = "start" Then
        didit = True
        uncover (sp)
        u.Left = tile(sp).Left + 100
        u.Top = tile(sp).Top + 100
        dude.Top = tile(sp).Top + 110
        dude.Left = tile(sp).Left + 160
End If
Next sp
End Sub

Private Sub CmdPlay_Click()
On Error Resume Next
tm.Caption = iMyTime
lvs.Caption = iMyLife
Key.Visible = False
Boots.Visible = False
TimeBomb.Enabled = False
iBombTick = 0
bWin = False
Shapey.Visible = False
Shapeo.Visible = False
ShapeR.Visible = False
'
' Assign Random Door
'
If iDoorcnt > 0 Then
  Call RanDoor
End If
'
' Assign Random Start
'
If iStartcnt > 1 Then
  Call RanStart
End If
'
' Assign Random key
'
If iKeycnt > 1 Then
  Call RanKey
End If
'
' Assign Random lock
'
If iLockcnt > 1 Then
  Call RanLock
End If
'
' Assign Random boot
'
If iBootcnt > 1 Then
  Call RanBoot
End If
If iBombcnt > 1 Then
  Call RanBomb
End If
If ldr.o1.Value = True Then q1.Enabled = True
If ldr.o2.Value = True Then q2.Enabled = True
If ldr.Option1.Value = True Then q3.Enabled = True

dd.Visible = False
Picture1.SetFocus
Dim e2
For e2 = 0 To 120
tile(e2).Picture = ddd.Picture
Next e2

cover.Visible = False

On Error Resume Next
tu.Visible = False
'Open ldr.Dir1.Path & "\mazetime.ttm" For Input As #1
'Dim ty
'Input #1, ty
tm.Caption = iMyTime
'Close #1

doom.Enabled = True
Command1.Enabled = True
Command2.Enabled = True
Command3.Enabled = True
Command4.Enabled = True

'Open ldr.Dir1.Path & "\MAPLIVES.nfm" For Input As #1
'Dim yy3
'Input #1, yy3
'Close #1
lvs.Caption = iMyLife


Dim e
For e = 0 To 120
tile(e).Picture = ddd.Picture
Next e

gostart
End Sub

Private Sub Command1_Click()

If Command1.Enabled = False Then Exit Sub
If bWin Then Exit Sub
dude.Picture = gfx.cup.Picture
If cover.Visible = True Then Exit Sub

On Error Resume Next
Picture1.SetFocus
If dude.Top <= 270 Then Exit Sub

u.Top = u.Top - 602
Dim g
For g = 0 To 120

If Collided(u, tile(g)) And tile(g).Tag = "wall" Then
uncover (g)
u.Top = u.Top + 602
Exit Sub
End If

If Collided(u, tile(g)) And tile(g).Tag = "lock" And Key.Visible = False Then
uncover (g)
u.Top = u.Top + 602
Exit Sub
End If


If Collided(u, tile(g)) Then
uncover (g)
End If
Next g

dude.Top = dude.Top - 602

End Sub

Private Sub Command2_Click()

If Command1.Enabled = False Then Exit Sub
If bWin Then Exit Sub
dude.Picture = gfx.cdn.Picture
On Error Resume Next

If cover.Visible = True Then Exit Sub

Picture1.SetFocus
If dude.Top >= 6130 Then Exit Sub
u.Top = u.Top + 602
Dim g
For g = 0 To 120

If Collided(u, tile(g)) And tile(g).Tag = "wall" Then
uncover (g)
u.Top = u.Top - 602
Exit Sub
End If

If Collided(u, tile(g)) And tile(g).Tag = "lock" And Key.Visible = False Then
uncover (g)
u.Top = u.Top - 602
Exit Sub
End If
If Collided(u, tile(g)) Then
uncover (g)
End If
Next g

dude.Top = dude.Top + 602
End Sub

Private Sub Command3_Click()

If Command1.Enabled = False Then Exit Sub
If bWin Then Exit Sub
dude.Picture = gfx.clt.Picture
On Error Resume Next
If cover.Visible = True Then Exit Sub

Picture1.SetFocus
If dude.Left <= 310 Then Exit Sub
u.Left = u.Left - 602
Dim g
For g = 0 To 120

If Collided(u, tile(g)) And tile(g).Tag = "wall" Then
uncover (g)
u.Left = u.Left + 602
Exit Sub
End If


If Collided(u, tile(g)) And tile(g).Tag = "lock" And Key.Visible = False Then
uncover (g)
u.Left = u.Left + 602
Exit Sub
End If

If Collided(u, tile(g)) Then
uncover (g)
End If
Next g

dude.Left = dude.Left - 602
End Sub

Private Sub Command4_Click()

If Command1.Enabled = False Then Exit Sub
If bWin Then Exit Sub
dude.Picture = gfx.crt.Picture
On Error Resume Next
If cover.Visible = True Then Exit Sub

Picture1.SetFocus
If dude.Left >= 6100 Then Exit Sub
u.Left = u.Left + 602
Dim g
For g = 0 To 120

If Collided(u, tile(g)) And tile(g).Tag = "wall" Then
uncover (g)
u.Left = u.Left - 602
Exit Sub
End If

If Collided(u, tile(g)) And tile(g).Tag = "lock" And Key.Visible = False Then
uncover (g)
u.Left = u.Left - 602
Exit Sub
End If

If Collided(u, tile(g)) Then
uncover (g)
End If
Next g

dude.Left = dude.Left + 602
End Sub

Private Sub Command5_Click()
CmdPlay_Click
bWin = False
dd.Visible = False
TimeDone.Enabled = False
If ldr.o1.Value = True Then q1.Enabled = True
If ldr.o2.Value = True Then q2.Enabled = True
If ldr.Option1.Value = True Then q3.Enabled = True

On Error Resume Next
Picture1.SetFocus

Command5.Visible = False
CmdPlay.Visible = True
rs.Visible = True
On Error Resume Next
tu.Visible = False

doom.Enabled = True

'Start 'er up!
didit = False
go.Visible = False
Command1.Enabled = True
Command2.Enabled = True
Command3.Enabled = True
Command4.Enabled = True
cover.Visible = False

Dim e
For e = 0 To 120
tile(e).BorderStyle = 1
Next e



gostart

End Sub

Private Sub Command6_Click()
cover.Visible = False
dd.Visible = False
rs_Click
End Sub

Private Sub Command7_Click()
dude.Left = 9999
u.Left = 9999
cover.Visible = False
Dim t
For t = 0 To 120
uncover (t)
Next t
End Sub

Private Sub Command8_Click()
'
' Assign Random Door
'
 TimeBomb.Enabled = True
End Sub

Private Sub doom_Timer()
On Error Resume Next
tm = tm - 1
If tm < 1 Then
cover.Visible = True
rs.Visible = True
CmdPlay.Visible = True
Command5.Visible = False
Command5.Visible = True
Command1.Enabled = False
Command2.Enabled = False
Command3.Enabled = False
Command4.Enabled = False
tu.Visible = True
doom.Enabled = False
End If
End Sub

Private Sub Form_Load()
  Randomize
End Sub

Private Sub Form_Unload(Cancel As Integer)
ldr.Show
End Sub


Sub killd()
gostart

lvs = lvs - 1
If lvs < 0 Then

'    Open ldr.Dir1.Path & "\MAPLIVES.nfm" For Input As #1
'    Dim yyd
'    Input #1, yyd
'    Close #1
lvs.Caption = iMyLife

cover.Visible = True
rs.Visible = True
CmdPlay.Visible = True
Command5.Visible = False
Command5.Visible = True
Command1.Enabled = False
Command2.Enabled = False
Command3.Enabled = False
Command4.Enabled = False
dd.Visible = True
doom.Enabled = False
End If

End Sub



Private Sub hurtimer_Timer()

l.Caption = dude.Left
t.Caption = dude.Top

Dim o2
For o2 = 0 To 120

If Collided(u, tile(o2)) And tile(o2).Tag = "hole" Then killd: Exit Sub
If Collided(u, tile(o2)) And tile(o2).Tag = "lava" Then killd: Exit Sub
If Collided(u, tile(o2)) And tile(o2).Tag = "spikes" And Boots.Visible = False Then killd: Exit Sub


If Collided(u, tile(o2)) And tile(o2).Tag = "door" Then
 bWin = True
 TimeDone.Enabled = True
'cover.Visible = True
'go.Visible = True
'sm.Visible = True
'doom.Enabled = False
End If

If Collided(u, tile(o2)) And tile(o2).Tag = "lock" And Key.Visible Then
 bWin = True
 TimeDone.Enabled = True
'cover.Visible = True
'go.Visible = True
'sm.Visible = True
'doom.Enabled = False
End If

Next o2
'hurtimer.Enabled = False
End Sub

Private Sub nrt_Timer()
If ldr.o2.Value = True Then
Dim h
For h = 0 To 120
tile(h).Picture = Form4.sa.Picture
Next h
nrt.Enabled = False
q2.Enabled = True
End If
End Sub

Private Sub Picture1_KeyDown(KeyCode As Integer, Shift As Integer)
DoEvents
Select Case KeyCode
Case vbKeyUp
Command1.BackColor = &HC0FFC0
Command1_Click
Case vbKeyDown
Command2.BackColor = &HC0FFC0
Command2_Click
Case vbKeyLeft
Command3.BackColor = &HC0FFC0
Command3_Click
Case vbKeyRight
Command4.BackColor = &HC0FFC0
Command4_Click



End Select
End Sub

Private Sub Picture1_KeyUp(KeyCode As Integer, Shift As Integer)
DoEvents
Select Case KeyCode
Case vbKeyUp
Command1.BackColor = &H8080FF
Case vbKeyDown
Command2.BackColor = &H8080FF
Case vbKeyLeft
Command3.BackColor = &H8080FF
Case vbKeyRight
Command4.BackColor = &H8080FF
End Select
End Sub

Private Sub q2_Timer()

If ldr.o2.Value = True Then
Dim hx
For hx = 0 To 120
uncover (hx)
Next hx

nrt.Enabled = True
q2.Enabled = False
End If




End Sub

Private Sub q3_Timer()

Dim h3
For h3 = 0 To 120

If Collided(sens, tile(h3)) Then
uncover (h3)
Else:
tile(h3).Picture = Form4.sa.Picture
End If
Next h3



End Sub

Private Sub rs_Click()
On Error Resume Next
tm.Caption = iMyTime
lvs.Caption = iMyLife
Key.Visible = False
Boots.Visible = False
TimeBomb.Enabled = False
iBombTick = 0
bWin = False
Shapey.Visible = False
    Shapeo.Visible = False
    ShapeR.Visible = False
iBombTick = 0
'
' Assign Random Door
'
'If iDoorcnt > 0 Then
'  Call RanDoor
'End If
''
'' Assign Random Start
''
'If iStartcnt > 1 Then
'  Call RanStart
'End If

If ldr.o1.Value = True Then q1.Enabled = True
If ldr.o2.Value = True Then q2.Enabled = True
If ldr.Option1.Value = True Then q3.Enabled = True

dd.Visible = False
Picture1.SetFocus
Dim e2
For e2 = 0 To 120
tile(e2).Picture = ddd.Picture
Next e2

cover.Visible = False

On Error Resume Next
tu.Visible = False
'Open ldr.Dir1.Path & "\mazetime.ttm" For Input As #1
'Dim ty
'Input #1, ty
tm.Caption = iMyTime
'Close #1

doom.Enabled = True
Command1.Enabled = True
Command2.Enabled = True
Command3.Enabled = True
Command4.Enabled = True

'Open ldr.Dir1.Path & "\MAPLIVES.nfm" For Input As #1
'Dim yy3
'Input #1, yy3
'Close #1
lvs.Caption = iMyLife


Dim e
For e = 0 To 120
tile(e).Picture = ddd.Picture
Next e

gostart
End Sub

Private Sub TimeBomb_Timer()
  iBombTick = iBombTick + 1
  Shapey.Visible = False
  Shapeo.Visible = False
  ShapeR.Visible = False
   Select Case iBombTick
    Case 1, 3, 5
     Shapey.Visible = True
    Case 2, 4, 6
     Shapey.Visible = False
    Case 7, 9, 11
     Shapeo.Visible = True
    Case 8, 10, 12
     Shapeo.Visible = False
    Case 13, 15
     ShapeR.Visible = True
    Case 14, 4
     ShapeR.Visible = False
   End Select
   
  If iBombTick = 16 Then
    Call killd
    TimeBomb.Enabled = False
    iBombTick = 0
    ShapeR.Visible = False
  End If
End Sub

Private Sub TimeDone_Timer()
  cover.Visible = True
  go.Visible = True
  sm.Visible = True
  doom.Enabled = False
  bWin = False
  TimeDone.Enabled = False
End Sub

Private Sub Timer1_Timer()

sens.Left = dude.Left - 460
sens.Top = dude.Top - 460

Dim s

For s = 0 To 120

If Collided(sens, tile(s)) And tile(s).Tag = "lava" Then
ldanger.BackColor = vbRed
ElseIf Collided(sens, tile(s)) And tile(s).Tag <> "lava" Then
ldanger.BackColor = vbGreen
End If


If Collided(sens, tile(s)) And tile(s).Tag = "hole" Then
hdanger.BackColor = vbRed
ElseIf Collided(sens, tile(s)) And tile(s).Tag <> "hole" Then
hdanger.BackColor = vbGreen
End If


If Collided(sens, tile(s)) And tile(s).Tag = "spike" And Boots.Visible = False Then
sdanger.BackColor = vbRed
ElseIf Collided(sens, tile(s)) And tile(s).Tag <> "spike" Then
sdanger.BackColor = vbGreen
End If


If Collided(sens, tile(s)) And tile(s).Tag = "door" Then
ddanger.BackColor = vbRed
ElseIf Collided(sens, tile(s)) And tile(s).Tag <> "door" Then
ddanger.BackColor = vbGreen
End If

'If Collided(sens, tile(s)) And tile(s).Tag = "lock" And Key.Visible Then
'   ddanger.BackColor = vbRed
''ElseIf Collided(sens, tile(s)) And tile(s).Tag <> "door" Then
''ddanger.BackColor = vbGreen
'End If

Next s


End Sub

Private Sub sm_Click()
On Error Resume Next
TimeDone.Enabled = False
q1.Enabled = False
q2.Enabled = False
q3.Enabled = False

Command1.Enabled = False
Command2.Enabled = False
Command3.Enabled = False
Command4.Enabled = False


dude.Left = 9999
u.Left = 9999
cover.Visible = False
Dim t
For t = 0 To 120
uncover (t)
Next t
End Sub



Private Sub Timer4_Timer()

End Sub



