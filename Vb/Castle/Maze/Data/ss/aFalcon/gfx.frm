VERSION 5.00
Begin VB.Form gfx 
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Graphical Archive"
   ClientHeight    =   5265
   ClientLeft      =   45
   ClientTop       =   315
   ClientWidth     =   4095
   LinkTopic       =   "Form5"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5265
   ScaleWidth      =   4095
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   705
      Index           =   2
      Left            =   2280
      Picture         =   "gfx.frx":0000
      Tag             =   "door"
      Top             =   2760
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   0
      Left            =   3240
      Picture         =   "gfx.frx":1516
      Tag             =   "dirt"
      Top             =   240
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   1
      Left            =   3240
      Picture         =   "gfx.frx":2818
      Tag             =   "wall"
      Top             =   1920
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   4
      Left            =   3240
      Picture         =   "gfx.frx":3B1A
      Tag             =   "spikes"
      Top             =   4440
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   5
      Left            =   3240
      Picture         =   "gfx.frx":4E1C
      Tag             =   "hole"
      Top             =   3600
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   6
      Left            =   3240
      Picture         =   "gfx.frx":611E
      Tag             =   "lava"
      Top             =   2760
      Width           =   660
   End
   Begin VB.Image dropper 
      BorderStyle     =   1  'Fixed Single
      Height          =   660
      Index           =   7
      Left            =   3240
      Picture         =   "gfx.frx":7420
      Tag             =   "door"
      Top             =   1080
      Width           =   660
   End
   Begin VB.Image clt 
      Height          =   360
      Left            =   240
      Picture         =   "gfx.frx":8722
      Top             =   840
      Width           =   360
   End
   Begin VB.Image crt 
      Height          =   360
      Left            =   240
      Picture         =   "gfx.frx":B68C
      Top             =   1560
      Width           =   360
   End
   Begin VB.Image cup 
      Height          =   360
      Left            =   240
      Picture         =   "gfx.frx":E5F6
      Top             =   2280
      Width           =   360
   End
   Begin VB.Image cdn 
      Height          =   360
      Left            =   240
      Picture         =   "gfx.frx":11560
      Top             =   240
      Width           =   360
   End
End
Attribute VB_Name = "gfx"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
