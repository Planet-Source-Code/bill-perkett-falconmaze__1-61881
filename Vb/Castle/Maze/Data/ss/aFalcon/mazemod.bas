Attribute VB_Name = "mazemod"
Option Explicit
Declare Function sndPlaySound Lib "winmm.dll" Alias "sndPlaySoundA" (ByVal lpszSoundName As String, ByVal uFlags As Long) As Long
Type tRect
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Declare Function IntersectRect Lib "user32" (lpDestRect As tRect, lpSrc1Rect As tRect, lpSrc2Rect As tRect) As Long

Global Const SND_ASYNC = &H1


Dim didit As Boolean
Public iDoorcnt As Integer
Public iDoorSave(200) As Integer
Public iOldLoc As Integer
'
Public iStartcnt As Integer
Public iStartSave(200) As Integer
Public iOldStartLoc As Integer
'
Public iKeycnt As Integer
Public iKeySave(200) As Integer
Public iOldKeyLoc As Integer
Public bWin As Boolean
'
'
Public iBootcnt As Integer
Public iBootSave(200) As Integer
Public iOldBootLoc As Integer
'
'
Public iBombcnt As Integer
Public iBombSave(200) As Integer
Public iBombMax As Integer
'
Public iBombTick As Integer
Public iLockcnt As Integer
Public iLockSave(200) As Integer
Public iOldLockLoc As Integer
Public iMyLife As Integer
Public iMyTime As Integer

Sub RanDoor()
  Dim k As Integer
  Dim n As Integer
  Dim i As Integer
   For k = 1 To iDoorcnt
    n = iDoorSave(k)
    Form1.tile(n).Tag = "dirt"
   Next
   For i = 1 To 50
     k = Rnd * (iDoorcnt * 100)
     n = k Mod iDoorcnt
     If iOldLoc <> iDoorSave(n + 1) Then
       Form1.tile(iDoorSave(n + 1)).Tag = "door"
       iOldLoc = iDoorSave(n + 1)
       Exit Sub
     End If
   Next
   Form1.tile(iDoorSave(n + 1)).Tag = "door"
     iOldLoc = iDoorSave(n + 1)
  End Sub
Sub RanStart()
  Dim k As Integer
  Dim n As Integer
  Dim i As Integer
   For k = 1 To iStartcnt
    n = iStartSave(k)
    Form1.tile(n).Tag = "dirt"
   Next
   For i = 1 To 50
     k = Rnd * (iStartcnt * 100)
     n = k Mod iStartcnt
     If iOldStartLoc <> iStartSave(n + 1) Then
       Form1.tile(iStartSave(n + 1)).Tag = "start"
       iOldStartLoc = iStartSave(n + 1)
       Exit Sub
     End If
   Next
    Form1.tile(iStartSave(n + 1)).Tag = "start"
    iOldStartLoc = iStartSave(n + 1)
  End Sub
Sub RanBoot()
  Dim k As Integer
  Dim n As Integer
  Dim i As Integer
   For k = 1 To iBootcnt
    n = iBootSave(k)
    Form1.tile(n).Tag = "dirt"
   Next
   For i = 1 To 50
     k = Rnd * (iBootcnt * 100)
     n = k Mod iBootcnt
     If iOldBootLoc <> iBootSave(n + 1) Then
       Form1.tile(iBootSave(n + 1)).Tag = "boots"
       iOldBootLoc = iBootSave(n + 1)
       Exit Sub
     End If
   Next
    Form1.tile(iBootSave(n + 1)).Tag = "boots"
    iOldBootLoc = iBootSave(n + 1)
  End Sub
 Sub RanBomb()
  Dim k As Integer
  Dim n As Integer
  Dim i As Integer
  Dim icnt As Integer
   For k = 1 To iBombcnt
    n = iBombSave(k)
    Form1.tile(n).Tag = "dirt"
   Next
   icnt = 0
   For i = 1 To 50
     k = Rnd * (iBombcnt * 100)
     n = k Mod iBombcnt
     If Form1.tile(iBombSave(n + 1)).Tag <> "bomb" Then
       Form1.tile(iBombSave(n + 1)).Tag = "bomb"
       icnt = icnt + 1
       If icnt = iBombMax Then Exit Sub
     End If
   Next
    Form1.tile(iBombSave(n + 1)).Tag = "bomb"
  End Sub
Sub RanKey()
  Dim k As Integer
  Dim n As Integer
  Dim i As Integer
   For k = 1 To iKeycnt
    n = iKeySave(k)
    Form1.tile(n).Tag = "dirt"
   Next
   For i = 1 To 50
     k = Rnd * (iKeycnt * 100)
     n = k Mod iKeycnt
     If iOldKeyLoc <> iKeySave(n + 1) Then
       Form1.tile(iKeySave(n + 1)).Tag = "key"
       iOldKeyLoc = iKeySave(n + 1)
       Exit Sub
     End If
   Next
    Form1.tile(iKeySave(n + 1)).Tag = "key"
    iOldKeyLoc = iKeySave(n + 1)
  End Sub
  Sub RanLock()
  Dim k As Integer
  Dim n As Integer
  Dim i As Integer
   For k = 1 To iLockcnt
    n = iLockSave(k)
    Form1.tile(n).Tag = "dirt"
   Next
   For i = 1 To 50
     k = Rnd * (iLockcnt * 100)
     n = k Mod iLockcnt
     If iOldLockLoc <> iLockSave(n + 1) Then
       Form1.tile(iLockSave(n + 1)).Tag = "lock"
       iOldLockLoc = iLockSave(n + 1)
       Exit Sub
     End If
   Next
    Form1.tile(iLockSave(n + 1)).Tag = "lock"
    iOldLockLoc = iLockSave(n + 1)
  End Sub
Sub uncover(tile_number As Integer)

If Form1.tile(tile_number).Tag = "dirt" Then Form1.tile(tile_number).Picture = Form2.dropper(0).Picture
If Form1.tile(tile_number).Tag = "door" Then Form1.tile(tile_number).Picture = Form2.dropper(7).Picture
If Form1.tile(tile_number).Tag = "wall" Then Form1.tile(tile_number).Picture = Form2.dropper(1).Picture
If Form1.tile(tile_number).Tag = "lava" Then Form1.tile(tile_number).Picture = Form2.dropper(6).Picture
If Form1.tile(tile_number).Tag = "hole" Then Form1.tile(tile_number).Picture = Form2.dropper(5).Picture
If Form1.tile(tile_number).Tag = "spikes" Then Form1.tile(tile_number).Picture = Form2.dropper(4).Picture
If Form1.tile(tile_number).Tag = "key" Then Form1.tile(tile_number).Picture = Form2.dropper(3).Picture
If Form1.tile(tile_number).Tag = "key" Then Form1.Key.Visible = True
If Form1.tile(tile_number).Tag = "lock" Then Form1.tile(tile_number).Picture = Form2.dropper(8).Picture
If Form1.tile(tile_number).Tag = "start" Then Form1.tile(tile_number).Picture = Form2.d2.Picture
If Form1.tile(tile_number).Tag = "boots" Then Form1.tile(tile_number).Picture = Form2.dropper(9).Picture
If Form1.tile(tile_number).Tag = "boots" Then Form1.Boots.Visible = True
If Form1.tile(tile_number).Tag = "bomb" Then Form1.tile(tile_number).Picture = Form2.dropper(10).Picture
If Form1.tile(tile_number).Tag = "bomb" And iBombTick = 0 Then Form1.TimeBomb.Enabled = True
End Sub
