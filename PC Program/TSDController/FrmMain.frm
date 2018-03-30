VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form FrmMain 
   BackColor       =   &H80000004&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "TSD Locomotive Controller （0.1）"
   ClientHeight    =   8595
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   10365
   DrawStyle       =   5  'Transparent
   BeginProperty Font 
      Name            =   "微软雅黑"
      Size            =   9
      Charset         =   134
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8595
   ScaleWidth      =   10365
   StartUpPosition =   2  '屏幕中心
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H80000008&
      Height          =   5415
      Left            =   120
      Picture         =   "FrmMain.frx":0000
      ScaleHeight     =   5385
      ScaleWidth      =   10065
      TabIndex        =   11
      Top             =   120
      Width           =   10095
   End
   Begin ComctlLib.Slider sliPower 
      Height          =   1935
      Left            =   2880
      TabIndex        =   9
      Top             =   5880
      Width           =   675
      _ExtentX        =   1191
      _ExtentY        =   3413
      _Version        =   327682
      Orientation     =   1
      Max             =   100
      SelStart        =   100
      TickStyle       =   3
      Value           =   100
   End
   Begin VB.Timer Timer2 
      Interval        =   1000
      Left            =   480
      Top             =   6360
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   480
      Top             =   6840
   End
   Begin VB.CommandButton cmdHigh 
      Caption         =   "照射灯"
      Height          =   495
      Left            =   6360
      Style           =   1  'Graphical
      TabIndex        =   8
      Top             =   6480
      Width           =   1215
   End
   Begin VB.CommandButton cmdMid 
      Caption         =   "提示灯"
      Height          =   495
      Left            =   6360
      Style           =   1  'Graphical
      TabIndex        =   7
      Top             =   7080
      Width           =   1215
   End
   Begin VB.CommandButton cmdOFF 
      BackColor       =   &H0000FF00&
      Caption         =   "灭灯"
      Height          =   495
      Left            =   6360
      Style           =   1  'Graphical
      TabIndex        =   6
      Top             =   7680
      Width           =   1215
   End
   Begin VB.CommandButton cmdEmer 
      BackColor       =   &H000000FF&
      Caption         =   "紧急制动"
      Height          =   495
      Left            =   5040
      Style           =   1  'Graphical
      TabIndex        =   5
      Top             =   7680
      Width           =   1215
   End
   Begin VB.CommandButton cmdLightBackward 
      Caption         =   "后照灯"
      Height          =   495
      Left            =   5040
      Style           =   1  'Graphical
      TabIndex        =   4
      Top             =   7080
      Width           =   1215
   End
   Begin VB.CommandButton cmdLightForward 
      BackColor       =   &H0000FF00&
      Caption         =   "前照灯"
      Height          =   495
      Left            =   5040
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   6480
      Width           =   1215
   End
   Begin VB.CommandButton cmdCheck 
      Caption         =   "检查连接"
      Height          =   495
      Left            =   5040
      TabIndex        =   2
      Top             =   5880
      Width           =   1215
   End
   Begin ComctlLib.Slider sliRes 
      Height          =   1935
      Left            =   3840
      TabIndex        =   10
      Top             =   5880
      Width           =   675
      _ExtentX        =   1191
      _ExtentY        =   3413
      _Version        =   327682
      Orientation     =   1
      Max             =   2
      SelStart        =   1
      TickStyle       =   2
      Value           =   1
   End
   Begin VB.Label lblRes 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "换向手柄"
      BeginProperty Font 
         Name            =   "微软雅黑"
         Size            =   10.5
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   300
      Left            =   3720
      TabIndex        =   1
      Top             =   7920
      Width           =   840
   End
   Begin VB.Label lblPower 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "功率手柄"
      BeginProperty Font 
         Name            =   "微软雅黑"
         Size            =   10.5
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   300
      Left            =   2640
      TabIndex        =   0
      Top             =   7920
      Width           =   840
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private aHttpRequest As WinHttp.WinHttpRequest
Private Declare Function CreateProcess Lib "kernel32" Alias "CreateProcessA" (ByVal lpApplicationName As String, ByVal lpCommandLine As String, lpProcessAttributes As SECURITY_ATTRIBUTES, lpThreadAttributes As SECURITY_ATTRIBUTES, ByVal bInheritHandles As Long, ByVal dwCreationFlags As Long, lpEnvironment As Any, ByVal lpCurrentDirectory As String, lpStartupInfo As STARTUPINFO, lpProcessInformation As PROCESS_INFORMATION) As Long
Private Declare Function CloseHandle Lib "kernel32.dll" (ByVal hObject As Long) As Long
Private Declare Function ReadFile Lib "kernel32" (ByVal hFile As Long, lpBuffer As Any, ByVal nNumberOfBytesToRead As Long, lpNumberOfBytesRead As Long, lpOverlapped As Long) As Long
Private Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Private Declare Function CreatePipe Lib "kernel32" (phReadPipe As Long, phWritePipe As Long, lpPipeAttributes As SECURITY_ATTRIBUTES, ByVal nSize As Long) As Long
Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Private Type STARTUPINFO
    cb                              As Long
    lpReserved                      As String
    lpDesktop                       As String
    lpTitle                         As String
    dwX                             As Long
    dwY                             As Long
    dwXSize                         As Long
    dwYSize                         As Long
    dwXCountChars                   As Long
    dwYCountChars                   As Long
    dwFillAttribute                 As Long
    dwFlags                         As Long
    wShowWindow                     As Integer
    cbReserved2                     As Integer
    lpReserved2                     As Long
    hStdInput                       As Long
    hStdOutput                      As Long
    hStdError                       As Long
End Type
Private Type PROCESS_INFORMATION
    hProcess                        As Long
    hThread                         As Long
    dwProcessId                     As Long
    dwThreadId                      As Long
End Type
Private Type SECURITY_ATTRIBUTES
    nLength                         As Long
    lpSecurityDescriptor            As Long
    bInheritHandle                  As Long
End Type
Private Const NORMAL_PRIORITY_CLASS  As Long = &H20&
Private Const STARTF_USESTDHANDLES   As Long = &H100&
Private Const STARTF_USESHOWWINDOW   As Long = &H1&
Private Const SW_HIDE                As Long = 0&
Private Const INFINITE               As Long = &HFFFF&
Private Function Cmd(commandline As String) As String
    Dim si As STARTUPINFO                                                       'used to send info the CreateProcess
    Dim pi As PROCESS_INFORMATION                                               'used to receive info about the created process
    Dim retval As Long                                                          'return value
    Dim hRead As Long                                                           'the handle to the read end of the pipe
    Dim hWrite As Long                                                          'the handle to the write end of the pipe
    Dim sBuffer(0 To 63) As Byte                                                'the buffer to store data as we read it from the pipe
    Dim lgSize As Long                                                          'returned number of bytes read by readfile
    Dim sa As SECURITY_ATTRIBUTES
    Dim strResult As String                                                     'returned results of the command line
    With sa
        .nLength = Len(sa)
        .bInheritHandle = 1&                                                    'inherit, needed for this to work
        .lpSecurityDescriptor = 0&
    End With
    retval = CreatePipe(hRead, hWrite, sa, 0&)
    If retval = 0 Then
        Cmd = ""
        Exit Function
    End If
    With si
        .cb = Len(si)
        .dwFlags = STARTF_USESTDHANDLES Or STARTF_USESHOWWINDOW                 'tell it to use (not ignore) the values below
        .wShowWindow = SW_HIDE
        .hStdOutput = hWrite                                                    'pass the write end of the pipe as the processes standard output
    End With
    retval = CreateProcess(vbNullString, commandline & vbNullChar, sa, sa, 1&, NORMAL_PRIORITY_CLASS, ByVal 0&, vbNullString, si, pi)
    If retval Then

        WaitForSingleObject pi.hProcess, INFINITE

        Do While ReadFile(hRead, sBuffer(0), 64, lgSize, ByVal 0&)
            strResult = strResult & StrConv(sBuffer(), vbUnicode)
            Erase sBuffer()
            If lgSize <> 64 Then Exit Do
            DoEvents
        Loop
        CloseHandle pi.hProcess
        CloseHandle pi.hThread
    End If
    CloseHandle hRead
    CloseHandle hWrite
    Cmd = Replace(strResult, vbNullChar, "")
End Function

Private Function JkGs2(Code As String, S1 As String, S2 As String, LastStr As String) As String 'è?μ?2?è·?¨×?·?μ?oó×o

Dim I As Long
I = InStr(1, Code, S1)
  If I > 0 Then
  I = I + Len(S1)
    I = InStr(I, Code, S2)
    If I > 0 Then
    I = I + Len(S2)
    JkGs2 = Mid(Code, I, InStr(I, Code, LastStr) - I)
    Else
    JkGs2 = "Nothing"
    End If
  Else
  JkGs2 = "Nothing"
  End If

End Function

Public Function GetWifiName() As String
  Dim Wifi As String
     Wifi = Cmd("netsh wlan show interfaces")
       If InStr(Wifi, "SSID") Then
              GetWifiName = JkGs2(Wifi, "SSID", ": ", vbCrLf)
           Else
              GetWifiName = "获取失败!"
       End If
End Function
Private Function GetResponse(ByVal XmlHttpMode$, ByVal XmlHttpURL$, ByVal XmlHttpData$)
    Dim MyXmlhttp
    On Error GoTo wrong
    Set MyXmlhttp = CreateObject("WinHttp.WinHttpRequest.5.1")                  '创建WinHttpRequest对象
    With MyXmlhttp
        .SetTimeouts 50000, 50000, 50000, 50000                                 '设置超时时间
        If XmlHttpMode = "GET" Then                                             '异步GET请求
            .Open "GET", XmlHttpURL, True
        Else
            .Open "POST", XmlHttpURL, True                                      '异步POST请求
            .SetRequestHeader "Content-Type", "application/x-www-form-urlencoded"
        End If
        .SetRequestHeader "Accept", "image/gif,image/x-xbitmap,image/jpeg,image/pjpeg,application/x-shockwave-flash,*/*"
        .SetRequestHeader "Referer", "https://passport.baidu.com/?login&tpl=mn"
        .SetRequestHeader "Accept-Language", "zh-cn"
        .SetRequestHeader "Accept-Encoding", "deflate"
        .SetRequestHeader "User-Agent", "Mozilla/4.0(compatible;MSIE6.0;WindowsNT5.1;SV1;.NETCLR2.0.50727)"

        .Send (XmlHttpData)
        .WaitForResponse                                                        '异步等待
        If MyXmlhttp.Status = 200 Then                                          '成功获取页面
            XMLHttpRequest = StrConv(.ResponseBody, vbUnicode)
        Else
            MsgBox "Http错误代码:" & .Status, vbInformation, "提示"
        End If
    End With
    Set MyXmlhttp = Nothing
    Exit Function
wrong:
    MsgBox "错误原因:" & err.Description & "", vbInformation, "提示"
    Set MyXmlhttp = Nothing
End Function


Private Sub cmdCheck_Click()
DoEvents
On Error GoTo err
    If GetWifiName <> "Locomotive@TSD" Then
        MsgBox "未连接到机车！", vbCritical + vbOKOnly, "检查连接"
    Else
        MsgBox "连接成功！", vbInformation + vbOKOnly, "检查连接"
    End If
    Sleep 1000
Exit Sub
err:
MsgBox "未连接到机车！", vbCritical + vbOKOnly, "检查连接"
End Sub

Private Sub cmdEmer_Click()
    DoEvents
    GetResponse "POST", "http://192.168.4.1/emergency", ""
    DoEvents
    sliRes.Value = 1
    sliPower.Value = 100
End Sub

Private Sub cmdHigh_Click()
    DoEvents
    cmdOFF.BackColor = &H8000000F
    cmdMid.BackColor = &H8000000F
    cmdHigh.BackColor = vbGreen
    GetResponse "POST", "http://192.168.4.1/lightmax", ""
    DoEvents
End Sub

Private Sub cmdLightBackward_Click()
    DoEvents
    cmdLightForward.BackColor = &H8000000F
    cmdLightBackward.BackColor = vbGreen
    Call cmdOFF_Click
    GetResponse "POST", "http://192.168.4.1/lightoff", ""
    DoEvents
    GetResponse "POST", "http://192.168.4.1/lightbackward", ""
    DoEvents
    GetResponse "POST", "http://192.168.4.1/lightoff", ""
    DoEvents
End Sub

Private Sub cmdLightForward_Click()
DoEvents
    cmdLightForward.BackColor = vbGreen
    cmdLightBackward.BackColor = &H8000000F
    Call cmdOFF_Click
    GetResponse "POST", "http://192.168.4.1/lightoff", ""
    DoEvents
    GetResponse "POST", "http://192.168.4.1/lightfoward", ""
    DoEvents
    GetResponse "POST", "http://192.168.4.1/lightoff", ""
    DoEvents
End Sub

Private Sub cmdMid_Click()
    DoEvents
    cmdOFF.BackColor = &H8000000F
    cmdMid.BackColor = vbGreen
    cmdHigh.BackColor = &H8000000F
    GetResponse "POST", "http://192.168.4.1/lightmid", ""
    DoEvents
End Sub

Private Sub cmdOFF_Click()
DoEvents
    cmdOFF.BackColor = vbGreen
    cmdMid.BackColor = &H8000000F
    cmdHigh.BackColor = &H8000000F
    GetResponse "POST", "http://192.168.4.1/lightoff", ""
End Sub


Private Sub Form_Load()
DoEvents
If GetWifiName <> "Locomotive@TSD" Then
    MsgBox "未连接机车！", vbCritical + vbOKOnly, "检查连接"
    End
End If
GetResponse "POST", "http://192.168.4.1/reset", ""
DoEvents
End Sub

Private Sub Form_Unload(Cancel As Integer)
End
End Sub

Private Sub sliPower_Change()
    DoEvents
    GetResponse "POST", "http://192.168.4.1/setpower?value=" & (100 - sliPower.Value), ""
    DoEvents
End Sub


Private Sub sliRes_Change()
DoEvents
    If sliRes.Value = 1 Then
        GetResponse "POST", "http://192.168.4.1/setneu", ""
        DoEvents
    ElseIf sliRes.Value = 2 Then
        GetResponse "POST", "http://192.168.4.1/setbackward", ""
        DoEvents
    ElseIf sliRes.Value = 0 Then
        GetResponse "POST", "http://192.168.4.1/setforward", ""
        DoEvents
    End If
End Sub


Private Sub Timer1_Timer()
DoEvents
If sliRes.Value = 1 Then
    sliPower.Value = 100
    sliPower.Enabled = False
Else
    sliPower.Enabled = True
End If
If sliPower.Value <> 100 Then
    sliRes.Enabled = False
Else
    sliRes.Enabled = True
End If
End Sub

Private Sub Timer2_Timer()
DoEvents
On Error Resume Next
    If GetWifiName <> "Locomotive@TSD" Then
        sliPower.Enabled = False
        sliRes.Enabled = False
        cmdHigh.Enabled = False
        cmdMid.Enabled = False
        cmdOFF.Enabled = False
        cmdLightForward.Enabled = False
        cmdLightBackward.Enabled = False
        cmdEmer.Enabled = False
    Else
        sliPower.Enabled = True
        sliRes.Enabled = True
        cmdHigh.Enabled = True
        cmdMid.Enabled = True
        cmdOFF.Enabled = True
        cmdLightForward.Enabled = True
        cmdLightBackward.Enabled = True
        cmdEmer.Enabled = True
    End If
End Sub
