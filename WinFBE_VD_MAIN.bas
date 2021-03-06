#Define UNICODE
#Define _WIN32_WINNT &h0602
#Include Once "windows.bi"
#Include Once "Afx\CWindow.inc"
#define CODEGEN_FORM
#define CODEGEN_RICHEDIT
#define CODEGEN_TEXTBOX
#define CODEGEN_MAINMENU
#Include once "WinFormsX\WinFormsX.bi"
Using Afx

' WINFBE_CODEGEN_START

Declare Function frmMain_MainMenu_Click( ByRef sender As wfxMenuItem, ByRef e As EventArgs ) As LRESULT
Declare Function frmMain_MainMenu_Popup( ByRef sender As wfxMenuItem, ByRef e As EventArgs ) As LRESULT
Declare Function frmMain_AllEvents( ByRef sender As wfxForm, ByRef e As EventArgs ) As LRESULT
Declare Function frmMain_Load( ByRef sender As wfxForm, ByRef e As EventArgs ) As LRESULT
Declare Function frmMain_Resize( ByRef sender As wfxForm, ByRef e As EventArgs ) As LRESULT
Declare Function frmMain_RichEdit1_TextChanged( ByRef sender As wfxRichEdit, ByRef e As EventArgs ) As LRESULT

type frmMainType extends wfxForm
    private:
        temp as byte
    public:
        declare static function FormInitializeComponent( byval pForm as frmMainType ptr ) as LRESULT
        declare constructor
        ' Controls
        RichEdit1 As wfxRichEdit
end type


function frmMainType.FormInitializeComponent( byval pForm as frmMainType ptr ) as LRESULT
    dim as long nClientOffset

    dim ncm As NONCLIENTMETRICS
    ncm.cbSize = SizeOf(ncm)
    SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(ncm), @ncm, 0)
    nClientOffset = AfxUnScaleY(ncm.iMenuHeight)  ' holds the height of the mainmenu

    pForm->MainMenu.MenuItems.Clear
    pForm->MainMenu.Parent = pForm
    dim mnuFile as wfxMenuItem = wfxMenuItem("File", "mnuFile", "", 0, 0)
    dim mnuOpen as wfxMenuItem = wfxMenuItem("Open", "mnuOpen", "Ctrl+O", 0, 0)
    dim mnuSave as wfxMenuItem = wfxMenuItem("Save", "mnuSave", "Ctrl+S", 0, 0)
    dim mnuSaveAs as wfxMenuItem = wfxMenuItem("Save As", "mnuSaveAs", "Ctrl+Shift+S", 0, 0)
    dim mnuClear as wfxMenuItem = wfxMenuItem("Clear", "mnuClear", "Ctrl+C", 0, 0)
    dim mnuBuild as wfxMenuItem = wfxMenuItem("Build", "mnuBuild", "", 0, 0)
    dim mnuCompile as wfxMenuItem = wfxMenuItem("Compile", "mnuCompile", "Ctrl+Shift+C", 0, 0)
    dim mnuRun as wfxMenuItem = wfxMenuItem("Run", "mnuRun", "Ctrl+Shift+R", 0, 0)
    dim mnuHelp as wfxMenuItem = wfxMenuItem("Help", "mnuHelp", "", 0, 0)
    dim mnuFBDocs as wfxMenuItem = wfxMenuItem("FB Docs", "mnuFBDocs", "F1", 0, 0)
    mnuFile.MenuItems.Add(mnuOpen)
    mnuFile.MenuItems.Add(mnuSave)
    mnuFile.MenuItems.Add(mnuSaveAs)
    mnuFile.MenuItems.Add(mnuClear)
    pForm->MainMenu.MenuItems.Add(mnuFile)
    mnuBuild.MenuItems.Add(mnuCompile)
    mnuBuild.MenuItems.Add(mnuRun)
    pForm->MainMenu.MenuItems.Add(mnuBuild)
    mnuHelp.MenuItems.Add(mnuFBDocs)
    pForm->MainMenu.MenuItems.Add(mnuHelp)

    pForm->MainMenu.OnPopup = @frmMain_MainMenu_Popup
    pForm->MainMenu.OnClick = @frmMain_MainMenu_Click
    pForm->Controls.Add(ControlType.MainMenu, @(pForm->MainMenu))

    pForm->Name = "frmMain"
    pForm->Text = "FBR_IDE"
    pForm->SetBounds(10,10,835,616)
    pForm->OnAllEvents = @frmMain_AllEvents
    pForm->OnLoad = @frmMain_Load
    pForm->OnResize = @frmMain_Resize
    pForm->RichEdit1.Parent = pForm
    pForm->RichEdit1.Name = "RichEdit1"
    pForm->RichEdit1.AcceptsTab = True
    pForm->RichEdit1.AllowDrop = True
    pForm->RichEdit1.Font = New wfxFont("Segoe UI",14,FontStyles.Normal,FontCharset.Ansi)
    pForm->RichEdit1.TextScrollBars = ScrollBars.Both
    pForm->RichEdit1.SetBounds(3,32-nClientOffset,451,282)
    pForm->RichEdit1.OnTextChanged = @frmMain_RichEdit1_TextChanged
    pForm->Controls.Add(ControlType.RichEdit, @(pForm->RichEdit1))
    Application.Forms.Add(ControlType.Form, pForm)
    function = 0
end function

constructor frmMainType
    InitializeComponent = cast( any ptr, @FormInitializeComponent )
    this.FormInitializeComponent( @this )
end constructor

dim shared frmMain as frmMainType

' WINFBE_CODEGEN_END

' ========================================================================================
' WinFBE - FreeBASIC Editor (Windows 32/64 bit)
' Visual Designer auto generated project
' ========================================================================================

' Main application entry point.
' Place any additional global variables or #include files here.

' For your convenience, below are some of the most commonly used WinFBX library
' include files. Uncomment the files that you wish to use in the project or add
' additional ones. Refer to the WinFBX Framework Help documentation for information
' on how to use the various functions.

' #Include Once "Afx\AfxFile.inc"
' #Include Once "Afx\AfxStr.inc"
' #Include Once "Afx\AfxTime.inc"
' #Include Once "Afx\CIniFile.inc"
' #Include Once "Afx\CMoney.inc"
' #Include Once "Afx\CPrint.inc"
#include "afx/AfxFile.inc"
#include once "Parser.inc"

declare function GetCharIndexFromPoint(Byval hWindow as HWND, byval x as long, byval y as long) as long
declare Function RichEdit_GetLastVisibleLine (BYVAL hRichEdit AS HWND) AS LONG



SUB txtfile(f AS STRING, t as string)
	
'	DIM AS STRING buffer
	DIM h AS LONG = FREEFILE()
	OPEN f FOR binary AS #h
'	buffer = SPACE(LOF(h))
'while not eof(h)
	put #h ,  , t
'    print #h, text
'    buffer = buffer
	CLOSE #h
'	PRINT buffer
End SUB

function txtfileopen(f AS string) as string
	
'	reDIM AS STRING buffer(0)
    dim as string r
	DIM h AS LONG = FREEFILE()
'	OPEN f FOR input AS #h
	open f for binary as #h
    r = SPACE(LOF(h))
'while not eof(h)
    get #h, ,r
'    buffer = buffer & r & !"\r\n"
'    sappend(buffer(), r)
'    wend
	CLOSE #h
    return r
End function




Application.Run(frmMain)

#include once "D:\repo\FBR_IDE\frmMain.inc"

