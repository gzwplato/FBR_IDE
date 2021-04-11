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
