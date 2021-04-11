
'Enum SyntaxTypes
'    ColorComment = 0
'    ColorString = 1
'    ColorReserved = 2
'    ColorFuncObj = 3                                                                                                                                                                       
'    ColorDelimiter = 4
'    ColorNormal = 5
'End Enum

Declare Function FileOpen() as CWSTR
declare Function Read_File( byval cws as CWSTR ) as long
declare Function ParseWord(ByVal Word As String) As long  ' SyntaxTypes 'declare Function ParseWord(ByVal Word As String) As SyntaxTypes
Declare Sub ParseLine(ByVal s As String, rtb As hwnd, ByVal RTBPos As Long)
declare Sub Highlight(rtb As hwnd, SyntaxType As long, StartPos As Long, Length As Long) 'declare Sub Highlight(rtb As hwnd, SyntaxType As SyntaxTypes, StartPos As Long, Length As Long)
'declare function rtfText( byval hEdit as HWND, byval wszText as CWSTR, byval wszFontName as CWSTR, byval nFontSize as long, byval nFontEffect as long = 0, byval nFontColor as COLORREF = 0, Byval StartPos as Long, Byval Length as Long ) as Long
declare function Parse_Selected_Line ( hWindow as hwnd ) as long
Declare Function ParsePart( Byval hRichEdit as hwnd, byval IndexStart as long, Byval IndexEnd as long) as long

Dim shared IsLoading as long  'keep track of loading new file turning ' ColorNormal off  

' Global variable used to suppress parsing until the end of a series of
' changes. Or, in the Change event itself to prevent cascaded Change events.
'Dim shared mbInChange As Boolean

' assumes one character long comment
Const COMMENT = "'"

Const DELIMITER = " {}[]()"  'vbTab & " ,(){}[]-+*%/='~!&|\<>?:;."

' Space surrounding each word is significant. It allows searching on whole
' words. Note that these constant declares are long and could reach the line
' length limit of 1023 characters. If so, simply split to 2 constants and
' combine into a third constant with the appropriate name, Only use uppercase 
Dim shared RESERVED As String: RESERVED = " DIM LONG BOOLEAN IN DECLARE FUNCTION "     'Const RESERVED As String = " BOOLEAN IN " 
Const FUNC_OBJ As String = " ENTRY " 
Const KEYWORD_PAD As String = " "                  

' Keeping track of current and previous insertion point. Used to determine
' what portion of text has changed.
Dim shared mlPrevSelStart As Long
dim shared mlCurSelStart As Long

Dim shared LnPosition as Integer 

' ========================================================================================                  
'
' ========================================================================================
Function FileOpen() as CWSTR
          DIM wszFile AS WSTRING * 260 = "*.odl"   'Make sure the extension really exists with in wszFilter
          DIM wszInitialDir AS STRING * 260 = CURDIR   
          DIM wszFilter AS WSTRING * 260 = "odl files (*.odl)|*.odl|" & "All Files (*.*)|*.*|"
          DIM dwFlags AS DWORD = OFN_EXPLORER OR OFN_FILEMUSTEXIST OR OFN_HIDEREADONLY OR OFN_ALLOWMULTISELECT
          DIM cws AS CWSTR = AfxOpenFileDialog(frmMain.hWindow, "", wszFile, wszInitialDir, wszFilter, "odl", @dwFlags, NULL) 
           'if FileOpen = null then Function = 0 : Exit Function   'OpenFileReading( cws ) 
     Function = cws
End function

' ========================================================================================
'  
' General purpose, load the text into the richbox control 
' ========================================================================================
Function Read_File( byval cws as CWSTR ) as long 

   'test for file existence and extension type
   dim FullFileName As CWSTR, sCase as CWSTR
    'test for file existence 
       if AfxFileExists (cws) = false then exit function 'for now thats fine
    'test for existence file extension, chop off before backslash to be sure we are in the destination folder
       FullFileName = Mid(cws, InStrRev(cws, "\") + 1, Len(cws))
    'returns the full input string if no dot was found 
       sCase = Mid(FullFileName, InStrRev(FullFileName, ".") + 1, Len(FullFileName))
    'compare the input value with the output value, if they are the same there is no file extension.
       if sCase = FullFileName then 
        sCase = ""
   end if 
   
'        ? RichEdit_GetFirstVisibleLine( frmMain.RichEdit1.hWindow ) & " GetFirstVisibleLine"
'        ? RichEdit_GetLastVisibleLine ( frmMain.RichEdit1.hWindow ) & " GetLastVisibleLine"
   
   'select the file reader, based on the file extension sCase
   dim s As CWSTR, Index as long = 1, x as string 
'    select case sCase
    
'    Case "odl", "" 
    'Create an instance of the CTextStream class
       DIM pTxtStm AS CTextStream                   '  Open file as a text stream
       DIM cbsFile AS CBSTR = cws 
         pTxtStm.Open(cbsFile, IOMode_ForReading)   '  Read the file sequentially
            DO UNTIL pTxtStm.EOS
            x = pTxtStm.ReadLine 
            s = s & x & chr( 13, 10 )  
            Index = Index + 1
            LOOP
         pTxtStm.Close
           
           RichEdit_SetText( frmMain.RichEdit1.hWindow, "" )
           RichEdit_SetText( frmMain.RichEdit1.hWindow, s )   'makes it look faster
           afxDoEvents
           IsLoading = 1 'turning ColorNormal OFF speeding up load time

' used to make it look faster
'        if index > 85 then 
         
'           ParsePart( frmMain.RichEdit1.hWindow, 0, 85 )
'           afxDoevents 
'           ParsePart( frmMain.RichEdit1.hWindow, 86, index ) 
'        else
           ParsePart( frmMain.RichEdit1.hWindow, 0, index ) 
'        end if
           IsLoading = 0 'turning ColorNormal OFF speeding up load time

exit function

'   case "txt"    
      
'       dim x as string 'CWSTR
'       dim f as long: f = freefile
'         Open cws For Input As #f
'            do until eof(f)
'            line input #f, x
'            s = s & x & chr( 13, 10 ) ' vbCrLf 
'            Index = Index + 1            
'            loop
'         Close #f
     
'   end select

'    mbInChange = True
'    frmMain.RichEdit1.text = s    
'    frmMain.RichEdit1.Refresh
'    rtb_SelChange()
'    mbInChange = False
'    HighlightRefresh
''    PropertyChanged "Text"
'    Function = 1
  dim xi as long
dim i as Integer 
       Dim hWindow as HWND  
       Dim CursorPos As Long
       Dim CurrLine As Long
       Dim ChrsToStart As Long
'       dim oldEventMask as long
'       dim s as String  'CWSTR 
 
    if index > 85 then 
        i = 85
    else
'grote bestanden in stappen laden telkens 85? regels

        i = index 
    end if
    
    
   
  
'============================================================      
        hWindow = frmMain.RichEdit1.hWindow
            RichEdit_SetText( hWindow, s )             'this is for your eyes only, reduce flickering
    IsLoading = 1 'turning ColorNormal OFF speeding up load time
 AfxDoEvents

   'Prevent the control from raising any events.
       dim oldEventMask as long
        oldEventMask = SendMessage( hWindow, EM_SETEVENTMASK, 0, 0 )
               SendMessage( hWindow, EM_HIDESELECTION,1,0 )
'   'prevent textbox from repainting
        SendMessage(hWindow, WM_SETREDRAW, 0, 0)
    
    
          



    for xi = 0 to i 
   'number of chrs up to the current line
       ChrsToStart = SendMessage( hWindow, EM_LINEINDEX, xi, null )
   'Copies a line of text from a rich edit control.
       s = RichEdit_GetLine( hWindow, xi )
       s = Left(s,len(s) - 1 )  'CHR(0) ?
   'Parse the line and adapt the colors from a rich edit control.   
       ParseLine( s & " " , hWindow, ChrsToStart )  'rtfText function moves the caret and gives flickering  

   ' ? xi
    next
       SendMessage( hwindow, EM_SETSEL, 0, 0 )
'        LnPosition = 0
'       For i = 1 to index
'        strLine = AfxStrParse(s, i, chr( 13, 10 )) ' vbCrLf ) 
'        ParseLine(strLine, hwindow, LnPosition)
'        LnPosition = LnPosition + Len(strLine) + 1 
'       Next i
      
'   'allow texbox to repaint
       SendMessage(hWindow, WM_SETREDRAW, 1, 0)
'   'Allow the control to raise event messages.
       SendMessage( hWindow, EM_SETEVENTMASK, 0, oldEventMask )
       SendMessage( hWindow, EM_HIDESELECTION,0,0 )
'   'Signal the Control to be redrawn.
       InvalidateRect hWindow, 0, True
       UpdateWindow hWindow
 
   IsLoading = 0 'turning ColorNormal ON speeding up load time
    
   Function = 0
end function

'
' 
'
Function ParsePart( Byval hRichEdit as hwnd, byval IndexStart as long, Byval IndexEnd as long) as long

dim oldEventMask as long    
Dim xi as long 
dim s as CWSTR

Dim ChrsToStart As Long      
        'Prevent the control from raising any events.
        oldEventMask = SendMessage( hRichEdit, EM_SETEVENTMASK, 0, 0 )
     
        SendMessage( hRichEdit, EM_HIDESELECTION,1,0 )
        'prevent control from repainting
        SendMessage( hRichEdit, WM_SETREDRAW, 0, 0)
      
        'IndexStart
    for xi = IndexStart to IndexEnd 
        'number of chrs up to the current line
        ChrsToStart = SendMessage( hRichEdit, EM_LINEINDEX, xi, null )
        'Copies a line of text from a rich edit control.
        s = RichEdit_GetLine( hRichEdit, xi )
        s = Left(s,len(s) - 1 )  'CHR(0) ?
        'Parse the line and adapt the colors from a rich edit control.   
        ParseLine( s & " " , hRichEdit, ChrsToStart )  'rtfText function moves the caret and gives flickering  
    next
       SendMessage( hRichEdit, EM_SETSEL, 0, 0 )
'        LnPosition = 0
'       For i = 1 to index
'        strLine = AfxStrParse(s, i, chr( 13, 10 )) ' vbCrLf ) 
'        ParseLine(strLine, hwindow, LnPosition)
'        LnPosition = LnPosition + Len(strLine) + 1 
'       Next i
      
'   'allow control to repaint
       SendMessage( hRichEdit, WM_SETREDRAW, 1, 0)
'   'Allow the control to raise event messages.
       SendMessage( hRichEdit, EM_SETEVENTMASK, 0, oldEventMask )
       SendMessage( hRichEdit, EM_HIDESELECTION,0,0 )
'   'Signal the Control to be redrawn.
       InvalidateRect hRichEdit, 0, True
       UpdateWindow hRichEdit     
   Function = 0    
end function

' ========================================================================================
' 
' General purpose,  
' ========================================================================================
function Parse_Selected_Line ( Byval hEdit as hwnd ) as long
'   ? "Parse_Selected_Line"
'   ? "anti flickering code turned of for testing keystrokes"
'   ? ""
    
   'EM_GETMODIFY message
       Dim CursorPos As Long
       Dim CurrLine As Long
       Dim ChrsToStart As Long
       dim oldEventMask as long
       dim s as String  'CWSTR 
      
   'Prevent the control from raising any events.
'       oldEventMask = SendMessage( hEdit, EM_SETEVENTMASK, 0, 0 )
   'prevent textbox from repainting
       SendMessage( hEdit, WM_SETREDRAW, 0, 0 )
       SendMessage( hEdit, EM_HIDESELECTION,1,0 )
       
   'get the cursor position in the textbox
       SendMessage( hEdit, EM_GETSEL, NULL, cast(LPARAM, @CursorPos) ) 
   'get the current line index
       CurrLine = SendMessage( hEdit, EM_LINEFROMCHAR, CursorPos, -1 )
   'number of chrs up to the current line
       ChrsToStart = SendMessage( hEdit, EM_LINEINDEX, CurrLine, null )
   'Copies a line of text from a rich edit control.
       s = RichEdit_GetLine( hEdit, CurrLine )
       s = Left(s,len(s) - 1 )  'CHR(0) ?
   'Parse the line and adapt the colors from a rich edit control.   
       ParseLine( s & " " , hEdit, ChrsToStart )  'rtfText function moves the caret and gives flickering       
   'set the cursor to the orginal location and clear the selection length  
       SendMessage( hEdit, EM_SETSEL, CursorPos, CursorPos )         
         
   'Allow the control to raise event messages.
'       SendMessage( hEdit, EM_SETEVENTMASK, 0, oldEventMask ) 
   'allow texbox to repaint
       SendMessage( hEdit, WM_SETREDRAW, true, null )
       SendMessage( hEdit, EM_HIDESELECTION,0,0 )
   'Signal the Control to be redrawn.
       InvalidateRect hEdit, Null, True
      ' UpdateWindow hEdit
         
   Function = 0
end function
    ' Ret = rtfText(  hEdit , wszText , wszFontName , nFontSize , nFontEffect , nFontColor , StartPos , Length )

' ========================================================================================
' Lines are treated independently. Parseline is the main parsing code. Scan
' line from left to right, emitting text to be colored.
''Private Sub ParseLine(ByVal s As String, rtb As hwnd, ByVal RTBPos As Integer)  ' ByRef sender As wfxRichEdit, ByRef e As EventArgs
' ========================================================================================
Sub ParseLine(ByVal s As String, rtb As hwnd , ByVal RTBPos As Long)
    
   
    
    Dim bInString As Boolean    ' are we in a quoted string?
    bInString = False
    
    Dim bInWord As Boolean      ' are we in a word? (not a string, comment,
                                ' or delimiter)
    bInWord = False
    
    Dim sCurString As String        ' the current set of characters
    Dim lCurStringStart As Long     '   - where it starts
    Dim sCurChar As String          ' the current character
    
    Dim i As Long
    
    For i = 1 To Len(s)
        sCurChar = Mid(s, i, 1)
        If sCurChar = COMMENT Then   ' '
            ' if comment character occurs within a quoted string, it doesn't count
            If Not bInString Then
                ' this is a comment. we are done with the line
                If bInWord Then
                    ' before we encounterd the comment we were processing a word
                    Highlight rtb, ParseWord(sCurString), lCurStringStart + RTBPos - 1, i - lCurStringStart
                    sCurString = ""
                    bInWord = False
                End If
                Highlight rtb, 0, i + RTBPos - 1, Len(s) - i + 1           'ColorComment
                exit sub ' rest of line is comment
            End If
        End If
        
        If sCurChar = """" Then
            ' if not already in a string, then this quote begins a string
            ' otherwise, we are in a string, and this quote ends it
            If bInString Then
                sCurString = sCurString & sCurChar
                Highlight rtb, 1, lCurStringStart + RTBPos - 1, i - lCurStringStart + 1  'ColorString
                sCurString = ""
                bInString = False
            Else
                If bInWord Then
                    ' before we encounterd the string we were processing a word
                    Highlight rtb, ParseWord(sCurString), lCurStringStart + RTBPos - 1, i - lCurStringStart
                    sCurString = ""
                    bInWord = False
                End If
                 
                bInString = True
                sCurString = sCurChar
                lCurStringStart = i
            End If
            Continue For ' get next character
        End If
       
        If InStr(1, DELIMITER, sCurChar) > 0 Then
            If bInWord Then
                ' before we encounterd the delimiter we were processing a word
                Highlight rtb, ParseWord(sCurString), lCurStringStart + RTBPos - 1, i - lCurStringStart
                sCurString = ""
                bInWord = False
            End If
              
            Highlight rtb, 4, i + RTBPos - 1, 1   'ColorDelimiter = 4
            Continue For 
        End If
              
        If (Not bInWord) And (Not bInString) Then
            bInWord = True
            sCurString = sCurChar
            lCurStringStart = i
              
            Continue For 
        End If
            
        ' add current character to the "word" we are in the middle of
        sCurString = sCurString & sCurChar
    Next
    
    If bInString Then
        ' before we encounterd the end of the line we were processing a string
        Highlight rtb, 1, lCurStringStart + RTBPos - 1, i - lCurStringStart           'ColorString = 1
    ElseIf bInWord Then
        ' before we encounterd the end of the line we were processing a word
        Highlight rtb, ParseWord(sCurString), lCurStringStart + RTBPos - 1, i - lCurStringStart
    End If

End Sub

' ========================================================================================
' Function ParseWord
' Determine color for this word by checking for its existence in the keyword lists. 
' The word being checked it padded with spaces to prevent matches with substrings of keywords.
' first = InStr( [ start, ] str, [ Any ] substring )
' ========================================================================================
Private Function ParseWord( ByVal Word As String ) As long 'SyntaxTypes
                   Word = UCase ( Word )
      If InStr( 1, RESERVED, KEYWORD_PAD & Word & KEYWORD_PAD ) > 0 Then                     ' If InStr(1, RESERVED, KEYWORD_PAD & Word & KEYWORD_PAD, vbTextCompare)
        ParseWord = 2         'ColorReserved = 2
    ElseIf InStr( 1, FUNC_OBJ, KEYWORD_PAD & Word & KEYWORD_PAD ) > 0 Then                   ' ElseIf InStr(1, FUNC_OBJ, KEYWORD_PAD & Word & KEYWORD_PAD, vbTextCompare) > 0 Then
        ParseWord = 3  'ColorFuncObj
    Else
        ParseWord = 5 'ColorNormal
    End If
End Function

' ========================================================================================
' Color this range in the RichTextBox. Note that you could also apply bold,
' italic, etc. to the selection at the same time.
' ========================================================================================
Private Sub Highlight(hEdit As hwnd, SyntaxType As long, StartPos As Long, Length As Long)
     
    dim cf AS CHARFORMAT2
        cf.cbsize            = sizeof(cf)
        cf.dwMask            = CFM_COLOR 
     
    select case SyntaxType
          case 0             ' ColorComment
              cf.crTextColor = colors.Green 
          Case 1             ' ColorString
              cf.crTextColor = colors.DarkGoldenRod 
          case 2             ' ColorReserved
              cf.crTextColor = colors.Blue 
          case 3             ' ColorFuncObj
              cf.crTextColor = colors.Red 
          case 4             ' ColorDelimiter
              cf.crTextColor = colors.YellowGreen 
          case 5             ' ColorNormal
             if IsLoading = 1 then exit sub 'loading a new file no need to set black
              cf.crTextColor = colors.Black 
    end select
     
        Length = Length + StartPos
        SendMessage( hEdit, EM_SETSEL, StartPos, Length )  'this moves the caret and makes flicker  
        SendMessage( hEdit, EM_SETCHARFORMAT, SCF_SELECTION, cast(LPARAM, @cf) ) 


'redraw only singel line or some charters ?   EM_SETRECT  EM_GETRECT
'http://www.devx.com/vb2themax/Tip/18612
' getting garet position left down etc
end sub

' ========================================================================================
' Load_Text DEZE WERKT GOED 
' General purpose, load the text into the richbox control 
' ========================================================================================
'Function Read_File1( byval cws as CWSTR ) as long 

'   'test for file existence and extension type
'   dim FullFileName As CWSTR, sCase as CWSTR
'    'test for file existence 
'       if AfxFileExists (cws) = false then exit function 'for now thats fine
'    'test for existence file extension, chop off before backslash to be sure we are in the destination folder
'       FullFileName = Mid(cws, InStrRev(cws, "\") + 1, Len(cws))
'    'returns the full input string if no dot was found 
'       sCase = Mid(FullFileName, InStrRev(FullFileName, ".") + 1, Len(FullFileName))
'    'compare the input value with the output value, if they are the same there is no file extension.
'       if sCase = FullFileName then 
'        sCase = ""
'   end if 
   
''        ? RichEdit_GetFirstVisibleLine( frmMain.RichEdit1.hWindow ) & " GetFirstVisibleLine"
''        ? RichEdit_GetLastVisibleLine ( frmMain.RichEdit1.hWindow ) & " GetLastVisibleLine"
   
'   'select the file reader, based on the file extension sCase
'   dim s As CWSTR, Index as long = 1, x as string 
'    select case sCase
    
'    Case "odl", "" 
'    'Create an instance of the CTextStream class
'       DIM pTxtStm AS CTextStream                   '  Open file as a text stream
'       DIM cbsFile AS CBSTR = cws 
'         pTxtStm.Open(cbsFile, IOMode_ForReading)   '  Read the file sequentially
'            DO UNTIL pTxtStm.EOS
'            x = pTxtStm.ReadLine 
'            s = s & x & chr( 13, 10 )  
'            Index = Index + 1
'            LOOP
'         pTxtStm.Close
      
'   case "txt"    
      
'       dim x as string 'CWSTR
'       dim f as long: f = freefile
'         Open cws For Input As #f
'            do until eof(f)
'            line input #f, x
'            s = s & x & chr( 13, 10 ) ' vbCrLf 
'            Index = Index + 1            
'            loop
'         Close #f
     
'   end select

''    mbInChange = True
''    frmMain.RichEdit1.text = s    
''    frmMain.RichEdit1.Refresh
''    rtb_SelChange()
''    mbInChange = False
''    HighlightRefresh
'''    PropertyChanged "Text"
''    Function = 1
''exit function

'       Dim hWindow as HWND  
'        hWindow = frmMain.RichEdit1.hWindow
'   'Prevent the control from raising any events.
'       dim oldEventMask as long
'        oldEventMask = SendMessage( hWindow, EM_SETEVENTMASK, 0, 0 )
'   'prevent textbox from repainting
'        SendMessage(hWindow, WM_SETREDRAW, 0, 0)
    
'   'split the string s and feed it to the parser 
'       dim i as Integer, strLine as string    
'        RichEdit_SetText( hWindow, s ) 
'        LnPosition = 0
'       For i = 1 to index
'        strLine = AfxStrParse(s, i, chr( 13, 10 )) ' vbCrLf ) 
'        ParseLine(strLine, hwindow, LnPosition)
'        LnPosition = LnPosition + Len(strLine) + 1 
'       Next i
      
'   'allow texbox to repaint
'       SendMessage(hWindow, WM_SETREDRAW, 1, 0)
'   'Allow the control to raise event messages.
'       SendMessage( hWindow, EM_SETEVENTMASK, 0, oldEventMask )
'   'Signal the Control to be redrawn.
'       InvalidateRect hWindow, 0, True
'       UpdateWindow hWindow

    
'   Function = 0
'end function

' ========================================================================================
' Load_Text
' General purpose, load the text into the richbox control 
' ========================================================================================
'Function Read_File2( byval cws as CWSTR ) as long 

'   'test for file existence and extension type
'   dim FullFileName As CWSTR, sCase as CWSTR
'    'test for file existence 
'       if AfxFileExists (cws) = false then exit function 'for now thats fine
'    'test for existence file extension, chop off before backslash to be sure we are in the destination folder
'       FullFileName = Mid(cws, InStrRev(cws, "\") + 1, Len(cws))
'    'returns the full input string if no dot was found 
'       sCase = Mid(FullFileName, InStrRev(FullFileName, ".") + 1, Len(FullFileName))
'    'compare the input value with the output value, if they are the same there is no file extension.
'       if sCase = FullFileName then 
'        sCase = ""
'   end if 
   
'   'select the file reader, based on the file extension sCase
'   dim s As CWSTR, Index as long = 1, x as string 
'    select case sCase
    
'    Case "odl", "" 
'    'Create an instance of the CTextStream class
'       DIM pTxtStm AS CTextStream                   '  Open file as a text stream
'       DIM cbsFile AS CBSTR = cws 
'         pTxtStm.Open(cbsFile, IOMode_ForReading)   '  Read the file sequentially
'            DO UNTIL pTxtStm.EOS
'            x = pTxtStm.ReadLine 
'            s = s & x & chr( 13, 10 ) 'vbCrLf  
'            Index = Index + 1
'            LOOP
'         pTxtStm.Close
      
'   case "txt"    
      
'       dim x as string 'CWSTR
'       dim f as long: f = freefile
'         Open cws For Input As #f
'            do until eof(f)
'            line input #f, x
'            s = s & x & chr( 13, 10 ) 'vbCrLf 
'            Index = Index + 1            
'            loop
'         Close #f
     
'   end select

''    mbInChange = True
''    frmMain.RichEdit1.text = s    
''    frmMain.RichEdit1.Refresh
''    rtb_SelChange()
''    mbInChange = False
''    HighlightRefresh
'''    PropertyChanged "Text"
''    Function = 1
''exit function

'       Dim hWindow as HWND  
'        hWindow = frmMain.RichEdit1.hWindow
'   'Prevent the control from raising any events.
'       dim oldEventMask as long
'        oldEventMask = SendMessage( hWindow, EM_SETEVENTMASK, 0, 0 )
'   'prevent textbox from repainting
'        SendMessage(hWindow, WM_SETREDRAW, 0, 0)
    
'   'split the string s and feed it to the parser 
'       dim i as Integer, strLine as string    
'        RichEdit_SetText( hWindow, s ) 
'        LnPosition = 0
'       For i = 1 to index
'        strLine = AfxStrParse(s, i, chr( 13, 10 )) 'vbCrLf ) 
'        ParseLine(strLine, hwindow, LnPosition)
'        LnPosition = LnPosition + Len(strLine) + 1 
'       Next i
      
'   'allow texbox to repaint
'       SendMessage(hWindow, WM_SETREDRAW, 1, 0)
'   'Allow the control to raise event messages.
'       SendMessage( hWindow, EM_SETEVENTMASK, 0, oldEventMask )
'   'Signal the Control to be redrawn.
'       InvalidateRect hWindow, 0, True
'       UpdateWindow hWindow

    
'   Function = 0
'end function

'Function Parse_Selected_Line2 ( hwindow as hwnd) as long
'   ? ""
'   ? ""   
'   ' Copies a line of text from a rich edit control.
'' ========================================================================================
''PRIVATE FUNCTION RichEdit_GetLine (BYVAL hRichEdit AS HWND, BYVAL which AS DWORD) AS CWSTR
' 'retrieves either the line number of the current line (the line containing the caret) or, if there is a selection, the line number of the line containing the beginning of the selection.
    
' 'If this parameter is -1, EM_LINEFROMCHAR retrieves either the line number of the current line (the line containing the caret)  
'      Dim as long i = RichEdit_LineFromChar ( hwindow, -1 )  
'      dim as long p = RichEdit_LineIndex ( hWindow, i)
          
'       'Copies a line of text from a rich edit control.
'      dim as string s = RichEdit_GetLine ( hWindow, i )
'       s = Left(s,len(s) - 1 ) 
'       'Parse the line and adapt the colors from a rich edit control.   
'       ParseLine( s & " " , hWindow, p )    'rtfText function moves the caret 
'       ' SendMessage(hWindow, EM_SETSEL, p, p)   


'    Function = 0
'end function

'PRIVATE FUNCTION RichEdit_LineFromChar (BYVAL hRichEdit AS HWND, BYVAL index AS DWORD) AS LONG
'   FUNCTION = SendMessageW(hRichEdit, EM_LINEFROMCHAR, index, 0)
'END FUNCTION

' ========================================================================================
' Color this range in the RichTextBox. Note that you could also apply bold,
' italic, etc. to the selection at the same time.
' ========================================================================================
'Private Sub Highlight(hwindow As hwnd, SyntaxType As SyntaxTypes, StartPos As Long, Length As Long)
'      Select Case SyntaxType
'            Case SyntaxTypes.ColorComment
'                  rtfText( hwindow, "", "arial", 9, Null, colors.Green, StartPos, Length )
'            Case SyntaxTypes.ColorString
'                  rtfText( hwindow, "", "arial", 9, Null, colors.DarkGoldenRod, StartPos, Length )
'            Case SyntaxTypes.ColorReserved
'                  rtfText( hwindow, "", "arial", 9, Null, colors.Blue , StartPos, Length )
'            Case SyntaxTypes.ColorFuncObj
'                  rtfText( hwindow, "", "arial", 9, Null, colors.Red , StartPos, Length )
'            Case SyntaxTypes.ColorDelimiter
'                  rtfText( hwindow, "", "arial", 9, Null, colors.YellowGreen, StartPos, Length )
'            Case SyntaxTypes.ColorNormal
'                  rtfText( hwindow, "", "arial", 9, Null, 0, StartPos, Length ) 'colors.DarkMagenta, StartPos, Length )
'            case  Else
'    End Select
'End Sub

'' ========================================================================================
'' rtfText 'this tacking very long
'' General purpose routine to set the font, size, effect, color for text to be entered.
'' ========================================================================================
'function rtfText( byval hEdit as HWND, _
'                  byval wszText as CWSTR, _
'                  byval wszFontName as CWSTR, _
'                  byval nFontSize as long, _
'                  byval nFontEffect as long = 0, _
'                  byval nFontColor as COLORREF = 0, _
'                  Byval StartPos as Long, _
'                  Byval Length as Long _
'                  ) as Long
   
'   'exit function
              
'     Length = Length + StartPos 
  
'  ' dim hDC AS HDC
'   dim lRet AS LONG
'   dim cf AS CHARFORMAT2
'   'DIM tlf AS LOGFONTW
    
'   'dim stex as SETTEXTEX
'   'stex.flags = ST_SELECTION or ST_UNICODE

'  ' hDC = GetDC(NULL)
'  ' EnumFontFamiliesExW( hDC, byval wszFontName, cast(FONTENUMPROCW, @RichEdit_EnumFontFamProcW), cast(LPARAM, @tlf), Null )
'  ' ReleaseDC NULL, hDC

'   cf.cbsize          = sizeof(cf)
'   cf.dwMask          = CFM_COLOR 'or CFM_SIZE 'CFM_FACE  or CFM_UNDERLINE OR CFM_STRIKEOUT or CFM_COLOR OR CFM_CHARSET 'CFM_FACE or CFM_SIZE or CFM_BOLD OR CFM_ITALIC OR CFM_UNDERLINE OR CFM_STRIKEOUT or CFM_COLOR OR CFM_CHARSET
'   'cf.szFaceName      = tlf.lfFaceName
'   'cf.bCharSet        = tlf.lfCharSet
'   'cf.bPitchAndFamily = tlf.lfPitchAndFamily
'   'cf.yHeight         = nFontSize * 20
'   'cf.dwEffects       = nFontEffect
'   cf.crTextColor     = nFontColor
''cf.yOffset


'' dim P as CharRange
''         P.cpmin = StartPos
''         P.cpmax = Length
''         SendMessage(hEdit, EM_EXSetSel, 0, @P)

''EM_REPLACESEL
'   SendMessage( hEdit, EM_SETSEL, StartPos, Length )  ' SendMessage( hEdit, EM_SETSEL, Cast(WPARAM, StartPos), Cast(LPARAM, Length ) )  'this moves the caret and makes flicker
'    SendMessage( hEdit, EM_SETCHARFORMAT, SCF_SELECTION, cast(LPARAM, @cf) )
 
'''' ' ========================================================================================
''''' Sets character formatting in a rich edit control.
''''' ========================================================================================
''''PRIVATE FUNCTION RichEdit_SetCharFormat (BYVAL hRichEdit AS HWND, BYVAL chfmt AS DWORD, BYVAL pchfmt AS DWORD) AS LONG
''''   FUNCTION = SendMessageW(hRichEdit, EM_SETCHARFORMAT, chfmt, pchfmt)
''''END FUNCTION
 
 
'    'SendMessage( hEdit, EM_SETSEL, Cast(WPARAM, StartPos), Cast(LPARAM, Length ) )  'this moves the caret and makes flicker
'   'SendMessage( hEdit, EM_SETCHARFORMAT, SCF_SELECTION or SCF_WORD, cast(LPARAM, @cf) )
''   SendMessage( hEdit, EM_SETCHARFORMAT, SCF_SELECTION, cast(LPARAM, @cf) )
   
'   'print "StartPos  " & StartPos 
'   'Print "length    " & length
  
'  ' RichEdit_SetTextExW( hEdit, @stex, wszText )or SCF_WORD
  
'   'SendMessage( hEdit, EM_SETCHARFORMAT, (WPARAM)SCF_SELECTION|SCF_WORD, (LPARAM)&cf)
   
'   function = 0
'end function



