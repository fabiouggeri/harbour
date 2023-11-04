/*
 * The Debugger Array Inspector
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject class */
#include "hbclass.ch"

#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBDbArray

   VAR oWindow
   VAR TheArray
   VAR arrayname
   VAR lEditable

   METHOD New( aArray, cVarName, lEditable )
   METHOD createWindow()
   METHOD doGet( nSet )
   METHOD SetsKeyPressed( nKey )

ENDCLASS

METHOD New( aArray, cVarName, lEditable ) CLASS HBDbArray

   ::arrayName := cVarName
   ::TheArray := aArray
   ::lEditable := hb_defaultValue( lEditable, .T. )
   ::createWindow()

   RETURN Self

METHOD createWindow() CLASS HBDbArray

   LOCAL nSize := Len( ::TheArray )
   LOCAL nWidth
   LOCAL nColWidth
   LOCAL oCol
   LOCAL nRow  := Row() + 1

   IF ( nsize < MaxRow() - 2 )
      nRow := GetTopPos( nRow )
      ::oWindow := HBDbWindow():New( nRow , 5, GetBottomPos( nRow + nSize + 1 ), MaxCol() - 5,;
                                    ::arrayName + "[1.." + AllTrim( Str( nsize, 6 ) ) + "]", "N/W" )
   ELSE
      ::oWindow := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5,;
                                    ::arrayName + "[1.." + AllTrim( Str( nsize, 6 ) ) + "]","N/W")
   ENDIF

   nWidth  := ::oWindow:nRight - ::oWindow:nLeft - 1
   ::oWindow:Browser := HBDbBrowser():New( ::oWindow:nTop + 1, ::oWindow:nLeft + 1, ::oWindow:nBottom - 1, ::oWindow:nRight - 1 )
   ::oWindow:Browser:autolite  := .f.
   ::oWindow:Browser:ColorSpec := __Dbg():ClrModal()
   ::oWindow:Browser:Cargo     := { 1, {} } 
   AAdd(::oWindow:Browser:Cargo[2],::TheArray)

   oCol := TBColumnNew("", { || ::arrayName+"["+alltrim(str(::oWindow:Browser:cargo[ 1 ],6))+"]"} )
   ::oWindow:Browser:AddColumn( oCol )
   ocol:width    := Len(::arrayName+"["+alltrim(str(len(::TheArray),6))+"]" )
   oCol:DefColor := { 1, 2 }
   nColWidth     := oCol:Width

   oCol := TBColumnNew( "" ,{ || PadR( __dbgValToExp( ::TheArray[::oWindow:Browser:cargo[ 1 ] ] ), nWidth - nColWidth - 1 ) } )
   ::oWindow:Browser:AddColumn( oCol )
   oCol:DefColor:={ 1, 3 }

   ::oWindow:Browser:GoTopBlock    := { || ::oWindow:Browser:cargo[ 1 ] := 1 }
   ::oWindow:Browser:GoBottomBlock := { || ::oWindow:Browser:cargo[ 1 ] := Len( ::oWindow:Browser:cargo[ 2, 1 ] ) }
   ::oWindow:Browser:SkipBlock     := { | nPos | nPos := ArrayBrowseSkip( nPos, ::oWindow:Browser ),;
                                                 ::oWindow:Browser:cargo[ 1 ] := ::oWindow:Browser:cargo[ 1 ] + nPos,; 
                                                 nPos }

   ::oWindow:bPainted    := { || ::oWindow:Browser:RefreshAll():forcestable(), RefreshVarsS( ::oWindow:Browser ) }
   ::oWindow:bKeyPressed := { | nKey | ::SetsKeyPressed( nKey ) }
   ::oWindow:Browser:colPos := 2
   SetCursor( SC_NONE )
   ::oWindow:ShowModal()

   RETURN Self

METHOD PROCEDURE doGet( nSet ) CLASS HBDbArray

   LOCAL nKey
   LOCAL nLenScrll  := ::oWindow:Browser:nRight - ::oWindow:Browser:nLeft - ::oWindow:Browser:GetColumn( 1 ):width
   LOCAL cValue     := __dbgValToExp( ::TheArray[ nSet ] )
   LOCAL lExit      := .F.
   LOCAL nRow
   LOCAL oError

   // make sure browse is stable
   ::oWindow:Browser:forcestable()

   nRow := row()

   WHILE ! lExit

      cValue  := PadR( cValue, Max( 255, Len( cValue ) ) )
      __dbgInput( nRow, ::oWindow:Browser:nLeft + ::oWindow:Browser:GetColumn( 1 ):width + 1,; 
                 nLenScrll, @cValue, __dbgExprValidBlock(), __Dbg():ClrModal(), 256 )

      lExit := .T.
      IF LastKey() == K_ENTER
         BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
            ::TheArray[ nSet ] = &cValue
         RECOVER USING oError
            lExit := .F.
            __dbgAlert( oError:description )
         END SEQUENCE
      ENDIF

   End

   // check exit key from get
   nKey := LastKey()
   IF nKey == K_UP .OR. nKey == K_DOWN .OR. nKey == K_PGUP .OR. nKey == K_PGDN
       __KEYBOARD( CHR( nKey ) )
   END

   RETURN

METHOD SetsKeyPressed( nKey ) CLASS HBDbArray

   LOCAL oBrw := ::oWindow:Browser
   LOCAL nSet := oBrw:cargo[1]
   LOCAL cTemp := Str( nSet, 4 )
   LOCAL nMRow
   LOCAL nMCol
   LOCAL n
   
   SWITCH nKey
      CASE K_UP
           oBrw:Up()
           Exit

      CASE K_DOWN
           oBrw:Down()
           Exit

      CASE K_HOME
      CASE K_CTRL_PGUP
      CASE K_CTRL_HOME
           oBrw:GoTop()
           Exit

      CASE K_END
      CASE K_CTRL_PGDN
      CASE K_CTRL_END
           oBrw:GoBottom()
           Exit

      CASE K_PGDN
           oBrw:pageDown()
           Exit

      CASE K_PGUP
           oBrw:PageUp()
           Exit

      CASE K_LBUTTONDOWN
      CASE K_LDBLCLK

           nMRow := MRow()
           nMCol := MCol()
           If ( nMRow >= oBrw:nTop  .And. nMRow <= oBrw:nBottom .And.;
                nMCol >= oBrw:nLeft .And. nMCol <= oBrw:nRight )

              n := oBrw:rowPos - ( nMRow - oBrw:nTop + 1 )
              while n > 0
                 oBrw:Up()
                 RefreshVarsS( oBrw )
                 n--
              end
              while n < 0
                 oBrw:Down()
                 RefreshVarsS( oBrw )
                 n++
              end

           EndIf
           If nKey == K_LBUTTONDOWN
              Exit
           EndIf

      CASE K_ENTER
         IF HB_ISARRAY( ::TheArray[nSet] )

            IF Len( ::TheArray[ nSet ] ) == 0
               __dbgAlert( "Array is empty" )
            ELSE
               oBrw:Hilite()
               __dbgArrays( ::TheArray[nSet], ::arrayname + "[" + AllTrim( cTemp ) + "]", ::lEditable )
            ENDIF

         ELSEIF HB_ISPOINTER( ::TheArray[ nSet ] ) .OR. HB_ISBLOCK( ::TheArray[ nSet ] ) .OR. ! ::lEditable

            Alert("Value cannot be edited")

         ELSE
            oBrw:RefreshCurrent()
            IF HB_ISOBJECT( ::TheArray[ nSet ] )
               __dbgObject( ::TheArray[ nSet ], ::arrayName + "[" + AllTrim( Str( nSet ) ) + "]", ::lEditable )
            ELSEIF HB_ISHASH( ::TheArray[ nSet ] )
               __dbgHashes( ::TheArray[ nSet ], ::arrayName + "[" + AllTrim( Str( nSet ) ) + "]" )
            ELSE
               ::doGet( nSet )
            ENDIF
            oBrw:RefreshCurrent()
            oBrw:ForceStable()

         ENDIF
         EXIT

   END

   RefreshVarsS( oBrw )

   RETURN Self

FUNCTION __dbgArrays( aArray, cVarName, lEditable )
   RETURN HBDbArray():New( aArray, cVarName, lEditable )

STATIC FUNCTION GetTopPos( nPos )
   RETURN iif( ( MaxRow() - nPos ) < 5, MaxRow() - nPos, nPos )

STATIC FUNCTION GetBottomPos( nPos )
   RETURN iif( nPos < MaxRow() - 2, nPos, MaxRow() - 2 )

STATIC FUNCTION ArrayBrowseSkip( nPos, oBrwSets )
   RETURN ;
      iif( oBrwSets:cargo[ 1 ] + nPos < 1, -oBrwSets:cargo[ 1 ] + 1, ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len( oBrwSets:cargo[ 2 ][ 1 ] ), ;
      Len( oBrwSets:cargo[ 2 ][ 1 ] ) - oBrwSets:cargo[ 1 ], nPos ) )

STATIC PROCEDURE RefreshVarsS( oBrowse )

   LOCAL nLen := oBrowse:ColCount

   oBrowse:refreshCurrent():forceStable()

   IF ( nLen == 2 )
      oBrowse:dehilite():colpos:=2
   ENDIF
   oBrowse:dehilite():forcestable()
   IF ( nLen == 2 )
      oBrowse:hilite():colpos:=1
   ENDIF
   oBrowse:hilite()

   RETURN
      