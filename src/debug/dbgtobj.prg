/*
 * The Debugger Object Inspector
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

/* object message description */
#define OMSG_NAME       1
#define OMSG_VALUE      2
#define OMSG_EDIT       3

CREATE CLASS HBDbObject

   VAR oWindow
   VAR Theobj
   VAR objname
   VAR pItems         INIT {}
   VAR ArrayIndex     INIT 1
   VAR lEditable

   METHOD New( oObject, cVarName, lEditable )
   METHOD CreateWindow()
   METHOD doGet( oBrowse )
   METHOD SetsKeyPressed( nKey, nSets )

ENDCLASS

METHOD New( oObject, cVarName, lEditable ) CLASS HBDbObject

   LOCAL cMsg, cMsgAcc
   LOCAL aMessages, aMethods
   LOCAL xValue

   __dbgSetGo( __dbg():pInfo )

   /* create list of object messages */
   aMessages := oObject:classSel()
   ASort( aMessages,,, {| x, y | x + Chr( 0 ) < y + Chr( 0 ) } )
   aMethods := {}
   FOR EACH cMsg IN aMessages
      IF hb_LeftEq( cMsg, "_" ) .AND. ;
         hb_AScan( aMessages, cMsgAcc := SubStr( cMsg, 2 ),,, .T. ) > 0
         xValue := __dbgObjGetValue( oObject, cMsgAcc )
         AAdd( ::pItems, { cMsgAcc, xValue, .T. } )
      ELSEIF hb_AScan( aMessages, "_" + cMsg,,, .T. ) == 0
         AAdd( aMethods, cMsg )
      ENDIF
   NEXT
   FOR EACH cMsg IN aMethods
      AAdd( ::pItems, { Lower( cMsg ), "Method", .F. } )
   NEXT

   ::objname := cVarName
   ::TheObj := oObject
   ::lEditable := hb_defaultValue( lEditable, .T. )

   ::createWindow()

   RETURN Self

METHOD createWindow() CLASS HBDbObject

   LOCAL oBrwSets
   LOCAL nSize := Len( ::pItems )
   LOCAL oCol
   LOCAL nMaxLen

   IF nSize < MaxRow() - 2
      ::oWindow := HBDbWindow():New( 1, 5, 2 + nSize, MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
   ELSE
      ::oWindow := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
   ENDIF

   oBrwSets := HBDbBrowser():New( ::oWindow:nTop + 1, ::oWindow:nLeft + 1, ::oWindow:nBottom - 1, ::oWindow:nRight - 1 )
   ::oWindow:Browser := oBrwSets

   oBrwSets:ColorSpec := __dbg():ClrModal()
   oBrwSets:GoTopBlock := {|| ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := {|| ::arrayindex := Len( ::pItems ) }
   oBrwSets:SkipBlock := {| nSkip, nPos | nPos := ::arrayindex, ;
                                          ::arrayindex := Max( 1, Min( ::arrayindex + nSkip, Len( ::pItems ) ) ), ;
                                          ::arrayindex - nPos }

   nMaxLen := 0
   AEval( ::pItems, {| x | nMaxLen := Max( nMaxLen, Len( x[ OMSG_NAME ] ) ) } )
   oBrwSets:AddColumn( oCol := TBColumnNew( "", {|| PadR( ::pItems[ ::arrayindex, OMSG_NAME ], nMaxLen ) } ) )
   oCol:DefColor := { 1, 2 }
   oBrwSets:Freeze := 1

   oBrwSets:AddColumn( oCol := TBColumnNew( "", {|| iif( ! ::pItems[ ::ArrayIndex, OMSG_EDIT ], ;
                                                           ::pItems[ ::ArrayIndex, OMSG_VALUE ], ;
                                                           __dbgValToExp( __dbgObjGetValue( ::TheObj, ::pItems[ ::arrayindex, OMSG_NAME ] ) ) ) } ) )

   oCol:DefColor := { 1, 3 }
   oCol:width := ::oWindow:nRight - ::oWindow:nLeft - nMaxLen - 2
   oBrwSets:colPos := 2

   ::oWindow:bPainted    := {|| ::oWindow:Browser:RefreshAll():ForceStable(), RefreshVarsS( ::oWindow:Browser ) }
   ::oWindow:bKeyPressed := {| nKey | ::SetsKeyPressed( nKey, Len( ::pItems ) ) }
   ::oWindow:cCaption := ::objname + " is of class: " + ::TheObj:ClassName()

   ::oWindow:ShowModal()

   RETURN Self

METHOD PROCEDURE doGet( oBrowse ) CLASS HBDbObject
   LOCAL oErr
   LOCAL cValue
   LOCAL lCanAcc
   LOCAL aItemRef
   LOCAL lExit := .F.
   LOCAL nLenScrll  := ::oWindow:Browser:nRight - ::oWindow:Browser:nLeft - ::oWindow:Browser:GetColumn( 1 ):width

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank
   aItemRef := ::pItems[ ::ArrayIndex ]
   cValue := __dbgObjGetValue( ::TheObj, aItemRef[ OMSG_NAME ], @lCanAcc )
   IF ! lCanAcc
      __dbgAlert( cValue )
      RETURN
   ENDIF
   cValue := __dbgValToExp( cValue )

   WHILE ! lExit
      cValue  := PadR( cValue, Max( 255, Len( cValue ) ) )
      __dbgInput( Row(), oBrowse:nLeft + oBrowse:GetColumn( 1 ):width + 1, ;
                  nLenScrll, @cValue, __dbgExprValidBlock(), __Dbg():ClrModal(), 256 )
      lExit := .T.
      IF LastKey() == K_ENTER
         BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
            __dbgObjSetValue( ::TheObj, aItemRef[ OMSG_NAME ], &cValue )
         RECOVER USING oErr
            lExit := .F.
            __dbgAlert( oErr:description )
         END SEQUENCE
      ENDIF
   ENDDO

   RETURN

METHOD PROCEDURE SetsKeyPressed( nKey, nSets ) CLASS HBDbObject

   LOCAL aItemRef
   LOCAL oBrw := ::oWindow:Browser
   Local nMRow
   Local nMCol
   LOCAL n

   SWITCH nKey
      CASE K_UP
           IF ::ArrayIndex > 1
              oBrw:RefreshCurrent()
              oBrw:Up()
              oBrw:ForceStable()
           ENDIF
           EXIT

      CASE K_DOWN
           IF ::ArrayIndex < nSets
              oBrw:RefreshCurrent()
              oBrw:Down()
              oBrw:ForceStable()
           ENDIF
           EXIT

      CASE K_HOME
           IF ::ArrayIndex > 1
              oBrw:GoTop()
              oBrw:ForceStable()
           ENDIF
           EXIT

      CASE K_END
           IF ::ArrayIndex < nSets
              oBrw:GoBottom()
              oBrw:ForceStable()
           ENDIF
           EXIT

      CASE K_PGUP
           oBrw:PageUp()
           oBrw:RefreshCurrent()
           oBrw:ForceStable()
           EXIT

      CASE K_PGDN
           oBrw:PageDown()
           oBrw:RefreshCurrent()
           oBrw:ForceStable()
           EXIT

      CASE K_LBUTTONDOWN
      CASE K_LDBLCLK

           nMRow := MRow()
           nMCol := MCol()
           IF ( nMRow >= oBrw:nTop  .And. nMRow <= oBrw:nBottom .And.;
                nMCol >= oBrw:nLeft .And. nMCol <= oBrw:nRight )

              n := oBrw:rowPos - ( nMRow - oBrw:nTop + 1 )
              WHILE n > 0
                 oBrw:Up()
                 RefreshVarsS( oBrw )
                 n--
              ENDDO
              WHILE n < 0
                 oBrw:Down()
                 RefreshVarsS( oBrw )
                 n++
              ENDDO

           ENDIF

           IF nKey == K_LBUTTONDOWN
              EXIT
           ENDIF
      CASE K_ENTER

         aItemRef := ::pItems[ ::ArrayIndex ]
         DO CASE
            CASE HB_ISARRAY( aItemRef[ OMSG_VALUE ] )
               IF Len( aItemRef[ OMSG_VALUE ] ) > 0
                  oBrw:Hilite()
                  __dbgArrays( aItemRef[ OMSG_VALUE ], aItemRef[ OMSG_NAME ] )
               ELSE   
                  __dbgAlert( "Array is empty" )
               ENDIF
            CASE HB_ISHASH( aItemRef[ OMSG_VALUE ] )
               IF Len( aItemRef[ OMSG_VALUE ] ) > 0
                  oBrw:Hilite()
                  __dbgHashes( aItemRef[ OMSG_VALUE ], aItemRef[ OMSG_NAME ] )
               ENDIF
            CASE HB_ISOBJECT( aItemRef[ OMSG_VALUE ] )
               oBrw:Hilite()
               __dbgObject( aItemRef[ OMSG_VALUE ], aItemRef[ OMSG_NAME ] )
            CASE ! aItemRef[ OMSG_EDIT ] .OR. ;
                 HB_ISBLOCK( aItemRef[ OMSG_VALUE ] ) .OR. ;
                 HB_ISPOINTER( aItemRef[ OMSG_VALUE ] ) .OR. ;
                 ! ::lEditable
               __dbgAlert( "Value cannot be edited" )
            OTHERWISE
               oBrw:RefreshCurrent()
               ::doGet( oBrw )
               oBrw:RefreshCurrent()
               oBrw:ForceStable()
         ENDCASE

   ENDSWITCH

   RefreshVarsS( oBrw )

   RETURN

FUNCTION __dbgObject( oObject, cVarName, lEditable )
   RETURN HBDbObject():New( oObject, cVarName, lEditable )

STATIC FUNCTION __dbgObjGetValue( oObject, cVar, lCanAcc )

   LOCAL nProcLevel := __dbg():nProcLevel
   LOCAL xResult
   LOCAL oErr

   BEGIN SEQUENCE WITH {|| Break() }
      xResult := __dbgSendMsg( nProcLevel, oObject, cVar )
      lCanAcc := .T.
   RECOVER
      BEGIN SEQUENCE WITH __BreakBlock()
         /* Try to access variables using class code level */
         xResult := __dbgSendMsg( 0, oObject, cVar )
         lCanAcc := .T.
      RECOVER USING oErr
         xResult := oErr:description
         lCanAcc := .F.
      END SEQUENCE
   END SEQUENCE

   RETURN xResult

STATIC FUNCTION __dbgObjSetValue( oObject, cVar, xValue )

   LOCAL nProcLevel := __dbg():nProcLevel
   LOCAL oErr

   BEGIN SEQUENCE WITH {|| Break() }
      __dbgSendMsg( nProcLevel, oObject, "_" + cVar, xValue )
   RECOVER
      BEGIN SEQUENCE WITH __BreakBlock()
         /* Try to access variables using class code level */
         __dbgSendMsg( 0, oObject, "_" + cVar, xValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   END SEQUENCE

   RETURN xValue

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
