/*
 * The Debugger Work Area Inspector
 *
 * Copyright 2001-2002 Ignacio Ortiz de Zuniga <ignacio@fivetech.com>
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

#include "box.ch"
#include "dbstruct.ch"
#include "setcurs.ch"
#include "inkey.ch"

REQUEST FieldGet

PROCEDURE __dbgShowWorkAreas()

   LOCAL oDlg
   LOCAL oCol
   LOCAL aAlias   := {}
   LOCAL aBrw[ 3 ]
   LOCAL aStruc
   LOCAL aInfo
   LOCAL cColor   := iif( __dbg():lMonoDisplay, "N/W, W/N, W+/W, W+/N", "N/W, N/BG, R/W, R/BG" )
   LOCAL cur_id
   LOCAL nOldArea := Select()
   LOCAL nCursor
   LOCAL nFocus   := 1

   hb_WAEval( {|| AAdd( aAlias, { Select(), Alias() } ) } )

   IF Len( aAlias ) == 0
      __dbgAlert( "No workareas in use" )
      RETURN
   ENDIF

   cur_id := AScan( aAlias, {| x | x[ 1 ] == nOldArea } )
   IF cur_id == 0
      cur_id := 1
      dbSelectArea( aAlias[ 1 ][ 1 ] )
   ENDIF

   /* Window creation */

   oDlg := HBDbWindow():New( 2, 3, 21, 76, "", cColor )

   oDlg:bKeyPressed := {| nKey | DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, @aStruc, @aInfo, @nFocus ) }
   oDlg:bPainted    := {|| DlgWorkAreaPaint( oDlg, aBrw ) }

   /* Alias browse */

   aBrw[ 1 ] := HBDbBrowser():new( oDlg:nTop + 1, oDlg:nLeft + 1, oDlg:nBottom - 1, oDlg:nLeft + 11 )
   aBrw[ 1 ]:Autolite      := .T.
   aBrw[ 1 ]:Cargo         := cur_id
   aBrw[ 1 ]:ColorSpec     := oDlg:cColor
   aBrw[ 1 ]:GoTopBlock    := {|| aBrw[ 1 ]:Cargo := 1 }
   aBrw[ 1 ]:GoBottomBlock := {|| aBrw[ 1 ]:Cargo := Len( aAlias ) }
   aBrw[ 1 ]:SkipBlock     := {| nSkip, nPos | nPos := aBrw[ 1 ]:Cargo, ;
                                               aBrw[ 1 ]:Cargo := iif( nSkip > 0, Min( Len( aAlias ), aBrw[ 1 ]:Cargo + nSkip ), ;
                                               Max( 1, aBrw[ 1 ]:Cargo + nSkip ) ), ;
                                               aBrw[ 1 ]:Cargo - nPos }

   aBrw[ 1 ]:AddColumn( oCol := TBColumnNew( "", {|| PadR( aAlias[ aBrw[ 1 ]:Cargo ][ 2 ], 11 ) } ) )

   oCol:ColorBlock := {|| iif( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] == nOldArea, { 3, 4 }, { 1, 2 } ) }

   /* Info Browse */

   aInfo := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( DbfInfo( nOldArea ) )

   aBrw[ 2 ] := HBDbBrowser():new( oDlg:nTop + 7, oDlg:nLeft + 13, oDlg:nBottom - 1, oDlg:nLeft + 52 )

   aBrw[ 2 ]:Autolite      := .F.
   aBrw[ 2 ]:Cargo         := 1
   aBrw[ 2 ]:ColorSpec     := oDlg:cColor
   aBrw[ 2 ]:GoTopBlock    := {|| aBrw[ 2 ]:Cargo := 1 }
   aBrw[ 2 ]:GoBottomBlock := {|| aBrw[ 2 ]:Cargo := Len( aInfo ) }
   aBrw[ 2 ]:SkipBlock     := {| nSkip, nPos | nPos := aBrw[ 2 ]:Cargo, ;
                                               aBrw[ 2 ]:Cargo := iif( nSkip > 0, Min( Len( aInfo ), aBrw[ 2 ]:Cargo + nSkip ), ;
                                               Max( 1, aBrw[ 2 ]:Cargo + nSkip ) ), ;
                                               aBrw[ 2 ]:Cargo - nPos }

   aBrw[ 2 ]:AddColumn( oCol := TBColumnNew( "", {|| PadR( aInfo[ aBrw[ 2 ]:Cargo, 2 ], 40 ) } ) )

   oCol:ColorBlock := {|| iif( aInfo[ aBrw[ 2 ]:Cargo, 1 ], { 3, 4 }, { 1, 2 } ) }

   /* Structure browser */

   aStruc := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( dbStruct() )

   aBrw[ 3 ] := HBDbBrowser():new( oDlg:nTop + 1, oDlg:nLeft + 54, oDlg:nBottom - 1, oDlg:nLeft + 72 )

   aBrw[ 3 ]:Autolite      := .F.
   aBrw[ 3 ]:Cargo         := 1
   aBrw[ 3 ]:ColorSpec     := oDlg:cColor
   aBrw[ 3 ]:GoTopBlock    := {|| aBrw[ 3 ]:Cargo := 1 }
   aBrw[ 3 ]:GoBottomBlock := {|| aBrw[ 3 ]:Cargo := Len( aStruc ) }
   aBrw[ 3 ]:SkipBlock     := {| nSkip, nPos | nPos := aBrw[ 3 ]:Cargo, ;
                                               aBrw[ 3 ]:Cargo := iif( nSkip > 0, Min( Len( aStruc ), aBrw[ 3 ]:Cargo + nSkip ), ;
                                               Max( 1, aBrw[ 3 ]:Cargo + nSkip ) ), aBrw[ 3 ]:Cargo - nPos }

   aBrw[ 3 ]:AddColumn( TBColumnNew( "", {|| PadR( aStruc[ aBrw[ 3 ]:Cargo ][ DBS_NAME ], 10 ) + " " + ;
                                             PadR( aStruc[ aBrw[ 3 ]:Cargo ][ DBS_TYPE ], 1 ) + " " + ;
                                             Str( aStruc[ aBrw[ 3 ]:Cargo ][ DBS_LEN ], 3 ) + " " + ;
                                             Str( aStruc[ aBrw[ 3 ]:Cargo ][ DBS_DEC ], 2 ) } ) )

   /* Show dialog */

   nCursor := SetCursor( SC_NONE )
   oDlg:ShowModal()
   SetCursor( nCursor )

   dbSelectArea( nOldArea )

   RETURN

STATIC PROCEDURE DlgWorkAreaPaint( oDlg, aBrw )

   DispBegin()
   /* Display captions */
   hb_DispOutAt( oDlg:nTop, oDlg:nLeft + 5, " Area ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop, oDlg:nLeft + 28, " Status ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop, oDlg:nLeft + 56, " Structure ", oDlg:cColor )

   /* Display separator lines */

   hb_DispBox( oDlg:nTop + 1, oDlg:nLeft + 12, oDlg:nBottom - 1, oDlg:nLeft + 12, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop, oDlg:nLeft + 12, hb_UTF8ToStrBox( "┬" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nBottom, oDlg:nLeft + 12, hb_UTF8ToStrBox( "┴" ), oDlg:cColor )

   hb_DispBox( oDlg:nTop + 1, oDlg:nLeft + 53, oDlg:nBottom - 1, oDlg:nLeft + 53, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop, oDlg:nLeft + 53, hb_UTF8ToStrBox( "┬" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nBottom, oDlg:nLeft + 53, hb_UTF8ToStrBox( "┴" ), oDlg:cColor )

   hb_DispBox( oDlg:nTop + 6, oDlg:nLeft + 13, oDlg:nTop + 6, oDlg:nLeft + 52, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop + 6, oDlg:nLeft + 12, hb_UTF8ToStrBox( "├" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop + 6, oDlg:nLeft + 53, hb_UTF8ToStrBox( "┤" ), oDlg:cColor )

   /* Display labels */

   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 15, "Alias:              Record:           ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 15, "   BOF:         Deleted:              ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 15, "   EOF:           Found:              ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 4, oDlg:nLeft + 15, "Filter:                               ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 5, oDlg:nLeft + 15, "   Key:                               ", oDlg:cColor )

   /* Stabilize browse */

   aBrw[ 1 ]:ForceStable()
   aBrw[ 2 ]:ForceStable()
   aBrw[ 3 ]:ForceStable()

   UpdateInfo( oDlg, Alias() )
   DispEnd()

   RETURN

STATIC PROCEDURE DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, /* @ */ aStruc, /* @ */ aInfo, /* @ */ nFocus )

   LOCAL nAlias

   IF nKey == K_TAB .or. nKey == K_SH_TAB
      aBrw[ nFocus ]:AutoLite := .F.
      aBrw[ nFocus ]:DeHilite()
      nFocus := nFocus + iif( nKey == K_TAB, 1, -1)
      IF nFocus < 1
         nFocus := 3
      ENDIF
      IF nFocus > 3
         nFocus := 1
      ENDIF
      aBrw[ nFocus ]:AutoLite := .T.
      aBrw[ nFocus ]:forceStable()
      aBrw[ nFocus ]:Hilite()
      RETURN
   ENDIF
   
   SWITCH nFocus
   CASE 1
      nAlias := aBrw[ 1 ]:Cargo
      WorkAreasKeyPressed( nKey, aBrw[ 1 ] )
      IF nAlias != aBrw[ 1 ]:Cargo
         aBrw[ 2 ]:GoTop()
         aBrw[ 2 ]:Invalidate()
         aBrw[ 2 ]:ForceStable()
         aInfo := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( DbfInfo( nAlias ) )
         aBrw[ 3 ]:Configure()
         aBrw[ 2 ]:Invalidate()
         aBrw[ 2 ]:RefreshAll()
         aBrw[ 2 ]:ForceStable()
         aBrw[ 2 ]:Dehilite()
         aBrw[ 3 ]:GoTop()
         aBrw[ 3 ]:Invalidate()
         aBrw[ 3 ]:ForceStable()
         aStruc := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( dbStruct() )
         aBrw[ 3 ]:Configure()
         aBrw[ 3 ]:Invalidate()
         aBrw[ 3 ]:RefreshAll()
         aBrw[ 3 ]:ForceStable()
         aBrw[ 3 ]:Dehilite()
         UpdateInfo( oDlg, aAlias[ aBrw[ 1 ]:Cargo ][ 2 ] )
      ENDIF
      EXIT
   CASE 2
      WorkAreasKeyPressed( nKey, aBrw[ 2 ] )
      EXIT
   CASE 3
      WorkAreasKeyPressed( nKey, aBrw[ 3 ] )
      EXIT
   ENDSWITCH

   RETURN

STATIC PROCEDURE WorkAreasKeyPressed( nKey, oBrw )

   SWITCH nKey
      CASE K_UP
         oBrw:Up()
         oBrw:ForceStable()
         EXIT

      CASE K_DOWN
         oBrw:Down()
         oBrw:ForceStable()
         EXIT

      CASE K_HOME
      CASE K_CTRL_PGUP
      CASE K_CTRL_HOME
         oBrw:GoTop()
         oBrw:ForceStable()
         EXIT

      CASE K_END
      CASE K_CTRL_PGDN
      CASE K_CTRL_END
         oBrw:GoBottom()
         oBrw:ForceStable()
         EXIT
      
      CASE K_PGDN
           oBrw:PageDown()
           oBrw:ForceStable()
           EXIT
           
      CASE K_PGUP
           oBrw:PageUp()
           oBrw:ForceStable()
           EXIT
           
   ENDSWITCH

   RETURN

STATIC FUNCTION DbfInfo( nCurrentArea )

   LOCAL nFor
   LOCAL xValue
   LOCAL cValue
   LOCAL aInfo := {}

   AAdd( aInfo, { nCurrentArea == Select(), "[" + hb_ntos( Select( Alias() ) ) + "] " + Alias() } )
   AAdd( aInfo, { .F., Space( 4 ) + "Current Driver" } )
   AAdd( aInfo, { .F., Space( 8 ) + rddName() } )
   If ! Empty( OrdName( 1 ) )
      AAdd( aInfo, { .F., Space( 4 ) + "Order Keys" } )
      nFor := 1
      While ! Empty( OrdName( nFor ) )
         AAdd(aInfo, { nFor == IndexOrd(), Space( 8 ) + OrdKey( nFor ) } )
         nFor++
      EndDo
   EndIf
   AAdd( aInfo, { .F., Space( 4 ) + "Workarea Information" } )
   AAdd( aInfo, { .F., Space( 8 ) + "Select Area: " + hb_ntos( Select() ) } )
   AAdd( aInfo, { .F., Space( 8 ) + "Record Size: " + hb_ntos( RecSize() ) } )
   AAdd( aInfo, { .F., Space( 8 ) + "Header Size: " + hb_ntos( Header() ) } )
   AAdd( aInfo, { .F., Space( 8 ) + "Field Count: " + hb_ntos( FCount() ) } )
   AAdd( aInfo, { .F., Space( 8 ) + "Last Update: " + DToC( LUpdate() ) } )
   AAdd( aInfo, { .F., Space( 8 ) + "Index order: " + hb_ntos( IndexOrd() ) } )
   AAdd( aInfo, { .F., Space( 4 ) + "Current Record" } )

   FOR nFor := 1 TO FCount()

      xValue := FieldGet( nFor )

      SWITCH ValType( xValue )
         CASE "C"
         CASE "M"
            cValue := xValue
            EXIT
#ifdef HB_CLP_STRICT
         CASE "L"
            cValue := iif( xValue, "T", "F" )
#endif
         OTHERWISE
            cValue := __dbgValToStr( xValue )
      ENDSWITCH

      AAdd( aInfo, { .F., Space( 8 ) + PadR( FieldName( nFor ), 10 ) + " = " + PadR( cValue, 19 ) } )

   NEXT

   RETURN aInfo

STATIC PROCEDURE UpdateInfo( oDlg, cAlias )

   LOCAL nOldArea

   IF Empty( cAlias )
      RETURN
   ENDIF

   nOldArea := Select()

   dbSelectArea( cAlias )

   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 22, PadR( cAlias, 12 ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 42, ;
      PadR( hb_ntos( RecNo() ) + "/" + hb_ntos( LastRec() ), 9 ), ;
      oDlg:cColor )

   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 23, iif( Bof(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 40, iif( Deleted(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 23, iif( Eof(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 40, iif( Found(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 4, oDlg:nLeft + 23, PadR( dbFilterInfo(), 30 ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 5, oDlg:nLeft + 23, PadR( ordKey(), 30 ), oDlg:cColor )

   dbSelectArea( nOldArea )

   RETURN

STATIC FUNCTION dbFilterInfo()
   RETURN iif( Empty( dbFilter() ), ;
               iif( Empty( hb_dbGetFilter() ), "", "{|| ... }" ), dbFilter() )
