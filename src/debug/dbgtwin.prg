/*
 * The Debugger
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (:Move())
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */

#include "hbclass.ch"

#include "hbmemvar.ch"

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define VS_ICON   0
#define VS_NORMAL 1
#define VS_ZOOM   2

CREATE CLASS HBDbWindow // Debugger windows and dialogs

   CLASSVAR aWindows
   CLASSVAR cBackImage
   CLASSVAR oFocusedWin
   CLASSVAR nTopBound
   CLASSVAR nLeftBound
   CLASSVAR nBottomBound
   CLASSVAR nRightBound

   VAR nTopOri
   VAR nLeftOri
   VAR nBottomOri
   VAR nRightOri
   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight
   VAR cCaption
   VAR cColor
   VAR lFocused     INIT .F.
   VAR bGotFocus
   VAR bKeyPressed
   VAR bLostFocus
   VAR bPainted
   VAR bLButtonDown
   VAR bLDblClick
   VAR lShadow      INIT .F.
   VAR lVisible     INIT .F.
   VAR Cargo
   VAR Browser
   VAR nZOrder
   VAR lCloseButton
   VAR nViewStyle
   
   EXPORTED:
   
      METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor )

      METHOD Hide( lRemoveFromList )
      METHOD IsOver( nRow, nCol )
      METHOD nWidth() INLINE ::nRight - ::nLeft + 1
      METHOD Clear()
      METHOD ScrollUp( nLines )
      METHOD SetCaption( cCaption )
      METHOD ShowCaption()
      METHOD SetFocus( lOnOff )
      METHOD Show( lFocused )
      METHOD ShowModal()
      METHOD LButtonDown( nMRow, nMCol )
      METHOD LDblClick( nMRow, nMCol )
      METHOD LoadColors()

      METHOD SaveBackImage( nTop, nLeft, nBottom, nRight )
      METHOD RestoreBackImage()
      METHOD RemoveWindow()
      METHOD ToTop()
      METHOD WindowIsOver( nRow, nCol )
      METHOD IsCloseButton( nRow, nCol )
      METHOD IsCaption( nRow, nCol )

      METHOD Move( nMRow, nMCol )
      METHOD KeyPressed( nKey )
      METHOD Refresh()
      METHOD Resize( nTop, nLeft, nBottom, nRight )
      METHOD ChangeSize( nMRow, nMCol )
      METHOD RefreshAll()
      METHOD NextWindow()
      METHOD PrevWindow()
      METHOD Zoom()
      METHOD Iconize()

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor ) CLASS HBDbWindow

   ::nTopOri      := nTop
   ::nLeftOri     := nLeft
   ::nBottomOri   := nBottom
   ::nRightOri    := nRight
   ::nTop         := nTop
   ::nLeft        := nLeft
   ::nBottom      := nBottom
   ::nRight       := nRight
   ::cCaption     := cCaption
   ::cColor       := hb_defaultValue( cColor, __dbgColors()[ 1 ] )
   ::lCloseButton := .T.
   ::nViewStyle   := VS_NORMAL
   ::nZOrder      := 1

   IF ::aWindows == NIL
      // ::SaveBackImage( 0, 0, MaxRow(), MaxCol() )
      ::aWindows    := { Self }
      ::oFocusedWin := Self
   ELSE
      AEval( ::aWindows, { | oWin | oWin:nZOrder++ } )
      hb_AIns( ::aWindows, 1, Self, .T. )
   ENDIF

   RETURN Self

METHOD Clear() CLASS HBDbWindow

   hb_Scroll( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1,,, ::cColor )

RETURN Self

METHOD Hide( lRemoveFromList ) CLASS HBDbWindow

   lRemoveFromList := hb_defaultValue( lRemoveFromList, .T. )
   
   DispBegin()

   //__dbgRestScreen( ::nTop, ::nLeft, ::nBottom + iif( ::lShadow, 1, 0 ), ::nRight + iif( ::lShadow, 2, 0 ), ::cBackImage )
   //::cBackImage := NIL
   ::lVisible := .F.

   If lRemoveFromList
      ::RemoveWindow()
   EndIf

   ::RefreshAll()

   DispEnd()

RETURN Self

METHOD RemoveWindow() CLASS HBDbWindow

   LOCAL nLoop

   If ::aWindows != NIL .And. ::nZOrder <= Len( ::aWindows )

      FOR nLoop := ::nZOrder + 1 To Len( ::aWindows )
         ::aWindows[ nLoop ]:nZOrder--
      NEXT
      hb_ADel( ::aWindows, ::nZOrder, .T. )
   EndIf

RETURN Self

   
METHOD IsOver( nRow, nCol ) CLASS HBDbWindow
RETURN nRow >= ::nTop  .AND. nRow <= ::nBottom .AND. nCol >= ::nLeft .AND. nCol <= ::nRight

METHOD WindowIsOver( nRow, nCol ) CLASS HBDbWindow

   LOCAL oWin
   LOCAL nLoop

   FOR nLoop := 1 To Len( ::aWindows )

      If ::aWindows[ nLoop ]:lVisible .And. ::aWindows[ nLoop ]:IsOver( nRow, nCol )
         oWin := ::aWindows[ nLoop ]
         EXIT
      EndIf

   NEXT

RETURN oWin 
      
METHOD IsCloseButton( nRow, nCol ) CLASS HBDbWindow
RETURN ::lCloseButton             .And. ;
       ::nViewStyle != VS_ICON    .And. ;
       ::nRight - ::nLeft > 3     .And. ;
        nRow == ::nTop            .And. ;
        nCol >= ::nLeft + 1       .And. ;
        nCol <= ::nLeft + 3


METHOD IsCaption( nRow, nCol ) CLASS HBDbWindow

   LOCAL lIsCaption

   IF ::nViewStyle == VS_ICON
      lIsCaption := ( nRow == ::nTop .And. nCol >= ::nLeft .And. nCol <= ::nRight )
   ELSE
      lIsCaption := ( nRow == ::nTop .And. nCol >= ::nLeft + If( ::lCloseButton, 4, 1 ) .And. nCol <= ::nRight - 1 )
   ENDIF
 
RETURN lIsCaption


METHOD ScrollUp( nLines ) CLASS HBDbWindow

   hb_Scroll( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1, hb_defaultValue( nLines, 1 ),, ::cColor )

RETURN Self

METHOD SetCaption( cCaption ) CLASS HBDbWindow

   ::cCaption := cCaption

RETURN Self

METHOD ShowCaption() CLASS HBDbWindow

   LOCAL nCol
   LOCAL nMargin

   IF ! Empty( ::cCaption )
      IF ::nViewStyle == VS_ICON
         hb_DispOutAt( ::nTop, ::nLeft + 1, Left( ::cCaption, ::nRight - ::nLeft - 1 ), ::cColor )
      ELSE
         nMargin := IIF( ::lCloseButton, 3, 0 )
         nCol    := ::nLeft + ( ( ( ::nRight - ::nLeft - nMargin ) - ( Len( Left( ::cCaption, ::nRight - ::nLeft - 1 ) ) + 2 ) ) / 2 ) + nMargin
         IF nCol > ::nLeft + nMargin + 1
            hb_DispOutAt( ::nTop, nCol, Left( " " + ::cCaption + " ", ::nRight - ::nLeft - ( nMargin + 1 ) ), ::cColor )
         ENDIF
      ENDIF      
   ENDIF

RETURN Self

METHOD ToTop() CLASS HBDbWindow

   LOCAL oWin
   LOCAL nLoop

   IF ::nZOrder > 1
      FOR nLoop := 1 To ::nZOrder - 1
         ::aWindows[ nLoop ]:nZOrder++
      NEXT
      oWin := ::aWindows[ ::nZOrder ]
      hb_ADel( ::aWindows, ::nZOrder )
      hb_AIns( ::aWindows, 1 )
      ::nZOrder := 1
      ::aWindows[ 1 ] := oWin
   ENDIF

RETURN Self

METHOD SetFocus( lOnOff ) CLASS HBDbWindow

   IF lOnOff .And. ! ::oFocusedWin == Self
      ::oFocusedWin:SetFocus( .F. )
      ::oFocusedWin := Self
   ENDIF

   ::lFocused := lOnOff

   IF ::lVisible

      IF ! lOnOff .AND. HB_ISEVALITEM( ::bLostFocus ) .And. ::nViewStyle != VS_ICON
         Eval( ::bLostFocus, Self )
      ENDIF

      ::Refresh()

      IF lOnOff .And. HB_ISEVALITEM( ::bGotFocus ) .And. ::nViewStyle != VS_ICON
         Eval( ::bGotFocus, Self )
      ENDIF

   ENDIF

RETURN Self

METHOD Refresh() CLASS HBDbWindow

   DispBegin()

   IF ::lVisible

      SetColor( ::cColor )

      IF ::nViewStyle == VS_ICON

         If ::lFocused
           hb_DispOutAt( ::nTop, ::nLeft, Replicate( Chr( 205 ), ::nRight - ::nLeft + 1 ) , ::cColor )
         Else
           hb_DispOutAt( ::nTop, ::nLeft, Replicate( Chr( 196 ), ::nRight - ::nLeft + 1 ) , ::cColor )
         EndIf

         ::ShowCaption( ::cCaption )

      Else

         hb_Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight,,, ::cColor )
         hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, iif( ::lFocused, HB_B_DOUBLE_UNI, HB_B_SINGLE_UNI ), ::cColor )

         IF ::lCloseButton .And. ( ::nRight - ::nLeft > 3 )
            hb_DispOutAt( ::nTop, ::nLeft + 1, "[" + Chr( 254 ) + "]", ::cColor )
         ENDIF

         ::ShowCaption( ::cCaption )
         
         IF ::lShadow
            hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
         ENDIF

         IF HB_ISEVALITEM( ::bPainted )
            Eval( ::bPainted, Self )
         ENDIF

      ENDIF

   ENDIF

   IF::nZOrder > 1
      ::aWindows[ ::nZOrder - 1 ]:Refresh()
   ENDIF

   DispEnd()
   
RETURN Self

METHOD Show( lFocused ) CLASS HBDbWindow

   LOCAL nRow, nCol

   lFocused := hb_defaultValue( lFocused, ::lFocused )

   nRow := Row()
   nCol := Col()

   DispBegin()

   ::lVisible := .t.

   IF lFocused
      ::SetFocus( lFocused )
   ELSE
      ::Refresh()
   ENDIF
   DispEnd()

   SetPos( nRow, nCol )

RETURN Self

METHOD ShowModal() CLASS HBDbWindow

   LOCAL lExit := .F.
   LOCAL nKey
   LOCAL nMRow
   LOCAL nMCol

   ::lShadow := .T.
   ::Show()

   WHILE ! lExit

      nKey := __dbgInKey()

      IF HB_ISEVALITEM( ::bKeyPressed ) 
         Eval( ::bKeyPressed, nKey )
      ENDIF

      DO CASE
         CASE nKey == K_ESC
              lExit := .t.

         CASE nKey == K_LBUTTONDOWN
              nMCol := MCol()
              nMRow := MRow()
              IF ::IsCloseButton( nMRow, nMCol )
                 lExit := .t.
              ELSEIF ::IsCaption( nMRow, nMCol )
                 ::Move( nMRow, nMCol )
              ENDIF
      ENDCASE
   END

   DispBegin()
   ::RemoveWindow()
   ::RefreshAll()
   DispEnd()

RETURN Self

METHOD LButtonDown( nMRow, nMCol ) CLASS HBDbWindow

   IF HB_ISEVALITEM( ::bLButtonDown )
      Eval( ::bLButtonDown, nMRow, nMCol )
   ENDIF

RETURN Self

METHOD LDblClick( nMRow, nMCol ) CLASS HBDbWindow

   IF HB_ISEVALITEM( ::bLDblClick )
      Eval( ::bLDblClick, nMRow, nMCol )
   ENDIF

RETURN Self

METHOD Move( nMRow, nMCol ) CLASS HBDbWindow

   LOCAL nOldTop    := ::nTop
   LOCAL nOldLeft   := ::nLeft
   LOCAL nOldBottom := ::nBottom
   LOCAL nOldRight  := ::nRight
   LOCAL nKey       := K_MOUSEMOVE
   LOCAL cBack
   Local lMoved     := .F.
   LOCAL lMouseMove := ( nMRow != NIL .And. nMCol != NIL )
   LOCAL nMLastRow  := nMRow
   LOCAL nMLastCol  := nMCol
   LOCAL nCursor    := SetCursor( SC_NONE )

   IF ::nViewStyle != VS_ZOOM

      ::Hide( .F. )
      cBack := __dbgSaveScreen()
      WHILE ! lMouseMove .Or. nKey == K_MOUSEMOVE

         DispBegin()
         __dbgRestScreen( ,,,,cBack )
         IF ::Browser != NIL
            ::Browser:Resize( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1 )
         ENDIF

         ::Show()
         IF ! lMouseMove
            hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, Replicate( Chr( 176 ), 8 ) )
         ENDIF

         DispEnd()

         SetCursor( SC_NONE )

         nKey := __dbgInkey()

         IF lMouseMove

            IF nKey == K_MOUSEMOVE

               nMRow := MRow()
               nMCol := MCol()

               ::nTop    += nMRow - nMLastRow
               ::nBottom += nMRow - nMLastRow
               ::nLeft   += nMCol - nMLastCol
               ::nRight  += nMCol - nMLastCol

               // Check bounds
               IF ::nTop < ::nTopBound
                  ::nBottom += ::nTopBound - ::nTop
                  ::nTop    += ::nTopBound - ::nTop
               ENDIF

               IF ::nBottom > ::nBottomBound
                  ::nTop   -= ::nBottom - ::nBottomBound
                  ::nBottom-= ::nBottom - ::nBottomBound
               ENDIF

               IF ::nLeft < ::nLeftBound
                  ::nRight += ::nLeftBound - ::nLeft
                  ::nLeft  += ::nLeftBound - ::nLeft
               ENDIF

               lMoved := .T.

               nMLastRow := nMRow
               nMLastCol := nMCol

            ELSEIF nKey == K_ESC

              ::nTop    := nOldTop
              ::nLeft   := nOldLeft
              ::nBottom := nOldBottom
              ::nRight  := nOldRight
              lMoved    := .F.
              EXIT

            ENDIF

         ELSE

            DO CASE

               CASE nKey == K_ENTER
                    exit

               CASE nkey == K_UP
                    IF ::nTop > ::nTopBound
                       ::nTop--
                       ::nBottom--
                       lMoved := .T.
                    ENDIF

               CASE nKey == K_DOWN
                    IF ::nBottom < ::nBottomBound
                       ::nTop++
                       ::nBottom++
                       lMoved := .T.
                    ENDIF

               CASE nKey == K_LEFT
                    IF ::nLeft > ::nLeftBound
                       ::nLeft--
                       ::nRight--
                       lMoved := .T.
                    ENDIF

               CASE nKey == K_RIGHT
                    IF ::nLeft < ::nRightBound
                       ::nLeft++
                       ::nRight++
                       lMoved := .T.
                    ENDIF

               CASE nKey == K_ESC
                    ::nTop    := nOldTop
                    ::nLeft   := nOldLeft
                    ::nBottom := nOldBottom
                    ::nRight  := nOldRight
                    lMoved := .F.
                    EXIT
            ENDCASE
         ENDIF

      END

      DispBegin()
      IF ::Browser != NIL
         ::Browser:Resize( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1 )
      ENDIF

      ::Show()
      ::RefreshAll()
      DispEnd()

      SetCursor( nCursor )

      ::nTopOri      += ::nTop    - nOldTop
      ::nLeftOri     += ::nLeft   - nOldLeft
      ::nBottomOri   += ::nBottom - nOldBottom
      ::nRightOri    += ::nRight  - nOldRight

   ENDIF

RETURN lMoved

METHOD KeyPressed( nKey ) CLASS HBDbWindow

   IF HB_ISEVALITEM( ::bKeyPressed )
      Eval( ::bKeyPressed, nKey, Self )
   ENDIF

RETURN Self

METHOD LoadColors() CLASS HBDbWindow

   LOCAL aClr := __dbgColors()

   ::cColor := aClr[ 1 ]

   IF ::Browser != NIL
      ::Browser:ColorSpec := aClr[ 2 ] + "," + aClr[ 5 ] + "," + aClr[ 3 ] + "," + aClr[ 6 ] + "," + aClr[ 12 ] + "," + aClr[ 13 ]
   ENDIF

RETURN Self

METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBDbWindow

   IF nTop != NIL
     ::nTop := nTop
   ENDIF
   IF nBottom != NIL
     ::nBottom := nBottom
   ENDIF
   IF nLeft != NIL
     ::nLeft := nLeft
   ENDIF
   IF nRight != NIL
     ::nRight := nRight
   ENDIF

   IF ::Browser != NIL
     ::Browser:Resize( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1 )
   ENDIF

   ::nViewStyle := VS_NORMAL
   ::nTopOri    := ::nTop
   ::nLeftOri   := ::nLeft
   ::nBottomOri := ::nBottom
   ::nRightOri  := ::nRight

RETURN self

METHOD ChangeSize( nMRow, nMCol ) Class HBDbWindow

   local nOldTop       := ::nTop
   local nOldLeft      := ::nLeft
   local nOldBottom    := ::nbottom
   local nOldRight     := ::nright
   local nKey          := K_MOUSEMOVE
   Local lChanged      := .F.
   Local lMouseResize  := ( nMRow != NIL .And. nMCol != NIL )
   Local nMLastRow     := nMRow
   Local nMLastCol     := nMCol
   Local nCursor       := SetCursor( SC_NONE )
   Local lLeftResize   := ( nMCol == ::nLeft   )
   Local lRightResize  := ( nMCol == ::nRight  )
   Local lTopResize    := ( nMRow == ::nTop .And. ( nMCol == ::nLeft .Or. nMCol == ::nRight ) )
   Local lBottomResize := ( nMRow == ::nBottom )
   Local cBack

   If ::nViewStyle == VS_NORMAL

      ::Hide( .F. )
      cBack := __dbgSaveScreen()
      WHILE ! lMouseResize .Or. nKey == K_MOUSEMOVE

         DispBegin()
         __dbgRestScreen( ,,,,cBack )
         IF ::Browser != NIL
            ::Browser:Resize( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1 )
         ENDIF

         ::Show()
         IF ! lMouseResize
            hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, Replicate( Chr( 176 ), 8 ) )
         ENDIF

         DispEnd()

         SetCursor( SC_NONE )

         nKey := __dbgInkey()

         IF lMouseResize

            IF nKey == K_MOUSEMOVE

               nMRow := MRow()
               nMCol := MCol()

               IF lLeftResize
                  ::nLeft += nMCol - nMLastCol
                  ::nLeft := Max( Min( ::nLeft, ::nRight - 5 ), ::nLeftBound )
               ENDIF

               IF lRightResize
                  ::nRight += nMCol - nMLastCol
                  ::nRight := Min( Max( ::nLeft + 5, ::nRight ), ::nRightBound )
               ENDIF

               IF lTopResize
                  ::nTop += nMRow - nMLastRow
                  ::nTop := Max( Min( ::nTop, ::nBottom - 2 ), ::nTopBound )
               ENDIF

               IF lBottomResize
                  ::nBottom += nMRow - nMLastRow
                  ::nBottom := Min( Max( ::nTop + 2, ::nBottom ), ::nBottomBound )
               ENDIF

               lChanged := .T.

               nMLastRow := nMRow
               nMLastCol := nMCol

            ELSEIF nKey == K_ESC

              ::nTop    := nOldTop
              ::nLeft   := nOldLeft
              ::nBottom := nOldBottom
              ::nRight  := nOldRight
              lChanged    := .F.
              EXIT

            ENDIF

         ELSE

            DO CASE

               CASE nKey == K_ENTER
                    EXIT

               CASE nkey == K_UP
                    IF ::nBottom > ::nTop
                       ::nBottom--
                       lChanged := .T.
                    ENDIF

               CASE nKey == K_DOWN
                    IF ::nBottom < ::nBottomBound
                       ::nBottom++
                       lChanged := .T.
                    ENDIF

               CASE nKey == K_LEFT
                    IF ::nRight > ::nLeft
                       ::nRight--
                       lChanged := .T.
                    ENDIF

               CASE nKey == K_RIGHT
                    IF ::nRight < ::nRightBound
                       ::nRight++
                       lChanged := .T.
                    ENDIF

               CASE nKey == K_ESC
                    ::nTop    := nOldTop
                    ::nLeft   := nOldLeft
                    ::nBottom := nOldBottom
                    ::nRight  := nOldRight
                    lChanged    := .F.
                    EXIT
            ENDCASE
         ENDIF

      END

      DispBegin()
      IF ::Browser != NIL
         ::Browser:Resize( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1 )
      ENDIF

      ::Show()
      ::RefreshAll()
      DispEnd()

      SetCursor( nCursor )

      ::nTopOri    += ::nTop    - nOldTop
      ::nLeftOri   += ::nLeft   - nOldLeft
      ::nBottomOri += ::nBottom - nOldBottom
      ::nRightOri  += ::nRight  - nOldRight

   ENDIF

RETURN lChanged

METHOD RefreshAll() CLASS HBDbWindow

   // Restore back image of Top windows
   If ::aWindows != NIL

      __dbgRestScreen( ::nTopBound, ::nLeftBound, ::nBottomBound, ::nRightBound, ::cBackImage )

      ATail( ::aWindows ):Refresh()

   EndIf

RETURN Self

Method RestoreBackImage() CLASS HBDbWindow

   If ::cBackImage  != Nil
      __dbgRestScreen( ::nTopBound, ::nLeftBound, ::nBottomBound, ::nRightBound, ::cBackImage )
   EndIf

RETURN Self

Method SaveBackImage( nTop, nLeft, nBottom, nRight ) CLASS HBDbWindow

   ::cBackImage   := __dbgSaveScreen( nTop, nLeft, nBottom, nRight )
   ::nTopBound    := nTop
   ::nLeftBound   := nLeft
   ::nBottomBound := nBottom
   ::nRightBound  := nRight

RETURN Self


METHOD NextWindow() CLASS HBDbWindow

   LOCAL nNext := ::nZOrder - 1

   IF nNext < 1
      nNext := Len( ::aWindows )
   ENDIF

   WHILE nNext != ::nZOrder .And. nNext > 0 .And. ! ::aWindows[ nNext ]:lVisible
      nNext--
   ENDDO

RETURN IIf( nNext > 0, ::aWindows[ nNext ], Self )


METHOD PrevWindow() CLASS HBDbWindow

   LOCAL nPrev := ::nZOrder + 1

   IF nPrev > Len( ::aWindows )
      nPrev := 1
   ENDIF

   WHILE nPrev != ::nZOrder .And. nPrev <= Len( ::aWindows ) .And. ! ::aWindows[ nPrev ]:lVisible
      nPrev++
   ENDDO

Return IIf( nPrev <= Len( ::aWindows ), ::aWindows[ nPrev ], Self )


METHOD Zoom() CLASS HBDbWindow

   IF ::nViewStyle == VS_ICON

      ::nViewStyle := VS_NORMAL
      ::nBottom    := ::nTop  + ::nBottomOri - ::nTopOri
      ::nRight     := ::nLeft + ::nRightOri  - ::nLeftOri
      ::nTopOri    := ::nTop
      ::nLeftOri   := ::nLeft
      ::nBottomOri := ::nBottom
      ::nRightOri  := ::nRight

      IF ::lFocused .And. HB_ISEVALITEM( ::bGotFocus )
         Eval( ::bGotFocus, Self )
      ENDIF

   ELSEIF ::nViewStyle == VS_NORMAL

      ::nViewStyle := VS_ZOOM
      ::nTop       := ::nTopBound
      ::nLeft      := ::nLeftBound
      ::nBottom    := ::nBottomBound
      ::nRight     := ::nRightBound

   ELSE

      ::nViewStyle := VS_NORMAL
      ::nTop       := ::nTopOri
      ::nLeft      := ::nLeftOri
      ::nBottom    := ::nBottomOri
      ::nRight     := ::nRightOri

   ENDIF

   IF ::Browser != NIL
     ::Browser:Resize( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1 )
   ENDIF

RETURN Self


METHOD Iconize() CLASS HBDbWindow

   IF ::nViewStyle == VS_ICON

      ::nViewStyle := VS_NORMAL
      ::nBottom    := ::nTop  + ::nBottomOri - ::nTopOri
      ::nRight     := ::nLeft + ::nRightOri  - ::nLeftOri
      ::nTopOri    := ::nTop
      ::nLeftOri   := ::nLeft
      ::nBottomOri := ::nBottom
      ::nRightOri  := ::nRight

      IF ::Browser != NIL
        ::Browser:Resize( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1 )
      ENDIF

      IF ::lFocused .And. HB_ISEVALITEM( ::bGotFocus )
         Eval( ::bGotFocus, Self )
      ENDIF

   ELSE

      ::nViewStyle := VS_ICON

      ::nBottom    := ::nTop
      ::nRight     := ::nLeft + 14

      IF ::lFocused .And. HB_ISEVALITEM( ::bLostFocus )
         Eval( ::bLostFocus, Self )
      ENDIF

   ENDIF

RETURN Self
