/*
 * $Id: tbrowse.prg,v 1.3.2.1 2005/08/02 12:47:11 fabio Exp $
 */

/*
 * Harbour Project source code:
 * TBrowse Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

 /*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000, '01, '02 Maurilio Longo <maurilio.longo@libero.it>
 * Cursor movement handling, stabilization loop, multi-line headers and footers support
 * ::PageUp(), ::PageDown(), ::Down(), ::Up(), ::GoBottom(), ::GoTop(), ::Stabilize()
 * ::GotoXY(), ::DispCell(), ::WriteMLineText(), ::RedrawHeaders(),
 * ::SetFrozenCols(), ::SetColumnWidth()
 *
 * Copyright 2001 Manu Exposito <maex14@dipusevilla.es>
 * Activate data PICTURE DispCell(nColumn, nColor)
 *
 */


/* NOTE: Don't use SAY in this module, use DispOut(), DispOutAt() instead,
         otherwise it will not be CA-Cl*pper compatible. [vszakats] */

/* TODO: :firstScrCol() --> nScreenCol
         Determines screen column where the first table column is displayed.
         Xbase++ compatible method */

/* TODO: :viewArea() --> aViewArea
         Determines the coordinates for the data area of a TBrowse object.
         Xbase++ compatible method */

//-------------------------------------------------------------------//

#include "common.ch"
#include "hbclass.ch"
#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "button.ch"
#include "tbrowse.ch"
#include "error.ch"

//-------------------------------------------------------------------//
#Define _COL_OBJ            1   // Object Column
#Define _COL_WIDTH          2   // Column Width
#Define _COL_PICTURE        3   // Column Picture
#Define _COL_CELL_WIDTH     4   // Width of the Cell
#Define _COL_SEPARATOR      5   // Column Seperator
#Define _COL_SEP_WIDTH      6   // Width of the Separator
#Define _COL_HEAD_SEP       7   // Column heading
#Define _COL_FOOT_SEP       8   // Heading separator character
#Define _COL_DEFAULT_COLOR  9   // Array with index of color
#Define _COL_COLUMN         10  // Col where text column must be displayed


#define TBC_CLR_STANDARD  1
#define TBC_CLR_ENHANCED  2
#define TBC_CLR_HEADING   3
#define TBC_CLR_FOOTING   4

//-------------------------------------------------------------------//
Class TBrowse

   Hidden:
      Data   aColsInfo
      Data   aHeaders
      Data   aFooters
      Data   aColsData
      Data   aKeys
      Data   aColorRect
      Data   aRefresh
      Data   aRedraw
      Data   cBorder
      Data   cColSep
      Data   cColorSpec
      Data   lConfigured
      Data   lHeadSep
      Data   lFirstConfig
      Data   lFootSep
      Data   lInvalid
      Data   nColCount
      Data   nFreeze
      Data   nLeftVisible
      Data   nCurRowPos
      Data   nRightVisible
      Data   nRowCount
      Data   nRowIni
      Data   nRowPos
      Data   nRowSkip
      Data   nClientBottom
      Data   nClientLeft
      Data   nClientRight
      Data   nClientTop


      Method initKeys()
      Method doConfigure( nMode )
      Method doStable( lForce )
      Method drawRow( nRow )
      Method drawHeaders()
      Method drawFooters()
      Method getMColPos()
      Method setMColPos( nCol )
      Method getMRowPos()
      Method setMRowPos( nRow )
      Method getColWidth( nCol, oCol, xVal )
      Method moved()
      Method widthClient()                        InLine ::nClientRight  - ::nClientLeft + 1
      Method heightClient()                       InLine ::nClientBottom - ::nClientTop + 1
      Method posCursor()
      Method setBorder( cNewBorder )
      Method setFreeze( nNewFreeze )
      Method setRowPos( nNewRowPos )
      Method dispCell( nRow, nCol, nColor )
      Method visibleColsDetermine()
      Method refreshData( nRow, lClear )
      Method scrollRows( nSkip )
      Method getColor( nColor )

   Protected:

   Export:
      Data   autoLite
      // Data   border
      Access border                               InLine ::cBorder
      Assign border( cNewBorder )                 InLine ::setBorder( cNewBorder )
      Data   cargo
      Access colorSpec                            InLine ::cColorSpec
      Assign colorSpec(cColor)                    InLine If( Empty( cColor ), ::cColorSpec, ( ::cColorSpec := cColor, ::configure(), ::cColorSpec ) )
      Data   colPos
      Access colSep                               InLine ::cColSep
      Assign colSep( cNewColSep )                 InLine ::cColSep := cNewColSep
      Data   footSep
      Access freeze                               InLine ::nFreeze
      Assign freeze( nNewFreeze )                 InLine ::setFreeze( nNewFreeze )
      Data   goBottomBlock
      Data   goTopBlock
      Data   headSep
      Data   hitBottom
      Data   hitTop
      Access mColpos                              InLine ::getMColPos()
      Assign mColpos( nCol )                      InLine ::setMColPos( nCol )
      Access mRowPos                              InLine ::getMRowPos()
      Assign mRowpos( nRow )                      InLine ::setMRowPos( nRow )
      // Data   message
      Access nBottom                              InLine ::nClientBottom + If( Empty( ::Border ), 0, 1 )
      Assign nBottom( nNewBottom )                InLine ( ::nClientBottom := nNewBottom - If( Empty( ::Border ), 0, 1 ) ), ::configure(), nNewBottom
      Access nLeft                                InLine ::nClientLeft - If( Empty( ::Border ), 0, 1 )
      Assign nLeft( nNewLeft )                    InLine ( ::nClientLeft := nNewLeft + If( Empty( ::Border ), 0, 1 ) ), ::configure(), nNewLeft
      Access nRight                               InLine ::nClientRight + If( Empty( ::Border ), 0, 1 )
      Assign nRight( nNewRight )                  InLine ( ::nClientRight := nNewRight - If( Empty( ::Border ), 0, 1 ) ), ::configure(), nNewRight
      Access nTop                                 InLine ::nClientTop - If( Empty( ::Border ), 0, 1 )
      Assign nTop( nNewTop )                      InLine ( ::nClientTop := nNewTop + If( Empty( ::Border ), 0, 1 ) ), ::configure(), nNewTop
      Access rowPos                               InLine ::nRowPos
      Assign rowPos( nNewRowPos )                 InLine ::setRowPos( nNewRowPos )
      Data   skipBlock
      Data   stable

      Method addColumn( oNewCol )
      Method applyKey( nKey )
      Method colCount()                           InLine ::nColCount
      Method colorRect( aRect, aColors )
      Method colWidth( nCol )
      Method configure()
      Method deHilite()
      Method delColumn( nCol )
      Method down()
      Method end()
      Method forceStable()                        InLine ::stable := ::doStable( .T. )
      Method getColumn( nCol )
      Method goBottom()
      Method goTop()
      Method hilite()
      Method hitTest( nRow, nCol )
      Method home()
      Method insColumn( nPos, oNewCol )
      Method invalidate()
      Method left()
      Method leftVisible()                        InLine ::nLeftVisible
      Method new( nTop, nLeft, nBottom, nRight )
      Method pageDown()
      Method pageUp()
      Method panEnd()
      Method panHome()
      Method panLeft()
      Method panRight()
      Method refreshAll()
      Method refreshCurrent()
      Method rightVisible()                       InLine ::nRightVisible
      Method rowCount()                           InLine ::nRowCount
      Method setColumn( nCol, oCol )
      Method setKey( nKey, bBlock )
      Method setStyle()
      Method stabilize()                          InLine ::stable := ::doStable( .F. )
      Method right()
      Method up()
//      Method Scroll()

EndClass


Method new( nTop, nLeft, nBottom, nRight ) Class TBrowse

   Local oErr

   Default  nTop    To 0
   Default  nLeft   To 0
   Default  nBottom To MaxRow()
   Default  nRight  To MaxCol()


   // Verify bounds...
   If nBottom < nTop
      oErr := ErrorNew()
      oErr:args          := { nBottom }
      oErr:candefault    := .F.
      oErr:canretry      := .F.
      oErr:cansubstitute := .T.
      oErr:cargo         := NIL
      oErr:description   := "Argument error"
      oErr:filename      := ""
      oErr:gencode       := EG_ARG
      oErr:operation     := ""
      oErr:oscode        := 0
      oErr:severity      := ES_ERROR
      oErr:subcode       := 1001
      oErr:subsystem     := "TBROWSE"
      oErr:tries         := 0
      Eval( ErrorBlock(), oErr )
   EndIf

   // Verify bounds...
   If nRight < nLeft
      oErr := ErrorNew()
      oErr:args          := { nRight }
      oErr:candefault    := .F.
      oErr:canretry      := .F.
      oErr:cansubstitute := .T.
      oErr:cargo         := NIL
      oErr:description   := "Argument error"
      oErr:filename      := ""
      oErr:gencode       := EG_ARG
      oErr:operation     := ""
      oErr:oscode        := 0
      oErr:severity      := ES_ERROR
      oErr:subcode       := 1001
      oErr:subsystem     := "TBROWSE"
      oErr:tries         := 0
      Eval( ErrorBlock(), oErr )
   EndIf

   // Internals
   ::aColsInfo     := {}
   ::aHeaders      := {}
   ::aFooters      := {}
   ::aKeys         := {}
   ::aColorRect    := {}
   ::aRefresh      := {}
   ::aRedraw       := {}
   ::cBorder       := ""
   ::cColSep       := " "
   ::lConfigured   := .F.
   ::lFirstConfig  := .T.
   ::lFootSep      := .F.
   ::lHeadSep      := .F.
   ::lInvalid      := .T.
   ::nColCount     := 0
   ::nFreeze       := 0
   ::nLeftVisible  := 1
   ::nRightVisible := 0
   ::nRowSkip      := 0
   ::nTop          := nTop
   ::nLeft         := nLeft
   ::nBottom       := nBottom
   ::nRight        := nRight
   ::nRowCount     := ::nClientBottom - ::nClientTop + 1

   ::nCurRowPos    := 1

   // Externals
   ::autoLite      := .T.
   ::colPos        := 1
   ::cColorSpec    := SetColor()
   ::footSep       := ""
   ::headSep       := ""
   ::hitBottom     := .F.
   ::hitTop        := .F.
   //::mColpos       := 0
   //::mRowPos       := 0
   // ::message       := ""
   ::nRowPos       := 1
   ::stable        := .F.
   ::nRowIni       := 0

Return( Self )


Method addColumn( oNewCol ) Class TBrowse

   Local cColSep  := If( oNewCol:ColSep != NIL, oNewCol:colSep, ::colSep )
   Local cHeadSep := If( oNewCol:headSep == NIL .Or. Len( oNewCol:headSep ) == 0, ::headSep, oNewCol:headSep )
   Local cFootSep := If( oNewCol:footSep == NIL .Or. Len( oNewCol:footSep ) == 0, ::footSep, oNewCol:footSep )

   AAdd( ::aColsInfo, { oNewCol, 0, '', 0, cColSep, Len( cColSep), cHeadSep, cFootSep, {}, 0 } )
   ::nColCount++
   ::configure()

Return( Self )


Method initKeys() Class TBrowse

   ::aKeys := { { K_DOWN,        {| oB | oB:Down()    , TBR_CONTINUE } } ,;
                { K_END,         {| oB | oB:End()     , TBR_CONTINUE } } ,;
                { K_CTRL_PGDN,   {| oB | oB:GoBottom(), TBR_CONTINUE } } ,;
                { K_CTRL_PGUP,   {| oB | oB:GoTop()   , TBR_CONTINUE } } ,;
                { K_HOME,        {| oB | oB:Home()    , TBR_CONTINUE } } ,;
                { K_LEFT,        {| oB | oB:Left()    , TBR_CONTINUE } } ,;
                { K_PGDN,        {| oB | oB:PageDown(), TBR_CONTINUE } } ,;
                { K_PGUP,        {| oB | oB:PageUp()  , TBR_CONTINUE } } ,;
                { K_CTRL_END,    {| oB | oB:PanEnd()  , TBR_CONTINUE } } ,;
                { K_CTRL_HOME,   {| oB | oB:PanHome() , TBR_CONTINUE } } ,;
                { K_CTRL_LEFT,   {| oB | oB:PanLeft() , TBR_CONTINUE } } ,;
                { K_CTRL_RIGHT,  {| oB | oB:PanRight(), TBR_CONTINUE } } ,;
                { K_RIGHT,       {| oB | oB:Right()   , TBR_CONTINUE } } ,;
                { K_UP,          {| oB | oB:Up()      , TBR_CONTINUE } } ,;
                { K_ESC,         {|    | TBR_EXIT                    } } ,;
                { K_LBUTTONDOWN, {| oB | tbmouse( ob, mrow(), mcol() ) } } }
Return( Self )


Method applyKey( nKey ) Class TBrowse

   Local bBlock  := ::setKey( nKey )
   Local nReturn

   If ISNIL( bBlock )
      nReturn := TBR_EXCEPTION
   Else
      nReturn := Eval( bBlock, Self, nKey )
   EndIf


Return( nReturn )


Method colorRect( aRect, aColors ) Class TBrowse

   Local nRow
   Local nCol
   Local lBottom := .F.
   Local nToSkip
   Local nSkiped

   If ISARRAY( aRect ) .And. Len( aRect ) > 3                                                               .And.;
      aRect[ 1 ] <= aRect[ 3 ] .And. aRect[ 2 ] <= aRect[ 4 ]                                               .And.;
      aRect[ 1 ] > 0 .And. aRect[ 2 ] > 0 .And. aRect[ 3 ] <= ::rowCount() .And. aRect[ 4 ] <= ::colCount() .And.;
      ISARRAY( aColors ) .And. Len( aColors ) > 1                                                           .And.;
      ISNUMBER( aColors[ 1 ] ) .And. aColors[ 1 ] > 0 .And. ISNUMBER( aColors[ 2 ] ) .And. aColors[ 2 ] > 0

      If ! ::lConfigured
         ::doConfigure()
      EndIf

      // Need verify visible cols
      If ::lInvalid
         ::visibleColsDetermine()
      EndIf

      // Draw lines in rect
      For nRow := 1 To ::rowCount()

         If ( nRow >= aRect[ 1 ] .And. nRow <= aRect[ 3 ] )

            If ::aRefresh[ nRow ]

               If ! lBottom
                  nToSkip := nRow - ::nCurRowPos
                  nSkiped := Eval( ::skipBlock, nToSkip )
                  If ( nToSkip == nSkiped .Or. ( nToSkip * nSkiped ) > 0 )
                     ::nCurRowPos += nSkiped
                  Else
                     lBottom  := .T.
                  EndIf
               EndIf
               ::refreshData( nRow, nRow != ::nCurRowPos )
               ::aRefresh[ nRow ] := .F.

            EndIf

            // Set color of rect
            For nCol := aRect[ 2 ] To aRect[ 4 ]
               ::aColorRect[ nRow, nCol ] := aColors
            Next

            ::aRedraw[ nRow ]  := .F.
            ::DrawRow( nRow )

         EndIf

      Next

   EndIf

Return( Self )


Method colWidth( nCol ) Class TBrowse
Return( If ( nCol > 0 .And. nCol <= ::colCount(), ::aColsInfo[ nCol, _COL_WIDTH ], 0 ) )


Method configure() Class TBrowse

   ::nRightVisible := 0
   ::lConfigured := .F.
   AFill( ::aRefresh, .T. )
   AFill( ::aRedraw , .T. )
   ::invalidate()

Return( Self )


Method doConfigure( nMode ) Class TBrowse

   Local nLoop
   Local aColInfo
   Local nCol
   Local oCol
   Local xVal
   Local aLabels

   // Initalize headers e footers information
   ::aHeaders := {}
   ::aFooters := {}
   ::lHeadSep := .F.
   ::lFootSep := .F.
   ::nRowIni  := 0

   Default nMode To 0

   // Initialize columns info
   For Each aColInfo In ::aColsInfo

      nCol := aColInfo:__enumIndex()

      oCol := aColInfo[ _COL_OBJ ]

      If ! ::lFirstConfig .And. nMode == 0
         aColInfo[ _COL_SEPARATOR ]  := If( oCol:ColSep != NIL, oCol:colSep, ::colSep )
         aColInfo[ _COL_SEP_WIDTH  ] := Len( aColInfo[ _COL_SEPARATOR ] )
         aColInfo[ _COL_HEAD_SEP   ] := If( oCol:headSep == NIL .Or. Len( oCol:headSep ) == 0, ::headSep, oCol:headSep )
         aColInfo[ _COL_FOOT_SEP   ] := If( oCol:footSep == NIL .Or. Len( oCol:footSep ) == 0, ::footSep, oCol:footSep )
      EndIf
      ::lHeadSep              := ::lHeadSep .Or. ! Empty( aColInfo[ _COL_HEAD_SEP   ] )
      ::lFootSep              := ::lFootSep .Or. ! Empty( aColInfo[ _COL_FOOT_SEP   ] )

      // Configure headers...
      aLabels := GetLabels( oCol:heading )
      While Len( aLabels ) > Len( ::aHeaders )
         hb_AIns( ::aHeaders, 1, AFill( Array( ::colCount ), "" ), .T. )
      EndDo
      For nLoop := 1 To Len( aLabels )
         ::aHeaders[ nLoop + ( Len( ::aHeaders ) - Len( aLabels ) ), nCol ] := aLabels[ nLoop ]
      Next

      // Configure footers...
      aLabels := GetLabels( oCol:footing )
      While Len( aLabels ) > Len( ::aFooters )
         hb_AIns( ::aFooters, 1, AFill( Array( ::colCount ), "" ), .T. )
      EndDo
      For nLoop := 1 To Len( aLabels )
         ::aFooters[ nLoop, nCol ] := aLabels[ nLoop ]
      Next

      // COL and CELL width
      aColInfo[ _COL_PICTURE      ] := If( Empty( oCol:Picture ), "", oCol:Picture )

      xVal := Eval( oCol:block )

      If oCol:width != NIL
         aColInfo[ _COL_WIDTH      ] := oCol:width
         aColInfo[ _COL_CELL_WIDTH ] := oCol:width
      Else
         aColInfo[ _COL_WIDTH      ] := ::getColWidth( nCol, oCol, xVal )
         aColInfo[ _COL_CELL_WIDTH ] := Min( aColInfo[ _COL_WIDTH ], Len( Transform( xVal, oCol:Picture ) ) ) // Len( xVal ) )
      EndIf

      aColInfo[ _COL_DEFAULT_COLOR  ] := { 1, 2, 1, 1 }

   Next

   ::nRowIni   := If( Len( ::aHeaders ) > 0, Len( ::aHeaders ), 0 ) + If( ::lHeadSep, 1, 0 )
   ::nRowCount := ::nClientBottom - ::nClientTop + 1 - ( Len( ::aHeaders ) + If( ::lHeadSep, 1, 0 ) ) - ( Len( ::aFooters ) + If( ::lFootSep, 1, 0 ) )

   // Reduce footers and headers if data not fit in visible area...
   While ::nRowCount <= 0

      If Len( ::aFooters ) > 0
         // Reduce footers

         ASize( ::aFooters, Len( ::aFooters ) - 1 )

      ElseIf Len( ::aHeaders ) > 0
         // Reduce headers

         ::nRowIni--
         ASize( ::aHeaders, Len( ::aHeaders ) - 1 )

      Else

         ::nRowCount := 0
         Exit

      EndIf

      ::nRowCount := ::nClientBottom - ::nClientTop + 1 - ( Len( ::aHeaders ) + If( ::lHeadSep, 1, 0 ) ) - ( Len( ::aFooters ) + If( ::lFootSep, 1, 0 ) )

   EndDo


   // Array to store data
   ::aColsData := Array( ::rowCount(), ::colCount() )
   AEval( ::aColsData, { | aItem | AFill( aItem, "" ) } )

   // ColorRect
   ::aColorRect := Array( ::rowCount(), ::colCount() )
   AEval( ::aColorRect, { | aItem | AFill( aItem, NIL ) } )

   // Indicate lines to update DATA
   ::aRefresh := Array( ::nRowCount )
   AFill( ::aRefresh, .T. )

   // Indicate lines to redraw
   ::aRedraw := Array( ::nRowCount )
   AFill( ::aRedraw, .T. )

   ::rowPos := ::rowPos
   ::invalidate()
   ::lConfigured  := .T.
   ::lFirstConfig := .F.

Return( NIL )


Method deHilite() Class TBrowse

   Local nRow
   Local nCol

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   If ::colPos > 0 .And. ::colPos <= ::colCount() // .And. Len( ::aColsData ) > 0

      nRow := ::nClientTop + ::nRowIni + ::rowPos - 1
      nCol := ::aColsInfo[ ::colPos, _COL_COLUMN ]

      SetPos( nRow, nCol )
      ::dispCell( ::rowPos, ::colPos, TBC_CLR_STANDARD )
      SetPos( nRow, nCol )
      ::stable := .F.

   EndIf

Return( Self )


Method delColumn( nCol ) Class TBrowse

   Local oCol

   If nCol > 0 .And. nCol <= ::nColCount

      ::nColCount--
      oCol := ::aColsInfo[ nCol, _COL_OBJ ]
      hb_ADel( ::aColsInfo, nCol, .T. )
      ::configure()

   EndIf

Return( oCol )


Method down() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::nRowSkip++

Return( Self )


Method end() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::colPos := ::nRightVisible

Return( Self )


Method getColumn( nCol ) Class TBrowse
Return( If ( nCol > 0 .And. nCol <= ::colCount(), ::aColsInfo[ nCol, _COL_OBJ ], NIL ) )


Method goBottom() Class TBrowse

   Local nSkiped

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   Eval( ::goBottomBlock )
   nSkiped := Eval( ::skipBlock, 1 - ::rowCount() )
   ::rowPos := 1
   Eval( ::SkipBlock, 0 )
   ::nCurRowPos := 1
   ::nRowSkip += Abs( nSkiped )

   // Redraw all lines
   AFill( ::aRefresh, .T. )
   AFill( ::aRedraw , .T. )
   ::invalidate()

Return( Self )


Method goTop() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   Eval( ::goTopBlock )
   ::rowPos     := 1
   ::nCurRowPos := 1

   // Redraw all lines
   ::refreshAll()

Return( Self )


Method hilite() Class TBrowse

   Local nRow
   Local nCol

   If ! ::lConfigured
      ::doConfigure()
   EndIf


   If ::colPos > 0 .And. ::colPos <= ::colCount() // .And. Len( ::aColsData ) > 0

      nRow := ::nClientTop + ::nRowIni + ::rowPos - 1
      nCol := ::aColsInfo[ ::colPos, _COL_COLUMN ]

      SetPos( nRow, nCol )
      ::dispCell( ::rowPos, ::colPos, TBC_CLR_ENHANCED )
      SetPos( nRow, nCol )

   EndIf

Return( Self )


Method getMColPos() Class TBrowse

   Local nCol      := 0
   Local nColData

   For nColData := 1 To Len( ::aColsInfo )

      If nColData <= ::freeze .Or. ( nColData >= ::leftVisible() .And. nColData <= ::rightVisible() )

         If MCol() >= ::aColsInfo[ nColData, _COL_COLUMN ] .And. MCol() < ::aColsInfo[ nColData, _COL_COLUMN ] + ::aColsInfo[ nColData, _COL_WIDTH ]

            nCol := nColData
            Exit

         EndIf

      EndIf

   Next

Return( nCol )


Method setMColPos( nCol ) Class TBrowse

   If nCol > 0 .And. nCol <= ::colCount()

      MSetPos( MRow(), ::aColsInfo[ nCol, _COL_COLUMN ] )

   EndIf

Return( nCol )


Method getMRowPos() Class TBrowse

   Local nRow := MRow() - ( ::nClientTop + ::nRowIni ) + 1

   If nRow < 1 .Or. nRow > ::RowCount()
      nRow := 0
   EndIf

Return( nRow )


Method setMRowPos( nRow ) Class TBrowse

    If ( nRow > 0 .And. nRow <= ::rowCount() )
       MSetPos( ::nClientTop + ::nRowIni + nRow - 1, MCol() )
    EndIf

Return( nRow )


Method hitTest( nRow, nCol ) Class TBrowse

   Local nHit    := HTNOWHERE
   Local nColData

   If nRow >= ::nTop .And. nRow <= ::nBottom .And. nCol >= ::nLeft .And. nCol <= ::nRight

      If ! Empty( ::border )

         If     nRow == ::nTop

            If     nCol == ::nLeft
               nHit := HTTOPLEFT
            ElseIf nCol == ::nRight
               nHit := HTTOPRIGHT
            ElseIf( nCol > ::nLeft .And. nCol < ::nRight )
               nHit := HTTOP
            EndIf

         ElseIf nRow == ::nBottom

            If     nCol == ::nLeft
               nHit := HTBOTTOMLEFT
            ElseIf nCol == ::nRight
               nHit := HTBOTTOMRIGHT
            ElseIf( nCol > ::nLeft .And. nCol < ::nRight )
               nHit := HTBOTTOM
            EndIf

         ElseIf nCol == ::nLeft

            nHit := HTLEFT

         ElseIf nCol == ::nRight

            nHit := HTRIGHT

         EndIf

      EndIf

      If nHit == HTNOWHERE

         // Verify header
         If Len( ::aHeaders ) > 0 .And. nRow <= ::nClientTop + Len( ::aHeaders ) - If( ::lHeadSep, 0, 1 )

            If ::lHeadSep .And. nRow == ::nClientTop + Len( ::aHeaders )
               nHit := HTHEADSEP
            Else
               nHit := HTHEADING
            EndIf

         // Verify footer
         ElseIf Len( ::aFooters ) > 0 .And. nRow >= ::nClientBottom - Len( ::aFooters ) + If( ::lFootSep, 0, 1 ) - 1

            If ::lFootSep .And. nRow == ::nClientBottom - Len( ::aHeaders )
               nHit := HTFOOTSEP
            Else
               nHit := HTFOOTING
            EndIf

         Else

            nHit := HTCELL
            For nColData := 1 To Len( ::aColsInfo )

               If ( nColData <= ::freeze .Or. ( nColData >= ::leftVisible() .And. nColData <= ::rightVisible() ) ) .And.;
                    nColData < ::rightVisible()                                                                    .And.;
                    nCol == ::aColsInfo[ nColData, _COL_COLUMN ] + ::aColsInfo[ nColData, _COL_WIDTH ]

                     nHit := HTCOLSEP
                     Exit

               EndIf

            Next

         EndIf

      EndIf

   EndIf

Return( nHit )


Method home() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::colPos := ::nLeftVisible

Return( Self )


Method insColumn( nPos, oNewCol ) Class TBrowse

   Local oColRet

   If nPos > 0 .And. nPos <= ::nColCount

      oColRet := oNewCol
      ::nColCount++
      hb_AIns( ::aColsInfo, nPos, { oNewCol, 0, '', 0, '', 0, '', '', {}, 0 }, .T. )
      ::configure()

   EndIf

Return( oColRet )


Method invalidate() Class TBrowse

   ::lInvalid := .T.
   ::stable   := .F.

Return( Self )


Method left() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   if ( ::colPos == ::leftVisible() .And. ::leftVisible() > ::freeze + 1 )
      ::invalidate()
   EndIf
   ::colPos--


Return( Self )


Method pageDown() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::nRowSkip += ::rowCount()

Return( Self )


Method pageUp() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::nRowSkip -= ::rowCount()

Return( Self )

Method panEnd() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   If ::rightVisible < ::colCount()
      ::invalidate()
   EndIf
   ::colPos := ::colCount()

Return( Self )


Method panHome() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   If ::leftVisible > ::freeze + 1
      ::invalidate()
   EndIf
   ::colPos := 1

Return( Self )


Method panLeft() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   If ::nLeftVisible - ::freeze > 1
      ::invalidate()
      ::nRightVisible := 0
      ::nLeftVisible--
   EndIf

Return( Self )


Method panRight() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   If ::nRightVisible < ::colCount()
      ::invalidate()
      ::nLeftVisible := 0
      ::nRightVisible++
   EndIf

Return( Self )


Method refreshAll() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   If ::nCurRowPos > 0
      Eval( ::skipBlock, 1 - ::nCurRowPos )
   Else
      Eval( ::skipBlock, 0 )
   EndIf
   ::nCurRowPos := 1
   AFill( ::aRefresh, .T. )
   AFill( ::aRedraw , .T. )
   ::invalidate()

Return( Self )


Method refreshCurrent() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::aRefresh[ ::rowPos ] := .T.
   ::aRedraw[ ::rowPos ]  := .T.
   ::stable := .F.

Return( Self )


Method setBorder( cNewBorder ) Class TBrowse

   If ! Empty( ::cBorder )
      ::nClientTop--
      ::nClientLeft--
      ::nClientBottom++
      ::nClientRight++
   EndIf

   ::cBorder := cNewBorder

   If ! Empty( ::cBorder )
      ::nClientTop++
      ::nClientLeft++
      ::nClientBottom--
      ::nClientRight--
   EndIf

   ::configure()

Return( cNewBorder )


Method setColumn( nCol, oCol ) Class TBrowse

   If nCol > 0 .And. nCol <= ::colCount()

      ::aColsInfo[ nCol ] := { oCol, 0, '', 0, '', 0, '', '', {}, 0 }
      ::configure()

   EndIf

Return( Self )


Method setFreeze( nNewFreeze ) Class TBrowse

   Local nFrozenWidth
   Local nLoop
   Local nMinNextCol

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   If nNewFreeze >= 0 .And. nNewFreeze < ::colCount()

      nFrozenWidth := 0
      For nLoop := 1 To nNewFreeze

         nFrozenWidth += ::aColsInfo[ nLoop, _COL_SEP_WIDTH ] + ::aColsInfo[ nLoop, _COL_WIDTH ]

      Next

      If nNewFreeze < ::colCount
         nMinNextCol := ::aColsInfo[ nNewFreeze + 1, _COL_SEP_WIDTH ] + 1
      Else
         nMinNextCol := 0
      EndIf

      If nFrozenWidth + nMinNextCol <= ::widthClient()

         ::nFreeze       := nNewFreeze
         If ::nLeftVisible <= nNewFreeze
            ::nLeftVisible  := nNewFreeze + 1
         EndIf
         ::nRightVisible := 0
         ::invalidate()

      EndIf

   EndIf

Return( ::nFreeze )


Method setKey( nKey, bBlock ) Class TBrowse

   Local bReturn
   Local nPos

   If ( nPos := AScan( ::akeys, { | aKey | aKey[ 1 ] == nKey } ) ) == 0

      // Add new key
      If ISBLOCK( bBlock )
         AAdd( ::aKeys, { nKey, bBlock } )
      EndIf

   ElseIf PCount() == 1

      bReturn := ::aKeys[ nPos, 2 ]

   Else

      // return old set
      bReturn := ::aKeys[ nPos, 2 ]
      If ISBLOCK( bBlock )
         ::aKeys[ nPos, 2 ] := bBlock
      Else
         hb_ADel( ::aKeys, nPos, .T. )
      EndIf

   EndIf

Return( bReturn )


Method setRowPos( nNewRowPos ) Class TBrowse

   If nNewRowPos < 1
      ::nRowPos := 1
   ElseIf nNewRowPos > ::rowCount()
      ::nRowPos := ::rowCount()
   Else
      ::nRowPos := nNewRowPos
   EndIf

Return( ::nRowPos )


Method setStyle() Class TBrowse
   // ToDo
Return( NIL )


Method doStable( lForce ) Class TBrowse

   Local lStable := .T.
   Local lRedraw
   Local nRow
   Local nSkiped
   Local lSetRow
   Local nToSkip
   Local lBottom


   If ! ::lConfigured
      ::doConfigure( 1 )
   EndIf

   If ::colPos < 1
      ::colPos := 1
      ::invalidate()
   ElseIf ::colPos > ::colCount()
      ::colPos := ::colCount()
      ::invalidate()
   EndIf

   DispBegin()
   If ! ::stable .And. ::colCount() > 0 .And. ::rowCount() > 0

      If ::lInvalid

         // Determine visible cols and positions
         ::visibleColsDetermine()

         // Draw border
         If ! Empty( ::border )
            DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, Left( ::border, 8 ), ::getColor() )
         EndIf

         // Draw Headers and Footers
         ::DrawHeaders()
         ::DrawFooters()

      EndIf


      If ::nRowSkip != 0

         lSetRow := .T.
         If ::nRowSkip < 0 .And. ( ( ABS( ::nRowSkip ) == ::rowCount() ) .Or. ( ABS( ::nRowSkip ) == ::nCurRowPos .And. ::rowPos == ( ::nCurRowPos + 1 ) ) )
            ::rowPos := ::nCurRowPos
         Else

         ::nRowSkip -= ( ::nCurRowPos - ::rowPos )

         If ::nCurRowPos + ::nRowSkip < 1

            ::nRowSkip += 1 - ::rowPos
            lSetRow := .F.

         ElseIf ::nCurRowPos + ::nRowSkip > ::rowCount()

            ::nRowSkip += ::rowCount() - ::rowPos
            lSetRow := .F.

         EndIf

         EndIf

         If ( nSkiped := Eval( ::skipBlock, ::nRowSkip ) ) != 0

            If ( ::nRowSkip == nSkiped .Or. ( ::nRowSkip * nSkiped ) > 0 )

               lSetRow := lSetRow .Or. ( ! lSetRow .And. nSkiped != ::nRowSkip )

               ::nCurRowPos += nSkiped
               If ::nCurRowPos < 1
                  ::scrollRows( ::nCurRowPos - 1 )
                  ::nCurRowPos := 1
               ElseIf ::nCurRowPos > ::rowCount()
                  ::scrollRows( ::nCurRowPos - ::rowCount() )
                  ::nCurRowPos := ::rowCount()
               EndIf

               If lSetRow
                  ::rowPos := ::nCurRowPos
               EndIf

            EndIf

         EndIf

         ::HitTop    := nSkiped > ::nRowSkip
         ::HitBottom := nSkiped < ::nRowSkip
         ::nRowSkip := 0

      EndIf

      // If nothing to redraw then stable
      lStable  := .T.
      lBottom  := .F.
      // Redraw lines

      For Each lRedraw In ::aRedraw

         nRow := lRedraw:__enumIndex()

         If ::aRefresh[ nRow ]

            If ! lBottom
               nToSkip := nRow - ::nCurRowPos
               nSkiped := Eval( ::skipBlock, nToSkip )
               If ( nToSkip == nSkiped .Or. ( nToSkip * nSkiped ) > 0 )
                  ::nCurRowPos += nSkiped
               Else
                  lBottom  := .T.
               EndIf
            EndIf
            ::refreshData( nRow, nRow != ::nCurRowPos )
            ::aRefresh[ nRow ] := .F.

         EndIf


         // Need redraw
         If lRedraw

            ::aRedraw[ nRow ] := .F.
            ::DrawRow( nRow )

            If ! lForce .And. ! lBottom
               lStable := .F.
               Exit
            EndIf

         EndIf

      Next

      If lStable

         If lBottom .And. ::rowPos > ::nCurRowPos
            ::rowPos := ::nCurRowPos
         ElseIf ::rowPos != ::nCurRowPos
            ::nCurRowPos += Eval( ::skipBlock, ::rowPos - ::nCurRowPos )
         Else
            ::rowPos := ::nCurRowPos
         EndIf

      EndIf

   EndIf

   If lStable
      If ::autoLite
         ::Hilite()
      Else
         ::PosCursor()
      EndIf
   EndIf

   DispEnd()

   ::lInvalid := .F.

Return( lStable )


Method right() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   if ( ::colPos == ::rightVisible() .And. ::rightVisible() < ::colCount() )
      ::invalidate()
   EndIf
   ::colPos++

Return( Self )


Method up() Class TBrowse

   If ! ::lConfigured
      ::doConfigure()
   EndIf

   ::moved()
   ::nRowSkip--

Return( Self )


Method drawRow( nRow ) Class TBrowse

   Local cColor
   Local nCol
   Local nPos

   cColor := ::getColor( 0 )

   DispOutAt( ::nClientTop + ::nRowIni + nRow - 1, ::nClientLeft, Space( ::nClientRight - ::nClientLeft + 1 ), cColor )

   For nCol := 1 To Len( ::aColsInfo )

      If nCol <= ::freeze .Or. ( nCol >= ::leftVisible() .And. nCol <= ::rightVisible() )

         SetPos( ::nClientTop + ::nRowIni + nRow - 1, ::aColsInfo[ nCol, _COL_COLUMN ] )

         ::dispCell( nRow, nCol, TBC_CLR_STANDARD )

         nPos := ::aColsInfo[ nCol, _COL_COLUMN ] + ::aColsInfo[ nCol, _COL_WIDTH ]

         If nCol < ::rightVisible()

            DispOutAt( ::nClientTop + ::nRowIni + nRow - 1, nPos, ::aColsInfo[ nCol + 1, _COL_SEPARATOR ], cColor )

         EndIf


      EndIf

   Next

Return( NIL )


Method drawHeaders() Class TBrowse

   Local nRow
   Local nCol
   Local nPos
   Local nWidthSep
   Local nWidth
   Local nPosSep
   Local cSep
   Local lFirst := .T.

   If Len( ::aHeaders ) > 0 .Or. ::lHeadSep

      DispBox( ::nClientTop, ::nClientLeft, ::nClientTop + Len( ::aHeaders ) - 1 + If( ::lHeadSep, 1, 0 ), ::nClientRight, Space( 9 ), ::getColor() )

      For nRow := 1 To Len( ::aHeaders )

         For nCol := 1 To Len( ::aHeaders[ nRow ] )
            // Draw Header line

            If nCol <= ::freeze .Or. ( nCol >= ::leftVisible() .And. nCol <= ::rightVisible() )

               If ::aColsInfo[ nCol, _COL_COLUMN ] + ::aColsInfo[ nCol, _COL_WIDTH ] <= ::nClientRight
                  nWidth := ::aColsInfo[ nCol, _COL_WIDTH ]
               Else
                  nWidth := ::nClientRight - ::aColsInfo[ nCol, _COL_COLUMN ] + 1
               EndIf

               DispOutAt( ::nClientTop + nRow - 1, ::aColsInfo[ nCol, _COL_COLUMN ], PadR( ::aHeaders[ nRow, nCol ], nWidth ), ::getColor( ColorToDisp( ::aColsInfo[ nCol, _COL_OBJ ]:DefColor, TBC_CLR_HEADING ) - 1 ) )

            EndIf

         Next

      Next

      If ::lHeadSep
         // Draw SEP

         nRow := ::nClientTop + Len( ::aHeaders )
         nPos := ::nClientLeft

         For nCol := 1 To Len( ::aColsInfo )
            // Draw Header line

            If nCol <= ::freeze .Or. ( nCol >= ::leftVisible() .And. nCol <= ::rightVisible() )

               If lFirst

                  If nCol == 1
                     nPosSep := Len( ::aColsInfo[ nCol, _COL_HEAD_SEP ] )
                  Else
                     nPosSep := Min( Len( ::aColsInfo[ nCol, _COL_SEPARATOR ] ) + 1, Len( ::aColsInfo[ nCol, _COL_HEAD_SEP ] ) )
                  EndIf

               Else

                  nPosSep := 1
                  cSep    := Left( ::aColsInfo[ nCol, _COL_HEAD_SEP ], Len( ::aColsInfo[ nCol, _COL_SEPARATOR ] ) )
                  nPosSep += Len( cSep )

                  // Draw columns
                  If ::freeze > 0 .And. ::leftVisible() == nCol

                     //nPosSep := 1
                     cSep    := Left( ::aColsInfo[ ::freeze + 1, _COL_HEAD_SEP ], Len( ::aColsInfo[ ::freeze + 1, _COL_SEPARATOR ] ) )
                     //nPosSep += Len( cSep )
                     cSep    := PadR( cSep, Len( ::aColsInfo[ ::freeze + 1, _COL_SEPARATOR ] ), Right( cSep, 1 ) )
                     DispOutAt( nRow, nPos, cSep, ::getColor() )
                     nPos += Len( cSep )

                  Else

                     cSep    := PadR( cSep, Len( ::aColsInfo[ nCol, _COL_SEPARATOR ] ), Right( cSep, 1 ) )
                     DispOutAt( nRow, nPos, cSep, ::getColor() )
                     nPos += Len( cSep )

                  EndIf

               EndIf

               If nCol > 1
                  nPosSep   := Min( nPosSep, Len( ::aColsInfo[ nCol, _COL_HEAD_SEP ] ) )
                  nWidthSep := ::aColsInfo[ nCol, _COL_COLUMN ] - nPos
                  DispOutAt( nRow, nPos, Replicate( SubStr( ::aColsInfo[ nCol, _COL_HEAD_SEP ], nPosSep, 1 ), nWidthSep ), ::getColor() )
                  nPos += nWidthSep
               EndIf

               lFirst    := .F.
               nWidthSep := ::aColsInfo[ nCol, _COL_COLUMN ] + ::aColsInfo[ nCol, _COL_WIDTH ] - nPos
               If nCol == ::rightVisible
                  nWidthSep := ::nClientRight - nPos + 1
               EndIf
               cSep := PadR( SubStr( ::aColsInfo[ nCol, _COL_HEAD_SEP ], nPosSep ), nWidthSep, Right( ::aColsInfo[ nCol, _COL_HEAD_SEP ], 1 ) )
               DispOutAt( nRow, nPos, cSep, ::getColor() )
               nPos += Len( cSep )

            EndIf

         Next

      EndIf

   EndIf

Return( NIL )


Method drawFooters() Class TBrowse

   Local nRow
   Local nCol
   Local nPos
   Local nWidthSep
   Local nWidth
   Local nPosSep
   Local cSep
   Local lFirst := .T.

   If Len( ::aFooters ) > 0 .Or. ::lFootSep

      DispBox( ::nClientBottom - ( Len( ::aFooters ) - 1 + If( ::lFootSep, 1, 0 ) ), ::nClientLeft, ::nClientBottom, ::nClientRight, Space( 9 ), ::getColor() )

      For nRow := 1 To Len( ::aFooters )

         For nCol := 1 To Len( ::aFooters[ nRow ] )
            // Draw Footer line

            If nCol <= ::freeze .Or. ( nCol >= ::leftVisible() .And. nCol <= ::rightVisible() )

               If ::aColsInfo[ nCol, _COL_COLUMN ] + ::aColsInfo[ nCol, _COL_WIDTH ] <= ::nClientRight
                  nWidth := ::aColsInfo[ nCol, _COL_WIDTH ]
               Else
                  nWidth := ::nClientRight - ::aColsInfo[ nCol, _COL_COLUMN ] + 1
               EndIf

               DispOutAt( ::nClientBottom - Len( ::aFooters ) + nRow, ::aColsInfo[ nCol, _COL_COLUMN ], PadR( ::aFooters[ nRow, nCol ], nWidth ), ::getColor( ColorToDisp( ::aColsInfo[ nCol, _COL_OBJ ]:DefColor, TBC_CLR_HEADING ) - 1 ) )

            EndIf

         Next

      Next

      If ::lFootSep
         // Draw SEP

         nRow := ::nClientBottom - Len( ::aFooters )
         nPos := ::nClientLeft

         For nCol := 1 To Len( ::aColsInfo )
            // Draw Footer line

            If nCol <= ::freeze .Or. ( nCol >= ::leftVisible() .And. nCol <= ::rightVisible() )

               If lFirst

                  If nCol == 1
                     nPosSep := Len( ::aColsInfo[ nCol, _COL_FOOT_SEP ] )
                  Else
                     nPosSep := Min( ::aColsInfo[ nCol, _COL_SEP_WIDTH ] + 1, Len( ::aColsInfo[ nCol, _COL_FOOT_SEP ] ) )
                  EndIf

               Else

                  // Draw columns
                  If ::freeze > 0 .And. ::leftVisible() == nCol

                     nPosSep := 1
                     cSep    := Left( ::aColsInfo[ ::freeze + 1, _COL_FOOT_SEP ], ::aColsInfo[ ::freeze + 1, _COL_SEP_WIDTH ] )
                     nPosSep += Len( cSep )
                     cSep    := PadR( cSep, ::aColsInfo[ ::freeze + 1, _COL_SEP_WIDTH ], Right( cSep, 1 ) )
                     DispOutAt( nRow, nPos, cSep, ::getColor() )
                     nPos += Len( cSep )

                  Else

                     nPosSep := 1
                     cSep    := Left( ::aColsInfo[ nCol, _COL_FOOT_SEP ], ::aColsInfo[ nCol, _COL_SEP_WIDTH ] )
                     nPosSep += Len( cSep )
                     cSep    := PadR( cSep, ::aColsInfo[ nCol, _COL_SEP_WIDTH ], Right( cSep, 1 ) )
                     DispOutAt( nRow, nPos, cSep, ::getColor() )
                     nPos += Len( cSep )

                  EndIf

               EndIf

               If nCol > 1
                  nPosSep   := Min( nPosSep, Len( ::aColsInfo[ nCol, _COL_FOOT_SEP ] ) )
                  nWidthSep := ::aColsInfo[ nCol, _COL_COLUMN ] - nPos
                  DispOutAt( nRow, nPos, Replicate( SubStr( ::aColsInfo[ nCol, _COL_FOOT_SEP ], nPosSep, 1 ), nWidthSep ), ::getColor() )
                  nPos += nWidthSep
               EndIf

               lFirst := .F.

               nWidthSep := ::aColsInfo[ nCol, _COL_COLUMN ] + ::aColsInfo[ nCol, _COL_WIDTH ] - nPos
               If nCol == ::rightVisible
                  nWidthSep := ::nClientRight - nPos + 1
               EndIf
               cSep := PadR( SubStr( ::aColsInfo[ nCol, _COL_FOOT_SEP ], nPosSep ), nWidthSep, Right( ::aColsInfo[ nCol, _COL_FOOT_SEP ], 1 ) )
               DispOutAt( nRow, nPos, cSep, ::getColor() )
               nPos += Len( cSep )

            EndIf

         Next

      EndIf

   EndIf

Return( NIL )


Method moved() Class TBrowse

   ::HitTop    := .F.
   ::HitBottom := .F.

   // autoLite?
   If ::Stable
      if ::autoLite
         ::deHilite()
      else
         ::PosCursor()
      endif
      ::stable := .F.
   EndIf

Return Self


// determine column width...
Method getColWidth( nCol, oCol, xVal )

   Local nColWidth
   Local aLabel
   Local nAux


   If oCol:width <> NIL

      nColWidth := oCol:Width

   Else

      nColWidth := Len( Transform( xVal, oCol:Picture ) ) // Len( xVal )
      If oCol:heading != NIL

         For Each aLabel In ::aHeaders

            nAux := Len( aLabel[ nCol ] )
            If nAux > nColWidth
               nColWidth := Min( nAux, ::widthClient() )
            EndIf

         Next

      EndIf

      If oCol:footing != NIL

         For Each aLabel In ::aFooters

            nAux := Len( aLabel[ nCol ] )
            If nAux > nColWidth
               nColWidth := Min( nAux, ::widthClient() )
            EndIf

         Next

      EndIf

   EndIf


Return( nColWidth )



Method dispCell( nRow, nCol, nColor ) Class TBrowse

   Local cColor
   Local oCol   := ::aColsInfo[ nCol, _COL_OBJ ]
   Local nWidth

   If nRow > 0 .And. nCol > 0

      If ::aColorRect[ nRow, nCol ] == NIL

         If oCol:ColorBlock == NIL

            cColor := ::getColor( ColorToDisp( oCol:DefColor, nColor ) - 1 )

         Else

            cColor := ::getColor( ColorToDisp( Eval( oCol:ColorBlock, ::aColsData[ nRow, nCol ], nRow, nCol ), nColor ) - 1 )

         EndIf

      Else

         cColor := ::getColor( ColorToDisp( ::aColorRect[ nRow, nCol ], nColor ) - 1 )

      EndIf

      If ::aColsInfo[ nCol, _COL_COLUMN ] + ::aColsInfo[ nCol, _COL_CELL_WIDTH ] <= ::nClientRight
         nWidth := ::aColsInfo[ nCol, _COL_CELL_WIDTH ]
      Else
         nWidth := ::nClientRight - ::aColsInfo[ nCol, _COL_COLUMN ] + 1
      EndIf

      DispOut( PadR( Transform( ::aColsData[ nRow, nCol ], ::aColsInfo[ nCol, _COL_PICTURE ] ), nWidth ), cColor )

   EndIf

Return( NIL )



Method visibleColsDetermine() Class TBrowse

   Local nLoop
   Local nWidthCols   := 0
   Local nWidthFreeze := 0
   Local nWidthNext
   Local lAjustado    := .F.
   Local nSpaceLeft
   Local nWidthAnt
   Local nColPosAnt
   Local nStep


   // Calcula a largura das colunas fixas...
   For nLoop := 1 To ::freeze
      nWidthFreeze += ::aColsInfo[ nLoop, _COL_WIDTH ] + ::aColsInfo[ nLoop + 1, _COL_SEP_WIDTH ]
   Next

   If ::colPos < ::nLeftVisible .And. ::colPos > ::freeze
      ::nLeftVisible  := ::colPos
      ::nRightVisible := 0
   ElseIf ::colPos < ::nLeftVisible .And. ::colPos <= ::freeze
      ::nLeftVisible  := ::freeze + 1
      ::nRightVisible := 0
   ElseIf ::nRightVisible > 0 .And. ::colPos > ::nRightVisible
      ::nLeftVisible  := 0
      ::nRightVisible := ::colPos
   ElseIf ::nRightVisible == 0 .And. ::nLeftVisible == 0
      ::nLeftVisible := ::freeze + 1
   EndIf

   For nStep := 1 To 2

      // Verifica colunas visiveis...
      If ::nRightVisible == 0
         // Deslocamento para esquerda mantendo coluna selecionada

         nWidthCols      := nWidthFreeze + ::aColsInfo[ ::nLeftVisible, _COL_WIDTH ]
         ::nRightVisible := ::nLeftVisible

         While ( ::nRightVisible < ::colCount() )

            // Size of right column
            nWidthNext := ::aColsInfo[ ::nRightVisible + 1, _COL_SEP_WIDTH ] + ::aColsInfo[ ::nRightVisible + 1, _COL_WIDTH ]

            If ( nWidthCols + nWidthNext <= ::widthClient() )
               nWidthCols += nWidthNext
               ::nRightVisible++
            Else
               Exit
            EndIf

         EndDo

         If nStep == 1
            ::nLeftVisible := 0
         Else
            lAjustado := .T.
         EndIf

      ElseIf ::nLeftVisible == 0
         // Deslocamento para direita mantendo coluna selecionada

         nWidthCols     := nWidthFreeze + ::aColsInfo[ ::nRightVisible, _COL_WIDTH ]
         ::nLeftVisible := ::nRightVisible

         While ( ::nLeftVisible > ::freeze + 1 )

            // Size of left column
            nWidthNext := ::aColsInfo[ ::nLeftVisible, _COL_SEP_WIDTH ] + ::aColsInfo[ ::nLeftVisible - 1, _COL_WIDTH ]

            If ( nWidthCols + nWidthNext <= ::widthClient() )
               nWidthCols += nWidthNext
               ::nLeftVisible--
            Else
               Exit
            EndIf

         EndDo
         If nStep == 1
            ::nRightVisible := 0
         Else
            lAjustado := .T.
         EndIf
      EndIf
   Next

   If lAjustado

      If ::colPos > ::nRightVisible
         ::colPos := ::nRightVisible
      ElseIf ::colPos < ::nLeftVisible .And. ::nLeftVisible > ::freeze + 1
         ::colPos := ::nLeftVisible
      EndIf

      // Determinar as posicoes onde as colunas serao exibidas...
      // Coloca espacos a esquerda da coluna leftVisible() e a direita de rightVisible()
      nWidthCols := nWidthFreeze
      For nLoop := ::nLeftVisible To ::nRightVisible

         If nLoop > ::nLeftVisible
            nWidthCols += ::aColsInfo[ nLoop, _COL_SEP_WIDTH ]
         EndIf

         nWidthCols += ::aColsInfo[ nLoop, _COL_WIDTH ]

      Next

      If ( ( nSpaceLeft := Int( ( ::widthClient() - nWidthCols ) / 2 ) ) < 0 )
         nSpaceLeft := 0
      EndIf

      nWidthAnt  := 0
      nColPosAnt := ::nClientLeft
      For nLoop := 1 To ::nRightVisible

         If nLoop <= ::freeze

            ::aColsInfo[ nLoop, _COL_COLUMN ] := nColPosAnt + nWidthAnt
            nWidthAnt := ::aColsInfo[ nLoop, _COL_WIDTH ] + ::aColsInfo[ nLoop + 1, _COL_SEP_WIDTH ]
            nColPosAnt := ::aColsInfo[ nLoop, _COL_COLUMN ]

         ElseIf nLoop >= ::nLeftVisible .And. nLoop <= ::nRightVisible

            If nLoop == ::nLeftVisible

               ::aColsInfo[ nLoop, _COL_COLUMN ] := nColPosAnt + nWidthAnt + nSpaceLeft
               nWidthAnt := ::aColsInfo[ nLoop, _COL_WIDTH ]

            Else

               ::aColsInfo[ nLoop, _COL_COLUMN ] := nColPosAnt + nWidthAnt + ::aColsInfo[ nLoop, _COL_SEP_WIDTH ]
               nWidthAnt := ::aColsInfo[ nLoop, _COL_WIDTH ]

            EndIf

            nColPosAnt := ::aColsInfo[ nLoop, _COL_COLUMN ]
         EndIf

      Next

      AFill( ::aRedraw , .T. )

   EndIf

Return( NIL )


Method posCursor() Class TBrowse

   Local nRow := ::rowPos + ::nRowIni + ::nClientTop - 1
   Local nCol := ::aColsInfo[ ::colPos, _COL_COLUMN ]

   SetPos( nRow, nCol )

Return( NIL )


Method refreshData( nRow, lClear ) Class TBrowse

   Local nCol
   Local xData

   If lClear
      For Each xData In ::aColsData[ nRow ]
         nCol  := xData:__enumIndex()
         ::aColorRect[ nRow, nCol ] := NIL
         xData := ""
      Next
   Else
      For Each xData In ::aColsData[ nRow ]
         nCol  := xData:__enumIndex()
         xData := Eval( ::aColsInfo[ nCol, _COL_OBJ ]:block )
         ::aColorRect[ nRow, nCol ] := NIL
      Next
   EndIf

Return( NIL )


Method scrollRows( nSkip ) Class TBrowse

   Local nLoop

   If nSkip < 0

      nSkip := Abs( nSkip )
      If nSkip > ::rowCount()
         nSkip := ::rowCount()
      EndIf

      Scroll( ::nClientTop + ::nRowIni, ::nClientLeft, ::nClientTop + ::nRowIni + ::rowCount() - 1, ::nClientRight, -nSkip  )
      nLoop := ::rowCount()
      While nLoop > 0

         If nLoop <= nSkip

            ::aColorRect[ nLoop ] := Array( ::colCount() )
            AFill( ::aColorRect[ nLoop ], NIL )

            ::aColsData[ nLoop ] := Array( ::colCount() )
            AFill( ::aColsData[ nLoop ], "" )
            ::aRedraw[ nLoop ]  := .T.
            ::aRefresh[ nLoop ] := .T.

         ElseIf nLoop > 1

            ::aColorRect[ nLoop ] := ::aColorRect[ nLoop - nSkip ]
            ::aColsData[ nLoop ]  := ::aColsData[ nLoop - nSkip ]
            ::aRedraw[ nLoop ]    := ::aRedraw[ nLoop - nSkip ]
            ::aRefresh[ nLoop ]   := ::aRefresh[ nLoop - nSkip ]

         EndIf

         nLoop--

      EndDo

   ElseIf nSkip > 0

      If nSkip > ::rowCount()
         nSkip := ::rowCount()
      EndIf

      Scroll( ::nClientTop + ::nRowIni, ::nClientLeft, ::nClientTop + ::nRowIni + ::rowCount() - 1, ::nClientRight, nSkip  )
      For nLoop := 1 To ::rowCount()

         If nLoop > ::rowCount() - nSkip

            ::aColorRect[ nLoop ] := Array( ::colCount() )
            AFill( ::aColorRect[ nLoop ], NIL )

            ::aColsData[ nLoop ] := Array( ::colCount() )
            AFill( ::aColsData[ nLoop ], "" )
            ::aRedraw[ nLoop ]  := .T.
            ::aRefresh[ nLoop ] := .T.

         ElseIf nLoop < ::rowCount()

            ::aColorRect[ nLoop ] := ::aColorRect[ nLoop + nSkip ]
            ::aColsData[ nLoop ]  := ::aColsData[ nLoop + nSkip ]
            ::aRedraw[ nLoop ]    := ::aRedraw[ nLoop + nSkip ]
            ::aRefresh[ nLoop ]   := ::aRefresh[ nLoop + nSkip ]

         EndIf

      Next

   EndIf

Return( NIL )

Method getColor( nColor ) Class TBrowse

   Local cColor := ::colorSpec

   If nColor != NIL
      cColor := hb_ColorIndex( cColor, nColor )
      If Empty( cColor )
         cColor := hb_ColorIndex( "W/N,N/W,N/N,N/N,N/W,N/W,W+/N,N/W,W+/W,W/N,W+/N", nColor )
      EndIf
   EndIf

Return( cColor )


Static Function GetLabels( cLabels )

   Local aLabels := {}
   Local nPos
   Local cSubStr

   If cLabels != NIL

      cSubStr := cLabels
      While ( ( nPos := At( ";", cSubStr ) ) > 0 )
         AAdd( aLabels, Left( cSubStr, nPos - 1 ) )
         cSubStr := SubStr( cSubStr, nPos + 1 )
      EndDo

      If Len( cSubStr ) > 0
         AAdd( aLabels, cSubStr )
      EndIf

   EndIf

Return( aLabels )

Static Function ColorToDisp( aColor, nColor )

   If aColor != NIL .And. Len( aColor ) >= nColor
      Return aColor[ nColor ]
   EndIf

Return( { 1, 2, 1, 1 }[ nColor ] )


function TBMOUSE( oBrowse, nMouseRow, nMouseCol )

   Local n

   If oBrowse:hittest( nMouseRow, nMouseCol ) == HTCELL

      n := oBrowse:mrowpos - oBrowse:rowpos

      While ( n < 0 )
         n++
         oBrowse:up()
      EndDo

      While ( n > 0 )
         n--
         oBrowse:down()
      EndDo

      n := oBrowse:mcolpos - oBrowse:colpos

      While ( n < 0 )
         n++
         oBrowse:left()
      EndDo

      While ( n > 0 )
         n--
         oBrowse:right()
      EndDo

      Return( 0 )

   EndIf

Return( 1 )


//-------------------------------------------------------------------//
//
//                   Function to Activate TBrowse
//
//-------------------------------------------------------------------//

Function TBrowseNew( nTop, nLeft, nBottom, nRight )
Return( TBrowse():New( nTop, nLeft, nBottom, nRight ) )
