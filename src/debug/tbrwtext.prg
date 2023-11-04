/*
 * Text file browser class
 *
 * Copyright 2008 Lorenzo Fiorini <lorenzo.fiorini@gmail.com>
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
#include "common.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"


// Color definitions and positions inside ::cColorSpec
#define  CLR_CODE       0        // color of code
#define  CLR_CURSOR     1        // color of highlighted line (the line to be executed)
#define  CLR_BKPT       2        // color of breakpoint line
#define  CLR_HIBKPT     3        // color of highlighted breakpoint line


CLASS HBBrwText FROM HBDbBrowser

   DATA   cFileName      // the name of the browsed file
   DATA   nActiveLine    // Active line inside Code Window (the line to be executed)

   DATA   aBreakPoints   // Array with line numbers of active Break Points

   DATA   lLineNumbers   // If .T. source code lines are preceded by their number
   DATA   aText          // Text from cFileName
   DATA   nCurRow       // Current row
   DATA   nWidth
   DATA   nColIni
   DATA   nMargin
   DATA   nMaxRow
   DATA   lCodeBlock

   METHOD New(nTop, nLeft, nBottom, nRight, cFileName, cColor, lLineNumbers)

   METHOD Resize( nTop, nLeft, nBottom, nRight )

   METHOD GotoLine(n)                      // Moves active line cursor
   METHOD SetActiveLine( n )               // Sets the line to be executed

   METHOD GetLine(nRow)                    // Redefine HBEditor method to add line number
   METHOD LineColor(nDataRow, nRow)        // Redefine HBEditor method to handle line coloring

   METHOD ToggleBreakPoint(nRow, lSet)     // if lSet is .T. there is a BreakPoint active at nRow,
                                           // if lSet is .F. BreakPoint at nRow has to be removed
   METHOD Search( cString, lCaseSensitive, nMode ) // 0 from Begining to end, 1 Forward, 2 Backwards

   METHOD LoadFile(cFileName,nTab)

   METHOD Left()                InLine  If( ::nColIni > 1, ::nColIni--, NIL ), ::RefreshAll(), Self
   METHOD Right()               InLine  ::nColIni++, ::RefreshAll(), Self
   METHOD Home()                InLine  ::nColIni := 1, ::RefreshAll(), Self
   METHOD End()                 InLine  ::nColIni := ::FirstColMaxRight(), ::RefreshAll(), Self
   METHOD FirstColMaxRight()

ENDCLASS


METHOD New(nTop, nLeft, nBottom, nRight, cFileName, cColor, lLineNumbers) CLASS HBBrwText

   Local oCol

   cColor       := hb_defaultValue( cColor, SetColor() )
   lLineNumbers := hb_defaultValue( lLineNumbers, .T. )

   ::Super:New(nTop, nLeft, nBottom, nRight)

   ::cFileName     := cFileName
   ::nActiveLine   := 1
   ::nCurRow       := 1
   ::aText         := {}
   ::nWidth        := nRight - nLeft + 1
   ::nColIni       := 1
   ::nMargin       := 0
   ::nMaxRow       := 0
   ::lCodeBlock    := .F.

   ::aBreakPoints  := {}

   ::lLineNumbers  := lLineNumbers


   ::GoTopBlock    := { || ::nCurRow := 1 }
   ::GoBottomBlock := { || ::nCurRow := Len( ::aText ) }
   ::SkipBlock     := { | nSkip, nOld | nOld := ::nCurRow,;
                                        ::nCurRow += nSkip,;
                                        ::nCurRow := Min( Max( ::nCurRow, 1 ),;
                                        ::nMaxRow ), ::nCurRow - nOld }

   ::AddColumn( oCol := TBColumnNew( "", { || ::GetLine( ::nCurRow ) } ) )
   oCol:ColorBlock := { | data, row | HB_SYMBOL_UNUSED( data ), ::LineColor( ::nCurRow, row ) }

   ::ColorSpec := cColor

   ::LoadFile(cFileName)

return Self


METHOD LoadFile(cFileName, nTab) CLASS HBBrwText

   nTab := hb_defaultValue( nTab, 3 )
   ::aText := {}

   EvalTxtFile( cFileName, { | cLine | AAdd( ::aText, StrTran( cLine, Chr( K_TAB ), Space(nTab) ) ), .T. } )

   ::cFileName := cFileName
   ::nMaxRow     := Len( ::aText )
   ::nMargin     := Len( AllTrim( Str( ::nMaxRow ) ) ) + 2
   ::nActiveLine := 0

   ::GoTop()

   If Len( ::aText ) < ::rowCount()
      ::nMaxRow := ::rowCount()
   EndIf

return Self


/* This method is to restore correct cursor position after Super:Resize() */
METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBBrwText

   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight
   ::nWidth  := nRight - nLeft + 1
   // ::GotoLine( ::nCurRow )

RETURN Self


METHOD SetActiveLine( n ) CLASS HBBrwText
   ::nActiveLine := n
   ::RefreshAll()
return Self


METHOD GotoLine(n) CLASS HBBrwText

  local nAux

  DispBegin()
  If n != NIL .and. n > 0

     nAux := n - ::nCurRow + 1

     If nAux > 0 .And. nAux <= ::rowCount

        ::rowPos := nAux

     Else

        ::rowPos  := 1
        ::RefreshAll()
        ::ForceStable()
        ::nCurRow := Max( n - Round( ::rowCount() / 2, 0 ), 1 )
        If Len( ::aText ) >= ::rowCount()
           If ::nCurRow + ( ::rowCount() - 1 ) > Len( ::aText )
              ::nCurRow := Max( Len( ::aText ) - ::rowCount() + 1, 1 )
           EndIf
        Else
           ::nMaxRow := ::nCurRow + ::rowCount() - 1
           ::nMargin := Len( AllTrim( Str( ::nMaxRow ) ) ) + 2
        EndIf

        ::rowPos  += ( n - ::nCurRow )

     EndIf

  Else
     ::nCurRow := 1
     ::nMaxRow := ::rowCount()
     ::nMargin := Len( AllTrim( Str( ::nMaxRow ) ) ) + 2
  EndIf

  // ::RefreshAll()
  // ::ForceStable()

  DispEnd()

return Self

METHOD GetLine(nRow) CLASS HBBrwText

   If ::lCodeBlock .And. nRow == ::nActiveLine
      Return( PadR( SubStr( If(::lLineNumbers, PadR( AllTrim(Str(nRow)) + ":", ::nMargin ), "") + "{|| ... } " + If( nRow > 0 .And. nRow <= Len( ::aText ), ::aText[ nRow ], "" ), ::nColIni ), ::nWidth ) )
   Else
      Return( PadR( SubStr( If(::lLineNumbers, PadR( AllTrim(Str(nRow)) + ":", ::nMargin ), "") + If( nRow > 0 .And. nRow <= Len( ::aText ), ::aText[ nRow ], "" ), ::nColIni ), ::nWidth ) )
   EndIf

Return ""

METHOD LineColor( nDataRow, nRow ) CLASS HBBrwText

   local nNormalColor
   local nSelColor

   // ::cCurRow -> 22
   // ::rowPos -> 9
   if AScan( ::aBreakPoints, nDataRow ) > 0
      nNormalColor := 3
      if nRow == ::rowPos .And. nDataRow != ::nActiveLine
         nSelColor := 6
      else
         nSelColor := 3
      endif   
   else
      nNormalColor := 1
      if nRow == ::rowPos .And. nDataRow != ::nActiveLine
         nSelColor := 5
      else
         nSelColor := 1
      endif
   endif

   if nDataRow == ::nActiveLine
      nNormalColor += 1
      nSelColor    += 1
   endif

return( { nNormalColor, nSelColor } )


METHOD ToggleBreakPoint( nRow, lSet ) CLASS HBBrwText

   local nAt := AScan( ::aBreakPoints, nRow )

   if lSet
      // add it only if not present
      if nAt == 0
         AAdd( ::aBreakPoints, nRow )
      endif

   elseif nAt != 0

      hb_ADel( ::aBreakPoints, nAt, .T. )

   endif

return Self

METHOD Search( cString, lCaseSensitive, nMode ) CLASS HBBrwText

   local nFrom, nTo, nFor
   local lFound

   lCaseSensitive := hb_defaultValue( lCaseSensitive, .f. )
   nMode          := hb_defaultValue( nMode, 0 )

   lFound := .f.

   if !lCaseSensitive
      cString := Upper( cString )
   endif

   do case
   case nMode == 0 // From Top
      nFrom := 1
      nTo   := Len( ::aText )
   case nMode == 1 // Forward
      nFrom := Min( ::nCurRow + 1, Len( ::aText ) )
      nTo   := Len( ::aText )
   case nMode == 2 // Backward
      nFrom := Max( ::nCurRow - 1, 1 )
      nTo   := 1
   end case

   for nFor := nFrom to nTo
      // Alert( AllTrim( Str( nFor ) ) + "[" + cString + "]:" + ::GetLine( nFor ) )
      if cString $ iif( lCaseSensitive, ::GetLine( nFor ), Upper( ::GetLine( nFor ) ) )
         lFound := .t.
         ::GotoLine( nFor )
         exit
      endif
   next

return lFound

METHOD FirstColMaxRight() CLASS HBBrwText

   Local nFirstCol := 1
   Local nLoop
   Local nAux

   For nLoop := ::nCurRow - ::rowPos + 1 To ::nCurRow + ::rowCount() - ::rowPos

      If nLoop > 0 .And. nLoop <= Len( ::aText )

         nAux := Len( ::aText[ nLoop ] ) - ( ::nWidth - If( ::lLineNumbers, ::nMargin, 0 ) ) + 1
         If nAux > nFirstCol
            nFirstCol := nAux
         EndIf

      EndIf

   Next

Return( nFirstCol )

#PRAGMA BEGINDUMP

#include "hbapi.h"
#include "hbvmpub.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbapierr.h"
#include "hbstack.h"
#include "hbapifs.h"

#define MAX_BLOCO 16384 // 8192 // 32768

HB_FUNC( EVALTXTFILE )
{
   PHB_ITEM   phbFile     = hb_param( 1 , HB_IT_STRING );
   PHB_ITEM   phbBlock    = hb_param( 2 , HB_IT_BLOCK  );
   HB_BOOL    lContinue   = HB_TRUE;
   HB_BOOL    lRead       = HB_FALSE;
   HB_BOOL    lEmpty      = HB_TRUE;
   int        nBytesRead;
   int        nPos;
   int        nRowLen   = 0;
   HB_FHANDLE hFile;
   PHB_ITEM   phbRow;
   PHB_ITEM   pReturn;
   char       pBuffer[ MAX_BLOCO ];
   char       pRow[ MAX_BLOCO ];

   if ( phbFile && phbBlock )
   {
      // Abre o arquivo
      hFile = hb_fsOpen( (const char *) hb_itemGetCPtr( phbFile ), 0 );

      if ( hFile > 0 )
      {

         phbRow = hb_itemNew( NULL );

         // Le o arquivo em blocos de MAX_BLOCO bytes
         while ( lContinue && ( nBytesRead = hb_fsRead( hFile, pBuffer, MAX_BLOCO ) ) > 0 )
         {

            lEmpty = HB_FALSE;
            nPos = 0;

            while ( lContinue && nPos < nBytesRead )
            {
               // Verifica se a string nao extrapola o tamanho maximo da linha...
               if ( nRowLen < MAX_BLOCO )
               {
                  while( nPos < nBytesRead && pBuffer[ nPos ] != '\n' )
                  {
                     pRow[ nRowLen++ ] = pBuffer[ nPos++ ];
                  }

                  // Verifica se nao chegou no final do buffer sem encontrar algum caracter de final de linha
                  if ( nPos < nBytesRead )
                  {

                     if ( nRowLen > 0 && pRow[ nRowLen - 1 ] == '\r' )
                     {
                        nRowLen--;
                     }

                     pRow[ nRowLen ] = '\0';

                     hb_itemPutC( phbRow, pRow );

                     // Executa o codeblock
                     pReturn = hb_vmEvalBlockV( phbBlock, 1, phbRow ); // Executa o CodeBlock passado para a funcao

                     hb_itemClear( phbRow );

                     // Verifica o retorno do codeblock
                     lContinue = hb_itemGetL( pReturn );

                     nPos++;

                     // Nova linha...
                     nRowLen = 0;
                  }
               }
               else
               {
                  // Gerar um erro aqui... Linha maior que MAX_BLOCO
                  lContinue = HB_FALSE;
               }
            }

         }

         // Ultima linha do arquivo sem Chr(13) e chr( 10 )
         if ( ! lEmpty )
         {
            hb_itemPutCL( phbRow, pRow, nRowLen );
            hb_vmEvalBlockV( phbBlock, 1, phbRow ); // Executa o CodeBlock passado para a funcao
            hb_itemClear( phbRow );
         }
 
         hb_itemRelease( phbRow );
         hb_fsClose( hFile );
         lRead = HB_TRUE;
      }

   }

   hb_retl( lRead );
}

#PRAGMA ENDDUMP

