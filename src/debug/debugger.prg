/*
 * The Debugger
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
 * Copyright 2003-2006 Phil Krylov <phil@newstar.rinet.ru>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

// #pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject class */
#include "hbclass.ch"

#include "hbdebug.ch"   /* for "nMode" of __dbgEntry() */
#include "hbgtinfo.ch"
#include "hbmemvar.ch"

#include "box.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "directry.ch"

#define HB_DBG_CS_STACKINDEX  8

/* Public or Private created in ::LoadVars(), value stored in HB_DBG_VAR_INDEX */
#define HB_DBG_VAR_MVALUE       HB_DBG_VAR_INDEX

/* Information structure stored in ::aWatch (watchpoints) */
#define WP_TYPE                 1  // wp: watchpoint, tr: tracepoint
#define WP_EXPR                 2  // source of an expression

#define WP_VAR_SCOPE            1
#define WP_VAR_VALUE            2 

/* The dimensions of the debugger window */
#define DEBUGGER_MINROW         0
#define DEBUGGER_MINCOL         0
#define DEBUGGER_MAXROW         22
#define DEBUGGER_MAXCOL         77

#define GET_BUFFER_SIZE         4096

/* command window scroll history */
#define DEBUGGER_CMDHIST_SIZE   128

#define VS_ICON   0
#define VS_NORMAL 1
#define VS_ZOOM   2

#ifdef __PLATFORM__LINUX
   #define WINDOW_HEIGHT HB_GTInfo(HB_GTI_VIEWMAXHEIGHT)
   #define WINDOW_WIDTH  HB_GTInfo(HB_GTI_VIEWMAXWIDTH)
   #define TRACE_KEY_NAME "F12"
   #define TRACE_KEY      K_F12
#else
   #define WINDOW_HEIGHT  HB_GTInfo(HB_GTI_VIEWPORTHEIGHT)
   #define WINDOW_WIDTH   HB_GTInfo(HB_GTI_VIEWPORTWIDTH)
   #define TRACE_KEY_NAME "F10"
   #define TRACE_KEY      K_F10
#endif


THREAD STATIC t_oDebugger

/* debugger entry point */
PROCEDURE __dbgEntry( nMode, uParam1, uParam2, uParam3, uParam4 )

   LOCAL lStartup

   SWITCH nMode
   CASE HB_DBG_ACTIVATE

      IF ( lStartup := ( t_oDebugger == NIL ) )
         t_oDebugger := HBDebugger():New()
         t_oDebugger:pInfo := uParam1
      ENDIF
      t_oDebugger:nProcLevel := uParam2
      t_oDebugger:aCallStack := uParam3
      t_oDebugger:aModules := uParam4
      IF lStartup
         IF t_oDebugger:lRunAtStartup
            __dbgSetGo( uParam1 )
            RETURN
         ENDIF
      ENDIF
      t_oDebugger:lGo := .F.
      t_oDebugger:Activate()
      RETURN

   CASE HB_DBG_GETENTRY

      __dbgSetEntry()

   ENDSWITCH

   RETURN

CREATE CLASS HBDebugger

   VAR pInfo
   VAR aWindows          INIT {}
   VAR nCurrentWindow    INIT 1
   VAR oCurrentWnd
   VAR oPullDown

   VAR oWndCode
   VAR oWndCommand
   VAR oWndStack
   VAR oWndVars

   VAR oBrwText
   VAR cPrgName
   VAR oBrwStack
   VAR oBrwVars
   VAR aVars             INIT {}

   VAR nAppDispCount

   VAR nAppDirCase
   VAR nAppFileCase
   VAR nAppTypeAhead
   VAR nAppLastKey

   VAR nMaxRow
   VAR nMaxCol

   VAR hUserWindow
   VAR hDebuggerWindow
   VAR lDebuggerWindowIsOpen INIT .F.

   VAR aCallStack        INIT {}    // stack of procedures with debug info
   VAR aProcStack        INIT {}    // stack of all procedures
   VAR nProcLevel                   // procedure level where the debugger is currently
   VAR aModules          INIT {}    // array of modules with static and GLOBAL variables
   VAR aWatch            INIT {}
   VAR aColors           INIT { "W+/BG", "N/BG", "R/BG", "N+/BG", "W+/B", "GR+/B", "W/B", "N/W", "R/W", "N/BG", "R/BG", "N/W", "R/W" }

   VAR aHistCommands
   VAR aLastCommands
   VAR nCommand
   VAR oGetCommand

   VAR lAnimate          INIT .F.
   VAR lEnd              INIT .F.
   VAR lCaseSensitive    INIT .F.
   VAR lMonoDisplay      INIT .F.
   VAR lSortVars         INIT .F.

   VAR cSearchString     INIT ""
   VAR cPathForFiles
   VAR cSettingsFileName INIT "init.cld"
   VAR aPathDirs

   VAR nTabWidth         INIT 3
   VAR nSpeed            INIT 0

   VAR lShowPublics      INIT .F.
   VAR lShowPrivates     INIT .F.
   VAR lShowStatics      INIT .F.
   VAR lShowLocals       INIT .F.
   VAR lShowGlobals      INIT .F.
   VAR lShowAllGlobals   INIT .F.
   VAR lAll              INIT .F.
   VAR lShowCallStack    INIT .F.
   VAR lGo                          // stores if GO was requested
   VAR lCBTrace          INIT .T.   // stores if codeblock tracing is allowed
   VAR oBrwPnt
   VAR oWndPnt
   VAR lPPO              INIT .F.
   VAR lRunAtStartup     INIT .T.   // Clipper compatible
   VAR lLineNumbers      INIT .T.
   VAR nHelpPage         INIT 1
   VAR nWaFocus          INIT 1
   VAR nCmdWndHight      INIT 3
   VAR lWindowsAutoSized INIT .T.
   VAR lExchangeScreens  INIT .T.
   VAR lMenuBar          INIT .T.
   VAR lTiled            INIT .T.
   VAR lChangeScreen     INIT .T.
   VAR bAppInkeyAfter    
   VAR bAppInkeyBefore   
   VAR nAppRow           INIT 0
   VAR nAppCol           INIT 0
   VAR cKeyboardBuffer   INIT ""
   VAR cAppColors        INIT ""
   var cAppScreen
   
   METHOD New()
   METHOD Activate()

   METHOD All()

   METHOD Animate() INLINE iif( ::lAnimate, ::Step(), NIL )

   METHOD BarDisplay()
   METHOD BuildVarsWindow()
   METHOD BuildWatchWindow()
   METHOD BuildStackWindow()
   METHOD BuildCommandWindow()
   METHOD BuildBrowseStack()
   METHOD ResizeCmdWnd( nLines )

   METHOD CallStackProcessKey( nKey )
   METHOD ClrModal() INLINE iif( ::lMonoDisplay, "N/W, W+/W, W/N, W+/N", "N/W, R/W, N/BG, R/BG" )
   METHOD GetColors() INLINE iif( ! ::lMonoDisplay, ::aColors, { "W+/N", "W+/N", "N/W", "N/W", ;
                                                                 "N/W", "N/W", "W+/N", "N/W", ;
                                                                 "W+/W", "W/N", "W+/N", "W/N", "W/N" } )
   METHOD CodeblockTrace()
   METHOD ExchangeScreens()
   METHOD CodeWindowProcessKey( nKey )
   METHOD Colors()
   METHOD CommandWindowProcessKey( nKey )
   METHOD CommandWindowDisplay( cLine, lCmd )
   METHOD DoCommand( cCommand )
   METHOD DoScript( cFileName )
   METHOD EditColor( nColor, oBrwColors )
   METHOD EditSet( nSet, oBrwSets )
   METHOD EditVar( nVar )
   METHOD Exit() INLINE ::lEnd := .T.
   METHOD FindNext()
   METHOD FindPrevious()
   METHOD GetExprValue( xExpr, lValid )
   METHOD GetSourceFiles()
   METHOD ModuleMatch( cModuleName1, cModuleName2 )

   METHOD Global()

   METHOD Go()
   METHOD GoToLine( nLine )
   METHOD HandleEvent()
   METHOD Hide()
   METHOD HideCallStack()
   METHOD HideVars()
   METHOD EditValue( cMsg, uValue, lEditable )
   METHOD InputBox( cMsg, uValue, bValid, lEditable )
   METHOD Inspect( uValue, cValueName )
   METHOD IsValidStopLine( cName, nLine )
   METHOD ListBox( cCaption, aItems )
   METHOD LoadColors()
   METHOD LoadSettings()
   METHOD LoadVars()
   METHOD LoadCallStack()

   METHOD Local()

   METHOD Locate( nMode, cValue )

   METHOD MonoDisplay()
   METHOD MoveWindow( nMRow, nMCol )
   METHOD NextWindow()
   METHOD Open( cFileName )
   METHOD OpenMenu( cName )
   METHOD OpenPPO()
   METHOD Resume() INLINE ::ShowCodeLine( 1 )
   METHOD OSShell()
   METHOD PathForFiles( cPathForFiles )
   METHOD Iconize()
   METHOD Zoom()
   METHOD MenuBar()
   METHOD ShowMenuBar()
   METHOD Size( nMRow, nMCol )
   METHOD Tile()
  
   METHOD PrevWindow()
   METHOD Private()
   METHOD Public()
   METHOD Quit()
   METHOD RefreshVars()
   METHOD RestoreAppState()
   METHOD RestoreSettings( cFileName )
   METHOD RunAtStartup( lRunAtStartup )
   METHOD SaveAppState()
   METHOD SaveSettings( cFileName )
   METHOD Show()
   METHOD ShowAllGlobals()
   METHOD ShowAppScreen()
   METHOD ShowCallStack()
   METHOD ShowCodeLine( nProc )
   METHOD ShowHelp( cTopic )
   METHOD ShowVars()
   METHOD LocatePrgPath( cPrgName, aDirs )
   METHOD Sort() INLINE ASort( ::aVars,,, {| x, y | x[ HB_DBG_VAR_NAME ] < y[ HB_DBG_VAR_NAME ] } ), ;
                        ::lSortVars := .T., ::oBrwVars:RefreshAll(), ;
                        iif( ::oWndVars:lVisible, iif( ! ::lGo, ::oBrwVars:ForceStable(), NIL ), NIL )

   METHOD Speed() INLINE ::nSpeed := ::InputBox( "Step delay (in tenths of a second)", ::nSpeed )

   METHOD Stack( cParam )
   METHOD Static()

   METHOD Step()

   METHOD TabWidth() INLINE ::nTabWidth := ::InputBox( "Tab width", ::nTabWidth ), ;
                            ::oBrwText:RefreshAll()

   METHOD BreakPointToggle( nLine, cFileName )
   METHOD BreakPointDelete( cPos )
   METHOD BreakPointFunc( cFuncName )
   METHOD BreakPointList()

   METHOD Trace()

   METHOD ToCursor()
   METHOD NextRoutine()
   METHOD StepOut()
   METHOD ViewSets()
   METHOD WndVarsLButtonDown( nMRow, nMCol )
   METHOD WndPntLButtonDown( nMRow, nMCol )
   METHOD LineNumbers( lLineNumbers ) // Toggles numbering of source code lines
   METHOD SearchLine( cLine )
   METHOD ToggleAnimate()       INLINE ::oPullDown:GetItemByIdent( "ANIMATE" ):checked := ::lAnimate := ! ::lAnimate
   METHOD ToggleCaseSensitive() INLINE ::oPullDown:GetItemByIdent( "CASE" ):checked := ::lCaseSensitive := ! ::lCaseSensitive
   METHOD ShowWorkAreas()       INLINE __dbgShowWorkAreas( Self )

   METHOD TracepointAdd( cExpr )
   METHOD WatchpointAdd( cExpr )
   METHOD WatchpointDel( xPos )
   METHOD WatchpointsShow()
   METHOD WatchpointsHide()
   METHOD WatchpointEdit( nPos )
   METHOD WatchpointInspect( nPos )
   METHOD WatchGetInfo( nWatch )
   METHOD WatchpointList()

   METHOD VarGetInfo( aVar )
   METHOD VarGetValue( aVar )
   METHOD VarSetValue( aVar, uValue )

   METHOD NotSupported() INLINE __dbgAlert( "Not implemented yet!" )

   METHOD OpenDebuggerWindow()
   METHOD CloseDebuggerWindow()
   METHOD SaveAppScreen()
   METHOD RestoreAppScreen()
   METHOD RedisplayBreakPoints()
   METHOD SetFocusCodeWindow()
   
ENDCLASS


METHOD New() CLASS HBDebugger

   t_oDebugger := Self

   ::lGo := ::lRunAtStartup

   /* Store the initial screen dimensions for now */
   ::nMaxRow := WINDOW_HEIGHT
   ::nMaxCol := WINDOW_WIDTH

   ::oPullDown := __dbgBuildMenu( Self )

   ::oWndCode              := HBDbWindow():New( 1, 0, ::nMaxRow - ::nCmdWndHight - 3, ::nMaxCol )
   ::oWndCode:bKeyPressed  := {| nKey | ::CodeWindowProcessKey( nKey ) }
   ::oWndCode:lCloseButton := .F.
   ::oWndCode:SetFocus( .T. )
   ::oWndCode:bGotFocus    := { || SetCursor( SC_SPECIAL1 ) }
   ::oWndCode:bLostFocus   := { || SetCursor( SC_NONE ) }
   ::oWndCode:bPainted     := { || ::oBrwText:RefreshAll():ForceStable(), IIf( ::oWndCode:lFocused, SetCursor( SC_SPECIAL1 ), NIL ) }
   ::oWndCode:bLButtonDown := { || ::CodeWindowProcessKey( K_LBUTTONDOWN ) }
   ::oCurrentWnd           := ::oWndCode

   AAdd( ::aWindows, ::oWndCode )

   ::BuildVarsWindow()
   ::BuildWatchWindow()
   ::BuildStackWindow()
   ::BuildCommandWindow()

   ::oBrwText := HBBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                                  ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, "",;
                                  __DbgColors()[ 2 ]  + ", " + __DbgColors()[ 5 ]  + ", " + ;
                                  __DbgColors()[ 3 ]  + ", " + __DbgColors()[ 6 ]  + ", " + ;
                                  __DbgColors()[ 12 ] + ", " + __DbgColors()[ 13 ], ::lLineNumbers )

   ::oWndCode:Browser := ::oBrwText
   ::oWndCode:ToTop()

   IF hb_FileExists( ::cSettingsFileName )
      ::LoadSettings()
      ::lGo := ::lRunAtStartup // Once again after settings file is loaded
   ENDIF
   IF Empty( ::cPathForFiles )
      /* default the search path for files to the current directory
         that way if the source is in the same directory it will still be found even if the application
         changes the current directory with the SET DEFAULT command. */
      ::cPathForFiles := GetEnv( "HB_DBG_PATH" )
      IF Empty( ::cPathForFiles )
         ::cPathForFiles := GetEnv( "PATH" )
         ::aPathDirs := PathToArray( ::cPathForFiles )
      ELSE
         ::aPathDirs := PathsWithSubDirs( ::cPathForFiles )
      ENDIF
   ENDIF   

  RETURN Self

METHOD PROCEDURE OpenDebuggerWindow() CLASS HBDebugger

   IF ! ::lDebuggerWindowIsOpen
      ::hUserWindow := hb_gtInfo( HB_GTI_GETWIN )
      IF ::hDebuggerWindow == NIL
         ::hDebuggerWindow := hb_gtInfo( HB_GTI_GETWIN, { "Debugger", DEBUGGER_MINROW, DEBUGGER_MINCOL, DEBUGGER_MAXROW, DEBUGGER_MAXCOL } )
      ELSE
         hb_gtInfo( HB_GTI_SETWIN, ::hDebuggerWindow )
      ENDIF
      ::lDebuggerWindowIsOpen := .T.
   ENDIF

   RETURN


METHOD PROCEDURE CloseDebuggerWindow() CLASS HBDebugger

   IF ::lDebuggerWindowIsOpen
      ::hDebuggerWindow := hb_gtInfo( HB_GTI_GETWIN )
      hb_gtInfo( HB_GTI_SETWIN, ::hUserWindow )
      ::lDebuggerWindowIsOpen := .F.
   ENDIF

   RETURN

METHOD PROCEDURE SaveAppScreen() CLASS HBDebugger

   ::cAppScreen := __dbgSaveScreen( 0, 0, WINDOW_HEIGHT, WINDOW_WIDTH )

   RETURN

METHOD PROCEDURE RestoreAppScreen() CLASS HBDebugger

   If ::cAppScreen != Nil
      __dbgRestScreen( 0, 0, WINDOW_HEIGHT, WINDOW_WIDTH, ::cAppScreen )
   EndIf

   RETURN

METHOD PROCEDURE Activate() CLASS HBDebugger

   If ::lChangeScreen

      // Flush screen buffer
      ::nAppDispCount := DispCount()
      DispBegin()
      ::SaveAppScreen()
      ::SaveAppState()
      ::OpenDebuggerWindow()
      ::LoadCallStack()
      ::Show()
      __dbgFlushScreenBuffer()

   Else

      ::SaveAppScreen()
      ::SaveAppState()
      ::OpenDebuggerWindow()
      ::LoadCallStack()
      ::Show()
      ::nAppDispCount := __dbgFlushScreenBuffer() - 1
      If ::nAppDispCount < 0
         ::nAppDispCount := 0
      EndIf

   EndIf

   ::HandleEvent()

   If ::lExchangeScreens .Or. ::lGo

      ::lChangeScreen := .T.
      ::RestoreAppScreen()
      ::CloseDebuggerWindow()
      ::RestoreAppState()
      While ( ::nAppDispCount-- > 0 )
         DispBegin()
      EndDo


   Else

      ::lChangeScreen := .F.
      While ( ::nAppDispCount-- > 0 )
         DispBegin()
      EndDo
      DispBegin()
      ::RestoreAppScreen()
      ::CloseDebuggerWindow()
      ::RestoreAppState()

   EndIf

   RETURN


METHOD PROCEDURE All() CLASS HBDebugger

   ::lShowPublics := ::lShowPrivates := ::lShowStatics := ;
   ::lShowLocals  := ::lShowGlobals  := ::lAll := ! ::lAll

   ::RefreshVars()

   RETURN

METHOD PROCEDURE BarDisplay() CLASS HBDebugger

   LOCAL cClrItem   := __dbgColors()[ 8 ]
   LOCAL cClrHotKey := __dbgColors()[ 9 ]

   DispBegin()

   hb_Scroll( ::nMaxRow, 0, ::nMaxRow, ::nMaxCol,,, cClrItem )
   hb_DispOutAt( ::nMaxRow,  0, "F1-Help F2-Zoom F3-Repeat F4-User F5-Go F6-WA F7-Here F8-Step F9-BkPt " + TRACE_KEY_NAME + "-Trace", cClrItem )
   hb_DispOutAt( ::nMaxRow,  0, "F1", cClrHotKey )
   hb_DispOutAt( ::nMaxRow,  8, "F2", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 16, "F3", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 26, "F4", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 34, "F5", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 40, "F6", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 46, "F7", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 54, "F8", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 62, "F9", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 70, TRACE_KEY_NAME, cClrHotKey )

   DispEnd()

   RETURN


METHOD PROCEDURE BuildBrowseStack() CLASS HBDebugger

   LOCAL aColors := __dbgColors()

   ::oBrwStack := HBDbBrowser():New( IIf( ::lMenuBar, 2, 1 ), WINDOW_WIDTH - 14, WINDOW_HEIGHT - If( ::lMenuBar, 7, 6 ), WINDOW_WIDTH - 1 )
   ::oBrwStack:ColorSpec := aColors[ 2 ] + "," + aColors[ 5 ] + "," + aColors[ 4 ]
   ::oBrwStack:GoTopBlock    := { || ::oBrwStack:Cargo := 1 }
   ::oBrwStack:GoBottomBlock := { || ::oBrwStack:Cargo := Len( ::aProcStack ) }
   ::oBrwStack:SkipBlock := { | nSkip, nOld | nOld := ::oBrwStack:Cargo,;
                                              ::oBrwStack:Cargo += nSkip,;
                                              ::oBrwStack:Cargo := Min( Max( ::oBrwStack:Cargo, 1 ), Len( ::aProcStack ) ),;
                                              ::oBrwStack:Cargo - nOld }

   ::oBrwStack:Cargo := 1 // Actual highligthed row

   ::oBrwStack:AddColumn( TBColumnNew( "", { || IIf( ::oBrwStack:Cargo > 0 .And. Len( ::aProcStack ) > 0,;
                                                     PadC( ::aProcStack[ ::oBrwStack:Cargo, HB_DBG_CS_FUNCTION ], ::oBrwStack:nRight - ::oBrwStack:nLeft + 1 ),;
                                                     Space( ::oBrwStack:nRight - ::oBrwStack:nLeft + 1 ) ) } ) )

   RETURN

METHOD BuildStackWindow() CLASS HBDebugger

   ::lShowCallStack = .f.

   ::oWndStack := HBDbWindow():New( IIf( ::lMenuBar, 1, 0 ), WINDOW_WIDTH - 15, WINDOW_HEIGHT - If( ::lMenuBar, 6, 5 ), WINDOW_WIDTH, "Calls" )

   ::oWndStack:bKeyPressed  := { | nKey | ::CallStackProcessKey( nKey ) }
   ::oWndStack:bLButtonDown := { || ::CallStackProcessKey( K_LBUTTONDOWN ) }

   AAdd( ::aWindows, ::oWndStack )

   ::BuildBrowseStack()
   ::oWndStack:Browser  := ::oBrwStack
   ::oWndStack:bPainted := { || ::oBrwStack:RefreshAll():ForceStable(),;
                                IIf( ::oWndStack:lFocused, SetCursor( SC_NONE ), NIL ) }
   ::oWndStack:bGotFocus = { || SetCursor( SC_NONE ) }

   RETURN NIL
   
METHOD BuildVarsWindow() CLASS HBDebugger

   Local oCol

   ::oWndVars := HBDbWindow():New( 1, 1, 1, 1, "Monitor" )

   ::oWndVars:bLButtonDown := { | nMRow, nMCol | ::WndVarsLButtonDown( nMRow, nMCol ) }
   ::oWndVars:bLDblClick   := { | nMRow, nMCol | If( ::oBrwVars:IsOver( nMRow, nMCol ),;
                                                     ( ::WndVarsLButtonDown( nMRow, nMCol ), ::EditVar( ::oBrwVars:Cargo ) ), NIL ) }

   ::oBrwVars := HBDbBrowser():New( 1, 1, 1, 1 )
   ::oWndVars:Browser := ::oBrwVars

   ::oBrwVars:Cargo         := 1 // Actual highligthed row
   ::oBrwVars:ColorSpec     := __DbgColors()[ 2 ] + "," + __DbgColors()[ 5 ] + "," + __DbgColors()[ 3 ] + "," + __DbgColors()[ 6 ]
   ::oBrwVars:GoTopBlock    := { || ::oBrwVars:cargo := 1 }
   ::oBrwVars:GoBottomBlock := { || ::oBrwVars:cargo := Len( ::aVars ) }
   ::oBrwVars:SkipBlock     := { | nSkip, nOld | nOld := ::oBrwVars:Cargo,;
                                                 ::oBrwVars:Cargo += nSkip,;
                                                 ::oBrwVars:Cargo := Min( Max( ::oBrwVars:Cargo, 1 ),;
                                                                               Len( ::aVars ) ),;
                                                 If( Len( ::aVars ) > 0, ::oBrwVars:Cargo - nOld, 0 ) }

   oCol   := TBColumnNew( "", { || PadR( If( Len( ::aVars ) > 0, ;
                                             AllTrim( Str( ::oBrwVars:Cargo -1 ) ) + ") " +  ::VarGetInfo( ::aVars[ Max( ::oBrwVars:Cargo, 1 ) ] ), ;
                                             " " ), ;
                                   ::oWndVars:nWidth() - 2 ) } )
   ::oBrwVars:AddColumn( oCol )
   oCol:DefColor:={1,2}

   ::oWndVars:bPainted    := { || If( Len( ::aVars ) > 0, ::obrwVars:RefreshAll():ForceStable(), NIL ),;
                                  If( ::oWndVars:lFocused, SetCursor( SC_NONE ), NIL ) }
   ::oWndVars:bKeyPressed := { | nKey | ( iif( nKey == K_DOWN, ::oBrwVars:Down(), nil ) ;
                                        , iif( nKey == K_UP, ::oBrwVars:Up(), nil ) ;
                                        , iif( nKey == K_PGDN, ::oBrwVars:PageDown(), nil ) ;
                                        , iif( nKey == K_PGUP, ::oBrwVars:PageUp(), nil ) ;
                                        , iif( nKey == K_HOME, ::oBrwVars:GoTop(), nil ) ;
                                        , iif( nKey == K_END, ::oBrwVars:GoBottom(), nil ) ;
                                        , iif( nKey == K_ENTER, ::EditVar( ::oBrwVars:Cargo ), nil ), IIF(LEN(::aVars)>0, ::oBrwVars:ForceStable(), nil) ) }

   AAdd( ::aWindows, ::oWndVars )

   RETURN NIL

METHOD BuildWatchWindow() CLASS HBDebugger

   Local oCol

   ::oWndPnt := HBDbWindow():New( 1, 1, 1, 1, "Watch" )

   ::oBrwPnt := HBDbBrowser():New( 1, 1, 1, 1 )

   ::oWndPnt:Browser    := ::oBrwPnt
   ::oBrwPnt:Cargo      := 1 // Actual highligthed row
   ::oBrwPnt:ColorSpec  := __DbgColors()[ 2 ] + "," + __DbgColors()[ 5 ] + "," + __DbgColors()[ 3 ] + "," + __DbgColors()[ 6 ]
   ::oBrwPnt:GoTopBlock := { || ::oBrwPnt:cargo := Min( 1, Len(::aWatch) ) }
   ::oBrwPnt:GoBottomBlock := { || ::oBrwPnt:cargo := Len( ::aWatch ) }
   ::oBrwPnt:SkipBlock := { | nSkip, nOld | nOld := ::oBrwPnt:Cargo,;
                            ::oBrwPnt:Cargo += nSkip,;
                            ::oBrwPnt:Cargo := Min( Max( ::oBrwPnt:Cargo, 1 ), Len( ::aWatch ) ),;
                            IIF( LEN(::aWatch) > 0, ::oBrwPnt:Cargo - nOld, 0 ) }

   oCol := TBColumnNew( "", { || PadR( IIF( LEN( ::aWatch ) > 0, ;
                                          AllTrim( Str( ::oBrwPnt:Cargo -1 ) ) + ") " + ;
                                          ::WatchGetInfo( Max( ::oBrwPnt:Cargo, 1 ) ), ;
                                          " " ), ;
                                      ::oWndPnt:nWidth() - 2 ) } )
   ::oBrwPnt:AddColumn( oCol )
   oCol:DefColor:={1,2}

   ::oWndPnt:bLButtonDown := { | nMRow, nMCol | ::WndPntLButtonDown( nMRow, nMCol ) }
   ::oWndPnt:bLDblClick   := { | nMRow, nMCol | If( ::oBrwPnt:IsOver( nMRow, nMCol ),;
                                                    ( ::WndPntLButtonDown( nMRow, nMCol ), ::WatchpointEdit( ::oBrwPnt:Cargo ) ), NIL ) }

   ::oWndPnt:bPainted := { || If( Len( ::aWatch ) > 0, ::oBrwPnt:RefreshAll():ForceStable(), NIL ),;
                              If( ::oWndPnt:lFocused, SetCursor( SC_NONE ), NIL ) }

   ::oWndPnt:bKeyPressed := { | nKey | ( If( nKey == K_DOWN, ::oBrwPnt:Down(), nil ) ;
                                       , If( nKey == K_UP, ::oBrwPnt:Up(), nil ) ;
                                       , If( nKey == K_PGDN, ::oBrwPnt:PageDown(), nil ) ;
                                       , If( nKey == K_PGUP, ::oBrwPnt:PageUp(), nil ) ;
                                       , If( nKey == K_HOME, ::oBrwPnt:GoTop(), nil ) ;
                                       , If( nKey == K_END, ::oBrwPnt:GoBottom(), nil ) ;
                                       , If( nKey == K_DEL, ::WatchpointDel( ::oBrwPnt:Cargo ), nil ) ;
                                       , If( nKey == K_ENTER, ::WatchpointEdit( ::oBrwPnt:Cargo ), nil );
                                       , If( ::oWndPnt:lVisible, ::oBrwPnt:ForceStable(), NIL ) ) }

   AAdd( ::aWindows, ::oWndPnt )

   RETURN NIL

METHOD PROCEDURE BuildCommandWindow() CLASS HBDebugger

   LOCAL nSize

   ::oWndCommand             := HBDbWindow():New( ::nMaxRow - ::nCmdWndHight - 2, 0, ::nMaxRow - 1, ::nMaxCol, "Command" )
   ::oWndCommand:bGotFocus   := { || ::oGetCommand:showCursor() }
   ::oWndCommand:bLostFocus  := { || SetCursor( SC_NONE ) }
   ::oWndCommand:bKeyPressed := {| nKey | ::CommandWindowProcessKey( nKey ) }
   ::oWndCommand:bPainted    := {|| ::CommandWindowDisplay(), ;
                                    hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, "> ", __dbgColors()[ 2 ] ), ;
                                    ::oGetCommand:newPos( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3 ),;
                                    ::oGetCommand:setWidth( ::oWndCommand:nRight - ::oWndCommand:nLeft - 3 ),;
                                    ::oGetCommand:SetColor( __dbgColors()[ 2 ] ):display() }
   AAdd( ::aWindows, ::oWndCommand )

   ::aHistCommands := { "" }
   ::aLastCommands := { "" }
   ::nCommand := 1

   nSize := ::oWndCommand:nRight - ::oWndCommand:nLeft - 3
   ::oGetCommand := HbDbInput():new( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3, ;
                                     nSize, "", __dbgColors()[ 2 ], Max( nSize, GET_BUFFER_SIZE ) )

   RETURN


METHOD PROCEDURE CallStackProcessKey( nKey ) CLASS HBDebugger

   LOCAL n
   LOCAL nSkip
   LOCAL lUpdate := .F.
   LOCAL nMCol
   LOCAL nMRow

   SWITCH nKey
      CASE K_HOME
         ::oBrwStack:GoTop()
         ::oBrwStack:ForceStable()
         lUpdate = .t.
         EXIT

      CASE K_END
         ::oBrwStack:GoBottom()
         ::oBrwStack:ForceStable()
         lUpdate = .t.
         EXIT

      CASE K_UP
         ::oBrwStack:Up()
         ::oBrwStack:ForceStable()
         lUpdate = .t.
         EXIT

      CASE K_DOWN
         ::oBrwStack:Down()
         ::oBrwStack:ForceStable()
         lUpdate = .t.
         EXIT

      CASE K_PGUP
         ::oBrwStack:PageUp()
         ::oBrwStack:ForceStable()
         lUpdate = .t.
         EXIT

      CASE K_PGDN
         ::oBrwStack:PageDown()
         ::oBrwStack:ForceStable()
         lUpdate = .t.
         EXIT

      CASE K_LBUTTONDOWN
         nMRow := MRow()
         nMCol := MCol()
         IF ::oWndStack:IsCloseButton( nMRow, nMCol )

            ::Stack()

         ELSEIF ( nMRow >= ::oBrwStack:nTop .And. nMRow <= ::oBrwStack:nBottom .And. nMCol >= ::oBrwStack:nLeft .And. nMCol <= ::oBrwStack:nRight )

            IF ( nSkip := nMRow - ::oWndStack:nTop - ::oBrwStack:RowPos ) != 0
               IF nSkip > 0
                  FOR n = 1 to nSkip
                     ::oBrwStack:Down()
                     ::oBrwStack:Stabilize()
                  NEXT
               ELSE
                  FOR n = 1 to nSkip + 2 step -1
                     ::oBrwStack:Up()
                     ::oBrwStack:Stabilize()
                  NEXT
               ENDIF
               ::oBrwStack:ForceStable()
            ENDIF
            lUpdate = .t.

         EndIf
         EXIT
   ENDSWITCH

   IF lUpdate
      DispBegin()
      IF ::oWndVars:lVisible
         ::LoadVars()
         ::ShowVars()
      ENDIF
      ::ShowCodeLine( ::oBrwStack:Cargo )
      DispEnd()
   ENDIF

   RETURN


METHOD PROCEDURE CodeblockTrace() CLASS HBDebugger

   ::oPullDown:GetItemByIdent( "CODEBLOCK" ):checked := ::lCBTrace := ! ::lCBTrace
   __dbgSetCBTrace( ::pInfo, ::lCBTrace )

   RETURN

METHOD PROCEDURE ExchangeScreens() CLASS HBDebugger

   ::lExchangeScreens := ! ::lExchangeScreens
   ::oPullDown:GetItemByIdent( "EXCHANGE" ):checked := ::lExchangeScreens

return


METHOD PROCEDURE CodeWindowProcessKey( nKey ) CLASS HBDebugger
   
   LOCAL nMRow
   LOCAL nMCol
   LOCAL nSkip
   LOCAL n
   
   SWITCH nKey
      CASE K_HOME
         ::oBrwText:Home()
         EXIT

      CASE K_END
         ::oBrwText:End()
         EXIT

      CASE K_LEFT
         ::oBrwText:Left()
         EXIT

      CASE K_RIGHT
         ::oBrwText:Right()
         EXIT

      CASE K_UP
         ::oBrwText:Up()
         EXIT

      CASE K_DOWN
         ::oBrwText:Down()
         EXIT

      CASE K_PGUP
         ::oBrwText:PageUp()
         EXIT

      CASE K_PGDN
         ::oBrwText:PageDown()
         EXIT

      CASE K_CTRL_PGUP
         ::oBrwText:goTop()
         EXIT

      CASE K_CTRL_PGDN
         ::oBrwText:goBottom()
         EXIT
         
      CASE K_LBUTTONDOWN
         nMRow := MRow()
         nMCol := MCol()

         IF ::oWndCode:IsCloseButton( nMRow, nMCol )

            ::Stack()

         ELSEIF ( nMRow >= ::oBrwText:nTop .And. nMRow <= ::oBrwText:nBottom .And. nMCol >= ::oBrwText:nLeft .And. nMCol <= ::oBrwText:nRight )

            IF ( nSkip := nMRow - ::oWndCode:nTop - ::oBrwText:RowPos ) != 0
               IF nSkip > 0
                  FOR n = 1 to nSkip
                     ::oBrwText:Down()
                     ::oBrwText:Stabilize()
                  NEXT
               ELSE
                  FOR n = 1 to nSkip + 2 step -1
                     ::oBrwText:Up()
                     ::oBrwText:Stabilize()
                  NEXT
               ENDIF
               ::oBrwText:ForceStable()
            ENDIF
         ENDIF
         EXIT

   ENDSWITCH
   ::oBrwText:ForceStable()

   RETURN


METHOD PROCEDURE Colors() CLASS HBDebugger

   LOCAL oWndColors := HBDbWindow():New( 4, 5, 16, ::nMaxCol - 5, "Debugger Colors[1..13]", ::ClrModal() )
   local aColors := { { 01, "Border"         }, { 02, "Text"                  }, { 03, "Text High"      },;
                      { 04, "Text PPO"       }, { 05, "Text Selected"         }, { 06, "Text High Sel." },;
                      { 07, "Text PPO Sel."  }, { 09, "Menu"                  }, { 09, "Menu High"      },;
                      { 10, "Menu Selected"  }, { 11, "Menu High Sel."        }, { 12, "Current Line"   },;
                      { 13, "Current Line With Break"  } }

   LOCAL oBrwColors := HBDbBrowser():New( oWndColors:nTop + 1, oWndColors:nLeft + 1, oWndColors:nBottom - 1, oWndColors:nRight - 1 )
   LOCAL nWidth     := oWndColors:nRight - oWndColors:nLeft - 1
   LOCAL oCol

   IF ::lMonoDisplay
      __dbgAlert( "Monochrome display" )
         RETURN
   ENDIF

   oBrwColors:Cargo         := 1 // Actual highligthed row
   oBrwColors:ColorSpec     := ::ClrModal()
   oBrwColors:GoTopBlock    := { || oBrwColors:cargo := 1 }
   oBrwColors:GoBottomBlock := { || oBrwColors:cargo := Len( aColors ) }
   oBrwColors:SkipBlock     := { | nSkip, nOld | nOld := oBrwColors:Cargo,;
                                                 oBrwColors:Cargo += nSkip,;
                                                 oBrwColors:Cargo := Min( Max( oBrwColors:Cargo, 1 ),;
                                                 Len( aColors ) ), oBrwColors:Cargo - nOld }

   oCol := TBColumnNew( "", { || PadR( aColors[ oBrwColors:Cargo, 2 ], 23 ) } )                                              
   oBrwColors:AddColumn( oCol )
   oCol:width    := 23 
   oCol:DefColor := { 1, 2 }
   oCol := TBColumnNew( "", { || PadR( '"' + __DbgColors()[ oBrwColors:Cargo ] + '"', nWidth - 24 ) } )
   oBrwColors:AddColumn( oCol )
   oCol:DefColor       := { 1, 3 } 
   ocol:width          := 42
   oBrwColors:autolite := .T.

   oWndColors:Browser     := oBrwColors
   oWndColors:bPainted    := { || oBrwColors:RefreshAll():ForceStable(), HiLiteRow( oBrwColors ) }
   oWndColors:bKeyPressed := { | nKey | SetsKeyPressed( aColors, nKey, oBrwColors, oWndColors, "Debugger Colors",;
                                                        { || ::EditColor( oBrwColors:Cargo, oBrwColors ) } ) }
   oWndColors:ShowModal()

   ::LoadColors()

   RETURN


METHOD PROCEDURE CommandWindowDisplay( cLine, lCmd ) CLASS HBDebugger

   LOCAL n, nRow, nSize

   IF cLine != NIL
      cLine := iif( lCmd, "> ", "      " ) + cLine
      IF Len( ::aHistCommands ) >= DEBUGGER_CMDHIST_SIZE
         ADel( ::aHistCommands, 1 )
         ::aHistCommands[ Len( ::aHistCommands ) ] := cLine
      ELSE
         AAdd( ::aHistCommands, cLine )
      ENDIF
   ENDIF

   n := Len( ::aHistCommands )
   nRow := ::oWndCommand:nBottom
   nSize := ::oWndCommand:nRight - ::oWndCommand:nLeft - 1
   hb_DispOutAt( --nRow, ::oWndCommand:nLeft + 1, PadR( "> ", nSize ), ;
                 __dbgColors()[ 2 ] )
   DO WHILE --nRow > ::oWndCommand:nTop
      hb_DispOutAt( nRow, ::oWndCommand:nLeft + 1, ;
                    PadR( iif( n > 0, ::aHistCommands[ n-- ], "" ), nSize ), ;
                    __dbgColors()[ 2 ] )
   ENDDO

   RETURN


METHOD PROCEDURE CommandWindowProcessKey( nKey ) CLASS HBDebugger

   LOCAL cCommand
   LOCAL n
   LOCAL nRow
   LOCAL nCol

   If ! ::oWndCommand:lFocused
      nRow := Row()
      nCol := Col()
   EndIf

   SWITCH nKey
      CASE K_UP
      CASE K_F3
         IF ::nCommand > 1
            ::aLastCommands[ ::nCommand ] := RTrim( ::oGetCommand:getValue() )
            ::oGetCommand:setValue( ::aLastCommands[ --::nCommand ] ):display()
         ENDIF
         EXIT
      CASE K_DOWN
         IF ::nCommand < Len( ::aLastCommands )
            ::aLastCommands[ ::nCommand ] := RTrim( ::oGetCommand:getValue() )
            ::oGetCommand:setValue( ::aLastCommands[ ++::nCommand ] ):display()
         ENDIF
         EXIT
      CASE K_ENTER
         cCommand := RTrim( ::oGetCommand:getValue() )
         IF ! Empty( cCommand )
            IF ( n := hb_AScan( ::aLastCommands, cCommand, , , .T. ) ) > 0 .AND. n < Len( ::aLastCommands )
               hb_ADel( ::aLastCommands, n, .T. )
            ENDIF
            ::nCommand := Len( ::aLastCommands )
            ::aLastCommands[ ::nCommand ] := cCommand
            AAdd( ::aLastCommands, "" )
            ::nCommand := Len( ::aLastCommands )
            ::CommandWindowDisplay( cCommand, .T. )
            ::DoCommand( cCommand )
         ENDIF
         hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ;
                       PadR( "> ", ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ), ;
                       __dbgColors()[ 2 ] )
         ::oGetCommand:setValue( "" ):display()
         EXIT
      OTHERWISE
         ::oGetCommand:applyKey( nKey )
   ENDSWITCH
   
   IF ::oWndCommand:lFocused
      ::oGetCommand:showCursor()
   ELSE
      SetPos( nRow, nCol )
   ENDIF   

   RETURN


/*
 * ?? <expr>
 *      displays inspect window with value or display nothing on error
 * ? <expr>
 *      displays either result or error description in command window
 */
METHOD DoCommand( cCommand ) CLASS HBDebugger

   LOCAL aCmnd[ 3 ]
   LOCAL cParam1 := ""
   LOCAL cParam2 := ""
   LOCAL cParams := ""
   LOCAL cResult
   LOCAL lValid
   LOCAL n
   LOCAL nAt
   cCommand := AllTrim( cCommand )

   DO CASE
   CASE Empty( cCommand )
      RETURN ""

   CASE hb_LeftEq( cCommand, "??" )
      cParams := AllTrim( SubStr( cCommand, 3 ) )
      cCommand := "??"

   CASE hb_LeftEq( cCommand, "?" )
      cParams := SubStr( cCommand, 2 )
      cCommand := "?"

   OTHERWISE
      IF ( n := At( " ", cCommand ) ) > 0
         cParam1 := cParams := AllTrim( SubStr( cCommand, n + 1 ) )
         cCommand := Left( cCommand, n - 1 )
         IF ( n := At( " ", cParam1 ) ) > 0
            cParam2 := AllTrim( SubStr( cParam1, n + 1 ) )
            cParam1 := Left( cParam1, n - 1 )
         ENDIF
      ENDIF
      cCommand := Upper( cCommand )
      cParam1 := Upper( cParam1 )
   ENDCASE

   DO CASE
   CASE cCommand == "??" .OR. cCommand == "?"
      aCmnd[ WP_TYPE ] := cCommand
      aCmnd[ WP_EXPR ] := cParams

      cResult := ::GetExprValue( cParams, @lValid )

      IF aCmnd[ WP_TYPE ] == "??"
         IF lValid
            ::Inspect( aCmnd[ WP_EXPR ], cResult )
            cResult := ""  // discard result
         ENDIF
      ELSEIF lValid
         cResult := __dbgValToStr( cResult )
      ENDIF
      
      // Refresh watchpoint
      If ::oWndPnt:lVisible
         ::oWndPnt:Refresh()
      EndIf

      // Refresh vars
      If ::oWndVars:lVisible
         ::oWndVars:Refresh()
      EndIf

   CASE hb_LeftEqN( "ANIMATE", cCommand, 4 )
      ::lAnimate := .T.
      ::Animate()

   CASE cCommand == "BP"
      IF Empty( cParam1 )
         ::BreakPointToggle()
      ELSEIF IsDigit( cParam1 )
         ::BreakPointToggle( Val( cParam1 ),  iif( Empty( cParam2 ), ::cPrgName, cParam2 ) )
      ELSEIF hb_asciiIsAlpha( cParam1 ) .OR. hb_LeftEq( cParam1, "_" )
         ::BreakPointFunc( cParam1 )
      ENDIF

   CASE hb_LeftEqN( "CALLSTACK", cCommand, 4 )
      ::Stack( cParam1 )

   CASE hb_LeftEqN( "DELETE", cCommand, 3 )
      DO CASE
      CASE cParam1 == "BP"
         ::BreakPointDelete( cParam2 )
      CASE cParam1 == "WP" .OR. cParam2 == "TP"
         ::WatchpointDel( cParam2 )
      CASE cParam1 == "ALL"
         DO CASE
         CASE Empty( cParam2 )
            ::BreakPointDelete( cParam1 )
            ::WatchpointDel( cParam1 )
         CASE hb_LeftEqI( "BP", cParam2 )
            ::BreakPointDelete( cParam1 )
         CASE hb_LeftEqI( "WP", cParam2 ) .OR. hb_LeftEqI( "TP", cParam2 )
            ::WatchpointDel( cParam1 )
         OTHERWISE
            /* Cl*pper clears break and watch points in such case */
            cResult := "Command error"
         ENDCASE
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "DOS"
      ::OsShell()

   CASE hb_LeftEq( "FILE", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "OPEN", cParam1 )
         ::Open( cParam2 )
      CASE hb_LeftEq( "RESUME", cParam1 )
         ::Resume()
      CASE hb_LeftEq( "OS", cParam1 ) .OR. hb_LeftEq( "DOS", cParam1 )
         ::OSShell()
      CASE hb_LeftEq( "EXIT", cParam1 )
         ::Quit()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "FIND"
      ::Locate( 0, cParams )

   CASE hb_LeftEqN( "GOTO", cCommand, 4 ) .AND. ( n := Val( cParam1 ) ) > 0
      ::GoToLine( n )

   CASE hb_LeftEq( "GO", cCommand )
      ::Go()

   CASE cCommand == "HELP"
      ::ShowHelp( cParam1 )

   CASE hb_LeftEqN( "INPUT", cCommand, 4 )
      IF Empty( cParams )
         cParams := AllTrim( ::InputBox( "File name",, ;
                                         {| cFile | hb_FileExists( cFile ) .OR. ( __dbgAlert( "File unavailable" ), .F. ) } ) )
         IF LastKey() == K_ESC
            cParams := ""
         ENDIF
      ENDIF
      IF ! Empty( cParams )
         ::DoScript( cParams )
      ENDIF

   CASE cCommand == "LIST"
      SWITCH cParam1
      CASE "BP"
         ::BreakPointList()
         EXIT
      CASE "WP"
      CASE "TP"
         ::WatchpointList()
         EXIT
      OTHERWISE
         cResult := "Command error"
      ENDSWITCH

   CASE hb_LeftEq( "LOCATE", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "FIND", cParam1 )
         ::Locate( 0, cParam2 )
      CASE hb_LeftEq( "NEXT", cParam1 )
         ::FindNext()
      CASE hb_LeftEq( "PREVIOUS", cParam1 )
         ::FindPrevious()
      CASE hb_LeftEq( "GOTOLINE", cParam1 )
         ::SearchLine( cParam2 )
      CASE hb_LeftEq( "CASESENSITIVE", cParam1 )
         ::ToggleCaseSensitive()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE hb_LeftEq( "MONITOR", cCommand )

      /* Here the order of CASEs makes sense: M P is Public, while M Pr is
       * Private, etc. */
      DO CASE
      CASE hb_LeftEq( "PUBLIC", cParam1 )
         ::Public()
      CASE hb_LeftEq( "PRIVATE", cParam1 )
         ::Private()
      CASE hb_LeftEq( "STATIC", cParam1 )
         ::Static()
      CASE hb_LeftEq( "LOCAL", cParam1 )
         ::Local()
      CASE hb_LeftEq( "GLOBAL", cParam1 )
         ::Global()
      CASE hb_LeftEq( "ALL", cParam1 )
         ::All()
      CASE hb_LeftEq( "SORT", cParam1 )
         ::Sort()
      CASE hb_LeftEq( "SHOWALLGLOBALS", cParam1 )
         ::ShowAllGlobals()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "NEXT"
      ::FindNext()

   CASE cCommand == "NUM"
      SWITCH cParam1
         CASE "OFF"
            ::LineNumbers( .F. ) ; EXIT
         CASE "ON"
            ::LineNumbers( .T. ) ; EXIT
         OTHERWISE
            ::LineNumbers() ; EXIT
      ENDSWITCH

   CASE hb_LeftEq( "OPTIONS", cCommand )
      DO CASE
         CASE Empty( cParam1 )
            ::OpenMenu( cCommand )
         CASE hb_LeftEq( "PREPROCESSEDCODE", cParam1 )
            ::OpenPPO()
         CASE hb_LeftEq( "LINENUMBERS", cParam1 )
            ::LineNumbers()
         CASE hb_LeftEq( "EXCHANGESCREENS", cParam1 ) .OR. ;
              hb_LeftEq( "SWAPONINPUT", cParam1 ) .OR. ;
              hb_LeftEq( "MENUBAR", cParam1 )
            ::NotSupported()
         CASE hb_LeftEq( "CODEBLOCKTRACE", cParam1 )
            ::CodeblockTrace()
         CASE hb_LeftEq( "MONODISPLAY", cParam1 )
            ::MonoDisplay()
         CASE hb_LeftEq( "COLORS", cParam1 )
            IF Empty( cParam2 )
               ::Colors()
            ELSE
               cParam2 := SubStr( cParam2, At( "{", cParam2 ) + 1 )
               n       := 1
               nAt     := At( ",", cParam2 )
               While nAt > 0
                  ::aColors[ n++ ] := StrTran( Left( cParam2, nAt - 1 ), '"', "" )
                   cParam2 := SubStr( cParam2, nAt + 1 )
                   nAt     := At( ",", cParam2 )
               EndDo
               nAt            := At( "}", cParam2 )
               ::aColors[ n ] := StrTran( Left( cParam2, nAt - 1 ), '"', "" )
               ::LoadColors()
            ENDIF
         CASE hb_LeftEq( "TABWIDTH", cParam1 )
            IF IsDigit( cParam2 )
               ::nTabWidth := Min( Val( cParam2 ), 16 )
            ELSE
               ::TabWidth()
            ENDIF
         CASE hb_LeftEq( "PATH", cParam1 )
            ::PathForFiles( AllTrim( cParam2 ) )
         CASE hb_LeftEq( "RUNATSTARTUP", cParam1 )
            ::RunAtStartup( .T. )
         CASE hb_LeftEq( "NORUNATSTARTUP", cParam1 )
            ::RunAtStartup( .F. )
         CASE hb_LeftEq( "SAVESETTINGS", cParam1 )
            ::SaveSettings( AllTrim( cParam2 ) )
         CASE hb_LeftEq( "RESTORESETTINGS", cParam1 )
            ::RestoreSettings( AllTrim( cParam2 ) )
         OTHERWISE
            cResult := "Command error"
      ENDCASE

   CASE hb_LeftEqN( "OUTPUT", cCommand, 4 )
      ::ShowAppScreen()

   CASE hb_LeftEq( "POINT", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "WATCHPOINT", cParam1 )
         ::WatchpointAdd()
      CASE hb_LeftEq( "TRACEPOINT", cParam1 )
         ::TracepointAdd()
      CASE hb_LeftEq( "BREAKPOINT", cParam1 )
         ::BreakPointToggle()
      CASE hb_LeftEq( "DELETE", cParam1 )
         ::WatchpointDel()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "PREV"
      ::FindPrevious()

   CASE hb_LeftEq( "QUIT", cCommand )
      ::Quit()

   /* TODO: Support RESTART */

   CASE hb_LeftEqN( "RESUME", cCommand, 4 )
      ::Resume()

   CASE hb_LeftEq( "RUN", cCommand )
      DO CASE
         CASE Empty( cParam1 )
            ::OpenMenu( cCommand )
         CASE hb_LeftEq( "ANIMATE", cParam1 )
            ::ToggleAnimate()
            ::Animate()
         CASE hb_LeftEq( "STEP", cParam1 )
            ::Step()
         CASE hb_LeftEq( "TRACE", cParam1 )
            ::Trace()
         CASE hb_LeftEq( "GO", cParam1 )
            ::Go()
         CASE hb_LeftEq( "TOCURSOR", cParam1 )
            ::ToCursor()
         CASE hb_LeftEq( "NEXTROUTINE", cParam1 )
            ::NextRoutine()
         CASE hb_LeftEq( "STEPOUT", cParam1 )
            ::StepOut()
         CASE hb_LeftEq( "SPEED", cParam1 )
            IF IsDigit( cParam2 )
               ::nSpeed := Min( Val( cParam2 ), 65534 )
            ELSE
               ::Speed()
            ENDIF
         OTHERWISE
            cResult := "Command error"
      ENDCASE

   CASE hb_LeftEqN( "SPEED", cCommand, 4 )
      IF IsDigit( cParam1 )
         ::nSpeed := Min( Val( cParam1 ), 65534 )
      ELSE
         ::Speed()
      ENDIF

   CASE cCommand == "STEP"
      ::Step()

   CASE cCommand == "TP"
      ::TracepointAdd( cParams )

   CASE hb_LeftEq( "VIEW", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "SETS", cParam1 )
         ::ViewSets()
      CASE hb_LeftEq( "WORKAREAS", cParam1 )
         ::ShowWorkAreas()
      CASE hb_LeftEq( "APPSCREEN", cParam1 )
         ::ShowAppScreen()
      CASE hb_LeftEq( "CALLSTACK", cParam1 )
         ::Stack()
      OTHERWISE
         ::Open( cParams )
      ENDCASE

   CASE hb_LeftEq( "WINDOW", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "NEXT", cParam1 )
         ::NextWindow()
      CASE hb_LeftEq( "PREV", cParam1 )
         ::PrevWindow()
      //CASE hb_LeftEq( "MOVE", cParam1 )
      //   IF Empty( cParam2 )
      //      ::NotSupported()
      //   ELSE
      //      oWindow := ::aWindows[ ::nCurrentWindow ]
      //      IF ( n := At( " ", cParam2 ) ) > 0
      //         n := Val( SubStr( cParam2, n ) )
      //      ENDIF
      //      oWindow:Resize( Val( cParam2 ), n, ;
      //         oWindow:nBottom + Val( cParam2 ) - oWindow:nTop, ;
      //         oWindow:nRight + n - oWindow:nLeft )
      //      ::lWindowsAutoSized := .F.
      //   ENDIF
      //CASE hb_LeftEq( "SIZE", cParam1 )
      //   IF Empty( cParam2 )
      //      ::NotSupported()
      //   ELSE
      //      IF Val( cParam2 ) >= 2 .AND. ;
      //        ( n := At( " ", cParam2 ) ) > 0 .AND. Val( SubStr( cParam2, n ) ) > 0
      //         oWindow := ::aWindows[ ::nCurrentWindow ]
      //         oWindow:Resize( oWindow:nTop, oWindow:nLeft, ;
      //                         Val( cParam2 ) - 1 + oWindow:nTop, ;
      //                         Val( SubStr( cParam2, n ) ) - 1 + oWindow:nLeft )
      //         ::lWindowsAutoSized := .F.
      //      ENDIF
      //   ENDIF
      CASE hb_LeftEq( "ZOOM", cParam1 )
         ::Zoom()
         
      CASE hb_LeftEq( "ICONIZE", cParam1 )
         ::Iconize()
         
      CASE hb_LeftEq( "TILE", cParam1 )
         ::Tile()
         
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "WP"
      ::WatchpointAdd( cParams )

   OTHERWISE
      cResult := "Command error"

   ENDCASE

   IF ! Empty( cResult ) .And. ::oWndCommand:lVisible
      hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ;
                    Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ), ;
                    __dbgColors()[ 2 ] )
      ::CommandWindowDisplay( cResult, .F. )
   ENDIF

   RETURN cResult


METHOD PROCEDURE DoScript( cFileName ) CLASS HBDebugger

   LOCAL cLine
   LOCAL oErr


   IF hb_FileExists( cFileName )
      FOR EACH cLine IN __dbgTextToArray( MemoRead( cFileName ) )
         BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
            ::DoCommand( AllTrim( cLine ) )
         RECOVER USING oErr
            If ::oWndCommand != Nil .And. ::oWndCommand:lVisible
               ::CommandWindowDisplay( oErr:description, .F. )
            EndIf   
         END SEQUENCE
      NEXT
   ENDIF

   RETURN


METHOD PROCEDURE EditColor( nColor, oBrwColors ) CLASS HBDebugger

   local cColor := PadR( '"' + ::aColors[ nColor ] + '"', oBrwColors:getColumn(2):Width )

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

   IF __dbgInput( Row(), oBrwColors:nLeft + oBrwColors:GetColumn( 1 ):width + 2, oBrwColors:getColumn( 2 ):Width,;
                  @cColor, __dbgExprValidBlock( "C" ), ;
                  SubStr( ::ClrModal(), 3 ) )
      ::aColors[ nColor ] := &cColor
   ENDIF

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

   RETURN

METHOD PROCEDURE EditSet( nSet, oBrwSets ) CLASS HBDebugger

   LOCAL cSet  := PadR( __dbgValToExp( Set( nSet ) ), ;
                        oBrwSets:getColumn( 2 ):Width )
   LOCAL cType := ValType( Set( nSet ) )

   oBrwSets:RefreshCurrent()
   oBrwSets:ForceStable()

   IF __dbgInput( Row(), oBrwSets:nLeft + oBrwSets:GetColumn( 1 ):width + 1, ;
                  oBrwSets:getColumn( 2 ):Width, ;
                  @cSet, __dbgExprValidBlock( cType ), ;
                  SubStr( ::ClrModal(), 3 ), GET_BUFFER_SIZE )
      Set( nSet, &cSet )
   ENDIF

   oBrwSets:RefreshCurrent()
   oBrwSets:ForceStable()

   RETURN


METHOD PROCEDURE EditVar( nVar ) CLASS HBDebugger

   LOCAL cVarName  := ::aVars[ nVar, 1 ]
   LOCAL uVarValue := ::VarGetValue( ::aVars[ nVar ] )
   LOCAL cVarStr
   LOCAL oErr

   IF ValType( uVarValue ) $ "AHOPB"
      ::EditValue( cVarName, uVarValue, .F. )
   ELSE
      cVarStr := ::EditValue( cVarName, uVarValue )

      IF LastKey() != K_ESC
         BEGIN SEQUENCE WITH __BreakBlock()
            ::VarSetValue( ::aVars[ nVar ], &cVarStr )
         RECOVER USING oErr
            __dbgAlert( oErr:description )
         END SEQUENCE
      ENDIF
   ENDIF

   ::oBrwVars:RefreshCurrent()
   ::oBrwVars:ForceStable()
   IF ::oWndPnt:lVisible
      ::oWndPnt:Refresh()
   ENDIF

   RETURN


METHOD FindNext() CLASS HBDebugger
   RETURN ::Locate( 1, ::cSearchString )


METHOD FindPrevious() CLASS HBDebugger
   RETURN ::Locate( 2, ::cSearchString )


METHOD GetExprValue( xExpr, lValid ) CLASS HBDebugger

   LOCAL xResult
   LOCAL oErr
   LOCAL nLastKey := LastKey()

   lValid := .F.
   ::RestoreAppState()
   xResult := __dbgGetExprValue( ::pInfo, xExpr, @lValid, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )
   IF ! lValid
      oErr := xResult
      IF oErr:ClassName() == "ERROR"
         xResult := oErr:operation + ": " + oErr:description
         IF HB_ISARRAY( oErr:args )
            xResult += "; arguments:"
            AEval( oErr:args, {| x, i | xResult += iif( i == 1, " ", ", " ) + __dbgValToStr( x ) } )
         ENDIF
      ELSE
         xResult := "Syntax error"
      ENDIF
   ENDIF
   ::SaveAppState()
   hb_keySetLast( nLastKey )
   RETURN xResult


METHOD GetSourceFiles() CLASS HBDebugger
   RETURN __dbgGetSourceFiles( ::pInfo )


METHOD ModuleMatch( cModuleName1, cModuleName2 ) CLASS HBDebugger
   RETURN __dbgModuleMatch( ::pInfo, cModuleName1, cModuleName2 )


METHOD PROCEDURE Global() CLASS HBDebugger

   ::lShowGlobals := ! ::lShowGlobals
   ::RefreshVars()

   RETURN


METHOD PROCEDURE Go() CLASS HBDebugger

   // we are starting to run again so reset to the deepest call if
   // displaying stack
   ::oBrwStack:GoTop()
   __dbgSetGo( ::pInfo )
   ::Exit()

   RETURN


METHOD PROCEDURE GotoLine( nLine ) CLASS HBDebugger

   DispBegin()

   if ::oWndVars:lVisible
      ::oBrwVars:ForceStable()
   endif

   if ::oWndPnt:lVisible
      ::oBrwPnt:ForceStable()
   endif

   if ::oWndStack:lVisible
      ::oBrwStack:ForceStable()
   endif

   ::oBrwText:GotoLine( nLine )
   ::oWndCode:Refresh()

   DispEnd()

   RETURN


METHOD PROCEDURE HandleEvent() CLASS HBDebugger

   LOCAL nPopup
   LOCAL oWnd
   LOCAL nKey
   LOCAL nMRow
   LOCAL nMCol

   IF ::lAnimate
      IF ::nSpeed != 0
         Inkey( ::nSpeed / 10 )
      ENDIF
      IF __dbgInvokeDebug()  // NextKey() == K_ALT_D
         ::lAnimate := .F.
      ELSE
         ::Step()
         RETURN
      ENDIF
   ENDIF

   ::lEnd := .F.

   DO WHILE ! ::lEnd

      nKey := __dbgInkey()

      IF nKey == K_ALT_X
         t_oDebugger:Quit()
      ELSEIF ::oPullDown:IsOpen()
         ::oPullDown:ProcessKey( nKey )
         IF ::oPullDown:nOpenPopup == 0 // Closed
            DispBegin()
            ::oCurrentWnd:refreshAll()
            ::oCurrentWnd:SetFocus( .T. )
            DispEnd()
         ENDIF
      ELSE
         SWITCH nKey
         CASE K_LDBLCLK

            nMRow := MRow()
            nMCol := MCol()
            IF nMRow > If( ::lMenuBar, 0, -1 ) .And. nMRow < ::nMaxRow + If( ::lMenuBar, 0, 1 )

               oWnd := ::oCurrentWnd:WindowIsOver( nMRow, nMCol )
               If oWnd != NIL
 
                  If oWnd:lFocused
 
                     If oWnd:IsCaption( nMRow, nMCol )
                        ::Zoom()
                     EndIf
 
                  ElseIf ! oWnd:IsCloseButton( nMRow, nMCol )
 
                     ::oCurrentWnd := oWnd
                     ::oCurrentWnd:ToTop():SetFocus( .t. )
 
                  EndIf
                  oWnd:LDblClick( nMRow, nMCol )
 
               EndIf
            ENDIF
            EXIT

         CASE K_LBUTTONDOWN

            nMRow := MRow()
            nMCol := MCol()
            if nMRow == If( ::lMenuBar, 0, -1 )
               if ( nPopup := ::oPullDown:GetItemOrdByCoors( 0, nMCol ) ) != 0
                  DispBegin()
                  if ! ::oPullDown:IsOpen()
                     if ::oCurrentWnd:lFocused .And. HB_ISEVALITEM( ::oCurrentWnd:bLostFocus )
                        Eval( ::oCurrentWnd:bLostFocus )
                     endif
                     SetCursor( SC_NONE )
                  endif
                  If ! ::lMenuBar
                     ::ShowMenuBar()
                  EndIf
                  ::oPullDown:ShowPopup( nPopup )
                  DispEnd()
               endif
            
            elseif nMRow != WINDOW_HEIGHT + If( ::lMenuBar, 0, 1 )
            
               oWnd := ::oCurrentWnd:WindowIsOver( nMRow, nMCol )
               If oWnd != NIL
            
                  If ! oWnd:IsCloseButton( nMRow, nMCol )
            
                     If ! oWnd:lFocused
            
                        DispBegin()
                        ::oCurrentWnd := oWnd
                        ::oCurrentWnd:ToTop():SetFocus( .T. )
                        DispEnd()
            
                     EndIf
            
                     If oWnd:IsCaption( nMRow, nMCol )
                        ::MoveWindow( nMRow, nMCol )
                     ElseIf nMCol == oWnd:nLeft .Or. nMCol == oWnd:nRight .Or. nMRow == oWnd:nBottom
                        ::Size( nMRow, nMCol )
                     EndIf
            
                  EndIf
            
                  oWnd:LButtonDown( nMRow, nMCol )
            
               EndIf
            
            endif
            EXIT

         CASE K_ENTER
            IF ! Empty( ::oGetCommand:getValue() )
               ::oWndCommand:KeyPressed( nKey )
               EXIT
            ENDIF
         CASE K_RBUTTONDOWN
         CASE K_UP
         CASE K_DOWN
         CASE K_LEFT
         CASE K_RIGHT
         CASE K_PGUP
         CASE K_PGDN
         CASE K_HOME
         CASE K_END
         CASE K_DEL
         CASE K_CTRL_PGUP
         CASE K_CTRL_PGDN
         CASE K_CTRL_HOME
         CASE K_CTRL_END
         CASE K_CTRL_ENTER
            If ::oCurrentWnd:nViewStyle != VS_ICON
               ::oCurrentWnd:KeyPressed( nKey )
            EndIf
            EXIT

         CASE K_ALT_U /* Move the border between Command and Code windows Up */
            ::ResizeCmdWnd( 1 )
            EXIT

         CASE K_ALT_D /* Move the border between Command and Code windows Down */
            ::ResizeCmdWnd( -1 )
            EXIT

         CASE K_ALT_G /* Grow active window */
         CASE K_ALT_S /* Shrink active window */
            ::NotSupported()
            EXIT

         CASE K_F1
            ::ShowHelp()
            EXIT

         CASE K_F2
            ::Zoom()
            EXIT

         CASE K_F4
            ::ShowAppScreen()
            EXIT

         CASE K_F5
            ::Go()
            EXIT

         CASE K_CTRL_F5
            ::NextRoutine()
            EXIT

         CASE K_F6
            ::ShowWorkAreas()
            EXIT

         CASE K_F7
            ::ToCursor()
            EXIT

         CASE K_F8
            ::Step()
            EXIT

         CASE K_F9
            ::BreakPointToggle()
            EXIT

         CASE TRACE_KEY
            ::Trace()
            EXIT

         CASE K_CTRL_F10
            ::StepOut()
            EXIT

         CASE K_TAB
            ::NextWindow()
            EXIT

         CASE K_SH_TAB
            ::PrevWindow()
            EXIT

         OTHERWISE
            IF ! ::OpenMenu( __dbgAltToKey( nKey ) )
               ::oWndCommand:KeyPressed( nKey )
               ::oWndCommand:Refresh()
            ENDIF
         ENDSWITCH
      ENDIF
   ENDDO

   RETURN


METHOD PROCEDURE Hide() CLASS HBDebugger

   ::CloseDebuggerWindow()

   RETURN


METHOD PROCEDURE HideCallStack() CLASS HBDebugger

   Local lFocused := ::oWndStack:lFocused

   ::lShowCallStack = .f.

   DispBegin()
   ::oBrwStack:goTop()
   ::oWndStack:Hide( .F. )

   If ::lTiled
      ::Tile()
   EndIf

   If lFocused
      ::SetFocusCodeWindow()
   Else
      ::oCurrentWnd:SetFocus( .t. )
   EndIf

   DispEnd()

   RETURN


METHOD PROCEDURE HideVars() CLASS HBDebugger

   Local lFocused := ::oWndVars:lFocused

   ::oBrwVars:goTop()
   ::oWndVars:Hide( .F. )

   If ::lTiled
      ::Tile()
   EndIf

   If lFocused
      ::SetFocusCodeWindow()
   Else
      ::oCurrentWnd:SetFocus( .t. )
   EndIf

   RETURN
   
METHOD EditValue( cMsg, uValue, lEditable ) CLASS HBDebugger

   LOCAL nTop      := Int( ( ::nMaxRow / 2 ) - 5 )
   LOCAL nLeft     := Int( ( ::nMaxCol / 2 ) - 25 )
   LOCAL nBottom   := nTop + 2
   LOCAL nRight    := nLeft + 50
   LOCAL cType     := ValType( uValue )
   LOCAL nWidth    := nRight - nLeft - 1
   LOCAL cValExpr
   LOCAL lExit
   LOCAL nOldCursor
   LOCAL oWndInput := HBDbWindow():New( nTop, nLeft, nBottom, nRight, cMsg, ::oPullDown:cClrPopup )

   oWndInput:lShadow      := .T.
   oWndInput:lCloseButton := .F.
   oWndInput:Show()

   cValExpr := uValue

   IF hb_defaultValue( lEditable, .T. ) .And. ! cType $ "AOBH"

      cValExpr := __dbgValToExp( uValue )
      cValExpr := PadR( cValExpr, Max( Len( cValExpr ) * 2, 255 ) )
      IF __dbgInput( nTop + 1, nLeft + 1, nWidth, @cValExpr, __dbgExprValidBlock(), __dbgColors()[ 5 ], Max( Max( nWidth, Len( cValExpr ) ), GET_BUFFER_SIZE ) )
         cValExpr := AllTrim( cValExpr )
      ELSE
         cValExpr := uValue
      ENDIF      

   ELSE

      oWndInput:bPainted := { || hb_DispOutAt( oWndInput:nTop + 1, oWndInput:nLeft + 1, PadR( __dbgValToStr( uValue ), oWndInput:nRight - oWndInput:nLeft - 1 ), ::ClrModal() ) }
      hb_DispOutAt( nTop + 1, nLeft + 1, left( __dbgValToStr( uValue ), nRight - nLeft - 1 ), ::ClrModal() )
      SetPos( nTop + 1, nLeft + 1 )
      nOldCursor := SetCursor( SC_NONE )

      lExit := .F.

      DO WHILE ! lExit

         SWITCH __dbgInkey()
         CASE K_ESC
            lExit := .T.
            EXIT

         CASE K_ENTER

            SWITCH cType
            CASE "A"
               IF Len( uValue ) == 0
                  __dbgAlert( "Array is empty" )
               ELSE
                  __dbgArrays( uValue, cMsg )
               ENDIF
               LOOP
            CASE "H"
               IF Len( uValue ) == 0
                  __dbgAlert( "Hash is empty" )
               ELSE
                  __dbgHashes( uValue, cMsg )
               ENDIF
               LOOP
            CASE "O"
               __dbgObject( uValue, cMsg )
               LOOP
            ENDSWITCH

         ENDSWITCH
      ENDDO
      
      SetCursor( nOldCursor )

   ENDIF

   oWndInput:Hide()

   RETURN cValExpr


METHOD InputBox( cMsg, uValue, bValid, lEditable ) CLASS HBDebugger

   Local nCursor
   Local nTop    := Int( ( ::nMaxRow / 2 ) - 5 )
   Local nLeft   := Int( ( ::nMaxCol / 2 ) - 25 )
   Local nBottom := nTop + 2
   Local nRight  := nLeft + 50
   Local cType   := ValType( uValue )
   Local nWidth  := nRight - nLeft - 1
   Local uTemp
   Local lExit
   Local oWndInput := HBDbWindow():New( nTop, nLeft, nBottom, nRight, cMsg, ::oPullDown:cClrPopup )

   oWndInput:lShadow      := .T.
   oWndInput:lCloseButton := .F.
   oWndInput:Show()

   uTemp := uValue

   IF hb_defaultValue( lEditable, .T. )

      IF ! cType == "C" .OR. Len( uValue ) < nWidth
         uTemp := PadR( iif( cType == "N", hb_ntos( uValue ), uValue ), nWidth )
      ENDIF
      IF bValid == NIL .AND. cType $ "N"
         bValid := __dbgExprValidBlock( cType )
      ENDIF
      __dbgInput( nTop + 1, nLeft + 1, nWidth, @uTemp, bValid, __dbgColors()[ 5 ], Max( Max( nWidth, Len( uTemp ) ), 256 ) )
      SWITCH cType
         CASE "C" ; uTemp := AllTrim( uTemp ) ; EXIT
         CASE "N" ; uTemp := Val( uTemp )     ; EXIT
      ENDSWITCH

   ELSE

      nCursor := SetCursor( SC_NONE ) 
      
      oWndInput:bPainted := { || hb_DispOutAt( oWndInput:nTop + 1, oWndInput:nLeft + 1,;
                                               PadR( __dbgValToStr( uValue ), oWndInput:nRight - oWndInput:nLeft - 1 ),;
                                               __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] ) }
      hb_DispOutAt( nTop + 1, nLeft + 1,;
                    PadR( __dbgValToStr( uValue ), nRight - nLeft - 1 ),;
                    __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] )
      SetPos( nTop + 1, nLeft + 1 )

      lExit := .F.

      DO WHILE ! lExit

         SWITCH __dbgInkey()
         CASE K_ESC
            lExit := .T.
            EXIT

         CASE K_ENTER
            SWITCH cType
               CASE "A"
                  IF Len( uValue ) == 0
                     __dbgAlert( "Array is empty" )
                  ELSE
                     __dbgArrays( uValue, cMsg, .F. )
                  ENDIF
                  LOOP
               CASE "H"
                  IF Len( uValue ) == 0
                     __dbgAlert( "Hash is empty" )
                  ELSE
                     __dbgHashes( uValue, cMsg, .F. )
                  ENDIF
                  LOOP
               CASE "O"
                  __dbgObject( uValue, cMsg, .F. )
                  LOOP
            ENDSWITCH
         
            EXIT
         ENDSWITCH
      ENDDO

      SetCursor( nCursor )

   ENDIF

   oWndInput:Hide()
   
   RETURN uTemp


METHOD PROCEDURE Inspect( uValue, cValueName ) CLASS HBDebugger

   ::InputBox( uValue, cValueName,, .F. )

   RETURN


METHOD IsValidStopLine( cName, nLine ) CLASS HBDebugger
   RETURN __dbgIsValidStopLine( ::pInfo, cName, nLine )


METHOD RunAtStartup( lRunAtStartup ) CLASS HBDebugger

   ::lRunAtStartup := hb_defaultValue( lRunAtStartup, ! ::lRunAtStartup )
   ::oPulldown:GetItemByIdent( "ALTD" ):checked := ::lRunAtStartup

   RETURN Self


METHOD LineNumbers( lLineNumbers ) CLASS HBDebugger

   ::lLineNumbers := hb_defaultValue( lLineNumbers, ! ::lLineNumbers )
   ::oPulldown:GetItemByIdent( "LINE" ):checked := ::lLineNumbers
   ::oBrwText:lLineNumbers := ::lLineNumbers
   ::oBrwText:RefreshAll()

   RETURN Self


METHOD ListBox( cCaption, aItems ) CLASS HBDebugger

   LOCAL nItems
   LOCAL nMaxWid
   LOCAL nLeft
   LOCAL nTop
   LOCAL nBottom
   LOCAL nRight
   LOCAL oWndList
   LOCAL aColors
   LOCAL n

   nItems := Len( aItems )
   nMaxWid := Len( cCaption ) + 2
   AEval( aItems, {| x | nMaxWid := Max( Len( x ), nMaxWid ) } )
   nMaxWid += 2

   nTop    := Int( ( ::nMaxRow / 2 ) - Min( nItems, ::nMaxRow - 5 ) / 2 )
   nBottom := Int( ( ::nMaxRow / 2 ) + Min( nItems, ::nMaxRow - 5 ) / 2 + 1 )
   nLeft   := Int( ( ::nMaxCol / 2 ) - Min( nMaxWid, ::nMaxCol * 3 / 2 ) / 2 )
   nRight  := Int( ( ::nMaxCol / 2 ) + Min( nMaxWid, ::nMaxCol * 3 / 2 ) / 2 )
   oWndList := HBDbWindow():new( nTop, nLeft, nBottom, nRight, cCaption, ::oPullDown:cClrPopup )
   oWndList:lShadow := .T.
   oWndList:Show()

   aColors := __dbgColors()
   n := __dbgAChoice( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1, aItems, ;
      aColors[ 8 ] + "," + aColors[ 10 ] )
   oWndList:Hide()

   RETURN n


METHOD PROCEDURE LoadCallStack() CLASS HBDebugger

   LOCAL i
   LOCAL nDebugStackLen
   LOCAL nLevel
   LOCAL nStackLevel
   LOCAL nPos

   ::aProcStack := Array( ::nProcLevel )
   nDebugStackLen := __dbgProcLevel() - ::nProcLevel

   FOR i := 1 TO ::nProcLevel
      nLevel := ::nProcLevel - i + 1
      nStackLevel := i + nDebugStackLen - 1
      IF ( nPos := AScan( ::aCallStack, {| a | a[ HB_DBG_CS_LEVEL ] == nLevel } ) ) > 0
         // a procedure with debug info
         ::aProcStack[ i ] := { ::aCallStack[ nPos, HB_DBG_CS_MODULE    ],;
                                ::aCallStack[ nPos, HB_DBG_CS_FUNCTION  ],;
                                ::aCallStack[ nPos, HB_DBG_CS_LINE      ],;
                                ::aCallStack[ nPos, HB_DBG_CS_LEVEL     ],;
                                ::aCallStack[ nPos, HB_DBG_CS_LOCALS    ],;
                                ::aCallStack[ nPos, HB_DBG_CS_STATICS   ],;
                                ::aCallStack[ nPos, HB_DBG_CS_CODEBLOCK ],;
                                Len( ::aCallStack ) - ( nPos - 1 ) }
      ELSEIF ! Empty( __dbgProcFile( nStackLevel ) )
         ::aProcStack[ i ] := { __dbgProcFile( nStackLevel ),;
                                __dbgProcName( nStackLevel ),;
                                __dbgProcLine( nStackLevel ),;
                                nLevel,; 
                                {},; 
                                {},;
                                Left( __dbgProcName( nStackLevel ), 3 ) == "(b)",;
                                0 }
      ELSE
         ::aProcStack[ i ] := { Nil,;
                                __dbgProcName( nStackLevel ) + "(" + hb_ntos( __dbgProcLine( nStackLevel ) ) + ")",; 
                                Nil,; 
                                nLevel,; 
                                Nil,; 
                                Nil,; 
                                Left( __dbgProcName( nStackLevel ), 3 ) == "(b)",;
                                0 }
      ENDIF
   NEXT

   RETURN

METHOD PROCEDURE LoadColors() CLASS HBDebugger

   LOCAL oWnd

   IF ! ::lDebuggerWindowIsOpen
      RETURN
   ENDIF
   
   ::oPullDown:LoadColors()
   ::oPullDown:Refresh()
   ::BarDisplay()
   FOR EACH oWnd IN ::aWindows
      oWnd:LoadColors()
      oWnd:Refresh()
   NEXT

   RETURN


METHOD PROCEDURE LoadSettings() CLASS HBDebugger

   ::DoScript( ::cSettingsFileName )

   RETURN


METHOD PROCEDURE LoadVars() CLASS HBDebugger  // updates monitored variables

   LOCAL nCount
   LOCAL n
   LOCAL m
   LOCAL xValue
   LOCAL cName
   LOCAL hSkip

   LOCAL aBVars := {}

   IF ::lShowPublics
      nCount := __mvDbgInfo( HB_MV_PUBLIC )
      FOR n := nCount TO 1 STEP -1
         xValue := __mvDbgInfo( HB_MV_PUBLIC, n, @cName )
         AAdd( aBVars, { cName, xValue, "Public" } )
      NEXT
   ENDIF

   IF ::lShowPrivates
      /* CA-Cl*pper shows only local private variables in monitor
       * We are marking non local private variables with "^" character
       */
      nCount := __mvDbgInfo( HB_MV_PRIVATE )
      IF nCount > 0
         m := __mvDbgInfo( HB_MV_PRIVATE_LOCAL, ::nProcLevel )
         hSkip := { => }
         hb_HAllocate( hSkip, nCount )
         FOR n := nCount TO 1 STEP -1
            xValue := __mvDbgInfo( HB_MV_PRIVATE, n, @cName )
            IF ! cName $ hSkip
               AAdd( aBVars, { cName, xValue, iif( m > 0, "Private", "Private^" ) } )
               hSkip[ cName ] := NIL
            ENDIF
            --m
         NEXT
      ENDIF
   ENDIF

   IF ::aProcStack[ ::oBrwStack:Cargo, HB_DBG_CS_LINE ] != NIL
      IF ::lShowGlobals
         cName := ::aProcStack[ ::oBrwStack:Cargo, HB_DBG_CS_MODULE ]
         FOR EACH n IN ::aModules
            IF ! ::lShowAllGlobals
               IF ! ::ModuleMatch( n[ HB_DBG_MOD_NAME ], cName )
                  LOOP
               ENDIF
            ENDIF
            FOR EACH m IN n[ HB_DBG_MOD_GLOBALS ]
               AAdd( aBVars, m )
            NEXT
            IF ! ::lShowAllGlobals
               FOR EACH m IN n[ HB_DBG_MOD_EXTGLOBALS ]
                  AAdd( aBVars, m )
               NEXT
            ENDIF
         NEXT
      ENDIF

      IF ::lShowStatics
         cName := ::aProcStack[ ::oBrwStack:Cargo, HB_DBG_CS_MODULE ]
         IF ( n := AScan( ::aModules, {| a | ::ModuleMatch( a[ HB_DBG_MOD_NAME ], cName ) } ) ) > 0
            FOR EACH m IN ::aModules[ n, HB_DBG_MOD_STATICS ]
               AAdd( aBVars, m )
            NEXT
         ENDIF
         FOR EACH n IN ::aProcStack[ ::oBrwStack:Cargo, HB_DBG_CS_STATICS ]
            AAdd( aBVars, n )
         NEXT
      ENDIF

      IF ::lShowLocals
         FOR EACH n IN ::aProcStack[ ::oBrwStack:Cargo, HB_DBG_CS_LOCALS ]
            cName := n[ HB_DBG_VAR_NAME ]
            // Is there another var with this name ?
            IF ( m := AScan( aBVars, {| aVar | aVar[ HB_DBG_VAR_NAME ] == cName .AND. aVar[ HB_DBG_VAR_TYPE ] == "S" } ) ) > 0
               aBVars[ m ] := n
            ELSE
               AAdd( aBVars, n )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   IF ::oBrwVars:cargo > Len( aBVars )
      ::oBrwVars:GoTop()
   ENDIF
   ::aVars := aBVars
   IF ::lSortVars .AND. ! Empty( ::aVars )
      ::Sort()
   ENDIF

   RETURN


METHOD PROCEDURE Local() CLASS HBDebugger

   ::lShowLocals := ! ::lShowLocals
   ::RefreshVars()

   RETURN


METHOD Locate( nMode, cValue ) CLASS HBDebugger

   LOCAL lFound

   IF Empty( cValue )
      cValue := ::InputBox( "Search string", ::cSearchString )
      IF Empty( cValue )
         RETURN NIL
      ENDIF
   ENDIF

   ::cSearchString := cValue

   lFound := ::oBrwText:Search( ::cSearchString, ::lCaseSensitive, nMode )

   RETURN lFound


METHOD LocatePrgPath( cPrgName, aDirs ) CLASS HBDebugger

   LOCAL cRetPrgName
   LOCAL cDir
   LOCAL cPath
   LOCAL cName
   LOCAL cExt

   IF hb_FileExists( cPrgName )
      RETURN cPrgName
   ENDIF

   HB_FNameSplit( cPrgName, @cPath, @cName, @cExt )

   FOR EACH cDir IN aDirs
      cRetPrgName := cDir + hb_ps() + cName + cExt
      IF hb_FileExists( cRetPrgName )
         RETURN cRetPrgName
      ENDIF
   NEXT

   RETURN NIL


METHOD PROCEDURE MonoDisplay() CLASS HBDebugger

   ::lMonoDisplay := ! ::lMonoDisplay
   ::oPullDown:GetItemByIdent( "MONO" ):checked := ::lMonoDisplay
   ::LoadColors()

   RETURN


METHOD NextRoutine() CLASS HBDebugger

   __dbgSetNextRoutine( ::pInfo )
   ::Exit()

   RETURN Self

METHOD StepOut() CLASS HBDebugger

   __dbgSetStepOut( ::pInfo )
   ::Exit()

   RETURN Self

METHOD Tile() CLASS HBDebugger

   Local nTop
   Local nLeft
   Local nBottom
   Local nRight

   DispBegin()

   // Command
   ::oWndCommand:Resize( WINDOW_HEIGHT - If( ::lMenuBar, 5, 4 ), 0, WINDOW_HEIGHT - If( ::lMenuBar, 1, 0 ), WINDOW_WIDTH )

   // Stack
   ::oWndStack:Resize( If( ::lMenuBar, 1, 0 ), WINDOW_WIDTH - 15, WINDOW_HEIGHT - If( ::lMenuBar, 6, 5 ), WINDOW_WIDTH )

   nLeft := 0
   If ::oWndStack:lVisible
      nRight := ::oWndStack:nLeft - 1
   Else
      nRight := WINDOW_WIDTH
   EndIf

   // WatchPoint
   nTop  := If( ::lMenuBar, 1, 0 )
   nBottom := nTop + Min( Len( ::aWatch ) + 1, 4 )
   ::oWndPnt:Resize( nTop, nLeft, nBottom, nRight )

   // Vars
   if ::oWndPnt:lVisible
      nTop := ::oWndPnt:nBottom + 1
   else
      nTop := If( ::lMenuBar, 1, 0 )
   endif
   nBottom := nTop + Min( Len( ::aVars ) + 1, 5 )
   ::oWndVars:Resize( nTop, nLeft, nBottom, nRight )

   // Code
   If ::oWndVars:lVisible
      nTop := ::oWndVars:nBottom + 1
   ElseIf ::oWndPnt:lVisible
      nTop := ::oWndPnt:nBottom + 1
   Else
      nTop := If( ::lMenuBar, 1, 0 )
   EndIf
   nBottom := ::oWndCommand:nTop - 1
   ::oWndCode:Resize( nTop, nLeft, nBottom, nRight )

   ::oWndCode:RefreshAll()
   DispEnd()

   ::lTiled := .T.

   RETURN nil

METHOD MoveWindow( nMRow, nMCol ) CLASS HBDebugger

   ::lTiled :=  ! ::oCurrentWnd:Move( nMRow, nMCol ) .And. ::lTiled

   RETURN NIL

METHOD PROCEDURE NextWindow() CLASS HBDebugger


   Local nPos
   Local nCurr

   DispBegin()
   nCurr := AScan( ::aWindows, { | oWnd | oWnd == ::oCurrentWnd } )
   If nCurr > 0
      nPos := nCurr + 1
      If nPos > Len( ::aWindows )
         nPos := 1
      EndIf
      While ! ::aWindows[ nPos ]:lVisible .And. nPos != nCurr
         If nPos > Len( ::aWindows )
            nPos := 1
         EndIf
         nPos++
      EndDo
      ::oCurrentWnd := ::aWindows[ nPos ]
   Else
      ::oCurrentWnd := ::oCurrentWnd:NextWindow()
   EndIf
   ::oCurrentWnd:ToTop():SetFocus( .T. )
   DispEnd()

   RETURN


METHOD PROCEDURE Open( cFileName ) CLASS HBDebugger

   LOCAL nFileName
   LOCAL cRealName
   LOCAL aFiles

   IF Empty( cFileName )
      aFiles := ::GetSourceFiles()
      ASort( aFiles )
      hb_AIns( aFiles, 1, "(Another file)", .T. )

      nFileName := ::ListBox( "Please choose a source file", aFiles )
      SWITCH nFileName
      CASE 0
         RETURN
      CASE 1
         cFileName := AllTrim( ::InputBox( "Please enter the filename" ) )
         EXIT
      OTHERWISE
         cFileName := aFiles[ nFileName ]
      ENDSWITCH
   ENDIF

   IF ! Empty( cFileName ) .AND.  ( ! Empty( ::cPrgName ) .OR. ! hb_FileMatch( cFileName, ::cPrgName ) )

      cRealName := ::LocatePrgPath( cFileName, ::aPathDirs )
      IF Empty( cRealName )
         __dbgAlert( "File '" + cFileName + "' not found!" )
         RETURN
      ENDIF
      DispBegin()
      ::cPrgName := cRealName
      ::lPPO := ( Lower( hb_FNameExt( cRealName ) ) == ".ppo" )
      ::oPulldown:GetItemByIdent( "PPO" ):Checked := ::lPPO
      ::oBrwText:LoadFile( cRealName, ::nTabWidth )
      ::oBrwText:cPrgName := cRealName
      ::oBrwText:RefreshAll()
      ::RedisplayBreakpoints()               // check for breakpoints in this file and display them
      ::oWndCode:SetCaption( ::cPrgName )
      ::oWndCode:Refresh()       // to force the window caption to update
      DispEnd()
      
   ENDIF

   RETURN

METHOD OpenMenu( cName ) CLASS HBDebugger

   LOCAL nPopup := ::oPullDown:GetHotKeyPos( Left( cName, 1 ) )
   IF nPopup == 0
      RETURN .F.
   ENDIF
   IF ::oPullDown:nOpenPopup != nPopup
      IF ::oCurrentWnd:lFocused .And. HB_ISEVALITEM( ::oCurrentWnd:bLostFocus )
         Eval( ::oCurrentWnd:bLostFocus )
      ENDIF
      If ! ::lMenuBar
         ::ShowMenuBar()
      EndIf
      ::oPullDown:ShowPopup( nPopup )
   ENDIF
   
   RETURN .T.

METHOD OpenPPO() CLASS HBDebugger

   LOCAL lSuccess

   IF Empty( ::cPrgName )
      RETURN .F.
   ENDIF

   IF Lower( hb_FNameExt( ::cPrgName ) ) == ".ppo"
      ::cPrgName := hb_FNameExtSet( ::cPrgName, ".prg" )
      lSuccess := hb_FileExists( ::cPrgName )
      ::lPPO := ! lSuccess
   ELSE
      ::cPrgName := hb_FNameExtSet( ::cPrgName, ".ppo" )
      lSuccess := hb_FileExists( ::cPrgName )
      ::lPPO := lSuccess
   ENDIF

   IF lSuccess
      ::oBrwText := HBBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1, ;
         ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, ::cPrgName, ;
         __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] + "," + ;
         __dbgColors()[ 3 ] + "," + __dbgColors()[ 6 ], ::lLineNumbers, ::nTabWidth )
      ::oWndCode:Browser := ::oBrwText
      ::oWndCode:SetCaption( ::cPrgName )
      ::oWndCode:Refresh()  // to force the window caption to update
   ENDIF

   ::oPullDown:GetItemByIdent( "PPO" ):checked := ::lPPO

   RETURN lSuccess


METHOD PROCEDURE OSShell() CLASS HBDebugger

   LOCAL cImage := __dbgSaveScreen()
   LOCAL cColors := SetColor()
   LOCAL oE

   SetColor( "W/N" )
   CLS
   QOut( "Type 'exit' to RETURN to the Debugger" )
   SetCursor( SC_NORMAL )     // standard cursor for OS shell

   BEGIN SEQUENCE WITH __BreakBlock()

#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
      hb_run( GetEnv( "COMSPEC" ) )
#elif defined( __PLATFORM__UNIX )
      hb_run( GetEnv( "SHELL" ) )
#else
      ::NotSupported()
#endif

   RECOVER USING oE

      __dbgAlert( "Error: " + oE:description )

   END SEQUENCE

   SetCursor( SC_NONE )
   __dbgRestScreen( ,,,, cImage )
   SetColor( cColors )

   RETURN


METHOD PROCEDURE Quit() CLASS HBDebugger

   ::Exit()
   ::Hide()
   __dbgSetQuit( ::pInfo )
   t_oDebugger := NIL

   __Quit()

   RETURN


METHOD PathForFiles( cPathForFiles ) CLASS HBDebugger

   IF ! HB_IsString( cPathForFiles )
      cPathForFiles := ::InputBox( "Search path for source files:", ::cPathForFiles )
      If LastKey() == K_ESC .Or. ! HB_IsString( cPathForFiles )
         Return Self
      EndIf
   ENDIF

   cPathForFiles := AllTrim( cPathForFiles )
   If ! Empty( cPathForFiles )
      ::aPathDirs := PathsWithSubDirs( cPathForFiles )
      ::cPathForFiles := ""
      AEval( ::aPathDirs, { | cPath | ::cPathForFiles += hb_OSPathListSeparator() + cPath } )
      ::cPathForFiles := SubStr( ::cPathForFiles, 2 )
   Else
      ::cPathForFiles := ""
      ::aPathDirs := {}
   EndIf

   // Force reload
   ::cPrgName := ""
   ::Resume()

   RETURN Self


METHOD PrevWindow() CLASS HBDebugger

   Local nPos
   Local nCurr

   DispBegin()
   nCurr := AScan( ::aWindows, { | oWnd | oWnd == ::oCurrentWnd } )
   If nCurr > 0
      nPos := nCurr - 1
      If nPos < 1
         nPos := Len( ::aWindows )
      EndIf
      While ! ::aWindows[ nPos ]:lVisible .And. nPos != nCurr
         If nPos < 1
            nPos := Len( ::aWindows )
         EndIf
         nPos--
      EndDo
      ::oCurrentWnd := ::aWindows[ nPos ]
   Else
      ::oCurrentWnd := ::oCurrentWnd:PrevWindow()
   EndIf
   ::oCurrentWnd:ToTop():SetFocus( .T. )
   DispEnd()

   RETURN Self 

METHOD Iconize() CLASS HBDebugger

   DispBegin()
   ::oCurrentWnd:Iconize():RefreshAll()
   ::lTiled := .F.
   DispEnd()

   RETURN Self



METHOD Zoom() CLASS HBDebugger

   DispBegin()
   ::oCurrentWnd:Zoom( If( ::lMenuBar, 1, 0 ), 0, WINDOW_HEIGHT - If( ::lMenuBar, 1, 0 ), WINDOW_WIDTH ):RefreshAll()
   ::lTiled := .F.
   DispEnd()

   RETURN Self 


METHOD MenuBar() CLASS HBDebugger

   DispBegin()

   ::lMenuBar := ! ::lMenuBar
   ::oPulldown:GetItemByIdent( "MENU" ):checked := ::lMenuBar

   __dbgRestScreen( 0, 0, WINDOW_HEIGHT, WINDOW_WIDTH, ::cAppScreen )
   if ::lMenuBar
      ::oPullDown:Display()
      ::BarDisplay()
   endif
   HBDbWindow():SaveBackImage( If( ::lMenuBar, 1, 0 ), 0, WINDOW_HEIGHT - If( ::lMenuBar, 1, 0 ), WINDOW_WIDTH )

   If ::lTiled
      ::Tile()
   Else
      ::oCurrentWnd:RefreshAll()
   EndIf

   DispEnd()

   RETURN Self


METHOD ShowMenuBar() CLASS HBDebugger

   ::oPullDown:Refresh()
   ::BarDisplay()

   RETURN Self 


METHOD Size( nMRow, nMCol ) CLASS HBDebugger

   ::lTiled :=  ! ::oCurrentWnd:ChangeSize( nMRow, nMCol ) .And. ::lTiled

   RETURN Self


METHOD PROCEDURE Private() CLASS HBDebugger

   ::lShowPrivates := ! ::lShowPrivates
   ::RefreshVars()

   RETURN


METHOD PROCEDURE Public() CLASS HBDebugger

   ::lShowPublics := ! ::lShowPublics
   ::RefreshVars()

   RETURN


METHOD PROCEDURE RefreshVars() CLASS HBDebugger

   ::oPulldown:GetItemByIdent( "GLOBAL" ):checked := ::lShowGlobals
   ::oPulldown:GetItemByIdent( "LOCAL" ):checked := ::lShowLocals
   ::oPulldown:GetItemByIdent( "PRIVATE" ):checked := ::lShowPrivates
   ::oPulldown:GetItemByIdent( "PUBLIC" ):checked := ::lShowPublics
   ::oPulldown:GetItemByIdent( "STATIC" ):checked := ::lShowStatics
   ::oPulldown:GetItemByIdent( "ALL" ):checked := ::lAll
   ::oPulldown:GetItemByIdent( "SHOWALLGLOBALS" ):checked := ::lShowAllGlobals

   IF ::lShowGlobals .OR. ::lShowPublics .OR. ::lShowPrivates .OR. ::lShowStatics .OR. ::lShowLocals
      ::LoadVars()
      ::ShowVars()
   ELSE
      ::HideVars()
   ENDIF

   RETURN


METHOD PROCEDURE RestoreAppState() CLASS HBDebugger

   Set( _SET_DIRCASE, ::nAppDirCase )
   Set( _SET_FILECASE, ::nAppFileCase )
   Set( _SET_TYPEAHEAD, ::nAppTypeAhead )
   hb_keySetLast( ::nAppLastKey )
   hb_gtInfo( HB_GTI_INKEYFILTER, ::bAppInkeyAfter )
   hb_gtInfo( HB_GTI_INKEYREAD, ::bAppInkeyBefore )
   __Keyboard( ::cKeyboardBuffer )
   SetPos( ::nAppRow, ::nAppCol )
   SetColor( ::cAppColors )

   RETURN


METHOD PROCEDURE RestoreSettings( cFileName ) CLASS HBDebugger

   IF Empty( cFileName )
      ::cSettingsFileName := ::InputBox( "File name", ::cSettingsFileName )
      IF LastKey() == K_ESC
         RETURN
      ENDIF
   ELSE
      ::cSettingsFileName := cFileName
   ENDIF

   // Default extension...
   If At( ".", ::cSettingsFileName ) == 0
      ::cSettingsFileName := AllTrim( ::cSettingsFileName ) + ".cld"
   EndIf

   ::LoadSettings()
   ::ShowVars()

   RETURN


METHOD PROCEDURE SaveAppState() CLASS HBDebugger
   
   LOCAL nKey

   ::nAppDirCase     := Set( _SET_DIRCASE, 0 )
   ::nAppFileCase    := Set( _SET_FILECASE, 0 )
   ::nAppTypeAhead   := Set( _SET_TYPEAHEAD, 16 )
   ::nAppLastKey     := LastKey()
   ::bAppInkeyAfter  := hb_gtInfo( HB_GTI_INKEYFILTER, NIL )
   ::bAppInkeyBefore := hb_gtInfo( HB_GTI_INKEYREAD, NIL )
   ::nAppRow         := Row()
   ::nAppCol         := Col()
   ::cKeyboardBuffer := ""
   ::cAppColors      := SetColor()
   while( ( nKey := Inkey() ) != 0 )
      ::cKeyboardBuffer += Chr( nKey )
   EndDo

   RETURN


METHOD PROCEDURE SaveSettings( cFileName ) CLASS HBDebugger

   LOCAL cInfo := ""
   LOCAL n
   LOCAL oWnd
   LOCAL aBreak, aWatch

   IF Empty( cFileName )
      ::cSettingsFileName := ::InputBox( "File name", ::cSettingsFileName )
      IF LastKey() == K_ESC
         RETURN
      ENDIF
   ELSE
      ::cSettingsFileName := cFileName
   ENDIF

   // Default extension...
   If At( ".", ::cSettingsFileName ) == 0
      ::cSettingsFileName := AllTrim( ::cSettingsFileName ) + ".cld"
   EndIf
   
   IF ! Empty( ::cPathForFiles )
      cInfo += "Options Path " + ::cPathForFiles + hb_eol()
   ENDIF

   cInfo += "Options Colors {"
   FOR EACH n IN ::aColors
      cInfo += '"' + n + '"'
      IF ! n:__enumIsLast()
         cInfo += ","
      ENDIF
   NEXT
   cInfo += "}" + hb_eol()

   IF ::lMonoDisplay
      cInfo += "Options mono " + hb_eol()
   ENDIF

   IF ! ::lRunAtStartup
      cInfo += "Options NoRunAtStartup " + hb_eol()
   ENDIF

   IF ::nSpeed != 0
      cInfo += "Run Speed " + hb_ntos( ::nSpeed ) + hb_eol()
   ENDIF

   IF ::nTabWidth != 4
      cInfo += "Options Tab " + hb_ntos( ::nTabWidth ) + hb_eol()
   ENDIF

   IF ::lShowStatics
      cInfo += "Monitor Static" + hb_eol()
   ENDIF

   IF ::lShowPublics
      cInfo += "Monitor Public" + hb_eol()
   ENDIF

   IF ::lShowLocals
      cInfo += "Monitor Local" + hb_eol()
   ENDIF

   IF ::lShowPrivates
      cInfo += "Monitor Private" + hb_eol()
   ENDIF

   IF ::lShowGlobals
      cInfo += "Monitor Global" + hb_eol()
   ENDIF

   IF ::lSortVars
      cInfo += "Monitor Sort" + hb_eol()
   ENDIF

   IF ::lShowCallStack
      cInfo += "View CallStack" + hb_eol()
   ENDIF

   IF ! ::lLineNumbers
      cInfo += "Num Off" + hb_eol()
   ENDIF

   FOR EACH aBreak IN __dbgGetBreakPoints( ::pInfo )
      cInfo += "BP " + hb_ntos( aBreak[ HB_DBG_BP_LINE ] ) + " " + ;
               aBreak[ HB_DBG_BP_MODULE ] + hb_eol()
   NEXT

   FOR EACH aWatch IN ::aWatch
      cInfo += Upper( aWatch[ 1 ] ) + " " + aWatch[ 2 ] + hb_eol()
   NEXT

   IF ! ::lWindowsAutoSized
      /* This part of the script must be executed after all windows are created */
      FOR EACH oWnd IN ::aWindows
         cInfo += ;
            "Window Size " + hb_ntos( oWnd:nBottom - oWnd:nTop + 1 ) + " " + ;
            hb_ntos( oWnd:nRight - oWnd:nLeft + 1 ) + hb_eol() + ;
            "Window Move " + hb_ntos( oWnd:nTop ) + " " + ;
            hb_ntos( oWnd:nLeft ) + hb_eol() + ;
            "Window Next" + hb_eol()
      NEXT
   ENDIF

   hb_MemoWrit( ::cSettingsFileName, cInfo )

   RETURN


METHOD ResizeCmdWnd( nLines ) CLASS HBDebugger

   LOCAL nRight
   LOCAL nTop
   LOCAL aShow
   LOCAL oWnd

   IF ! ::lTiled
      RETURN Self
   ENDIF
   
   nTop := 1
   nRight := ::nMaxCol
   IF ::oWndVars:lVisible
      nTop := Max( nTop, ::oWndVars:nBottom + 1 )
   ENDIF
   IF ::oWndPnt:lVisible
      nTop := Max( nTop, ::oWndPnt:nBottom + 1 )
   ENDIF
   IF ::oWndStack:lVisible
      nRight -= 16
   ENDIF

   IF ::nCmdWndHight + nLines >= 2 .AND. ::nMaxRow - ( ::nCmdWndHight + nLines + 4 ) > nTop

      DispBegin()
      aShow := {}
      IF ::oWndCode:lVisible
         ::oWndCode:Hide( .F. )
         AAdd( aShow, ::oWndCode )
      ENDIF
      IF ::oWndPnt:lVisible
         ::oWndPnt:Hide( .F. )
         AAdd( aShow, ::oWndPnt )
      ENDIF
      IF ::oWndVars:lVisible
         ::oWndVars:Hide( .F. )
         AAdd( aShow, ::oWndVars )
      ENDIF
      IF ::oWndStack:lVisible
         ::oWndStack:Hide( .F. )
         AAdd( aShow, ::oWndStack )
      ENDIF
      IF ::oWndCommand:lVisible
         ::oWndCommand:Hide( .F. )
         AAdd( aShow, ::oWndCommand )
      ENDIF

      ::nCmdWndHight += nLines
      ::oWndCommand:Resize( ::nMaxRow - ::nCmdWndHight - 2, 0, ::nMaxRow - 1, ::nMaxCol )
      ::oGetCommand:newPos( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3 )
      ::oBrwStack:nTop := 2
      ::oBrwStack:nLeft := ::nMaxCol - 14
      ::oBrwStack:nRight := ::nMaxCol - 1
      ::oBrwStack:nBottom := ::nMaxRow - ::nCmdWndHight - 4
      ::oBrwStack:Configure()
      ::oWndStack:Resize( , nRight + 1, ::nMaxRow - ::nCmdWndHight - 3, ::nMaxCol )

      IF ::oWndVars:lVisible
         ::oWndVars:Resize( , , , nRight )
      ENDIF
      IF ::oWndPnt:lVisible
         ::oWndPnt:Resize( , , , nRight )
      ENDIF
      ::oWndCode:Resize( nTop, 0, ::nMaxRow - ::nCmdWndHight - 3, nRight )
      FOR EACH oWnd IN aShow DESCEND
         oWnd:Show()
      NEXT
      DispEnd()

   ENDIF

   RETURN Self


METHOD PROCEDURE SearchLine( cLine ) CLASS HBDebugger

   ::GotoLine( Max( 1, iif( HB_ISSTRING( cLine ) .AND. IsDigit( cLine ), ;
                            Val( cLine ), ::InputBox( "Line number", 1 ) ) ) )
   RETURN


METHOD PROCEDURE Show() CLASS HBDebugger
   
   DispBegin()

   If ::lMenuBar
      ::oPullDown:Display()
      ::BarDisplay()
   EndIf

    If ::nMaxRow != WINDOW_HEIGHT .Or. ::nMaxCol != WINDOW_WIDTH
      ::nMaxRow := WINDOW_HEIGHT
      ::nMaxCol := WINDOW_WIDTH
      If ::lTiled
         ::Tile()
      EndIf   
   EndIf

   ::oWndCode:Show()

   If ::lShowCallStack
     ::ShowCallStack()
   EndIf
   ::loadVars()
   ::ShowVars()
   If ::oWndPnt:lVisible
     ::WatchpointsShow()
   EndIf

   ::oWndCommand:Show()

   hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ">", __DbgColors()[ 2 ] )

   // show the topmost procedure
   ::ShowCodeLine( 1 )

   If ::oCurrentWnd:bGotFocus != Nil
      Eval( ::oCurrentWnd:bGotFocus )
   EndIf

   DispEnd()

   RETURN


METHOD PROCEDURE ShowAllGlobals() CLASS HBDebugger

   ::lShowAllGlobals := ! ::lShowAllGlobals
   ::RefreshVars()

   RETURN


METHOD PROCEDURE ShowAppScreen() CLASS HBDebugger

   ::CloseDebuggerWindow()

   IF LastKey() == K_LBUTTONDOWN
      __dbgInkey()
   ENDIF
   DO WHILE __dbgInkey() == K_MOUSEMOVE
   ENDDO

   ::OpenDebuggerWindow()

   RETURN


METHOD PROCEDURE ShowCallStack() CLASS HBDebugger

   ::lShowCallStack := .T.

   DispBegin()

   ::oBrwStack:RefreshAll()
   ::oWndStack:Show( .f. )

   If ::lTiled
      ::Tile()
   EndIf

   DispEnd()

   RETURN

METHOD PROCEDURE ShowCodeLine( nProc ) CLASS HBDebugger

   LOCAL nLine
   LOCAL cPrgPathName

   // we only update the stack window and up a new browse
   // to view the code if we have just broken execution
   IF ! ::lGo // .AND. !::lTrace

      ::oBrwStack:RefreshAll()
      ::oBrwText:lCodeBlock := ::aProcStack[ nProc, HB_DBG_CS_CODEBLOCK ]
      nLine                 := ::aProcStack[ nProc, HB_DBG_CS_LINE ]
      cPrgPathName          := ::aProcStack[ nProc, HB_DBG_CS_MODULE ]
      
      IF nLine == NIL .Or. ::aProcStack[ nProc, HB_DBG_CS_STACKINDEX ] == 0
         ::oBrwText:LoadFile( "", ::nTabWidth )
         If ! Empty( ::aProcStack[ nProc, HB_DBG_CS_MODULE ] )
            ::cPrgName := hb_FNameName( ::aProcStack[ nProc, HB_DBG_CS_MODULE ] ) + hb_FNameExt( ::aProcStack[ nProc, HB_DBG_CS_MODULE ] )
            ::oWndCode:SetCaption( ::cPrgName + ": Code not available" )
         Else
            ::cPrgName := ""
            ::oWndCode:SetCaption( ::aProcStack[ nProc, HB_DBG_CS_FUNCTION ] + ": Code not available" )
         EndIf
         ::oWndCode:Refresh() // to force the window caption to update
         ::oBrwText:SetActiveLine( 0 )
         ::GotoLine( nLine )
         RETURN
      ENDIF

      IF ::lPPO
         cPrgPathName := hb_FNameExtSet( cPrgPathName, ".ppo" )
      ENDIF

      IF ! Empty( cPrgPathName ) .And. ! cPrgPathName == ::oBrwText:cFileName .And. ! ::ModuleMatch( cPrgPathName, ::cPrgName )

         cPrgPathName := ::LocatePrgPath( cPrgPathName, ::aPathDirs )
         
         IF Empty( cPrgPathName )
            
            ::cPrgName := hb_FNameName( ::aProcStack[ nProc, HB_DBG_CS_MODULE ] ) + hb_FNameExt( ::aProcStack[ nProc, HB_DBG_CS_MODULE ] )
            ::oBrwText:LoadFile( "", ::nTabWidth )
            ::oWndCode:SetCaption( ::cPrgName + "  File not found" )
            ::oWndCode:Refresh()
            
         ELSEIF ! cPrgPathName == ::oBrwText:cFileName
            
            ::cPrgName := hb_FNameName( cPrgPathName ) + hb_FNameExt( cPrgPathName )
            ::oBrwText:LoadFile( cPrgPathName, ::nTabWidth )
            ::RedisplayBreakpoints()               // check for breakpoints in this file and display them
            ::oWndCode:SetCaption( ::cPrgName + IIF( ::aProcStack[ nProc, HB_DBG_CS_STACKINDEX ] <= 0, ": Without debug info", "" ) )
            ::oWndCode:Refresh()                   // to force the window caption to update

         ENDIF

      ENDIF
      ::oBrwText:SetActiveLine( IIF( nProc == 1, nLine, 0 ) )
      ::GotoLine( nLine )
   ENDIF

   RETURN


METHOD PROCEDURE ShowHelp( cTopic ) CLASS HBDebugger

   __dbgHelp( cTopic )

   RETURN


#define MAX_VARS_HEIGHT  7

METHOD PROCEDURE ShowVars() CLASS HBDebugger

   IF ::lGo
      RETURN
   ENDIF

   IF ! ( ::lShowLocals .OR. ::lShowStatics .OR. ::lShowPrivates .OR. ::lShowPublics .OR. ::lShowGlobals )
      RETURN
   ENDIF

   ::oWndVars:cCaption := "Monitor:" + ;
                          iif( ::lShowGlobals, " Global", "" ) + ;
                          iif( ::lShowLocals, " Local", "" ) + ;
                          iif( ::lShowStatics, " Static", "" ) + ;
                          iif( ::lShowPrivates, " Private", "" ) + ;
                          iif( ::lShowPublics, " Public", "" )

   DispBegin()

   If ::lTiled
      ::oWndVars:Show()
      ::Tile()
   Else
      ::oWndVars:Resize( ,, ::oWndVars:nTop + Min( Len( ::aVars ) + 1, 5 ) )
      ::oWndVars:Show()
      ::oWndVars:RefreshAll()
   EndIf

   DispEnd()
   

   RETURN


METHOD PROCEDURE Stack( cParam ) CLASS HBDebugger

   SWITCH iif( HB_ISSTRING( cParam ), cParam, "" )
      CASE "ON"
         ::lShowCallStack := .T.
         EXIT
      CASE "OFF"
         ::lShowCallStack := .F.
         EXIT
      OTHERWISE
         ::lShowCallStack := ! ::lShowCallStack
   ENDSWITCH

   ::oPulldown:GetItemByIdent( "CALLSTACK" ):checked := ::lShowCallStack

   IF ::lShowCallStack
      ::ShowCallStack()
   ELSE
      ::HideCallStack()
   ENDIF

   RETURN


METHOD PROCEDURE Static() CLASS HBDebugger

   ::lShowStatics := ! ::lShowStatics
   ::RefreshVars()

   RETURN


METHOD PROCEDURE Step() CLASS HBDebugger

   DispBegin()
   ::oBrwStack:GoTop()
   ::Exit()
   DispEnd()

   RETURN


METHOD ToCursor() CLASS HBDebugger

   IF ::IsValidStopLine( ::cPrgName, ::oBrwText:nCurRow )
      __dbgSetToCursor( ::pInfo, ::cPrgName, ::oBrwText:nCurRow )
      ::Exit()
   ENDIF

   RETURN Self


// Toggle a breakpoint at the cursor position in the currently viewed file
// which may be different from the file in which execution was broken
METHOD PROCEDURE BreakPointToggle( nLine, cFileName ) CLASS HBDebugger

   // look for a breakpoint which matches both line number and module name

   LOCAL nAt

   IF nLine == NIL
      cFileName := ::cPrgName
      nLine     := ::oBrwText:nCurRow
   ENDIF

   nAt := __dbgIsBreak( ::pInfo, cFileName, nLine )
   IF nAt >= 0
      __dbgDelBreak( ::pInfo, nAt )
       ::oBrwText:ToggleBreakPoint(nLine, .F.)
   ELSEIF ::IsValidStopLine( cFileName, nLine )
      __dbgAddBreak( ::pInfo, cFileName, nLine )
       ::oBrwText:ToggleBreakPoint(nLine, .T.)
   ENDIF

   ::oBrwText:RefreshCurrent()
   ::oBrwText:ForceStable()

   RETURN


METHOD BreakPointDelete( cPos ) CLASS HBDebugger

   LOCAL nAt

   IF Empty( cPos )
      cPos := AllTrim( ::InputBox( "Item number to delete", "0" ) )
      IF LastKey() == K_ESC
         cPos := ""
      ENDIF
   ENDIF

   IF IsDigit( cPos )
      __dbgDelBreak( ::pInfo, Val( cPos ) )
   ELSEIF Upper( cPos ) == "ALL"
      FOR nAt := Len( __dbgGetBreakPoints( ::pInfo ) ) - 1 TO 0 STEP -1
         __dbgDelBreak( ::pInfo, nAt )
      NEXT
   ENDIF

   ::oBrwText:RefreshAll():ForceStable()

   RETURN Self


METHOD BreakPointFunc( cFuncName ) CLASS HBDebugger

   __dbgAddBreak( ::pInfo,,, cFuncName )

   RETURN Self


METHOD BreakPointList() CLASS HBDebugger

   LOCAL aBreak, cType

   FOR EACH aBreak IN __dbgGetBreakPoints( ::pInfo )
      cType := iif( aBreak[ HB_DBG_BP_FUNC ] != NIL, ;
                    aBreak[ HB_DBG_BP_FUNC ], ;
                    hb_ntos( aBreak[ HB_DBG_BP_LINE ] ) + " " + ;
                    aBreak[ HB_DBG_BP_MODULE ] )
      ::CommandWindowDisplay( hb_ntos( aBreak:__enumIndex() - 1 ) + ") " + ;
                              cType, .F. )
   NEXT

   RETURN Self


METHOD Trace() CLASS HBDebugger

   __dbgSetTrace( ::pInfo )
   ::Step() // forces a Step()

   RETURN Self


METHOD TracepointAdd( cExpr ) CLASS HBDebugger

   LOCAL aWatch
   LOCAL nLastKey

   IF cExpr == NIL
      cExpr := AllTrim( ::InputBox( "Enter Tracepoint",, __dbgExprValidBlock() ) )
      IF LastKey() == K_ESC
         RETURN Self
      ENDIF
   ENDIF

   cExpr := AllTrim( cExpr )

   IF Empty( cExpr )
      RETURN Self
   ENDIF
   nLastKey := LastKey()
   aWatch := { "tp", cExpr, NIL }
   ::RestoreAppState()
   __dbgAddWatch( ::pInfo, cExpr, .T. )
   ::SaveAppState()
   AAdd( ::aWatch, aWatch )
   ::WatchpointsShow()
   hb_keySetLast( nLastKey )

   RETURN Self


METHOD VarGetInfo( aVar ) CLASS HBDebugger

   LOCAL uValue := ::VarGetValue( aVar )
   LOCAL cType

   SWITCH aVar[ HB_DBG_VAR_TYPE ]
      CASE "G"  ; cType := "Global" ; EXIT
      CASE "L"  ; cType := "Local" ; EXIT
      CASE "S"  ; cType := "Static" ; EXIT
      OTHERWISE ; cType := aVar[ HB_DBG_VAR_TYPE ]
   ENDSWITCH

   RETURN aVar[ HB_DBG_VAR_NAME ] + " <" + cType + ", " + ValType( uValue ) + ">: " + __dbgValToStr( uValue )


METHOD VarGetValue( aVar ) CLASS HBDebugger

   SWITCH aVar[ HB_DBG_VAR_TYPE ]
      CASE "G" ; RETURN __dbgVMVarGGet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ] )
      CASE "L" ; RETURN __dbgVMVarLGet( __dbgProcLevel() - aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ] )
      CASE "S" ; RETURN __dbgVMVarSGet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ] )
   ENDSWITCH

   // Public or Private created in ::LoadVars(), value stored in HB_DBG_VAR_INDEX
   RETURN aVar[ HB_DBG_VAR_MVALUE ]


METHOD VarSetValue( aVar, uValue ) CLASS HBDebugger

   LOCAL nProcLevel

   SWITCH aVar[ HB_DBG_VAR_TYPE ]
   CASE "G"
      __dbgVMVarGSet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ], uValue )
      EXIT
   CASE "L"
      nProcLevel := __dbgProcLevel() - aVar[ HB_DBG_VAR_FRAME ]  // skip debugger stack
      __dbgVMVarLSet( nProcLevel, aVar[ HB_DBG_VAR_INDEX ], uValue )
      EXIT
   CASE "S"
      __dbgVMVarSSet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ], uValue )
      EXIT
   OTHERWISE
      // Public or Private created in ::LoadVars(), value stored in HB_DBG_VAR_INDEX
      aVar[ HB_DBG_VAR_MVALUE ] := uValue
      &( aVar[ HB_DBG_VAR_NAME ] ) := uValue
   ENDSWITCH

   RETURN Self


METHOD PROCEDURE ViewSets() CLASS HBDebugger

   LOCAL aSets    := __dbgGetSETs()
   LOCAL oWndSets := HBDbWindow():New( 1, 7, ::nMaxRow - 2, ::nMaxCol - 7, "System Settings[1.." + hb_ntos( aTail( aSets )[ HB_DBG_SET_POS ] ) + "]", ::ClrModal() )
   LOCAL oBrwSets := HBDbBrowser():new( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   LOCAL nWidth   := oWndSets:nRight - oWndSets:nLeft - 1
   LOCAL oCol

   oBrwSets:Cargo         := 1
   oBrwSets:autolite      :=.f.
   oBrwSets:ColorSpec     := ::ClrModal()
   oBrwSets:GoTopBlock    := { || oBrwSets:cargo := 1 }
   oBrwSets:GoBottomBlock := { || oBrwSets:cargo := Len( aSets ) }
   oBrwSets:SkipBlock     := { | nSkip, nOld | nOld := oBrwSets:Cargo,;
                                              oBrwSets:Cargo += nSkip,;
                                              oBrwSets:Cargo := Min( Max( oBrwSets:Cargo, 1 ),;
                                              Len( aSets ) ), oBrwSets:Cargo - nOld }
   oBrwSets:AddColumn( ocol := TBColumnNew( "", { || PadR( aSets[ oBrwSets:cargo, 2 ], 12 ) } ) )
   ocol:width               := 12
   ocol:defcolor            := {1,2}
   oBrwSets:AddColumn( oCol := TBColumnNew( "", { || PadR( __dbgValToStr( Set( aSets[ oBrwSets:cargo, 1 ] ) ), nWidth - 13 ) } ) )
   ocol:defcolor            := {1,3}
   ocol:width               := nWidth - 13
   oWndSets:Browser         := oBrwSets
   oWndSets:bPainted        := { || oBrwSets:RefreshAll():ForceStable(), HiLiteRow( oBrwSets ) }
   oWndSets:bKeyPressed     := { | nKey | SetsKeyPressed( aSets, nKey, oBrwSets, oWndSets, "System Settings",;
                                                          { || ::EditSet( aSets[ oBrwSets:Cargo, 1 ], oBrwSets ) } ) }

   oWndSets:ShowModal()

   RETURN


METHOD WatchGetInfo( nWatch ) CLASS HBDebugger

   LOCAL xVal
   LOCAL cType
   LOCAL lValid
   LOCAL aVar
   LOCAL aWatch := ::aWatch[ nWatch ]
   LOCAL nLastKey

   IF __dbgIsIdentifier( aWatch[ WP_EXPR ] )
      aVar := __dbgGetVarWatch( ::pInfo, nWatch - 1, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )
   
      IF aVar != NIL
         nLastKey := LastKey()
         ::RestoreAppState()
         xVal := __dbgGetExprValue( ::pInfo, nWatch, @lValid, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )
         ::SaveAppState()
         hb_keySetLast( nLastKey )
         IF lValid
            cType := ValType( xVal )
            xVal  := __dbgValToStr( xVal )
         ELSE
            cType := "U"
            xVal := "Undefined"
         ENDIF
         __dbgFreeVarWatch( aVar )
      ELSE
         cType := "U"
         xVal := "Variable does not exist"
      ENDIF
   ELSE   
      xVal := ::GetExprValue( nWatch, @lValid )
      IF lValid
         cType := ValType( xVal )
         xVal  := __dbgValToStr( xVal )
      ELSE
         cType := "U"
         xVal := "Undefined"
      ENDIF
   ENDIF


   RETURN aWatch[ WP_EXPR ] + " <" + aWatch[ WP_TYPE ] + ", " + cType + ">: " + xVal


METHOD WatchpointAdd( cExpr ) CLASS HBDebugger

   LOCAL aWatch

   IF cExpr == NIL
      cExpr := ::InputBox( "Enter Watchpoint",, __dbgExprValidBlock() )
      IF LastKey() == K_ESC
         RETURN Self
      ENDIF
   ENDIF

   cExpr := AllTrim( cExpr )

   IF Empty( cExpr )
      RETURN Self
   ENDIF

   aWatch := { "wp", cExpr }
   __dbgAddWatch( ::pInfo, cExpr, .F. )
   AAdd( ::aWatch, aWatch )
   ::WatchpointsShow()

   RETURN Self


METHOD WatchpointDel( xPos ) CLASS HBDebugger

   LOCAL nPos := -1, lAll := .F.

   IF ::oWndPnt:lVisible
      IF Empty( xPos )
         nPos := ::InputBox( "Enter item number to delete", ::oBrwPnt:cargo - 1 )
         IF LastKey() == K_ESC
            nPos := -1
         ENDIF
      ELSEIF HB_ISSTRING( xPos )
         IF Upper( xPos ) == "ALL"
            lAll := .T.
         ELSEIF IsDigit( xPos )
            nPos := Val( xPos )
         ENDIF
      ELSEIF HB_ISNUMERIC( xPos )
         nPos := xPos - 1
      ENDIF

      IF lAll .OR. ( nPos >= 0 .AND. nPos < Len( ::aWatch ) )
         ::oBrwPnt:gotop()
         IF lAll
            FOR nPos := Len( ::aWatch ) - 1 TO 0 STEP -1
               __dbgDelWatch( ::pInfo, nPos )
            NEXT
            ASize( ::aWatch, 0 )
         ELSE
            __dbgDelWatch( ::pInfo, nPos )
            hb_ADel( ::aWatch, nPos + 1, .T. )
         ENDIF
         IF Len( ::aWatch ) == 0
            ::WatchpointsHide()
         ELSE
            ::WatchpointsShow()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

METHOD WatchpointEdit( nPos ) CLASS HBDebugger

   LOCAL cVarName
   LOCAL cVarStr
   LOCAL aVar
   LOCAL cExpr
   LOCAL aWatch
   LOCAL oErr
   LOCAL xResult
   LOCAL lValid
   LOCAL nLastKey
   LOCAL xVal
   
   IF __dbgIsIdentifier( ::aWatch[ nPos, WP_EXPR ] )
      aVar := __dbgGetVarWatch( ::pInfo, nPos - 1, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )

      IF aVar != NIL

         cVarName := ::aWatch[ nPos, WP_EXPR ]
         cVarStr  := ::EditValue( cVarName, aVar[ WP_VAR_VALUE ] )

         IF LastKey() != K_ESC
            nLastKey := LastKey()
            ::RestoreAppState()
            xResult := __dbgGetExprValue( ::pInfo, cVarStr, @lValid, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )
            ::SaveAppState()
            hb_keySetLast( nLastKey )
            IF lValid
               __dbgSetVarWatch( aVar[ WP_VAR_SCOPE ], xResult, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )
            ELSE
               oErr := xResult
               IF oErr:ClassName() == "ERROR"
                  xResult := oErr:operation + ": " + oErr:description
                  IF HB_ISARRAY( oErr:args )
                     xResult += "; arguments:"
                     AEval( oErr:args, {| x, i | xResult += iif( i == 1, " ", ", " ) + __dbgValToStr( x ) } )
                  ENDIF
               ELSE
                  xResult := "Syntax error"
               ENDIF
                __dbgAlert( xResult )
            ENDIF
         ENDIF
         
         __dbgFreeVarWatch( aVar )
      ELSE

         cVarName := ::aWatch[ nPos, WP_EXPR ]
         cVarStr  := ::EditValue( cVarName, NIL )
         IF LastKey() != K_ESC
            nLastKey := LastKey()
            ::RestoreAppState()
            xResult := __dbgGetExprValue( ::pInfo, cVarStr, @lValid, ::aProcStack[ ::oBrwStack:cargo, HB_DBG_CS_STACKINDEX ] )
            ::SaveAppState()
            hb_keySetLast( nLastKey )
            IF lValid
               __mvPublic( cVarName )
               __mvPut( cVarName, xResult )
            ELSE
               oErr := xResult
               IF oErr:ClassName() == "ERROR"
                  xResult := oErr:operation + ": " + oErr:description
                  IF HB_ISARRAY( oErr:args )
                     xResult += "; arguments:"
                     AEval( oErr:args, {| x, i | xResult += iif( i == 1, " ", ", " ) + __dbgValToStr( x ) } )
                  ENDIF
               ELSE
                  xResult := "Syntax error"
               ENDIF
                __dbgAlert( xResult )
            ENDIF
         ENDIF
         
      ENDIF
   
   ELSE
      xVal := ::GetExprValue( nPos, @lValid )
      IF lValid .AND. ValType( xVal ) $ "AHOPB"
         ::EditValue( ::aWatch[ nPos, WP_EXPR ], xVal, .F. )
      ELSE
         cExpr := ::InputBox( "Enter Watchpoint", ::aWatch[ nPos, WP_EXPR ], __dbgExprValidBlock() )
         IF LastKey() == K_ESC
            RETURN Self
         ENDIF

         cExpr := AllTrim( cExpr )

         IF Empty( cExpr )
            RETURN Self
         ENDIF

         aWatch := { "wp", cExpr }

         __dbgSetWatch( ::pInfo, nPos - 1, cExpr, .F. )
         ::aWatch[ nPos ] := aWatch
      ENDIF
      
   ENDIF

   IF ::oWndVars:lVisible
      ::oWndVars:Refresh()
   ENDIF
   ::oBrwPnt:RefreshCurrent()
   ::oBrwPnt:ForceStable()

   RETURN Self


METHOD WatchpointInspect( nPos ) CLASS HBDebugger

   LOCAL xValue
   LOCAL lValid

   xValue := ::GetExprValue( nPos, @lValid )

   ::InputBox( ::aWatch[ nPos, WP_EXPR ], xValue,, .F. )
   ::RefreshVars()

   RETURN Self


METHOD PROCEDURE WatchpointsHide() CLASS HBDebugger

   Local lFocused := ::oWndPnt:lFocused

   DispBegin()
   ::oWndPnt:Hide( .F. )

   IF ::lTiled
      ::Tile()
   ENDIF

   IF lFocused
      ::SetFocusCodeWindow()
   ELSE
      ::oCurrentWnd:SetFocus( .t. )
   ENDIF
   DispEnd()

   RETURN


METHOD PROCEDURE WatchpointsShow() CLASS HBDebugger

   Local nRow
   Local nCol
   Local nCursor

   IF ::lGo
      RETURN
   ENDIF

   IF Len( ::aWatch ) == 0
      RETURN
   ENDIF

   nRow    := Row()
   nCol    := Col()
   nCursor := SetCursor()

   DispBegin()

   If ::oBrwPnt:cargo > Len( ::aWatch )
      ::oBrwPnt:cargo := Len( ::aWatch )
   ElseIf ::oBrwPnt:cargo < 1
      ::oBrwPnt:cargo := 1
   EndIf

   If ::lTiled
      ::oWndPnt:Show()
      ::Tile()
   Else
      ::oWndPnt:Resize( ,, ::oWndPnt:nTop + Min( Len( ::aWatch ) + 1, 4 ) )
      ::oWndPnt:Show()
      ::oWndPnt:RefreshAll()
   EndIf

   SetPos( nRow, nCol )
   SetCursor( nCursor )

   DispEnd()

   RETURN


METHOD PROCEDURE WatchpointList() CLASS HBDebugger

   LOCAL aWatch, cType

   FOR EACH aWatch IN ::aWatch
      SWITCH aWatch[ WP_TYPE ]
      CASE "wp"
         cType := "WatchPoint"
         EXIT
      CASE "tp"
         cType := "TracePoint"
         EXIT
      OTHERWISE
         cType := aWatch[ WP_TYPE ]
      ENDSWITCH
      ::CommandWindowDisplay( hb_ntos( aWatch:__enumIndex() - 1 ) + ") " + ;
                              cType + " " + ;
                              AllTrim( aWatch[ WP_EXPR ] ), .F. )
   NEXT

   RETURN


METHOD PROCEDURE WndVarsLButtonDown( nMRow, nMCol ) CLASS HBDebugger

   IF ::oWndVars:IsCloseButton( nMRow, nMCol )

      ::lShowPublics := ::lShowPrivates := ::lShowStatics := ::lShowLocals  := ::lShowGlobals  := ::lAll := .F.
      ::RefreshVars()


   ELSEIF nMRow > ::oWndVars:nTop    .and. ;
          nMRow < ::oWndVars:nBottom .and. ;
          nMCol > ::oWndVars:nLeft   .and. ;
          nMCol < ::oWndVars:nRight

      IF nMRow - ::oWndVars:nTop >= 1 .and. ;
         nMRow - ::oWndVars:nTop <= Len( ::aVars )
         DispBegin()
         WHILE ::oBrwVars:RowPos > nMRow - ::oWndVars:nTop
            ::oBrwVars:Up()
            ::oBrwVars:ForceStable()
         END
         WHILE ::oBrwVars:RowPos < nMRow - ::oWndVars:nTop
            ::oBrwVars:Down()
            ::oBrwVars:ForceStable()
         END
         DispEnd()
      ENDIF

   ENDIF

   RETURN

METHOD WndPntLButtonDown( nMRow, nMCol ) CLASS HBDebugger

   IF ::oWndPnt:IsCloseButton( nMRow, nMCol )

      ::WatchPointsHide()

   ELSEIF nMRow > ::oWndPnt:nTop    .and. ;
          nMRow < ::oWndPnt:nBottom .and. ;
          nMCol > ::oWndPnt:nLeft   .and. ;
          nMCol < ::oWndPnt:nRight

      IF nMRow - ::oWndPnt:nTop >= 1 .and. ;
         nMRow - ::oWndPnt:nTop <= Len( ::aWatch )
         DispBegin()
         WHILE ::oBrwPnt:RowPos > nMRow - ::oWndPnt:nTop
            ::oBrwPnt:Up()
            ::oBrwPnt:ForceStable()
         END
         WHILE ::oBrwPnt:RowPos < nMRow - ::oWndPnt:nTop
            ::oBrwPnt:Down()
            ::oBrwPnt:ForceStable()
         END
         DispEnd()
      ENDIF

   EndIf

   RETURN NIL


METHOD RedisplayBreakPoints() CLASS HBDebugger
   LOCAL aBreak
   
   FOR EACH aBreak IN __dbgGetBreakPoints( ::pInfo )
    IF hb_FileMatch( aBreak[ HB_DBG_BP_MODULE ], hb_FNameName( ::cPrgName ) )
      ::oBrwText:ToggleBreakPoint(aBreak[ HB_DBG_BP_LINE ], .T.)
    ENDIF
  NEXT
  RETURN Self

METHOD SetFocusCodeWindow() CLASS HBDebugger

   DispBegin()
   ::oCurrentWnd := ::oWndCode
   ::oWndCode:ToTop():SetFocus( .T. )
   DispEnd()

   RETURN Self

STATIC PROCEDURE SetsKeyPressed( aData, nKey, oBrw, oWnd, cCaption, bEdit )

   LOCAL lKeyProcessed := .F.

   DO CASE
      CASE nKey == K_UP
              oBrw:Up()
              lKeyProcessed := .T.
      CASE nKey == K_DOWN
              oBrw:Down()
              lKeyProcessed := .T.
      CASE nKey == K_HOME .or. (nKey == K_CTRL_PGUP) .or. (nKey == K_CTRL_HOME)
              oBrw:GoTop()
              lKeyProcessed := .T.
      CASE nKey == K_END .or. (nkey == K_CTRL_PGDN) .or. (nkey == K_CTRL_END )
              oBrw:GoBottom()
              lKeyProcessed := .T.
      CASE nKey == K_PGDN
              oBrw:pageDown()
              lKeyProcessed := .T.
      CASE nKey == K_PGUP
              oBrw:PageUp()
              lKeyProcessed := .T.

      CASE nKey == K_ENTER
           if bEdit != nil
              Eval( bEdit )
              if LastKey() == K_ENTER
                 oBrw:refreshCurrent():forceStable()
                 oBrw:Down()
              endif
              lKeyProcessed := .T.
           endif

   ENDCASE

   IF lKeyProcessed
      DispBegin()
      HiLiteRow( oBrw )
      oWnd:SetCaption( cCaption + "[" + hb_ntos( aData[ oBrw:Cargo, 1 ] ) + ".." + hb_ntos( aData[ Len( aData ), 1 ] ) + "]" )
      oWnd:ShowCaption()
      DispEnd()
   ENDIF

   RETURN


FUNCTION __dbgColors()
   RETURN t_oDebugger:GetColors()


FUNCTION __dbg()
   RETURN t_oDebugger

Static Function PathsWithSubDirs( cPathList )
   Local cDir
   Local cPath
   Local aAllPaths := {}
   Local aPaths
   Local aSubDirectories
   Local aSubDir

   aPaths := PathToArray( cPathList )
   for each cDir In aPaths
      cPath := AllTrim( cDir )
      If Right( cPath, 2 ) == "\*" .Or. Right( cPath, 2 ) == "/*"
         cPath := Left( cPath, Len( cPath ) - 2 )
         AAdd( aAllPaths, cPath )
         aSubDirectories := Directory( cPath + hb_ps() + "*", "D" )
         For Each aSubDir In aSubDirectories 
            If "D" $ aSubDir[ F_ATTR ] .And. ! Left( aSubDir[ F_NAME ], 1 ) == "." 
               AEval( PathsWithSubDirs( cPath + hb_ps() + aSubDir[ F_NAME ] ), { | cItem | AAdd( aAllPaths, cItem ) } )
            EndIf
         Next
      ElseIf Right( cPath, 1 ) == "*"
         aSubDirectories := Directory( cPath, "D" )
         For Each aSubDir In aSubDirectories 
            If "D" $ aSubDir[ F_ATTR ] .And. ! Left( aSubDir[ F_NAME ], 1 ) == "." 
               AEval( PathsWithSubDirs( cPath + hb_ps() + aSubDir[ F_NAME ] ), { | cItem | AAdd( aAllPaths, cItem ) } )
            EndIf
         Next
      ElseIf ! Empty( cPath )
         AAdd( aAllPaths, cPath )
      EndIf
   Next

Return aAllPaths 

STATIC FUNCTION PathToArray( cList )

   LOCAL aList := {}
   LOCAL cSep := hb_osPathListSeparator()
   LOCAL cDirSep := hb_osPathDelimiters()
   LOCAL nPos

   IF cList != NIL

      DO WHILE ( nPos := At( cSep, cList ) ) > 0
         AAdd( aList, Left( cList, nPos - 1 ) )        // Add a new element
         cList := SubStr( cList, nPos + 1 )
      ENDDO

      AAdd( aList, cList )              // Add final element

      /* Strip ending delimiters */
      AEval( aList, {| x, i | iif( Right( x, 1 ) $ cDirSep, aList[ i ] := hb_StrShrink( x ), ) } )
   ENDIF

   RETURN aList


/* Check if a string starts with another string with a min length */
STATIC FUNCTION hb_LeftEqN( cLine, cStart, nMin )
   RETURN Len( cStart ) >= nMin .AND. hb_LeftEq( cLine, cStart )


FUNCTION __dbgExprValidBlock( cType )

   LOCAL cTypeName

   IF HB_ISSTRING( cType )
      SWITCH cType
      CASE "N"
         cTypeName := "numeric"
         EXIT
      CASE "C"
         cTypeName := "string"
         EXIT
      CASE "L"
         cTypeName := "logical"
         EXIT
      CASE "D"
         cTypeName := "date"
         EXIT
      CASE "T"
         cTypeName := "timestamp"
         EXIT
      CASE "S"
         cTypeName := "symbol"
         EXIT
      CASE "A"
         cTypeName := "array"
         EXIT
      CASE "H"
         cTypeName := "hash"
         EXIT
      CASE "P"
         cTypeName := "pointer"
         EXIT
      ENDSWITCH
   ENDIF

   IF cTypeName != NIL
      RETURN {| u | iif( Type( u ) == "UE", ;
                         ( __dbgAlert( "Expression error" ), .F. ), ;
                         Type( u ) == cType .OR. ;
                         ( __dbgAlert( "Must be " + cTypeName ), .F. ) ) }
   ENDIF

   RETURN {| u | ! Type( u ) == "UE" .OR. ( __dbgAlert( "Expression error" ), .F. ) }


FUNCTION __dbgInput( nRow, nCol, nWidth, cValue, bValid, cColor, nSize )

   LOCAL lOK := .F.
   LOCAL nKey
   LOCAL oGet

   IF ! HB_ISNUMERIC( nWidth )
      nWidth := Len( cValue )
   ENDIF
   oGet := HbDbInput():new( nRow, nCol, nWidth, cValue, cColor, nSize )

   oGet:display()

   DO WHILE .T.
      oGet:showCursor()
      nKey := __dbgInkey()
      DO CASE
      CASE nKey == K_ESC
         EXIT
      CASE nKey == K_ENTER
         IF bValid == NIL .OR. Eval( bValid, oGet:getValue() )
            cValue := oGet:getValue()
            lOK := .T.
            EXIT
         ENDIF
      OTHERWISE
         oGet:applyKey( nKey )
      ENDCASE
   ENDDO

   SetCursor( SC_NONE )

   RETURN lOK


FUNCTION __dbgAChoice( nTop, nLeft, nBottom, nRight, aItems, cColors )

   LOCAL oBrw
   LOCAL oCol
   LOCAL nRow
   LOCAL nLen

   oBrw := HBDbBrowser():New( nTop, nLeft, nBottom, nRight )
   oBrw:colorSpec := cColors
   nLen := nRight - nLeft + 1
   nRow := 1
   oCol := TBColumnNew( "", {|| PadR( aItems[ nRow ], nLen ) } )
   oBrw:AddColumn( oCol )
   oBrw:goTopBlock := {|| nRow := 1 }
   oBrw:goBottomBlock := {|| nRow := Len( aItems ) }
   oBrw:skipBlock := {| n | n := iif( n < 0, Max( n, 1 - nRow ), ;
      Min( Len( aItems ) - nRow, n ) ), ;
      nRow += n, n }
   DO WHILE .T.
      oBrw:forceStable()
      SWITCH __dbgInkey()
         CASE K_UP;     oBrw:up();        EXIT
         CASE K_DOWN;   oBrw:down();      EXIT
         CASE K_PGUP;   oBrw:pageUp();    EXIT
         CASE K_PGDN;   oBrw:pageDown();  EXIT
         CASE K_HOME;   oBrw:goTop();     EXIT
         CASE K_END;    oBrw:goBottom();  EXIT
         CASE K_ESC;    nRow := 0
         CASE K_ENTER;  RETURN nRow
      ENDSWITCH
   ENDDO

   RETURN 0


FUNCTION __dbgAlert( cMessage )
   RETURN hb_gtAlert( cMessage, { "Ok" }, "W+/R", "W+/B" )


FUNCTION __dbgInkey()

   LOCAL nKey
   LOCAL lDebug, lCancel

   lDebug := Set( _SET_DEBUG, .F. )
   lCancel := Set( _SET_CANCEL, .F. )
   nKey := Inkey( 0, INKEY_ALL )
   Set( _SET_CANCEL, lCancel )
   Set( _SET_DEBUG, lDebug )

   RETURN nKey


FUNCTION __dbgSaveScreen( ... )

   LOCAL lAppCompatBuffer := hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   LOCAL cScreen := SaveScreen( ... )

   hb_gtInfo( HB_GTI_COMPATBUFFER, lAppCompatBuffer )

   RETURN cScreen


FUNCTION __dbgRestScreen( ... )

   LOCAL lAppCompatBuffer := hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )

   RestScreen( ... )
   hb_gtInfo( HB_GTI_COMPATBUFFER, lAppCompatBuffer )

   RETURN NIL


FUNCTION __dbgTextToArray( cString )
   RETURN hb_ATokens( cString, .T. )

FUNCTION __dbgValToStr( uVal )

   SWITCH ValType( uVal )
#ifdef HB_CLP_STRICT
   CASE "C"
   CASE "M" ; RETURN '"' + uVal + '"'
   CASE "D" ; RETURN DToC( uVal )
   CASE "T" ; RETURN hb_TToC( uVal )
   CASE "O" ; RETURN "{ ... }"
#else
   CASE "C"
   CASE "M" ; RETURN hb_StrToExp( uVal )
   CASE "D" ; RETURN Left( hb_TSToStr( uVal, .F. ), 10 )
   CASE "T" ; RETURN hb_TSToStr( uVal, .T. )
   CASE "O" ; RETURN "Class " + uVal:ClassName() + " object"
#endif
   CASE "N" ; RETURN AllTrim( Str( uVal ) )
   CASE "L" ; RETURN iif( uVal, ".T.", ".F." )
   CASE "S" ; RETURN "@" + uVal:name + "()"
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ ... }"
   CASE "H" ; RETURN "{ => }"
   CASE "P" ; RETURN "<pointer>"
   OTHERWISE
      IF uVal == NIL
         RETURN "NIL"
      ENDIF
   ENDSWITCH

   RETURN "U"


FUNCTION __dbgValToExp( uVal )

   SWITCH ValType( uVal )
#ifdef HB_CLP_STRICT
   CASE "C"
   CASE "M" ; RETURN '"' + uVal + '"'
   CASE "D" ; RETURN 'CToD("' + DToC( uVal ) + '")'
   CASE "T" ; RETURN 'hb_CToT("' + hb_TToC( uVal ) + '")'
   CASE "O" ; RETURN "Object"
#else
   CASE "C"
   CASE "M" ; RETURN hb_StrToExp( uVal )
   CASE "D" ; RETURN 'CToD("' + DToC( uVal ) + '")' // 'd"' + Left( hb_TSToStr( uVal, .F. ), 10 ) + '"'
   CASE "T" ; RETURN 't"' + hb_TSToStr( uVal, .T. ) + '"'
   CASE "O" ; RETURN "{ " + uVal:className() + " Object }"
#endif
   CASE "N" ; RETURN hb_ntos( uVal )
   CASE "L" ; RETURN iif( uVal, ".T.", ".F." )
   CASE "S" ; RETURN "@" + uVal:name + "()"
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ ... }"
   CASE "H" ; RETURN "{ => }"
   CASE "P" ; RETURN "<pointer>"
   OTHERWISE
      IF uVal == NIL
         RETURN "NIL"
      ENDIF
   ENDSWITCH

   RETURN "U"

STATIC FUNCTION __dbgFlushScreenBuffer()
   LOCAL nInitCount := DispCount()
   Local nCount
   FOR nCount := 1 TO nInitCount
      DispEnd()
   NEXT
RETURN nInitCount 

STATIC PROCEDURE HiLiteRow( oBrowse )

   LOCAL nLen := oBrowse:ColCount
   LOCAL nPos

   oBrowse:refreshCurrent():forceStable()
   FOR nPos := 1 To nLen
      oBrowse:colPos := nPos
      oBrowse:dehilite():hilite()
   NEXT
   oBrowse:colPos := 1

RETURN

