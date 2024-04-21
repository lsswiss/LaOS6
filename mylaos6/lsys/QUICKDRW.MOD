DECLARE SUB clf ()
DECLARE FUNCTION question% (Titel$, a$, B$)
DECLARE SUB Coordinates ()
DECLARE SUB FileCopy (Text$, Source$, Dest$)
DECLARE SUB ProgressBar (Stat AS INTEGER)
DECLARE SUB FontShow (x!, y!, Text$, ein%)
DECLARE SUB Schriftformat ()
DECLARE SUB Textausgabe ()
DECLARE SUB GetMouseVal ()
DECLARE FUNCTION AngewaehltEtwas% ()
DECLARE SUB WaitOnPosition (modus!)
DECLARE SUB SetDrawMode (modus AS INTEGER)
DECLARE SUB GetToolEvents ()
DECLARE SUB DrawToolBar ()
DECLARE SUB SCircle (x!, y!, r!, asp!)
DECLARE SUB SLine (x1!, y1!, x2!, y2!)
DECLARE SUB StaticDraw (Figur AS SINGLE)
'lavers. QuickDraw basiert auf LaOS 3D-Studio 3 und integriert
'die Maus inkl. Symbolleiste und visuelles Zeichnen.
'Sie ist fÅr AnfÑnger gemacht
'LastChange: 30.07.1999

'15.7.1999: mit LaOS FontScript 1.1 (lff-Dateien): Speichereinheit 16 Bit
'30.7.1999: an gewissen Stellen PCopy eingefÅhrt (fÅr schnellerer Bildaufbau)
'14.8.1999: Neue Menuleiste eingefÅhrt. (Wie aus lavers. Animate)

DECLARE SUB Paintic (a!, B!, f!)
DECLARE FUNCTION GetCombo$ (Index!)
DECLARE SUB SetCombo (Index!, Wert$)
DECLARE SUB ERASECombo ()
DECLARE SUB mausxbereich (x1, x2)
DECLARE SUB MausTempo (speed%)
DECLARE SUB warteknopflos ()
DECLARE SUB maus (OnOff%)
DECLARE SUB mausinit ()
DECLARE SUB GetMaus ()
DECLARE SUB wartebewegung ()
DECLARE SUB ReadData ()
DECLARE SUB mausset (x, y)
DECLARE FUNCTION Interr% (Num%, AX%, bx%, cx%, dx%)
DECLARE SUB MausForm (SoftHard%, BMaske%, CMaske%)
DECLARE SUB SetAttr (aDatei$, Attr$)
DECLARE FUNCTION LaOSFileName$ (Dosfile$)
DECLARE SUB Spiegeln (d AS INTEGER)
DECLARE SUB RadioButton (z!, s!, akt!)
DECLARE SUB Drehen (Grad AS INTEGER)
DECLARE SUB Setze (Satz!, Feld!, Wert AS SINGLE)
DECLARE FUNCTION Befehle! (Satz!, Feld!)
DEFSNG A-Z
DECLARE SUB DelTemp ()
'LaOS-Draw-Script-Zeichnungstool: 3D-Studio
'Version mit Drehen-Funktion
'Version mit Festplattenspeicherauslagerung
'Version mit Spiegeln-Funktion
'MadeDate: Oberglatt, im April 1999

DECLARE SUB Dreh (x!, y!, DpX!, DpY!)
DECLARE SUB Regedit (Eintrag$)
DECLARE FUNCTION RegItem$ (Nenner$)
DECLARE SUB Symbolmake (Art!)
DECLARE FUNCTION FrameWindow$ (a!, B!, c!, d!, del!)
DECLARE FUNCTION GetFile$ (Sym$, Art1$, Link1$)
DECLARE FUNCTION Dosfile$ (Kennung$, InOrdner$)
DECLARE FUNCTION LText$ (Text$, ab!)
DECLARE FUNCTION exist! (Datei$)
DECLARE FUNCTION IsExist! (Datei$, Ordner$)
DECLARE FUNCTION FileChoose$ (Art1$)
DECLARE FUNCTION Linkit$ ()
DECLARE SUB Befset (fileNr!, i!, Nr!, Wert!)
DECLARE FUNCTION Befcopy! (fileNr!, i!, Nr!)
DECLARE SUB BASIC3D ()
DECLARE FUNCTION NextOne! (rely.on!)
DECLARE SUB Markieren (px!, py!, x!, y!)
DECLARE SUB Showactiv (i!)
DECLARE SUB NextActiv (Retour AS INTEGER)
DECLARE SUB ShowAlls ()
DECLARE FUNCTION ColorChoice! ()
DECLARE SUB MShows ()
DECLARE SUB Zeichnen ()
DECLARE SUB Cursor (x!, y!)
DECLARE SUB Echo (Text$)
DECLARE FUNCTION ComboBox$ (Nr!, z!, s!, zu!, su!)
DECLARE FUNCTION Trim$ (Variable$, Trennzeichen$, a!)
DECLARE FUNCTION MenuInt$ (Item$, Trennzeichen$)
DECLARE SUB Dialog (Caption$)
DECLARE SUB MenuBox (Zeilen!, Spalten!, Caption$)
DECLARE SUB Menuact (Menu$)
DECLARE FUNCTION SDate$ ()
DECLARE SUB TimeLock (Status!)
DECLARE SUB StoppUhrBeep (Uhr!)
DECLARE SUB Symbact (Symbol$)
DECLARE SUB Clearase (Objekt$)
DECLARE SUB Register (z!, s!, zu!, su!, Text$)
DECLARE SUB Frame ()
DECLARE SUB GetInp ()
DECLARE FUNCTION GetKey$ ()
DECLARE SUB CLW ()
DECLARE SUB ClearControls (Connect!)
DECLARE SUB NextControl ()
DECLARE SUB LastControl ()
DECLARE SUB Activate (Control$)
DECLARE FUNCTION Control$ (Zeilen!, Spalten!, Text$)
DECLARE SUB Delete (Objekt$)
DECLARE FUNCTION InputBox$ (Zeilen!, Spalten!, Text$, Break!, MaxDown!)
DECLARE SUB ExitBox (x!, y!)
DECLARE FUNCTION TextBox$ (Zeilen!, Spalten!, Text$, Break!)
ON ERROR GOTO fehlernext
9
'LaOS
SCREEN 9, , 0, 0
VIEW PRINT 1 TO 25

DIM SHARED MS%(45) 'Variablen fÅr die Maus
DIM SHARED mausx AS INTEGER, mausy AS INTEGER, mausk AS INTEGER, taste$, fal%, knopf%

DIM SHARED Stoppuhr(1 TO 4) '1 ist Reserviert!
DIM SHARED AlteZeit(1 TO 4)
DIM kbd AS STRING
DIM SHARED TextScale
DIM SHARED TextBreak
DIM SHARED Activ AS STRING
DIM SHARED ControlNr AS INTEGER
DIM SHARED ControlList(3) AS STRING
DIM SHARED Break AS INTEGER
DIM SHARED EBox.left, EBox.top AS INTEGER
DIM SHARED Left, Top, Right, Down AS INTEGER
DIM SHARED ControlConnect AS INTEGER
DIM SHARED MenuList(5) AS STRING, MenuNr
DIM SHARED Focus AS STRING, DriveName AS STRING
DIM SHARED keyb AS STRING
DIM SHARED Change(2) AS INTEGER
DIM SHARED FileBox AS STRING
DIM SHARED errorlevel AS INTEGER
DIM SHARED choice AS INTEGER, ActivItem AS STRING

FesteEinstellungen:
WIDTH 80, 25
SHELL "c:": CHDIR "\laos"

Einstellungen:
3
CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
GET 1, 1, Schema$
CLOSE 1: OPEN "c:\laos\" + Schema$ + ".scl" FOR INPUT AS 1: LINE INPUT #1, a$
DIM m(1 TO 15)
FOR i = 1 TO 15
INPUT #1, m(i)
NEXT
8
DIM SHARED Fensterfarbe AS INTEGER, Randfarbe AS INTEGER, Schattenfarbe AS INTEGER, Controlfarbe AS INTEGER
DIM SHARED Symbolactiv AS INTEGER, SymbactStil AS INTEGER, SymbolFrame AS INTEGER, SymbolColor AS INTEGER
DIM SHARED Hintergrundfarbe  AS INTEGER, Farbe AS INTEGER, IconVerticalSpace AS INTEGER
DIM SHARED InfoColor  AS INTEGER, InfoFrame, Datumsformat AS INTEGER

Fensterfarbe = m(1)'Fensterfarben
Randfarbe = m(2)
Schattenfarbe = m(3)
Controlfarbe = m(4)  'SchaltflÑchenfarbe
Symbolactiv = m(5) 'Umrahmungsfarbe des aktiven Symbols
SymbactStil = m(6) 'Umrahmungs-Stil des aktivierten Symbols (-1=voll)

SymbolFrame = m(7) 'Farbe des Symbolrahmens
SymbolColor = m(8) 'Farbe des Symbolhintergrundes

Hintergrundfarbe = m(9) 'Bildschirmfarben
Farbe = m(10) '= Vordergrundfarbe

IconVerticalSpace = m(11) 'Setzt den Vertikalen Standard-Abstand der Symbole
                            'Auf Null gesetzt ist der Abstand genÅgend.
InfoColor = m(13) 'Setzt die Farbe, in der der Quick-Info-Text erscheint.
InfoFrame = m(14) 'Setzt die Farbe des Quick-Info-Rahmens.

Datumsformat = m(15)

ERASE m

CONST Fe1 = 6, Fe2 = 4, Fe3 = 75, Fe4 = 20 'Startfensterkoordinaten
Stoppuhr(2) = 60 '2.Stoppuhr: Bildschirmschoner

CONST Temp = "c:\laos"

ReadData 'Mauszeiger einlesen
mausinit 'Maus initialisieren
maus 0 'ausschalten des Mauszeigers


VorStart:
COLOR Farbe, Hintergrundfarbe

Code = 0 'Legt die Ereignisverfolgung fÅr die Alt-Taste fest
FOR i = 15 TO 19
KEY i, CHR$(Code) + CHR$(56)
ON KEY(i) GOSUB AltKey
KEY(i) ON
Code = Code + 32
NEXT i
Code = 0


Programm:
DIM SHARED NoMouseReg
NoMouseReg = 0
CONST cmbFile = 3
ERASECombo

CONST an = 254, aus = 0, ab = 0
DIM SHARED cross, charwind, Menucase, Painticon, Mausspeed AS INTEGER, VisualDraw AS INTEGER, Changed AS INTEGER
charwind = VAL(RegItem("3DStudio.Koordinatenfenster"))
cross = VAL(RegItem("3DStudio.3DKreuz"))
Menucase = VAL(RegItem("3DStudio.Versteckte Menus"))
Painticon = VAL(RegItem("3DStudio.FÅllungen andeuten"))
Mausspeed = VAL(RegItem("QuickDraw.Mousespeed"))
VisualDraw = VAL(RegItem("QuickDraw.VisualDraw"))

Changed = 0
VIEW
WINDOW

DIM SHARED crs(100)
DIM SHARED Punkt(10), ToolBarMode AS INTEGER
CLS
x = 319: y = 174

LINE (x, y)-(x + 10, y + 5), InfoColor: GET (x, y)-(x, y), Punkt
LINE (x + 10, y + 5)-(x + 1, y + 10), InfoColor
LINE (x, y + 10)-(x, y), InfoColor
PAINT (x + 2, y + 2), InfoColor

GET (x, y)-(x + 10, y + 10), crs
PUT (x, y), crs

DIM SHARED xplus AS INTEGER, yplus AS INTEGER
DIM SHARED Grad AS INTEGER

DIM anfuegen AS INTEGER

CLW
CONST PI = 3.141592654#
col = Farbe

4
DIM SHARED FontShadow AS SINGLE, FontBold AS INTEGER, Size AS SINGLE, FontSpace AS INTEGER
FontShadow = 0
FontBold = 1
Size = 1
FontSpace = 4
IF NOT exist("c:\laos\newfont.lff") THEN ERROR 281
CLOSE

xplus = 319
yplus = 174
c = 1
mf = 1

COLOR Farbe

5
IF Linkit <> "" THEN
DelTemp: a$ = Linkit
IF a$ = "/newlds" THEN
    a$ = ""
    Changed = -1: CLOSE
    IF question("Neu beginnen", "Wollen Sie wirklich alle énderungen", "zurÅcksetzen?") THEN KILL "c:\LAOS\3dStudio.DFL": OPEN "c:\LAOS\3dStudio.DFL" FOR BINARY AS 2: GOSUB onlink ELSE OPEN "c:\LAOS\3dStudio.DFL" FOR BINARY AS 2: GOSUB onlink
ELSE
    nam$ = LaOSFileName(a$): GOSUB onlink
END IF

PUT (x, y), crs: GOTO 0
END IF

'TemporÑrdatei wird nicht gelîscht; nach dem DFL-Prinzip - nur geladen:
OPEN "c:\LAOS\3dStudio.DFL" FOR BINARY AS 2

Befehl = 0

'Zum Datei-Ende gehen:
DO UNTIL Befehle(Befehl + 1, 0) = 0
    Befehl = Befehl + 1
LOOP

Zeichnen
0
MenuBox 0, 0, "Menu"
MenuBox 0, 0, "Zeichnung"
MenuBox 0, 0, "Extras"
MenuBox 0, 0, "Einstellungen"
Frame

MausTempo Mausspeed

CONST mks = 20

ToolBarMode = 0


1 IF rsv = 0 AND tb = 1 THEN LINE (17, 195)-(29, 189), 0, BF
COLOR col: LOCATE 2, 42: PRINT "€€€"; : COLOR Farbe
WHILE INKEY$ <> "": WEND

warteknopflos

mausx = x
mausy = y
mausset x, y

IF charwind = an THEN Coordinates

IF px = 0 THEN px = 1
IF rsv = 7 THEN Markieren px, py, x, y
Cursor x, y: a$ = "":
StaticDraw rsv


DO WHILE a$ = ""
a$ = INKEY$

GetMaus

IF mausy <> y OR mausx <> x THEN
        ux = mausx
        uy = mausy
        a$ = CHR$(0)
END IF

IF mausk = 1 THEN a$ = CHR$(13)
IF mausk = 2 THEN a$ = CHR$(27)

LOOP


IF a$ = CHR$(13) AND rsv = 2 THEN
        IF x = xm AND y = ym THEN xm = 0: ym = 0: rsv = 0
END IF

IF a$ = CHR$(13) THEN

IF AngewaehltEtwas THEN posIt = 0

IF posIt = 0 THEN
GetToolEvents
IF ToolBarMode > 0 THEN
        Cursor x, y
        posIt = 1
        rsv = 0: arsv = 0
        IF ToolBarMode = 7 THEN a$ = "s"
END IF
ELSEIF posIt = 1 THEN

SELECT CASE ToolBarMode
CASE 1: a$ = "l"
CASE 2: a$ = "r"
CASE 3: a$ = "b"
CASE 4: a$ = "k"
CASE 5: a$ = "L"
CASE 6: a$ = "p"
CASE 7: a$ = CHR$(13): rsv = 10
CASE 8: a$ = " "
END SELECT

posIt = 0
IF ToolBarMode > 3 THEN ToolBarMode = 0
DrawToolBar
444
END IF

END IF

StaticDraw rsv

IF a$ = "4" OR a$ = "6" OR a$ = "2" OR a$ = "8" THEN c$ = a$
Cursor x, y
IF rsv = 3 THEN Cursor px + (px - x), py + (py - y)
IF rsv = 4 THEN PSET (x, y), col: Befehl = Befehl + 1: Setze Befehl, 0, ASC("p"): Setze Befehl, 1, x - xplus: Setze Befehl, 2, y - yplus: Setze Befehl, 3, col
IF a$ <> "s" AND a$ <> "f" THEN cu = 0

IF rsv = 9 THEN
        LINE (px, py)-(x, y), col
        Befehl = Befehl + 1: Setze Befehl, 0, ASC("l"): Setze Befehl, 1, px - xplus: Setze Befehl, 2, py - yplus: Setze Befehl, 3, x - xplus: Setze Befehl, 4, y - yplus: Setze Befehl, 5, col
        px = x: py = y
END IF

IF px = 0 THEN px = 1
IF rsv = 7 THEN Markieren px, py, x, y

nm:
xr = x: yr = y
SELECT CASE a$
CASE CHR$(0): x = ux: y = uy
CASE CHR$(127): GOSUB RG: Cursor x, y'CTR+BackSpace=RÅckgÑngig
CASE CHR$(27): rsv = 0: arsv = 0
CASE "p": rsv = 9: px = x: py = y
CASE "4": x = x - mf
CASE "6": x = x + mf
CASE "2": y = y + mf
CASE "8": y = y - mf
CASE "9": x = x + mf: y = y - mf * .73
CASE "1": x = x - mf: y = y + mf * .73
CASE "+": IF m.1 = 0 THEN mf = mf + 1 ELSE NextActiv (0): CLW: Zeichnen: MShows
CASE "-": IF m.1 = 0 THEN mf = mf - 1 ELSE NextActiv (1): CLW: Zeichnen: MShows
CASE "7": IF mf = 1 THEN mf = 10 ELSE mf = 1
CASE CHR$(9): Cursor x, y: GOSUB AltKey: Cursor x, y
CASE CHR$(0) + "M": xplus = xplus - 10: CLW: Zeichnen: MShows
CASE CHR$(0) + "K": xplus = xplus + 10: CLW: Zeichnen: MShows
CASE CHR$(0) + "H": yplus = yplus + 10: CLW: Zeichnen: MShows
CASE CHR$(0) + "P": yplus = yplus - 10: CLW: Zeichnen: MShows
CASE CHR$(0) + "G": xplus = 319: yplus = 174: CLW: Zeichnen: MShows'Home
CASE CHR$(0) + "O" 'End
c = 0: a = 0: B = 0
FOR i = 1 TO 2500
IF Befehle(i, 0) = 0 THEN EXIT FOR
IF Befehle(i, 1) > a THEN a = Befehle(i, 1)
IF Befehle(i, 2) > B THEN B = Befehle(i, 2)
NEXT
xplus = a * -1 + 630
yplus = B * -1 + 340
CLW
Zeichnen
MShows
CASE CHR$(3): col = ColorChoice: CLW: Zeichnen: MShows
CASE CHR$(17): CHAIN "c:\laos\run.bas"
CASE " ": arsv = 0: IF m.1 = 0 THEN rsv = 7: px = x: py = y ELSE Markieren m.1, m.2, m.3, m.4: ShowAlls: m.1 = 0: m.2 = 0: m.3 = 0: m.4 = 0
CASE "z": CLW: Zeichnen: MShows
CASE "5": rsv = 0: a$ = c$: PSET (x, y), col: Befehl = Befehl + 1: Setze Befehl, 0, ASC("p"): Setze Befehl, 1, x - xplus: Setze Befehl, 2, y - yplus: Setze Befehl, 3, col: GOTO nm
CASE "r": box = 0: rsv = 1: px = x: py = y: PSET (x, y), Farbe
CASE "b": box = 1: rsv = 1: px = x: py = y: PSET (x, y), Farbe
CASE "L": rsv = 2: px = x: py = y: PSET (x, y), Farbe
CASE "k": rsv = 3: px = x: py = y: Cursor x, y
CASE "m"
rsv = 0
Befehl = Befehl + 1: Setze Befehl, 0, ASC("m"): Setze Befehl, 1, x - xplus: Setze Befehl, 2, y - yplus: Setze Befehl, 3, col: Setze Befehl, 4, col
IF Painticon = 0 THEN
        IF cross = an THEN cross = aus: CLW: Zeichnen: cross = an
        PAINT (x, y), col
        MShows
    ELSE Paintic x, y, col
END IF

IF cross = an THEN LINE (xplus, 0)-(xplus, 349): LINE (0, yplus)-(639, yplus)

CASE "P": rsv = 4
CASE "l": rsv = 5: px = x: py = y: PSET (x, y), Farbe
CASE "f": col = col + 1: WHILE col > 15: col = 0: WEND
CASE "t", "T": Textausgabe
CASE CHR$(20): Schriftformat 'Ctrl + T
CASE "s": rsv = 10

CASE CHR$(8): rsv = 0: mf = 1

CASE CHR$(13)
Changed = -1
SELECT CASE rsv
CASE 10
RANDOMIZE TIMER
DO
cu = cu + 2
FOR i = 1 TO cu / 2
s1 = INT(RND * 200) + 1
s1 = s1 * (PI / 100)
x. = cu * SIN(s1)
y. = cu * COS(s1)
PSET (x + x., y + y.), col
Befehl = Befehl + 1: Setze Befehl, 0, ASC("p"): Setze Befehl, 1, x - xplus + x.: Setze Befehl, 2, y - yplus + y.: Setze Befehl, 3, col
NEXT
GetMaus
IF mausy <> y OR mausx <> x THEN x = mausx: y = mausy: cu = 0

LOOP WHILE mausk = 1

CASE 9: rsv = 0
CASE 0
IF m.1 = 0 THEN rsv = 7: px = x: a$ = " ": py = y ELSE Markieren m.1, m.2, m.3, m.4: ShowAlls: m.1 = 0: m.2 = 0: m.3 = 0: m.4 = 0
IF m.1 = 0 AND arsv > 0 THEN PSET (x, y), Farbe

CASE 1
IF box = 0 THEN IF xm <> x OR ym <> y THEN LINE (px, py)-(x, y), col, B: Befehl = Befehl + 1: Setze Befehl, 0, ASC("r"): Setze Befehl, 1, px - xplus: Setze Befehl, 2, py - yplus: Setze Befehl, 3, x - xplus: Setze Befehl, 4, y - yplus: Setze Befehl, 5, col
IF box = 1 THEN IF xm <> x OR ym <> y THEN LINE (px, py)-(x, y), col, BF: Befehl = Befehl + 1: Setze Befehl, 0, ASC("b"): Setze Befehl, 1, px - xplus: Setze Befehl, 2, py - yplus: Setze Befehl, 3, x - xplus: Setze Befehl, 4, y - yplus: Setze Befehl, 5, col

CASE 2: IF xm <> x OR ym <> y THEN LINE (px, py)-(x, y), col: Befehl = Befehl + 1: Setze Befehl, 0, ASC("l"): Setze Befehl, 1, px - xplus: Setze Befehl, 2, py - yplus: Setze Befehl, 3, x - xplus: Setze Befehl, 4, y - yplus: Setze Befehl, 5, col: px = x: py = y
CASE 3: rx = px - x
IF rx < 0 THEN rx = rx * -1
ry = py - y
IF ry < 0 THEN ry = ry * -1
IF rx <> 0 THEN asp = ry / rx ELSE asp = 1
IF rx > ry THEN r = rx ELSE r = ry
CIRCLE (px, py), r, col, , , asp: Befehl = Befehl + 1: Setze Befehl, 0, ASC("k"): Setze Befehl, 1, px - xplus: Setze Befehl, 2, py - yplus: Setze Befehl, 3, r: Setze Befehl, 4, col: Setze Befehl, 5, asp
x = px: y = py: rsv = 0: Cursor x, y
CASE 4: rsv = 0
CASE 5: IF xm <> x OR ym <> y THEN LINE (px, py)-(x, y), col: Befehl = Befehl + 1: Setze Befehl, 0, ASC("l"): Setze Befehl, 1, px - xplus: Setze Befehl, 2, py - yplus: Setze Befehl, 3, x - xplus: Setze Befehl, 4, y - yplus: Setze Befehl, 5, col: px = x: py = y
CASE 7
Markieren px, py, x, y
m.1 = px: m.2 = py: m.3 = x: m.4 = y
NextActiv (3)
rsv = 0


END SELECT

IF rsv <> 10 AND rsv <> 2 THEN IF rsv = 0 OR rsv = 7 THEN rsv = arsv ELSE arsv = rsv: rsv = 0
xm = x: ym = y'Verwendet zum doppelklicken
END SELECT
IF x < 0 OR x > 639 OR y < 0 OR y > 349 THEN x = xr: y = yr
IF mf > 50 THEN mf = 50
IF mf < 1 THEN mf = 1
IF rsv = 3 THEN Cursor px + (px - x), py + (py - y): GOTO 1
GOTO 1



AltKey:
IF MenuList(1) <> "" THEN Activate CHR$(0): ControlConnect = 1: 'Activate MenuList(1): MenuNr = 1
FOR i = 15 TO 19: KEY(i) STOP: NEXT i
GOSUB MenuInputing

FOR i = 15 TO 19: KEY(i) ON: NEXT i
RETURN

fehlernext:
ON ERROR GOTO errabort
errorlevel = 1
SELECT CASE ERL
CASE 9: IF ERR = 76 THEN PRINT "This Programm need LaOS": SYSTEM ELSE RESUME 470
CASE 3: RESUME 47
47 CLS : PRINT "Das Bildschirmschema muss neu installiert werden.": a$ = INPUT$(1): RUN "c:\laos\start.bas"
CASE 4: Dialog "Fehler: Die Schrift konnte nicht geladen werden."
CASE ELSE
IF ERR = 7 THEN 470
IF ERR = 9 THEN Dialog "Zu grosse Zeichnung.": ON ERROR GOTO fehlernext: RESUME 1
IF ERR <> 5 AND ERR <> 11 AND ERL <> 28 THEN Dialog "Der Fehler" + STR$(ERR) + " ist aufgetreten."
END SELECT
ON ERROR GOTO fehlernext
RESUME NEXT

errabort:
RESUME 470
470
ON ERROR GOTO 471
SCREEN 0
COLOR 15, 0
WHILE INKEY$ <> "": WEND
PRINT "Zuwenig Speicher oder schwerwiegender Fehler. Programm beendet.": PRINT INPUT$(1)
RUN "c:\laos\run.bas"
471 SYSTEM

fehlernext2:
errorlevel = 1
RESUME NEXT

MenuInputing:
IF Menucase = an THEN Menucase = aus: MShows: Menucase = an
Cursor x, y
GetInp
IF keyb = "esc" THEN
Activate CHR$(0): Cursor x, y: RETURN
END IF

rsv = 0

IF Focus = "Menu" THEN
m1:
a$ = MenuInt("Neu,Bild speichern,Bild laden,-,Clipart einfÅgen,-,Beenden", ",")

SELECT CASE choice
CASE 0: Activate CHR$(0): Cursor x, y: RETURN
CASE 1
    Register 6, 20, 13, 60, "Neu beginnen"
    Echo "Wollen Sie wirklich alle énderungen"
    Echo "zurÅcksetzen?"
    ClearControls 0
    TextScale = TextScale + 1
    PRINT Control(0, 0, "Ja")
    PRINT Control(0, 0, "Nein")
    ExitBox -1, -1
    Activate "Nein"
    GetInp
    IF Focus = "Ja" THEN DelTemp: Befehl = 0
    x = 320: y = 175
    ClearControls 0
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN

CASE 5:
    Register 6, 20, 13, 60, "Clipart einfÅgen"
    ClearControls 0
    TextScale = TextScale + 2
    PRINT Control(0, 0, "Symbol")
    PRINT Control(0, 0, "Zeichnung")
    ExitBox -1, -1
    Activate "Zeichnung"
    GetInp
    anfuegen = 1
    ClearControls 0
    IF Focus = "Symbol" THEN GOTO IconLoading
    IF Focus = "Zeichnung" THEN GOTO laden

ClipartBack:
    anfuegen = 0
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN


CASE 7: CHAIN "c:\laos\run.bas"

CASE 2
a$ = GetFile("draw", "LDS-Zeichnung", "qd2000")

Speichern:
errorlevel = 0
IF keyb = "esc" THEN CLW: Zeichnen: MShows: Activate CHR$(0): Cursor x, y: RETURN

FileCopy "Datei speichern...", "c:\LAOS\3dStudio.DFL", a$

IF errorlevel THEN Dialog "Das Speichern konnte nicht erfolgreich abgeschlossen werden. Mag sein, dass der DatentrÑger voll ist. Speichern Sie das Dokument sicherheitshalber an   einem anderen Ort."

Changed = 0
Befehl = i - 1
AlterName$ = a$
TimeLock 0
CLW
Zeichnen
MShows
Activate CHR$(0): Cursor x, y: RETURN

CASE 3
IF NOT question("", "Aktion setzt Zeichnung zurÅck!", "Fortfahren ohne zu speichern?") THEN Cursor x, y: RETURN

TimeLock 1
xplus = 319: yplus = 174

laden:
a$ = FileChoose("LDS-Zeichnung")
onlink:
IF keyb = "esc" OR a$ = "" THEN CLW: Zeichnen: MShows: Activate CHR$(0): Cursor x, y: RETURN
errorlevel = 0
AlterName$ = a$

IF anfuegen = 0 THEN
    FileCopy "Laden...", a$, "c:\LAOS\3dStudio.DFL"

ELSE

CLOSE 1: OPEN a$ FOR BINARY AS 1
AlteZeit = TIMER
TimeLock 1

i = 0
DO
IF TIMER - AlteZeit > .5 THEN TimeLock 1: AlteZeit = TIMER
i = i + 1
Befehl = Befehl + 1
FOR Nr = 0 TO 5
    GET 1, , tmpa: Setze Befehl, Nr, tmpa
NEXT
LOOP WHILE Befehle(i, 0) <> 0
END IF

IF errorlevel = 1 THEN Dialog "Es ist ein Fehler beim Laden aufgetreten."
TimeLock 0
IF anfuegen = 1 THEN GOTO ClipartBack
CLW
Zeichnen
MShows
Activate CHR$(0): Cursor x, y: RETURN

END SELECT

END IF 'Focus=Menu

IF Focus = "Zeichnung" THEN
a$ = MenuInt("RÅckgÑngig,Wiederherstellen,-,Ansicht aktualisieren,Farbe wÑhlen,-,Vergrîssern,Verkleinern", ",")

SELECT CASE choice
CASE 0: Activate CHR$(0): Cursor x, y: RETURN
CASE 4: CLW: Zeichnen: MShows: Cursor x, y: RETURN
CASE 5: col = ColorChoice: CLW: Zeichnen: MShows: Cursor x, y: RETURN
CASE 7: Symbolmake 2: Cursor x, y: RETURN
CASE 8: Symbolmake 3: Cursor x, y: RETURN
CASE 1
RG:
IF Befehl <= 0 THEN Befehl = 1
DIM Letzter(5)
FOR i = 0 TO 5
Letzter(i) = Befehle(Befehl, i)
Setze Befehl, i, 0
NEXT

Befehl = Befehl - 1

IF Letzter(0) = ASC("x") THEN
l.1 = Letzter(1)
l.2 = Letzter(2)
    FOR i = 0 TO 5
        Letzter(i) = Befehle(Befehl, i)
        Setze Befehl, i, 0
    NEXT
Befehl = Befehl - 1
END IF
CLW
Zeichnen
Cursor x, y
MShows
RETURN

CASE 2
IF Letzter(0) <> 0 THEN

Befehl = Befehl + 1
FOR i = 0 TO 5
Setze Befehl, i, Letzter(i)
Letzter(i) = 0
NEXT

IF Befehle(Befehl, 0) = ASC("x") THEN
Befehl = Befehl + 1
Setze Befehl, 0, ASC("x")
Setze Befehl, 1, l.1
Setze Befehl, 2, l.2
l.1 = 0
l.2 = 0
END IF

END IF
CLW
Zeichnen
MShows
Cursor x, y
RETURN

END SELECT

END IF

IF Focus = "Extras" THEN
IF m.1 = 0 THEN
    a$ = MenuInt("Symbolgrîsse|Symbol speichern|Symbol laden|-|Basic-Code erstellen|-|Als Startbild", "|")

SELECT CASE choice
CASE 0: Activate CHR$(0): Cursor x, y: RETURN
CASE 1: IF question("", "Kann nicht rÅckgÑngig gemacht werden!", "Weiterfahren, ohne zu speichern?") THEN Symbolmake 1
        Activate CHR$(0): Cursor x, y: RETURN
CASE 2
22202
    Register 6, 20, 12, 60, "Bild als Symbol speichern unter:"
    TextScale = 9
    ExitBox -1, -1
    IF RIGHT$(AlterName$, 1) <> "." THEN AlterName$ = AlterName$ + "."
    IF LCASE$(LEFT$(AlterName$, 8)) = "c:\laos\" THEN AlterName$ = MID$(AlterName$, 9)
    a$ = InputBox(0, 0, LEFT$(AlterName$, INSTR(AlterName$, ".") - 1), 8, 1)
    IF keyb = "esc" THEN PCOPY 1, 0: clf: Cursor x, y: RETURN
    a$ = "c:\laos\" + a$ + ".lsb"
    IF exist(a$) THEN
        Dialog "Die Datei ist schon vorhanden. Enter - Åberschreiben, Escape - andere Datei"
        IF keyb = "esc" THEN 22202
    END IF

    GOTO Speichern


CASE 3
IF NOT question("", "Aktion setzt Zeichnung zurÅck!", "Fortfahren ohne zu speichern?") THEN Cursor x, y: RETURN

IconLoading:

TimeLock 1
CLOSE 1: SHELL "dir c:\laos\*.lsb /b /o:n >c:\laos\mirror.bnk"
xplus = 300: yplus = 155
OPEN "c:\laos\mirror.bnk" FOR INPUT AS 1

TimeLock 1
ERASECombo

Nr = 0
DO WHILE EOF(1) = 0
Nr = Nr + 1
LINE INPUT #1, a$
SetCombo Nr, a$
LOOP
TimeLock 0
IF GetCombo(1) = "" THEN Dialog "Es wurden keine Bilder gefunden.": GOTO m1
Register 6, 20, 12, 60, "Icon laden"
a$ = ComboBox(1, 0, 0, 0, 0)
SHELL "c:"
CHDIR "\LAOS"
GOTO onlink

CASE 5
22203
    COLOR Farbe
    Register 6, 20, 12, 60, "Basic Grafik-Code erstellen unter:"
    Echo "Geben Sie bitte die Datei ein:"
    TextScale = 9
    ExitBox -1, -1
    IF AlterName$ = "" OR INSTR(AlterName$, ".") = 0 THEN AlterName$ = "."
    a$ = InputBox(0, 0, LEFT$(AlterName$, INSTR(AlterName$, ".") - 1), 8, 1)
    IF keyb = "esc" THEN CLW: Zeichnen: Cursor x, y: MShows: RETURN

    IF exist("c:\laos\" + a$ + ".bas") THEN
        IF LCASE$(a$) = "run" OR LCASE$(a$) = "start" THEN Dialog "Reservierter Dateiname.": GOTO 22203
        Dialog "Die Datei mit dem angegebenen Namen ist schon vorhanden. DrÅcken Sie nach-  einander J und Enter, um die Datei zu Åberschreiben."
        IF UCASE$(keyb) <> "J" THEN GOTO 22203
    END IF

    CLOSE 1: OPEN "c:\laos\" + a$ + ".bas" FOR OUTPUT AS 1

    PRINT #1, "SCREEN 9"
    i = 0
    DO
        i = i + 1
        SELECT CASE CHR$(Befehle(i, 0))
            CASE "p": PRINT #1, "PSET (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")," + STR$(Befehle(i, 3))
            CASE "m": PRINT #1, "PAINT (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")," + STR$(CINT(Befehle(i, 3))) + "," + STR$(Befehle(i, 4))
            CASE "l": PRINT #1, "LINE (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")-(" + STR$(CINT(Befehle(i, 3) + xplus)) + "," + STR$(CINT(Befehle(i, 4) + yplus)) + ")," + STR$(Befehle(i, 5))
            CASE "r": PRINT #1, "LINE (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")-(" + STR$(CINT(Befehle(i, 3) + xplus)) + "," + STR$(CINT(Befehle(i, 4) + yplus)) + ")," + STR$(Befehle(i, 5)) + ", B"
            CASE "b": PRINT #1, "LINE (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")-(" + STR$(CINT(Befehle(i, 3) + xplus)) + "," + STR$(CINT(Befehle(i, 4) + yplus)) + ")," + STR$(Befehle(i, 5)) + ", BF"
            CASE "k": PRINT #1, "CIRCLE (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")," + STR$(CINT(Befehle(i, 3))) + "," + STR$(Befehle(i, 4)) + ", , ," + STR$(Befehle(i, 5))
            CASE "x": PRINT #1, "CIRCLE (" + STR$(CINT(Befehle(i, 1) + xplus)) + "," + STR$(CINT(Befehle(i, 2) + yplus)) + ")," + STR$(CINT(Befehle(i, 3))) + "," + STR$(Befehle(i, 4)) + "," + STR$(Befehle(i + 1, 1)) + "," + STR$(Befehle(i + 1, 2)) + "," + STR$(Befehle(i, 5))
                      i = i + 1
        END SELECT
    LOOP WHILE Befehle(i, 0) <> 0
    CLOSE 1
8001
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN

CASE 7
Dialog "Dieses Bild soll in Zukunft als Startbild beim Starten von LaOS angezeigt   werden... DrÅcken Sie 'j' und Enter, um zu bestÑtigen."
    IF LCASE$(keyb) <> "j" THEN 8001
    f% = FREEFILE
    OPEN "c:\laos\config.lcg" FOR RANDOM AS f%
    PUT f%, 4, xplus
    PUT f%, 5, yplus
    CLOSE f%
    a$ = "c:\laos\laostart.lds"
    GOTO Speichern

END SELECT


ELSE
    a$ = MenuInt("Lîschen|FÑrben|Verschieben|Duplizieren|Spiegeln", "|")

SELECT CASE LEFT$(a$, 4)
CASE "": Activate CHR$(0): Cursor x, y: RETURN
CASE "Spie"
    Register 6, 20, 12, 50, "Spiegeln:"
    ExitBox -1, -1
    TextScale = 9
    'DIM ar(1 TO 2) AS INTEGER
    r(1) = 1
    r(2) = 0
    DO
    RadioButton 8, 22, r(1): PRINT "Vertikal"
    RadioButton 10, 22, r(2): PRINT "Horizontal"

    LINE (308, 98)-(354, 142), 0, BF
    IF r(1) = 1 THEN LINE (332, 98)-(332, 142), InfoColor
    IF r(2) = 1 THEN LINE (308, 119)-(354, 119), InfoColor

    a$ = GetKey$
    IF a$ = "up" OR a$ = "down" THEN SWAP r(1), r(2) 'Achtung: Wird nur simuliert, weil nur zwei Buttons!
    IF a$ = CHR$(27) THEN GOTO EndSpieg

    LOOP UNTIL a$ = CHR$(13)

Spiegeln (r(1)) '0=Horizontal,1=Vertikal

EndSpieg:
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN

CASE "Lîsc"
CLOSE 1: OPEN Temp + "\ldrawcop.tmp" FOR RANDOM AS 1: CLOSE 1: KILL Temp + "\ldrawcop.tmp"
OPEN Temp + "\ldrawcop.tmp" FOR RANDOM AS 1
    'Lîschmarken erstellen
        DO
            a = NextOne(1)
            IF a > 0 THEN Setze a, 0, 16
            IF IsTue = 1 THEN Setze a + 1, 0, 16
        LOOP UNTIL a = 0

    i = 0: ic = 0
    DO
    i = i + 1
        IF Befehle(i, 0) <> 16 THEN
            ic = ic + 1
            FOR Nr = 0 TO 5: Befset 1, ic, Nr, Befehle(i, Nr): NEXT Nr
        END IF
    LOOP UNTIL Befehle(i, 0) = 0
    'zurÅckkopieren:
        FOR i = 1 TO 2000
            IF Befehle(i, 0) = 0 THEN EXIT FOR
            FOR Nr = 0 TO 5: Setze i, Nr, Befcopy(1, i, Nr): NEXT Nr
        NEXT i
    i = 0
    DO: i = i + 1: LOOP WHILE Befehle(i, 0) <> 0
    Befehl = i - 1: i = 0

    CLOSE 1
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN

CASE "FÑrb": c = ColorChoice
IF keyb <> CHR$(27) THEN
i = 0
DO
i = NextOne(1)
    SELECT CASE CHR$(Befehle(i, 0))
        CASE "p": Setze i, 3, c
        CASE "m": Setze i, 3, c
        CASE "l": Setze i, 5, c
        CASE "r": Setze i, 5, c
        CASE "b": Setze i, 5, c
        CASE "k": Setze i, 4, c
        CASE "x": Setze i, 4, c: i = i + 1
    END SELECT
LOOP UNTIL i = 0
END IF
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN


CASE "Dupl"
dup = 1
GOTO vers

CASE "Vers"
dup = 0
vers:
AlterModus% = Painticon
Painticon = an
px = m.1: py = m.2: x = m.3: y = m.4
xm = 0: ym = 0
NotGK = 1
PCOPY 0, 1

DO
az = TIMER
NewInkey = 1
a$ = ""
warteknopflos

mausx = x
mausy = y
mausset x, y

DO
a$ = INKEY$
WHILE INKEY$ <> "": WEND

GetMaus

IF mausy <> y OR mausx <> x THEN
        ux = mausx
        uy = mausy
        a$ = CHR$(0)
END IF
IF mausk = 1 THEN a$ = CHR$(13)
IF mausk = 2 THEN a$ = CHR$(27)

IF TIMER - az > .2 AND NewInkey = 1 OR a$ = "z" THEN
PCOPY 1, 0

DO
i = NextOne(1)
SELECT CASE CHR$(Befehle(i, 0))
CASE "p": PSET (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym), Befehle(i, 3)
CASE "m": IF Painticon = 0 THEN PAINT (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym), Befehle(i, 3), Befehle(i, 4) ELSE Paintic Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym, Befehle(i, 3)
CASE "l": LINE (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym)-(Befehle(i, 3) + xplus + xm, Befehle(i, 4) + yplus + ym), Befehle(i, 5)
CASE "r": LINE (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym)-(Befehle(i, 3) + xplus + xm, Befehle(i, 4) + yplus + ym), Befehle(i, 5), B
CASE "b": LINE (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym)-(Befehle(i, 3) + xplus + xm, Befehle(i, 4) + yplus + ym), Befehle(i, 5), BF
CASE "k": CIRCLE (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym), Befehle(i, 3), Befehle(i, 4), , , Befehle(i, 5)
CASE "x": CIRCLE (Befehle(i, 1) + xplus + xm, Befehle(i, 2) + yplus + ym), Befehle(i, 3), Befehle(i, 4), Befehle(i + 1, 1), Befehle(i + 1, 2), Befehle(i, 5)
          i = i + 1
END SELECT
LOOP UNTIL i = 0
Markieren px, py, x, y
NewInkey = 0
END IF

LOOP WHILE a$ = ""

Markieren px, py, x, y

SELECT CASE a$
CASE CHR$(0): px = ux - x + px: py = uy - y + py: xm = px - m.1: ym = py - m.2: x = ux: y = uy
CASE CHR$(0) + "H", "8": py = py - mf: y = y - mf: ym = ym - mf
CASE CHR$(0) + "P", "2": py = py + mf: y = y + mf: ym = ym + mf
CASE CHR$(0) + "K", "4": x = x - mf: px = px - mf: xm = xm - mf
CASE CHR$(0) + "M", "6": x = x + mf: px = px + mf: xm = xm + mf
CASE "7": IF mf = 1 THEN mf = 10 ELSE mf = 1
CASE CHR$(13): EXIT DO
CASE CHR$(27): GOTO escape
END SELECT
Markieren px, py, x, y
IF charwind = an THEN Coordinates
                      
LOOP

IF dup = 0 THEN
DO
    i = NextOne(1)

    SELECT CASE CHR$(Befehle(i, 0))
        CASE "p", "m", "k": Setze i, 1, Befehle(i, 1) + xm: Setze i, 2, Befehle(i, 2) + ym
        CASE "x": Setze i, 1, Befehle(i, 1) + xm: Setze i, 2, Befehle(i, 2) + ym
        CASE "r", "b", "l": Setze i, 1, Befehle(i, 1) + xm: Setze i, 2, Befehle(i, 2) + ym: Setze i, 3, Befehle(i, 3) + xm: Setze i, 4, Befehle(i, 4) + ym
    END SELECT
LOOP UNTIL i = 0

ELSEIF dup = 1 THEN
    s = 0: DO: s = s + 1: LOOP UNTIL Befehle(s, 0) = 0
    alts = s
        DO
            i = NextOne(1)
            IF i >= alts THEN s = s + 1: EXIT DO

                FOR Nr = 0 TO 5: Setze s, Nr, Befehle(i, Nr): NEXT

                SELECT CASE CHR$(Befehle(i, 0))
                    CASE "p", "m", "k": Setze s, 1, Befehle(i, 1) + xm: Setze s, 2, Befehle(i, 2) + ym
                    CASE "x": Setze s, 1, Befehle(i, 1) + xm: Setze s, 2, Befehle(i, 2) + ym: i = i + 1: s = s + 1: FOR Nr = 0 TO 2: Setze s, Nr, Befehle(i, Nr): NEXT
                    CASE "r", "b", "l": Setze s, 1, Befehle(i, 1) + xm: Setze s, 2, Befehle(i, 2) + ym: Setze s, 3, Befehle(i, 3) + xm: Setze s, 4, Befehle(i, 4) + ym
                END SELECT

            s = s + 1
        LOOP UNTIL i = 0
    Befehl = s - 2
    s = 0
END IF

escape:
    Painticon = AlterModus%
    NotGK = 0
    IF dup = 0 THEN m.1 = px: m.2 = py: m.3 = x: m.4 = y
    dup = 0
    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN

END SELECT


END IF

END IF



IF Focus = "Einstellungen" THEN
PCOPY 0, 1
a$ = MenuInt(CHR$(cross) + " 3D-Kreuz|" + CHR$(charwind) + " Koordinatenfenster|" + CHR$(Menucase) + " Versteckte Menus|  Schrift...|  Maus...|" + CHR$(Painticon) + " FÅllungen andeuten", "|")

SELECT CASE MID$(a$, 3, 4)
CASE "Maus": GetMouseVal
CASE "Schr": Schriftformat
CASE "Koor": IF charwind = an THEN charwind = aus ELSE charwind = an
Regedit "3DStudio.Koordinatenfenster=" + STR$(charwind)
CASE "3D-K": IF cross = an THEN cross = aus ELSE cross = an
Regedit "3DSTudio.3DKreuz=" + STR$(cross)
CASE "Vers": IF Menucase = an THEN Menucase = aus ELSE Menucase = an
Regedit "3DSTudio.Versteckte Menus=" + STR$(Menucase)
CASE "FÅll": IF Painticon = an THEN Painticon = aus ELSE Painticon = an
Regedit "3DSTudio.FÅllungen andeuten=" + STR$(Painticon)
END SELECT

    IF keyb = "esc" THEN PCOPY 1, 0: Cursor x, y: Activate CHR$(0): RETURN

    CLW
    Zeichnen
    MShows
    Cursor x, y
    RETURN

END IF


MS.Data:
DATA 55,8b,ec,56,57,8b,76,0c,8b,04,8b,76,0a,8b,1c,8b,76,08,8b,0c
DATA 8b,76,06,8b,14,cd,21,8b,76,0c,89,04,8b,76,0a,89,1c,8b,76,08,89,0c
DATA 8b,76,06,89,14,5f,5e,5d,ca,08,00,-

SUB Activate (Control$)
SELECT CASE ControlConnect
CASE 0 'Controls
i = 0
DO
i = i + 1
a$ = ControlList(i)
IF a$ = "" THEN EXIT SUB
ControlNr = i
Activ = Control$
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
p$ = Control(Zeilen, Spalten, Texta$)
LOOP UNTIL a$ = ""
CASE 1 'Menus
i = 0
Activ = Control$
DO
i = i + 1
a$ = MenuList(i)
IF a$ = "" THEN EXIT DO
Menuact a$
LOOP UNTIL a$ = ""

END SELECT
END SUB

FUNCTION AngewaehltEtwas%
AngewaehltEtwas% = 0
SHARED x, y

IF x > 5 AND x < 316 AND y > 11 AND y < 32 THEN AngewaehltEtwas% = 1
IF x > 327 AND x < 352 AND y > 13 AND y < 28 THEN AngewaehltEtwas% = 1
IF y > 326 AND y < 346 AND x > 3 AND x < 163 THEN AngewaehltEtwas% = 1

END FUNCTION

FUNCTION Befcopy (fileNr, i, Nr)
GET fileNr, 6 * i - 5 + Nr, a
Befcopy = a
END FUNCTION

FUNCTION Befehle (Satz, Feld) 'Liest aus dem temporÑren Speicher
IF Satz < 1 THEN EXIT FUNCTION

DIM gelesen AS SINGLE, Datensatz AS LONG

Datensatz = ((Satz - 1) * 6 + Feld + 1) * 4 - 3

GET 2, Datensatz, gelesen

Befehle = gelesen

END FUNCTION

SUB Befset (fileNr, i, Nr, Wert)
PUT fileNr, 6 * i - 5 + Nr, Wert
END SUB

SUB Clearase (Objekt$)
a = VAL(MID$(Objekt$, 1, 3))
B = VAL(MID$(Objekt$, 4, 3))
c = VAL(MID$(Objekt$, 7, 3))
d = VAL(MID$(Objekt$, 10, 3))


'Fenster:
LINE (a + 2, B + 2)-(c - 1, d - 1), 0, BF

Objekt$ = ""
END SUB

SUB ClearControls (Connect)

SELECT CASE Connect
CASE 0: ControlNr = 0: ERASE ControlList
CASE 1: MenuNr = 0: ERASE MenuList
CASE 3 'Alle lîschen
ControlNr = 0: ERASE ControlList
MenuNr = 0: ERASE MenuList
END SELECT

END SUB

'Clear Frame
SUB clf

TextScale = 2 '=Zeile
TextBreak = 2 '=Spalte
EBox.left = 603: EBox.top = 321
Left = 2: Top = 2: Right = 79: Down = 24

END SUB

SUB CLW
'Fenster:
LINE (1, 1)-(637, 347), 0, BF
LINE (0, 0)-(638, 348), Fensterfarbe, B
'Schatten:
LINE (1, 1)-(637, 1), Schattenfarbe
LINE (1, 1)-(1, 347), Schattenfarbe
'Rand:
LINE (639, 349)-(0, 349), Randfarbe
LINE (639, 349)-(639, 0), Randfarbe

TextScale = 2 '=Zeile
TextBreak = 2 '=Spalte
EBox.left = 603: EBox.top = 321
Left = 2: Top = 2: Right = 79: Down = 24
END SUB

FUNCTION ColorChoice
Register 6, 10, 12, 30, "Farbe wÑhlen:"
ExitBox -1, -1
SHARED col

act = col
DO
IF act < 0 THEN act = 15
IF act > 15 THEN act = 0
LOCATE Top, Left

FOR i = 0 TO 15
IF i = 5 THEN LOCATE Top + 1, Left
IF i = 10 THEN LOCATE Top + 2, Left
COLOR i
IF act <> i THEN PRINT "€€€";  ELSE PRINT "€" + CHR$(8) + "€";
NEXT

a$ = GetKey
IF a$ = CHR$(27) THEN ColorChoice = col: COLOR Farbe: EXIT FUNCTION
SELECT CASE a$
CASE "up", "left", "4", "8": act = act - 1
CASE "down", "right", "6", "2": act = act + 1
END SELECT
LOOP UNTIL a$ = CHR$(13)

ColorChoice = act
COLOR Farbe
END FUNCTION

FUNCTION ComboBox$ (Nr, z, s, zu, su)

IF GetCombo(1) = "" THEN SetCombo 1, " "
DIM Trenner(1 TO 5)


MaxItem% = 30000

IF z = 0 THEN z = Top
IF s = 0 THEN s = Left
IF zu = 0 THEN zu = Down
IF su = 0 THEN su = Right

MaxLenth = su - s + 1
MaxDown = zu - z + 1

DO
i = i + 1
IF GetCombo(i) = "-" THEN SetCombo i, STRING$(MaxLenth, 196): tr = tr + 1: Trenner(tr) = Standort + i
LOOP UNTIL GetCombo(i) = ""


a = s * 8 - 11
B = z * 14 - 17
c = su * 8 + 1
d = zu * 14 + 1


p$ = FrameWindow(a, B, c, d, 1)

ItemPos = 1
ItemPage = 1

IF ActivItem <> "" THEN act$ = ActivItem: zrl = 1: GOTO aktivieren
weiter2:

DO

 Standort = ItemPage * MaxDown - MaxDown + 1
LOCATE z

 FOR i = 0 TO MaxDown - 1
  IF Standort + i <= MaxItem% THEN
  IF LEN(GetCombo(Standort + i)) <= MaxLenth THEN
  Eintrag$ = GetCombo(Standort + i) + SPACE$(MaxLenth - LEN(GetCombo(Standort + i)))
  ELSE
  Eintrag$ = LEFT$(GetCombo(Standort + i), MaxLenth)
  END IF
  ELSE Eintrag$ = SPACE$(MaxLenth)
  END IF

   IF Standort + i = ItemPos THEN COLOR InfoColor ELSE COLOR Farbe
  
   LOCATE , s: PRINT Eintrag$
    IF i = 0 THEN LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), 0, B ELSE LINE (s * 8 - 9, (z + i) * 14 - 14)-(su * 8, (z + i) * 14), 0, B

 NEXT i

i = ItemPos - Standort
LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), InfoFrame, B

SELECT CASE GetKey
CASE "up"
ItemPos = ItemPos - 1

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos - 1
NEXT
IF ItemPos < Standort THEN ItemPage = ItemPage - 1


CASE "down"
IF ItemPos < MaxItem% THEN

IF GetCombo(ItemPos + 1) <> "" THEN

ItemPos = ItemPos + 1

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos + 1
NEXT


IF ItemPos > Standort + MaxDown - 1 THEN ItemPage = ItemPage + 1

END IF
END IF

CASE "home"
    ItemPos = 1
    ItemPage = 1
    IF Trenner(1) = 1 THEN ItemPos = ItemPos + 1

CASE "end"
    DO
        ItemPos = ItemPos + 1
        Standort = ItemPage * MaxDown - MaxDown + 1
        IF ItemPos > Standort + MaxDown THEN ItemPage = ItemPage + 1
    LOOP UNTIL GetCombo(ItemPos) = ""
        ItemPos = ItemPos - 1

    FOR i = 1 TO UBOUND(Trenner)
        IF Trenner(i) = ItemPos THEN ItemPos = ItemPos - 1
    NEXT


CASE "pageup"
IF ItemPos + MaxDown > 0 THEN
ItemPage = ItemPage - 1
ItemPos = ItemPos - MaxDown
END IF
CASE "pagedown"
IF ItemPos + MaxDown < MaxItem% + 1 THEN
IF GetCombo(ItemPos + MaxDown) <> "" THEN
ItemPage = ItemPage + 1
ItemPos = ItemPos + MaxDown
END IF
END IF
CASE CHR$(13): COLOR Farbe: choice = ItemPos: ComboBox = GetCombo(ItemPos): EXIT FUNCTION
CASE CHR$(27), CHR$(8): COLOR Farbe: choice = 0: ComboBox = "": keyb = "esc": EXIT FUNCTION
CASE CHR$(9)
LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), 0, B
keyb = "tab": choice = ItemPos: ComboBox = GetCombo(ItemPos): EXIT FUNCTION

CASE ELSE
IF LEN(keyb) = 1 THEN
act$ = keyb
aktivieren:

AltePosition = ItemPos
AlteSeite = ItemPage

DO
 ItemPos = ItemPos + 1
 IF ItemPos = AltePosition THEN ItemPage = AlteSeite: EXIT DO
 Standort = ItemPage * MaxDown - MaxDown + 1
 IF ItemPos > Standort + MaxDown - 1 THEN ItemPage = ItemPage + 1

   Eintrag$ = LEFT$(GetCombo(ItemPos), MaxLenth)
   IF Eintrag$ = "" OR ItemPos > MaxItem% - 1 THEN ItemPos = 0: ItemPage = 1

LOOP UNTIL LCASE$(LEFT$(Eintrag$, LEN(act$))) = LCASE$(act$)
GOTO weiter2

END IF

END SELECT

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos + 1
NEXT

IF ItemPos < 1 THEN ItemPos = 1: ItemPage = 1
IF GetCombo(ItemPos) = "" THEN ItemPos = ItemPos - 1
IF ItemPos < 1 THEN ItemPos = 1: ItemPage = 1

LOOP

END FUNCTION

FUNCTION Control$ (Zeilen, Spalten, Text$)

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak + 1



a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + LEN(Text$)
EndZeile = Zeilen + INT(LEN(Text$) / 80 + 1)
IF LEN(Text$) = 80 THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

LOCATE Zeilen, Spalten: COLOR Farbe: PRINT Text$
LINE (a, B)-(c, d), Controlfarbe, B

'Schatten: (Wird angezeigt, falls das Control aktiviert ist.
IF Activ = Text$ THEN
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
ELSE
LINE (a + 1, B + 1)-(c - 1, B + 1), 0
LINE (a + 1, B + 1)-(a + 1, d - 1), 0
END IF
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe
Zeilen$ = MID$(STR$(Zeilen), 2)
Spalten$ = MID$(STR$(Spalten), 2)
IF LEN(Zeilen$) < 2 THEN Zeilen$ = "0" + Zeilen$
IF LEN(Spalten$) < 2 THEN Spalten$ = "0" + Spalten$

'Control eintragen:
a$ = Zeilen$ + Spalten$ + Text$
i = 0
DO: i = i + 1
IF ControlList(i) = a$ THEN GOTO 2
LOOP UNTIL ControlList(i) = ""
ControlList(i) = a$
ControlNr = 1

2 Control$ = "": ControlConnect = 0: TextScale = Zeilen + 2
END FUNCTION

SUB Coordinates
SHARED x, y, mf
DIM a AS STRING * 5, B AS STRING * 5, c AS STRING * 3
a = STR$(CINT(x - xplus))
B = STR$(CINT(y - yplus))
c = STR$(mf)
a$ = TextBox(2, 55, "x: " + a + " y: " + B + " c: " + c, 0)
END SUB

SUB Cursor (x, y)
7 PUT (x, y), crs
END SUB

SUB Delete (Objekt$)
a = VAL(MID$(Objekt$, 1, 3))
B = VAL(MID$(Objekt$, 4, 3))
c = VAL(MID$(Objekt$, 7, 3))
d = VAL(MID$(Objekt$, 10, 3))


'Fenster:
LINE (a, B)-(c + 1, d + 1), 0, BF

Objekt$ = ""
END SUB

SUB DelTemp
'lîscht den TemporÑren Speicher

CLOSE 2
OPEN "c:\LAOS\3DSTUDIO.DFL" FOR BINARY AS 2
CLOSE 2: KILL "c:\LAOS\3DSTUDIO.DFL"         'lîschen des Tmp-Files

OPEN "c:\LAOS\3dStudio.DFL" FOR BINARY AS 2

'DFL: Document-Filing (Dokumentablagedatei)
Changed = 0

END SUB

SUB Dialog (Caption$)
SHARED NoGet
IF LEN(Caption$) < 22 THEN Caption$ = STRING$(INT((22 - LEN(Kennung$)) / 2), " ") + Caption$ + STRING$(CINT((22 - LEN(Kennung$)) / 2 + .1), " ")

IF LEN(Caption$) > 303 THEN Caption$ = LEFT$(Caption$, 300) + "..."
IF LEN(Caption$) > 76 THEN Length = 76 ELSE Length = LEN(Caption$)
Spalten = INT(40 - Length / 2 + 1)
Zeilen = 11 - INT(LEN(Caption$) / 76) + 1

x1! = Spalten * 8 - 11
y1! = Zeilen * 14 - 17
EndSpalten = 40 + INT(Length / 2)
EndZeilen = 14 + INT(LEN(Caption$) / 76)
IF INT(LEN(Caption$) / 76) > 1 THEN EndZeilen = EndZeilen - (INT(LEN(Caption$) / 76) - 1)
IF LEN(Caption$) < 76 THEN EndZeilen = EndZeilen + 1
x2! = EndSpalten * 8 + 4
y2! = EndZeilen * 14 - 4

IF NoGet = 0 THEN
Groesse = 4 + INT(((PMAP(x2!, 0) - PMAP(x1!, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2!, 1) - PMAP(y1!, 1) + 1)
DIM Background(Groesse) AS INTEGER
GET (x1!, y1!)-(x2!, y2!), Background
END IF

Objekt$ = FrameWindow(x1!, y1!, x2! - 1, y2! - 1, 0)
LINE (x1 + 1, y1 + 1)-(x2 - 1, y1 + 1), Schattenfarbe
LINE (x1 + 1, y1 + 1)-(x1 + 1, y2 - 1), Schattenfarbe
Clearase Objekt$


ecr = 1
FOR i = 1 TO INT(LEN(Caption$) / 76) + 1
LOCATE Zeilen, Spalten
PRINT MID$(Caption$, ecr, 76)
ecr = ecr + 76
Zeilen = Zeilen + 1
NEXT

PRINT TextBox(Zeilen + 1, 33, "      OK", 14)
DO
B$ = a$
a$ = GetKey
LOOP UNTIL a$ = CHR$(27) OR a$ = CHR$(13)
IF a$ = CHR$(27) THEN keyb = "esc" ELSE keyb = B$

IF NoGet = 0 THEN PUT (x1!, y1!), Background, PSET

x1! = 0: y1! = 0
END SUB

FUNCTION Dosfile$ (Kennung$, InOrdner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF Kennung$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN Dosfile = RTRIM$(Dateiname$): CLOSE 1: EXIT FUNCTION
LOOP UNTIL LText$(Dateiname$, 1) = ""
errorlevel = 1
CLOSE 1

END FUNCTION

SUB DrawToolBar

COLOR Farbe
LINE (3, 326)-(163, 346), 0, BF
SELECT CASE ToolBarMode
CASE 1: LINE (3, 326)-(23, 346), InfoColor, BF
CASE 2: LINE (23, 326)-(43, 346), InfoColor, BF
CASE 3: LINE (43, 326)-(63, 346), InfoColor, BF
CASE 4: LINE (63, 326)-(83, 346), InfoColor, BF
CASE 5: LINE (83, 326)-(103, 346), InfoColor, BF
CASE 6: LINE (103, 326)-(123, 346), InfoColor, BF
CASE 7: LINE (123, 326)-(143, 346), InfoColor, BF
CASE 8: LINE (143, 326)-(163, 346), InfoColor, BF
END SELECT

LINE (3, 326)-(23, 346), , B
LINE (23, 326)-(43, 346), , B
LINE (43, 326)-(63, 346), , B
LINE (63, 346)-(83, 326), , B
LINE (83, 326)-(103, 346), , B
LINE (103, 346)-(123, 326), , B
LINE (123, 326)-(143, 346), , B
LINE (5.5, 342)-(20.5, 329.5)
LINE (25.5, 329.5)-(40.5, 342), , B
CIRCLE (73, 335.75), 7.5, , , , .8333333
LINE (45.5, 329.5)-(60.5, 342), , BF
LINE (85.5, 342)-(100.5, 342)
LINE (100.5, 329.5)-(100.5, 342)
LINE (105.5, 342)-(113, 339.5)
LINE (113, 339.5)-(118, 337)
LINE (118, 337)-(120.5, 334.5)
PSET (128, 332)
PSET (130.5, 334.5)
PSET (130.5, 339.5)
PSET (135.5, 337)
LINE (143, 326)-(163, 346), , B
LINE (145.5, 329.5)-(160.5, 342), , B, 21845

END SUB

SUB Echo (Text$)
IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Break = 0 THEN Break = 80
ecr = 1
FOR i = 1 TO INT(LEN(Text$) / (Break - TextBreak)) + 1
LOCATE TextScale, TextBreak
PRINT MID$(Text$, ecr, Break - TextBreak + 1)
ecr = ecr + Break - TextBreak + 1
TextScale = TextScale + 1
NEXT
END SUB

SUB ERASECombo

CLOSE cmbFile
OPEN "c:\laos\cmb.tmp" FOR BINARY AS cmbFile
CLOSE cmbFile
KILL "c:\laos\cmb.tmp"
OPEN "c:\laos\cmb.tmp" FOR RANDOM AS cmbFile


END SUB

FUNCTION exist (Datei$)
28
f% = FREEFILE
ae% = errorlevel
errorlevel = 0
OPEN Datei$ FOR INPUT AS f%
IF errorlevel = 0 THEN exist = -1 ELSE exist = 0
CLOSE f%
errorlevel = ae%
29
END FUNCTION

SUB ExitBox (x, y)
IF x = -1 THEN x = EBox.left
IF y = -1 THEN y = EBox.top
LINE (x, y)-(x + 26, y + 20), Fensterfarbe, B
LINE (x + 1, y + 1)-(x + 25, y + 1), Schattenfarbe
LINE (x + 1, y + 1)-(x + 1, y + 19), Schattenfarbe
LINE (x, y + 21)-(x + 26, y + 21), Randfarbe
LINE (x + 27, y + 21)-(x + 27, y), Randfarbe

LINE (x + 2, y + 19)-(x + 24, y + 2), Fensterfarbe
LINE (x + 3, y + 19)-(x + 25, y + 2), Schattenfarbe
LINE (x + 2, y + 2)-(x + 24, y + 19), Fensterfarbe
LINE (x + 3, y + 2)-(x + 25, y + 19), Schattenfarbe

END SUB

FUNCTION FileChoose$ (Art1$)
'LÑsst den Benutzer eine Datei auswÑhlen. Art1$: Die Art-Spalte. Leer = alle.

31
Indrive = 0
SHELL "c:"
ERASECombo
ON ERROR GOTO fehlernext2
CLOSE 1: OPEN "drvlist.bnk" FOR RANDOM AS 1
'vorhanden-Test
p = 0
f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
DO
    GET 1, , LNam$
    IF LNam$ = "" THEN EXIT DO
    errorlevel = 0
    nf = FREEFILE
    lw$ = LEFT$(LNam$, 1): OPEN lw$ + ":\nul" FOR BINARY AS nf: CLOSE nf
    IF errorlevel = 0 THEN 'LW vorhanden:
        PUT f, , lw$
        p = p + 1

        B$ = ""
        OPEN lw$ + ":\laos\medname.inf" FOR RANDOM AS nf
        GET nf, 1, B$
        IF B$ = "" THEN B$ = MID$(LNam$, 4)
        SetCombo p, B$
        CLOSE nf
    END IF
LOOP UNTIL LNam$ = ""
CLOSE f

32
othDrives = p



fileNr = FREEFILE
DO
301
OPEN "\laos\ordner.bnk" FOR RANDOM AS fileNr
i = 0
DO
i = i + 1: p = p + 1
GET fileNr, i, a$
SetCombo p, a$
LOOP UNTIL a$ = ""
Register 6, 1, 18, 80, "Zeichnung laden:"

ExitBox -1, -1
ActivItem = ord$
ord$ = ComboBox(1, 8, 0, 0, 33)

IF keyb = "esc" AND Indrive = 1 THEN
CLOSE fileNr
GOTO 31
ELSEIF keyb = "esc" THEN EXIT FUNCTION
END IF

IF choice <= othDrives THEN 'fremdes LW angewÑhlt
    CLOSE fileNr: f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
    GET f, choice, lw$
    CLOSE f
    SHELL lw$ + ":"
    p = 0
    Indrive = 1
    ERASECombo
    GOTO 32
END IF

302
ERASECombo
CLOSE fileNr: OPEN "\laos\DATEIEN.BNK" FOR RANDOM AS fileNr LEN = 400
FIELD fileNr, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
i = 0
DO
a = a + 1
GET fileNr, a
IF RTRIM$(Ordner$) = ord$ AND RTRIM$(Art$) = Art1$ THEN i = i + 1: SetCombo i, RTRIM$(OsName$)
LOOP UNTIL Dateiname$ = STRING$(11, 0)

dat$ = ComboBox(1, 8, 35, 0, 72)
IF keyb = "esc" THEN EXIT FUNCTION
IF keyb = "tab" THEN 31
LOOP UNTIL keyb = CHR$(13)

CLOSE fileNr
CHDIR "\LAOS"
FileChoose = Dosfile(dat$, ord$)

SHARED nam$
nam$ = dat$
END FUNCTION

SUB FileCopy (Text$, Source$, Dest$)

Register 8, 20, 15, 60, ""
Echo ""
Echo " " + Text$

CLOSE 2
errorlevel = 0
f% = FREEFILE
OPEN Source$ FOR BINARY AS f%

 whole = LOF(f%) \ 500
 part = LOF(f%) MOD 500
 buffer$ = STRING$(500, 0)
 start& = 1

f2% = FREEFILE
OPEN Dest$ FOR BINARY AS f2%: CLOSE f2%
IF errorlevel <> 0 THEN 33210
KILL Dest$ 'stellt sicher, dass Dest$ leer ist.

OPEN Dest$ FOR BINARY AS f2%

  FOR x = 1 TO whole
       GET f%, start&, buffer$
       PUT f2%, start&, buffer$
       start& = start& + 500
     p% = INT(100 * LOF(f2%) / LOF(f%))
     ProgressBar p%
   IF errorlevel <> 0 THEN 33210
  NEXT x

 buffer$ = STRING$(part, 0)
  GET f%, start&, buffer$
  PUT f2%, start&, buffer$

ProgressBar 100
33210
CLOSE f%, f2%

OPEN "c:\LAOS\3dStudio.DFL" FOR BINARY AS 2

END SUB

SUB FontShow (x, y, Text$, ein%)
SHARED col, Befehl

FontFile$ = "NewFont.lff"

DIM Space AS INTEGER


Space = FontSpace * Size    'hochrechnen des Zeichenabstandes auf die
                        'gewÑhlte Schriftgrîsse

DIM f AS INTEGER, a AS INTEGER, B AS INTEGER, c AS INTEGER, d AS INTEGER
Text$ = RTRIM$(Text$)

FOR z = 1 TO LEN(Text$)
115
Zeichen = ASC(MID$(Text$, z, 1))

IF Zeichen = 32 THEN x = x + grx / 5 * Size + Space * 1: z = z + 1: GOTO 115


IF FontShadow = 0 THEN minmode% = 2 ELSE minmode% = 1
FOR mode% = minmode% TO 2
'1=normal, 2=schatten
CLOSE f
f = FREEFILE
OPEN "c:\laos\" + FontFile$ FOR BINARY AS f


'Zeichen suchen:
DO
GET f, , a
IF a = -1 THEN
    GET f, , a
    IF a = Zeichen THEN EXIT DO
END IF
LOOP UNTIL EOF(f)

'Zeichen zeichnen:
grx = 0

DO
GET f, , a
IF a = -1 THEN EXIT DO
GET f, , B
GET f, , c
GET f, , d
IF a > grx THEN grx = a
IF c > grx THEN grx = c

IF FontShadow > 0 AND mode% = 1 THEN
        LINE (a / 5 * Size + x - 1, B / 5 * Size + y - 1)-(c / 5 * Size + x - 1, d / 5 * Size + y - 1), FontShadow
IF ein% THEN Befehl = Befehl + 1: Setze Befehl, 0, ASC("l"): Setze Befehl, 1, a / 5 * Size + x - xplus - 1: Setze Befehl, 2, B / 5 * Size + y - yplus - 1: Setze Befehl, 3, c / 5 * Size + x - xplus - 1: Setze Befehl, 4, d / 5 * Size + y - yplus - 1: Setze Befehl, 5, FontShadow
ELSE
        LINE (a / 5 * Size + x, B / 5 * Size + y)-(c / 5 * Size + x, d / 5 * Size + y), col
IF ein% THEN Befehl = Befehl + 1: Setze Befehl, 0, ASC("l"): Setze Befehl, 1, a / 5 * Size + x - xplus: Setze Befehl, 2, B / 5 * Size + y - yplus: Setze Befehl, 3, c / 5 * Size + x - xplus: Setze Befehl, 4, d / 5 * Size + y - yplus: Setze Befehl, 5, col
END IF



IF FontBold THEN
        LINE (a / 5 * Size + x + 1, B / 5 * Size + y)-(c / 5 * Size + x + 1, d / 5 * Size + y), col
IF ein% THEN Befehl = Befehl + 1: Setze Befehl, 0, ASC("l"): Setze Befehl, 1, a / 5 * Size + x - xplus + 1: Setze Befehl, 2, B / 5 * Size + y - yplus: Setze Befehl, 3, c / 5 * Size + x - xplus + 1: Setze Befehl, 4, d / 5 * Size + y - yplus: Setze Befehl, 5, col
END IF

LOOP UNTIL a = -1 OR EOF(f)

NEXT mode%

x = x + grx / 5 * Size + Space

NEXT z 'NÑchstes Zeichen!


CLOSE f
END SUB

SUB Frame
'Frame kann ausgefÅhrt werden, um ein Rahmen um das Fenster zu machen,
'und dabei aber die Left- Top- usw.-Kordinaten zu behalten.
'einzig die TextScale- und TextBreak- Koordinaten wechseln nach oben links.
'-> Siehe auch die Sub "CLW".

'Fenster:
LINE (0, 0)-(638, 348), Fensterfarbe, B
'Schatten:
LINE (1, 1)-(637, 1), Schattenfarbe
LINE (1, 1)-(1, 347), Schattenfarbe
'Rand:
LINE (639, 349)-(0, 349), Randfarbe
LINE (639, 349)-(639, 0), Randfarbe


'EBox.left = 603: EBox.top = 321
'Left = 2: Top = 2: Right = 79: Down = 24
TextScale = 2: TextBreak = 2
END SUB

FUNCTION FrameWindow$ (a, B, c, d, del)

'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B

IF del = 1 THEN
LINE (a + 1, B + 1)-(c - 1, d - 1), 0, BF

'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
END IF

'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe


a$ = MID$(STR$(a), 2)
IF LEN(a$) < 3 THEN a$ = STRING$(3 - LEN(a$), "0") + a$
B$ = MID$(STR$(B), 2)
IF LEN(B$) < 3 THEN B$ = STRING$(3 - LEN(B$), "0") + B$
c$ = MID$(STR$(c), 2)
IF LEN(c$) < 3 THEN c$ = STRING$(3 - LEN(c$), "0") + c$
d$ = MID$(STR$(d), 2)
IF LEN(d$) < 3 THEN d$ = STRING$(3 - LEN(d$), "0") + d$

FrameWindow$ = a$ + B$ + c$ + d$
END FUNCTION

FUNCTION GetCombo$ (Index)

GET cmbFile, Index, GetCombo$

END FUNCTION

FUNCTION GetFile$ (Sym$, Art1$, Link1$)
SHARED nam$


'Laufwerke laden:
71
Indrive = 0
SHELL "c:"
ERASECombo
ON ERROR GOTO fehlernext2
CLOSE 1: OPEN "drvlist.bnk" FOR RANDOM AS 1
'vorhanden-Test
p = 0
f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
DO
    GET 1, , LNam$
    IF LNam$ = "" THEN EXIT DO
    errorlevel = 0
    nf = FREEFILE
    lw$ = LEFT$(LNam$, 1): OPEN lw$ + ":\nul" FOR BINARY AS nf: CLOSE nf
    IF errorlevel = 0 THEN 'LW vorhanden:
        PUT f, , lw$
        p = p + 1

        B$ = ""
        OPEN lw$ + ":\laos\medname.inf" FOR RANDOM AS nf
        GET nf, 1, B$
        IF B$ = "" THEN B$ = MID$(LNam$, 4)
        SetCombo p, B$
        CLOSE nf
    END IF
LOOP UNTIL LNam$ = ""
CLOSE f

72
othDrives = p

'Ordner laden:
CLOSE 1: OPEN "\laos\ordner.bnk" FOR RANDOM AS 1
i = 0
DO
i = i + 1: p = p + 1
GET 1, i, a$
SetCombo p, a$
LOOP UNTIL a$ = ""
200
Register 6, 1, 18, 80, "Datei speichern unter"
TextBreak = 35
Echo ""
Echo "Dateiname:"
Echo ""
Echo "Dateiinfo:"

DO
ExitBox -1, -1
201
ActivItem = ord$
ord$ = ComboBox(1, 8, 0, 0, 33)

IF keyb = "esc" AND Indrive = 1 THEN
GOTO 71
ELSEIF keyb = "esc" THEN EXIT FUNCTION
END IF

IF choice <= othDrives THEN 'fremdes LW angewÑhlt
    f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
    GET f, choice, lw$
    CLOSE f
    SHELL lw$ + ":"
    p = 0
    Indrive = 1
    ERASECombo
    GOTO 72
END IF


202
nam$ = InputBox(8, 47, nam$, 32, 1)
IF keyb = "esc" THEN EXIT FUNCTION
IF keyb = "down" THEN 203
IF keyb = "left" THEN 201

203
inf$ = InputBox(10, 47, inf$, 32, 8)
IF keyb = "esc" THEN EXIT FUNCTION
IF keyb = "up" THEN 202
IF keyb = "left" THEN 201
LOOP UNTIL keyb = CHR$(13)


IF IsExist(nam$, ord$) = 1 THEN
Register 8, 5, 16, 75, "Datei existiert schon"
Echo ""
Echo "Dieser Dateiname existiert schon im angewÑhlten Ordner."
Echo "Soll diese Datei Åberschrieben werden, oder soll eine neue erstellt  werden?"
ClearControls 0
PRINT Control(14, 20, "     Neue Datei     ")
PRINT Control(14, 42, "   öberschreiben   ")
Activate "     Neue Datei     "
GetInp
IF Focus = "   öberschreiben   " THEN
ClearControls 0
errorlevel = 0
CLOSE 1: OPEN "\laos\wrt~.tmp" FOR OUTPUT AS 1
IF errorlevel = 1 THEN Dialog "Das Schreiben auf den DatentrÑger ist nicht mîglich. öberprÅfen Sie den     Schreibschutz.": GOTO 71
a$ = Dosfile(nam$, ord$)
SetAttr a$, "": IF RIGHT$(Linkit, 6) = "runtmp" THEN Dialog "Zeichnung muss erneut verschlÅsselt werden."
GetFile = a$: EXIT FUNCTION
END IF
IF Focus = "     Neue Datei     " THEN ClearControls 0: GOTO 200
END IF

'File îffnen:
errorlevel = 0

IF Sym$ = "" THEN Sym$ = "sys"
CLOSE 1: OPEN "\laos\DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
DO
a = a + 1
GET 1, a
LOOP UNTIL LText$(Dateiname$, 1) = ""

B = a
DO
dat$ = MID$(STR$(B), 2)
B = B + 1
LOOP WHILE exist(dat$)

IF errorlevel = 0 THEN

LSET Symbol$ = Sym$
LSET Dateiname$ = dat$
LSET Art$ = Art1$
LSET OsName$ = nam$
LSET Ordner$ = ord$
LSET link$ = Link1$
LSET Datum$ = SDate
LSET Zeit$ = TIME$
LSET FileInfo$ = inf$
PUT 1, a

ELSE Dialog "Das Schreiben auf den DatentrÑger ist nicht mîglich. öberprÅfen Sie den     Schreibschutz.": GOTO 71
END IF

CLOSE 1

GetFile = dat$
CHDIR "\LAOS"

END FUNCTION

SUB GetInp
DIM kbd AS STRING
'Aktiviere das Control mit dem Fokus:
SELECT CASE ControlConnect
CASE 0: Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
END SELECT


DO
  FOR i = 1 TO 4
    AlteZeit(i) = TIMER
  NEXT i

warteknopflos
kbd = ""
amx = mausx
amy = mausy

WHILE kbd = ""
  kbd = INKEY$

GetMaus

IF mausx > amx + mks THEN kbd = CHR$(0) + "M"
IF mausx < amx - mks THEN kbd = CHR$(0) + "K"

IF mausy > amy + mks THEN kbd = CHR$(0) + "P"
IF mausy < amy - mks THEN kbd = CHR$(0) + "H"

IF mausk = 1 THEN kbd = CHR$(13)
IF mausk = 2 THEN kbd = CHR$(27)
IF mausk = 4 THEN kbd = CHR$(0) + "M"

   FOR i = 1 TO 4
     IF Stoppuhr(i) > 0 THEN
       IF TIMER - AlteZeit(i) > Stoppuhr(i) THEN StoppUhrBeep (i): AlteZeit(i) = TIMER
     END IF
   NEXT i
 WEND
TIMER OFF: FOR i = 15 TO 19: KEY(i) OFF: NEXT i

warteknopflos

SELECT CASE kbd
CASE CHR$(13)
SELECT CASE ControlConnect
CASE 0: Focus = MID$(ControlList(ControlNr), 5)
CASE 1: Focus = MID$(MenuList(MenuNr), 5)
END SELECT
keyb = kbd
EXIT SUB
CASE CHR$(9) 'Tab
STATIC ch AS INTEGER

Activate CHR$(0) 'Aktiviere ein Control, das gar nicht existiert, um alle
                 'anderen zu deaktivieren.
ControlConnect = ControlConnect + 1
IF ControlConnect > 2 THEN ControlConnect = 0
FOR i = 1 TO 3
SELECT CASE ControlConnect
CASE 0: IF ControlList(1) = "" THEN ControlConnect = 1
CASE 1: IF Change(ch + 1) <> 0 THEN ControlConnect = 0: EXIT FOR
        IF MenuList(1) = "" THEN ControlConnect = 2
END SELECT
NEXT i
SELECT CASE ControlConnect
CASE 0
ch = ch + 1
IF Change(ch) = 0 THEN ch = 0: ControlNr = 1
IF ch <> 0 THEN ControlNr = Change(ch)
Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
END SELECT
CASE CHR$(0) + "H", CHR$(0) + "K", "4": LastControl
CASE CHR$(0) + "P", CHR$(0) + "M", "6": NextControl
CASE CHR$(27)
keyb = "esc": EXIT SUB'Minimize
END SELECT
LOOP

END SUB

FUNCTION GetKey$
'erweitert: Mit Maus-Funktion
a$ = ""
warteknopflos
amx = mausx
amy = mausy

DO WHILE a$ = ""

a$ = INKEY$

GetMaus


IF NOT NoMouseReg THEN
IF mausx > amx + mks THEN a$ = CHR$(0) + "M"
IF mausx < amx - mks THEN a$ = CHR$(0) + "K"

IF mausy > amy + mks THEN a$ = CHR$(0) + "P"
IF mausy < amy - mks THEN a$ = CHR$(0) + "H"
END IF

IF mausk = 1 THEN a$ = CHR$(13)
IF mausk = 2 THEN a$ = CHR$(27)

IF mausx = 0 THEN mausx = 638: mausset FIX(mausx), FIX(mausy): EXIT DO
IF mausy = 0 THEN mausy = 348: mausset FIX(mausx), FIX(mausy): EXIT DO
IF mausx = 639 THEN mausx = 1: mausset FIX(mausx), FIX(mausy): EXIT DO
IF mausy = 349 THEN mausy = 1: mausset FIX(mausx), FIX(mausy): EXIT DO
LOOP

SELECT CASE MID$(a$, 2)
CASE "H": a$ = "up"
CASE "P": a$ = "down"
CASE "K": a$ = "left"
CASE "M": a$ = "right"
CASE "R": a$ = "insert"
CASE "S": a$ = "delete"
CASE "G": a$ = "home"
CASE "O": a$ = "end"
CASE "I": a$ = "pageup"
CASE "Q": a$ = "pagedown"
END SELECT

GetKey = a$
keyb = a$
END FUNCTION

SUB GetMaus


  r% = Interr%(&H33, 3, bx%, cx%, dx%)
 
  mausk% = bx%
  mausx% = cx%
  mausy% = dx%

END SUB

SUB GetMouseVal
    Register 6, 20, 16, 60, "Maus"
    ExitBox -1, -1
    Echo ""
    Echo "Maustempo: (je grîsser desto langsamer)"
    Echo ""
        a$ = InputBox(0, 0, LTRIM$(STR$(Mausspeed)), 2, 1)

IF keyb = "esc" THEN EXIT SUB


DO
ClearControls 0
B$ = Control(14, 22, CHR$(VisualDraw) + " Visuelles Zeichnen")
B$ = Control(14, 48, "  OK  ")
Activate "  OK  "
GetInp

IF keyb = "esc" THEN EXIT SUB

IF ControlNr = 1 THEN
IF VisualDraw = an THEN VisualDraw = aus ELSE VisualDraw = an
END IF
LOOP UNTIL Focus = "  OK  "
ClearControls 0

Mausspeed = VAL(a$)

Regedit "QuickDraw.Mousespeed=" + STR$(Mausspeed)
MausTempo Mausspeed
Regedit "QuickDraw.VisualDraw=" + STR$(VisualDraw)

END SUB

SUB GetToolEvents
SHARED x, y, col, a$, rsv, arsv
ToolBarMode = 0

IF x > 5 AND x < 316 AND y > 11 AND y < 32 THEN
'ein Menu ausgewÑhlt: welches?
SELECT CASE x
CASE IS < 45: MenuNr = 1
CASE IS < 132: IF x > 53 THEN MenuNr = 2
CASE IS < 197: IF x > 141 THEN MenuNr = 3
CASE IS > 205: MenuNr = 4
END SELECT

a$ = CHR$(9)
ToolBarMode = 0
DrawToolBar
END IF

IF x > 327 AND x < 352 AND y > 13 AND y < 28 THEN
'Farbe ausgewÑhlt
        PCOPY 0, 1
        col = ColorChoice
        PCOPY 1, 0
        TextScale = 2: TextBreak = 2
        Left = 2: Top = 2: Right = 79: Down = 24
        StaticDraw rsv
        arsv = 0: rsv = 0
        ToolBarMode = 0
        DrawToolBar
        EXIT SUB
END IF

IF y > 326 AND y < 346 THEN
        IF x > 3 AND x < 163 THEN

'Toolbar ausgewÑhlt
StaticDraw rsv

SELECT CASE x
CASE IS > 143: ToolBarMode = 8
CASE IS > 123: ToolBarMode = 7
CASE IS > 103: ToolBarMode = 6
CASE IS > 83:  ToolBarMode = 5
CASE IS > 63:  ToolBarMode = 4
CASE IS > 43:  ToolBarMode = 3
CASE IS > 23:  ToolBarMode = 2
CASE IS > 3:   ToolBarMode = 1
END SELECT

DrawToolBar

        END IF
END IF


END SUB

SUB GraphicView (a, B, c, d)
'Diese Sub Ñndert die Umgebungsvariablen (Left, Top, usw.) fÅr einen bestimmt-
'en Fenster-Bereich. Die darauffolgenden Grafikausgaben erfolgen in diesem
'Bereich.
'LINE (a, B)-(c, d), Randfarbe, B

EBox.left = c - 31: EBox.top = d - 24
Left = INT(a / 8) + 2: Top = INT(B / 14) + 2: Right = INT((c + 8) / 8) - 1: Down = INT((d + 14) / 14) - 1
END SUB

FUNCTION InputBox$ (Zeilen, Spalten, Text$, Break, MaxDown)
'Break=Zeilenumbruch | MaxDown=Maximale Zeilen

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak + 1

IF Break = 0 OR Break > Right - Spalten THEN Break = Right - Spalten
IF MaxDown = 0 THEN MaxDown = 24 - Zeilen
TotalBreak = Break
Texta$ = Text$: a$ = ""
GOTO Deletefirst

Lesen:
NoMouseReg = -1
a$ = GetKey
NoMouseReg = 0

IF a$ = CHR$(13) THEN InputBox$ = Texta$: GOTO delcur
IF a$ = "up" OR a$ = "down" OR a$ = "left" OR a$ = "right" THEN InputBox$ = Texta$: GOTO delcur
IF a$ = CHR$(9) THEN keyb = "tab": InputBox$ = Texta$: GOTO delcur
IF a$ = CHR$(27) THEN keyb = "esc": InputBox$ = Texta$: GOTO delcur
IF a$ = CHR$(8) THEN
IF LEN(Texta$) > 0 THEN Texta$ = LEFT$(Texta$, LEN(Texta$) - 1)
LINE (a, B)-(c + 1, d + 1), 0, BF
a$ = ""
END IF
IF Dwn > MaxDown THEN a$ = "": nocur = 1

IF LEN(a$) = 1 THEN Texta$ = Texta$ + a$
Break = LEN(Texta$) + 1

Deletefirst:
Break = LEN(Texta$) + 1
Dwn = INT(LEN(Texta$) / TotalBreak) + 1
IF Break > TotalBreak THEN Break = TotalBreak: nocur = 1
IF LEN(Texta$) < (INT(LEN(Texta$) / TotalBreak) + 1) * TotalBreak AND LEN(Texta$) > INT(LEN(Texta$) / TotalBreak) * TotalBreak THEN nocur = 0

a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + Break
EndZeile = Zeilen + INT(LEN(Texta$) / Break + 1)
IF Dwn > MaxDown AND MaxDown > 1 THEN EndZeile = EndZeile - 1
IF Break = LEN(Texta$) THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

p$ = FrameWindow(a, B, c, d, 1)


ecr = TotalBreak * -1 + 1
FOR i = 1 TO INT(LEN(Texta$) / TotalBreak) + 1
LOCATE Zeilen + i - 1, Spalten
ecr = ecr + TotalBreak
PRINT MID$(Texta$, ecr, TotalBreak);
NEXT i
ecr = 0

IF nocur = 0 THEN LINE (POS(0) * 8, CSRLIN * 14)-(POS(0) * 8 - 8, CSRLIN * 14 - 14), InfoColor, BF ELSE nocur = 0
GOTO Lesen

delcur:
LINE (POS(0) * 8, CSRLIN * 14)-(POS(0) * 8 - 8, CSRLIN * 14 - 14), 0, BF

END FUNCTION

FUNCTION Interr% (Num%, AX%, bx%, cx%, dx%)


 IF MS%(0) = 0 THEN
  PRINT "FEHLER: Maschinenprogramm nicht eingelesen! Abbruch!"
  END
 END IF


 DEF SEG = VARSEG(MS%(0))
 POKE VARPTR(MS%(0)) + 26, Num%

 CALL ABSOLUTE(AX%, bx%, cx%, dx%, VARPTR(MS%(0)))

 Interr% = AX%

END FUNCTION

FUNCTION IsExist (Datei$, InOrdner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN IsExist = 1: EXIT FUNCTION
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = "" THEN IsExist = 1: EXIT FUNCTION
LOOP UNTIL LText$(Dateiname$, 1) = ""

IsExist = 0

END FUNCTION

FUNCTION LaOSFileName$ (Dosfile$)

CLOSE 1: OPEN "\laos\DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
DO
a = a + 1
GET 1, a
IF RTRIM$(Dateiname$) = Dosfile$ THEN LaOSFileName = RTRIM$(OsName$): EXIT FUNCTION
LOOP UNTIL LText$(Dateiname$, 1) = ""

END FUNCTION

SUB LastControl
SELECT CASE ControlConnect
CASE 0
ControlNr = ControlNr - 1
IF ControlNr < 1 THEN ControlNr = 1
i = 0
DO
i = i + 1
a$ = ControlList(i)
Activ = MID$(ControlList(ControlNr), 5)
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
IF a$ = "" THEN EXIT DO
p$ = Control(Zeilen, Spalten, Texta$)
LOOP UNTIL a$ = ""
CASE 1
MenuNr = MenuNr - 1
IF MenuNr < 1 THEN MenuNr = 1
Activate MenuList(MenuNr)
END SELECT
END SUB

FUNCTION Linkit$
CLOSE 1: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 1
GET 1, 1, a$
Linkit = a$
CLOSE 1
END FUNCTION

FUNCTION LText$ (Text$, ab)
IF ab = 0 THEN ab = 1
FOR i = ab TO LEN(Text$) - ab + 1
IF MID$(Text$, i, 1) = " " OR ASC(MID$(Text$, i, 1)) = 0 THEN EXIT FOR
NEXT i
LText$ = MID$(Text$, ab, i - ab)
END FUNCTION

SUB Markieren (px, py, x, y)
SHARED m.1, m.2, m.3, m.4
IF px = 0 THEN px = m.1: py = m.2: x = m.3: y = m.4

IF px > x THEN SWAP px, x
IF py > y THEN SWAP py, y


FOR Nr = 1 TO 2
FOR i = px TO x STEP 5
PUT (i, y), Punkt
NEXT
SWAP y, py
NEXT Nr

FOR Nr = 1 TO 2
FOR i = py TO y STEP 5
PUT (x, i), Punkt
NEXT
SWAP x, px
NEXT Nr

END SUB

SUB maus (OnOff%)


 IF OnOff% = 0 THEN OnOff% = 2 ELSE OnOff% = 1
 r% = Interr%(&H33, OnOff%, bx%, cx%, dx%)

END SUB

SUB MausForm (SoftHard%, BMaske%, CMaske%)

 r% = Interr%(&H33, 10, SoftHard%, BMaske%, CMaske%)      '** Mausform setzen

END SUB

SUB mausinit


 r% = Interr%(&H33, 0, bx%, cx%, dx%)           '** Maustreiber initialisieren

 r% = Interr%(&H33, 10, 0, &HFFFF, &HFF00)      '** Maushintergrund: blinken

END SUB

SUB mausset (x, y)

 r% = Interr%(&H33, 4, bx%, FIX(x), FIX(y))

END SUB

SUB MausTempo (speed%)


 r% = Interr%(&H33, 15, bx%, speed%, speed% * 2)

END SUB

SUB mausxbereich (x1, x2)

 r% = Interr%(&H33, 7, 0, x1 * 8 - 8, x2 * 8 - 8)

END SUB

SUB MausYBereich (y1, y2)

 r% = Interr%(&H33, 8, bx%, y1 * 8 - 8, y2 * 8 - 8)

END SUB

SUB Menuact (Menu$)
Zeilen = VAL(LEFT$(Menu$, 2))
Spalten = VAL(MID$(Menu$, 3, 2))
Caption$ = MID$(Menu$, 5)
a = Spalten * 8 - 11: B = Zeilen * 14 - 17
c = (Spalten + LEN(Caption$)) * 8 - 5: d = (Zeilen + 1) * 14 - 11


IF Activ = Menu$ THEN
LOCATE Zeilen, Spalten: PRINT Caption$
Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
ELSE
LINE (a + 1, B + 1)-(c - 1, B + 1), 0
LINE (a + 1, B + 1)-(a + 1, d - 1), 0
END IF
END SUB

SUB MenuBox (Zeilen, Spalten, Caption$)
STATIC NMZeilen AS INTEGER, NMSpalten AS INTEGER
IF NMZeilen = 0 THEN NMZeilen = Top
IF NMSpalten = 0 THEN NMSpalten = Left
IF Zeilen = 0 THEN Zeilen = NMZeilen
IF Spalten = 0 THEN Spalten = NMSpalten

IF Spalten + LEN(Caption$) - 1 > Right THEN Spalten = Left: Zeilen = Zeilen + 2
IF Zeilen > Down THEN Zeilen = Top: Spalten = Left

a = Spalten * 8 - 11: B = Zeilen * 14 - 17: c = (Spalten + LEN(Caption$)) * 8 - 5
d = (Zeilen + 1) * 14 - 11

LINE (a, B)-(c, d), 0, BF
LOCATE Zeilen, Spalten: PRINT Caption$
LINE (a, B)-(c, d), Fensterfarbe, B
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe



NMSpalten = Spalten + LEN(Caption$) + 2
NMZeilen = Zeilen
IF NMSpalten > Right THEN NMSpalten = Left: NMZeilen = Zeilen + 2
IF NMZeilen > Down THEN NMZeilen = Top: NMSpalten = Left

'Menu eintragen:
Zeilen$ = MID$(STR$(Zeilen), 2)
Spalten$ = MID$(STR$(Spalten), 2)
IF LEN(Zeilen$) < 2 THEN Zeilen$ = "0" + Zeilen$
IF LEN(Spalten$) < 2 THEN Spalten$ = "0" + Spalten$

a$ = Zeilen$ + Spalten$ + Caption$
i = 0
DO: i = i + 1
IF MenuList(i) = a$ THEN EXIT SUB
LOOP UNTIL MenuList(i) = ""
MenuList(i) = a$
MenuNr = 1

TextScale = Zeilen + 2
END SUB

FUNCTION MenuInt$ (Item$, Trennzeichen$)

Item$ = Item$ + Trennzeichen$

i = 0
MaxLenth = 1
ERASECombo

DO
i = i + 1

a$ = Trim(Item$, Trennzeichen$, i)

IF LEN(a$) > 77 THEN a$ = LEFT$(a$, 77)
IF LEN(a$) > MaxLenth THEN MaxLenth = LEN(a$)

IF a$ <> "" THEN SetCombo i, a$


LOOP WHILE a$ <> ""

Zeilen = VAL(LEFT$(MenuList(MenuNr), 2)) + 1
Spalten = VAL(MID$(MenuList(MenuNr), 3, 2))
IF Spalten + MaxLenth > 79 THEN Spalten = 79 - MaxLenth
PCOPY 0, 1

a$ = ComboBox(1, Zeilen, Spalten, Zeilen + i - 2, Spalten + MaxLenth)


DO

SELECT CASE keyb
CASE CHR$(13): MenuInt$ = a$: EXIT DO
CASE "esc": MenuInt$ = "": choice = 0: keyb = "esc": EXIT DO
END SELECT
LOOP

PCOPY 1, 0
Activate CHR$(0)



END FUNCTION

SUB MShows
Frame
IF Menucase = aus THEN
SHARED col
COLOR col: LOCATE 2, 42: PRINT "€€€"; : COLOR Farbe

FOR i = 1 TO 10
a$ = MenuList(i)
IF a$ = "" THEN EXIT SUB

a = VAL(MID$(a$, 3, 2)) * 8 - 11: B = VAL(LEFT$(a$, 2)) * 14 - 17: c = (VAL(MID$(a$, 3, 2)) + LEN(MID$(a$, 5))) * 8 - 5
d = (VAL(LEFT$(a$, 2)) + 1) * 14 - 11

B$ = FrameWindow(a, B, c, d, 0)
Clearase B$

LOCATE VAL(LEFT$(a$, 2)), VAL(MID$(a$, 3, 2))
PRINT MID$(a$, 5)


NEXT
END IF
END SUB

SUB NextActiv (Retour AS INTEGER)
SHARED ActivNummer AS INTEGER
SHARED NrActiv 'îffentliche Variable zur Abfrage des aktiven Befehls
SHARED m.1, m.2, m.3, m.4

IF Retour = 1 THEN
ActivNummer = ActivNummer - 1
ELSEIF Retour = 0 THEN
ActivNummer = ActivNummer + 1
END IF

nochmals:
anr = 0

i = 0
DO
IF anr = ActivNummer THEN NrActiv = i: Showactiv i: EXIT SUB
i = i + 1
SELECT CASE CHR$(Befehle(i, 0))
CASE "p", "m":
IF Befehle(i, 1) + xplus >= m.1 AND Befehle(i, 1) + xplus <= m.3 AND Befehle(i, 2) + yplus >= m.2 AND Befehle(i, 2) + yplus <= m.4 THEN
    anr = anr + 1
END IF
CASE "l", "r", "b"
IF Befehle(i, 1) + xplus >= m.1 AND Befehle(i, 1) + xplus <= m.3 AND Befehle(i, 2) + yplus >= m.2 AND Befehle(i, 2) + yplus <= m.4 AND Befehle(i, 3) + xplus >= m.1 AND Befehle(i, 3) + xplus <= m.3 AND Befehle(i, 4) + yplus >= m.2 AND Befehle(i, 4) + yplus <= m.4 THEN
    anr = anr + 1
END IF
CASE "k", "x"
IF Befehle(i, 5) < 1 THEN rx = Befehle(i, 3): ry = Befehle(i, 5) * rx
IF Befehle(i, 5) >= 1 THEN ry = Befehle(i, 3): rx = ry / Befehle(i, 5)

IF Befehle(i, 1) - rx + xplus >= m.1 AND Befehle(i, 1) + rx + xplus <= m.3 AND Befehle(i, 2) - ry + yplus >= m.2 AND Befehle(i, 2) + ry + yplus <= m.4 THEN
    anr = anr + 1
    IF Befehle(i, 0) = ASC("x") THEN i = i + 1
END IF

END SELECT
LOOP WHILE Befehle(i, 0) <> 0

IF ActivNummer < 0 THEN ActivNummer = i ELSE ActivNummer = 0
GOTO nochmals
END SUB

SUB NextControl
SELECT CASE ControlConnect
CASE 0
tC = 0
DO
tC = tC + 1
LOOP WHILE ControlList(tC) <> ""
tC = tC - 1

ControlNr = ControlNr + 1
IF ControlNr > tC THEN ControlNr = tC
i = 0
DO
i = i + 1
a$ = ControlList(i)
Activ = MID$(ControlList(ControlNr), 5)
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
IF a$ = "" THEN EXIT DO
p$ = Control(Zeilen, Spalten, Texta$)
LOOP UNTIL a$ = ""
CASE 1
tC = 0: DO: tC = tC + 1: LOOP WHILE MenuList(tC) <> ""
MenuNr = MenuNr + 1
IF MenuNr > tC - 1 THEN MenuNr = tC - 1
Activate MenuList(MenuNr)
END SELECT
END SUB

FUNCTION NextOne (rely.on)
SHARED m.1, m.2, m.3, m.4, NrActiv, IsTue
IsTue = 0
STATIC LastOne

    IF NrActiv <> 0 AND rely.on <> 0 THEN
        IF LastOne <> -1 THEN NextOne = NrActiv: LastOne = -1: EXIT FUNCTION
        IF LastOne = -1 THEN LastOne = 0: NextOne = 0: EXIT FUNCTION
    END IF

i = LastOne + 1
IF rely.on = 0 THEN NextOne = LastOne
DO
SELECT CASE CHR$(Befehle(i, 0))
CASE "p", "m"
IF Befehle(i, 1) + xplus >= m.1 AND Befehle(i, 1) + xplus <= m.3 AND Befehle(i, 2) + yplus >= m.2 AND Befehle(i, 2) + yplus <= m.4 THEN
    PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs
    LastOne = i: NextOne = i: EXIT FUNCTION
END IF
CASE "l", "r", "b"
IF Befehle(i, 1) + xplus >= m.1 AND Befehle(i, 1) + xplus <= m.3 AND Befehle(i, 2) + yplus >= m.2 AND Befehle(i, 2) + yplus <= m.4 AND Befehle(i, 3) + xplus >= m.1 AND Befehle(i, 3) + xplus <= m.3 AND Befehle(i, 4) + yplus >= m.2 AND Befehle(i, 4) + yplus <= m.4 THEN
    PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs: PUT (Befehle(i, 3) + xplus, Befehle(i, 4) + yplus), crs
    LastOne = i: NextOne = i: EXIT FUNCTION
END IF
CASE "k", "x"
IF Befehle(i, 5) < 1 THEN rx = Befehle(i, 3): ry = Befehle(i, 5) * rx
IF Befehle(i, 5) >= 1 THEN ry = Befehle(i, 3): rx = ry / Befehle(i, 5)

IF Befehle(i, 1) - rx + xplus >= m.1 AND Befehle(i, 1) + rx + xplus <= m.3 AND Befehle(i, 2) - ry + yplus >= m.2 AND Befehle(i, 2) + ry + yplus <= m.4 THEN

PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs
PUT (Befehle(i, 1) + rx + xplus, Befehle(i, 2) + ry + yplus), crs
PUT (Befehle(i, 1) - rx + xplus, Befehle(i, 2) - ry + yplus), crs


IF Befehle(i, 0) = ASC("x") THEN LastOne = i + 1: IsTue = 1:  ELSE LastOne = i

NextOne = i: EXIT FUNCTION
END IF

END SELECT
i = i + 1
LOOP WHILE Befehle(i, 0) <> 0

LastOne = 0
NextOne = 0
END FUNCTION

SUB Paintic (a, B, f)
'a = Befehle(i, 1) + xplus: b = Befehle(i, 2) + yplus
'f = Befehle(i, 3)
'PRINT a, b, c
LINE (a, B)-(a + 3, B + 3), f
LINE (a + 3, B + 3)-(a + 3, B - 1), f
LINE (a + 1, B - 1)-(a + 5, B - 3), f, BF
END SUB

SUB ProgressBar (Stat AS INTEGER)
SHARED aStat AS INTEGER

'Die Anzeige wird nur generiert, falls der Status-Wert (Stat) seit dem
'letzen Aufruf geÑndert hat:
IF Stat = aStat THEN EXIT SUB

IF Stat > 100 THEN Stat = 100

LINE (179, 174)-(459, 194), Farbe, B

x = 278 * Stat / 100

LINE (180, 175)-(180 + x, 193), InfoColor, BF

aStat = Stat


END SUB

FUNCTION question% (Titel$, a$, B$)
IF NOT Changed THEN question% = -1: EXIT FUNCTION

PCOPY 0, 1
    Register 6, 20, 13, 60, Titel$
    Echo a$
    Echo B$
    ClearControls 0
    TextScale = TextScale + 1
    PRINT Control(0, 0, "Ja")
    PRINT Control(0, 0, "Nein")
    ExitBox -1, -1
    Activate "Nein"
    GetInp
    IF Focus = "Ja" THEN question% = -1 ELSE question% = 0
    IF keyb = "esc" THEN question% = 0
    ClearControls 0
PCOPY 1, 0

clf
END FUNCTION

SUB RadioButton (z, s, akt)
CIRCLE (s * 8 - 4, z * 14 - 7), 8, Farbe
PAINT (s * 8 - 4, z * 14 - 7), 0, 0

IF akt <> 0 THEN CIRCLE (s * 8 - 4, z * 14 - 7), 5, Farbe: PAINT (s * 8 - 4, z * 14 - 7), Farbe
LOCATE z, s + 3
TextScale = z
TextBreak = s + 3
END SUB

SUB ReadData


 RESTORE MS.Data
 DEF SEG = VARSEG(MS%(0))

 FOR i% = 0 TO 99
  READ Byte$
  IF Byte$ = "-" THEN EXIT FOR
  POKE VARPTR(MS%(0)) + i%, VAL("&H" + Byte$)
 NEXT i%

END SUB

SUB Regedit (Eintrag$)
'Eintrag$: ein Eintrag in Form von 'Nenner=Bezeichner'

'PrÅfen, ob Eintrag schon vorhanden:
Vh = 0
f = FREEFILE
OPEN "qd.cfg" FOR BINARY AS f: CLOSE f
OPEN "qd.cfg" FOR INPUT AS f

lin = 0
DO WHILE NOT EOF(f)
lin = lin + 1
LINE INPUT #f, a$
IF UCASE$(LEFT$(a$, INSTR(Eintrag$, "="))) = UCASE$(LEFT$(Eintrag$, INSTR(Eintrag$, "="))) THEN Vh = 1: EXIT DO
LOOP

f2 = FREEFILE
CLOSE f

IF Vh = 1 THEN 'Eintrag ersetzen:

OPEN "qd.cfg" FOR INPUT AS f
OPEN "reg.tmp" FOR OUTPUT AS f2
FOR i = 1 TO lin - 1
LINE INPUT #f, a$
PRINT #f2, a$
NEXT
PRINT #f2, Eintrag$
LINE INPUT #f, a$
DO WHILE NOT EOF(f)
LINE INPUT #f, a$
PRINT #f2, a$
LOOP
CLOSE f, f2
KILL "qd.cfg"
NAME "reg.tmp" AS "qd.cfg"


ELSE    'Eintrag hinzufÅgen:

OPEN "qd.cfg" FOR APPEND AS f
PRINT #f, Eintrag$
CLOSE f

END IF

END SUB

SUB Register (z, s, zu, su, Text$)
IF z = 0 THEN z = 1
IF s = 0 THEN s = 1
IF zu = 0 THEN zu = 25
IF su = 0 THEN su = 80
IF s + LEN(Text$) > su - 1 AND su - s > 2 THEN Text$ = LEFT$(Text$, su - s - 3) + ".."
a = s * 8 - 4
B = z * 14 - 9
c = su * 8 - 7
d = zu * 14 - 7
'Fenster:
LINE (a, B - 4)-(c, d), 0, BF
LINE (a, B)-(c, d), Fensterfarbe, B
'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe

TextScale = INT(B / 14) + 2 '=Zeile
TextBreak = INT(a / 8) + 2 '=Spalte
LOCATE INT(B / 14) + 1, TextBreak
PRINT Text$
Break = su - 1
IF z = 1 THEN LINE (0, 0)-(638, 0), Fensterfarbe: LINE (1, 1)-(637, 1), 0: LINE (1, 1)-(1, 347), 0
EBox.left = c - 31: EBox.top = d - 24
Left = s + 1: Top = z + 1: Right = su - 1: Down = zu - 1
END SUB

FUNCTION RegItem$ (Nenner$)

Vh = 0
f = FREEFILE
OPEN "qd.cfg" FOR BINARY AS f: CLOSE f
OPEN "qd.cfg" FOR INPUT AS f

DO WHILE NOT EOF(f)
LINE INPUT #f, a$
IF UCASE$(LEFT$(a$, LEN(Nenner$))) = UCASE$(Nenner$) AND MID$(a$, LEN(Nenner$) + 1, 1) = "=" THEN Vh = 1: EXIT DO
LOOP
CLOSE f

IF Vh = 0 THEN errorlevel = 1: EXIT FUNCTION

RegItem = MID$(a$, LEN(Nenner$) + 2)
END FUNCTION

SUB Schriftformat
PCOPY 0, 1
Register 6, 10, 20, 70, "Schrifteinstellungen"
ExitBox -1, -1

LOCATE 8, 11
PRINT "Fett (1=Ja, 0=Nein)"
8601 FontBold = VAL(InputBox(8, 40, LTRIM$(STR$(FontBold)), 0, 3))
LINE (87, 196)-(521, 270), 0, BF
FontShow 88, 197, "ABC", 0

IF keyb = "esc" THEN PCOPY 1, 0: EXIT SUB
IF keyb <> "down" AND keyb <> CHR$(13) THEN 8601

LOCATE 10, 11
PRINT "Schriftgrîsse:"
8602 Size = VAL(InputBox(10, 40, LTRIM$(STR$(Size)), 0, 3))
LINE (87, 196)-(521, 270), 0, BF
FontShow 88, 197, "ABC", 0
IF keyb = "up" THEN 8601
IF keyb = "esc" THEN PCOPY 1, 0: EXIT SUB

LOCATE 12, 11
PRINT "Zeichenabstand"
8603 FontSpace = VAL(InputBox(12, 40, LTRIM$(STR$(FontSpace)), 0, 3))
LINE (87, 196)-(521, 270), 0, BF
FontShow 88, 197, "ABC", 0
IF keyb = "up" THEN 8602
IF keyb = "esc" THEN PCOPY 1, 0: EXIT SUB

LOCATE 14, 11
PRINT "Schattenfarbe (0=keiner)"
8604 FontShadow = VAL(InputBox(14, 40, LTRIM$(STR$(FontShadow)), 0, 3))
LINE (87, 196)-(521, 270), 0, BF
FontShow 88, 197, "ABC", 0
IF keyb = "up" THEN 8603
IF keyb = "down" THEN 8604
PCOPY 1, 0
IF keyb = "esc" THEN EXIT SUB

END SUB

SUB SCircle (xi, yi, r, asp)
x = xi
y = yi

alta = r * SIN(0 * PI / 180)
altb = r * COS(0 * PI / 180)

FOR Winkel = 18 TO 360 STEP 18 '20 Ecken (weil 360/18=20)

a = r * SIN(Winkel * PI / 180)
B = r * COS(Winkel * PI / 180)

SLine x + a, y + B * asp, x + alta, y + altb * asp

alta = a
altb = B

NEXT

END SUB

FUNCTION SDate$ 'Function fÅr das System-Datum
SELECT CASE Datumsformat
CASE 0'TT.MM.JJJJ
SDate$ = MID$(DATE$, 4, 2) + "." + LEFT$(DATE$, 2) + "." + RIGHT$(DATE$, 4)
CASE 1'MM-TT-JJJJ
SDate$ = DATE$
CASE 2'JJJJ/MM/TT
SDate$ = RIGHT$(DATE$, 4) + "/" + LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2)
END SELECT
END FUNCTION

SUB SetAttr (aDatei$, Attr$)
Datei$ = aDatei$'ByVal simulieren

IF RIGHT$(Datei$, 4) = ".lao" THEN Datei$ = LEFT$(Datei$, LEN(Datei$) - 4)
nf = FREEFILE

CLOSE nf: OPEN "DATEIEN.BNK" FOR RANDOM AS #nf LEN = 400
FIELD nf, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
DO
a = a + 1
GET nf, a
IF RTRIM$(Dateiname$) = Datei$ THEN LSET Attribut$ = Attr$: PUT nf, a: EXIT SUB
LOOP UNTIL LText$(Dateiname$, 1) = ""

END SUB

SUB SetCombo (Index, Wert$)

PUT cmbFile, Index, Wert$

END SUB

SUB SetDrawMode (modus AS INTEGER)
SHARED box, rsv

'Åbersetzt den Toolbar-Modus in ein Draw-Modus
SELECT CASE modus
CASE 7: rsv = 10

CASE 5: rsv = 5
CASE 7: rsv = 8
CASE 1: rsv = 2: box = 0
CASE 1: rsv = 3: box = 1
CASE 2: rsv = 5
CASE 3: rsv = 4
CASE 4: rsv = 6
CASE 5: rsv = 1

END SELECT


END SUB

SUB Setze (Satz, Feld, Wert AS SINGLE)

Datensatz& = ((Satz - 1) * 6 + Feld + 1) * 4 - 3

PUT 2, Datensatz&, Wert

END SUB

SUB Showactiv (i)
IF i = 0 THEN ShowAlls: EXIT SUB
SHARED m.1, m.2, m.3, m.4

SELECT CASE CHR$(Befehle(i, 0))
CASE "p", "m"
    PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs
CASE "l", "r", "b"
    PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs: PUT (Befehle(i, 3) + xplus, Befehle(i, 4) + yplus), crs
CASE "k", "x"
IF Befehle(i, 0) = ASC("x") THEN i = i - 1

IF Befehle(i, 5) < 1 THEN rx = Befehle(i, 3): ry = Befehle(i, 5) * rx
IF Befehle(i, 5) >= 1 THEN ry = Befehle(i, 3): rx = ry / Befehle(i, 5)


PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs
PUT (Befehle(i, 1) + rx + xplus, Befehle(i, 2) + ry + yplus), crs
PUT (Befehle(i, 1) - rx + xplus, Befehle(i, 2) - ry + yplus), crs

END SELECT

END SUB

SUB ShowAlls
SHARED m.1, m.2, m.3, m.4
i = 0
DO: i = i + 1
SELECT CASE CHR$(Befehle(i, 0))
CASE "p", "m":
IF Befehle(i, 1) + xplus >= m.1 AND Befehle(i, 1) + xplus <= m.3 AND Befehle(i, 2) + yplus >= m.2 AND Befehle(i, 2) + yplus <= m.4 THEN
    PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs
END IF
CASE "l", "r", "b"
IF Befehle(i, 1) + xplus >= m.1 AND Befehle(i, 1) + xplus <= m.3 AND Befehle(i, 2) + yplus >= m.2 AND Befehle(i, 2) + yplus <= m.4 AND Befehle(i, 3) + xplus >= m.1 AND Befehle(i, 3) + xplus <= m.3 AND Befehle(i, 4) + yplus >= m.2 AND Befehle(i, 4) + yplus <= m.4 THEN
    PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs: PUT (Befehle(i, 3) + xplus, Befehle(i, 4) + yplus), crs
END IF
CASE "k", "x"
IF Befehle(i, 5) < 1 THEN rx = Befehle(i, 3): ry = Befehle(i, 5) * rx
IF Befehle(i, 5) >= 1 THEN ry = Befehle(i, 3): rx = ry / Befehle(i, 5)

IF Befehle(i, 1) - rx + xplus >= m.1 AND Befehle(i, 1) + rx + xplus <= m.3 AND Befehle(i, 2) - ry + yplus >= m.2 AND Befehle(i, 2) + ry + yplus <= m.4 THEN

PUT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), crs
PUT (Befehle(i, 1) + rx + xplus, Befehle(i, 2) + ry + yplus), crs
PUT (Befehle(i, 1) - rx + xplus, Befehle(i, 2) - ry + yplus), crs

IF Befehle(i, 0) = ASC("x") THEN i = i + 1

END IF

END SELECT
LOOP WHILE Befehle(i, 0) <> 0
END SUB

SUB SLine (x1, y1, x2, y2)

IF x2 < x1 THEN xmin = -1 ELSE xmin = 1
IF y2 < y1 THEN ymin = -1 ELSE ymin = 1


hseite = INT(x1 - x2)
IF hseite < 0 THEN hseite = -hseite
vseite = INT(y1 - y2)
IF vseite < 0 THEN vseite = -vseite

IF vseite >= hseite THEN
IF vseite = 0 THEN EXIT SUB
ds = hseite / vseite
DO
a = a + ds
B = B + 1

PUT (x1 + a * xmin, y1 + B * ymin), Punkt, XOR

LOOP UNTIL B > vseite

ELSEIF hseite > vseite THEN
IF hseite = 0 THEN EXIT SUB
ds = vseite / hseite
DO
a = a + 1
B = B + ds

PUT (x1 + a * xmin, y1 + B * ymin), Punkt, XOR
LOOP UNTIL a >= hseite

END IF

END SUB

SUB Spiegeln (d AS INTEGER) 'd=Direction: 0=Horizontal,1=Vertikal
SHARED m.1, m.2, m.3, m.4

i = 0
DO
i = NextOne(1)
    SELECT CASE CHR$(Befehle(i, 0))
        CASE "p", "m", "k"

                IF d = 1 THEN
                    Setze i, 1, -(Befehle(i, 1) - (m.1 - xplus)) + (m.3 - xplus)

                ELSE
                    Setze i, 2, -(Befehle(i, 2) - (m.2 - yplus)) + (m.4 - yplus)

                END IF

        CASE "x"

                IF d = 1 THEN
                    Setze i, 1, -(Befehle(i, 1) - (m.1 - xplus)) + (m.3 - xplus)

                ELSE
                    Setze i, 2, -(Befehle(i, 2) - (m.2 - yplus)) + (m.4 - yplus)

                END IF

                i = i + 1

        CASE "l", "r", "b"

                IF d = 1 THEN
                    Setze i, 1, -(Befehle(i, 1) - (m.1 - xplus)) + (m.3 - xplus)
                    Setze i, 3, -(Befehle(i, 3) - (m.1 - xplus)) + (m.3 - xplus)

                ELSE
                    Setze i, 2, -(Befehle(i, 2) - (m.2 - yplus)) + (m.4 - yplus)
                    Setze i, 4, -(Befehle(i, 4) - (m.2 - yplus)) + (m.4 - yplus)

                END IF

    END SELECT
LOOP UNTIL i = 0

END SUB

SUB StaticDraw (Figur AS SINGLE)
'ON ERROR GOTO 0
IF VisualDraw = aus THEN EXIT SUB

SHARED px, py, x, y
DIM vseite AS INTEGER, hseite AS INTEGER

SELECT CASE Figur
CASE 5, 2 '"l", "L"
SLine px, py, x, y

CASE 3 '"k"
rx = px - x
IF rx < 0 THEN rx = rx * -1
ry = py - y
IF ry < 0 THEN ry = ry * -1
IF rx <> 0 THEN asp = ry / rx ELSE asp = 1
r = rx
SCircle px, py, r, asp

CASE 1 '"r"
SLine px, py, x, py
SLine x, py, x, y
SLine x, y, px, y
SLine px, y, px, py + 1

END SELECT
END SUB

SUB StoppUhrBeep (Uhr)
SELECT CASE Uhr
CASE 1: TimeLock (1)
CASE 2: SCREEN , , , 1: COLOR , 0
a$ = "": DO: a$ = INKEY$: LOOP WHILE a$ = ""
SCREEN , , , 0: COLOR Farbe, Hintergrundfarbe
END SELECT
END SUB

SUB Symbolmake (Art)
Nr = 0: max.x = 0: min.x = 0: max.y = 0: min.y = 0
xtoo = 0: minyet = 1
DO
Nr = Nr + 1
SELECT CASE CHR$(Befehle(Nr, 0))
CASE "r", "b", "l"
        IF Befehle(Nr, 3) > max.x THEN max.x = Befehle(Nr, 3)
        IF Befehle(Nr, 1) < min.x OR minyet = 1 THEN min.x = Befehle(Nr, 1)
        IF Befehle(Nr, 4) > max.y THEN max.y = Befehle(Nr, 4)
        IF Befehle(Nr, 2) < min.y OR minyet = 1 THEN min.y = Befehle(Nr, 2)

CASE "p", "m"
        IF Befehle(Nr, 1) > max.x THEN max.x = Befehle(Nr, 1)
        IF Befehle(Nr, 1) < min.x OR minyet = 1 THEN min.x = Befehle(Nr, 1)
        IF Befehle(Nr, 2) > max.y THEN max.y = Befehle(Nr, 2)
        IF Befehle(Nr, 2) < min.y OR minyet = 1 THEN min.y = Befehle(Nr, 2)

CASE "x", "k"
    IF xtoo = 1 AND CHR$(Befehle(Nr, 0)) = "x" THEN
        xtoo = 0
    ELSE
        IF CHR$(Befehle(Nr, 0)) = "x" THEN xtoo = 1
        IF Befehle(Nr, 1) + Befehle(Nr, 3) > max.x THEN max.x = Befehle(Nr, 1) + Befehle(Nr, 3)
        IF Befehle(Nr, 1) - Befehle(Nr, 3) < min.x OR minyet = 1 THEN min.x = Befehle(Nr, 1) - Befehle(Nr, 3)
        IF Befehle(Nr, 2) + Befehle(Nr, 3) > max.y THEN max.y = Befehle(Nr, 2) + Befehle(Nr, 3)
        IF Befehle(Nr, 2) - Befehle(Nr, 3) < min.y OR minyet = 1 THEN min.y = Befehle(Nr, 2) - Befehle(Nr, 3)
    END IF
END SELECT
minyet = 0
LOOP UNTIL Befehle(Nr, 0) = 0

SELECT CASE Art
CASE 1: wa = 40: wb = 40
CASE 2
wa = (max.x - min.x) * 2
wb = (max.y - min.y) * 2
CASE 3
wa = (max.x - min.x) / 2
wb = (max.y - min.y) / 2
END SELECT

zerra = wa / (max.x - min.x)
zerrb = wb / (max.y - min.y)


IF zerra < zerrb THEN zerrb = zerra ELSE zerra = zerrb

IF zerra >= 1 AND Art = 1 THEN GOTO 801

IF Art = 1 THEN
min.x = min.x * zerra
min.y = min.y * zerrb
ELSE min.x = 0: min.y = 0
END IF

i = 0
DO
i = i + 1
SELECT CASE CHR$(Befehle(i, 0))
CASE "p", "m": Setze i, 1, Befehle(i, 1) * zerra - min.x: Setze i, 2, Befehle(i, 2) * zerrb - min.y
CASE "l", "b", "r"
Setze i, 1, Befehle(i, 1) * zerra - min.x: Setze i, 3, Befehle(i, 3) * zerra - min.x
Setze i, 2, Befehle(i, 2) * zerrb - min.y: Setze i, 4, Befehle(i, 4) * zerrb - min.y
CASE "k", "x"
Setze i, 1, Befehle(i, 1) * zerra - min.x: Setze i, 2, Befehle(i, 2) * zerrb - min.y
Setze i, 3, Befehle(i, 3) * zerra
'Befehle(i, 5, Befehle(i, 5) * zerrb / zerra
IF Befehle(i, 0) = ASC("x") THEN i = i + 1

END SELECT

LOOP WHILE Befehle(i, 0) <> 0
801
IF Art = 1 THEN
xplus = 300
yplus = 155
END IF
CLW
Zeichnen
MShows

END SUB

SUB Textausgabe
SHARED x, y
PCOPY 0, 1
Register 5, 40, 12, 75, "Textausgabe"
Echo ""
Echo "Geben Sie den auszugebenden Text ein:"
Echo ""
a$ = InputBox(0, 0, "", 0, 0)

PCOPY 1, 0
IF keyb <> "esc" THEN FontShow x, y, a$, 1

END SUB

FUNCTION TextBox$ (Zeilen, Spalten, Text$, Break)
'Break heisst hier: Hier wird der Text gebrochen (Zeilenumbruch)

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak + 1

IF Zeilen > 24 THEN Zeilen = 24

IF Spalten + LEN(Text$) > Right AND Break = 0 THEN Break = Right - Spalten
IF Break = 0 THEN Break = LEN(Text$)
IF Break = 0 THEN Spalten = Spalten - 1: Break = 1


a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + Break
EndZeile = Zeilen + INT(LEN(Text$) / Break + 1)
IF Break = LEN(Text$) THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

Objekt1$ = FrameWindow(a, B, c, d, 0)
Clearase (Objekt1$)
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe

ecr = Break * -1 + 1
FOR i = 1 TO INT(LEN(Text$) / Break) + 1
LOCATE Zeilen + i - 1, Spalten
ecr = ecr + Break
PRINT MID$(Text$, ecr, Break)
NEXT i
ecr = 0
TextBox$ = ""

TextScale = EndZeile + 1
END FUNCTION

SUB TimeLock (Status)
STATIC on.too
STATIC tm
Radius = 14 * 2
Farbe1 = 12
Farbe2 = 9
x = 319 - Radius
y = 174 - Radius

SELECT CASE Status
CASE 1:

LINE (x + Radius - Radius, y + Radius - Radius)-(x + Radius + Radius, y + Radius + Radius), 0, BF
IF tm = 1 THEN tm = 0 ELSE tm = 1

IF tm = 0 THEN
LINE (x + Radius / 3 * 2, y + Radius / 3 * 2)-(x + Radius / 3 * 4, y + Radius / 3 * 4), Farbe1
LINE (x + Radius / 3 * 2, y + Radius / 3 * 4)-(x + Radius / 3 * 4, y + Radius / 3 * 2), Farbe1
CIRCLE (x + Radius, y + Radius), Radius / 2, Farbe1, , , .9
PAINT (x + Radius + 1, y + Radius), Farbe1
PAINT (x + Radius - 1, y + Radius), Farbe1
END IF


IF tm = 1 THEN
LINE (x + Radius / 3 * 2, y + Radius / 3 * 2)-(x + Radius / 3 * 4, y + Radius / 3 * 4), Farbe2
LINE (x + Radius / 3 * 2, y + Radius / 3 * 4)-(x + Radius / 3 * 4, y + Radius / 3 * 2), Farbe2
CIRCLE (x + Radius, y + Radius), Radius / 2, Farbe2, , , .9
PAINT (x + Radius, y + Radius + 1), Farbe2
PAINT (x + Radius, y + Radius - 1), Farbe2
END IF
CASE 0: LINE (x + Radius - Radius, y + Radius - Radius)-(x + Radius + Radius, y + Radius + Radius), 0, BF
END SELECT
END SUB

FUNCTION Trim$ (Variable$, Trennzeichen$, a)

mcr = 0
gef = 0
FOR i = 1 TO LEN(Variable$)
mcr = mcr + 1
a$ = MID$(Variable$, mcr)
IF LEFT$(a$, 1) = Trennzeichen$ THEN gef = gef + 1
IF gef = a - 1 THEN EXIT FOR
NEXT
IF LEFT$(a$, 1) = Trennzeichen$ THEN a$ = MID$(a$, 2) ELSE a$ = a$

mcr = 0
FOR i = 1 TO LEN(a$)
mcr = mcr + 1
IF MID$(a$, mcr, 1) = Trennzeichen$ THEN a$ = LEFT$(a$, mcr - 1): EXIT FOR
NEXT

Trim$ = a$

END FUNCTION

SUB wartebewegung


 GetMaus
 x% = mausx%: y% = mausy%: k% = mausk%

 DO
  GetMaus
  taste$ = INKEY$
  IF taste$ = "l" THEN EXIT SUB
 LOOP UNTIL x% <> mausx% OR y% <> mausy% OR k% <> mausk%
END SUB

SUB warteknopflos


 WHILE mausk%
  GetMaus
 WEND

END SUB

SUB Zeichnen

SHARED px, py, rsv, m.1, NotGK, Befehl

DIM Bef(5) AS SINGLE, Bef2(5) AS SINGLE, i AS INTEGER


Neu:
SEEK 2, 1
IF rsv = 3 THEN Cursor px, py

i = 0
DO
i = i + 1
IF NotGK = 0 THEN
SELECT CASE INKEY$
CASE CHR$(0) + "M": xplus = xplus - 10: px = px - 10: a = a - 10: CLW: GOTO Neu
CASE CHR$(0) + "K": xplus = xplus + 10: px = px + 10: a = a + 10: CLW: GOTO Neu
CASE CHR$(0) + "H": yplus = yplus + 10: py = py + 10: B = B + 10: CLW: GOTO Neu
CASE CHR$(0) + "P": yplus = yplus - 10: py = py - 10: B = B - 10: CLW: GOTO Neu
END SELECT
END IF

FOR Nr% = 0 TO 5
GET 2, , Bef(Nr%)
NEXT

SELECT CASE Bef(0)
CASE 112: PSET (Bef(1) + xplus, Bef(2) + yplus), Bef(3)
CASE 109: IF Painticon = 0 THEN PAINT (Bef(1) + xplus, Bef(2) + yplus), Bef(3), Bef(4) ELSE Paintic Bef(1) + xplus, Bef(2) + yplus, Bef(3)
CASE 108: LINE (Bef(1) + xplus, Bef(2) + yplus)-(Bef(3) + xplus, Bef(4) + yplus), Bef(5)
CASE 114: LINE (Bef(1) + xplus, Bef(2) + yplus)-(Bef(3) + xplus, Bef(4) + yplus), Bef(5), B
CASE 98: LINE (Bef(1) + xplus, Bef(2) + yplus)-(Bef(3) + xplus, Bef(4) + yplus), Bef(5), BF
CASE 107: CIRCLE (Bef(1) + xplus, Bef(2) + yplus), Bef(3), Bef(4), , , Bef(5)
CASE 120: CIRCLE (Bef(1) + xplus, Bef(2) + yplus), Bef(3), Bef(4), Befehle(i + 1, 1), Befehle(i + 1, 2), Bef(5)
          i = i + 1
END SELECT

LOOP WHILE Bef(0) <> 0
Befehl = i - 1


IF m.1 <> 0 THEN Markieren 0, 0, 0, 0: NextActiv (3)

IF cross = an THEN
COLOR Farbe
LINE (xplus, 0)-(xplus, 349)
LINE (0, yplus)-(639, yplus)

END IF

DrawToolBar
END SUB

