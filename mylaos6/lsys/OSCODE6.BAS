'Hier legst du die Werte fÅr die maximale Anzahl anzeigbarer Menus,
'Symbole und ListeneintrÑgen fest. Du sparst Speicher, je tiefer die Werte
'sind:

CONST MaxListItems = 50, MaxIcons = 150, MaxMenus = 11, MaxControls = 11
CONST Programmdatei = "oscode6" 'Die Programmdatei OHNE Dateiendung!

'Wichtig: Schreibe dein Programm in die Main-Sub.
'         (In QB: Ansicht -> Subs -> Main)


'LaOS Version 6.00 Copyright (C) Urs Langmeier, New BCO

'<<<LaOSCode 6>>>

'Entwickelt unter QBASIC von Urs Langmeier
'Idee & Programmierung: Urs Langmeier

'Alle Urheber- und Leistungsschutzrechte vorbehalten. Der Code darf nicht un-
'erlaubt kopiert oder verkauft werden.

'LastChange: 27. Juni 2002

'Schnelles Bereich-Kopieren bei wiederholten Symbolen.



DECLARE SUB MsgBeep ()
DECLARE SUB Main ()
DECLARE SUB Info (Ctrl$, InfoText$)
DECLARE FUNCTION ListBox$ (z!, s!, zu!, su!)
DECLARE FUNCTION SDate$ (InDate$)
DECLARE FUNCTION Format$ (FormatString AS STRING)
DECLARE SUB PartFormat (Text$, Setting$)
DECLARE SUB MenuBox (Zeilen!, Spalten!, Caption$)
DECLARE SUB SetAttr (aDatei$, Attr$)
DECLARE FUNCTION RegItem$ (Nenner$)
DECLARE FUNCTION LinkIt$ ()
DECLARE FUNCTION Drivelist$ (Nr!)
DECLARE SUB Switchdrive (lw$)
DECLARE FUNCTION FileChoose$ (Art1$)
DECLARE FUNCTION GetFile$ (Sym$, Art1$)
DECLARE FUNCTION DosFile$ (Kennung$, InOrdner$)
DECLARE SUB ExitBox (x!, y!)
DECLARE FUNCTION exist! (p$)
DECLARE SUB TextBox (Zeilen!, Spalten!, Text$, Break!)
DECLARE SUB Register (z!, s!, zu!, su!, Text$)
DECLARE SUB FrameWindow (a!, B!, c!, d!, del!)
DECLARE SUB Echo (Text$)
DECLARE SUB GetInp ()
DECLARE SUB Control (Zeilen!, Spalten!, Text$)
DECLARE SUB CLW ()
DECLARE FUNCTION Gerade! (Zahl!)
DECLARE FUNCTION IsExist! (Datei$, InOrdner$)
DECLARE FUNCTION IsEmptyBox! (Ordner$)
DECLARE FUNCTION IsExistBox! (Box$)
DECLARE FUNCTION Trim$ (Variable$, Trennzeichen$, a!)
DECLARE FUNCTION MenuInt$ (Item$, Trennzeichen$)
DECLARE SUB Dialog (Caption$)
DECLARE SUB LoadSymbol (name$, Kennung$)
DECLARE SUB ShowSymbols ()
DECLARE SUB Menuact (Menu$)
DECLARE SUB TimeLock (Status!)
DECLARE SUB StoppUhrBeep (Uhr!)
DECLARE SUB Symbact (Symbol$)
DECLARE FUNCTION Ltext$ (Text$, ab!)
DECLARE SUB ShowInfo ()
DECLARE FUNCTION GetKey$ ()
DECLARE SUB ClearControls (Connect!)
DECLARE SUB NextControl ()
DECLARE SUB LastControl ()
DECLARE SUB Activate (Ctrl$)
DECLARE FUNCTION InputBox$ (Zeilen!, Spalten!, Text$, Break!, MaxDown!)

SCREEN 9
VIEW PRINT 1 TO 25
'    -> 640 x 350 Grafik
'    ˛ 80 x 25 oder 80 x 43 Textformat, 8 x 14 oder 8 x 8 Zeichenfeldgrî·e
'    ˛ Zuweisung von 16 Farben zu 4 Attributen (64 KB Adapterspeicher) oder
'      64 Farben zu 16 Attributen (mehr als 64 KB Adapterspeicher)
'    ˛ Falls 64KB EGA Adapterspeicher, 1 Bildschirmspeicherseite (0);
'      ansonsten 2 Seiten (0-1)

DIM SHARED Hintergrund(456) AS LONG
DIM SHARED StoppUhr(1 TO 4) AS SINGLE '1 ist Reserviert!
DIM SHARED AlteZeit(1 TO 4) AS LONG
DIM SHARED TextScale AS INTEGER, TextBreak AS INTEGER
DIM SHARED Activ AS STRING
DIM SHARED ControlList(MaxControls) AS STRING, ControlNr AS INTEGER
DIM SHARED EBox.left AS INTEGER, EBox.top AS INTEGER
DIM SHARED Left AS INTEGER, Top AS INTEGER, Right AS INTEGER, Down AS INTEGER
DIM SHARED NextBreak AS INTEGER
DIM SHARED QuickInfos(MaxControls) AS STRING
DIM SHARED ControlConnect AS INTEGER 'Zur Unterscheidung der 3 Eingabecontrols
DIM SHARED SymbolList(MaxIcons) AS STRING, SymbolNr AS INTEGER
DIM SHARED Symb.left AS INTEGER, Symb.top AS INTEGER, Symb.total AS INTEGER
DIM SHARED Symb.ipp AS INTEGER 'IPP=Icons par page (misst die letzte volle Seite)
DIM SHARED Symb.count 'ZÑhlt die Symbole in der aktuellen Seite
DIM SHARED DimY AS LONG, DimNow AS INTEGER, Symb.pnr'Nr. in der aktuellen Seite
DIM SHARED MenuList(MaxMenus) AS STRING, MenuNr AS INTEGER
DIM SHARED Focus AS STRING, keyb AS STRING, Change(6) AS INTEGER
DIM SHARED errorlevel AS INTEGER, Choice AS INTEGER
DIM SHARED Liste(1 TO MaxListItems) AS STRING
DIM SHARED ActivItem AS STRING, DriveName AS STRING
DIM SHARED Bef2(5) AS SINGLE

CONST Default = 0, True = -1, False = 0

FesteEinstellungen:
WIDTH 80, 25

Einstellungen:
DIM SHARED Fensterfarbe AS INTEGER, Randfarbe AS INTEGER, Schattenfarbe AS INTEGER, Controlfarbe AS INTEGER
DIM SHARED Symbolactiv AS INTEGER, SymbactStil AS INTEGER, SymbolFrame AS INTEGER, SymbolColor AS INTEGER
DIM SHARED Hintergrundfarbe  AS INTEGER, Farbe AS INTEGER, IconVerticalSpace AS INTEGER
DIM SHARED InfoTime AS INTEGER, InfoColor  AS INTEGER, InfoFrame, Datumsformat AS INTEGER

CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
GET 1, 1, Schema$

DIM M(1 TO 15)

IF Schema$ <> "" THEN

IF LOF(1) <> 0 THEN
CLOSE 1: OPEN "c:\laos\" + Schema$ + ".scl" FOR INPUT AS 1: LINE INPUT #1, a$
FOR i = 1 TO 15
INPUT #1, M(i)
NEXT
ELSE M(10) = 15: M(1) = 14
END IF

ELSE M(10) = 15: M(1) = 14
END IF

Fensterfarbe = M(1)'Fensterfarben
Randfarbe = M(2)
Schattenfarbe = M(3)
Controlfarbe = M(4)  'SchaltflÑchenfarbe
Symbolactiv = M(5) 'Umrahmungsfarbe des aktiven Symbols
SymbactStil = M(6) 'Umrahmungs-Stil des aktivierten Symbols (-1=voll)

SymbolFrame = M(7) 'Farbe des Symbolrahmens
SymbolColor = M(8) 'Farbe des Symbolhintergrundes

Hintergrundfarbe = M(9) 'Bildschirmfarben
Farbe = M(10) '= Vordergrundfarbe

IconVerticalSpace = M(11) 'Setzt den Vertikalen Standard-Abstand der Symbole
                            'Auf Null gesetzt ist der Abstand genÅgend.
InfoTime = M(12)'Setzt die zu verstreichende Zeit in Sekunden, bis die Quick-
                  'Info erscheint. (0=Ausgeschaltet)
InfoColor = M(13) 'Setzt die Farbe, in der der Quick-Info-Text erscheint.
InfoFrame = M(14) 'Setzt die Farbe des Quick-Info-Rahmens.

Datumsformat = M(15)

ERASE M

CONST Fe1 = 6, Fe2 = 4, Fe3 = 75 'Startfensterkoordinaten
DIM SHARED Fe4
IF IconVerticalSpace = 0 THEN Fe4 = 20 ELSE Fe4 = 21

StoppUhr(1) = 60 '1.Stoppuhr: Bildschirmschoner


VorStart:
CLOSE
VIEW
COLOR Farbe, Hintergrundfarbe
Switchdrive "c"

Code = 0 'Legt die Ereignisverfolgung fÅr die Alt-Taste fest
FOR i = 15 TO 19
KEY i, CHR$(Code) + CHR$(56)
ON KEY(i) GOSUB AltKey
Code = Code + 32
NEXT i
Code = 0: i = 0

Programm:
'Schreiben Sie hier Ihren persînlichen Code:
ON ERROR GOTO fehlernext
CLW

CALL Main



SYSTEM
TimeEreignis:
TIMER OFF
ShowInfo
RETURN


AltKey:
IF MenuList(1) <> "" THEN Activate CHR$(0): ControlConnect = 1: Activate MenuList(1): MenuNr = 1
RETURN

fehlernext:
errorlevel = 1
RESUME NEXT

'Diese SUB wird fÅr internen Gebrauch verwendet.
SUB Activate (Ctrl$)
SELECT CASE ControlConnect
CASE 0 'Controls
i = 0
DO
i = i + 1
a$ = ControlList(i)
Activ = Ctrl$
IF a$ = "" THEN EXIT SUB
'ControlNr = i
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1 'Menus
i = 0
Activ = Ctrl$
DO
i = i + 1
IF i > MaxMenus THEN EXIT DO
a$ = MenuList(i)
IF a$ = "" THEN EXIT DO
Menuact a$
LOOP UNTIL a$ = ""



CASE 2 'Symbole
i = 0
Activ = Ctrl$
'i = Symb.total * Symb.page
DO
i = i + 1
a$ = SymbolList(i)
IF MID$(a$, 19) = Activ THEN Activ = a$
IF a$ = "" THEN EXIT DO
Symbact a$
LOOP UNTIL a$ = ""
END SELECT
END SUB

'ClearControls lîscht den Steuerelemente-Speicher
'
'Dies ist wichtig, wenn ein neuer Bildschirm aufgebaut wird. (-> CLW)
'
'Der Parameter 'Connect' bestimmt hier, welche Arten von Steuerelementen
'gelîscht werden sollen, wobei gilt:
'
'0 = Nur Buttons (Controls)
'1 = Nur MenÅs
'2 = Nur Symbole
'3 = Alle
'
'
SUB ClearControls (Connect)
SELECT CASE Connect
CASE 0: ControlNr = 0: ERASE ControlList: ERASE QuickInfos
CASE 1: MenuNr = 0: ERASE MenuList: ERASE QuickInfos
CASE 2: SymbolNr = 0: ERASE SymbolList: Symb.left = 0: Symb.top = 0: Symb.total = 0
CASE 3 'Alle lîschen
ControlNr = 0: ERASE ControlList: ERASE QuickInfos
MenuNr = 0: ERASE MenuList: ERASE QuickInfos
SymbolNr = 0: ERASE SymbolList: Symb.left = 0: Symb.top = 0: Symb.total = 0
Focus = ""
END SELECT

END SUB

'CLW - CLear Window
'
'Lîscht den gesamten Bildschirm, und zeichnet den LaOS-typischen Rahmen drum.
'
'Bitte nach jedem CLW noch den Befehl ClearControls 3 verwenden, um die
'Steuerelemente wieder aus dem Speicher zu entfernen.
'
SUB CLW
'Fenster:
LINE (2, 2)-(637, 347), 0, BF
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

'Control erstellt einen Command-Button (hier Control-Button genannt).
'
'Zeilen: Die Zeile
'Spalten: Die Spalte
'Text: Der Text, der den DrÅckknopf beschreibt.
'
'Bsp:
'
'Control 0, 0, "Beenden" + Format("Beendet das Programm")
'
'Hier wurde noch das QuickInfo dazu als Format-Parameter Åbergeben.
'
SUB Control (Zeilen, Spalten, Text$)
'Als Formatparameter kann das QuickInfo Åbergeben werden.
PartFormat Text$, fmt$


IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak
IF Zeilen > Down THEN Zeilen = Top: Spalten = NextBreak + 1: TextScale = Top: TextBreak = NextBreak

IF LEN(Text$) + Spalten + 2 > NextBreak THEN NextBreak = LEN(Text$) + Spalten + 2


a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + LEN(Text$)
EndZeile = Zeilen + INT(LEN(Text$) / 80 + 1)
IF LEN(Text$) = 80 THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

LOCATE Zeilen, Spalten: COLOR Farbe: PRINT Text$
LINE (a, B)-(c, d), Controlfarbe, B

'Schatten: (Wird angezeigt, falls das Control aktiviert ist.)
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

IF fmt$ <> "" THEN Info Text$, fmt$

2 Activ = Text$: ControlConnect = 0: TextScale = Zeilen + 2
END SUB

'Zeigt eine Message-Box an.
'
'Bsp:
'
'Dialog "Die Datei wurde erfolgreich erstellt!"
'
SUB Dialog (Caption$)
'Formatoptionen: gleich wie echo. Standard: "c"=zentriert
PartFormat Caption$, fmt$

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
Groesse = 4 + INT(((PMAP(x2!, 0) - PMAP(x1!, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2!, 1) - PMAP(y1!, 1) + 1)
DIM Background(Groesse) AS INTEGER


GET (x1!, y1!)-(x2!, y2!), Background

FrameWindow x1!, y1!, x2! - 1, y2! - 1, 1

a% = TextBreak: B% = TextScale: c% = Right: d% = Down
    TextBreak = Spalten
    TextScale = Zeilen
    Right = EndSpalten
    Down = EndZeilen

        IF fmt$ = "" THEN fmt$ = "c"
        Echo Caption$ + Format(fmt$)

Zeilen = TextScale

TextBox Zeilen + 1, 33, "      OK", 14
TextBreak = a%: TextScale = B%: Right = c%: Down = d%

DO
B$ = a$
a$ = GetKey
LOOP UNTIL a$ = "esc" OR a$ = "enter"
IF a$ = "esc" THEN keyb = "esc" ELSE keyb = B$
PUT (x1!, y1!), Background, PSET: x1! = 0: y1! = 0: EXIT SUB


END SUB

'Gibt die DOS-Datei eines LaOS-Files zurÅck.
'
'Bsp:
'
'OPEN DosFile("LaOS Draw","Zubehîr") FOR INPUT AS 1
'
FUNCTION DosFile$ (Kennung$, InOrdner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF Kennung$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN DosFile = RTRIM$(Dateiname$): CLOSE 1: EXIT FUNCTION
LOOP UNTIL Ltext$(Dateiname$, 1) = ""
errorlevel = 1
CLOSE 1

END FUNCTION

'Wird fÅr internen Gebrauch verwendet.
FUNCTION Drivelist$ (Nr)
STATIC LastNr
IF Nr < 1 THEN Nr = LastNr + 1

CLOSE 1: OPEN "c:\laos\drvlist.bnk" FOR RANDOM AS 1
GET 1, Nr, a$
IF a$ = "" THEN GET 1, 1, a$: Nr = 1

LastNr = Nr
Drivelist = LEFT$(a$, 1)
CLOSE 1
END FUNCTION

'Echo gibt einen Text unter BerÅcksichtigung des Zeilenumbruches aus. Es
'kînnen folgende Formatparameter Åbergeben werden:
'
'"l", "c", "r": LinksbÅndig, zentriert, rechtsbÅndig
'
'Bsp: (gibt einen zentrierten Text zurÅck)
'
'Echo "Hallo und herzlich willkommen in LaOS" + Format("c")
'
SUB Echo (Txt$)
PartFormat Txt$, fmt$

'Format: "l", "c", "r": LinksbÅndig, zentriert, rechtsbÅndig

SELECT CASE fmt$
    CASE "l", "": Align% = 0
    CASE "c": Align% = 1
    CASE "r": Align% = 2
END SELECT

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF TextScale > Down THEN TextScale = Top

'10.11.1999: Echo mit Zeilenumbruch (C) Urs Langmeier.

Text$ = Txt$ + " "

DIM s AS INTEGER, z AS INTEGER, t AS INTEGER, brk AS INTEGER


s = 1
z = Right - TextBreak + 2'ZeilenlÑnge
dwn = 0

p1% = s
p2% = p1%
DO
    t = t + 1
        IF t = p1% THEN p2% = p1%'alten Retten
        p1% = INSTR(t, Text$, " ")
        IF p1% = 0 OR p1% = LEN(Text$) THEN
            IF p1% <= s + z - 1 THEN p2% = LEN(Text$): brk = -1
        END IF

        IF p1% > s + z - 1 OR brk THEN 'nehme alten, weil neuen Åber die Zeile ragt.
            'Zeile ausgeben:
            p1% = p2%
            IF TextScale + dwn > Down THEN EXIT DO
                IF p1% - s <= 0 THEN 'falls Wort zu lange (nicht in die Zeile passt:)

                    p1% = s + z - 1
                    a$ = MID$(Text$, s, p1% - s)
                    t = p1% - 1
                    s = t + 1

                ELSE

                    a$ = MID$(Text$, s, p1% - s)
                    t = p1% + 1
                    s = t
                END IF

            'Ausrichtung:
            SELECT CASE Align%
                CASE 0: x% = TextBreak
                CASE 1: x% = TextBreak + FIX((Right - TextBreak - LEN(a$) + 1) / 2)
                CASE 2: x% = Right - LEN(a$) + 1
            END SELECT

            LOCATE TextScale + dwn, x%
            PRINT a$;

            dwn = dwn + 1
        END IF
LOOP UNTIL t >= LEN(Text$)
TextScale = TextScale + dwn


END SUB

'PrÅft, ob die Åbergebene DOS-Datei existiert oder nicht. Gibt -1 oder 0
'zurÅck.
'
FUNCTION exist (Datei$)
f% = FREEFILE
ae% = errorlevel
errorlevel = 0
OPEN Datei$ FOR INPUT AS f%
IF errorlevel = 0 THEN exist = -1 ELSE exist = 0
CLOSE f%
errorlevel = ae%
END FUNCTION

'ExitBox zeigt das Kreuz an, das dem User zeigen soll, dass an dieser Stelle
'mit Escape einen Schritt zurÅck gesprungen werden kann.
'
'Wird ExitBox mit den Parametern -1, -1 aufgerufen, so wird sie immer unten
'rechts angezeigt. Mit positiven Parametern kann sie beliebig auf dem Bild-
'schirm positioniert werden.
'
'Bsp:
'
'ExitBox -1, -1
'
SUB ExitBox (x, y)
IF x < 0 THEN x = EBox.left
IF y < 0 THEN y = EBox.top
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

'Zeigt den Datei-ôffnen-Dialog an, wo der Benutzer eine Datei auswÑhlen kann.
'
'Art1$ - Die Datei-Art
'
'Bsp:
'
'OPEN FileChoose$("LDS-Zeichnung") FOR BINARY AS 1
'
FUNCTION FileChoose$ (Art1$)
'LÑsst den Benutzer eine Datei auswÑhlen. Art1$: Die Art-Spalte. Leer = alle.
301
Indrive = 0
SHELL "c:": CHDIR "\laos"
DriveName = ""
Register Fe2, Fe1, Fe4, Fe3, ""
ClearControls (2)
TimeLock 1
Datics = 0

'Laufwerke laden:
ON ERROR GOTO fehlernext
CLOSE 1: OPEN "drvlist.bnk" FOR RANDOM AS 1
nf = FREEFILE
lmf = nf + 1
CLOSE lmf: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS lmf

i = 0
DO
i = i + 1
errorlevel = 0

GET 1, i, LNam$
IF LNam$ = "" THEN EXIT DO
lw$ = LEFT$(LNam$, 1): OPEN lw$ + ":\nul" FOR BINARY AS nf: CLOSE nf
IF errorlevel = 0 THEN 'Laufwerk vorhanden:
IF MID$(LNam$, 4) <> DriveName$ THEN
OPEN lw$ + ":\laos\medname.inf" FOR RANDOM AS nf
GET nf, 1, B$
IF B$ = "" THEN B$ = MID$(LNam$, 4)
errorlevel = 0
CLOSE nf: OPEN lw$ + ":\laos\drvicon.lsb" FOR BINARY AS nf
Datics = Datics + 1

PUT lmf, , lw$

IF LOF(nf) = 0 THEN
LoadSymbol "drvicon", B$
ELSE
'Symbol des DatentrÑgers verwenden:
TimeLock 1
KILL "~otdric" + lw$ + ".lsb"
f = FREEFILE
OPEN "~otdric" + lw$ + ".lsb" FOR BINARY AS f
DIM g AS SINGLE
DIM Befehle(5) AS SINGLE
DO
FOR Nr = 0 TO 5
GET nf, , Befehle(Nr)
PUT f, , Befehle(Nr)
NEXT
LOOP WHILE Befehle(0) <> 0
CLOSE f
LoadSymbol "~otdric" + lw$, B$
END IF
CLOSE nf
LNam$ = ""
B$ = ""
END IF
END IF
LOOP
CLOSE lmf
TimeLock 0


'Ordner laden:
304
CLOSE 1: OPEN "ordner.bnk" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ <> "" THEN LoadSymbol "ordner", a$
LOOP UNTIL a$ = ""

ShowSymbols

307
GetInp
IF keyb = "esc" AND Indrive = 0 THEN EXIT FUNCTION
IF keyb = "esc" THEN GOTO 301

IF SymbolNr <= Datics THEN 'Laufwerk angewÑhlt:
Datics = 0
f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
GET f, SymbolNr, lw$: CLOSE f
Switchdrive lw$
ClearControls (2)
Register Fe2, Fe1, Fe4, Fe3, DriveName
Indrive = 1
GOTO 304
END IF

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

Register Fe2, Fe1, Fe4, Fe3, Focus
TimeLock 1
Az = TIMER
ClearControls (2)
DO
GET 1
IF RTRIM$(InOrdner$) = Focus AND RTRIM$(Art$) = Art1$ OR Art1$ = "" AND RTRIM$(InOrdner$) = Focus THEN
LoadSymbol Ltext(Symbol$, 1), RTRIM$(OsName$)
END IF
IF TIMER - Az > .5 THEN TimeLock (1): Az = TIMER
LOOP UNTIL Ltext(Dateiname$, 1) = ""

TimeLock (0)
ord$ = Focus

ShowSymbols

306
GetInp
IF keyb = "esc" AND Indrive = 0 THEN GOTO 301
IF keyb = "esc" THEN DriveName = "": ClearControls 2: Register Fe2, Fe1, Fe4, Fe3, DriveName: GOTO 304

ClearControls 2
FileChoose = DosFile(Focus, ord$)
END FUNCTION

'Wird fÅr internen Gebrauch verwendet.
FUNCTION Format$ (FormatString AS STRING)
    Format$ = CHR$(0) + FormatString
END FUNCTION

'Wird fÅr internen Gebrauch verwendet.
SUB Frame
'Frame kann ausgefÅhrt werden, um ein Rahmen um das Fenster zu machen,
'und dabei aber die Left- Top- usw.-Kordinaten zu behalten.
'einzig die TextScale- und TextBreak- Koordinaten wechseln nach oben links.
'-> Siehe auch die Sub "CLW".

FrameWindow 0, 0, 638, 348, 0

'EBox.left = 603: EBox.top = 321
'Left = 2: Top = 2: Right = 79: Down = 24
TextScale = 2: TextBreak = 2
END SUB

'Wird fÅr internen Gebrauch verwendet.
SUB FrameWindow (a, B, c, d, del)
'Erstellt ein Fenster, ohne dabei die Umgebungsvariablen zu verÑndern
'Wahlweise wird der Inhalt des Fensters gelîscht (del = 1), oder nicht.

'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B
IF del = 1 THEN LINE (a + 2, B + 2)-(c - 1, d - 1), 0, BF
'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe

END SUB

'prÅft, ob eine Zahl gerade ist oder nicht. Gibt -1 oder 0 zurÅck.
FUNCTION Gerade (Zahl)
IF Zahl / 2 - FIX(Zahl / 2) = 0 THEN Gerade = -1:  ELSE Gerade = 0
END FUNCTION

'Fragt den Benutzer nach den Dateinamensangaben ab,
'und erstellt diese Datei. GetFile gibt den DOS-Dateiname zurÅck.
'Beispiel:
'
'OPEN GetFile("sys","Zeichnung") FOR OUTPUT AS 1
'
FUNCTION GetFile$ (Sym$, Art1$)

'Laufwerke laden:
71
Indrive = 0
Switchdrive "c"
ERASE Liste
ON ERROR GOTO fehlernext
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
        Liste(p) = B$
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
Liste(p) = a$
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
ord$ = ListBox(8, 0, 0, 33)

IF keyb = "esc" AND Indrive = 1 THEN
GOTO 71
ELSEIF keyb = "esc" THEN EXIT FUNCTION
END IF

IF Choice <= othDrives THEN 'fremdes LW angewÑhlt
    f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
    GET f, Choice, lw$
    CLOSE f
    Switchdrive lw$
    p = 0
    Indrive = 1
    ERASE Liste
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
LOOP UNTIL keyb = "enter"


IF IsExist(nam$, ord$) = 1 THEN
Register 8, 5, 16, 75, "Datei existiert schon"
Echo ""
Echo "Dieser Dateiname existiert schon im angewÑhlten Ordner."
Echo "Soll diese Datei Åberschrieben werden, oder soll eine neue erstellt  werden?"
ClearControls 0
Control 14, 20, "     Neue Datei     "
Control 14, 42, "   öberschreiben   "
GetInp
IF Focus = "   öberschreiben   " THEN
errorlevel = 0
CLOSE 1: OPEN "\laos\wrt~.tmp" FOR OUTPUT AS 1
IF errorlevel = 1 THEN Dialog "Das Schreiben auf den DatentrÑger ist nicht mîglich. öberprÅfen Sie den     Schreibschutz.": GOTO 71
a$ = DosFile(nam$, ord$)
SetAttr a$, ""
IF RIGHT$(LinkIt, 6) = "runtmp" THEN Dialog "Datei muss erneut verschlÅsselt werden."
GetFile = a$: EXIT FUNCTION
END IF
IF Focus = "     Neue Datei     " THEN 200
END IF

'File îffnen:
CHDIR "\LAOS"
errorlevel = 0

IF Sym$ = "" THEN Sym$ = "sys"
CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
DO
a = a + 1
GET 1, a
LOOP UNTIL Ltext$(Dateiname$, 1) = ""

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
LSET link$ = Programmdatei 'ersetzen Sie diesen Eintrag durch Ihre Programmdatei. (ohne Suffix. Bsp: "studio")
LSET Datum$ = DATE$
LSET Zeit$ = TIME$
LSET FileInfo$ = inf$
PUT 1, a

ELSE Dialog "Das Schreiben auf den DatentrÑger ist nicht mîglich. öberprÅfen Sie den     Schreibschutz.": GOTO 71
END IF

CLOSE 1

GetFile = dat$

END FUNCTION

'GetInp Åbergibt dem Benutzer die Kontrolle Åber die aufgebauten Steuer-
'elemente, wie MenÅs, Control-Buttons und Symbole, nachdem diese mit den
'entsprechenden Prozeduren aufgebaut wurden. (->Control, ->MenuBox,
'                                             ->LoadSymbol)
'
SUB GetInp
DIM kbd AS STRING
'Aktiviere das Control mit dem Fokus:
SELECT CASE ControlConnect
CASE 0: Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
CASE 2: Activate SymbolList(SymbolNr)
END SELECT


DO
FOR i = 15 TO 19: KEY(i) ON: NEXT i
IF InfoTime > 0 THEN
TIMER ON
ON TIMER(InfoTime) GOSUB TimeEreignis
END IF
  FOR i = 1 TO 4
    AlteZeit(i) = TIMER
  NEXT i
WHILE INKEY$ = CHR$(13) OR INKEY$ = CHR$(27): WEND
kbd = ""
 WHILE kbd = ""
  kbd = INKEY$
   FOR i = 1 TO 4
     IF StoppUhr(i) > 0 THEN
       IF TIMER - AlteZeit(i) > StoppUhr(i) THEN StoppUhrBeep (i): AlteZeit(i) = TIMER
     END IF
   NEXT i
 WEND
TIMER OFF: FOR i = 15 TO 19: KEY(i) OFF: NEXT i

SELECT CASE kbd
CASE CHR$(0) + "G": DO: a$ = Activ: LastControl: LOOP UNTIL a$ = Activ OR INKEY$ <> ""
CASE CHR$(0) + "O": DO: a$ = Activ: NextControl: LOOP UNTIL a$ = Activ OR INKEY$ <> ""
CASE CHR$(0) + "T", CHR$(0) + ";": ShowInfo'Manueller Q-Info-Abruf mit Shift+F1
CASE CHR$(13): keyb = "enter": EXIT DO
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
CASE 2: IF SymbolList(1) = "" THEN ControlConnect = 0
END SELECT
NEXT i
SELECT CASE ControlConnect
CASE 0
ch = ch + 1
IF Change(ch) = 0 THEN ch = 0
IF ch <> 0 THEN ControlNr = Change(ch)
Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
CASE 2: Activate SymbolList(SymbolNr)
END SELECT
CASE CHR$(0) + "H", CHR$(0) + "K": LastControl
CASE CHR$(0) + "P", CHR$(0) + "M": NextControl
CASE CHR$(27), CHR$(8)
keyb = "esc": EXIT DO'Minimize
END SELECT
LOOP

'Focus setzen:
SELECT CASE ControlConnect
CASE 0: Focus = MID$(ControlList(ControlNr), 5)
CASE 1: Focus = MID$(MenuList(MenuNr), 5)
CASE 2: Focus = MID$(SymbolList(SymbolNr), 19)
END SELECT

END SUB

'Wartet, bis der Benutzer eine Taste drÅckt, und gibt diese Taste dann als
'Funktionsergebnis zurÅck.
'
FUNCTION GetKey$

  FOR i = 1 TO 4
    AlteZeit(i) = TIMER
  NEXT i

WHILE INKEY$ <> "": WEND

a$ = ""
 WHILE a$ = ""
  a$ = INKEY$
   FOR i = 1 TO 4
     IF StoppUhr(i) > 0 THEN
       IF TIMER - AlteZeit(i) > StoppUhr(i) THEN StoppUhrBeep (i): AlteZeit(i) = TIMER
     END IF
   NEXT i
 WEND


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

SELECT CASE ASC(a$)
CASE 27: a$ = "esc"
CASE 9: a$ = "tab"
CASE 13: a$ = "enter"
END SELECT

GetKey = a$
keyb = a$
END FUNCTION

'Wird fÅr internen Gebrauch verwendet.
SUB GraphicView (a, B, c, d)
'Diese Sub Ñndert die Umgebungsvariablen (Left, Top, usw.) fÅr einen bestimmt-
'en Fenster-Bereich. Die darauffolgenden Grafikausgaben erfolgen in diesem
'Bereich.
'LINE (a, B)-(c, d), Randfarbe, B



EBox.left = c - 31: EBox.top = d - 24
Left = INT(a / 8) + 2: Top = INT(B / 14) + 2: Right = INT((c + 8) / 8) - 1: Down = INT((d + 14) / 14) - 1
TextBreak = Left
TextScale = Top
END SUB

'Wird fÅr internen Gebrauch verwendet.
SUB Info (Ctrl$, InfoText$)
SHARED QuickInfos() AS STRING
i = 0
DO
i = i + 1
IF MID$(ControlList(i), 5) = Ctrl$ THEN EXIT DO
IF MID$(MenuList(i), 5) = Ctrl$ THEN EXIT DO
LOOP
QuickInfos(i) = InfoText$
END SUB

'Erstellt eine Box, worin der Benutzer einen Text eingeben kann.
'
'Die Parameter:
'
'Zeilen - Zeilenposition
'Spalten - Spaltenposition
'Text$ - Der Text, der bereits in der Textbox stehen soll
'Break - Die Anzahl Spalten, bis der Text umgebrochen werden soll
'MaxDown - Die Anzahl Zeilen, die verwendet werden dÅrfen
'
'Bsp:
'
'a$ = InputBox (5, 5, "Annelies", 12 , 1)
'
'   In diesem Beispiel kann der Benutzer nur 12 Zeichen eingeben, da MaxDown
'   auf 1 gesetzt ist, und Break auf 12.
'
'Tipp: Wenn du den Text$ auf CHR$(0) setzt, so werden statt den eingegebenen
'      Zeichen Sterne (*) angezeigt.
'
FUNCTION InputBox$ (Zeilen, Spalten, Text$, Break, MaxDown)
'Break=Zeilenumbruch | MaxDown=Maximale Zeilen

IF Text$ = CHR$(0) THEN HidePasswordChar = -1: Text$ = ""

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak + 1

IF Break = 0 OR Break > Right - Spalten THEN Break = Right - Spalten
IF MaxDown = 0 THEN MaxDown = 24 - Zeilen
TotalBreak = Break
Texta$ = Text$: a$ = ""
GOTO Deletefirst

lesen:
a$ = GetKey

IF a$ = "enter" THEN InputBox$ = Texta$: GOTO delcur
IF a$ = "up" OR a$ = "down" OR a$ = "left" OR a$ = "right" OR a$ = "tab" THEN InputBox$ = Texta$: GOTO delcur
IF a$ = "esc" THEN InputBox$ = Texta$: GOTO delcur
IF a$ = CHR$(8) THEN
IF LEN(Texta$) > 0 THEN Texta$ = LEFT$(Texta$, LEN(Texta$) - 1)
LINE (a, B)-(c + 1, d + 1), 0, BF
a$ = ""
END IF
IF dwn > MaxDown THEN a$ = "": nocur = 1

IF LEN(a$) = 1 THEN Texta$ = Texta$ + a$
Break = LEN(Texta$) + 1

Deletefirst:
Break = LEN(Texta$) + 1
dwn = INT(LEN(Texta$) / TotalBreak) + 1
IF Break > TotalBreak THEN Break = TotalBreak: nocur = 1
IF LEN(Texta$) < (INT(LEN(Texta$) / TotalBreak) + 1) * TotalBreak AND LEN(Texta$) > INT(LEN(Texta$) / TotalBreak) * TotalBreak THEN nocur = 0

a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + Break
EndZeile = Zeilen + INT(LEN(Texta$) / Break + 1)
IF dwn > MaxDown AND MaxDown > 1 THEN EndZeile = EndZeile - 1
IF Break = LEN(Texta$) THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

FrameWindow a, B, c, d, 1


ecr = TotalBreak * -1 + 1
FOR i = 1 TO INT(LEN(Texta$) / TotalBreak) + 1
LOCATE Zeilen + i - 1, Spalten
ecr = ecr + TotalBreak
IF HidePasswordChar THEN PRINT MID$(STRING$(LEN(Texta$), "*"), ecr, TotalBreak);  ELSE PRINT MID$(Texta$, ecr, TotalBreak);
NEXT i
ecr = 0

IF nocur = 0 THEN LINE (POS(0) * 8, CSRLIN * 14)-(POS(0) * 8 - 8, CSRLIN * 14 - 14), InfoColor, BF ELSE nocur = 0
GOTO lesen

delcur:
LINE (POS(0) * 8, CSRLIN * 14)-(POS(0) * 8 - 8, CSRLIN * 14 - 14), 0, BF

END FUNCTION

'PrÅft, ob die LaOS-Datei in einem Bestimmten Ordner existiert, und gibt falls
'ja -1 zurÅck, falls nein 0.
'
FUNCTION IsExist (Datei$, InOrdner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN IsExist = -1: EXIT FUNCTION
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = "" THEN IsExist = -1: EXIT FUNCTION
LOOP UNTIL Ltext$(Dateiname$, 1) = ""

IsExist = 0


END FUNCTION

'Wird nur fÅr interne Zwecke verwendet.
SUB LastControl
'Wird von GetInp verwendet.

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
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1
MenuNr = MenuNr - 1
IF MenuNr < 1 THEN MenuNr = 1
Activate MenuList(MenuNr)
CASE 2
Symb.pnr = Symb.pnr - 1
SymbolNr = SymbolNr - 1
IF SymbolNr < 1 THEN SymbolNr = 1: Symb.pnr = 1
IF Symb.pnr < 1 THEN

pagerows% = FIX((Down - Top - 4) / (IconVerticalSpace + 4)) + 1

i = SymbolNr
rows% = 0
y2 = VAL(MID$(SymbolList(i), 6, 5))
DO
    i = i - 1
    y = VAL(MID$(SymbolList(i), 6, 5))
    IF y < y2 THEN
        y2 = y
        rows% = rows% + 1
    END IF

LOOP UNTIL rows% >= pagerows%

DimNow = 1
SymbolNr.old = SymbolNr
SymbolNr = SymbolNr - ABS(i - SymbolNr)
IF SymbolNr < 0 THEN SymbolNr = 0
LINE (Left * 8 - 8, Top * 14 - 14)-(Right * 8, Down * 14), 0, BF
ShowSymbols
SymbolNr = SymbolNr.old
Symb.pnr = Symb.count
END IF
Activate SymbolList(SymbolNr)
END SELECT
END SUB

'FÅr DocServer-Anwendungen: LinkIt$ gibt den Dos-Dateinamen des Dokumentes
'zurÅck, welches im LaOS-DateimenÅ gestartet wurde, und nun geladen werden
'muss.
'
'Bsp:
'
'Dokumentdatei$ = LinkIt$
'IF Dokumentdatei$ <> "" THEN Lade Dokumentdatei$
'
FUNCTION LinkIt$
CLOSE 1: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 1
GET 1, 1, a$
LinkIt = a$
CLOSE 1
END FUNCTION

'Zeigt eine Listenbox an. Die EintrÑge mÅssen ins Datenfeld Liste eingetragen
'werden. Die Funktion gibt den ausgewÑhlten Listeneintrag zurÅck. Die globale
'Variable Choice erhÑlt ausserdem den Wert der Nummer des ausgewÑhlten Listen-
'eintrages.
'
'Bsp:
'
'ERASE Liste
'Liste(1) = "Kaffe trinken gehen"
'Liste(2) = "Arbeiten gehen"
'Liste(3) = "Schlafen gehen"
'
'a$ = ListBox(0, 0, 0, 0)
'
'IF Choice = 1 THEN GOTO trinkcafe
'IF Choice = 2 THEN GOTO arbeiten
'IF Choice = 3 THEN GOTO gehschlafen
'
FUNCTION ListBox$ (z, s, zu, su)

IF Liste(1) = "" THEN Liste(1) = " "
DIM Trenner(1 TO 5)


MaxItem% = UBOUND(Liste)

IF z = 0 THEN z = Top
IF s = 0 THEN s = Left
IF zu = 0 THEN zu = Down
IF su = 0 THEN su = Right

MaxLenth = su - s + 1
MaxDown = zu - z + 1

DO
i = i + 1
IF Liste(i) = "-" THEN Liste(i) = STRING$(MaxLenth, 196): tr = tr + 1: Trenner(tr) = Standort + i
LOOP UNTIL Liste(i) = ""


a = s * 8 - 11
B = z * 14 - 17
c = su * 8 + 1
d = zu * 14 + 1


FrameWindow a, B, c, d, 0

ItemPos = 1
ItemPage = 1

IF ActivItem <> "" THEN act$ = ActivItem: zrl = 1: GOTO aktivieren
weiter2:

DO

 Standort = ItemPage * MaxDown - MaxDown + 1
LOCATE z

 FOR i = 0 TO MaxDown - 1
  IF Standort + i <= MaxItem% THEN
  IF LEN(Liste(Standort + i)) <= MaxLenth THEN
  Eintrag$ = Liste(Standort + i) + SPACE$(MaxLenth - LEN(Liste(Standort + i)))
  ELSE
  Eintrag$ = LEFT$(Liste(Standort + i), MaxLenth)
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

IF Liste(ItemPos + 1) <> "" THEN

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
    LOOP UNTIL Liste(ItemPos) = ""
        ItemPos = ItemPos - 1

    FOR i = 1 TO UBOUND(Trenner)
        IF Trenner(i) = ItemPos THEN ItemPos = ItemPos - 1
    NEXT

CASE "pageup"
IF ItemPos - MaxDown > 0 THEN
    ItemPage = ItemPage - 1
    ItemPos = ItemPos - MaxDown
END IF

CASE "pagedown"
IF ItemPos + MaxDown < MaxItem% + 1 THEN
IF Liste(ItemPos + MaxDown) <> "" THEN
ItemPage = ItemPage + 1
ItemPos = ItemPos + MaxDown
END IF
END IF
CASE "enter", "esc", "tab"
    LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), 0, B
    Choice = ItemPos: ListBox = Liste(ItemPos): GOTO ExitBox

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

   Eintrag$ = LEFT$(Liste(ItemPos), MaxLenth)
   IF Eintrag$ = "" OR ItemPos > MaxItem% - 1 THEN ItemPos = 0: ItemPage = 1

LOOP UNTIL LCASE$(LEFT$(Eintrag$, LEN(act$))) = LCASE$(act$)
GOTO weiter2

END IF

END SELECT

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos + 1
NEXT

IF ItemPos < 1 THEN ItemPos = 1: ItemPage = 1
IF Liste(ItemPos) = "" THEN ItemPos = ItemPos - 1
IF ItemPos < 1 THEN ItemPos = 1: ItemPage = 1

LOOP

ExitBox:
COLOR Farbe
END FUNCTION

'LÑdt ein Symbol in den Symbolspeicher
'
'Bsp:
'
'LoadSymbol "drvicon", "Laufwerk anwÑhlen"
'LoadSymbol "schema", "Farbschema Ñndern"
'
'Mit ->ShowSymbols werden die Symbole erst angezeigt
'Mit ->GetInp wird dem Benutzer dann die Kontrolle Åber die Symbole gegeben.
'
SUB LoadSymbol (name$, Kennung$)
Symb.total = Symb.total + 1
IF Symb.total > MaxIcons THEN Dialog "Zu viele Symbole": errorlevel = 1: EXIT SUB
IF LEN(Kennung$) < 4 THEN Kennung$ = Kennung$ + SPACE$(4 - LEN(Kennung$))


IF Symb.left = 0 THEN Symb.left = Left
IF Symb.top = 0 THEN Symb.top = Top

x = (Symb.left - 2 + LEN(Kennung$) / 2) * 8
y = Symb.top * 14

IF (INT(x / 8)) + 4 - CINT(LEN(Kennung$) / 2) + LEN(Kennung$) > Right THEN
x = (Left - 2 + LEN(Kennung$) / 2) * 8: Symb.left = Left: Symb.top = Symb.top + 4 + IconVerticalSpace: y = Symb.top * 14
END IF

Symb.left = (INT(x / 8)) + 4 - CINT(LEN(Kennung$) / 2) + LEN(Kennung$)

'Symbol eintragen:

x$ = MID$(STR$(x), 2)
IF LEN(x$) < 5 THEN x$ = STRING$(5 - LEN(x$), "0") + x$

y$ = MID$(STR$(y), 2)
IF LEN(y$) < 5 THEN y$ = STRING$(5 - LEN(y$), "0") + y$

IF LEN(name$) < 8 THEN name$ = name$ + STRING$(8 - LEN(name$), " ")

a$ = x$ + y$ + name$ + RTRIM$(Kennung$)

i = 0
DO: i = i + 1: LOOP UNTIL SymbolList(i) = ""
SymbolList(i) = a$
SymbolNr = 0
END SUB

'Wird fÅr internen Gebrauch verwendet.
FUNCTION Ltext$ (Text$, ab) 'ab bedeutet: AB diesem Zeichen wird gemessen
IF ab = 0 THEN ab = 1
FOR i = ab TO LEN(Text$) - ab + 1
IF MID$(Text$, i, 1) = " " OR ASC(MID$(Text$, i, 1)) = 0 THEN EXIT FOR
NEXT i
Ltext$ = MID$(Text$, ab, i - ab)
END FUNCTION

SUB Main
'Hier kommt Ihr Code hin:

'Das folgende ist ein BeispielprogrÑmmchen, das viele Funktionen von LaOS 5
'aufzeigt, u.a. das Ausrichten eines Textes links oder Rechts)

'Um ein eigenes Programm zu schreiben, lîschen Sie den Text innerhalb dieser
'SUB und schreiben Ihr Programm da rein.


'lîscht den Bildschirm:
    CLW

'zeichnet die Exit-Box: (Die Exit-Box soll dem User zeigen, dass hier mit
'                        Escape zurÅck gesprungen werden kann)
ExitBox -1, -1


'Ein Registerblatt wird im linken Bereich des Bildschirms angezeigt
'Beachte: SÑmtliche Parameter zur Bestimmung der Position von Steuerelementen
'sind in folgender Reihenfolge anzugeben:
'Zeile1, Spalte1, (Zeile2, Spalte2)

Register 6, 5, 14, 40, "Text eingeben:"

'Ein Text wird im zuletzt erstellten Registerblatt ausgegeben. Der Zeilen-
'umbruch wird dabei von der Echo-Funktion Åbernommen.
Echo "Geben Sie einen Text ein:"

a$ = InputBox(9, 7, "Hallîchen?", 0, 0)


Register 6, 5, 14, 40, "Passwort eingeben:"
Echo "Geben Sie einen Text ein:"
'Diesmal wird als Standardtext CHR$(0) Åbergeben, wobei die Passworteingabe
'zum Zuge kommt, und Sterne (*) angezeigt werden, statt den eingegebenen
'Buchstaben:
a$ = InputBox(9, 7, CHR$(0), 0, 0)

'Folgender Aufruf erstellt ein BildschirmfÅllendes Registerblatt:
Register 0, 0, 0, 0, "MenÅ-Auswahl:"

'baut das Menu auf: (Als Format-Parameter wird hier noch das QuickInfo
'                    Åbergeben, welches mit F1 hervorgerufen werden kann)
    MenuBox 0, 0, "Datei" + Format("Dies ist das Datei-MenÅ")
    MenuBox 0, 0, "Hallo" + Format("Dies ist ein Witz.")



'Das folgende Beispiel veranschaulicht die Verwendung der Sanduhr. Bei
'grîsserem Rechneraufwand ist es sinnvoll, die Sanduhr alle halbe Sekunden
'drehen zu lassen:

nz = TIMER + .5
TimeLock 1 'erster Aufruf der Sanduhr
DO
   IF TIMER > nz THEN nz = TIMER + .5: TimeLock 1 'Sanduhr drehen
LOOP WHILE INKEY$ = ""
TimeLock 0 'Sanduhr wieder entfernen

'gibt einen Text aus:
Echo "Das MenÅ wÑhlen Sie mit Alt an!"

'zeigt ein Registerblatt an:
Register 8, 8, 18, 34, "Test"


'zeigt drei verschiedene OK-Dialoge an, wobei der Text mal zentriert, mal
'rechtsbÅndig, mal linksbÅndig angeordnet wird:
    Dialog "Achtung: Die Datei C:\LAOS\DOWNLOAD\inst1.bas ist keine gÅltige PacOpen2-Datei." + Format("c")
    Dialog "Achtung: Die Datei C:\LAOS\DOWNLOAD\inst1.bas ist keine gÅltige PacOpen2-Datei. WÑhlen Sie eine andere Datei aus, oder wandeln Sie sie ins PacOpen2-Format um." + Format("r")
    Dialog "Achtung: Die Datei config.sys existiert nich, oder ist beschÑdigt. (Dies ist nur ein Beispiel-Text!)" + Format("l")

'echo ist die Standard-Textausgaberoutine. Auch hier wird anhand des
'angehÑngten Format-Parameters die Ausrichtung (links, rechts, zentriert)
'bestimmt: (Kein Format-Parameter bedeutet linksbÅndige Anordnung)
Echo "Dieser Text ist linksbÅndig angeordnet."
Echo ""
Echo "Dieser Text ist zentriert." + Format("c")
Echo ""
Echo "Und jetzt noch ein rechtsbÅndig angeordneter Text." + Format("r")

'Mit GetInp gibt das Programm die Kontrolle dem Anwender, wobei sÑmtliche
'aufgebauten Steuerelemente angewÑhlt werden kînnen:
GetInp

'Nochmals ein OK-Dialog:
Dialog "Sie haben " + Focus + " angewÑhlt. Sie haben " + keyb + " gedrÅck."


'Hier wird das angewÑhlte MenÅ anhand der Variable 'Focus' ausgewertet:
IF Focus = "Datei" THEN
   
    'MenuInt erzeugt das UntermenÅ fÅr das angewÑhlte MenÅ:
        a$ = MenuInt("Lîschen,-,Suchen,Speichern,ôffnen,-,Schliessen", ",")

        Dialog "Sie haben " + a$ + " angewÑhlt. Sie haben " + keyb + " gedrÅckt."
END IF

'Mit GetFile wird dem Benutzer die Mîglichkeit geboten, einen Dateinamen und
'einen Ordner auszuwÑhlen, fÅr die Speicherung in einer LaOS-Datei.
'a$ gibt den DOS-Dateinamen zurÅck. Mit OPEN a$ FOR OUTPUT AS #f% kînnte dann
'direkt in die Datei geschrieben werden:
    a$ = GetFile("", "")

'Anhand der folgenden Zeile erhÑlt der Benutzer die Mîglichkeit, eine Datei
'vom Typ 'Text' auszuwÑhlen, und gibt danach den DOS-Dateinamen zurÅck, der
'danach mit OPEN geîffnet wird:
File$ = FileChoose("Text")

'Das ausgewÑhlte File$ wird nun mit geladen und - der Einfachheit halber -
'am StÅck angezeigt:
IF keyb <> "esc" THEN
    f% = FREEFILE
    OPEN File$ FOR INPUT AS f%

        Text$ = ""
        DO WHILE NOT EOF(f%)
            LINE INPUT #f%, a$
            IF errorlevel THEN Dialog "Fehler beim Laden!": EXIT DO
            Text$ = Text$ + a$
        LOOP

    'Bildschirm lîschen und Text zum Schluss mit 'Echo' ausgeben:
    CLW
    Echo Text$ + " Enter drÅcken!" + Format("c")
END IF

'Zum Schluss wird GetInp ausgefÅhrt, damit der Benutzer Enter drÅcken muss,
'um fortzufahren:
GetInp

'errorlevel wird, falls ein Fehler auftaucht auf 1 gesetzt. Hier wird deshalb
'der Wert wieder auf 0 zurÅck gesetzt:
    errorlevel = 0

'Hier wird eine Liste als Beispiel aufgebaut. DafÅr werden die gewÅnschten
'EintrÑge in das Datenfeld 'Liste' eingetragen.
    FOR i = 1 TO MaxListItems + 1
        Liste(i) = "Hallo" + STR$(i)
    NEXT i

'Hier wird die Liste angezeigt und dem Benutzer gleich die Kontrolle Åber
'die Liste Åbergeben: (Bei der ListBox braucht es kein GetInp)
    a$ = ListBox(0, 0, 0, 0)

'Bildschirm wird wieder gelîscht:
CLW

'Ein neues Registerblatt wird aufgebaut:
Register 0, 0, 0, 0, "Quickinfo-Demo:"

'ClearControls ist wichtig, um den Steuerelemente-Speicher wieder zu lîschen.
'Dies ist also immer wichtig, wenn ein neuer Bildschirm aufgebaut wird, da
'sonst die alten Steuerelemente mit in den neuen Bildschirm Åbertragen werden.
'
'Der Parameter von ClearControls bestimmt hier, welche Arten von
'Steuerelementen gelîscht werden sollen, wobei gilt:
'
'0 = Nur Buttons (Controls)
'1 = Nur MenÅs
'2 = Nur Symbole
'3 = Alle

    ClearControls 3


'Hier werden drei Buttons (hier 'Control' genannt) aufgebaut. Die Buttons
'werden sofort angezeigt, aber die Kontrolle Åber die Buttons wird dem
'User erst wieder bei Aufruf von GetInp Åbergeben:
    Control 0, 0, "Das ist ein Control" + Format("Und das ist das QuickInfo dazu")
    Control 0, 0, "Neu starten" + Format("Startet das Demoprogramm nochmals von vorne")
    Control 0, 0, "Beenden" + Format("Beendet das Programm")

'Control Connect legt fest, auf welchem Typ Steuerelement der Fokus anfÑnglich
'gelegt werden soll, wobei gilt:
'0 = Button (Controls)
'1 = MenÅ
'2 = Symbol
    ControlConnect = 0

'Die Kontrolle wird wieder dem User Åbergeben:
    GetInp

'Hier wird mit SELECT CASE der Focus ÅberprÅft, und entsprechende
'Funktionen eingeleitet:
    SELECT CASE Focus
        CASE "Beenden": SYSTEM
        CASE "Neu starten": RUN
    END SELECT

END SUB

'Wird fÅr internen Gebrauch verwendet.
SUB MakeWindow (a, B, c, d)

'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B
LINE (a + 2, B + 2)-(c - 1, d - 1), 0, BF
'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe

TextScale = INT(B / 14) + 2 '=Zeile
TextBreak = INT(a / 8) + 2 '=Spalte


EBox.left = c - 31: EBox.top = d - 24
Left = INT(a / 8) + 2: Top = INT(B / 14) + 2: Right = INT((c + 8) / 8) - 1: Down = INT((d + 14) / 14) - 1

END SUB

'Wird fÅr internen Gebrauch verwendet.
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

'Erstellt eine MenÅbox. MenÅboxen erhalten ihren Fokus durch die Alt-Taste,
'sind also in der Regel auch ausserhalb von GetInp aufrufbar.
'
'Bsp:
'
'MenuBox 0, 0, "Datei"
'MenuBox 0, 0, "Bearbeiten"
'
'GetInp
'IF Focus = "Datei" THEN
'        a$ = MenuInt("Lîschen,-,Suchen,Speichern,ôffnen,-,Schliessen", ",")
'END IF
'
SUB MenuBox (Zeilen, Spalten, Caption$)
'Als Formatparameter kann das QuickInfo Åbergeben werden.
PartFormat Caption$, fmt$

STATIC NMZeilen AS INTEGER, NMSpalten AS INTEGER
IF NMZeilen = 0 THEN NMZeilen = Top
IF NMSpalten = 0 THEN NMSpalten = Left
IF Zeilen = 0 THEN Zeilen = NMZeilen
IF Spalten = 0 THEN Spalten = NMSpalten

IF Spalten + LEN(Caption$) - 1 > Right THEN Spalten = Left: Zeilen = Zeilen + 2
IF Zeilen > Down THEN Zeilen = Top: Spalten = Left

a = Spalten * 8 - 11: B = Zeilen * 14 - 17: c = (Spalten + LEN(Caption$)) * 8 - 5
d = (Zeilen + 1) * 14 - 11

'Lîschen:
LINE (a, B)-(c, d), 0, BF
LOCATE Zeilen, Spalten: PRINT Caption$
'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B
'Schatten:
'LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
'LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
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

Info Caption$, fmt$
TextScale = Zeilen + 2
END SUB

'Erzeugt das Dropdown-MenÅ fÅr ein ausgewÑhltes MenÅ. Die Funktion gibt die
'Bezeichnung des angewÑhlten Eintrages zurÅck.
'
'Bsp:
'
'MenuBox 0, 0, "Datei"
'MenuBox 0, 0, "Bearbeiten"
'GetInp
''IF Focus = "Datei" THEN
'
'        a$ = MenuInt("Lîschen,-,Suchen,Speichern,ôffnen,-,Schliessen", ",")
'
'END IF
'
FUNCTION MenuInt$ (Item$, Trennzeichen$)

Item$ = Item$ + Trennzeichen$

i = 0
MaxLenth = 1
ERASE Liste

DO
i = i + 1

a$ = Trim(Item$, Trennzeichen$, i)

IF LEN(a$) > 77 THEN a$ = LEFT$(a$, 77)
IF LEN(a$) > MaxLenth THEN MaxLenth = LEN(a$)

IF a$ <> "" THEN Liste(i) = a$


LOOP WHILE a$ <> ""

Zeilen = VAL(LEFT$(MenuList(MenuNr), 2)) + 1
Spalten = VAL(MID$(MenuList(MenuNr), 3, 2))
IF Spalten + MaxLenth > 79 THEN Spalten = 79 - MaxLenth
IF Zeilen + i - 1 > 25 THEN Zeilen = 25 - i + 1
PCOPY 0, 1

a$ = ListBox(Zeilen, Spalten, Zeilen + i - 2, Spalten + MaxLenth)


DO

SELECT CASE keyb
CASE "enter": MenuInt$ = a$: EXIT DO
CASE "esc": MenuInt$ = a$: keyb = "esc": EXIT DO
END SELECT
LOOP

PCOPY 1, 0
Activate CHR$(0)

END FUNCTION

'Erzeugt einen unaufdringlichen Piepston
SUB MsgBeep
    SOUND 1000, .1: SOUND 100, .1
END SUB

'Nur fÅr internen Gebrauch bestimmt.
SUB NextControl
'Wird von GetInp verwendet.

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
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""

CASE 1
tC = 0
DO
tC = tC + 1
LOOP WHILE MenuList(tC) <> ""
MenuNr = MenuNr + 1
IF MenuNr > tC - 1 THEN MenuNr = tC - 1
Activate MenuList(MenuNr)

CASE 2
tC = 0
DO
tC = tC + 1
LOOP WHILE SymbolList(tC) <> ""
SymbolNr = SymbolNr + 1: Symb.pnr = Symb.pnr + 1

IF SymbolNr > tC - 1 THEN
SymbolNr = tC - 1: Symb.pnr = Symb.pnr - 1
ELSE
IF Symb.pnr > Symb.count THEN
DimNow = 1
SymbolNr = SymbolNr - 1: Symb.pnr = Symb.pnr - 1
LINE (Left * 8 - 8, Top * 14 - 14)-(Right * 8, Down * 14), 0, BF
ShowSymbols
END IF
END IF
Activate SymbolList(SymbolNr)
END SELECT
END SUB

'Nur fÅr internen Gebrauch bestimmt.
'
'Schneidet die dem Text$ angehÑngte Formatierung ab, und speichert sie in Setting$
SUB PartFormat (Text$, Setting$)

IF INSTR(Text$, CHR$(0)) THEN
    Setting$ = MID$(Text$, INSTR(Text$, CHR$(0)) + 1)
    Text$ = LEFT$(Text$, INSTR(Text$, CHR$(0)) - 1)
ELSE
    Setting$ = ""
END IF

END SUB

'Schreibt einen beliebigen Ausdruck in die Registry. Existiert der Nenner
'schon, so Åberschreibt Regedit den Bezeichner.
'Eintrag$: ein Eintrag in Form von 'Nenner=Bezeichner'
'
'Bsp: (Siehe auch die Function RegItem)
'
'RegEdit "Tetris.Musik=an"
'
SUB RegEdit (Eintrag$)

'PrÅfen, ob Eintrag schon vorhanden:
vh = 0
f = FREEFILE
OPEN "c:\laos\registry.bnk" FOR BINARY AS f: CLOSE f
OPEN "c:\laos\registry.bnk" FOR INPUT AS f

lin = 0
DO WHILE NOT EOF(f)
lin = lin + 1
LINE INPUT #f, a$
IF UCASE$(LEFT$(a$, INSTR(Eintrag$, "="))) = UCASE$(LEFT$(Eintrag$, INSTR(Eintrag$, "="))) THEN vh = 1: EXIT DO
LOOP

f2 = FREEFILE
CLOSE f

IF vh = 1 THEN 'Eintrag ersetzen:

OPEN "c:\laos\registry.bnk" FOR INPUT AS f
OPEN "c:\laos\registry.tmp" FOR OUTPUT AS f2
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
KILL "c:\laos\registry.bnk"
NAME "c:\laos\registry.tmp" AS "c:\laos\registry.bnk"


ELSE    'Eintrag hinzufÅgen:

OPEN "c:\laos\registry.bnk" FOR APPEND AS f
PRINT #f, Eintrag$
CLOSE f

END IF

END SUB

'Zeigt ein Registerblatt mit den Åbergegebenen Zeilen-Spalten Koordinaten und
'dem Text$ als Titel an. Setzt den neuen Seitenbereich zudem auf den Inhalt
'dieses Registerblattes
'
'Bsp:
'
'Register 0, 0, 0, 0, "Willkommen bei LaOS!"
'
'Hier werden wieder die Standard-Parameter 0 verwendet. Das Registerblatt
'nimmt somit den gesamten Bildschirm ein, und man hat somit einen schînen
'Bildschirmtitel.
'
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
IF z = 1 THEN LINE (0, 0)-(638, 0), Fensterfarbe: LINE (1, 1)-(637, 1), 0: LINE (1, 1)-(1, 347), 0
EBox.left = c - 31: EBox.top = d - 24
Left = s + 1: Top = z + 1: Right = su - 1: Down = zu - 1
END SUB

'Gibt einen Eintrag aus der Registry zurÅck. Nenner$: Ein beliebiger Nenner,
'welcher in der Registry vorhanden ist.
'Siehe auch SUB Regedit
'
'Bsp:
'
'IF RegItem$("Tetris.Musik") = "an" THEN sound = 1
'
FUNCTION RegItem$ (Nenner$)

vh = 0
f = FREEFILE
OPEN "c:\laos\registry.bnk" FOR BINARY AS f: CLOSE f
OPEN "c:\laos\registry.bnk" FOR INPUT AS f

DO WHILE NOT EOF(f)
LINE INPUT #f, a$
IF UCASE$(LEFT$(a$, LEN(Nenner$))) = UCASE$(Nenner$) AND MID$(a$, LEN(Nenner$) + 1, 1) = "=" THEN vh = 1: EXIT DO
LOOP
CLOSE f

IF vh = 0 THEN errorlevel = 1: EXIT FUNCTION

RegItem = MID$(a$, LEN(Nenner$) + 2)
END FUNCTION

'Gibt das Datum formatiert nach der LÑndereinstellung des aktiven Bildschirm-
'schemas zurÅck.
'InDate$ muss im Format MM-TT-JJJJ angegeben werden.
'
'Bsp:
'
'Echo "Heutiges Datum ist: " + SDate$(DATE$)
'
FUNCTION SDate$ (InDate$)

SELECT CASE Datumsformat
CASE 0'TT.MM.JJJJ
SDate$ = MID$(InDate$, 4, 2) + "." + LEFT$(InDate$, 2) + "." + RIGHT$(InDate$, 4)
CASE 1'MM-TT-JJJJ
SDate$ = InDate$
CASE 2'JJJJ/MM/TT
SDate$ = RIGHT$(InDate$, 4) + "/" + LEFT$(InDate$, 2) + "/" + MID$(InDate$, 4, 2)
END SELECT
END FUNCTION

'Nur fÅr internen Gebrauch bestimmt
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
LOOP UNTIL Ltext$(Dateiname$, 1) = ""

END SUB

'Nur fÅr internen Gebrauch bestimmt
SUB ShowInfo
'wird von GetInp zur Anzeige von QuickInfos benîtigt.

IF ControlConnect = 2 THEN EXIT SUB
i = 0
DO
i = i + 1
IF i > 500 THEN EXIT SUB
IF MID$(ControlList(i), 5) = Activ OR MenuList(i) = Activ THEN EXIT DO
LOOP
IF QuickInfos(i) = "" THEN EXIT SUB
IF Activ = MenuList(i) THEN
z = VAL(LEFT$(MenuList(i), 2)) + 1 'Menu
s = VAL(MID$(MenuList(i), 3, 2)) + 1
ELSE
z = VAL(LEFT$(ControlList(i), 2)) + 1 'Control
s = VAL(MID$(ControlList(i), 3, 2)) + 1
END IF
IF s > 79 THEN s = 79 - LEN(MID$(ControlList(i), 5))

IF z + INT(LEN(QuickInfos(i)) / (79 - s + 1)) + 1 > 24 THEN z = 24 - (INT(LEN(QuickInfos(i)) / (79 - s + 1)) + 1) + 1
IF z < 2 THEN
s = 3
z = 24 - INT(LEN(QuickInfos(i)) / (79 - s + 1))
END IF

REDIM BG(4 * 8 * 14 * 5) AS DOUBLE
GET (s * 8 - 9, z * 14 - 15)-(s * 8 + (79 - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (79 - s + 1)) + 1) * 14), BG
LINE (s * 8 - 9, z * 14 - 15)-(s * 8 + (79 - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (79 - s + 1)) + 1) * 14), 0, BF
LINE (s * 8 - 9, z * 14 - 15)-(s * 8 + (79 - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (79 - s + 1)) + 1) * 14), InfoFrame, B


ecr = (79 - s + 1) * -1 + 1
FOR a = 1 TO INT(LEN(QuickInfos(i)) / (79 - s + 1)) + 1
LOCATE z + a - 1, s
ecr = ecr + 79 - s + 1
COLOR InfoColor: PRINT MID$(QuickInfos(i), ecr, 79 - s + 1)
NEXT
COLOR Farbe
ecr = 0
a = 0
i = 0
SLEEP
PUT (s * 8 - 9, z * 14 - 15), BG, PSET
END SUB

'Zeigt eine in einem Dosfile ausgelagerte LDS-Zeichnung an:
'
'xplus - Die horizontale Positionierung der Zeichnung
'yplus - Die vertikale Positionierung der Zeichnung
'Streck - Der Streckfaktor (1 = Normal)
'
'z.B:
'
'ShowPicture "haus1.lds", 0, 0, 1
'
'
SUB ShowPicture (Dateiname$, xplus, yplus, streck)

IF streck = 0 THEN streck = 1

DIM i AS INTEGER
DIM Nr AS INTEGER
DIM Befehle(5) AS SINGLE

CLOSE 1: OPEN Dateiname$ FOR BINARY AS 1

i = 0
DO

FOR Nr = 0 TO 5
GET 1, , Befehle(Nr)
NEXT

SELECT CASE CHR$(Befehle(0))
CASE "p": PSET ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck), Befehle(3)
CASE "m": PAINT ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck), Befehle(3), Befehle(4)
CASE "l": LINE ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck)-((Befehle(3) + xplus) * streck, (Befehle(4) + yplus) * streck), Befehle(5)
CASE "r": LINE ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck)-((Befehle(3) + xplus) * streck, (Befehle(4) + yplus) * streck), Befehle(5), B
CASE "b": LINE ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck)-((Befehle(3) + xplus) * streck, (Befehle(4) + yplus) * streck), Befehle(5), BF
CASE "k": CIRCLE ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck), Befehle(3), Befehle(4), , , Befehle(5)
CASE "x": FOR Nr = 0 TO 5
                GET 1, , Bef2(Nr)
          NEXT
          CIRCLE ((Befehle(1) + xplus) * streck, (Befehle(2) + yplus) * streck), Befehle(3), Befehle(4), Bef2(1), Bef2(2), Befehle(5)

END SELECT


LOOP WHILE Befehle(0) <> 0

END SUB

'Nur fÅr internen Gebrauch bestimmt
SUB ShowSymbols
Symb.count = 0
Symbol = SymbolNr
DIM Befehle(5) AS SINGLE
DIM i AS INTEGER
DIM Nr AS INTEGER
DIM LastSymbol AS STRING


IF Symbol = 0 THEN DimY = 0

DO
Symbol = Symbol + 1
Symb.count = Symb.count + 1

a$ = SymbolList(Symbol)
IF a$ = "" THEN EXIT DO

name$ = MID$(a$, 11, 8)
Kennung$ = MID$(a$, 19)

IF LEN(Kennung$) < 4 THEN Kennung$ = STRING$(INT((4 - LEN(Kennung$)) / 2), " ") + Kennung$ + STRING$(CINT((4 - LEN(Kennung$)) / 2 + .1), " ")

x = VAL(LEFT$(a$, 5))
y = VAL(MID$(a$, 6, 5))
IF DimNow = 1 THEN DimY = y - Top * 14: DimNow = 0
y = y - DimY
IF y > Down * 14 - 46 THEN DimNow = 1: EXIT DO'DimY = DimY + y - Top * 14: EXIT DO

VIEW (x, y)-(x + 40, y + 40), SymbolColor, SymbolFrame
Spalten = (INT(x / 8)) + 4 - CINT(LEN(Kennung$) / 2)
IF Spalten <= 1 THEN Spalten = 2
IF Spalten + LEN(Kennung$) > Right THEN Spalten = Right - LEN(Kennung$)
IF Spalten <= Left THEN Kennung$ = LEFT$(Kennung$, LEN(Kennung$) - ((Left - Spalten) + 1)): Spalten = Left + 1
LOCATE INT((y + 40) / 14) + 2, Spalten: PRINT Kennung$
Symb.left = Spalten + LEN(Kennung$)

IF LastSymbol <> name$ THEN
LastSymbol = name$

TA = 0
CLOSE 1: OPEN RTRIM$(name$) + ".lsb" FOR BINARY AS 1: GET 1, 1, TA
IF TA = 0 THEN
    name$ = "c:\laos\" + name$
    CLOSE 1: OPEN RTRIM$(name$) + ".lsb" FOR BINARY AS 1: GET 1, 1, TA
    IF TA = 0 THEN name$ = "c:\laos\sys"
END IF

CLOSE 1: OPEN name$ + ".lsb" FOR BINARY AS 1

i = 0
DO

FOR Nr = 0 TO 5
GET 1, , Befehle(Nr)
NEXT

SELECT CASE CHR$(Befehle(0))
CASE "p": PSET (Befehle(1) + xplus, Befehle(2) + yplus), Befehle(3)
CASE "m": PAINT (Befehle(1) + xplus, Befehle(2) + yplus), Befehle(3), Befehle(4)
CASE "l": LINE (Befehle(1) + xplus, Befehle(2) + yplus)-(Befehle(3) + xplus, Befehle(4) + yplus), Befehle(5)
CASE "r": LINE (Befehle(1) + xplus, Befehle(2) + yplus)-(Befehle(3) + xplus, Befehle(4) + yplus), Befehle(5), B
CASE "b": LINE (Befehle(1) + xplus, Befehle(2) + yplus)-(Befehle(3) + xplus, Befehle(4) + yplus), Befehle(5), BF
CASE "k": CIRCLE (Befehle(1) + xplus, Befehle(2) + yplus), Befehle(3), Befehle(4), , , Befehle(5)
CASE "x": FOR Nr = 0 TO 5
                GET 1, , Bef2(Nr)
          NEXT
          CIRCLE (Befehle(1) + xplus, Befehle(2) + yplus), Befehle(3), Befehle(4), Bef2(1), Bef2(2), Befehle(5)

END SELECT


LOOP WHILE Befehle(0) <> 0

i = 0
    GET (0, 0)-(40, 40), Hintergrund
ELSE
    PUT (0, 0), Hintergrund, PSET
END IF

VIEW 'Setzt das Darstellungsfeld wieder auf den ganzen Bildschirm.
LOOP
IF a$ <> "" THEN Symb.ipp = Symb.count - 1
Symb.total = Symbol - 1
Symb.count = Symb.count - 1

ControlConnect = 2 'Setzt den Verweis auf das Symbol-Steuerelement
SymbolNr = Symb.total - Symb.count + 1
Symb.pnr = 1
END SUB

'Nur fÅr internen Gebrauch bestimmt
SUB StoppUhrBeep (Uhr)

SELECT CASE Uhr

    CASE 1: PCOPY 0, 1: COLOR , 0: CLS
        a$ = "": DO: a$ = INKEY$: LOOP WHILE a$ = ""
        PCOPY 1, 0
        COLOR , Hintergrundfarbe

    CASE 2: 'Hier kommen Ihre Uhrereignisse hin...


    CASE 3: MsgBeep


    CASE 4:


END SELECT
END SUB

'Nur fÅr internen Gebrauch bestimmt
SUB Switchdrive (lw$)
IF LCASE$(lw$) = "c" THEN SHELL "c:": CHDIR "\laos": EXIT SUB

ON ERROR GOTO fehlernext
errorlevel = 0
SHELL lw$ + ":"
CHDIR "\laos"

IF errorlevel = 0 THEN

CLOSE 1: OPEN "c:\laos\drvlist.bnk" FOR RANDOM AS 1

    i = 0
    DO
        i = i + 1
        GET 1, i, a$
    LOOP UNTIL LEFT$(a$, 1) = lw$ OR a$ = ""

CLOSE 1: OPEN lw$ + ":\laos\medname.inf" FOR RANDOM AS 1
GET 1, 1, DriveName
IF DriveName = "" THEN DriveName = MID$(a$, 4)

END IF

END SUB

'Nur fÅr internen Gebrauch bestimmt
SUB Symbact (Symbol$)
'Wird von Activate verwendet, und aktiviert ein Symbol.

x = VAL(MID$(Symbol$, 1, 5))
y = VAL(MID$(Symbol$, 6, 5))
IF DimNow = 1 THEN DimY = DimY + (y - Top * 14): DimNow = 0
y = y - DimY

IF Activ = Symbol$ THEN
LINE (x - 2, y - 2)-(x + 42, y + 42), Symbolactiv, B, SymbactStil
ELSE
IF y >= Top * 14 AND y < Down * 14 - 46 THEN LINE (x - 2, y - 2)-(x + 42, y + 42), 0, B, SymbactStil
END IF
END SUB

'Nur fÅr internen Gebrauch bestimmt
SUB TextBox (Zeilen, Spalten, Text$, Break)
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

FrameWindow a, B, c, d, 1

ecr = Break * -1 + 1
FOR i = 1 TO INT(LEN(Text$) / Break) + 1
LOCATE Zeilen + i - 1, Spalten
ecr = ecr + Break
PRINT MID$(Text$, ecr, Break)
NEXT i
ecr = 0


TextScale = EndZeile + 1
END SUB

'TimeLock zeigt die Sanduhr an, die anzeigt, dass das Programm gerade arbeitet
'
'Wenn ihr Programm etwas machen muss, das lange dauert, wird TimeLock wird in
'der Regel alle halbe Sekunde erneut aufgerufen, damit dieSanduhr bewegt wird.
'
'Er wird immer mit TimeLock 1 aufgerufen. Wenn Sie den TimeLock nicht mehr
'brauchen, mÅssen Sie ihn mit TimeLock 0 wieder entfernen.
'
'Bsp:
'
'nz = TIMER + .5
'TimeLock 1 'erster Aufruf der Sanduhr
'DO
'   IF TIMER > nz THEN nz = TIMER + .5: TimeLock 1 'Sanduhr drehen
'LOOP WHILE INKEY$= ""
'TimeLock 0 'Sanduhr entfernen
'
SUB TimeLock (Status)
STATIC On.too
STATIC tm
Radius = 14 * 2
Farbe1 = 12
Farbe2 = 9
x = 319 - Radius
y = 174 - Radius

SELECT CASE Status
CASE 1:
'Falls es das erste Mal ist, muss der Hintergrund noch gespeichert werden:
IF On.too = 0 THEN On.too = 1: GET (319 - Radius, 174 - Radius)-(319 + Radius, 174 + Radius), Hintergrund

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
CASE 0: IF On.too <> 0 THEN On.too = 0: PUT (319 - Radius, 174 - Radius), Hintergrund, PSET: StoppUhr(1) = 0
END SELECT
END SUB

'Nur fÅr internen Gebrauch bestimmt
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

