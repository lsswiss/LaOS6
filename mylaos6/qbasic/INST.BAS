'Installationsprogramm fÅr LaOS 6
'24.7.2002
'   Ausblenden der shell "copy..."-Fehlermeldungen
'   WinPif-Filecopy-Fehlermeldungen ausblenden
'   QBasic kopieren - keine Fehlermeldung falls QB-Path schon C:\LAOS (FileCopy)
'5.7.2002
'   Hintergrundbild: LaOS 6
'   QBasic wird immer nach C:\LAOS\ kopiert!
'   Echo mit Zeilenumbruch


'Nicht kopieren. Alle Urheber- und Leistungsschutz-Rechte vorbehalten.
'3.2.2000
'(C) 2000 by Urs Langmeier

DECLARE SUB PartFormat (Text$, Setting$)
DECLARE SUB Extract (Datei$, NachOrdner AS STRING, NachDatei AS STRING, sym$, Art1$, Link1$, Info1$)
DECLARE SUB LoadSchem ()
DECLARE SUB FileCopy (Source$, Dest$)
DECLARE SUB ProgressBar (Stat AS INTEGER)
DECLARE SUB InstallWinIcons ()
DECLARE SUB SearchToDirAndCopyPif (Dir$)
DECLARE FUNCTION OverwriteAll! ()
DECLARE SUB NewBox (name$)
DECLARE FUNCTION IsExistBox! (Box$)
DECLARE FUNCTION SDate$ ()
DECLARE FUNCTION IsExist! (Datei$, InOrdner$)
DECLARE SUB ExtractFF (Datei$)
DECLARE SUB Copy (Datei AS STRING, DestFile AS STRING)
DECLARE SUB ExitBox (x!, y!)
DECLARE SUB Dialog (Caption$)
DECLARE SUB Register (z!, s!, zu!, su!, Text$)
DECLARE SUB State (Stat AS INTEGER)
DECLARE FUNCTION InputYesNo! (Message AS STRING)
DECLARE SUB ClearControls (Connect!)
DECLARE SUB Info (Ctrl$, InfoText$)
DECLARE SUB DrawBCO ()
DECLARE FUNCTION Drivelist$ (Nr!)
DECLARE FUNCTION exist! (p$)
DECLARE SUB TextBox (Zeilen!, Spalten!, Text$, Break!)
DECLARE SUB FrameWindow (a!, B!, c!, d!, del!)
DECLARE SUB Echo (Text$)
DECLARE SUB GetInp ()
DECLARE SUB Control (Zeilen!, Spalten!, Text$)
DECLARE SUB CLW ()
DECLARE FUNCTION Gerade! (Zahl!)
DECLARE FUNCTION Trim$ (Variable$, Trennzeichen$, a!)
DECLARE FUNCTION MenuInt$ (Item$, Trennzeichen$)
DECLARE SUB Menuact (Menu$)
DECLARE SUB TimeLock (Status!)
DECLARE SUB StoppUhrBeep (Uhr!)
DECLARE FUNCTION Ltext$ (Text$, ab!)
DECLARE SUB ShowInfo ()
DECLARE FUNCTION GetKey$ ()
DECLARE SUB NextControl ()
DECLARE SUB LastControl ()
DECLARE SUB Activate (Ctrl$)
'LaOS Version 2.00 Copyright (C) Urs Langmeier, New BCO
'Sommer 1998, Version ohne ScreenText und ohne Run-on-command

'Entwickelt unter QBASIC von Urs Langmeier
'Idee & Programmierung: Urs Langmeier

'Alle Urheber- und Leistungsschutzrechte vorbehalten. Der Code darf nicht un-
'erlaubt kopiert oder verkauft werden.


ON ERROR GOTO Fehler
1 SCREEN 9
VIEW PRINT 1 TO 25
3
DIM SHARED Hintergrund(456) AS LONG
DIM SHARED Stoppuhr(1 TO 4) AS INTEGER '1 ist Reserviert!
DIM SHARED AlteZeit(1 TO 4) AS LONG
DIM SHARED TextScale AS INTEGER
DIM SHARED TextBreak AS INTEGER
DIM SHARED Activ AS STRING
DIM SHARED ControlNr AS INTEGER
DIM SHARED ControlList(40) AS STRING
DIM SHARED Break AS INTEGER
DIM SHARED EBox.left AS INTEGER, EBox.top AS INTEGER
DIM SHARED Left AS INTEGER, Top AS INTEGER, Right AS INTEGER, Down AS INTEGER
DIM SHARED NextBreak AS INTEGER
DIM SHARED QuickInfos(40) AS STRING
DIM SHARED ControlConnect AS INTEGER 'Zur Unterscheidung der 3 Eingabecontrols
DIM SHARED MenuList(40) AS STRING, MenuNr AS INTEGER
DIM SHARED Focus AS STRING
DIM SHARED Keyb AS STRING
DIM SHARED Change(6) AS INTEGER
DIM SHARED errorlevel AS INTEGER
DIM SHARED ActivItem AS STRING, DriveName AS STRING

FesteEinstellungen:
4
WIDTH 80, 25
5
Einstellungen:
DIM SHARED Fensterfarbe AS INTEGER, Randfarbe AS INTEGER, Schattenfarbe AS INTEGER, Controlfarbe AS INTEGER
DIM SHARED Hintergrundfarbe  AS INTEGER, Farbe AS INTEGER
DIM SHARED InfoTime AS INTEGER, InfoColor  AS INTEGER, InfoFrame

ON ERROR GOTO Fehlernext
LoadSchem

IF errorlevel = 1 THEN

Fensterfarbe = 11
Randfarbe = 3
Schattenfarbe = 3
Controlfarbe = 11

Hintergrundfarbe = 63
Farbe = 8

InfoTime = 3

InfoColor = 9
InfoFrame = 3

Stoppuhr(2) = 60 '2.Stoppuhr: Bildschirmschoner
END IF

VorStart:
CLOSE
VIEW
COLOR Farbe, Hintergrundfarbe

Code = 0 'Legt die Ereignisverfolgung fÅr die Alt-Taste fest
FOR i = 15 TO 19
KEY i, CHR$(Code) + CHR$(56)
ON KEY(i) GOSUB AltKey
Code = Code + 32
NEXT i
Code = 0: i = 0

Programm:
DIM SHARED LaOSexist AS INTEGER
LaOSexist = 0
CLOSE

ON ERROR GOTO Fehlernext
'Schreiben Sie hier Ihren persînlichen Code:
CONST Ziel = "C:\LAOS\"
'Quelle angegeben durch aktiven Pfad
CHDIR "LSYS"
errorlevel = 0

IF NOT exist("start.bas") THEN CHDIR "..\LSYS"

CLW
IF NOT exist("start.bas") THEN
        Dialog "  Die Quelldateien liegen nicht im aktuellen Verzeichnis. Wechseln Sie ins  Installationsverzeichnis und fÅhren Sie das Installationsprogramm erneut aus"
        SYSTEM
END IF
Echo "Willkommen in der LaOS - Installation"
Echo ""
Echo "LaOS Version 6.00"
CALL DrawBCO
Control 10, 2, SPACE$(30) + "LaOS Installieren" + SPACE$(31)
Control 0, 2, SPACE$(34) + "Abbrechen" + SPACE$(35)
Info SPACE$(30) + "LaOS Installieren" + SPACE$(31), "DrÅcken Sie jetzt die Eingabetaste, um LaOS zu installieren."
ShowInfo
GetInp
IF ControlNr = 2 OR Keyb = "esc" THEN SYSTEM


DiskCheck:
IF NOT exist("start.bas") THEN
        Dialog "  Die Quelldateien liegen nicht im aktuellen Verzeichnis. Wechseln Sie ins  Installationsverzeichnis und fÅhren Sie das Installationsprogramm erneut aus"
        SYSTEM
END IF

GOSUB qbsearch

PRINT "LaOS wird in das Verzeichnis C:\LAOS installiert."
ON ERROR GOTO errornext
MKDIR "c:\laos"
errorlevel = 0
CLOSE 1: OPEN "c:\laos\nul" FOR INPUT AS 1: CLOSE 1
IF errorlevel = 1 THEN PRINT "Fehler beim Erstellen des Verzeichnisses. Installation abgebrochen.": a$ = INPUT$(1): SYSTEM

    FileCopy a$, "C:\LAOS\QBASIC.EXE"
    a$ = "C:\LAOS\QBASIC.EXE"

        IF INSTR(a$, " ") > 0 THEN a$ = CHR$(34) + a$ + CHR$(34)'Wenn Langer Pfad unter Win95, muss in AnfÅhrungszeichen zurÅckgegeben werden
        CLOSE f%

    QBPath$ = a$

IF exist("c:\laos\dateien.bnk") THEN LaOSexist = -1

CLW
Register 0, 0, 0, 0, "LaOS 6 - Installation"
DrawBCO
Register 8, 20, 15, 60, "ôffne..."
Echo ""
Echo " Verzeichnis"

TimeLock 1
SHELL "dir *.* /b /O:N >c:\laos.tmp"
TimeLock 0

OPEN "c:\laos.tmp" FOR INPUT AS 3
DO WHILE NOT EOF(3)
LINE INPUT #3, B$

IF LCASE$(B$) <> "dateien.bnk" AND LCASE$(B$) <> "ordner.bnk" THEN
ExtractFF B$
END IF

IF INKEY$ = CHR$(27) OR Keyb = "esc" THEN
    Dialog "Vorgang angehalten. X + ENTER - Installation abbrechen."
    IF LCASE$(Keyb) = "x" THEN Dialog "Installation ist nicht komplett. Sie mÅssen sie erneut ausfÅhren": IF Keyb <> "esc" THEN SYSTEM
    Keyb = ""
END IF

LOOP
CLOSE 3
KILL "c:\laos.tmp"

IF NOT exist(Ziel + "start.bas") THEN PRINT "Fehler beim Kopieren oder Vorgang abgebrochen. Installation abgebrochen.": a$ = INPUT$(1): SLEEP 1: SYSTEM

CLOSE 1: OPEN Ziel + "start.bat" FOR OUTPUT AS 1
PRINT #1, "@echo off"
PRINT #1, "cls"
PRINT #1, "echo Das Langmeier Operating System."
PRINT #1, "echo."
PRINT #1, "echo Copyright (C) 1998-2002, Urs Oliver Langmeier. Open-Source System."
PRINT #1, QBPath$ + " /RUN C:\LAOS\START.BAS"
CLOSE 1

IF NOT LaOSexist THEN
        Copy "dateien.bnk", ""
        Copy "ordner.bnk", ""
END IF

IF NOT exist(Ziel + "start.bas") OR NOT exist(Ziel + "dateien.bnk") THEN PRINT "Fehler beim Kopieren oder Vorgang abgebrochen. Installation abgebrochen.": a$ = INPUT$(1): SLEEP 1: SYSTEM

CLW
Echo ""
Echo ""
Echo "Die Installation ist abgeschlossen."
Echo ""
Echo "Falls Sie Windows besitzen, bietet Ihnen das Installationsprogramm an,"
Echo "ein Symbol auf Ihrem Desktop und im StartmenÅ zu erstellen."
Echo ""
Echo "Dieses ermîglicht Ihnen das bequeme Aufrufen von LaOS direkt aus Windows"
Echo "heraus."
Echo ""
Echo ""
Echo ""
Echo "Wollen Sie ein LaOS-Symbol im StartmenÅ bzw. auf dem Desktop erstellen?"
Echo ""
Echo "WÑhlen Sie 'Ja' falls Sie Windows ab der Version 3 (auch 95 und 98) be-"
Echo "sitzen."
Echo ""
ClearControls 3
Control 0, 0, "Ja"
Info "Ja", "DrÅcken Sie jetzt Enter, falls Sie Windows besitzen."
Control 0, 0, "Nein, ich besitze kein Windows."
GetInp
IF Focus = "Ja" THEN
        InstallWinIcons

ELSE

CLW
DrawBCO
Echo ""
Echo ""
Echo "Starten von LaOS unter Dos:"
Echo ""
Echo "Von nun an kînnen Sie LaOS jederzeit mit der Datei LAOS.BAT im Verzeichnis"
Echo "C:\LAOS starten."
Echo ""
Echo "Sie kînnen diese Datei auch in Ihre Autoexec.bat-Datei aufnehmen, um LaOS beim"
Echo "Aufstarten des Computers immer gleich zu starten."
DO: LOOP WHILE INKEY$ <> ""
a$ = INPUT$(1)

END IF


CLW
ClearControls 0
Echo "Um die Bedienung von LaOS lernen zu kînnen, sollten Sie einen 5 Minuten-"
Echo "Schnellkurs absolvieren."
Echo "DrÅcken Sie auf 'Schnellkurs starten', um den Kurs zu starten."
Control 7, 3, "Schnellkurs starten"
Control 9, 3, "Direkt zu LaOS!"
GetInp
IF ControlNr = 1 AND Keyb <> "esc" THEN
    CHAIN "C:\LAOS\LP.LAO"
    Dialog "Der Schnellkurs (c:\laos\lp.lao) konnte nicht ausgefÅhrt werden."
END IF

CHAIN "C:\LAOS\START.BAS"


TimeEreignis:
TIMER OFF
ShowInfo
RETURN


AltKey:
IF MenuList(1) <> "" THEN Activate CHR$(0): ControlConnect = 1: Activate MenuList(1): MenuNr = 1
RETURN

Fehlernext:
errorlevel = 1
RESUME NEXT

Fehler:
SELECT CASE ERL
CASE 1, 4: SCREEN 0: PRINT "Lieber Benutzer. Du wolltest gerade LaOS installieren."
PRINT "Aber um LaOS installieren zu kînnen, benîtigen Sie mindestens einen VGA- oder"
PRINT "EGA-Adapter. Falls Sie doch so einen haben, so kînnen Sie noch einmal versuch-"
PRINT "en, die Installation zu starten. Sonst wÅrden Sie sich bitte mit Herrn"
PRINT "Urs Langmeier in Verbindung setzen. (laos@datacomm.ch)"
CASE ELSE: PRINT "Unbekannter Fehler aufgetreten. Erneut versuchen."
END SELECT

a$ = INPUT$(1)
SLEEP 2
SYSTEM

qbsearch:
'Programm fÅr die Suche von QBasic.exe, Version 1.0
'Copyright (C) was BCO, 26.7.1998
'
'Alle Rechte vorbehalten. Keine unerlaubte Nutzung. Keine unerlaubte
'VervielfÑltigung. Keine énderungen am Code.
CLS
ON ERROR GOTO existalready
DIM deletelater AS INTEGER
deletelater = 1
MKDIR "C:\TEMP"


ENVIRON "DIRCMD="
DIM lw AS INTEGER
ON ERROR GOTO errha

FOR lw = 67 TO 90
LOCATE 1, 1: PRINT "PrÅfe: " + CHR$(lw)

CLOSE 1: OPEN CHR$(lw) + ":\nul" FOR BINARY AS 1
LOCATE 2, 2: PRINT "Durchsuche " + CHR$(lw) + " nach QBASIC"
LOCATE 3, 3
SHELL "dir " + CHR$(lw) + ":\qbasic.exe /S /B >C:\TEMP\mirror.tmp"
LOCATE 3, 3: PRINT SPACE$(40)
CLOSE 1: OPEN "C:\TEMP\mirror.tmp" FOR INPUT AS 1
LINE INPUT #1, a$
CLOSE 1
IF a$ <> "" THEN GOTO gefunden
LOCATE 2, 2: PRINT SPACE$(12)

7
NEXT
ON ERROR GOTO errornext
IF deletelater = 1 THEN KILL "C:\TEMP\*.*": RMDIR "C:\TEMP": deletelater = 0
PRINT
PRINT " Es ist kein QBASIC gefunden worden."
PRINT " Sie kînnen aber das der Installation beigelete QBASIC verwenden."
DO
PRINT
PRINT " Wollen Sie QBASIC auf Ihre Festplatte kopieren? [J/N]";
LINE INPUT a$
IF UCASE$(a$) = "N" THEN DO: LOOP WHILE INKEY$ <> "": PRINT : PRINT "Die Installation kann ohne QBasic nicht fortgesetzt werden.": SLEEP 5: SYSTEM

LOOP UNTIL UCASE$(a$) = "J"
'QBASIC auf die Festplatte kopieren:
ON ERROR GOTO errornext
MKDIR "C:\LAOS"
PRINT : PRINT "QBASIC wird in das Verzeichnis C:\LAOS installiert."
Copy "..\qbasic\qbas.exe", "qbasic.exe"

errorlevel = 0
CLOSE 1: OPEN "C:\LAOS\QBASIC.EXE" FOR INPUT AS 1
CLOSE 1
IF errorlevel = 1 THEN PRINT : PRINT "Fehlerhafter Kopiervorgang. Versuchen Sie es erneut.": DO: LOOP WHILE INKEY$ <> "": PRINT : PRINT "Die Installation wird abgebrochen.": SLEEP 2: SYSTEM

a$ = "C:\LAOS\QBASIC.EXE"
GOTO gefunden


errha:
RESUME 7

existalready:
deletelater = 0
RESUME NEXT

errornext:
errorlevel = 1
RESUME NEXT


gefunden:
ON ERROR GOTO errornext
IF deletelater = 1 THEN KILL "C:\TEMP\*.*": RMDIR "C:\TEMP"
RETURN

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
a$ = MenuList(i)
IF a$ = "" THEN EXIT DO
Menuact a$
LOOP UNTIL a$ = ""



END SELECT
END SUB

SUB ClearControls (Connect)
SELECT CASE Connect
CASE 0: ControlNr = 0: ERASE ControlList: ERASE QuickInfos
CASE 1: MenuNr = 0: ERASE MenuList: ERASE QuickInfos
CASE 3 'Alle lîschen
ControlNr = 0: ERASE ControlList: ERASE QuickInfos
MenuNr = 0: ERASE MenuList: ERASE QuickInfos
END SELECT

END SUB

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

SUB Control (Zeilen, Spalten, Text$)

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
'LINE (a, B)-(c, d), Farbe, B
'PAINT (a + 1, B + 1), Controlfarbe, Farbe
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


2 Activ = Text$: ControlConnect = 0: TextScale = Zeilen + 2
END SUB

SUB Copy (Datei AS STRING, DestFile AS STRING)

IF DestFile = "" THEN DestFile = Datei

IF Datei$ <> "ordner.bnk" AND Datei$ <> "dateien.bnk" THEN

IF exist(Ziel + DestFile) THEN

'Ausnahmen nie zu Åberschreibender Dateien:
SELECT CASE LCASE$(DestFile)
    CASE "adressen.bnk", "scuts.dat", "config.lcg", "ordner.lsb", "registry.bnk", "drvlist.bnk": EXIT SUB
END SELECT

    IF NOT OverwriteAll THEN
    IF NOT InputYesNo("Soll die existierende Datei '" + Datei + "' Åberschrieben werden?" + CHR$(0) + "c") THEN EXIT SUB
    END IF
END IF

END IF

11
IF NOT exist(Datei) THEN
    IF NOT InputYesNo("Die Datei '" + Datei + "' kann nicht gefunden werden. Neu versuchen?") THEN EXIT SUB
    GOTO 11
END IF

12

FileCopy Datei$, Ziel + DestFile

END SUB

SUB Dialog (Caption$)
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

ecr = 1
FOR i = 1 TO INT(LEN(Caption$) / 76) + 1
LOCATE Zeilen, Spalten
PRINT MID$(Caption$, ecr, 76)
ecr = ecr + 76
Zeilen = Zeilen + 1
NEXT

TextBox Zeilen + 1, 33, "      OK", 14

DO
B$ = a$
a$ = GetKey
LOOP UNTIL a$ = "esc" OR a$ = CHR$(13)
IF a$ = "esc" THEN Keyb = "esc" ELSE Keyb = B$
PUT (x1!, y1!), Background, PSET: x1! = 0: y1! = 0: EXIT SUB


END SUB

SUB DrawBCO
LINE (399, 190)-(619, 330), 15, BF
LINE (619, 330)-(399, 190), 7, B
LINE (401, 192)-(617, 328), 8, B
LINE (403, 288)-(423, 268), 3
LINE (443, 288)-(423, 268), 3
LINE (443, 288)-(483, 248), 3
LINE (483, 248)-(503, 268), 3
LINE (503, 268)-(523, 248), 3
LINE (523, 248)-(563, 288), 3
LINE (563, 288)-(543, 308), 3
LINE (543, 308)-(523, 288), 3
LINE (523, 288)-(503, 308), 3
LINE (503, 308)-(483, 326), 3
LINE (481, 326)-(615, 326), 3
LINE (615, 326)-(615, 266), 3
LINE (595, 246)-(615, 266), 3
LINE (595, 246)-(543, 270), 3
LINE (443, 326)-(487, 286), 3
LINE (443, 288)-(405, 326), 3
LINE (483, 326)-(403, 326), 3
LINE (403, 288)-(403, 326), 3
PAINT (423, 286), 11, 3
LINE (503, 266)-(483, 286), 3
LINE (487, 286)-(483, 286), 3
PAINT (505, 286), 3, 3
PAINT (465, 286), 7, 3
LINE (563, 288)-(599, 324), 3
PAINT (589, 284), 11, 3
LINE (527, 304)-(513, 318), 8
LINE (517, 318)-(523, 312), 8
LINE (525, 314)-(529, 310), 8
LINE (565, 304)-(561, 306), 8
LINE (565, 306)-(559, 310), 8
LINE (565, 310)-(569, 308), 8
LINE (567, 306)-(563, 310), 8
LINE (551, 318)-(555, 316), 8
LINE (553, 320)-(555, 320), 8
LINE (559, 320)-(555, 318), 8
CIRCLE (577, 218), 20, 14, , , .8
PAINT (577, 218), 14, 14
LINE (575, 238)-(575, 248), 14
LINE (545, 248)-(559, 234), 14
LINE (563, 200)-(561, 198), 14
LINE (581, 196)-(581, 198), 14
LINE (597, 202)-(605, 194), 14
LINE (601, 214)-(615, 214), 14
LINE (597, 232)-(611, 242), 14
LINE (553, 218)-(531, 218), 14
LINE (559, 204)-(547, 196), 14
LINE (407, 198)-(407, 228), 11
LINE (408, 198)-(408, 228), 11
LINE (407, 228)-(422, 228), 11
LINE (408, 228)-(423, 228), 11
LINE (443, 228)-(440, 225), 11
LINE (444, 228)-(441, 225), 11
LINE (440, 225)-(440, 216), 11
LINE (441, 225)-(441, 216), 11
LINE (440, 216)-(437, 213), 11
LINE (441, 216)-(438, 213), 11
LINE (437, 213)-(431, 213), 11
LINE (438, 213)-(432, 213), 11
LINE (431, 213)-(428, 216), 11
LINE (432, 213)-(429, 216), 11
LINE (428, 216)-(428, 225), 11
LINE (429, 216)-(429, 225), 11
LINE (428, 225)-(431, 228), 11
LINE (429, 225)-(432, 228), 11
LINE (431, 228)-(437, 228), 11
LINE (432, 228)-(438, 228), 11
LINE (437, 228)-(440, 225), 11
LINE (438, 228)-(441, 225), 11
LINE (452, 198)-(449, 201), 11
LINE (453, 198)-(450, 201), 11
LINE (449, 201)-(449, 225), 11
LINE (450, 201)-(450, 225), 11
LINE (449, 225)-(452, 228), 11
LINE (450, 225)-(453, 228), 11
LINE (452, 228)-(464, 228), 11
LINE (453, 228)-(465, 228), 11
LINE (464, 228)-(467, 225), 11
LINE (465, 228)-(468, 225), 11
LINE (467, 225)-(467, 201), 11
LINE (468, 225)-(468, 201), 11
LINE (467, 201)-(464, 198), 11
LINE (468, 201)-(465, 198), 11
LINE (464, 198)-(452, 198), 11
LINE (465, 198)-(453, 198), 11
LINE (488, 201)-(485, 198), 11
LINE (489, 201)-(486, 198), 11
LINE (485, 198)-(476, 198), 11
LINE (486, 198)-(477, 198), 11
LINE (476, 198)-(473, 201), 11
LINE (477, 198)-(474, 201), 11
LINE (473, 201)-(473, 210), 11
LINE (474, 201)-(474, 210), 11
LINE (473, 210)-(476, 213), 11
LINE (474, 210)-(477, 213), 11
LINE (476, 213)-(485, 213), 11
LINE (477, 213)-(486, 213), 11
LINE (485, 213)-(488, 216), 11
LINE (486, 213)-(489, 216), 11
LINE (488, 216)-(488, 225), 11
LINE (489, 216)-(489, 225), 11
LINE (488, 225)-(485, 228), 11
LINE (489, 225)-(486, 228), 11
LINE (485, 228)-(476, 228), 11
LINE (486, 228)-(477, 228), 11
LINE (476, 228)-(473, 225), 11
LINE (477, 228)-(474, 225), 11
LINE (515, 225)-(518, 228), 11
LINE (516, 225)-(519, 228), 11
LINE (518, 228)-(527, 228), 11
LINE (519, 228)-(528, 228), 11
LINE (527, 228)-(530, 225), 11
LINE (528, 228)-(531, 225), 11
LINE (518, 213)-(527, 213), 11
LINE (519, 213)-(528, 213), 11
LINE (527, 213)-(530, 216), 11
LINE (528, 213)-(531, 216), 11
LINE (530, 216)-(530, 225), 11
LINE (531, 216)-(531, 225), 11
LINE (515, 225)-(515, 201), 11
LINE (516, 225)-(516, 201), 11
LINE (515, 201)-(518, 198), 11
LINE (516, 201)-(519, 198), 11
LINE (518, 198)-(527, 198), 11
LINE (519, 198)-(528, 198), 11
LINE (527, 198)-(530, 201), 11
LINE (528, 198)-(531, 201), 11
LINE (518, 213)-(515, 216), 11
LINE (519, 213)-(516, 216), 11
LINE (406, 197)-(406, 227), 8
LINE (407, 197)-(407, 227), 8
LINE (406, 227)-(421, 227), 8
LINE (407, 227)-(422, 227), 8
LINE (442, 227)-(439, 224), 8
LINE (443, 227)-(440, 224), 8
LINE (439, 224)-(439, 215), 8
LINE (440, 224)-(440, 215), 8
LINE (439, 215)-(436, 212), 8
LINE (440, 215)-(437, 212), 8
LINE (436, 212)-(430, 212), 8
LINE (437, 212)-(431, 212), 8
LINE (430, 212)-(427, 215), 8
LINE (431, 212)-(428, 215), 8
LINE (427, 215)-(427, 224), 8
LINE (428, 215)-(428, 224), 8
LINE (427, 224)-(430, 227), 8
LINE (428, 224)-(431, 227), 8
LINE (430, 227)-(436, 227), 8
LINE (431, 227)-(437, 227), 8
LINE (436, 227)-(439, 224), 8
LINE (437, 227)-(440, 224), 8
LINE (451, 197)-(448, 200), 8
LINE (452, 197)-(449, 200), 8
LINE (448, 200)-(448, 224), 8
LINE (449, 200)-(449, 224), 8
LINE (448, 224)-(451, 227), 8
LINE (449, 224)-(452, 227), 8
LINE (451, 227)-(463, 227), 8
LINE (452, 227)-(464, 227), 8
LINE (463, 227)-(466, 224), 8
LINE (464, 227)-(467, 224), 8
LINE (466, 224)-(466, 200), 8
LINE (467, 224)-(467, 200), 8
LINE (466, 200)-(463, 197), 8
LINE (467, 200)-(464, 197), 8
LINE (463, 197)-(451, 197), 8
LINE (464, 197)-(452, 197), 8
LINE (487, 200)-(484, 197), 8
LINE (488, 200)-(485, 197), 8
LINE (484, 197)-(475, 197), 8
LINE (485, 197)-(476, 197), 8
LINE (475, 197)-(472, 200), 8
LINE (476, 197)-(473, 200), 8
LINE (472, 200)-(472, 209), 8
LINE (473, 200)-(473, 209), 8
LINE (472, 209)-(475, 212), 8
LINE (473, 209)-(476, 212), 8
LINE (475, 212)-(484, 212), 8
LINE (476, 212)-(485, 212), 8
LINE (484, 212)-(487, 215), 8
LINE (485, 212)-(488, 215), 8
LINE (487, 215)-(487, 224), 8
LINE (488, 215)-(488, 224), 8
LINE (487, 224)-(484, 227), 8
LINE (488, 224)-(485, 227), 8
LINE (484, 227)-(475, 227), 8
LINE (485, 227)-(476, 227), 8
LINE (475, 227)-(472, 224), 8
LINE (476, 227)-(473, 224), 8
LINE (514, 224)-(517, 227), 8
LINE (515, 224)-(518, 227), 8
LINE (517, 227)-(526, 227), 8
LINE (518, 227)-(527, 227), 8
LINE (526, 227)-(529, 224), 8
LINE (527, 227)-(530, 224), 8
LINE (517, 212)-(526, 212), 8
LINE (518, 212)-(527, 212), 8
LINE (526, 212)-(529, 215), 8
LINE (527, 212)-(530, 215), 8
LINE (529, 215)-(529, 224), 8
LINE (530, 215)-(530, 224), 8
LINE (514, 224)-(514, 200), 8
LINE (515, 224)-(515, 200), 8
LINE (514, 200)-(517, 197), 8
LINE (515, 200)-(518, 197), 8
LINE (517, 197)-(526, 197), 8
LINE (518, 197)-(527, 197), 8
LINE (526, 197)-(529, 200), 8
LINE (527, 197)-(530, 200), 8
LINE (517, 212)-(514, 215), 8
LINE (518, 212)-(515, 215), 8
LINE (402, 331)-(620, 331), 8
LINE (620, 331)-(620, 193), 8
END SUB

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

FUNCTION exist (p$)
f% = FREEFILE
ae% = errorlevel
errorlevel = 0
OPEN p$ FOR INPUT AS f%
IF errorlevel = 0 THEN exist = -1 ELSE exist = 0
CLOSE f%
errorlevel = ae%
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

SUB Extract (Datei$, NachOrdner AS STRING, NachDatei AS STRING, sym$, Art1$, Link1$, Info1$)
DIM a AS INTEGER

Register 8, 20, 15, 60, "Dateisystem anpassen..."
Echo ""
Echo " " + NachDatei

Dif$ = "" 'Dif wird, falls Datei$ ein DocIntFile ist, entsprechend gesetzt.

'PrÅfen, ob NachDatei im Dateisystem schon existiert.
IF IsExist(NachDatei, NachOrdner) THEN
    IF NOT OverwriteAll THEN
        IF NOT InputYesNo("Die Datei '" + NachDatei + "' existiert schon im Ordner '" + NachOrdner + "'. Soll diese Åberschrieben werden?" + CHR$(0) + "c") THEN EXIT SUB
    END IF
END IF


'Wichtig: Die Datei$ darf kein Suffix enthalten:
a = INSTR(Datei$, ".")
IF a > 0 THEN dat$ = LEFT$(Datei$, a - 1): GOTO 22 ELSE dat$ = Datei$

IF VAL(dat$) > 0 AND LaOSexist THEN
'Falls DocInt-Datei:
'neuer Dateiname suchen:

a = 1
DO
dat$ = MID$(STR$(a), 2)
a = a + 1
LOOP WHILE exist(Ziel + dat$)

Dif$ = dat$'DIF$ wird auf den eingetragenen DocInt-Wert gesetzt.
END IF

22
IF sym$ = "" THEN sym$ = "sys"

'Wichtig: Das Symbol darf kein Suffix enthalten...
a = INSTR(sym$, ".")
IF a > 0 THEN sym$ = LEFT$(sym$, a - 1)

'... und nicht mehr als 8 Zeichen lang sein: (Zur KompatibilitÑt zu LaOS-Icons)
sym$ = LEFT$(sym$, 8)


errorlevel = 0

CLOSE 1: OPEN Ziel + "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
DO
a = a + 1
GET 1, a
LOOP UNTIL Ltext$(Dateiname$, 1) = "" OR NachDatei = RTRIM$(OsName$) AND NachOrdner = RTRIM$(Ordner$)

IF Ltext$(Dateiname$, 1) <> "" AND Dif$ <> "" THEN Dif$ = RTRIM$(Dateiname$): dat$ = Dif$

IF RTRIM$(Attribut$) = "L" THEN Dialog "Das verschlÅsselte Programm '" + RTRIM$(OsName$) + "' wird durch eine unverschlÅsselte Version ersetzt."

IF errorlevel = 0 THEN

LSET Symbol$ = sym$
LSET Dateiname$ = dat$
LSET Art$ = Art1$
LSET OsName$ = NachDatei
LSET Ordner$ = NachOrdner
LSET Link$ = Link1$
LSET Datum$ = SDate
LSET Zeit$ = TIME$
LSET Attribut$ = ""
LSET FileInfo$ = Info1$

PUT 1, a
'PrÅfen, ob Installations-Ordner schon existiert, allenfalls erstellen:
IF NOT IsExistBox(NachOrdner) THEN NewBox NachOrdner

ELSE Dialog "Ein Fehler ist aufgetreten beim Erzeugen der Datei " + NachDatei
END IF

Copy Datei$, Dif$ 'Zum Schluss Datei kopieren:

END SUB

SUB ExtractFF (Datei$)
'Sucht in der Dateiliste nach der entsprechenden Datei$ und trÑgt sie dann
'in die Dateitabelle des Users ein, oder kopiert sie einfach nur.
'(FÅr Dateien, welche nicht in der Dateitabelle eingetragen sind)
DIM a AS INTEGER

dat$ = Datei$

IF LaOSexist THEN

a = INSTR(Datei$, ".")
IF a > 0 THEN dat$ = LEFT$(Datei$, a - 1)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$
a = 0
DO
a = a + 1
GET 1, a
IF LCASE$(RTRIM$(Dateiname$)) = LCASE$(dat$) THEN
        Extract RTRIM$(Datei$), RTRIM$(Ordner$), RTRIM$(OsName$), RTRIM$(Symbol$), RTRIM$(Art$), RTRIM$(Link$), RTRIM$(FileInfo$)
        EXIT SUB
END IF
LOOP UNTIL Ltext$(Dateiname$, 1) = ""

END IF

Copy Datei$, ""

END SUB

SUB FileCopy (Source$, Dest$)

IF LCASE$(Source$) = LCASE$(Dest$) THEN EXIT SUB

Register 8, 20, 15, 60, "Kopieren..."
Echo ""
Echo " " + Source$


50012
errorlevel = 0
f% = FREEFILE
OPEN Dest$ FOR BINARY AS f%: CLOSE f%: KILL Dest$

OPEN Source$ FOR BINARY AS f%
 whole = LOF(f%) \ 500
 part = LOF(f%) MOD 500
 buffer$ = STRING$(500, 0)
 start& = 1

IF errorlevel = 1 THEN
    IF LCASE$(Source$) = "c:\laos\laos.pif" THEN EXIT SUB 'Beim Pfad %windir%\StartmenÅ kann es zu Fehlern in bestimmten Win-Versionen kommen -> Fehlermeldung ausblenden
    IF NOT InputYesNo("Fehler beim Lesen der Datei '" + Source$ + "'. Neu versuchen?") THEN EXIT SUB
    GOTO 50012
END IF


f2% = FREEFILE
OPEN Dest$ FOR BINARY AS f2%: CLOSE f2%: KILL Dest$ 'stellt sicher, dass Dest$ leer ist.
OPEN Dest$ FOR BINARY AS f2%
  FOR x = 1 TO whole
       GET f%, start&, buffer$
       PUT f2%, start&, buffer$
       start& = start& + 500
     p% = INT(100 * LOF(f2%) / LOF(f%))
     ProgressBar p%
  NEXT x

 buffer$ = STRING$(part, 0)
  GET f%, start&, buffer$
  PUT f2%, start&, buffer$

ProgressBar 100

CLOSE f%, f2%

IF errorlevel THEN
    IF NOT InputYesNo("Fehler beim Schreiben der Datei '" + Dest$ + "'. Neu versuchen?") THEN EXIT SUB
    GOTO 50012
END IF


END SUB

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

FUNCTION Gerade (Zahl)
IF Zahl / 2 - FIX(Zahl / 2) = 0 THEN Gerade = 1:  ELSE Gerade = 0
END FUNCTION

SUB GetInp
DIM kbd AS STRING
'Aktiviere das Control mit dem Fokus:
SELECT CASE ControlConnect
CASE 0: Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
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
     IF Stoppuhr(i) > 0 THEN
       IF TIMER - AlteZeit(i) > Stoppuhr(i) THEN StoppUhrBeep (i): AlteZeit(i) = TIMER
     END IF
   NEXT i
 WEND
TIMER OFF: FOR i = 15 TO 19: KEY(i) OFF: NEXT i

SELECT CASE kbd
CASE CHR$(0) + "T": ShowInfo 'Manueller Q-Info-Abruf mit Shift+F1
CASE CHR$(13)
SELECT CASE ControlConnect
CASE 0: Focus = MID$(ControlList(ControlNr), 5)
CASE 1: Focus = MID$(MenuList(MenuNr), 5)
END SELECT
Keyb = kbd
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
IF Change(ch) = 0 THEN ch = 0
IF ch <> 0 THEN ControlNr = Change(ch)
Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
END SELECT
CASE CHR$(0) + "H", CHR$(0) + "K": LastControl
CASE CHR$(0) + "P", CHR$(0) + "M": NextControl
CASE CHR$(27)
Keyb = "esc": EXIT SUB'Minimize
END SELECT
LOOP

END SUB

FUNCTION GetKey$

WHILE INKEY$ <> "": WEND

a$ = "": WHILE a$ = "": a$ = INKEY$: WEND

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
END SELECT

GetKey = a$
Keyb = a$
END FUNCTION

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

FUNCTION InputYesNo (Message AS STRING)

Register 8, 20, 15, 60, ""

ClearControls 3
Echo Message
Echo ""
Control 13, 33, "  Ja  "
Control 13, 41, " Nein "

GetInp
IF Focus = "  Ja  " THEN InputYesNo = -1 ELSE InputYesNo = 0

END FUNCTION

SUB InstallWinIcons
TimeLock 1
CLOSE
'Pif-Datei fÅr Windows 95,98 auf dem Desktop installieren
'Deutsche Version von Windows 95,98 erforderlich!
SHELL "C:"
CHDIR "\"

SearchToDirAndCopyPif ("StartmenÅ")
SearchToDirAndCopyPif ("Desktop")


egt% = 0
'Windows 3.0, 3.1, 3.11
'sucht progman.ini und trÑgt darin die neue Programmgruppe (LaOS.grp) ein.
TimeLock 1
LOCATE 2, 3
SHELL "dir progman.ini /b /s >C:\LAOS\pmpath.tmp"
LOCATE 2, 3: PRINT SPACE$(40); 'Fehlermeldung sofort ausblenden!
CLOSE
OPEN "c:\LAOS\pmpath.tmp" FOR BINARY AS 1
IF LOF(1) = 0 THEN EXIT SUB
grz = 0
CLOSE
OPEN "C:\LAOS\pmpath.tmp" FOR INPUT AS 1
LINE INPUT #1, PMPath$

TimeLock 1

CLOSE
FileCopy PMPath$, LEFT$(PMPath$, LEN(PMPath$) - 4) + ".BAK"
OPEN PMPath$ FOR INPUT AS 1
OPEN "C:\LAOS\Progman.tmp" FOR OUTPUT AS 2


errorlevel = 0
DO WHILE NOT EOF(1)

LINE INPUT #1, s$
PRINT #2, s$
IF errorlevel = 1 THEN EXIT DO
IF s$ = "[Groups]" THEN

        DO WHILE NOT EOF(1)

        LINE INPUT #1, s$

        IF LCASE$(LEFT$(s$, 5)) = "group" THEN
                grz1 = VAL(MID$(s$, 6)): PRINT #2, s$
                IF grz1 > grz THEN grz = grz1
        ELSE

        PRINT #2, "Group" + LTRIM$(STR$(grz + 1)) + "=C:\LAOS\LAOS.GRP"
        PRINT #2, s$
        egt% = -1

        EXIT DO
        END IF

        LOOP




END IF '[Group]

LOOP

IF NOT egt% THEN PRINT #2, "Group" + LTRIM$(STR$(grz + 1)) + "=C:\LAOS\LAOS.GRP"
CLOSE
TimeLock 1

FileCopy "C:\LAOS\Progman.tmp", PMPath$
CLOSE
KILL "C:\LAOS\Progman.tmp"
TimeLock 0
END SUB

FUNCTION IsExist (Datei$, InOrdner$)

CLOSE 1: OPEN Ziel + "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN IsExist = -1: EXIT FUNCTION
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = "" THEN IsExist = -1: EXIT FUNCTION
LOOP UNTIL Ltext$(Dateiname$, 1) = ""

IsExist = 0


END FUNCTION

FUNCTION IsExistBox (Box$)

CLOSE 1: OPEN Ziel + "ORDNER.BNK" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ = Box$ THEN IsExistBox = -1: EXIT FUNCTION
LOOP UNTIL a$ = ""

IsExistBox = 0

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
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1
MenuNr = MenuNr - 1
IF MenuNr < 1 THEN MenuNr = 1
Activate MenuList(MenuNr)
END SELECT
END SUB

SUB LoadSchem
DIM m(1 TO 15)

errorlevel = 0
IF NOT exist("c:\laos\config.lcg") THEN errorlevel = 1: EXIT SUB
CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
GET 1, 1, schema$


IF schema$ <> "" THEN
CLOSE 1: OPEN "c:\laos\" + schema$ + ".scl" FOR INPUT AS 1: LINE INPUT #1, a$
FOR i = 1 TO 15
INPUT #1, m(i)
NEXT
ELSE
    errorlevel = 1
END IF

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
InfoTime = m(12)'Setzt die zu verstreichende Zeit in Sekunden, bis die Quick-
                  'Info erscheint. (0=Ausgeschaltet)
InfoColor = m(13) 'Setzt die Farbe, in der der Quick-Info-Text erscheint.
InfoFrame = m(14) 'Setzt die Farbe des Quick-Info-Rahmens.

Datumsformat = m(15)

END SUB

FUNCTION Ltext$ (Text$, ab) 'ab bedeutet: AB diesem Zeichen wird gemessen
IF ab = 0 THEN ab = 1
FOR i = ab TO LEN(Text$) - ab + 1
IF MID$(Text$, i, 1) = " " OR ASC(MID$(Text$, i, 1)) = 0 THEN EXIT FOR
NEXT i
Ltext$ = MID$(Text$, ab, i - ab)
END FUNCTION

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


TextScale = Zeilen + 2
END SUB

FUNCTION MenuInt$ (Item$, Trennzeichen$)
Item$ = Item$ + Trennzeichen$
DIM MenuItem(1 TO 256) AS STRING

i = 0
MaxLenth = 1
DO
i = i + 1
MenuItem(i) = Trim(Item$, Trennzeichen$, i)

IF LEN(MenuItem(i)) > 77 THEN MenuItem(i) = LEFT$(MenuItem(i), 77)
IF LEN(MenuItem(i)) > MaxLenth THEN MaxLenth = LEN(MenuItem(i))

LOOP WHILE MenuItem(i) <> ""

'MaxLenth = MaxLenth + 2 '2 Spalten sind reserviert fÅr die Pfeile

Zeilen = VAL(LEFT$(MenuList(MenuNr), 2)) + 1
Spalten = VAL(MID$(MenuList(MenuNr), 3, 2))
IF Spalten + MaxLenth > 79 THEN Spalten = 79 - MaxLenth


a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + MaxLenth
EndZeile = Zeilen + 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

Groesse = 4 + INT(((PMAP(c + 1, 0) - PMAP(a, 0) + 1) * (1) + 7) / 8) * 4 * (PMAP(d + 1, 1) - PMAP(B, 1) + 1)
DIM Hg(Groesse)
GET (a, B)-(c + 1, d + 1), Hg


i = 0
DO

IF i < 1 THEN i = 1
IF MenuItem(i) = "" THEN i = i - 1
IF i < 1 THEN i = 1

TextBox Zeilen, Spalten, MenuItem(i), MaxLenth

SELECT CASE GetKey
CASE "up", "left": i = i - 1
CASE "down", "right": i = i + 1
CASE CHR$(13): MenuInt$ = MenuItem(i): Choice = i: EXIT DO
CASE "esc": MenuInt$ = "": Choice = 0: Keyb = "esc": EXIT DO
CASE ELSE
END SELECT
LOOP

ERASE MenuItem
PUT (a, B), Hg, PSET

END FUNCTION

SUB NewBox (name$) 'Richtet einen neuen Ordner ein.
IF name$ <> "Papierkorb" AND name$ <> "Versteckt" THEN
CLOSE 1: OPEN Ziel + "ORDNER.BNK" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ = name$ THEN EXIT SUB
LOOP UNTIL a$ = ""

PUT 1, i, name$
errorlevel = 0
END IF

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
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1
tC = 0: DO: tC = tC + 1: LOOP WHILE MenuList(tC) <> ""
MenuNr = MenuNr + 1
IF MenuNr > tC - 1 THEN MenuNr = tC - 1
Activate MenuList(MenuNr)
END SELECT
END SUB

FUNCTION OverwriteAll
STATIC Too, All

IF NOT Too THEN

Register 6, 5, 18, 75, "Es wurde schon ein LaOS gefunden."
Echo ""
Echo "Sie haben offensichtlich schon ein LaOS installiert. WÑhlen Sie"
Echo "'Alle Åberschreiben', um alle Dateien zu aktualisieren. WÑhlen Sie"
Echo "'Einzeln bestÑtigen', um selbst zu bestimmen, welche Dateien Åber-"
Echo "schrieben werden."
Echo ""
Echo "Um alle modernsten Features von LaOS 6 zu integrieren, wÑhlen Sie"
Echo "'Alle Åberschreiben' (empfohlen!)."
ClearControls 0
Control 16, 20, " Alle Åberschreiben "
Control 16, 42, " Einzeln bestÑtigen "
Info " Alle Åberschreiben ", "Alle neuen Programme werden installiert."
GetInp
IF Focus = " Alle Åberschreiben " THEN All = -1 ELSE All = 0
Too = -1

Register 0, 0, 0, 0, "LaOS 6 - Installation"
DrawBCO

END IF

OverwriteAll = All
END FUNCTION

SUB PartFormat (Text$, Setting$)

IF INSTR(Text$, CHR$(0)) THEN
    Setting$ = MID$(Text$, INSTR(Text$, CHR$(0)) + 1)
    Text$ = LEFT$(Text$, INSTR(Text$, CHR$(0)) - 1)
ELSE
    Setting$ = ""
END IF

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

SUB SearchToDirAndCopyPif (Dir$)
CLOSE
LOCATE 2, 3
SHELL "dir " + Dir$ + " /A:D /B /S >C:\LAOS\winpif.tmp"
LOCATE 2, 3: PRINT SPACE$(40);
LOCATE 2, 3
IF NOT exist("C:\LAOS\winpif.tmp") THEN EXIT SUB
OPEN "c:\LAOS\winpif.tmp" FOR INPUT AS 1

WHILE NOT EOF(1)
LINE INPUT #1, a$
TimeLock 1
IF NOT exist("c:\laos\LaOS.Pif") THEN Dialog "Pif-Datei konnte nicht gefunden werden. Eventuell fehlende Komponente in diesem System.": EXIT SUB
FileCopy "c:\laos\LaOS.Pif", a$ + "\LaOS.Pif"
WEND
CLOSE

END SUB

SUB ShowInfo
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
IF s > Right THEN s = Right - LEN(MID$(ControlList(i), 5))

IF z + INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1 > Down THEN z = Down - (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) + 1
IF z < Top THEN
s = Left + 1
z = Down - INT(LEN(QuickInfos(i)) / (Right - s + 1))
END IF

REDIM BG(4 * 8 * 14 * 5) AS DOUBLE
GET (s * 8 - 9, z * 14 - 15)-(s * 8 + (Right - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) * 14), BG
LINE (s * 8 - 9, z * 14 - 15)-(s * 8 + (Right - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) * 14), 0, BF
LINE (s * 8 - 9, z * 14 - 15)-(s * 8 + (Right - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) * 14), InfoFrame, B


ecr = (Right - s + 1) * -1 + 1
FOR a = 1 TO INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1
LOCATE z + a - 1, s
ecr = ecr + Right - s + 1
COLOR InfoColor: PRINT MID$(QuickInfos(i), ecr, Right - s + 1)
NEXT
COLOR Farbe
ecr = 0
a = 0
i = 0
SLEEP
PUT (s * 8 - 9, z * 14 - 15), BG, PSET
END SUB

SUB State (Stat AS INTEGER)
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

SUB StoppUhrBeep (Uhr)
SELECT CASE Uhr
CASE 1: TimeLock (1)
CASE 2: SCREEN , , , 1: COLOR , 0
a$ = "": DO: a$ = INKEY$: LOOP WHILE a$ = ""
SCREEN , , , 0: COLOR Farbe, Hintergrundfarbe
END SELECT
END SUB

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
CASE 0: IF On.too <> 0 THEN On.too = 0: PUT (319 - Radius, 174 - Radius), Hintergrund, PSET: Stoppuhr(1) = 0
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

