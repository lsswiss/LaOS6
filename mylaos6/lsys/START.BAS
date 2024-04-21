DECLARE SUB ShowPicture ()
DECLARE SUB OnPict ()
DECLARE SUB GoDrvClean ()
DECLARE SUB CLW ()
DECLARE SUB FrameWindow (a!, B!, c!, d!, del!)
DECLARE SUB ExitBox (x!, y!)
ON ERROR GOTO error1
SCREEN 9
VIEW PRINT 1 TO 25
WIDTH 80, 25

DIM SHARED Fensterfarbe AS INTEGER, Randfarbe AS INTEGER, Schattenfarbe AS INTEGER, Controlfarbe AS INTEGER
DIM SHARED Symbolactiv AS INTEGER, SymbactStil AS INTEGER, SymbolFrame AS INTEGER, SymbolColor AS INTEGER
DIM SHARED Hintergrundfarbe  AS INTEGER, Farbe AS INTEGER, IconVerticalSpace AS INTEGER
DIM SHARED InfoTime AS INTEGER, InfoColor  AS INTEGER, InfoFrame, Datumsformat AS INTEGER
DIM SHARED xplus AS INTEGER, yplus AS INTEGER

ON ERROR GOTO Fehler
1
SHELL "c:"
CHDIR "c:\laos"
CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
GET 1, 1, Schema$
GET 1, 3, Autoexec$
GET 1, 4, xplus
GET 1, 5, yplus

DIM m(1 TO 15)

IF Schema$ <> "" THEN

IF LOF(1) <> 0 THEN
CLOSE 1: OPEN "c:\laos\" + Schema$ + ".scl" FOR INPUT AS 1: LINE INPUT #1, a$
FOR i = 1 TO 15
INPUT #1, m(i)
NEXT
ELSE m(10) = 15: m(1) = 14
END IF

ELSE GOTO 2
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

ERASE m

CONST Fe1 = 6, Fe2 = 4, Fe3 = 75, Fe4 = 20 'Startfensterkoordinaten
Stoppuhr(2) = 60 '2.Stoppuhr: Bildschirmschoner
ON ERROR GOTO error1

VIEW
COLOR Farbe, Hintergrundfarbe
CLW

FrameWindow 4, 6, 633, 343, 0
LOCATE INT(B / 14) + 1, 2
PRINT "LaOS-Betriebssystem"
LINE (0, 0)-(638, 0), Fensterfarbe: LINE (1, 1)-(637, 1), 0: LINE (1, 1)-(1, 347), 0

ON ERROR GOTO fen
CLOSE 1: OPEN "c:\laos\laostart.lds" FOR BINARY AS 1
IF LOF(1) = 0 THEN OnPict ELSE ShowPicture

LOCATE 24, 3: PRINT "F5 - Fehlerbehebung LaOS-Dateisystem"

'öberprÅfen, ob ein neuer Vorgang eingetragen werden muss: (Anhand von nh.go)
CLOSE 1: OPEN "c:\laos\nh.go" FOR BINARY AS 1
IF LOF(1) <> 0 THEN

TYPE handlertype
    Vorgangsname AS STRING * 30
    Anwendung AS STRING * 12
    SubID AS STRING * 8
END TYPE
DIM handler AS handlertype

CLOSE 1: OPEN "c:\laos\nh.go" FOR INPUT AS 1
    DO WHILE NOT EOF(1)
        IF errorlevel THEN EXIT DO
        INPUT #1, a$, B$
        INPUT #1, c$
IF a$ = "" OR B$ = "" THEN EXIT DO

OPEN "c:\laos\handler.rsc" FOR RANDOM AS 2
i% = 0
DO
i% = i% + 1
GET 2, i%, handler
IF handler.Vorgangsname = STRING$(30, CHR$(0)) OR RTRIM$(handler.Vorgangsname) = "" THEN
        handler.Vorgangsname = a$
        handler.Anwendung = B$
        IF LEFT$(c$, 1) = "/" THEN c$ = MID$(c$, 2)
        handler.SubID = c$
        PUT 2, i%, handler
    EXIT DO
END IF
LOOP

    LOOP

CLOSE
KILL "c:\laos\nh.go"
END IF
CLOSE

az = TIMER
Right = 76

a$ = ""
DO
a$ = INKEY$
IF TIMER - az > 3 THEN EXIT DO
IF a$ = CHR$(0) + "?" THEN GoDrvClean

LOOP UNTIL a$ = CHR$(13) OR a$ = CHR$(27)

ON ERROR GOTO fen
IF Autoexec$ <> "" THEN CHAIN "C:\LAOS\" + Autoexec$

ON ERROR GOTO error1
CHAIN "c:\laos\run.bas"


error1:
SCREEN 0
CLS
COLOR 15
PRINT "UngÅltiges System"
SYSTEM

Fehler:
RESUME 2
2
ON ERROR GOTO error1

CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
Schema$ = "AUTO"
PUT 1, 1, Schema$

CLOSE 1: OPEN "c:\laos\auto.scl" FOR OUTPUT AS #1
PRINT #1, "Automatisch"
PRINT #1, "8"
PRINT #1, "14"
PRINT #1, "8"
PRINT #1, "8"
PRINT #1, "8"
PRINT #1, "16000"
PRINT #1, "0"
PRINT #1, "0"
PRINT #1, "63"
PRINT #1, "8"
PRINT #1, "1"
PRINT #1, "7"
PRINT #1, "2"
PRINT #1, "1"
PRINT #1, "0"
CLOSE 1
RUN

fen:
errorlevel = 1
RESUME NEXT

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

SUB GoDrvClean

ON ERROR GOTO fen

CHAIN "c:\laos\drvclean.lao"

DO
CLS
PRINT "DrvClean konnte nicht gefunden werden. Suchpfad? (z.B. A:\LSYS\)"
LINE INPUT a$
IF a$ = "" THEN SYSTEM
CHAIN a$ + "drvclean.lao"
LOOP

END SUB

SUB OnPict
LINE (129, 70)-(495, 269), 8, BF
LINE (126, 66)-(492, 265), 3, BF
LINE (195, 84)-(415, 224), 15, BF
LINE (415, 224)-(195, 84), 7, B
LINE (197, 86)-(413, 222), 8, B
LINE (199, 182)-(219, 162), 3
LINE (239, 182)-(219, 162), 3
LINE (239, 182)-(279, 142), 3
LINE (279, 142)-(299, 162), 3
LINE (299, 162)-(319, 142), 3
LINE (319, 142)-(359, 182), 3
LINE (359, 182)-(339, 202), 3
LINE (339, 202)-(319, 182), 3
LINE (319, 182)-(299, 202), 3
LINE (299, 202)-(279, 220), 3
LINE (277, 220)-(411, 220), 3
LINE (411, 220)-(411, 160), 3
LINE (391, 140)-(411, 160), 3
LINE (391, 140)-(339, 164), 3
LINE (239, 220)-(283, 180), 3
LINE (239, 182)-(201, 220), 3
LINE (279, 220)-(199, 220), 3
LINE (199, 182)-(199, 220), 3
PAINT (219, 180), 11, 3
LINE (299, 160)-(279, 180), 3
LINE (283, 180)-(279, 180), 3
PAINT (301, 180), 3, 3
PAINT (261, 180), 7, 3
LINE (359, 182)-(395, 218), 3
PAINT (385, 178), 11, 3
LINE (323, 198)-(309, 212), 8
LINE (313, 212)-(319, 206), 8
LINE (321, 208)-(325, 204), 8
LINE (361, 198)-(357, 200), 8
LINE (361, 200)-(355, 204), 8
LINE (361, 204)-(365, 202), 8
LINE (363, 200)-(359, 204), 8
LINE (347, 212)-(351, 210), 8
LINE (349, 214)-(351, 214), 8
LINE (355, 214)-(351, 212), 8
CIRCLE (373, 112), 20, 14, , , .8
PAINT (373, 112), 14, 14
LINE (371, 132)-(371, 142), 14
LINE (341, 142)-(355, 128), 14
LINE (359, 94)-(357, 92), 14
LINE (377, 90)-(377, 92), 14
LINE (393, 96)-(401, 88), 14
LINE (397, 108)-(411, 108), 14
LINE (393, 126)-(407, 136), 14
LINE (349, 112)-(327, 112), 14
LINE (355, 98)-(343, 90), 14
LINE (203, 92)-(203, 122), 11
LINE (204, 92)-(204, 122), 11
LINE (203, 122)-(218, 122), 11
LINE (204, 122)-(219, 122), 11
LINE (239, 122)-(236, 119), 11
LINE (240, 122)-(237, 119), 11
LINE (236, 119)-(236, 110), 11
LINE (237, 119)-(237, 110), 11
LINE (236, 110)-(233, 107), 11
LINE (237, 110)-(234, 107), 11
LINE (233, 107)-(227, 107), 11
LINE (234, 107)-(228, 107), 11
LINE (227, 107)-(224, 110), 11
LINE (228, 107)-(225, 110), 11
LINE (224, 110)-(224, 119), 11
LINE (225, 110)-(225, 119), 11
LINE (224, 119)-(227, 122), 11
LINE (225, 119)-(228, 122), 11
LINE (227, 122)-(233, 122), 11
LINE (228, 122)-(234, 122), 11
LINE (233, 122)-(236, 119), 11
LINE (234, 122)-(237, 119), 11
LINE (248, 92)-(245, 95), 11
LINE (249, 92)-(246, 95), 11
LINE (245, 95)-(245, 119), 11
LINE (246, 95)-(246, 119), 11
LINE (245, 119)-(248, 122), 11
LINE (246, 119)-(249, 122), 11
LINE (248, 122)-(260, 122), 11
LINE (249, 122)-(261, 122), 11
LINE (260, 122)-(263, 119), 11
LINE (261, 122)-(264, 119), 11
LINE (263, 119)-(263, 95), 11
LINE (264, 119)-(264, 95), 11
LINE (263, 95)-(260, 92), 11
LINE (264, 95)-(261, 92), 11
LINE (260, 92)-(248, 92), 11
LINE (261, 92)-(249, 92), 11
LINE (284, 95)-(281, 92), 11
LINE (285, 95)-(282, 92), 11
LINE (281, 92)-(272, 92), 11
LINE (282, 92)-(273, 92), 11
LINE (272, 92)-(269, 95), 11
LINE (273, 92)-(270, 95), 11
LINE (269, 95)-(269, 104), 11
LINE (270, 95)-(270, 104), 11
LINE (269, 104)-(272, 107), 11
LINE (270, 104)-(273, 107), 11
LINE (272, 107)-(281, 107), 11
LINE (273, 107)-(282, 107), 11
LINE (281, 107)-(284, 110), 11
LINE (282, 107)-(285, 110), 11
LINE (284, 110)-(284, 119), 11
LINE (285, 110)-(285, 119), 11
LINE (284, 119)-(281, 122), 11
LINE (285, 119)-(282, 122), 11
LINE (281, 122)-(272, 122), 11
LINE (282, 122)-(273, 122), 11
LINE (272, 122)-(269, 119), 11
LINE (273, 122)-(270, 119), 11
LINE (311, 119)-(314, 122), 11
LINE (312, 119)-(315, 122), 11
LINE (314, 122)-(323, 122), 11
LINE (315, 122)-(324, 122), 11
LINE (323, 122)-(326, 119), 11
LINE (324, 122)-(327, 119), 11
LINE (314, 107)-(323, 107), 11
LINE (315, 107)-(324, 107), 11
LINE (323, 107)-(326, 110), 11
LINE (324, 107)-(327, 110), 11
LINE (326, 110)-(326, 119), 11
LINE (327, 110)-(327, 119), 11
LINE (311, 119)-(311, 95), 11
LINE (312, 119)-(312, 95), 11
LINE (311, 95)-(314, 92), 11
LINE (312, 95)-(315, 92), 11
LINE (314, 92)-(323, 92), 11
LINE (315, 92)-(324, 92), 11
LINE (323, 92)-(326, 95), 11
LINE (324, 92)-(327, 95), 11
LINE (314, 107)-(311, 110), 11
LINE (315, 107)-(312, 110), 11
LINE (202, 91)-(202, 121), 8
LINE (203, 91)-(203, 121), 8
LINE (202, 121)-(217, 121), 8
LINE (203, 121)-(218, 121), 8
LINE (238, 121)-(235, 118), 8
LINE (239, 121)-(236, 118), 8
LINE (235, 118)-(235, 109), 8
LINE (236, 118)-(236, 109), 8
LINE (235, 109)-(232, 106), 8
LINE (236, 109)-(233, 106), 8
LINE (232, 106)-(226, 106), 8
LINE (233, 106)-(227, 106), 8
LINE (226, 106)-(223, 109), 8
LINE (227, 106)-(224, 109), 8
LINE (223, 109)-(223, 118), 8
LINE (224, 109)-(224, 118), 8
LINE (223, 118)-(226, 121), 8
LINE (224, 118)-(227, 121), 8
LINE (226, 121)-(232, 121), 8
LINE (227, 121)-(233, 121), 8
LINE (232, 121)-(235, 118), 8
LINE (233, 121)-(236, 118), 8
LINE (247, 91)-(244, 94), 8
LINE (248, 91)-(245, 94), 8
LINE (244, 94)-(244, 118), 8
LINE (245, 94)-(245, 118), 8
LINE (244, 118)-(247, 121), 8
LINE (245, 118)-(248, 121), 8
LINE (247, 121)-(259, 121), 8
LINE (248, 121)-(260, 121), 8
LINE (259, 121)-(262, 118), 8
LINE (260, 121)-(263, 118), 8
LINE (262, 118)-(262, 94), 8
LINE (263, 118)-(263, 94), 8
LINE (262, 94)-(259, 91), 8
LINE (263, 94)-(260, 91), 8
LINE (259, 91)-(247, 91), 8
LINE (260, 91)-(248, 91), 8
LINE (283, 94)-(280, 91), 8
LINE (284, 94)-(281, 91), 8
LINE (280, 91)-(271, 91), 8
LINE (281, 91)-(272, 91), 8
LINE (271, 91)-(268, 94), 8
LINE (272, 91)-(269, 94), 8
LINE (268, 94)-(268, 103), 8
LINE (269, 94)-(269, 103), 8
LINE (268, 103)-(271, 106), 8
LINE (269, 103)-(272, 106), 8
LINE (271, 106)-(280, 106), 8
LINE (272, 106)-(281, 106), 8
LINE (280, 106)-(283, 109), 8
LINE (281, 106)-(284, 109), 8
LINE (283, 109)-(283, 118), 8
LINE (284, 109)-(284, 118), 8
LINE (283, 118)-(280, 121), 8
LINE (284, 118)-(281, 121), 8
LINE (280, 121)-(271, 121), 8
LINE (281, 121)-(272, 121), 8
LINE (271, 121)-(268, 118), 8
LINE (272, 121)-(269, 118), 8
LINE (310, 118)-(313, 121), 8
LINE (311, 118)-(314, 121), 8
LINE (313, 121)-(322, 121), 8
LINE (314, 121)-(323, 121), 8
LINE (322, 121)-(325, 118), 8
LINE (323, 121)-(326, 118), 8
LINE (313, 106)-(322, 106), 8
LINE (314, 106)-(323, 106), 8
LINE (322, 106)-(325, 109), 8
LINE (323, 106)-(326, 109), 8
LINE (325, 109)-(325, 118), 8
LINE (326, 109)-(326, 118), 8
LINE (310, 118)-(310, 94), 8
LINE (311, 118)-(311, 94), 8
LINE (310, 94)-(313, 91), 8
LINE (311, 94)-(314, 91), 8
LINE (313, 91)-(322, 91), 8
LINE (314, 91)-(323, 91), 8
LINE (322, 91)-(325, 94), 8
LINE (323, 91)-(326, 94), 8
LINE (313, 106)-(310, 109), 8
LINE (314, 106)-(311, 109), 8
LINE (191, 245)-(192, 246), 8
LINE (192, 246)-(196, 246), 8
LINE (196, 246)-(197, 245), 8
LINE (191, 245)-(191, 237), 8
LINE (191, 237)-(192, 236), 8
LINE (192, 236)-(196, 236), 8
LINE (196, 236)-(197, 237), 8
LINE (199, 242)-(199, 245), 8
LINE (199, 245)-(200, 246), 8
LINE (200, 246)-(203, 246), 8
LINE (203, 246)-(204, 245), 8
LINE (204, 245)-(204, 242), 8
LINE (204, 242)-(203, 241), 8
LINE (203, 241)-(200, 241), 8
LINE (200, 241)-(199, 242), 8
LINE (206, 250)-(206, 241), 8
LINE (206, 246)-(209, 246), 8
LINE (209, 246)-(210, 245), 8
LINE (210, 245)-(210, 242), 8
LINE (210, 242)-(209, 241), 8
LINE (209, 241)-(206, 241), 8
LINE (212, 241)-(215, 246), 8
LINE (218, 241)-(215, 246), 8
LINE (213, 250)-(215, 246), 8
LINE (220, 246)-(220, 241), 8
LINE (220, 242)-(221, 241), 8
LINE (221, 241)-(223, 241), 8
LINE (223, 241)-(224, 242), 8
LINE (226, 246)-(226, 240), 8
LINE (226, 237)-(226, 236), 8
LINE (228, 242)-(229, 241), 8
LINE (229, 241)-(232, 241), 8
LINE (232, 241)-(233, 242), 8
LINE (233, 242)-(233, 245), 8
LINE (233, 245)-(232, 246), 8
LINE (232, 246)-(229, 246), 8
LINE (229, 246)-(228, 245), 8
LINE (228, 245)-(228, 242), 8
LINE (233, 245)-(233, 249), 8
LINE (233, 249)-(232, 250), 8
LINE (232, 250)-(229, 250), 8
LINE (229, 250)-(228, 249), 8
LINE (235, 236)-(235, 246), 8
LINE (235, 246)-(235, 242), 8
LINE (235, 242)-(236, 241), 8
LINE (236, 241)-(239, 241), 8
LINE (239, 241)-(240, 242), 8
LINE (240, 242)-(240, 246), 8
LINE (244, 239)-(244, 245), 8
LINE (244, 245)-(245, 246), 8
LINE (245, 246)-(246, 246), 8
LINE (246, 246)-(247, 245), 8
LINE (245, 241)-(243, 241), 8
LINE (244, 239)-(244, 238), 8
LINE (256, 242)-(256, 244), 8
LINE (256, 244)-(257, 246), 8
LINE (256, 244)-(256, 238), 8
LINE (256, 238)-(257, 236), 8
LINE (259, 245)-(260, 246), 8
LINE (260, 246)-(264, 246), 8
LINE (264, 246)-(265, 245), 8
LINE (259, 245)-(259, 237), 8
LINE (259, 237)-(260, 236), 8
LINE (260, 236)-(264, 236), 8
LINE (264, 236)-(265, 237), 8
LINE (267, 236)-(268, 238), 8
LINE (268, 238)-(268, 244), 8
LINE (268, 244)-(267, 246), 8
LINE (273, 239)-(276, 236), 8
LINE (276, 236)-(276, 246), 8
LINE (283, 237)-(282, 236), 8
LINE (282, 236)-(279, 236), 8
LINE (279, 236)-(278, 237), 8
LINE (278, 237)-(278, 240), 8
LINE (278, 240)-(279, 241), 8
LINE (279, 241)-(282, 241), 8
LINE (282, 241)-(283, 240), 8
LINE (283, 240)-(283, 237), 8
LINE (283, 240)-(283, 245), 8
LINE (283, 245)-(282, 246), 8
LINE (282, 246)-(279, 246), 8
LINE (279, 246)-(278, 245), 8
LINE (290, 237)-(289, 236), 8
LINE (289, 236)-(286, 236), 8
LINE (286, 236)-(285, 237), 8
LINE (285, 237)-(285, 240), 8
LINE (285, 240)-(286, 241), 8
LINE (286, 241)-(289, 241), 8
LINE (289, 241)-(290, 240), 8
LINE (290, 240)-(290, 237), 8
LINE (290, 240)-(290, 245), 8
LINE (290, 245)-(289, 246), 8
LINE (289, 246)-(286, 246), 8
LINE (286, 246)-(285, 245), 8
LINE (293, 241)-(292, 242), 8
LINE (292, 242)-(292, 245), 8
LINE (292, 245)-(293, 246), 8
LINE (293, 246)-(296, 246), 8
LINE (296, 246)-(297, 245), 8
LINE (297, 245)-(297, 242), 8
LINE (297, 242)-(296, 241), 8
LINE (296, 241)-(293, 241), 8
LINE (293, 241)-(292, 240), 8
LINE (292, 240)-(292, 237), 8
LINE (292, 237)-(293, 236), 8
LINE (293, 236)-(296, 236), 8
LINE (296, 236)-(297, 237), 8
LINE (297, 237)-(297, 240), 8
LINE (297, 240)-(296, 241), 8
LINE (299, 241)-(304, 241), 8
LINE (306, 237)-(307, 236), 8
LINE (307, 236)-(310, 236), 8
LINE (310, 236)-(311, 237), 8
LINE (311, 237)-(311, 240), 8
LINE (311, 240)-(310, 241), 8
LINE (310, 241)-(307, 241), 8
LINE (307, 241)-(306, 242), 8
LINE (306, 242)-(306, 245), 8
LINE (306, 245)-(307, 246), 8
LINE (307, 246)-(310, 246), 8
LINE (311, 246)-(310, 246), 8
LINE (313, 237)-(313, 245), 8
LINE (313, 245)-(314, 246), 8
LINE (314, 246)-(317, 246), 8
LINE (317, 246)-(318, 245), 8
LINE (318, 245)-(318, 237), 8
LINE (318, 237)-(317, 236), 8
LINE (317, 236)-(314, 236), 8
LINE (314, 236)-(313, 237), 8
LINE (313, 245)-(318, 237), 8
LINE (320, 237)-(320, 245), 8
LINE (320, 245)-(321, 246), 8
LINE (321, 246)-(324, 246), 8
LINE (324, 246)-(325, 245), 8
LINE (325, 245)-(325, 237), 8
LINE (325, 237)-(324, 236), 8
LINE (324, 236)-(321, 236), 8
LINE (321, 236)-(320, 237), 8
LINE (320, 245)-(325, 237), 8
LINE (327, 237)-(328, 236), 8
LINE (328, 236)-(331, 236), 8
LINE (331, 236)-(332, 237), 8
LINE (332, 237)-(332, 240), 8
LINE (332, 240)-(331, 241), 8
LINE (331, 241)-(328, 241), 8
LINE (328, 241)-(327, 242), 8
LINE (327, 242)-(327, 245), 8
LINE (327, 245)-(328, 246), 8
LINE (328, 246)-(331, 246), 8
LINE (332, 246)-(331, 246), 8
LINE (335, 246)-(334, 247), 8
LINE (334, 247)-(335, 247), 8
LINE (335, 247)-(336, 246), 8
LINE (336, 246)-(335, 246), 8
LINE (342, 236)-(342, 245), 8
LINE (342, 245)-(343, 246), 8
LINE (343, 246)-(347, 246), 8
LINE (347, 246)-(348, 245), 8
LINE (348, 245)-(348, 236), 8
LINE (350, 246)-(350, 241), 8
LINE (350, 242)-(351, 241), 8
LINE (351, 241)-(353, 241), 8
LINE (353, 241)-(354, 242), 8
LINE (360, 241)-(359, 240), 8
LINE (359, 240)-(357, 240), 8
LINE (357, 240)-(356, 241), 8
LINE (356, 241)-(356, 242), 8
LINE (356, 242)-(357, 243), 8
LINE (357, 243)-(359, 243), 8
LINE (359, 243)-(360, 244), 8
LINE (360, 244)-(360, 245), 8
LINE (360, 245)-(359, 246), 8
LINE (359, 246)-(357, 246), 8
LINE (357, 246)-(356, 245), 8
LINE (368, 236)-(368, 246), 8
LINE (368, 246)-(373, 246), 8
LINE (380, 246)-(379, 245), 8
LINE (379, 245)-(379, 242), 8
LINE (379, 242)-(378, 241), 8
LINE (378, 241)-(376, 241), 8
LINE (376, 241)-(375, 242), 8
LINE (375, 242)-(375, 245), 8
LINE (375, 245)-(376, 246), 8
LINE (376, 246)-(378, 246), 8
LINE (378, 246)-(379, 245), 8
LINE (382, 246)-(382, 241), 8
LINE (382, 242)-(383, 241), 8
LINE (383, 241)-(386, 241), 8
LINE (386, 241)-(387, 242), 8
LINE (387, 242)-(387, 246), 8
LINE (389, 242)-(390, 241), 8
LINE (390, 241)-(393, 241), 8
LINE (393, 241)-(394, 242), 8
LINE (394, 242)-(394, 245), 8
LINE (394, 245)-(393, 246), 8
LINE (393, 246)-(390, 246), 8
LINE (390, 246)-(389, 245), 8
LINE (389, 245)-(389, 242), 8
LINE (394, 245)-(394, 249), 8
LINE (394, 249)-(393, 250), 8
LINE (393, 250)-(390, 250), 8
LINE (390, 250)-(389, 249), 8
LINE (396, 246)-(396, 241), 8
LINE (396, 242)-(397, 241), 8
LINE (397, 241)-(399, 241), 8
LINE (399, 241)-(400, 242), 8
LINE (400, 242)-(400, 246), 8
LINE (400, 242)-(401, 241), 8
LINE (403, 241)-(401, 241), 8
LINE (403, 241)-(404, 242), 8
LINE (404, 242)-(404, 246), 8
LINE (408, 243)-(410, 243), 8
LINE (410, 243)-(410, 242), 8
LINE (410, 242)-(409, 241), 8
LINE (409, 241)-(407, 241), 8
LINE (407, 241)-(406, 242), 8
LINE (406, 242)-(406, 245), 8
LINE (406, 245)-(407, 246), 8
LINE (407, 246)-(409, 246), 8
LINE (409, 246)-(410, 245), 8
LINE (408, 243)-(406, 243), 8
LINE (412, 246)-(412, 240), 8
LINE (412, 237)-(412, 236), 8
LINE (416, 243)-(418, 243), 8
LINE (418, 243)-(418, 242), 8
LINE (418, 242)-(417, 241), 8
LINE (417, 241)-(415, 241), 8
LINE (415, 241)-(414, 242), 8
LINE (414, 242)-(414, 245), 8
LINE (414, 245)-(415, 246), 8
LINE (415, 246)-(417, 246), 8
LINE (417, 246)-(418, 245), 8
LINE (416, 243)-(414, 243), 8
LINE (420, 246)-(420, 241), 8
LINE (420, 242)-(421, 241), 8
LINE (421, 241)-(423, 241), 8
LINE (423, 241)-(424, 242), 8

END SUB

SUB ShowPicture
VIEW (10, 15)-(628, 315)
xplus = xplus - 10
yplus = yplus - 15
Streck = 1

DIM i AS INTEGER
DIM Nr AS INTEGER
DIM Befehle(5) AS SINGLE
DIM Bef2(5) AS SINGLE

CLOSE 1: OPEN "c:\laos\laostart.lds" FOR BINARY AS 1

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

END SUB

