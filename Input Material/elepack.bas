10 REM ===========================================================================
20 REM                       ELEPACK - pacchetto di routines di interesse
30 REM                       generale in elettronica.
40 REM
50 REM                       Massimo Corinaldesi - rev.1 dic. 1984
60 REM ============================================================================
70 DEF FNPC$(IR%, IC%)=CHR$(27)+CHR$(61)+CHR$(IR%+32)+CHR$(IC%+32)
80 PG#=3.14159265359#: LCC=&HFF79 : REM LOCAZIONE CARATTERE CURSORE
90 ON ERROR GOTO 0 : PRINT CHR$(26); "max '84" : PRINT
100 PRINT "     ======================================================="
110 PRINT "==== COMPENDIO DI ROUTINES DI UTILITA' NEL CAMPO ELETTRONICO ===="
120 PRINT "     ======================================================="
130 PRINT : PRINT : PRINT
140 PRINT "-----------------------------------------------------------------"
150 PRINT ":                                                               :"
160 PRINT ":             Routines disponibili:                             :"
170 PRINT ":                                                               :"
180 PRINT ":             1 *****        TABELLE DI VALORI EIA              :"
190 PRINT ":             2 *****        TOTALIZZATORE DI COMPONENTI        :"
200 PRINT ":             3 *****        CALCOLATRICE                       :"
210 PRINT ":             4 *****        DIAGRAMMI DI BODE                  :"
220 PRINT ":             9 *****        fine                               :"
230 PRINT ":                                                               :"
240 PRINT "-----------------------------------------------------------------"
250 PRINT : PRINT : PRINT
260 POKE LCC,13: PRINT "quale scegli ? :"; : R$=INPUT$(1) : PRINT
270 IF R$="9" THEN POKE LCC,95 : STOP
280 IF INSTR("1234",R$)=0 THEN PRINT CHR$(11); : GOTO 260
290 ON VAL(R$) GOTO 330,770,1300,3770
300 REM -----------------------------------------------------------------------
310 REM                  GENERAZIONE DI TABELLE NUMERICHE DALLA SERIE EIA 10%
320 REM -----------------------------------------------------------------------
330 DIM R(12), V(12,12): REM MATRICI PER LE TABELLE EIA
340 DATA 1,1.2,1.5,1.0,2.2,2.7,3.3,3.9,4.7,5.6,6.8,8.2
350 FOR K%=1 TO 12 : READ R(K%) : NEXT K% : RESTORE 340
360 T$(1)="tabella dei paralleli": T$(2)="tabella delle serie"
370 T$(3)="tabella dei partitori": T$(4)="tabella dei prodotti"
380 T$(5)="tabella dei rapporti": T$(6)="tabella delle frequenze naturali" 
390 T$(7)="tabella delle frequenze risonanti"
400 PRINT CHR$(26); "max '84" : PRINT
410 PRINT "GENERAZIONE DI TABELLE NUMERICHE OTTENUTE DALLA SERIE EIA AL 10%."
420 PRINT:PRINT:PRINT : PRINT "tabelle disponibili:" : PRINT
430 PRINT "   1 ..... PARALLELI"
440 PRINT "   2 ..... SERIE"
450 PRINT "   3 ..... PARTITORI"
460 PRINT "   4 ..... PRODOTTI"
470 PRINT "   5 ..... RAPPORTI"
480 PRINT "   6 ..... FREQ. NATURALI"
490 PRINT "   7 ..... FREQ. RISONANTI"
500 PRINT "   9 ..... ritorno al menu": PRINT:PRINT:PRINT:PRINT
510 PRINT "quale scegli?:"; : R$=INPUT$(1) : PRINT
520 IF R$="9" THEN ERASE R,V,T$ : GOTO 90
530 IF INSTR("1234567",R$) = 0 THEN PRINT CHR$(11); : GOTO 510
540 IF R$ <> "3" THEN GOTO 550 ELSE INPUT "riferiti a che tensione "; TV
550 POKE LCC,32 : PRINT CHR$(26); "sto calcolando" : N% = VAL(R$)
560 FOR I% = 1 TO 12:FOR J% = 1 TO 12
570 ON N% GOTO 580,590,600,610,620,630,640
580 V(I%, J%) = R(I%) * R(J%) / (R(I%) + R(J%)) : GOTO 650
590 V(I%, J%) = R(I%) + R(J%) : GOTO 650
600 V(I%, J%) = TV * R(I%) / (R(I%) + R(J%)) : GOTO 650
610 V(I%, J%) = R(I%) * R(J%) : GOTO 650
620 V(I%, J%) = R(I%) / R(J%) : GOTO 650
630 V(I%, J%) = 10 / (R(I%) * R(J%) * 2 * PG#) : GOTO 650
640 V(I%, J%) = 10 / SQR(R(I%) * R(J%) * 2 * PG#)
650 NEXT J%: NEXT I%
660 PRINT CHR$(26); "max '84"; TAB(20); T$(N%); "tra valori EIA 10%"
670 PRINT : PRINT : PRINT "         ";
680 FOR K% = 1 TO 12 : PRINT USING " #.# :"; R(K%); : NEXT K% : PRINT
690 PRINT STRING$(78, "-")
700 FOR K% = 1 TO 12 : PRINT USING " #.# :"; R(K%);
710 FOR L% = 1 TO 12 : PRINT USING "##.##:"; V(K%, L%); : NEXT L% : PRINT : NEXT K%
720 PRINT STRING$(78, "-"); : PRINT : PRINT : PRINT
730 PRINT "'return' per continuare"; : K$ = INPUT$(1) : POKE LCC,13 : GOT0 400
740 REM ----------------------------------------------------------
750 REM                    TOTALIZZATORE DI COMPONENTI
760 REM ----------------------------------------------------------
770 NM% = 99 : REM MAX NUMERO (DISPARI) DI VALORI DIVERSI TRA LORO
780 DIM VS(NM% + l), VN%(NM% + 1), F$(NM% + 1)
790 FOR K% = O TO NM% : VS(K%) = O : VN%(K%) = O : NEXT K% : NC% = O : NI% = O
800 PRINT CHR$(26); "max '84"; PRINT
810 PRINT "TOTALIZZATORE DEL NUMERO DI COMPONENTI EGUALI IN UNA LISTA."
820 PRINT : PRINT "(introduci 'E' per terminare la lista)"
830 PRINT FNPC$(20,0); "Totale di "; NC%; " componenti"
840 PRINT FNPC$(10,0); CHR$(24);
850 INPUT "VALORE DEL COMPONENTE : "; VC$
860 IF VC$ = "E" THEN GOTO 920 ELSE VC = VAL(VC$)
870 IF VC <= O THEN GOTO 830 ELSE N1% = 1 : NC% = NC% + 1
B80 IF VS(N1%) = VC THEN VN%(N1%) = VN%(N1%) + 1 : GOTO 830
890 IF VS(N1%) = 0 THEN VS(N1%) = VC : VN%(N1%) = 1 : NI% = N1% : GOTO 830 ELSE N1% = N1% + 1
900 IF N1% < NM% + 1 THEN GOTO 880 : ELSE PRINT "!!! TROPPI COMPONENTI !!!" : STOP
910 REM------ ORDINAMENTO DEI VALORI E PRESENTAZIONE --------
920 PRINT CHR$(26); "sto ordinando i valori"
930 FOR I% = 1 TO NI% + 1
940 IF VS(I%) < 10000 THEN F$(I%) = "####." ELSE F$(I%) = "##.###"
950 IF VS(I%) < 100 THEN F$(I%) = "###.#"
960 IF VS(I%) < 10 THEN F$(I%) = "##.##"
970 NEXT I%
980 FOR I% = 1 TO NI% - 1 : FOR J% = I% + 1 TO NI%
990 IF VS(J%) < VS(I%) THEN SWAP VS(I%), VS(J%) : SWAP VN%(I%), VN%(J%)
1000 NEXT J% : NEXT I%
1010 PRINT CHR$(26); "-max"; STRING$(46,"-")
1020 PRINT "TOTALE DI "; NC%; " COMPONENTI E "; NI%; "VALORI DIVERSI." : PRINT
1030 PRINT STRING$(50, "-")
1040 PRINT "TABELLA NUMERO DEI COMPONENTI PER SINGOLO VALORE: " : PRINT
1050 FOR N2% = 1 TO NI% STEP 2
1060 PRINT TAB(5); VN%(N2%); TAB(8); USING F$(N2%); VS(N2%);
1070 IF VN%(N2% + 1) = 0 THEN PRINT : GOTO 1090
1080 PRINT TAB(30); VN%(N2% + 1); TAB(33); USING F$(N2%+1); VS(N2%+l)
1090 NEXT N2% : PRINT : PRINT
1100 PRINT STRING$(50, "-") : PRINT
1110 PRINT "[ Continua, Reinizia, Stampa, Menu' ]"; : R$ = INPUT$(1)
1120 IF R$ = "C" THEN GOTO 800
1130 IF R$ = "R" THEN GOTO 790
1140 IF R$ = "M" THEN ERASE VS, VN%, F$ : GOTO 90
1150 IF R$ <> "S" THEN PRINT CHR$(11); CHR$(24); : GOTO 1110
1160 LPRINT: LPRINT: LPRINT
1170 LPRINT "=max"; STRING$(46, "=")
1180 LPRINT "TOTALE DI "; NC%; "COMPONENTI E "; NI%; " VALORI DIVERSI."
1190 LPRINT STRING$(50, "=")
1200 LPRINT "TABELLA NUMERO DEI COMPONENTI PER SINGOLO VALORE :" : LPRINT
1210 FOR N2% = 1 TO NI% STEP 2
1220 LPRINT TAB(S); VN%(N2%); TAB(8); USING F$(N2%); VS(N2%);
1230 IF VN%(N2% + 1) = 0 THEN LPRINT : GOTO 1250
1240 LPRINT TAB(30); VN%(N2% + l); TAB(33); USING F$(N2% + l); VS(N2% + 1)
1250 NEXT N2% : LPRINT : LPRINT STRING$(50, "=")
1260 LPRINT : LPRINT : LPRINT : LPRINT : PRINT CHR$(11); CHR$(24); : GOTO 1110
1270 REM------------------------------------------------------------------
1280 REM                       SIMULATORE DI CALCOLATRICE TASCABILE
1290 REM------------------------------------------------------------------
1300 NMOP% = 50: DIM S$(NMOP%) : REM MAX NUMERO TRA OPERATORI E NUMERI
1310 LN10# = 2.302585093# : POKE LCC,32 : XK$ = "0" : ND% = 15 : EE% = -1 : GOSUB 3260
1320 MANG$ = "deg" : GRA# = 180# : MEM# = 0 : NERR% = 0 : ON ERROR GOTO 3240
1330 FLAG% = 1 : NS% = 1 : NOP% = 0 : NPA% = 0
1340 GOSUB 3560 : REM AGGIORNA IL DISPLAY
1350 K$ = INPUT$(1)
1360 IF K$ = "u" THEN ERASE S$ : GOTO 90
1370 NERR% = 0 : N# = VAL(S$(NS%))
1380 IF INSTR("1234567890.", K$) <> 0 THEN GOTO 2910
1390 IF INSTR("+-*/#Y", K$) <> 0 THEN GOTO 2760
1400 IF K$ = "=" THEN GOTO 2860
1410 IF K$ = "A" THEN GOTO 1610
1420 IF K$ = "(" THEN GOTO 2650
1430 IF K$ = ")" THEN GOTO 2670
1440 IF K$ = "V" THEN GOTO 2600
1450 IF K$ = "^" THEN GOTO 2630
1460 IF K$ = "P" THEN GOTO 2570
1470 IF K$ = "\" THEN GOTO 2550
1480 IF K$ = "N" THEN GOTO 2520
1490 IF K$ = "L" THEN GOTO 2460
1500 IF K$ = "M" THEN GOTO 2360
1510 IF K$ = "C" THEN GOTO 2300
1520 IF K$ = "D" THEN GOTO 2060
1530 IF K$ = ">" THEN GOTO 2040
1540 IF K$ = "<" THEN GOTO 2020
1550 IF K$ = "I" THEN GOTO 1980
1560 IF K$ = "E" THEN GOTO 1890
1570 IF K$ = "S" THEN GOTO 1830
1580 IF K$ = "H" THEN GOTO 1690
1590 IF K$ = "B" THEN GOTO 1790
1600 GOTO 1340
1610 K$ = INPUT$(1)
1620 IF INSTR("DRG", K$) <> 0 THEN GOTO 2260
1630 IF INSTR("SCT", K$) <> 0 THEN GOTO 2220
1640 IF K$ = "M" THEN GOTO 2000
1650 IF K$ = "A" THEN GOTO 2160
1660 IF K$ = "F" OR K$ = "E" THEN GOTO 18S0
1670 GOTO 1340
1680 REM----- numeri HEX ----------------
1690 H$ = "" : M% = 16
1700 K$ = INPUT$(1)
1710 IF INSTR("1234567890ABCOEF", K$) = 0 THEN GOTO 1730
1720 H$ = H$ + K$ : XK$ = H$ : GOSUB 3560 : GOTO 1700
1730 LH% = LEN(H$) : N# = 0 : FOR I% = LH% TO 1 STEP -1
1740 C% = ASC(MID(H$, I%, 1))
1750 IF C% >= 65 AND C% <= 70 THEN C% = C% - 7
1760 N# = N# + (C% - 48) * M% ^ (LH% - I%) : NEXT I% : IF NS% > 1 THEN NS% = NS% + 1
1770 XK$ = STR$(N#) : S$(NS%) = XK$ : FLAG% = 1 : GOTO 1360
1780 REM----- numeri binari -------------
1790 H$ = "" : M% = 2
1800 K$ = INPUT$(1) : IF INSTR("10", K$) = 0 THEN GOTO 1730
1810 H$ = H$ + K$ : XK$ = H$ : GOSUB 3560 : GOTO 1800
1820 REM----- cambio segno --------------
1830 N# = -1 * N# : GOTO 3220
1840 REM----- formato del risultato -----
1850 IF K$ = "E" THEN EE% = -1 * EE% : GOTO 1340
1860 K$ = INPUT$(1) : IF INSTR("0123456789", K$) = 0 THEN GOTO 1860
1870 IF K$ = "9" THEN ND% = 15 : GOTO 1340 ELSE ND% = VAL(K$) : GOTO 1340
1880 REM----- notazione esponenziale ----
1890 SE$ = "+" : E$ = "" : XK$ = S$(NS%) + " E" : GOSUB 3560
1900 K$ = INPUT$(1)
1910 IF INSTR("1234567890S", K$) = O THEN GOTO 1950
1920 IF K$ = "S" AND SE$ = "+" THEN SE$ = "-" : K$ = ""
1930 IF K$ = "-" AND SE$ = "-" THEN SE$ = "+" : K$ = ""
1940 E$ = RIGHT$(E$ + K$, 2) : XK$ = S$(NS%) + " E" + SE$ + E$ : GOSUB 3560 : GOTO 1900
1950 N# = VAL(S$(NS%)) : E# = VAL(SE$ + E$) : N# = N# * EXP(E# * LN10#)
1960 XK$ = STR$(N#) : S$(NS%) = XK$ : FLAG% = 1 : GOTO 1360
1970 REM--------- INT(X) ----------------
1980 N# = FIX(N#) : GOTO 3190
1990 REM--------- |X| -------------------
2000 N# = ABS(N#) : GOTO 3190
2010 REM--------- 10^X ------------------
2020 N# = 10# ^ N# : GOTO 3190
2030 REM--------- e^X -------------------
2040 N# = EXP(N#) : GOTO 3190
2050 REM--------- conv. base -------------
2060 K$ = INPUT$(1)
2070 IF INSTR("HB", K$) = 0 THEN NERR% = 109 : GOTO 1330
2080 IF K$ = "H" THEN XK$ = HEX$(N#) : GOTO 3230
2090 XK$ = "" : Q% = 1 : WHILE Q%
2100 Q% = INT(N# / 2#) : R% = N# - Q% * 2# : XK$ = STR$(R%) + XK$ : N# = Q% : WEND
2110 N$ = STR$(VAL(XK$)) : XK$ = "" : LN% = LEN(N$)
2120 FOR K% = 1 TO LN% : XK$ =  MID$(N$, LN% - K% + 1, 1) + XK$
2130 IF K% / 4 = INT(K% / 4) THEN XK$ = " " + XK$
2140 NEXT K% : GOTO 3230
2150 REM--------- trigon. inverse ------
2160 K$ = INPUT$(1)
2170 IF INSTR("TSC", K$) = 0 THEN NERR% = 108 : GOTO 1330
2180 IF K$ = "S" THEN N# = ATN(N# / SQR(1 - N# * N#)) / PG# * GRA# : GOTO 3220
2190 IF K$ = "C" THEN N# = ATN(SQR(1 - N# * N#) / N#) / PG# * GRA# : GOTO 3220
2200 N# = ATN(N#) / PG# * GRA# : GOTO 3220
2210 REM--------- trigonometriche --------
2220 N# = N# * PG#/GRA#
2230 IF K$ = "S" THEN N# = SIN(N#) : GOTO 3220
2240 IF K$ = "C" THEN N# = COS(N#) : GOTO 3220 ELSE N# = TAN(N#) : GOTO 3220
2250 REM---------- modo angolare ---------
2260 IF K$ = "D" THEN GRA# = 180# : MANG$ = "deg" : GOTO 1340
2270 IF K$ = "R" THEN GRA# = PG# : MANG$ = "rad" : GOTO 1340
2280 GRA# = 200# : MANG$ = "gra" : GOTO 1340
2290 REM--------- clear, cambio segno ----
2300 K$ = INPUT$(1)
2310 IF INSTR("IA", K$) = O THEN NERR% = 107 : GOTO 1330
2320 IF K$ = "A" THEN S$(1) = "0" : XK$ = "O" : FLAG% = 1 : GOTO 1330
2330 IF NS% > 1 THEN NS% = NS% - 1
2340 XK$ = "O" : GOTO 3210
2350 REM--------- memoria ---------------
2360 K$ = INPUT$(1)
2370 IF INSTR( "+-*/RIX" ,K$) = O THEN NERR% = 106 : GOTO 1330
2380 IF K$ = "+" THEN MEM# = MEM# + N# : GOTO 3210
2390 IF K$ = "-" THEN MEM# = MEM# - N# : GOTO 3210
2400 IF K$ = "*" THEN MEM# = MEM# * N# : GOTO 3210
2410 IF K$ = "/" THEN MEM# = MEM# / N# : GOTO 3210
2420 IF K$ = "I" THEN MEM# = N# : GOTO 3210
2430 IF K$ = "X" THEN SWAP MEM#, N# : GOTO 3220
2440 N# = MEM# : NS% = NS% + 1 : GOTO 3220
2450 REM--------- log ln ----------------
2460 K$ = INPUT$(1)
2470 IF INSTR("GN", K$) = O THEN NERR% = 105 : GOTO 1330
2480 N# = LOG(N#)
2490 IF K$ = "G" THEN N# = N# / LOG(10)
2500 GOTO 3220
2510 REM--------- N! ---------------------
2520 IF N# <> INT(N#) THEN NERR% = 104 : GOTO 1330
2530 N1% = N# - 1 : FOR I% - 2 TO N1% : N# = N# * I% : NEXT I% : GOTO 3220
2540 REM--------- 1/X --------------------
2550 N# = 1# / N# : GOTO 3220
2560 REM--------- pi greco ----------------
2570 IF NS% > 1 THEN NS% = NS% + 1
2580 XK$ = STR$(PG#) : GOTO 3230
2590 REM--------- sqr --------------------
2600 IF SGN(VAL(S$(NS%))) < 1 THEN NERR% = 103 : GOTO 1330
2610 N# = SQR(N#) : GOTO 3220
2620 REM--------- X^2 --------------------
2630 XK$ = STR$(N# * N#) : GOTO 3230
2640 REM--------- ( ----------------------
2650 NS% = NS% + 1 : S$(NS%) = "(" : NPA% = NPA% + 1 : GOTO 3210
2660 REM--------- ) ----------------------
2670 FLAG% = 1 : NPA% = NPA% - 1 : FOR J% = NS% TO 1 STEP -1
2680 IF S$(J%) = "(" THEN GOTO 2700
2690 NEXT J% : NERR% = 101 : GOTO 1330
2700 FOR R% = J% TO NS% - 1 : S$(R%) = S$(R% + 1) : NEXT R%
2710 NS1% = J% : NS2% = NS% - 1
2720 GOSUB 2960 : REM CALCOLO ARITMETICO
2730 IF NS1% = 2 THEN NS1% = 1 : S$(1) = S$(2)
2740 NS% = NS1% : GOTO 1340
2750 REM--------- + - * / # Y ------------
2760 FLAG% = 1 : NS% = NS% + 1 : S$(NS%) = K$ : NOP% = NOP% + 1
2770 IF NOP% < 2 THEN GOTO 1340
2780 FOR J% = NS% TO 1 STEP -1
2790 IF S$(J%) <> "(" THEN GOTO 2810
2800 IF J% < NS% - 2 THEN GOTO 2820 ELSE GOTO 1340
2810 NEXT J%
2820 NS1% = J% + 1 : NS2% = NS%
2830 GOSUB 2960 : REM CALCOLO ARITMETICO
2840 NS% = NS2% : S$(NS%) = K$ : GOTO 1340
2850 REM--------- = ----------------------
2860 IF NPA% = 0 THEN GOTO 2870 ELSE NERR% = 102 : GOTO 1330
2870 NS1% = 1 : NS2% = NS%
2880 GOSUB 2960 : REM CALCOLO ARITMETICO
2890 GOTO 1330
2900 REM--------- cifre ------------------
2910 IF FLAG% = O THEN S$(NS%) = S$(NS%) + K$ : XK$ = S$(NS%) : GOTO 1340
2920 IF NS% > 1 THEN NS% = NS% + 1
2930 FLAG% = O : S$(NS%) = K$
2940 XK$ = S$(NS%) : GOTO 1340
2950 REM------ CALCOLO ARITMETICO -----------------
2960 L% = NS1% : WHILE L% <= NS2%
2970 IF INSTR("*/#Y", S$(L%)) = 0 THEN GOTO 3070
2980 IF L% >= NS2% THEN GOTO 3170
2990 N1# = VAL(S$(L% - 1)) : N2# = VAL(S$(L% + 1)) : NOP% = NOP% - 1
3000 IF S$(L%) = "#" THEN RI# = N1# * N2# / (N1# + N2#) : GOTO 3040
3010 IF S$(L%) = "*" THEN RI# = N1# * N2# : GOTO 3040
3020 IF S$(L%) = "/" THEN RI# = N1# / N2# : GOTO 3040
3030 IF S$(L%) = "Y" THEN RI# = N1# ^ N2#
3040 S$(L% - 1) = STR$(RI#)
3050 N% = L% : WHILE N% <= NS2% - 2 : S$(N%) = S$(N% + 2) : N% = N% + 1 : WEND
3060 NS2% = NS2% - 2 : L% = NS1% - 1
3070 L% = L% + 1 : WEND
3080 L% = NS1% : WHILE L% <= NS2%
3090 IF INSTR("+-", S$(L%)) = 0 THEN GOTO 3160
3100 IF L% >= NS2% THEN GOTO 3170
3110 N1# = VAL(S$(L% - 1)) : N2# = VAL(S$(L% + 1)) : NOP% = NOP% - 1
3120 IF S$(L%) = "+" THEN RI# = N1# + N2# ELSE RI# = N1# - N2#
3130 S$(L% - 1) = STR$(RI#)
3140 N% = L% : WHILE N% <= NS2% - 2 : S$(N%) = S$(N% + 2) : N% = N% + 1 : WEND
3150 NS2% = NS2% - 2 : L% = NS1% - 1
3160 L% = L% + 1 : WEND
3170 XK$ = S$(NS1%) : RETURN
3180 REM------- accessori ------------------
3190 XK$ = STR$(N#) : S$(NS%) = XK$
3200 IF NS% > 1 THEN NS% = NS% + 1
3210 FLAG% = 1 : GOTO 1340
3220 XK$ = STR$(N#)
3230 S$(NS%) = XK$ : GOTO 3210
3240 NERR% = ERR : S$(1) = "0" : RESUME 1330
3250 REM----- DISEGNO DEL DISPLAY -------------------
3260 PRINT CHR$(26); "max '84"; TAB(23);
3270 PRINT "SIMULATORE DI CALCOLATRICE TASCABILE"
3280 PRINT FNPC$(2, 27); "=========================="
3290 FOR K% = 1 TO 3 : PRINT TAB(28); "#"; TAB(53); "#" : NEXT K%
3300 PRINT TAB(28); "=========================="
3310 PRINT : PRINT
3320 PRINT "+-----------------------+--------------------------------------";
3330 PRINT "------------+----------+";
3340 PRINT "|  <=10^x      >=exp    |  ^= x^2     Y=y^x    \=1/x     N=n!";
3350 PRINT "     I=int  |  P=3.14  |"
3360 PRINT "|                       |                             +--------";
3370 PRINT "------------+----------+";
3380 PRINT "|LG=1og      LN=ln      |  V=x^2      m=|x|    S=+\-  | M+=M + ";
3390 PRINT "   M*=M *    MI=M  ln   |"
3400 PRINT "+------------------------+----------------------------+        ";
3410 PRINT "                        |"
3420 PRINT "|Hx=hex(x)   Bx=bin(x)|    s=sin      c=cos    t=tan  | M-=M - ";
3430 PRINT "   M/=M /    MR=M rec   |"
3440 PRINT "|                       |                             |        ";
3450 PRINT "  +---------+----------+"
3460 PRINT "|DH=d~h      DB=d~b     | as=asin    ac=acos  at=atan | HX=M~x ";
3470 PRINT "  | #=x//   | u=menu'   |"
3480 PRINT "+-----------------------+-----------------------------+--------";
3490 PRINT "--+---------+----------+"
3500 PRINT "|CI=Cl in    CA=Cl all  |  d=DEG      r=RAD    g=GRA  | fx=FIX(";
3510 PRINT "x)  e=EE              |"
3520 PRINT "+----------------------+------------------------------+--------";
3530 PRINT "----------------------+"
3540 PRINT:PRINT "( funzioni in minuscolo = precedute da A )"
3550 REM----- SCRIVI NEL DISPLAY --------------------
3560 PRINT FNPC$(4, 29); : IF EE% = -1 AND ND% = 15 THEN GOTO 3620
3570 XK# = VAL(XK$) : IF EE% = 1 THEN GOTO 3600
3580 PRINT SPC(22); FNPC$(4, 29);
3590 PRINT USING STRING$(21-ND%, "#") + "." + STRING$(ND%, "#"); XK# : GOTO 3630
3600 PRINT SPC(22); FNPC$(4, 29);
3610 PRINT USING "##." + STRING$(ND%, "#") + "^^^^"; XK# : GOTO 3630
3620 DK$ = RIGHT$(SPACE$(21) + XK$, 22) : PRINT USING "\                    \"; DK$
3630 PRINT FNPC$(4, 1); MANG$
364O IF MEM# = 0 THEN MEMF$ = "    " ELSE MEMF$ = "mem"
3650 PRINT FNPC$(4, 7); MEMF$
3660 IF NPA% > 0 THEN NPAF$ = STR$(NPA%) + "par" ELSE NPAF$ = "     "
3670 IF ND% = 15 THEN FXF$ = "    " ELSE FXF$ ="fix"
3680 PRINT FNPC$(4, 20); FXF$
3690 PRINT FNPC$(4, 12); NPAF$
3700 IF NERR% = 0 THEN ERF$ = "          " ELSE ERF$ = "errore" + STR$(NERR%)
3710 PRINT FNPC$(4, 60); ERF$
3720 RETURN
3730 REM----------------------------------------------------------------
3740 REM     CALCOLO DI DIAGRAMMI DI BODE DA RETI RLC
3750 REM     revisione nov. 1982 - riconv. Mbasic nov. 1984
3760 REM----------------------------------------------------------------
3770 MAXOP% = 100 : MAXC% = 4 : MAXR% = 21 : MAXITE% = 70
3780 DIM CAT$(MAXC%), V1$(MAXOP%), V2$(MAXOP%), WRE#(MAXOP%), WIM#(MAXOP%)
3790 DIM K$(MAXOP%), MDL#(MAXITE%), FASE#(MAXITE%), FREQ#(MAXITE%)
3800 CAT$(0) = "#" : CAT$(1) = "*" : CAT$(2) = "/" : CAT$(3) = "+" : CAT$(4) = "-"
3810 PIN% = 4 : PFIN% = PIN% + MAXITE% + 1
3820 RIGA1$ = STRING$(9, "-") + ":" : RIGA2$ = SPACE$(9) + ":" : FOR K% = 1 TO 3
3830 RIGA1$ = RIGA1$ + RIGA1$ : RIGA2$ = RIGA2$ + RIGA2$ : NEXT K%
3840 RIGA1$ = LEFT$(RIGA1$, PFIN% - PIN% - 1) : RIGA2$ = LEFT$(RIGA2$, PFIN% - PIN% - 1)
3850 PRINT CHR$(26); "max '84"; TAB(25); "CALCOLO DI DIAGRAMMI DI BODE" : PRINT
3B60 PRINT : PRINT
3870 REM------ INGRESSO, MEMORIZZAZIONE E CORREZIONE ESPRESSIONE -----
3880 N.OP% = 0 : FLAG% = 0 : AT$ = ""
3890 PRINT "---- Introduci l'espressione algebrica terminando con @"
3900 POKE LCC, 13 : LINE INPUT "EXPR :? "; A$
3910 IF A$ = "" THEN PRINT CHR$(11); : GOTO 3900
3920 L% = LEN(A$) : AT$ = AT$ + A$
3930 FOR K% = 1 TO L%
3940 C$ = MID$(A$, K%, 1)
3950 C% = ASC(C$) : N.OP% = N.OP% + 1
3960 IF FLAG% = 0 THEN GOTO 4020
3970 FLAG% = 0
3980 IF C% >= 48 AND C% <= 57 THEN GOTO 4000
3990 IF LEFT$(A$, 1) <> "E" THEN GOTO 4020
4000 K% = K% - 1 - LEN(BUFF$) : BUFF$ = BUFF$ + A$
4010 N.OP% = N.OP% - 1 : GOTO 4100
4020 FOR I% = 0 TO MAXC%
4030 IF C$ <> CAT$(I%) THEN GOTO 4060
4040 V1$(N.OP%) = C$
4050 GOTO 4270
4060 NEXT I%
4070 IF C% = 40 OR C% = 41 THEN V1$(N.OP%) = C$ : GOTO 4270
4080 IF C% <> 82 AND C% <> 76 AND C% <> 67 THEN GOTO 4240
4090 V1$(N.OP%) = C$ : BUFF$ = RIGHT$(A$, L% - K%)
4100 V2$(N.OP%) = STR$(VAL(BUFF$))
4110 IF V2$(N.OP%) = "" THEN PRINT "!!!   errore in un dato" : GOTO 3880
4120 L1% = LEN(BUFF$)
4130 FOR J% = 1 TO L1%
4140 K% = K% + 1
4150 C$ = ASC(MID$(BUFF$, J%, 1))
4160 IF C% = 32 THEN GOTO 4220
4170 IF C% >= 48 AND C% <= 57 THEN GOTO 4220
4180 IF C% = 69 OR C% = 46 OR C% = 44 THEN GOTO 4220
4190 IF MID$(BUFF$, J% - 1, 1) <> "E" THEN GOTO 3940
4200 IF C% = 43 OR C% = 45 THEN GOTO 4220
4210 PRINT "!!! errore  115" : GOTO 3880
4220 NEXT J%
4230 FLAG% = 1 : GOTO 3900
4240 IF C$ = "@" THEN GOTO 4290
4250 IF C$ = " " THEN GOTO 4270
4260 PRINT "!!! errore al carattere "; C$ : GOTO 3880
4270 NEXT K%
4280 GOTO 3900
4290 N.OP% = N.OP% - 1
4300 IF N.OP% = 1 THEN GOTO 4600
4310 FOR J% = 1 TO N.OP% - 1
4320 C1$ = ASC(V1$(J%))
4330 C2$ = ASC(V1$(J% + 1))
4340 FOR A% = O TO MAXC%
4350 IF C1% = ASC(CAT$(A%)) THEN G010 4380
4360 NEXT A%
4370 IF C1% <> 40 THEN GOTO 4400
4380 IF C2% = 82 OR C2% = 76 OR C2% = 67 OR C2% = 40 THEN GOTO 4520
4390 PRINT "!!! errore   111" : GOTO 3880
4400 IF C1% <> 41 THEN GOTO 4460
4410 IF C2% = 41 THEN GOTO 4520
4420 FOR A% = 0 TO MAXC%
4430 IF C2% = ASC(CAT$(A%)) THEN GOTO 4520
4440 NEXT A%
4450 PRINT "!!! errore 112" : GOTO 3880
4460 IF C1% <> 82 AND C1% <> 76 AND C1% <> 67 THEN PRINT "!!! errore 113" : GOTO 3880
4470 IF C2% = 41 THEN GOTO 4520
4480 FOR AX=O TO HAXCX
4490 IF C2X=ASC(CATS(AX»                  THEN GOTO .1,520
4500 NEXT A%
4510 PRINT "!!! errore 114" : GOTO 3880
4520 NEXT J%
4530 NPA% = 0 : NPC% = 0
4540 FOR N% = 1 TO N.OP%
4550 IF V1$(N%) = "(" THEN NPA% = NPA% + 1
4560 IF V1$(N%) = ")" THEN NPC% = NPC% + 1
4570 NEXT N%
4580 IF NPA% = NPC% THEN GOTO 4600
4590 PRINT "!!! errore per parentesi non accoppiate" : GOTO 3880
4600 PRINT
4610 PRINT TAB(22); ">>> ESPRESSIONE ALGEBRICA CORRETTA <<<"
4620 PRINT : PRINT "---- Introduci i parametri per la scansione"
4630 POKE LCC, 13 : INPUT "FREQUENZA DI INIZIO (Hz) :"; FI#
4640 IF FI# < 0 THEN PRINT CHR$(11); CHR$(24); : GOTO 4630
4650 INPUT "FREQUENZA FINALE (Hz) :"; FF#
4660 IF FF# <= FI# THEN PRINT CHR$(11); CHR$(24); CHR$(11); CHR$(24); : GOTO 4630
4670 INPUT "ASCISSA LIN/LOG          :" ; AS$
4680 IF AS$ = "LIN" THEN AS$ = "lin" : STEPF# = (FF# - FI#) / (MAXITE% - 1) : GOTO 4760
4690 IF AS$ <> "LOG" THEN PRINT CHR$(11) : CHR$(24); : GOTO 4670
4700 AS$ = "log" : S# = O : K10# = LOG(10#)
4710 FOR K% = 1 TO MAXITE%
4720 S# = S# + LOG(K%) / K10#
4730 NEXT K%
4740 K1# = (FF# - FI#) / S#
4750 REM------ SCANSIONE IN FREQUENZA DELLA ESPRESSIONE COMPLESSA -----
4760 PRINT : PRINT "     Calcolo dei" ; MAXITE%; " valori della scansione" : PRINT
4770 F# = FI# : POKE LCC, 32 : ERN% = 0
4780 K3# = 8.6858896381#
4790 K4# = 114.5915590262#
4800 POKE LCC, 32
4810 FOR NF% = 1 TO MAXITE%
4820 PRINT CHR$(11); CHR$(24); "PASSO N."; NF%
4830 OM# = 2# * F# * PG#
4840 FOR A% = 1 TO N.OP%
4850 K$(A%) = V1$(A%)
4860 V# = VAL(V2$(A%))
4B70 OP$ = K$(A%)
4880 IF OP$ = "R" THEN WRE#(A%) = V# : WIM#(A%) = 0 : GOTO 4910
4890 IF OP$ = "C" THEN WRE#(A%) = 0 : WIM#(A%) = -1# / V# / OM# : GOTO 4910
4900 IF OP$ = "L" THEN WRE#(A%) = O : WIM#(A%) = V# * OM#
4910 NEXT A%
4920 IF N.OP% = 1 THEN K$(1) = "=" : RR% = 1 : GOTO 5460
4930 FOR IL% = N.OP% TO 0 STEP -1
4940 IP% = IL% + 1
4950 IF K$(IL%) = "(" THEN GOTO 4970
4960 NEXT IL%
4970 FOR J% = 0 TO MAXC%
4980 NJ% = J% + 1
4990 FOR KK% = IP% TO N.OP%
5000 K% = KK%
5010 C$ = LEFT$(K$(KK%), 1)
5020 IF C$ = ")" THEN GOTO 5060
5030 IF C$ <> CAT$(J%) THEN GOTO 5050
5040 GOSUB 5090
5050 NEXT KK%
5060 NEXT J%
5070 K$(IP% - 1) = " " : K$(K%) = " "
5080 IF IP% = 1 THEN GOTO 5410 ELSE GOTO 4930
5090 FOR L% = K% - 1 TO 1 STEP -1
5100 PL% = L% : V% = ASC(K$(L%))
5110 IF V% = 82 OR V% = 76 OR V% = 67 OR V% = 61 THEN GOTO 5140
5120 NEXT L%
5130 PRINT "!!! errore 121" : STOP
5140 FOR L% = K% + 1 TO N.OP%
5150 SL% = L% : V% = ASC(K$(L%))
5160 IF V% = 82 OR V% = 76 OR V% = 67 OR V% = 61 THEN GOTO 5190
5170 NEXT L%
5180 PRINT "!!! errore 122" : STOP
5190 A# = WRE#(PL%) : B# = WIM#(PL%)
5200 C# = WRE#(SL%) : D# = WIM#(SL%)
5210 ON NJ% GOTO 5220, 5270, 5300, 5330, 5360
5220 PARZ# = ((A# * C# - B# * D#) * (A# + C#) + (A# * D# + B# * C#) * (B# + D#))
5230 WRE#(K%) = PARZ# / ((A# + C#) * (A# + C#) * (B# + D#) * (B# + D#))
5240 PARZ# = ((A# * D# + B# * C#) * (A# + C#) - (A# * C# - B# * D#) * (B# + D#))
5250 WIM#(K%) = PARZ# / ((A# + C#) * (A# + C#) * (B# + D#) * (B# + D#))
5260 GOTO 5380
   5.270 WREM(KZ)"AM"CH-O".Otl                                                                                                 
   5280 IHH"(KX)""AM.O,,·OMIICM                                                                                                
   5290 GOI O 5380                                                                                                             
   ~300 IolREM     (KX)     '" (AM"CM+O"110" ) / (C"IIC"+O"IIO"                  )                                             
   ~310 l.'I H" ( KZ )"'1 8"IIC"· A!fIlO") I (C".C.HoMIIo,,)                                                                   
   ~320 GOTO 5380                                                                                                              
   ~330 WRE"(KZ)=A"'Cn                                                                                                         
   531,0 l.'IMM(KZ)=O"'UM                                                                                                      
   5350 LJO    f O 5380                                                                                                        
   5360 WRlMtKZ)=A"                  C"                                                                                        
   5370 IolIM"(KX'=OM-OIt                                                                                                      
   ~,380 K.IK);)=           '"                                                                                                 
   5390 K.(PLX)-                   :t<.(SL7.):-                                                                                
   5400 RE.1URN                                                                                                                
   5 ••10 FOR KX=I IO N.OP%                                                                                                    
   51,20 RR7.z;K%                                                                                                              
   ~t,30 lF K.(KZ) ••..·"                 THEN GOro 5460                                                                       
   5440 NEXI K%                                                                                                                
   5t,50PRIN           "tlt        e,.-,.-ol'"E' 123 :STOP                                                                     
   5460 HE."21WREW(RR7.)                                                                                                       
   ::'4/0 IM••.• ::IolIri••(RR7.)                                                                                              
   51,80 MOL" l NF7.)"'K3"lIl0L(                 !:>QR (RE.""R~M'-IMlhIM"»)                                                    
   ::'490 rASE."(NF:l)>::Kt,"IIAI~1                lMM/REtO                                                                    
   '5500 ~l(lJ ••tNF7.)"'F"                                                                                                    
   5~.10 1F AS.'" l tn                  rHEN F"=FH+SIEP~ •• E.LSE:F"=FM+-KUIILOG(NFr.+I)/K10tt                                 
                                                                                                                               
   'J~2U NEX'I NFi;                                                                                                            
                                                                                                                               
   5~30 PHIN1:POKE. lLL.13                                                                                                     
   ::'540 REM- ----              INGRESSO PARAMETRi PU~ LA COS1HULIONE DEL GRAFICO                                    ----     
   5550 MI""'10~38:HS"=                      10'-38                                                                            
   'J~60 l'OR KX"'1            IO MAXIIE:;(                                                                                    
   ~~70 TOTKM=MOL"(K %)                                                                                                        
   5~80    H     101K"         -   "    THEfII   MIM     lolKM                                                                 
   5590    H     10lKH)I"I~"             IHlN I"IS••"'IOTK"                                                                    
    5600   NEXl Kr.                                                                                                            
    5610   PHIM"10--38:PHS"""-1D.-38                                                                                           
   ::'620  FOR 1<'%"'1lO MAXI 'Li:                                                                                             
    ~6jO    l'OTKMs:FASE:."         (Kr.)                                                                                      
   564()    IF -IO-TK"<PHI•• rHEN PHI"=10IKU                                                                                   
    56~U    lf   rOH")PHSM                THEN PHS""'IOTK"                                                                     
   •..•
      "60  NEXI K;C                                                                                                            
    •.•610 PRINr CHR.(11):CHR.lll):CHR.(11),CHRt(11);CHR.(24)                                                                  
   5680    PRINl         VALORI ESfRlMI                  01 MODULOE FASE RISCONIRAII                      NlL CALCOLO :-       
   5690    f'R1NI        MI· -:CSNGIMlli):-(dO)-:rA8(30):-PHl'"                             ·;CSNG(PHI,,),-(gl'")              
   ::,700  PRIN! "MS"" ~:CSNG(MSM): '(dO)-:TA8(30);-PH5'"                                   -:CSNGIPHS"):-<g,.-)~              
   5/10    "1""'M1"          : M2".M5"            : PH1"""PHIM : f'H2tt=PHSN                                                   
   ~/20 f'R]NI:PRINl                          - lnt,.-odtJCI       glt     esl:t'"l?mt d! g,.-••.rlca:.-tone                   
   5J3U "'OKE LCC. '3                                                                                                          
   ~/ •• U llNE      INPUI           HOOULO INrERI0RE                  (dO).:';'      ·;H1$                           •        
   575U lF HU=-                  ANO MT""'M2" rHEN PRINT CHR.(11):CHR.(24)::GOI0                                5/40
                                                                                                                               
   5760 IF M'."'--                THEN Goro 5790                                                                               
   5/70    lNPUI -MODULO SUPERIORE (da)                                  :-,TH2M                                               
   5/80    Ml""'VAL(Hl.):M2M=TH21t                                                                                             
   5/90    IF M2"<=HU                THEN PRINl CHR$( 11) :CHR.(24) :CHR$( 11) :C.HR.(24):                          :GOIO 5740 
   5BOlJ STE.PM"=(M2"-Ml")/(MAXRZ                            1)                                                                
   5810 f'RINl                                                                                                                 
   5820 UNE. INPUl "FASE. JNFERIORE.                                   (g,.-)":l      ",PHH                                    
   5830 IF PHl$="H ANO PH1••.;::f'H~'" THEN PRINI' CHR$( 11) ;CHR$(24): :GOTO 5820                                             
   5840 IF PHH::H-                 THEN GOI0 5870                                                                              
   5850 INPUT ·FASE SUPERIORI:.                               (gr):,          rPH2"                                            
   5860 PH1H·VAl(PHI'I)                   :PH2""'lPH2H                                                                         
   5870 IF PH2"(=PH1"                     THEN PRINT CHR.( 11':1 HA.(24) ;CHR$(11) :CHR$(24): :130TO 5820                      
   5880 POKE LCC. 32                                                                                                           
   5890 5 rE.PPH"""(PH2"-PH1M) I (HAXA:t-l)                                                                                    
   5900 PRlNT                                                                                                                  
   5910 REH- ----                - PRESENIAZIONE su VIDEO DEL GRAFICO MODULOE FASE ----                                        
   5920 PRINI CHR.(26),                                                                                                        
5980 IF K:t/5"'INT(Kr.IS)        THEN RIGA."'RIGAU               ElSE RIGA''''RIGA2.
~990 PRINT FNPC$(Kit:.INr.) ;RIGA$ : NEXT KX
6000 PRINT FNPC'(2.0);"MOD.·               : PRINT FNPC'(3.0);H(dB)-
60"10 PAINT FNPC.(2.PFINit:+l);"FASE"                 : PRINT FNPC$(3.PFlNl(+I):                    '(g"-)-
6020 FOR IR';"'O TO MAXRZ-1 STEP 5
6030 OATOH"=M2"-IRXIISTEPM"
6040 IF ABS<OATOM")( 10 THEN FORM.=-N ••••• - ELSE FORH."'-" ••••M~
6050 PRINT FNPC.(lRX.OhUSING                FORH.:DATOH"
6060 oA1PHM"'PH2"-IRr.IISIEPPH"
6070 IF ABS(OATPH")( 10 THEN FOR'U=~HH."~                        ELSE FORH.""-IU •••••-
6080 PRINT FNPC.( IAX.MAXITEr.+PINX+2)                 :USING FORH.,OATPH"
6090 NEXT IRi::
6100 PRINT FNPC'(HAXRi::+1.0) ;USING "1t••••• It" ••·,.                   ":FREQ ••( 1):
61-10 PRINT - Hz",TAO(MAXITEit:-l):
6120 PRINT USING - •••••••••••••.v····.. -;FREQ ••(MAXITEr.);
6130 PRINT FNPC$(MAXRX+l,MAXlTEX+6),~                     H=~
6140 PRINT FNPC.(HAXRX.PINX+9) ;AS$
6150 FOR K7.""l TO l'1AXITEX
6160 HX"'MAXR;(-INT< (MOL"(Kit:)-H1")/STEPMM)--1
6170 IF HX)MAXRX-l THEN IRr.=HAXRZ-1 : C$"'~v-                              GOTO 6190
6180 Ho HX(O lHEN IR;(=O : C.=~·'"               ELSE IRiC"'Hi::            C'=-IlI-
6190 PRINT FNPC'(lR7..Kr.+PINX):Ct
6200 P;.::
         .••HAXR%-INI'«FASEH(K:U           PHI")/STl..PPHH)            I
6210 lF PZ'HAXRi::-l         T'HEN HO. M"Ak,.          •••.•             , uvlO 6230
6220 IF P7.(O rHEN IR;(zO : C$""-"-              ELSE IRi::"'P;( : c'.-r~
6230 IF MX=Pi:: ANO HX)=O ANO Hi::(HAXR:l THEN IR:l:a:Hl( : C.= x·
6240 PRINT FNPC$( IRX.KX+PINX) ,c.
62S0 N XT KX
6260 PRINT FNPC.(HAXRX .PF1NZ+ 1); ~ max-
6270 POKE LCC.13 : PRINT FNPC.(HAXRX+1.32);-C/F/G/S/E/M                                       ? •
6280 R''''INPUH(l)          : IF INSTR(·CFGSEH·,R$)=O                 HiEN GOTO 6280
6290 IF R,,,,"CH THEN GOTO 6560
6300 IF R.="FH        THEN GOTO 63BO ELSE PRINl CHR.(26)
63-10 IF Rt"'~E~ THEN GOTO 3850
6320 IF Rt=-S~        THEN PRINT -ripetizIOne                dlrll~      scanslonlr~:OPZX"'1:GOTO               6350
6330 IF R.=·~GH THEH PRIHT ~rlplrt              IZlone       del    gr~rlco-:OPZl(""2:GOTO                6350
6340 ERASE CATt.V1t.V2$.IolRE".loIIH".K              •• MoLM,FASE".FREQ"                 : GOTO 90
6350 PRINT:PRINT          ~EXPR • H:LEF"T.(AT'.LEN(AH)-l)                     :PRINT:PRINT
6360 ON OPZX GOTO 4620.5680
6370 REH-------         PHESENTAZIONE DATI PUNTUALI ------------------
6380 'POKE LCC.32:PCr.=PINX·tl         :PCVX"'PCX:PRINT FNPC'(HAXR%+1.0) ,CHR$(24):
6390 GOTO 6440
6400 PCVit:=PC" : R.""INPUr.(1)            : R"""ASC<R.)
6410 IF Rr.""11 THEN GOTO 6500
6420 IF RX:8 lHEH PC:t""PCVX-1 : IF PCX(PINr.+l                          THEN PCr..PFINr.-l
6430 IF R;("'14 THEN PCX.PCVl(+l            : IF PCl()PFINl(-'l             THEN PCl(zPINX" I
6440 PRINT FNPC. (HAXRX. PCVX) ; - -~ ; FNPC. (HAXRX. PINX+9) ; AS.
6450 PRINT FNPC$(HAXRX. PCX) ; CHR. (31 ) ,CHRt (7)
6460 PRINT FHPCt(HAXRit:+·1.4),-r,.-eq             "'-:USIHG        - ••••••• MM"M••'·.•.•.•. ··~:FREQ'HPCX-PIN7.)
6470 PRIfliT FHPC.(HAXRX+l.32);"",od               ·-;USING         - ••••••••••••••••••..
                                                                                        M· •.•-'HoL    ••(PCr.-PIN:l)
6'.80 PRINT FNPC$(HAXRX+l.57);-rasE'                 =",USING         • ••••• tH.NM••••" •..•..•..
                                                                                                 -;FASE ••(PC7.-PINl()
6490 GOTO 6400
6500 PRINT FNPC. (MAXRr.. PCX) ; - - - : FNPC. (HAXRil:, PINX+9) ; ASt
65'10 PRINT FNPC. (HAXRX+1 • O) : CHR. (24) ; USING ~NH••• M••••·.•.                 M ••••-;   FREli" ( 1) ;
6520 PRlHT - Hz~;TAB(HAXITE:t);
6530 PRIHT USING ~H••••••••••••..•••••••.. ~:FREQ••(HAXITEX):
6540 PRINT FHPC.(HAXRX+l.HAXITEr.+6):-                    Hz- : GOTO 6270
65:50 REH-------        STAHPA DEL GRAFICO ---------------------------
6560 LPRINT : LPRIHT CHRt(27):~[1·)~;CHR.(27);70~;CHR.(27>:~73~
6570 LPRINT : LPRINT : LPRIHT
6580 lPR INT TA8 (15); ~RAPPRESENTAZIONE IN MODULOE FASE DELLA RETE
6590 LPRINT : lPRINT lEFU(ATt.LEN(ATt)-·I)                          : LPRINT
6600 POKE LCC.32 : PRINT FNPC$(HAXRX+l,32);SPACE.(15)
6610 FOR RX=O TO HAXR7.-1 : F.RX/5                 : STt,.--
6620 IF R7.=;2 THEN LPRINT -HOo.: -:               : GOSUO6770 : LPRINT H: FASE- : GOTO 6700
6630 IF Rl("'3 THEN LPRIHT -(dB):~;                : GOSUO6770 : LPRINT -:(9r)·                            : GOTO 6700
6640 IF FOINT(f)            THEN LPRINTH            :~;:GOSUO 6770:LPRINIH:                           H:GOTO 6700
6650 OATOH"""H2"-RXIISTEPH"
6660 IF AOS(oATOM")(10           THEN FH•• ~"".":-            ELSE FH.·H ••••••••:~
6670 OATPH"=PH2"-RX.STEPPHM
6680 IF AOS(OATPH")(10           THEN FP$••.-: ••••• It~ ElSE FP'·H:""M"~
6690 LPRINT USING FH$:OATOHN: : GOSUB 6770 : LPRIHT USING FPt:oATPHM
6700 NEXT RX : LPRINT ~                +~:STRING$(8.--~);AS.;
6710 LPRINT STRIHG.(HAXITEX-11,               ~-~) ;-+ Max"
6720 LPRINT USING ~•••••••••••••••••••••••••H ,FREG"('I);
6730 LPRINT - H:-:TAO(HAXITEX-3):
6740 LPRINT USING ~"".""",,"'''''''''''''-;FREQ''(f'IAXITE;();
6750 LPRINT ~ Hz- :LPRIN'r:LPRINT:LPRINT:Lf'RINT:LPRINT
6760 FOR Kl(-1 TO 16000: NEXT KX : LPRINT CHR$ (27) : -c -                                 GOTO 6270
6770 IF F""IHHF)          'THEH RIGA."'RIGA1t        ElSE RIGAt"RIGA2t
6780 FOR Cr."l      TO MAXITEX : Ct-MIO$( RIGAt. Cl(.l)
6790 Hl(-HAXRX-INT( (HOL"(Cr.)-Hl")/SrEPM")-l
6800 IF RX=O THEN IF Mr.(O lHEN C$""~"'~ : GOTO 6830
6810 IF HX=RX THEN C.·~.~            : GOTO 6830
6820 IF" RX"'MAXRX-1 THEN IF Hl()MAXRX-1 lHEN C•• -\l~
6830   PX"'HAXRX-INT< <FASE.t<CX) -PH1") ISTEPPH")-1
 6840  IF RX=O THEH lF Pr.(O THEN C.=- .••.            - : GOTO 6880
 68:50 IF Pr."'R:t THEN Ct",~r~       : GOTO 6870
 6860  IF R:l"'MAXRr.-1 THEN IF PX)HAXRX-l                 THEN Ct-~v-            : GOTO 6880
 6870  IF HX"'Pr. THEN IF HX-RX ANO MX)=O ANO MX(MAXRi:: THEN C$",HX"
 6880  SU·SH+C$          : NEXT CX : LPRINT SU;                : RETURN