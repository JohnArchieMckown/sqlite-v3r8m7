000100 PROCESS DYNAM,NAME,PGMNAME(COMPAT)                               00010000
000200 PROCESS ADV,AWO,NOC(W),FSRT,FLAG(I,I)                            00020000
000300 PROCESS INTDATE(ANSI),LANG(EN),LIB,LIST,MAP                      00030000
000400 PROCESS NONUM,NUMPROC(PFD),OBJ,APOST                             00040000
000500 PROCESS RENT,NOSEQ,SOURCE,NOSSRANGE,                             00050000
000600 PROCESS NOTERM,NOTEST,VBREF,                                     00060000
000700 PROCESS XREF(FULL),ZWB,FASTSRT,AR(E)                             00070000
000800 ID DIVISION.                                                     00080000
000900 PROGRAM-ID. 'TESTCOB2'.                                          00090001
001000 AUTHOR. JOHN MCKOWN.                                             00100000
001100 INSTALLATION. CBT Tape.                                          00110000
001200 DATE-WRITTEN.                                                    00120000
001300 DATE-COMPILED.                                                   00130000
001400 SECURITY. NONE.                                                  00140000
001500*                                                                 00150000
001600* This program is designed to open a existing SQLITE file         00160002
001700* which contains one or more tables. It will then display         00170002
001800* the name of each table and the contents of every row            00180002
001900* within each table.                                              00190002
002000 ENVIRONMENT DIVISION.                                            00200000
002100 CONFIGURATION SECTION.                                           00210000
002200 SOURCE-COMPUTER. IBM-370.                                        00220000
002300 OBJECT-COMPUTER. IBM-370.                                        00230000
002400 SPECIAL-NAMES.                                                   00240000
002500 INPUT-OUTPUT SECTION.                                            00250000
002600 FILE-CONTROL.                                                    00260000
002700 I-O-CONTROL.                                                     00270000
002800*    APPLY WRITE-ONLY ON REPORT1-FD.                              00280000
002900*                                                                 00290000
003000 DATA DIVISION.                                                   00300000
003100 FILE SECTION.                                                    00310000
003200                                                                  00320000
003300*                                                                 00330000
003400 WORKING-STORAGE SECTION.                                         00340000
003500 COPY COBSQLTE .                                                  00350000
003600 01  CEE3DMP-PARAMETERS.                                          00360000
003700     05 CEE3DMP-TITLE              PIC X(80)                      00370000
003800        VALUE 'TEST DUMP'.                                        00380000
003900     05 CEE3DMP-OPTIONS            PIC X(255)                     00390000
004000        VALUE 'NOTRACE NOENTRY'.                                  00400000
004100 01  LE-FC.                                                       00410000
004200     02  CONDITION-TOKEN-VALUE.                                   00420000
004300     COPY  CEEIGZCT.                                              00430000
004400         03  CASE-1-CONDITION-ID.                                 00440000
004500             04  SEVERITY    PIC S9(4) BINARY.                    00450000
004600             04  MSG-NO      PIC S9(4) BINARY.                    00460000
004700         03  CASE-2-CONDITION-ID                                  00470000
004800                   REDEFINES CASE-1-CONDITION-ID.                 00480000
004900             04  CLASS-CODE  PIC S9(4) BINARY.                    00490000
005000             04  CAUSE-CODE  PIC S9(4) BINARY.                    00500000
005100         03  CASE-SEV-CTL    PIC X.                               00510000
005200         03  FACILITY-ID     PIC XXX.                             00520000
005300     02  I-S-INFO            PIC S9(9) BINARY.                    00530000
005400*                                                                 00540000
005500 LOCAL-STORAGE SECTION.                                           00550000
005600 01  LS-FULLWORD-RETURN.                                          00560000
005700     05  LS-SOURCEID-POINTER     POINTER.                         00570000
005800     05  LS-SOURCEID-BINVALUE    REDEFINES LS-SOURCEID-POINTER    00580000
005900                                 PIC S9(9) COMP-5.                00590000
006000 77  LS-COLUMN-NUMBER        PIC S9(9) COMP-5.                    00600003
006100 77  LS-DATABASE-NAME        PIC X(255).                          00610000
006200 77  LS-DB                   USAGE IS POINTER.                    00620000
006300 77  LS-INSERT-STMT          USAGE IS POINTER.                    00630000
006400 77  LS-SELECT-STMT          USAGE IS POINTER.                    00640000
006500 77  LS-ZERO                 PIC S9(9) COMP-5                     00650000
006600                             VALUE IS ZERO.                       00660000
006700 77  LS-RC                   PIC S9(9) COMP-5.                    00670000
006800 77  LS-SQL-RC               PIC S9(9) COMP-5.                    00680000
006900 77  SQL-ZCHAR               PIC X(255).                          00690000
007000 77  LS-I                    PIC S9(9) COMP-5.                    00700000
007100 77  LS-RECORD-COUNT         PIC S9(9) COMP-5.                    00710000
007200 77  LS-MINUS-ONE            PIC S9(9) COMP-5                     00720000
007300        VALUE IS -1.                                              00730000
007400 77  LS-ONE                  PIC S9(9) COMP-5                     00740000
007500        VALUE IS 1.                                               00750000
007600 77  LS-SOURCEID-LENGTH      PIC S9(9) COMP-5.                    00760000
007700 77  LS-DOUBLE-BFP           USAGE COMP-2.                        00770000
007800 77  LS-DOUBLE-HFP           USAGE COMP-2.                        00780003
007900 77  LS-OPEN-FLAGS           PIC S9(9) COMP-5.                    00790002
008000 77  LS-NULL                 POINTER VALUE IS NULL.               00800002
008100 LINKAGE SECTION.                                                 00810000
008200 01  MVS-PARM.                                                    00820000
008300     05 MVS-PARM-LENGTH      PIC S9(4) COMP-5.                    00830000
008400     05 MVS-PARM-VALUE       PIC X(32760).                        00840000
008500 77  LINKAGE-SOURCEID        PIC X(101).                          00850000
008600* MVS-PARM-VALUE LENGTH IS REALLY ONLY THE NUMBER OF CHARACTERS   00860000
008700* CONTAINED IN MVS-PARM-LENGTH. YOU NEED TO USE REFERENCE         00870000
008800* MODIFICATION TO MAKE SURE YOU DON'T EXCEED THE ACTUAL LENGTH    00880000
008900* PASSED TO THIS PROGRAM.                                         00890000
009000*                                                                 00900000
009100 PROCEDURE DIVISION USING MVS-PARM.                               00910000
009200 START-UP.                                                        00920000
009300**                                                                00930000
009400** How to access the data when SQLITE returns a                   00940000
009500** pointer. The following shows how to get the pointer            00950000
009600** and then determine the length of the C "string"                00960000
009700** which ends with a LOW-VALUES (0x00).                           00970000
009800     CALL SQLITE3A USING SQLITE3-LIBVERSION                       00980000
009900          RETURNING LS-SOURCEID-POINTER                           00990000
010000     END-CALL                                                     01000000
010100     IF LS-SOURCEID-POINTER NOT EQUAL TO NULL THEN                01010000
010200        SET ADDRESS OF LINKAGE-SOURCEID TO LS-SOURCEID-POINTER    01020000
010300*                                                                 01030000
010400* Get the length of the returned string by finding the first      01040000
010500* LOW-VALUE (0x00). Probably not as efficient as calling the      01050000
010600* C "strlen" routine, but is "pure" COBOL.                        01060000
010700        INSPECT LINKAGE-SOURCEID                                  01070000
010800                TALLYING LS-SOURCEID-LENGTH                       01080000
010900                FOR CHARACTERS BEFORE INITIAL LOW-VALUE           01090000
011000*                                                                 01100000
011100* If you're interested, this is how to call "strlen". It requires 01110000
011200* that CEE.SCEELKED be available for dynamic calling or linking   01120000
011300* depending on the compile option DYNAM or NODYNAM, respectively. 01130000
011400*       CALL    'STRLEN' USING BY VALUE LS-SOURCEID-POINTER       01140000
011500*               RETURNING LS-SOURCEID-LENGTH                      01150000
011600*       END-CALL                                                  01160000
011700        DISPLAY "Sqlite version is "                              01170000
011800                LINKAGE-SOURCEID(1:LS-SOURCEID-LENGTH)            01180000
011900                UPON SYSOUT                                       01190000
012000     END-IF                                                       01200000
012100     CALL SQLITE3A USING SQLITE3-INITIALIZE                       01210000
012200     RETURNING LS-SQL-RC                                          01220000
012300     END-CALL                                                     01230000
012400D    DISPLAY 'INITIALIZE LS-SQL-RC=' LS-SQL-RC                    01240000
012500D            UPON SYSOUT                                          01250000
012600     IF LS-SQL-RC NOT = SQLITE-OK THEN                            01260000
012700        DISPLAY 'SQLITE-INITIALIZE FAILED. RC='                   01270000
012800            RETURN-CODE                                           01280000
012900            UPON SYSOUT                                           01290000
013000        GOBACK                                                    01300000
013100     END-IF                                                       01310000
013200     .                                                            01320000
013300 INITIALIZE-SQLITE.                                               01330000
013400*                                                                 01340000
013500* Note: in my testing, this creates a z/OS UNIX file in           01350000
013600* the /tmp subdirectory, with the name testcob2.sqlite3 .         01360006
013700* That is, the data base name is actually the UNIX file name.     01370000
013800* This can be absolute, as in my example, or relative to the      01380000
013900* user's UNIX $HOME directory if the value does not start with    01390000
014000* a slash character. This is the norm for UNIX file names.        01400000
014100*                                                                 01410000
014200* Note that a prefix of a tilde, ~/, does not expand to the user's01420000
014300* $HOME as it would in a UNIX shell.                              01430000
014400* Again, in my testing, the OPEN fails with SQLITE-CANTOPEN (14). 01440000
014500*                                                                 01450000
014600     IF MVS-PARM-LENGTH IS GREATER THAN 254 THEN                  01460000
014700        DISPLAY 'INPUT DATA BASE NAME IS TOO LONG.'               01470000
014800                ' LENGTH=' MVS-PARM-LENGTH                        01480000
014900                UPON SYSOUT                                       01490000
015000        MOVE +16 TO RETURN-CODE                                   01500000
015100     END-IF                                                       01510000
015200     IF MVS-PARM-LENGTH IS EQUAL TO ZERO THEN                     01520000
015300        MOVE Z'/tmp/testcob2.sqlite3' TO SQL-ZCHAR                01530006
015400     ELSE                                                         01540000
015500        MOVE MVS-PARM-VALUE(1:MVS-PARM-LENGTH) TO                 01550000
015600             SQL-ZCHAR                                            01560000
015700        MOVE LOW-VALUES TO                                        01570000
015800             SQL-ZCHAR(MVS-PARM-LENGTH + 1:1)                     01580000
015900     END-IF                                                       01590000
016000     MOVE SQLITE-OPEN-READONLY TO LS-OPEN-FLAGS                   01600002
016100     CALL SQLITE3A USING BY REFERENCE SQLITE3-OPEN-V2             01610002
016200          BY REFERENCE SQL-ZCHAR                                  01620000
016300          BY REFERENCE LS-DB                                      01630000
016400          BY VALUE LS-OPEN-FLAGS                                  01640002
016500          BY VALUE LS-NULL                                        01650002
016600          RETURNING LS-SQL-RC                                     01660000
016700     END-CALL                                                     01670000
016800     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01680000
016900        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01690000
017000        DISPLAY 'SQLITE3-OPEN FAILED. RC='                        01700000
017100           LS-SQL-RC UPON SYSOUT                                  01710000
017200        MOVE +16 TO RETURN-CODE                                   01720002
017300        GOBACK                                                    01730000
017400     END-IF                                                       01740000
017500     .                                                            01750000
017600 PREPARE-SELECT.                                                  01760003
017700     MOVE Z'SELECT int,fd FROM xz;' TO SQL-ZCHAR                  01770003
017800     CALL SQLITE3A USING BY REFERENCE SQLITE3-PREPARE-V2          01780004
017900          BY VALUE LS-DB                                          01790003
018000          BY REFERENCE SQL-ZCHAR                                  01800003
018100          BY VALUE LS-MINUS-ONE                                   01810003
018200          BY REFERENCE LS-SELECT-STMT                             01820003
018300          BY VALUE LS-ZERO                                        01830003
018400          RETURNING LS-SQL-RC                                     01840003
018500     END-CALL                                                     01850003
018600*    MOVE RETURN-CODE TO LS-SQL-RC                                01860003
018700D    DISPLAY 'SELECT PREPARE RC=' LS-SQL-RC UPON SYSOUT           01870003
018800     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01880003
018900        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01890003
019000        DISPLAY 'PREPARE OF SELECT FAILED. RC='                   01900003
019100           LS-SQL-RC UPON SYSOUT                                  01910003
019200        GOBACK                                                    01920003
019300     END-IF                                                       01930003
019400     .                                                            01940003
019500 SELECT-LOOP.                                                     01950003
019600     MOVE +0 TO LS-SQL-RC                                         01960003
019700     PERFORM UNTIL LS-SQL-RC IS NOT EQUAL TO ZERO                 01970003
019800       DISPLAY 'SQLITE3-STEP' UPON SYSOUT                         01980003
019900       CALL SQLITE3A USING                                        01990003
020000            BY REFERENCE SQLITE3-STEP                             02000003
020100            BY VALUE LS-SELECT-STMT                               02010003
020200            RETURNING LS-SQL-RC                                   02020003
020300       END-CALL                                                   02030003
020400       DISPLAY 'LS-SQL-RC=' LS-SQL-RC UPON SYSOUT                 02040003
020500       EVALUATE LS-SQL-RC                                         02050003
020600       WHEN SQLITE-ROW                                            02060003
020700            MOVE +0 TO LS-COLUMN-NUMBER                           02070003
020800            DISPLAY 'SQLITE-ROW' UPON SYSOUT                      02080003
020900            CALL SQLITE3A USING                                   02090003
021000                 BY REFERENCE SQLITE3-COLUMN-INT                  02100003
021100                 BY VALUE LS-SELECT-STMT                          02110003
021200                 BY VALUE LS-COLUMN-NUMBER                        02120003
021300                 RETURNING LS-I                                   02130003
021400            END-CALL                                              02140003
021500            MOVE +1 TO LS-COLUMN-NUMBER                           02150003
021600            CALL SQLITE3A USING                                   02160003
021700                 BY REFERENCE SQLITE3-COLUMN-DOUBLE               02170003
021800                 BY REFERENCE LS-DOUBLE-BFP                       02180003
021900                 BY VALUE LS-SELECT-STMT                          02190003
022000                 BY VALUE LS-COLUMN-NUMBER                        02200003
022100            END-CALL                                              02210003
022200            CALL SQLITE3A USING                                   02220003
022300                 BY REFERENCE CONVERT-BFP-TO-HFP                  02230003
022400                 BY VALUE LS-DOUBLE-BFP                           02240003
022500                 BY REFERENCE LS-DOUBLE-HFP                       02250003
022600            END-CALL                                              02260003
022700*           MOVE -1.999 TO LS-DOUBLE-HFP                          02270003
022800            DISPLAY 'FETCHED DATA int=' LS-I                      02280005
022900                    ' fd=' LS-DOUBLE-HFP                          02290005
023000                    UPON SYSOUT                                   02300003
023100            MOVE +0 TO LS-SQL-RC                                  02310003
023200       WHEN SQLITE-OK                                             02320003
023300            DISPLAY 'SQLITE-OK' UPON SYSOUT                       02330003
023400            MOVE +0 TO LS-SQL-RC                                  02340003
023500       WHEN SQLITE-DONE                                           02350003
023600            DISPLAY 'SQLITE-DONE' UPON SYSOUT                     02360003
023700            MOVE +4 TO LS-SQL-RC                                  02370003
023800       WHEN OTHER                                                 02380003
023900            DISPLAY 'SQLITE-OTHER ' LS-SQL-RC UPON SYSOUT         02390003
024000            MOVE -2 TO LS-SQL-RC                                  02400003
024100     END-EVALUATE                                                 02410003
024200     END-PERFORM                                                  02420003
024300     CALL SQLITE3A USING                                          02430003
024400          BY REFERENCE SQLITE3-FINALIZE                           02440003
024500          BY VALUE LS-SELECT-STMT                                 02450003
024600          RETURNING LS-SQL-RC                                     02460003
024700     END-CALL                                                     02470003
024800D    DISPLAY 'SELECT FINALIZE RC=' LS-SQL-RC UPON SYSOUT          02480003
024900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02490003
025000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02500003
025100        DISPLAY 'FINALIZE FAILED. RC='                            02510003
025200           LS-SQL-RC UPON SYSOUT                                  02520003
025300        GOBACK                                                    02530003
025400     END-IF                                                       02540003
025500     .                                                            02550003
025600 SHUTDOWN.                                                        02560000
025700     CALL SQLITE3A USING SQLITE3-SHUTDOWN                         02570000
025800          RETURNING LS-SQL-RC                                     02580000
025900     END-CALL                                                     02590000
026000     GOBACK                                                       02600000
026100     .                                                            02610000
026200 END PROGRAM 'TESTCOB2'.                                          02620001
