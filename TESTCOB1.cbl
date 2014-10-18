000100 PROCESS DYNAM,NAME,PGMNAME(COMPAT)                               00010031
000200 PROCESS ADV,AWO,NOC(W),FSRT,FLAG(I,I)                            00020031
000300 PROCESS INTDATE(ANSI),LANG(EN),LIB,LIST,MAP                      00030000
000400 PROCESS NONUM,NUMPROC(PFD),OBJ,APOST                             00040015
000500 PROCESS RENT,NOSEQ,SOURCE,NOSSRANGE,                             00050000
000600 PROCESS NOTERM,NOTEST,VBREF,                                     00060015
000700 PROCESS XREF(FULL),ZWB,FASTSRT,AR(E)                             00070000
000800 ID DIVISION.                                                     00080000
000900 PROGRAM-ID. 'TESTCOB1'.                                          00090006
001000 AUTHOR. JOHN MCKOWN.                                             00100000
001100 INSTALLATION. CBT Tape.                                          00110000
001200 DATE-WRITTEN.                                                    00120000
001300 DATE-COMPILED.                                                   00130000
001400 SECURITY. NONE.                                                  00140000
001500*                                                                 00150000
001600 ENVIRONMENT DIVISION.                                            00160000
001700 CONFIGURATION SECTION.                                           00170000
001800 SOURCE-COMPUTER. IBM-370.                                        00180024
001900 OBJECT-COMPUTER. IBM-370.                                        00190000
002000 SPECIAL-NAMES.                                                   00200000
002100 INPUT-OUTPUT SECTION.                                            00210000
002200 FILE-CONTROL.                                                    00220000
002300 I-O-CONTROL.                                                     00230000
002400*    APPLY WRITE-ONLY ON REPORT1-FD.                              00240000
002500*                                                                 00250000
002600 DATA DIVISION.                                                   00260000
002700 FILE SECTION.                                                    00270000
002800                                                                  00280000
002900*                                                                 00290000
003000 WORKING-STORAGE SECTION.                                         00300000
003100 COPY COBSQLTE .                                                  00310004
003200 01  CEE3DMP-PARAMETERS.                                          00320000
003300     05 CEE3DMP-TITLE              PIC X(80)                      00330000
003400        VALUE 'TEST DUMP'.                                        00340000
003500     05 CEE3DMP-OPTIONS            PIC X(255)                     00350000
003600        VALUE 'NOTRACE NOENTRY'.                                  00360000
003700 01  LE-FC.                                                       00370000
003800     02  CONDITION-TOKEN-VALUE.                                   00380000
003900     COPY  CEEIGZCT.                                              00390000
004000         03  CASE-1-CONDITION-ID.                                 00400000
004100             04  SEVERITY    PIC S9(4) BINARY.                    00410000
004200             04  MSG-NO      PIC S9(4) BINARY.                    00420000
004300         03  CASE-2-CONDITION-ID                                  00430000
004400                   REDEFINES CASE-1-CONDITION-ID.                 00440000
004500             04  CLASS-CODE  PIC S9(4) BINARY.                    00450000
004600             04  CAUSE-CODE  PIC S9(4) BINARY.                    00460000
004700         03  CASE-SEV-CTL    PIC X.                               00470000
004800         03  FACILITY-ID     PIC XXX.                             00480000
004900     02  I-S-INFO            PIC S9(9) BINARY.                    00490000
005000*                                                                 00500000
005100 LOCAL-STORAGE SECTION.                                           00510000
005200 01  LS-FULLWORD-RETURN.                                          00520031
005300     05  LS-SOURCEID-POINTER     POINTER.                         00530031
005400     05  LS-SOURCEID-BINVALUE    REDEFINES LS-SOURCEID-POINTER    00540031
005500                                 PIC S9(9) COMP-5.                00550031
005600 77  LS-LILIAN               PIC S9(9) COMP-5.                    00560008
005700 77  LS-GMT-SECS-1           USAGE COMP-2.                        00570008
005800 77  LS-GMT-SECS-2           USAGE COMP-2.                        00580008
005900 77  LS-GMT-SECS-3           USAGE COMP-2.                        00590008
006000 77  LS-GMT-SECS-4           USAGE COMP-2.                        00600008
006100 77  LS-GMT-SECS-5           USAGE COMP-2.                        00610011
006200 77  LS-GMT-SECS-6           USAGE COMP-2.                        00620011
006300 77  LS-DB                   USAGE IS POINTER.                    00630007
006400 77  LS-INSERT-STMT          USAGE IS POINTER.                    00640007
006500 77  LS-SELECT-STMT          USAGE IS POINTER.                    00650011
006600 77  LS-ZERO                 PIC S9(9) COMP-5                     00660007
006700                             VALUE IS ZERO.                       00670007
006800 77  LS-RC                   PIC S9(9) COMP-5.                    00680007
006900 77  LS-SQL-RC               PIC S9(9) COMP-5.                    00690007
007000 77  SQL-ZCHAR               PIC X(255).                          00700007
007100 77  LS-I                    PIC S9(9) COMP-5.                    00710007
007200 77  LS-RECORD-COUNT         PIC S9(9) COMP-5.                    00720014
007300 77  LS-MINUS-ONE            PIC S9(9) COMP-5                     00730007
007400        VALUE IS -1.                                              00740007
007500 77  LS-ONE                  PIC S9(9) COMP-5                     00750007
007600        VALUE IS 1.                                               00760007
007700 77  LS-SOURCEID-LENGTH      PIC S9(9) COMP-5.                    00770028
007800 LINKAGE SECTION.                                                 00780000
007900 01  MVS-PARM.                                                    00790005
008000     05 MVS-PARM-LENGTH      PIC S9(4) COMP-5.                    00800005
008100     05 MVS-PARM-VALUE       PIC X(32760).                        00810005
008200 77  LINKAGE-SOURCEID        PIC X(101).                          00820028
008300* MVS-PARM-VALUE LENGTH IS REALLY ONLY THE NUMBER OF CHARACTERS   00830005
008400* CONTAINED IN MVS-PARM-LENGTH. YOU NEED TO USE REFERENCE         00840005
008500* MODIFICATION TO MAKE SURE YOU DON'T EXCEED THE ACTUAL LENGTH    00850005
008600* PASSED TO THIS PROGRAM.                                         00860005
008700*                                                                 00870000
008800 PROCEDURE DIVISION USING MVS-PARM.                               00880000
008900 START-UP.                                                        00890000
009000**                                                                00900031
009100** How to access the data when SQLITE returns a                   00910031
009200** pointer. The following shows how to get the pointer            00920031
009300** and then determine the length of the C "string"                00930031
009400** which ends with a LOW-VALUES (0x00).                           00940031
009500     CALL 'SQLITE3A' USING SQLITE3-LIBVERSION                     00950031
009600          RETURNING LS-SOURCEID-POINTER                           00960031
009700     END-CALL                                                     00970031
009800     IF LS-SOURCEID-POINTER NOT EQUAL TO NULL THEN                00980031
009900        SET ADDRESS OF LINKAGE-SOURCEID TO LS-SOURCEID-POINTER    00990031
010000*                                                                 01000032
010100* Get the length of the returned string by finding the first      01010032
010200* LOW-VALUE (0x00). Probably not as efficient as calling the      01020033
010300* C "strlen" routine, but is "pure" COBOL.                        01030033
010400        INSPECT LINKAGE-SOURCEID                                  01040032
010500                TALLYING LS-SOURCEID-LENGTH                       01050032
010600                FOR CHARACTERS BEFORE INITIAL LOW-VALUE           01060032
010700*                                                                 01070033
010800* If you're interested, this is how to call "strlen". It requires 01080033
010900* that CEE.SCEELKED be available for dynamic calling or linking   01090033
011000* depending on the compile option DYNAM or NODYNAM, respectively. 01100033
011100*       CALL    'STRLEN' USING BY VALUE LS-SOURCEID-POINTER       01110033
011200*               RETURNING LS-SOURCEID-LENGTH                      01120033
011300*       END-CALL                                                  01130033
011400        DISPLAY "Sqlite version is "                              01140031
011500                LINKAGE-SOURCEID(1:LS-SOURCEID-LENGTH)            01150031
011600                UPON SYSOUT                                       01160030
011700     END-IF                                                       01170030
011800     CALL 'SQLITE3A' USING SQLITE3-INITIALIZE                     01180005
011900     RETURNING LS-SQL-RC                                          01190021
012000     END-CALL                                                     01200005
012100D    DISPLAY 'INITIALIZE LS-SQL-RC=' LS-SQL-RC                    01210022
012200D            UPON SYSOUT                                          01220022
012300     IF LS-SQL-RC NOT = SQLITE-OK THEN                            01230021
012400        DISPLAY 'SQLITE-INITIALIZE FAILED. RC='                   01240007
012500            RETURN-CODE                                           01250007
012600            UPON SYSOUT                                           01260007
012700        GOBACK                                                    01270005
012800     END-IF                                                       01280005
012900     .                                                            01290006
013000 INITIALIZE-SQLITE.                                               01300011
013100*                                                                 01310011
013200* Note: in my testing, this creates a z/OS UNIX file in           01320011
013300* the /tmp subdirectory, with the name testcob1.sqlite3 .         01330034
013400* That is, the data base name is actually the UNIX file name.     01340011
013500* This can be absolute, as in my example, or relative to the      01350011
013600* user's UNIX $HOME directory if the value does not start with    01360011
013700* a slash character. This is the norm for UNIX file names.        01370011
013800*                                                                 01380011
013900* Note that a prefix of a tilde, ~/, does not expand to the user's01390011
014000* $HOME as it would in a UNIX shell.                              01400011
014100* Again, in my testing, the OPEN fails with SQLITE-CANTOPEN (14). 01410011
014200*                                                                 01420011
014300     MOVE Z'/tmp/testcob1.sqlite3' TO SQL-ZCHAR                   01430034
014400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-OPEN              01440007
014500          BY REFERENCE SQL-ZCHAR                                  01450007
014600          BY REFERENCE LS-DB                                      01460007
014700          RETURNING LS-SQL-RC                                     01470020
014800     END-CALL                                                     01480007
014900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01490033
015000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01500007
015100        DISPLAY 'SQLITE3-OPEN FAILED. RC='                        01510007
015200           LS-SQL-RC UPON SYSOUT                                  01520007
015300        GOBACK                                                    01530011
015400     END-IF                                                       01540007
015500     .                                                            01550011
015600 DROP-TABLE.                                                      01560011
015700     MOVE Z'DROP TABLE xz' TO SQL-ZCHAR                           01570025
015800     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              01580007
015900          BY VALUE LS-DB                                          01590007
016000          BY REFERENCE SQL-ZCHAR                                  01600007
016100          BY VALUE LS-ZERO                                        01610007
016200          BY VALUE LS-ZERO                                        01620007
016300          BY VALUE LS-ZERO                                        01630007
016400          RETURNING LS-SQL-RC                                     01640020
016500     END-CALL                                                     01650007
016600     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01660033
016700        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01670014
016800        DISPLAY 'DROP TABLE  FAILED. RC='                         01680013
016900           LS-SQL-RC UPON SYSOUT                                  01690013
017000     END-IF                                                       01700014
017100     .                                                            01710011
017200 CREATE-TABLE.                                                    01720011
017300     MOVE Z'CREATE TABLE xz(N INTEGER)' TO SQL-ZCHAR              01730025
017400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              01740007
017500          BY VALUE LS-DB                                          01750007
017600          BY REFERENCE SQL-ZCHAR                                  01760007
017700          BY VALUE LS-ZERO                                        01770007
017800          BY VALUE LS-ZERO                                        01780007
017900          BY VALUE LS-ZERO                                        01790007
018000          RETURNING LS-SQL-RC                                     01800020
018100     END-CALL                                                     01810007
018200D    DISPLAY 'CREATE TABLE RC=' LS-SQL-RC UPON SYSOUT             01820014
018300     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01830033
018400        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01840011
018500        DISPLAY 'CREATE TABLE  FAILED. RC='                       01850011
018600           LS-SQL-RC UPON SYSOUT                                  01860011
018700        GOBACK                                                    01870011
018800     END-IF                                                       01880011
018900     .                                                            01890011
019000 CREATE-INDEX.                                                    01900011
019100     MOVE Z'CREATE INDEX r1 ON xz(N)' TO SQL-ZCHAR                01910025
019200     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              01920007
019300          BY VALUE LS-DB                                          01930007
019400          BY REFERENCE SQL-ZCHAR                                  01940007
019500          BY VALUE LS-ZERO                                        01950007
019600          BY VALUE LS-ZERO                                        01960007
019700          BY VALUE LS-ZERO                                        01970007
019800          RETURNING LS-SQL-RC                                     01980020
019900     END-CALL                                                     01990007
020000D    DISPLAY 'CREATE TABLE RC=' RETURN-CODE UPON SYSOUT           02000014
020100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02010033
020200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02020007
020300        DISPLAY 'CREATE TABLE  FAILED. RC='                       02030007
020400           LS-SQL-RC UPON SYSOUT                                  02040007
020500        GOBACK                                                    02050011
020600     END-IF                                                       02060007
020700     .                                                            02070011
020800 PREPARE-INSERT.                                                  02080011
020900     MOVE Z'INSERT INTO xz(N) VALUES(?)' TO SQL-ZCHAR             02090025
021000     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-PREPARE           02100007
021100          BY VALUE LS-DB                                          02110007
021200          BY REFERENCE SQL-ZCHAR                                  02120007
021300          BY VALUE LS-MINUS-ONE                                   02130007
021400          BY REFERENCE LS-INSERT-STMT                             02140007
021500          BY VALUE LS-ZERO                                        02150007
021600          RETURNING LS-SQL-RC                                     02160021
021700     END-CALL                                                     02170007
021800D    DISPLAY 'PREPARE INSERT RC=' LS-SQL-RC UPON SYSOUT           02180014
021900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02190033
022000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02200007
022100        DISPLAY 'PREPARE OF INSERT FAILED. RC='                   02210007
022200           LS-SQL-RC UPON SYSOUT                                  02220007
022300        GOBACK                                                    02230011
022400     END-IF                                                       02240007
022500     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-CHANGES           02250012
022600          BY VALUE LS-DB                                          02260011
022700          RETURNING LS-SQL-RC                                     02270020
022800     END-CALL                                                     02280011
022900     DISPLAY 'CHANGES BEFORE BEGIN=' LS-SQL-RC                    02290020
023000       UPON SYSOUT                                                02300011
023100     .                                                            02310011
023200 BEGIN-TRANSACTION.                                               02320011
023300     MOVE Z'BEGIN TRANSACTION' TO SQL-ZCHAR                       02330007
023400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              02340007
023500          BY VALUE LS-DB                                          02350007
023600          BY REFERENCE SQL-ZCHAR                                  02360007
023700          BY VALUE LS-ZERO                                        02370007
023800          BY VALUE LS-ZERO                                        02380007
023900          BY VALUE LS-ZERO                                        02390007
024000          RETURNING LS-SQL-RC                                     02400020
024100     END-CALL                                                     02410007
024200D    DISPLAY 'BEGIN TRANSACTION RC=' LS-SQL-RC UPON SYSOUT        02420014
024300     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02430033
024400        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02440007
024500        DISPLAY 'BEGIN TRANSACTION. RC='                          02450007
024600           LS-SQL-RC UPON SYSOUT                                  02460007
024700        GOBACK                                                    02470011
024800     END-IF                                                       02480007
024900     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-1, LE-FC          02490016
025000     .                                                            02500011
025100 INSERT-VALUES.                                                   02510011
025200*                                                                 02520011
025300* Inline PERFORM to insert values.                                02530011
025400     PERFORM VARYING LS-I FROM 0 BY 1 UNTIL LS-I >= 50000         02540020
025500* Bind the host variable contents to the prepared statement       02550011
025600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-BIND-INT          02560007
025700          BY VALUE LS-INSERT-STMT                                 02570007
025800          BY VALUE LS-ONE                                         02580007
025900          BY VALUE LS-I                                           02590007
026000          RETURNING LS-SQL-RC                                     02600020
026100     END-CALL                                                     02610007
026200D    DISPLAY 'INSERT BIND RC=' LS-SQL-RC UPON SYSOUT              02620014
026300     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02630033
026400        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02640007
026500        DISPLAY 'INSERT BIND FAILED. RC='                         02650011
026600           LS-SQL-RC UPON SYSOUT                                  02660007
026700        GOBACK                                                    02670011
026800     END-IF                                                       02680007
026900* Actually insert the data.                                       02690011
027000     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-STEP              02700007
027100          BY VALUE LS-INSERT-STMT                                 02710007
027200          RETURNING LS-SQL-RC                                     02720020
027300     END-CALL                                                     02730007
027400D    DISPLAY 'INSERT STEP RC=' LS-SQL-RC                          02740014
027500D            ' VALUE=' LS-I                                       02750014
027600D            UPON SYSOUT                                          02760014
027700     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02770033
027800        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02780007
027900        DISPLAY 'INSERT (STEP) FAILED. RC='                       02790007
028000           LS-SQL-RC UPON SYSOUT                                  02800007
028100        GOBACK                                                    02810011
028200     END-IF                                                       02820007
028300* Do a "reset" to reset the prepared statement for reuse.         02830011
028400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-RESET             02840007
028500          BY VALUE LS-INSERT-STMT                                 02850007
028600          RETURNING LS-SQL-RC                                     02860021
028700     END-CALL                                                     02870007
028800D    DISPLAY 'INSERT RESET RC=' LS-SQL-RC UPON SYSOUT             02880014
028900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02890033
029000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02900007
029100        DISPLAY 'RESET FAILED. RC='                               02910007
029200           LS-SQL-RC UPON SYSOUT                                  02920007
029300        GOBACK                                                    02930011
029400     END-IF                                                       02940007
029500     END-PERFORM                                                  02950007
029600* End of INSERT loop                                              02960011
029700     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-CHANGES           02970012
029800          BY VALUE LS-DB                                          02980011
029900          RETURNING LS-SQL-RC                                     02990020
030000     END-CALL                                                     03000011
030100     DISPLAY 'CHANGES BEFORE COMMIT=' LS-SQL-RC                   03010020
030200             UPON SYSOUT                                          03020014
030300* Finalize the prepared statement to release resources.           03030011
030400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-FINALIZE          03040007
030500          BY VALUE LS-INSERT-STMT                                 03050007
030600          RETURNING LS-SQL-RC                                     03060021
030700     END-CALL                                                     03070007
030800*    MOVE RETURN-CODE TO LS-SQL-RC                                03080021
030900D    DISPLAY 'INSERT FINIALIZE RC=' LS-SQL-RC UPON SYSOUT         03090014
031000     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   03100033
031100        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                03110007
031200        DISPLAY 'FINALIZE FAILED. RC='                            03120007
031300           LS-SQL-RC UPON SYSOUT                                  03130007
031400        GOBACK                                                    03140011
031500     END-IF                                                       03150007
031600     .                                                            03160011
031700 COMMIT-TRANSACTION.                                              03170011
031800* Commit the data just inserted.                                  03180011
031900     MOVE Z'COMMIT TRANSACTION' TO SQL-ZCHAR                      03190014
032000     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              03200007
032100          BY VALUE LS-DB                                          03210007
032200          BY REFERENCE SQL-ZCHAR                                  03220007
032300          BY VALUE LS-ZERO                                        03230007
032400          BY VALUE LS-ZERO                                        03240007
032500          BY VALUE LS-ZERO                                        03250007
032600          RETURNING LS-SQL-RC                                     03260020
032700     END-CALL                                                     03270007
032800D    DISPLAY 'COMMIT EXEC RC=' LS-SQL-RC UPON SYSOUT              03280014
032900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   03290033
033000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                03300007
033100        DISPLAY 'DROP TABLE  FAILED. RC='                         03310007
033200           LS-SQL-RC UPON SYSOUT                                  03320007
033300        GOBACK                                                    03330011
033400     END-IF                                                       03340007
033500     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-CHANGES           03350012
033600          BY VALUE LS-DB                                          03360011
033700          RETURNING LS-SQL-RC                                     03370020
033800     END-CALL                                                     03380011
033900     DISPLAY 'CHANGES AFTER COMMIT=' LS-SQL-RC                    03390020
034000             UPON SYSOUT                                          03400014
034100     .                                                            03410011
034200 GET-TIMING-1.                                                    03420011
034300     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-2, LE-FC          03430009
034400     SUBTRACT LS-GMT-SECS-1 FROM LS-GMT-SECS-2                    03440009
034500              GIVING LS-GMT-SECS-4.                               03450009
034600     DISPLAY 'TIME DIFFERENCE #1=' LS-GMT-SECS-4                  03460009
034700             UPON SYSOUT                                          03470008
034800     .                                                            03480007
034900 PREPARE-SELECT.                                                  03490011
035000     MOVE Z'SELECT N FROM xz;' TO SQL-ZCHAR                       03500025
035100     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-PREPARE           03510011
035200          BY VALUE LS-DB                                          03520011
035300          BY REFERENCE SQL-ZCHAR                                  03530011
035400          BY VALUE LS-MINUS-ONE                                   03540011
035500          BY REFERENCE LS-SELECT-STMT                             03550011
035600          BY VALUE LS-ZERO                                        03560011
035700          RETURNING LS-SQL-RC                                     03570020
035800     END-CALL                                                     03580011
035900*    MOVE RETURN-CODE TO LS-SQL-RC                                03590020
036000D    DISPLAY 'SELECT PREPARE RC=' LS-SQL-RC UPON SYSOUT           03600014
036100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   03610033
036200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                03620011
036300        DISPLAY 'PREPARE OF SELECT FAILED. RC='                   03630011
036400           LS-SQL-RC UPON SYSOUT                                  03640011
036500        GOBACK                                                    03650011
036600     END-IF                                                       03660011
036700     .                                                            03670011
036800 SETUP-SELECT-LOOP.                                               03680011
036900     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-4, LE-FC          03690011
037000     MOVE ZERO TO LS-SQL-RC                                       03700012
037100     MOVE ZERO TO LS-I                                            03710012
037200     MOVE ZERO TO LS-RECORD-COUNT                                 03720014
037300     .                                                            03730011
037400 SELECT-LOOP.                                                     03740011
037500* Inline PERFORM to insert values.                                03750011
037600     PERFORM UNTIL LS-SQL-RC NOT = 0                              03760011
037700* Bind the host variable contents to the prepared statement       03770011
037800D    DISPLAY 'LS-RECORD-COUNT=' LS-RECORD-COUNT UPON SYSOUT       03780014
037900D    DISPLAY 'LS-ONE=' LS-ONE UPON SYSOUT                         03790014
038000     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-STEP              03800011
038100          BY VALUE LS-SELECT-STMT                                 03810011
038200          RETURNING LS-SQL-RC                                     03820020
038300     END-CALL                                                     03830011
038400D    DISPLAY 'SELECT STEP RC=' LS-SQL-RC UPON SYSOUT              03840014
038500     EVALUATE LS-SQL-RC                                           03850011
038600     WHEN SQLITE-OK                                               03860011
038700D         DISPLAY 'SQLITE-OK' UPON SYSOUT                         03870014
038800          ADD +1 TO LS-RECORD-COUNT                               03880014
038900          MOVE 0 TO LS-SQL-RC                                     03890016
039000     WHEN SQLITE-ROW                                              03900011
039100D         DISPLAY 'SQLITE-ROW' UPON SYSOUT                        03910014
039200          ADD +1 TO LS-RECORD-COUNT                               03920014
039300D         CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-COLUMN-INT   03930018
039400D              BY VALUE LS-SELECT-STMT                            03940018
039500D              BY VALUE LS-ZERO                                   03950018
039600D              RETURNING LS-I                                     03960020
039700D         END-CALL                                                03970018
039800D         DISPLAY 'GOT VALUE ' LS-I ' ON CALL ' LS-RECORD-COUNT   03980014
039900D                 UPON SYSOUT                                     03990019
040000          MOVE 0 TO LS-SQL-RC                                     04000016
040100     WHEN SQLITE-DONE                                             04010011
040200D         DISPLAY 'SQLITE-DONE' UPON SYSOUT                       04020014
040300          MOVE +4 TO LS-SQL-RC                                    04030011
040400     WHEN OTHER                                                   04040011
040500D         DISPLAY 'OTHER' UPON SYSOUT                             04050014
040600          DISPLAY 'SELECT FAILED ERROR=' LS-SQL-RC                04060011
040700                  'COUNT=' LS-RECORD-COUNT                        04070014
040800                  UPON SYSOUT                                     04080011
040900          MOVE +8 TO LS-SQL-RC                                    04090011
041000     END-EVALUATE                                                 04100011
041100     END-PERFORM                                                  04110011
041200* End of INSERT loop                                              04120011
041300* Finalize the prepared statement to release resources.           04130011
041400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-FINALIZE          04140011
041500          BY VALUE LS-SELECT-STMT                                 04150011
041600          RETURNING LS-SQL-RC                                     04160020
041700     END-CALL                                                     04170011
041800D    DISPLAY 'SELECT FINALIZE RC=' LS-SQL-RC UPON SYSOUT          04180014
041900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   04190033
042000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                04200011
042100        DISPLAY 'FINALIZE FAILED. RC='                            04210011
042200           LS-SQL-RC UPON SYSOUT                                  04220011
042300        GOBACK                                                    04230011
042400     END-IF                                                       04240011
042500     .                                                            04250011
042600                                                                  04260011
042700 GET-TIMING-2.                                                    04270011
042800     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-5, LE-FC          04280011
042900     SUBTRACT LS-GMT-SECS-4 FROM LS-GMT-SECS-5                    04290014
043000              GIVING LS-GMT-SECS-6.                               04300011
043100     DISPLAY 'SELECT TIME=' LS-GMT-SECS-6 ' '                     04310014
043200             LS-RECORD-COUNT ' ROWS SELECTED.'                    04320014
043300             UPON SYSOUT                                          04330011
043400     .                                                            04340011
043500                                                                  04350011
043600 SHUTDOWN.                                                        04360007
043700     CALL 'SQLITE3A' USING SQLITE3-SHUTDOWN                       04370005
043800          RETURNING LS-SQL-RC                                     04380020
043900     END-CALL                                                     04390005
044000     GOBACK                                                       04400000
044100     .                                                            04410000
044200 END PROGRAM 'TESTCOB1'.                                          04420006
