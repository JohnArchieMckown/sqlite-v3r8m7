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
005300     05  LS-LIBVERSION-POINTER   POINTER.                         00530035
005400     05  LS-LIBVERSION-BINVALUE  REDEFINES LS-LIBVERSION-POINTER  00540035
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
007700 77  LS-LIBVERSION-LENGTH    PIC S9(9) COMP-5                     00770035
007800        VALUE IS 0.                                               00780035
007900 LINKAGE SECTION.                                                 00790000
008000 01  MVS-PARM.                                                    00800005
008100     05 MVS-PARM-LENGTH      PIC S9(4) COMP-5.                    00810005
008200     05 MVS-PARM-VALUE       PIC X(32760).                        00820005
008300 77  LINKAGE-LIBVERSION      PIC X(101).                          00830035
008400* MVS-PARM-VALUE LENGTH IS REALLY ONLY THE NUMBER OF CHARACTERS   00840005
008500* CONTAINED IN MVS-PARM-LENGTH. YOU NEED TO USE REFERENCE         00850005
008600* MODIFICATION TO MAKE SURE YOU DON'T EXCEED THE ACTUAL LENGTH    00860005
008700* PASSED TO THIS PROGRAM.                                         00870005
008800*                                                                 00880000
008900 PROCEDURE DIVISION USING MVS-PARM.                               00890000
009000 START-UP.                                                        00900000
009100**                                                                00910031
009200** How to access the data when SQLITE returns a                   00920031
009300** pointer. The following shows how to get the pointer            00930031
009400** and then determine the length of the C "string"                00940031
009500** which ends with a LOW-VALUES (0x00).                           00950031
009600     CALL 'SQLITE3A' USING SQLITE3-LIBVERSION                     00960031
009700          RETURNING LS-LIBVERSION-POINTER                         00970035
009800     END-CALL                                                     00980031
009900     IF LS-LIBVERSION-POINTER NOT EQUAL TO NULL THEN              00990035
010000        SET ADDRESS OF LINKAGE-LIBVERSION TO LS-LIBVERSION-POINTER01000035
010100*                                                                 01010032
010200* Get the length of the returned string by finding the first      01020032
010300* LOW-VALUE (0x00). Probably not as efficient as calling the      01030033
010400* C "strlen" routine, but is "pure" COBOL.                        01040033
010500        MOVE ZERO TO LS-LIBVERSION-LENGTH                         01050035
010600        INSPECT LINKAGE-LIBVERSION                                01060035
010700                TALLYING LS-LIBVERSION-LENGTH                     01070035
010800                FOR CHARACTERS BEFORE INITIAL LOW-VALUE           01080032
010900*                                                                 01090033
011000* If you're interested, this is how to call "strlen". It requires 01100033
011100* that CEE.SCEELKED be available for dynamic calling or linking   01110033
011200* depending on the compile option DYNAM or NODYNAM, respectively. 01120033
011300*       CALL    'STRLEN' USING BY VALUE LS-LIBVERSION-POINTER     01130035
011400*               RETURNING LS-LIBVERSION-LENGTH                    01140035
011500*       END-CALL                                                  01150033
011600        DISPLAY "Sqlite version is "                              01160031
011700                LINKAGE-LIBVERSION(1:LS-LIBVERSION-LENGTH)        01170035
011800                UPON SYSOUT                                       01180030
011900     END-IF                                                       01190030
012000     CALL 'SQLITE3A' USING SQLITE3-INITIALIZE                     01200005
012100     RETURNING LS-SQL-RC                                          01210021
012200     END-CALL                                                     01220005
012300D    DISPLAY 'INITIALIZE LS-SQL-RC=' LS-SQL-RC                    01230022
012400D            UPON SYSOUT                                          01240022
012500     IF LS-SQL-RC NOT = SQLITE-OK THEN                            01250021
012600        DISPLAY 'SQLITE-INITIALIZE FAILED. RC='                   01260007
012700            RETURN-CODE                                           01270007
012800            UPON SYSOUT                                           01280007
012900        GOBACK                                                    01290005
013000     END-IF                                                       01300005
013100     .                                                            01310006
013200 INITIALIZE-SQLITE.                                               01320011
013300*                                                                 01330011
013400* Note: in my testing, this creates a z/OS UNIX file in           01340011
013500* the /tmp subdirectory, with the name testcob1.sqlite3 .         01350034
013600* That is, the data base name is actually the UNIX file name.     01360011
013700* This can be absolute, as in my example, or relative to the      01370011
013800* user's UNIX $HOME directory if the value does not start with    01380011
013900* a slash character. This is the norm for UNIX file names.        01390011
014000*                                                                 01400011
014100* Note that a prefix of a tilde, ~/, does not expand to the user's01410011
014200* $HOME as it would in a UNIX shell.                              01420011
014300* Again, in my testing, the OPEN fails with SQLITE-CANTOPEN (14). 01430011
014400*                                                                 01440011
014500     MOVE Z'/tmp/testcob1.sqlite3' TO SQL-ZCHAR                   01450034
014600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-OPEN              01460007
014700          BY REFERENCE SQL-ZCHAR                                  01470007
014800          BY REFERENCE LS-DB                                      01480007
014900          RETURNING LS-SQL-RC                                     01490020
015000     END-CALL                                                     01500007
015100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01510033
015200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01520007
015300        DISPLAY 'SQLITE3-OPEN FAILED. RC='                        01530007
015400           LS-SQL-RC UPON SYSOUT                                  01540007
015500        GOBACK                                                    01550011
015600     END-IF                                                       01560007
015700     .                                                            01570011
015800 DROP-TABLE.                                                      01580011
015900     MOVE Z'DROP TABLE xz' TO SQL-ZCHAR                           01590025
016000     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              01600007
016100          BY VALUE LS-DB                                          01610007
016200          BY REFERENCE SQL-ZCHAR                                  01620007
016300          BY VALUE LS-ZERO                                        01630007
016400          BY VALUE LS-ZERO                                        01640007
016500          BY VALUE LS-ZERO                                        01650007
016600          RETURNING LS-SQL-RC                                     01660020
016700     END-CALL                                                     01670007
016800     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01680033
016900        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01690014
017000        DISPLAY 'DROP TABLE  FAILED. RC='                         01700013
017100           LS-SQL-RC UPON SYSOUT                                  01710013
017200     END-IF                                                       01720014
017300     .                                                            01730011
017400 CREATE-TABLE.                                                    01740011
017500     MOVE Z'CREATE TABLE xz(N INTEGER)' TO SQL-ZCHAR              01750025
017600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              01760007
017700          BY VALUE LS-DB                                          01770007
017800          BY REFERENCE SQL-ZCHAR                                  01780007
017900          BY VALUE LS-ZERO                                        01790007
018000          BY VALUE LS-ZERO                                        01800007
018100          BY VALUE LS-ZERO                                        01810007
018200          RETURNING LS-SQL-RC                                     01820020
018300     END-CALL                                                     01830007
018400D    DISPLAY 'CREATE TABLE RC=' LS-SQL-RC UPON SYSOUT             01840014
018500     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   01850033
018600        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                01860011
018700        DISPLAY 'CREATE TABLE  FAILED. RC='                       01870011
018800           LS-SQL-RC UPON SYSOUT                                  01880011
018900        GOBACK                                                    01890011
019000     END-IF                                                       01900011
019100     .                                                            01910011
019200 CREATE-INDEX.                                                    01920011
019300     MOVE Z'CREATE INDEX r1 ON xz(N)' TO SQL-ZCHAR                01930025
019400     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              01940007
019500          BY VALUE LS-DB                                          01950007
019600          BY REFERENCE SQL-ZCHAR                                  01960007
019700          BY VALUE LS-ZERO                                        01970007
019800          BY VALUE LS-ZERO                                        01980007
019900          BY VALUE LS-ZERO                                        01990007
020000          RETURNING LS-SQL-RC                                     02000020
020100     END-CALL                                                     02010007
020200D    DISPLAY 'CREATE TABLE RC=' RETURN-CODE UPON SYSOUT           02020014
020300     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02030033
020400        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02040007
020500        DISPLAY 'CREATE TABLE  FAILED. RC='                       02050007
020600           LS-SQL-RC UPON SYSOUT                                  02060007
020700        GOBACK                                                    02070011
020800     END-IF                                                       02080007
020900     .                                                            02090011
021000 PREPARE-INSERT.                                                  02100011
021100     MOVE Z'INSERT INTO xz(N) VALUES(?)' TO SQL-ZCHAR             02110025
021200     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-PREPARE           02120007
021300          BY VALUE LS-DB                                          02130007
021400          BY REFERENCE SQL-ZCHAR                                  02140007
021500          BY VALUE LS-MINUS-ONE                                   02150007
021600          BY REFERENCE LS-INSERT-STMT                             02160007
021700          BY VALUE LS-ZERO                                        02170007
021800          RETURNING LS-SQL-RC                                     02180021
021900     END-CALL                                                     02190007
022000D    DISPLAY 'PREPARE INSERT RC=' LS-SQL-RC UPON SYSOUT           02200014
022100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02210033
022200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02220007
022300        DISPLAY 'PREPARE OF INSERT FAILED. RC='                   02230007
022400           LS-SQL-RC UPON SYSOUT                                  02240007
022500        GOBACK                                                    02250011
022600     END-IF                                                       02260007
022700     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-CHANGES           02270012
022800          BY VALUE LS-DB                                          02280011
022900          RETURNING LS-SQL-RC                                     02290020
023000     END-CALL                                                     02300011
023100     DISPLAY 'CHANGES BEFORE BEGIN=' LS-SQL-RC                    02310020
023200       UPON SYSOUT                                                02320011
023300     .                                                            02330011
023400 BEGIN-TRANSACTION.                                               02340011
023500     MOVE Z'BEGIN TRANSACTION' TO SQL-ZCHAR                       02350007
023600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              02360007
023700          BY VALUE LS-DB                                          02370007
023800          BY REFERENCE SQL-ZCHAR                                  02380007
023900          BY VALUE LS-ZERO                                        02390007
024000          BY VALUE LS-ZERO                                        02400007
024100          BY VALUE LS-ZERO                                        02410007
024200          RETURNING LS-SQL-RC                                     02420020
024300     END-CALL                                                     02430007
024400D    DISPLAY 'BEGIN TRANSACTION RC=' LS-SQL-RC UPON SYSOUT        02440014
024500     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02450033
024600        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02460007
024700        DISPLAY 'BEGIN TRANSACTION. RC='                          02470007
024800           LS-SQL-RC UPON SYSOUT                                  02480007
024900        GOBACK                                                    02490011
025000     END-IF                                                       02500007
025100     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-1, LE-FC          02510016
025200     .                                                            02520011
025300 INSERT-VALUES.                                                   02530011
025400*                                                                 02540011
025500* Inline PERFORM to insert values.                                02550011
025600     PERFORM VARYING LS-I FROM 0 BY 1 UNTIL LS-I >= 50000         02560020
025700* Bind the host variable contents to the prepared statement       02570011
025800     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-BIND-INT          02580007
025900          BY VALUE LS-INSERT-STMT                                 02590007
026000          BY VALUE LS-ONE                                         02600007
026100          BY VALUE LS-I                                           02610007
026200          RETURNING LS-SQL-RC                                     02620020
026300     END-CALL                                                     02630007
026400D    DISPLAY 'INSERT BIND RC=' LS-SQL-RC UPON SYSOUT              02640014
026500     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02650033
026600        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02660007
026700        DISPLAY 'INSERT BIND FAILED. RC='                         02670011
026800           LS-SQL-RC UPON SYSOUT                                  02680007
026900        GOBACK                                                    02690011
027000     END-IF                                                       02700007
027100* Actually insert the data.                                       02710011
027200     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-STEP              02720007
027300          BY VALUE LS-INSERT-STMT                                 02730007
027400          RETURNING LS-SQL-RC                                     02740020
027500     END-CALL                                                     02750007
027600D    DISPLAY 'INSERT STEP RC=' LS-SQL-RC                          02760014
027700D            ' VALUE=' LS-I                                       02770014
027800D            UPON SYSOUT                                          02780014
027900     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02790033
028000        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02800007
028100        DISPLAY 'INSERT (STEP) FAILED. RC='                       02810007
028200           LS-SQL-RC UPON SYSOUT                                  02820007
028300        GOBACK                                                    02830011
028400     END-IF                                                       02840007
028500* Do a "reset" to reset the prepared statement for reuse.         02850011
028600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-RESET             02860007
028700          BY VALUE LS-INSERT-STMT                                 02870007
028800          RETURNING LS-SQL-RC                                     02880021
028900     END-CALL                                                     02890007
029000D    DISPLAY 'INSERT RESET RC=' LS-SQL-RC UPON SYSOUT             02900014
029100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   02910033
029200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                02920007
029300        DISPLAY 'RESET FAILED. RC='                               02930007
029400           LS-SQL-RC UPON SYSOUT                                  02940007
029500        GOBACK                                                    02950011
029600     END-IF                                                       02960007
029700     END-PERFORM                                                  02970007
029800* End of INSERT loop                                              02980011
029900     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-CHANGES           02990012
030000          BY VALUE LS-DB                                          03000011
030100          RETURNING LS-SQL-RC                                     03010020
030200     END-CALL                                                     03020011
030300     DISPLAY 'CHANGES BEFORE COMMIT=' LS-SQL-RC                   03030020
030400             UPON SYSOUT                                          03040014
030500* Finalize the prepared statement to release resources.           03050011
030600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-FINALIZE          03060007
030700          BY VALUE LS-INSERT-STMT                                 03070007
030800          RETURNING LS-SQL-RC                                     03080021
030900     END-CALL                                                     03090007
031000*    MOVE RETURN-CODE TO LS-SQL-RC                                03100021
031100D    DISPLAY 'INSERT FINIALIZE RC=' LS-SQL-RC UPON SYSOUT         03110014
031200     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   03120033
031300        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                03130007
031400        DISPLAY 'FINALIZE FAILED. RC='                            03140007
031500           LS-SQL-RC UPON SYSOUT                                  03150007
031600        GOBACK                                                    03160011
031700     END-IF                                                       03170007
031800     .                                                            03180011
031900 COMMIT-TRANSACTION.                                              03190011
032000* Commit the data just inserted.                                  03200011
032100     MOVE Z'COMMIT TRANSACTION' TO SQL-ZCHAR                      03210014
032200     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-EXEC              03220007
032300          BY VALUE LS-DB                                          03230007
032400          BY REFERENCE SQL-ZCHAR                                  03240007
032500          BY VALUE LS-ZERO                                        03250007
032600          BY VALUE LS-ZERO                                        03260007
032700          BY VALUE LS-ZERO                                        03270007
032800          RETURNING LS-SQL-RC                                     03280020
032900     END-CALL                                                     03290007
033000D    DISPLAY 'COMMIT EXEC RC=' LS-SQL-RC UPON SYSOUT              03300014
033100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   03310033
033200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                03320007
033300        DISPLAY 'DROP TABLE  FAILED. RC='                         03330007
033400           LS-SQL-RC UPON SYSOUT                                  03340007
033500        GOBACK                                                    03350011
033600     END-IF                                                       03360007
033700     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-CHANGES           03370012
033800          BY VALUE LS-DB                                          03380011
033900          RETURNING LS-SQL-RC                                     03390020
034000     END-CALL                                                     03400011
034100     DISPLAY 'CHANGES AFTER COMMIT=' LS-SQL-RC                    03410020
034200             UPON SYSOUT                                          03420014
034300     .                                                            03430011
034400 GET-TIMING-1.                                                    03440011
034500     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-2, LE-FC          03450009
034600     SUBTRACT LS-GMT-SECS-1 FROM LS-GMT-SECS-2                    03460009
034700              GIVING LS-GMT-SECS-4.                               03470009
034800     DISPLAY 'TIME DIFFERENCE #1=' LS-GMT-SECS-4                  03480009
034900             UPON SYSOUT                                          03490008
035000     .                                                            03500007
035100 PREPARE-SELECT.                                                  03510011
035200     MOVE Z'SELECT N FROM xz;' TO SQL-ZCHAR                       03520025
035300     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-PREPARE           03530011
035400          BY VALUE LS-DB                                          03540011
035500          BY REFERENCE SQL-ZCHAR                                  03550011
035600          BY VALUE LS-MINUS-ONE                                   03560011
035700          BY REFERENCE LS-SELECT-STMT                             03570011
035800          BY VALUE LS-ZERO                                        03580011
035900          RETURNING LS-SQL-RC                                     03590020
036000     END-CALL                                                     03600011
036100*    MOVE RETURN-CODE TO LS-SQL-RC                                03610020
036200D    DISPLAY 'SELECT PREPARE RC=' LS-SQL-RC UPON SYSOUT           03620014
036300     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   03630033
036400        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                03640011
036500        DISPLAY 'PREPARE OF SELECT FAILED. RC='                   03650011
036600           LS-SQL-RC UPON SYSOUT                                  03660011
036700        GOBACK                                                    03670011
036800     END-IF                                                       03680011
036900     .                                                            03690011
037000 SETUP-SELECT-LOOP.                                               03700011
037100     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-4, LE-FC          03710011
037200     MOVE ZERO TO LS-SQL-RC                                       03720012
037300     MOVE ZERO TO LS-I                                            03730012
037400     MOVE ZERO TO LS-RECORD-COUNT                                 03740014
037500     .                                                            03750011
037600 SELECT-LOOP.                                                     03760011
037700* Inline PERFORM to insert values.                                03770011
037800     PERFORM UNTIL LS-SQL-RC NOT = 0                              03780011
037900* Bind the host variable contents to the prepared statement       03790011
038000D    DISPLAY 'LS-RECORD-COUNT=' LS-RECORD-COUNT UPON SYSOUT       03800014
038100D    DISPLAY 'LS-ONE=' LS-ONE UPON SYSOUT                         03810014
038200     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-STEP              03820011
038300          BY VALUE LS-SELECT-STMT                                 03830011
038400          RETURNING LS-SQL-RC                                     03840020
038500     END-CALL                                                     03850011
038600D    DISPLAY 'SELECT STEP RC=' LS-SQL-RC UPON SYSOUT              03860014
038700     EVALUATE LS-SQL-RC                                           03870011
038800     WHEN SQLITE-OK                                               03880011
038900D         DISPLAY 'SQLITE-OK' UPON SYSOUT                         03890014
039000          ADD +1 TO LS-RECORD-COUNT                               03900014
039100          MOVE 0 TO LS-SQL-RC                                     03910016
039200     WHEN SQLITE-ROW                                              03920011
039300D         DISPLAY 'SQLITE-ROW' UPON SYSOUT                        03930014
039400          ADD +1 TO LS-RECORD-COUNT                               03940014
039500D         CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-COLUMN-INT   03950018
039600D              BY VALUE LS-SELECT-STMT                            03960018
039700D              BY VALUE LS-ZERO                                   03970018
039800D              RETURNING LS-I                                     03980020
039900D         END-CALL                                                03990018
040000D         DISPLAY 'GOT VALUE ' LS-I ' ON CALL ' LS-RECORD-COUNT   04000014
040100D                 UPON SYSOUT                                     04010019
040200          MOVE 0 TO LS-SQL-RC                                     04020016
040300     WHEN SQLITE-DONE                                             04030011
040400D         DISPLAY 'SQLITE-DONE' UPON SYSOUT                       04040014
040500          MOVE +4 TO LS-SQL-RC                                    04050011
040600     WHEN OTHER                                                   04060011
040700D         DISPLAY 'OTHER' UPON SYSOUT                             04070014
040800          DISPLAY 'SELECT FAILED ERROR=' LS-SQL-RC                04080011
040900                  'COUNT=' LS-RECORD-COUNT                        04090014
041000                  UPON SYSOUT                                     04100011
041100          MOVE +8 TO LS-SQL-RC                                    04110011
041200     END-EVALUATE                                                 04120011
041300     END-PERFORM                                                  04130011
041400* End of INSERT loop                                              04140011
041500* Finalize the prepared statement to release resources.           04150011
041600     CALL 'SQLITE3A' USING BY REFERENCE SQLITE3-FINALIZE          04160011
041700          BY VALUE LS-SELECT-STMT                                 04170011
041800          RETURNING LS-SQL-RC                                     04180020
041900     END-CALL                                                     04190011
042000D    DISPLAY 'SELECT FINALIZE RC=' LS-SQL-RC UPON SYSOUT          04200014
042100     IF LS-SQL-RC IS NOT EQUAL TO SQLITE-OK AND                   04210033
042200        LS-SQL-RC IS NOT EQUAL TO SQLITE-DONE THEN                04220011
042300        DISPLAY 'FINALIZE FAILED. RC='                            04230011
042400           LS-SQL-RC UPON SYSOUT                                  04240011
042500        GOBACK                                                    04250011
042600     END-IF                                                       04260011
042700     .                                                            04270011
042800                                                                  04280011
042900 GET-TIMING-2.                                                    04290011
043000     CALL 'CEEGMT' USING LS-LILIAN, LS-GMT-SECS-5, LE-FC          04300011
043100     SUBTRACT LS-GMT-SECS-4 FROM LS-GMT-SECS-5                    04310014
043200              GIVING LS-GMT-SECS-6.                               04320011
043300     DISPLAY 'SELECT TIME=' LS-GMT-SECS-6 ' '                     04330014
043400             LS-RECORD-COUNT ' ROWS SELECTED.'                    04340014
043500             UPON SYSOUT                                          04350011
043600     .                                                            04360011
043700                                                                  04370011
043800 SHUTDOWN.                                                        04380007
043900     CALL 'SQLITE3A' USING SQLITE3-SHUTDOWN                       04390005
044000          RETURNING LS-SQL-RC                                     04400020
044100     END-CALL                                                     04410005
044200     GOBACK                                                       04420000
044300     .                                                            04430000
044400 END PROGRAM 'TESTCOB1'.                                          04440006
