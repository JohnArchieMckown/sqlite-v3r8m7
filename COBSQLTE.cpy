000100 01  SQLITE3-RETURN-VALUES.                                       00010003
000200     05 SQLITE-ABORT                         PIC S9(9) COMP-5     00020003
000300            VALUE IS 4 .                                          00030003
000400     05 SQLITE-ABORT-ROLLBACK                PIC S9(9) COMP-5     00040003
000500            VALUE IS 516 .                                        00050003
000600     05 SQLITE-ACCESS-EXISTS                 PIC S9(9) COMP-5     00060003
000700            VALUE IS 0 .                                          00070003
000800     05 SQLITE-ACCESS-READ                   PIC S9(9) COMP-5     00080003
000900            VALUE IS 2 .                                          00090003
001000     05 SQLITE-ACCESS-READWRITE              PIC S9(9) COMP-5     00100003
001100            VALUE IS 1 .                                          00110003
001200     05 SQLITE-ALTER-TABLE                   PIC S9(9) COMP-5     00120003
001300            VALUE IS 26 .                                         00130003
001400     05 SQLITE-ANALYZE                       PIC S9(9) COMP-5     00140003
001500            VALUE IS 28 .                                         00150003
001600     05 SQLITE-ANY                           PIC S9(9) COMP-5     00160003
001700            VALUE IS 5 .                                          00170003
001800     05 SQLITE-ATTACH                        PIC S9(9) COMP-5     00180003
001900            VALUE IS 24 .                                         00190003
002000     05 SQLITE-AUTH                          PIC S9(9) COMP-5     00200003
002100            VALUE IS 23 .                                         00210003
002200     05 SQLITE-BLOB                          PIC S9(9) COMP-5     00220003
002300            VALUE IS 4 .                                          00230003
002400     05 SQLITE-BUSY                          PIC S9(9) COMP-5     00240003
002500            VALUE IS 5 .                                          00250003
002600     05 SQLITE-BUSY-RECOVERY                 PIC S9(9) COMP-5     00260003
002700            VALUE IS 261 .                                        00270003
002800     05 SQLITE-CANTOPEN                      PIC S9(9) COMP-5     00280003
002900            VALUE IS 14 .                                         00290003
003000     05 SQLITE-CANTOPEN-FULLPATH             PIC S9(9) COMP-5     00300003
003100            VALUE IS 782 .                                        00310003
003200     05 SQLITE-CANTOPEN-ISDIR                PIC S9(9) COMP-5     00320003
003300            VALUE IS 526 .                                        00330003
003400     05 SQLITE-CANTOPEN-NOTEMPDIR            PIC S9(9) COMP-5     00340003
003500            VALUE IS 270 .                                        00350003
003600     05 SQLITE-CHECKPOINT-FULL               PIC S9(9) COMP-5     00360003
003700            VALUE IS 1 .                                          00370003
003800     05 SQLITE-CHECKPOINT-PASSIVE            PIC S9(9) COMP-5     00380003
003900            VALUE IS 0 .                                          00390003
004000     05 SQLITE-CHECKPOINT-RESTART            PIC S9(9) COMP-5     00400003
004100            VALUE IS 2 .                                          00410003
004200     05 SQLITE-CONFIG-COVRNG-INDEX-SCN       PIC S9(9) COMP-5     00420003
004300            VALUE IS 20 .                                         00430003
004400     05 SQLITE-CONFIG-GETMALLOC              PIC S9(9) COMP-5     00440003
004500            VALUE IS 5 .                                          00450003
004600     05 SQLITE-CONFIG-GETMUTEX               PIC S9(9) COMP-5     00460003
004700            VALUE IS 11 .                                         00470003
004800     05 SQLITE-CONFIG-GETPCACHE              PIC S9(9) COMP-5     00480003
004900            VALUE IS 15 .                                         00490003
005000     05 SQLITE-CONFIG-GETPCACHE2             PIC S9(9) COMP-5     00500003
005100            VALUE IS 19 .                                         00510003
005200     05 SQLITE-CONFIG-HEAP                   PIC S9(9) COMP-5     00520003
005300            VALUE IS 8 .                                          00530003
005400     05 SQLITE-CONFIG-LOG                    PIC S9(9) COMP-5     00540003
005500            VALUE IS 16 .                                         00550003
005600     05 SQLITE-CONFIG-LOOKASIDE              PIC S9(9) COMP-5     00560003
005700            VALUE IS 13 .                                         00570003
005800     05 SQLITE-CONFIG-MALLOC                 PIC S9(9) COMP-5     00580003
005900            VALUE IS 4 .                                          00590003
006000     05 SQLITE-CONFIG-MEMSTATUS              PIC S9(9) COMP-5     00600003
006100            VALUE IS 9 .                                          00610003
006200     05 SQLITE-CONFIG-MMAP-SIZE              PIC S9(9) COMP-5     00620003
006300            VALUE IS 22 .                                         00630003
006400     05 SQLITE-CONFIG-MULTITHREAD            PIC S9(9) COMP-5     00640003
006500            VALUE IS 2 .                                          00650003
006600     05 SQLITE-CONFIG-MUTEX                  PIC S9(9) COMP-5     00660003
006700            VALUE IS 10 .                                         00670003
006800     05 SQLITE-CONFIG-PAGECACHE              PIC S9(9) COMP-5     00680003
006900            VALUE IS 7 .                                          00690003
007000     05 SQLITE-CONFIG-PCACHE                 PIC S9(9) COMP-5     00700003
007100            VALUE IS 14 .                                         00710003
007200     05 SQLITE-CONFIG-PCACHE2                PIC S9(9) COMP-5     00720003
007300            VALUE IS 18 .                                         00730003
007400     05 SQLITE-CONFIG-SCRATCH                PIC S9(9) COMP-5     00740003
007500            VALUE IS 6 .                                          00750003
007600     05 SQLITE-CONFIG-SERIALIZED             PIC S9(9) COMP-5     00760003
007700            VALUE IS 3 .                                          00770003
007800     05 SQLITE-CONFIG-SINGLETHREAD           PIC S9(9) COMP-5     00780003
007900            VALUE IS 1 .                                          00790003
008000     05 SQLITE-CONFIG-SQLLOG                 PIC S9(9) COMP-5     00800003
008100            VALUE IS 21 .                                         00810003
008200     05 SQLITE-CONFIG-URI                    PIC S9(9) COMP-5     00820003
008300            VALUE IS 17 .                                         00830003
008400     05 SQLITE-CONSTRAINT                    PIC S9(9) COMP-5     00840003
008500            VALUE IS 19 .                                         00850003
008600     05 SQLITE-CONSTRAINT-CHECK              PIC S9(9) COMP-5     00860003
008700            VALUE IS 275 .                                        00870003
008800     05 SQLITE-CONSTRAINT-COMMITHOOK         PIC S9(9) COMP-5     00880003
008900            VALUE IS 531 .                                        00890003
009000     05 SQLITE-CONSTRAINT-FOREIGNKEY         PIC S9(9) COMP-5     00900003
009100            VALUE IS 787 .                                        00910003
009200     05 SQLITE-CONSTRAINT-FUNCTION           PIC S9(9) COMP-5     00920003
009300            VALUE IS 1043 .                                       00930003
009400     05 SQLITE-CONSTRAINT-NOTNULL            PIC S9(9) COMP-5     00940003
009500            VALUE IS 1299 .                                       00950003
009600     05 SQLITE-CONSTRAINT-PRIMARYKEY         PIC S9(9) COMP-5     00960003
009700            VALUE IS 1555 .                                       00970003
009800     05 SQLITE-CONSTRAINT-TRIGGER            PIC S9(9) COMP-5     00980003
009900            VALUE IS 1811 .                                       00990003
010000     05 SQLITE-CONSTRAINT-UNIQUE             PIC S9(9) COMP-5     01000003
010100            VALUE IS 2067 .                                       01010003
010200     05 SQLITE-CONSTRAINT-VTAB               PIC S9(9) COMP-5     01020003
010300            VALUE IS 2323 .                                       01030003
010400     05 SQLITE-COPY                          PIC S9(9) COMP-5     01040003
010500            VALUE IS 0 .                                          01050003
010600     05 SQLITE-CORRUPT                       PIC S9(9) COMP-5     01060003
010700            VALUE IS 11 .                                         01070003
010800     05 SQLITE-CORRUPT-VTAB                  PIC S9(9) COMP-5     01080003
010900            VALUE IS 267 .                                        01090003
011000     05 SQLITE-CREATE-INDEX                  PIC S9(9) COMP-5     01100003
011100            VALUE IS 1 .                                          01110003
011200     05 SQLITE-CREATE-TABLE                  PIC S9(9) COMP-5     01120003
011300            VALUE IS 2 .                                          01130003
011400     05 SQLITE-CREATE-TEMP-INDEX             PIC S9(9) COMP-5     01140003
011500            VALUE IS 3 .                                          01150003
011600     05 SQLITE-CREATE-TEMP-TABLE             PIC S9(9) COMP-5     01160003
011700            VALUE IS 4 .                                          01170003
011800     05 SQLITE-CREATE-TEMP-TRIGGER           PIC S9(9) COMP-5     01180003
011900            VALUE IS 5 .                                          01190003
012000     05 SQLITE-CREATE-TEMP-VIEW              PIC S9(9) COMP-5     01200003
012100            VALUE IS 6 .                                          01210003
012200     05 SQLITE-CREATE-TRIGGER                PIC S9(9) COMP-5     01220003
012300            VALUE IS 7 .                                          01230003
012400     05 SQLITE-CREATE-VIEW                   PIC S9(9) COMP-5     01240003
012500            VALUE IS 8 .                                          01250003
012600     05 SQLITE-CREATE-VTABLE                 PIC S9(9) COMP-5     01260003
012700            VALUE IS 29 .                                         01270003
012800     05 SQLITE-DBCONFIG-ENABLE-FKEY          PIC S9(9) COMP-5     01280003
012900            VALUE IS 1002 .                                       01290003
013000     05 SQLITE-DBCONFIG-ENABLE-TRIGGER       PIC S9(9) COMP-5     01300003
013100            VALUE IS 1003 .                                       01310003
013200     05 SQLITE-DBCONFIG-LOOKASIDE            PIC S9(9) COMP-5     01320003
013300            VALUE IS 1001 .                                       01330003
013400     05 SQLITE-DBSTATUS-CACHE-HIT            PIC S9(9) COMP-5     01340003
013500            VALUE IS 7 .                                          01350003
013600     05 SQLITE-DBSTATUS-CACHE-MISS           PIC S9(9) COMP-5     01360003
013700            VALUE IS 8 .                                          01370003
013800     05 SQLITE-DBSTATUS-CACHE-USED           PIC S9(9) COMP-5     01380003
013900            VALUE IS 1 .                                          01390003
014000     05 SQLITE-DBSTATUS-CACHE-WRITE          PIC S9(9) COMP-5     01400003
014100            VALUE IS 9 .                                          01410003
014200     05 SQLITE-DBSTATUS-LOOKASIDE-HIT        PIC S9(9) COMP-5     01420003
014300            VALUE IS 4 .                                          01430003
014400     05 SQLITE-DBSTAT-LKASD-MISS-FULL        PIC S9(9) COMP-5     01440003
014500            VALUE IS 6 .                                          01450003
014600     05 SQLITE-DBSTAT-LKASD-MISS-SIZE        PIC S9(9) COMP-5     01460003
014700            VALUE IS 5 .                                          01470003
014800     05 SQLITE-DBSTATUS-LOOKASIDE-USED       PIC S9(9) COMP-5     01480003
014900            VALUE IS 0 .                                          01490003
015000     05 SQLITE-DBSTATUS-MAX                  PIC S9(9) COMP-5     01500003
015100            VALUE IS 9 .                                          01510003
015200     05 SQLITE-DBSTATUS-SCHEMA-USED          PIC S9(9) COMP-5     01520003
015300            VALUE IS 2 .                                          01530003
015400     05 SQLITE-DBSTATUS-STMT-USED            PIC S9(9) COMP-5     01540003
015500            VALUE IS 3 .                                          01550003
015600     05 SQLITE-DELETE                        PIC S9(9) COMP-5     01560003
015700            VALUE IS 9 .                                          01570003
015800     05 SQLITE-DENY                          PIC S9(9) COMP-5     01580003
015900            VALUE IS 1 .                                          01590003
016000     05 SQLITE-DETACH                        PIC S9(9) COMP-5     01600003
016100            VALUE IS 25 .                                         01610003
016200     05 SQLITE-DONE                          PIC S9(9) COMP-5     01620003
016300            VALUE IS 101 .                                        01630003
016400     05 SQLITE-DROP-INDEX                    PIC S9(9) COMP-5     01640003
016500            VALUE IS 10 .                                         01650003
016600     05 SQLITE-DROP-TABLE                    PIC S9(9) COMP-5     01660003
016700            VALUE IS 11 .                                         01670003
016800     05 SQLITE-DROP-TEMP-INDEX               PIC S9(9) COMP-5     01680003
016900            VALUE IS 12 .                                         01690003
017000     05 SQLITE-DROP-TEMP-TABLE               PIC S9(9) COMP-5     01700003
017100            VALUE IS 13 .                                         01710003
017200     05 SQLITE-DROP-TEMP-TRIGGER             PIC S9(9) COMP-5     01720003
017300            VALUE IS 14 .                                         01730003
017400     05 SQLITE-DROP-TEMP-VIEW                PIC S9(9) COMP-5     01740003
017500            VALUE IS 15 .                                         01750003
017600     05 SQLITE-DROP-TRIGGER                  PIC S9(9) COMP-5     01760003
017700            VALUE IS 16 .                                         01770003
017800     05 SQLITE-DROP-VIEW                     PIC S9(9) COMP-5     01780003
017900            VALUE IS 17 .                                         01790003
018000     05 SQLITE-DROP-VTABLE                   PIC S9(9) COMP-5     01800003
018100            VALUE IS 30 .                                         01810003
018200     05 SQLITE-EMPTY                         PIC S9(9) COMP-5     01820003
018300            VALUE IS 16 .                                         01830003
018400     05 SQLITE-ERROR                         PIC S9(9) COMP-5     01840003
018500            VALUE IS 1 .                                          01850003
018600     05 SQLITE-FAIL                          PIC S9(9) COMP-5     01860003
018700            VALUE IS 3 .                                          01870003
018800     05 SQLITE-FCNTL-BUSYHANDLER             PIC S9(9) COMP-5     01880003
018900            VALUE IS 15 .                                         01890003
019000     05 SQLITE-FCNTL-CHUNK-SIZE              PIC S9(9) COMP-5     01900003
019100            VALUE IS 6 .                                          01910003
019200     05 SQLITE-FCNTL-FILE-POINTER            PIC S9(9) COMP-5     01920003
019300            VALUE IS 7 .                                          01930003
019400     05 SQLITE-FCNTL-LOCKSTATE               PIC S9(9) COMP-5     01940003
019500            VALUE IS 1 .                                          01950003
019600     05 SQLITE-FCNTL-MMAP-SIZE               PIC S9(9) COMP-5     01960003
019700            VALUE IS 18 .                                         01970003
019800     05 SQLITE-FCNTL-OVERWRITE               PIC S9(9) COMP-5     01980003
019900            VALUE IS 11 .                                         01990003
020000     05 SQLITE-FCNTL-PERSIST-WAL             PIC S9(9) COMP-5     02000003
020100            VALUE IS 10 .                                         02010003
020200     05 SQLITE-FCNTL-PWRSAFE-OVERWRITE       PIC S9(9) COMP-5     02020003
020300            VALUE IS 13 .                                         02030003
020400     05 SQLITE-FCNTL-PRAGMA                  PIC S9(9) COMP-5     02040003
020500            VALUE IS 14 .                                         02050003
020600     05 SQLITE-FCNTL-SIZE-HINT               PIC S9(9) COMP-5     02060003
020700            VALUE IS 5 .                                          02070003
020800     05 SQLITE-FCNTL-SYNC-OMITTED            PIC S9(9) COMP-5     02080003
020900            VALUE IS 8 .                                          02090003
021000     05 SQLITE-FCNTL-TEMPFILENAME            PIC S9(9) COMP-5     02100003
021100            VALUE IS 16 .                                         02110003
021200     05 SQLITE-FCNTL-VFSNAME                 PIC S9(9) COMP-5     02120003
021300            VALUE IS 12 .                                         02130003
021400     05 SQLITE-FCNTL-WIN32-AV-RETRY          PIC S9(9) COMP-5     02140003
021500            VALUE IS 9 .                                          02150003
021600     05 SQLITE-FLOAT                         PIC S9(9) COMP-5     02160003
021700            VALUE IS 2 .                                          02170003
021800     05 SQLITE-FORMAT                        PIC S9(9) COMP-5     02180003
021900            VALUE IS 24 .                                         02190003
022000     05 SQLITE-FULL                          PIC S9(9) COMP-5     02200003
022100            VALUE IS 13 .                                         02210003
022200     05 SQLITE-FUNCTION                      PIC S9(9) COMP-5     02220003
022300            VALUE IS 31 .                                         02230003
022400     05 SQLITE-GET-LOCKPROXYFILE             PIC S9(9) COMP-5     02240003
022500            VALUE IS 2 .                                          02250003
022600     05 SQLITE-IGNORE                        PIC S9(9) COMP-5     02260003
022700            VALUE IS 2 .                                          02270003
022800     05 SQLITE-INDEX-CONSTRAINT-EQ           PIC S9(9) COMP-5     02280003
022900            VALUE IS 2 .                                          02290003
023000     05 SQLITE-INDEX-CONSTRAINT-GE           PIC S9(9) COMP-5     02300003
023100            VALUE IS 32 .                                         02310003
023200     05 SQLITE-INDEX-CONSTRAINT-GT           PIC S9(9) COMP-5     02320003
023300            VALUE IS 4 .                                          02330003
023400     05 SQLITE-INDEX-CONSTRAINT-LE           PIC S9(9) COMP-5     02340003
023500            VALUE IS 8 .                                          02350003
023600     05 SQLITE-INDEX-CONSTRAINT-LT           PIC S9(9) COMP-5     02360003
023700            VALUE IS 16 .                                         02370003
023800     05 SQLITE-INDEX-CONSTRAINT-MATCH        PIC S9(9) COMP-5     02380003
023900            VALUE IS 64 .                                         02390003
024000     05 SQLITE-INSERT                        PIC S9(9) COMP-5     02400003
024100            VALUE IS 18 .                                         02410003
024200     05 SQLITE-INTEGER                       PIC S9(9) COMP-5     02420003
024300            VALUE IS 1 .                                          02430003
024400     05 SQLITE-INTERNAL                      PIC S9(9) COMP-5     02440003
024500            VALUE IS 2 .                                          02450003
024600     05 SQLITE-INTERRUPT                     PIC S9(9) COMP-5     02460003
024700            VALUE IS 9 .                                          02470003
024800     05 SQLITE-IOCAP-ATOMIC                  PIC S9(9) COMP-5     02480003
024900            VALUE IS 1 .                                          02490003
025000     05 SQLITE-IOCAP-ATOMIC1K                PIC S9(9) COMP-5     02500003
025100            VALUE IS 4 .                                          02510003
025200     05 SQLITE-IOCAP-ATOMIC16K               PIC S9(9) COMP-5     02520003
025300            VALUE IS 64 .                                         02530003
025400     05 SQLITE-IOCAP-ATOMIC2K                PIC S9(9) COMP-5     02540003
025500            VALUE IS 8 .                                          02550003
025600     05 SQLITE-IOCAP-ATOMIC32K               PIC S9(9) COMP-5     02560003
025700            VALUE IS 128 .                                        02570003
025800     05 SQLITE-IOCAP-ATOMIC4K                PIC S9(9) COMP-5     02580003
025900            VALUE IS 16 .                                         02590003
026000     05 SQLITE-IOCAP-ATOMIC512               PIC S9(9) COMP-5     02600003
026100            VALUE IS 2 .                                          02610003
026200     05 SQLITE-IOCAP-ATOMIC64K               PIC S9(9) COMP-5     02620003
026300            VALUE IS 256 .                                        02630003
026400     05 SQLITE-IOCAP-ATOMIC8K                PIC S9(9) COMP-5     02640003
026500            VALUE IS 32 .                                         02650003
026600     05 SQLITE-IOCAP-PWRSAFE-OVERWRITE       PIC S9(9) COMP-5     02660003
026700            VALUE IS 4096 .                                       02670003
026800     05 SQLITE-IOCAP-SAFE-APPEND             PIC S9(9) COMP-5     02680003
026900            VALUE IS 512 .                                        02690003
027000     05 SQLITE-IOCAP-SEQUENTIAL              PIC S9(9) COMP-5     02700003
027100            VALUE IS 1024 .                                       02710003
027200     05 SQLITE-IOCAP-UNDEL-WHEN-OPEN         PIC S9(9) COMP-5     02720003
027300            VALUE IS 2048 .                                       02730003
027400     05 SQLITE-IOERR                         PIC S9(9) COMP-5     02740003
027500            VALUE IS 10 .                                         02750003
027600     05 SQLITE-IOERR-ACCESS                  PIC S9(9) COMP-5     02760003
027700            VALUE IS 3338 .                                       02770003
027800     05 SQLITE-IOERR-BLOCKED                 PIC S9(9) COMP-5     02780003
027900            VALUE IS 2826 .                                       02790003
028000     05 SQLITE-IOERR-CHECKRESERVEDLOCK       PIC S9(9) COMP-5     02800003
028100            VALUE IS 3594 .                                       02810003
028200     05 SQLITE-IOERR-CLOSE                   PIC S9(9) COMP-5     02820003
028300            VALUE IS 4106 .                                       02830003
028400     05 SQLITE-IOERR-DELETE                  PIC S9(9) COMP-5     02840003
028500            VALUE IS 2570 .                                       02850003
028600     05 SQLITE-IOERR-DELETE-NOENT            PIC S9(9) COMP-5     02860003
028700            VALUE IS 5898 .                                       02870003
028800     05 SQLITE-IOERR-DIR-CLOSE               PIC S9(9) COMP-5     02880003
028900            VALUE IS 4362 .                                       02890003
029000     05 SQLITE-IOERR-DIR-FSYNC               PIC S9(9) COMP-5     02900003
029100            VALUE IS 1290 .                                       02910003
029200     05 SQLITE-IOERR-FSTAT                   PIC S9(9) COMP-5     02920003
029300            VALUE IS 1802 .                                       02930003
029400     05 SQLITE-IOERR-FSYNC                   PIC S9(9) COMP-5     02940003
029500            VALUE IS 1034 .                                       02950003
029600     05 SQLITE-IOERR-LOCK                    PIC S9(9) COMP-5     02960003
029700            VALUE IS 3850 .                                       02970003
029800     05 SQLITE-IOERR-MMAP                    PIC S9(9) COMP-5     02980003
029900            VALUE IS 6154 .                                       02990003
030000     05 SQLITE-IOERR-NOMEM                   PIC S9(9) COMP-5     03000003
030100            VALUE IS 3082 .                                       03010003
030200     05 SQLITE-IOERR-RDLOCK                  PIC S9(9) COMP-5     03020003
030300            VALUE IS 2314 .                                       03030003
030400     05 SQLITE-IOERR-READ                    PIC S9(9) COMP-5     03040003
030500            VALUE IS 266 .                                        03050003
030600     05 SQLITE-IOERR-SEEK                    PIC S9(9) COMP-5     03060003
030700            VALUE IS 5642 .                                       03070003
030800     05 SQLITE-IOERR-SHMLOCK                 PIC S9(9) COMP-5     03080003
030900            VALUE IS 5130 .                                       03090003
031000     05 SQLITE-IOERR-SHMMAP                  PIC S9(9) COMP-5     03100003
031100            VALUE IS 5386 .                                       03110003
031200     05 SQLITE-IOERR-SHMOPEN                 PIC S9(9) COMP-5     03120003
031300            VALUE IS 4618 .                                       03130003
031400     05 SQLITE-IOERR-SHMSIZE                 PIC S9(9) COMP-5     03140003
031500            VALUE IS 4874 .                                       03150003
031600     05 SQLITE-IOERR-SHORT-READ              PIC S9(9) COMP-5     03160003
031700            VALUE IS 522 .                                        03170003
031800     05 SQLITE-IOERR-TRUNCATE                PIC S9(9) COMP-5     03180003
031900            VALUE IS 1546 .                                       03190003
032000     05 SQLITE-IOERR-UNLOCK                  PIC S9(9) COMP-5     03200003
032100            VALUE IS 2058 .                                       03210003
032200     05 SQLITE-IOERR-WRITE                   PIC S9(9) COMP-5     03220003
032300            VALUE IS 778 .                                        03230003
032400     05 SQLITE-LAST-ERRNO                    PIC S9(9) COMP-5     03240003
032500            VALUE IS 4 .                                          03250003
032600     05 SQLITE-LIMIT-ATTACHED                PIC S9(9) COMP-5     03260003
032700            VALUE IS 7 .                                          03270003
032800     05 SQLITE-LIMIT-COLUMN                  PIC S9(9) COMP-5     03280003
032900            VALUE IS 2 .                                          03290003
033000     05 SQLITE-LIMIT-COMPOUND-SELECT         PIC S9(9) COMP-5     03300003
033100            VALUE IS 4 .                                          03310003
033200     05 SQLITE-LIMIT-EXPR-DEPTH              PIC S9(9) COMP-5     03320003
033300            VALUE IS 3 .                                          03330003
033400     05 SQLITE-LIMIT-FUNCTION-ARG            PIC S9(9) COMP-5     03340003
033500            VALUE IS 6 .                                          03350003
033600     05 SQLITE-LIMIT-LENGTH                  PIC S9(9) COMP-5     03360003
033700            VALUE IS 0 .                                          03370003
033800     05 SQLITE-LIMIT-LIKE-PATTERN-LEN        PIC S9(9) COMP-5     03380003
033900            VALUE IS 8 .                                          03390003
034000     05 SQLITE-LIMIT-SQL-LENGTH              PIC S9(9) COMP-5     03400003
034100            VALUE IS 1 .                                          03410003
034200     05 SQLITE-LIMIT-TRIGGER-DEPTH           PIC S9(9) COMP-5     03420003
034300            VALUE IS 10 .                                         03430003
034400     05 SQLITE-LIMIT-VARIABLE-NUMBER         PIC S9(9) COMP-5     03440003
034500            VALUE IS 9 .                                          03450003
034600     05 SQLITE-LIMIT-VDBE-OP                 PIC S9(9) COMP-5     03460003
034700            VALUE IS 5 .                                          03470003
034800     05 SQLITE-LOCK-EXCLUSIVE                PIC S9(9) COMP-5     03480003
034900            VALUE IS 4 .                                          03490003
035000     05 SQLITE-LOCK-NONE                     PIC S9(9) COMP-5     03500003
035100            VALUE IS 0 .                                          03510003
035200     05 SQLITE-LOCK-PENDING                  PIC S9(9) COMP-5     03520003
035300            VALUE IS 3 .                                          03530003
035400     05 SQLITE-LOCK-RESERVED                 PIC S9(9) COMP-5     03540003
035500            VALUE IS 2 .                                          03550003
035600     05 SQLITE-LOCK-SHARED                   PIC S9(9) COMP-5     03560003
035700            VALUE IS 1 .                                          03570003
035800     05 SQLITE-LOCKED                        PIC S9(9) COMP-5     03580003
035900            VALUE IS 6 .                                          03590003
036000     05 SQLITE-LOCKED-SHAREDCACHE            PIC S9(9) COMP-5     03600003
036100            VALUE IS 262 .                                        03610003
036200     05 SQLITE-MISMATCH                      PIC S9(9) COMP-5     03620003
036300            VALUE IS 20 .                                         03630003
036400     05 SQLITE-MISUSE                        PIC S9(9) COMP-5     03640003
036500            VALUE IS 21 .                                         03650003
036600     05 SQLITE-MUTEX-FAST                    PIC S9(9) COMP-5     03660003
036700            VALUE IS 0 .                                          03670003
036800     05 SQLITE-MUTEX-RECURSIVE               PIC S9(9) COMP-5     03680003
036900            VALUE IS 1 .                                          03690003
037000     05 SQLITE-MUTEX-STATIC-LRU              PIC S9(9) COMP-5     03700003
037100            VALUE IS 6 .                                          03710003
037200     05 SQLITE-MUTEX-STATIC-LRU2             PIC S9(9) COMP-5     03720003
037300            VALUE IS 7 .                                          03730003
037400     05 SQLITE-MUTEX-STATIC-MASTER           PIC S9(9) COMP-5     03740003
037500            VALUE IS 2 .                                          03750003
037600     05 SQLITE-MUTEX-STATIC-MEM              PIC S9(9) COMP-5     03760003
037700            VALUE IS 3 .                                          03770003
037800     05 SQLITE-MUTEX-STATIC-MEM2             PIC S9(9) COMP-5     03780003
037900            VALUE IS 4 .                                          03790003
038000     05 SQLITE-MUTEX-STATIC-OPEN             PIC S9(9) COMP-5     03800003
038100            VALUE IS 4 .                                          03810003
038200     05 SQLITE-MUTEX-STATIC-PMEM             PIC S9(9) COMP-5     03820003
038300            VALUE IS 7 .                                          03830003
038400     05 SQLITE-MUTEX-STATIC-PRNG             PIC S9(9) COMP-5     03840003
038500            VALUE IS 5 .                                          03850003
038600     05 SQLITE-NOLFS                         PIC S9(9) COMP-5     03860003
038700            VALUE IS 22 .                                         03870003
038800     05 SQLITE-NOMEM                         PIC S9(9) COMP-5     03880003
038900            VALUE IS 7 .                                          03890003
039000     05 SQLITE-NOTADB                        PIC S9(9) COMP-5     03900003
039100            VALUE IS 26 .                                         03910003
039200     05 SQLITE-NOTFOUND                      PIC S9(9) COMP-5     03920003
039300            VALUE IS 12 .                                         03930003
039400     05 SQLITE-NOTICE                        PIC S9(9) COMP-5     03940003
039500            VALUE IS 27 .                                         03950003
039600     05 SQLITE-NOTICE-RECOVER-ROLLBACK       PIC S9(9) COMP-5     03960003
039700            VALUE IS 539 .                                        03970003
039800     05 SQLITE-NOTICE-RECOVER-WAL            PIC S9(9) COMP-5     03980003
039900            VALUE IS 283 .                                        03990003
040000     05 SQLITE-NULL                          PIC S9(9) COMP-5     04000003
040100            VALUE IS 5 .                                          04010003
040200     05 SQLITE-OK                            PIC S9(9) COMP-5     04020003
040300            VALUE IS 0 .                                          04030003
040400     05 SQLITE-OPEN-AUTOPROXY                PIC S9(9) COMP-5     04040003
040500            VALUE IS 32 .                                         04050003
040600     05 SQLITE-OPEN-CREATE                   PIC S9(9) COMP-5     04060003
040700            VALUE IS 4 .                                          04070003
040800     05 SQLITE-OPEN-DELETEONCLOSE            PIC S9(9) COMP-5     04080003
040900            VALUE IS 8 .                                          04090003
041000     05 SQLITE-OPEN-EXCLUSIVE                PIC S9(9) COMP-5     04100003
041100            VALUE IS 16 .                                         04110003
041200     05 SQLITE-OPEN-FULLMUTEX                PIC S9(9) COMP-5     04120003
041300            VALUE IS 65536 .                                      04130003
041400     05 SQLITE-OPEN-MAIN-DB                  PIC S9(9) COMP-5     04140003
041500            VALUE IS 256 .                                        04150003
041600     05 SQLITE-OPEN-MAIN-JOURNAL             PIC S9(9) COMP-5     04160003
041700            VALUE IS 2048 .                                       04170003
041800     05 SQLITE-OPEN-MASTER-JOURNAL           PIC S9(9) COMP-5     04180003
041900            VALUE IS 16384 .                                      04190003
042000     05 SQLITE-OPEN-MEMORY                   PIC S9(9) COMP-5     04200003
042100            VALUE IS 128 .                                        04210003
042200     05 SQLITE-OPEN-NOMUTEX                  PIC S9(9) COMP-5     04220003
042300            VALUE IS 32768 .                                      04230003
042400     05 SQLITE-OPEN-PRIVATECACHE             PIC S9(9) COMP-5     04240003
042500            VALUE IS 262144 .                                     04250003
042600     05 SQLITE-OPEN-READONLY                 PIC S9(9) COMP-5     04260003
042700            VALUE IS 1 .                                          04270003
042800     05 SQLITE-OPEN-READWRITE                PIC S9(9) COMP-5     04280003
042900            VALUE IS 2 .                                          04290003
043000     05 SQLITE-OPEN-SHAREDCACHE              PIC S9(9) COMP-5     04300003
043100            VALUE IS 131072 .                                     04310003
043200     05 SQLITE-OPEN-SUBJOURNAL               PIC S9(9) COMP-5     04320003
043300            VALUE IS 8192 .                                       04330003
043400     05 SQLITE-OPEN-TEMP-DB                  PIC S9(9) COMP-5     04340003
043500            VALUE IS 512 .                                        04350003
043600     05 SQLITE-OPEN-TEMP-JOURNAL             PIC S9(9) COMP-5     04360003
043700            VALUE IS 4096 .                                       04370003
043800     05 SQLITE-OPEN-TRANSIENT-DB             PIC S9(9) COMP-5     04380003
043900            VALUE IS 1024 .                                       04390003
044000     05 SQLITE-OPEN-URI                      PIC S9(9) COMP-5     04400003
044100            VALUE IS 64 .                                         04410003
044200     05 SQLITE-OPEN-WAL                      PIC S9(9) COMP-5     04420003
044300            VALUE IS 524288 .                                     04430003
044400     05 SQLITE-PERM                          PIC S9(9) COMP-5     04440003
044500            VALUE IS 3 .                                          04450003
044600     05 SQLITE-PRAGMA                        PIC S9(9) COMP-5     04460003
044700            VALUE IS 19 .                                         04470003
044800     05 SQLITE-PROTOCOL                      PIC S9(9) COMP-5     04480003
044900            VALUE IS 15 .                                         04490003
045000     05 SQLITE-RANGE                         PIC S9(9) COMP-5     04500003
045100            VALUE IS 25 .                                         04510003
045200     05 SQLITE-READ                          PIC S9(9) COMP-5     04520003
045300            VALUE IS 20 .                                         04530003
045400     05 SQLITE-READONLY                      PIC S9(9) COMP-5     04540003
045500            VALUE IS 8 .                                          04550003
045600     05 SQLITE-READONLY-CANTLOCK             PIC S9(9) COMP-5     04560003
045700            VALUE IS 520 .                                        04570003
045800     05 SQLITE-READONLY-RECOVERY             PIC S9(9) COMP-5     04580003
045900            VALUE IS 264 .                                        04590003
046000     05 SQLITE-READONLY-ROLLBACK             PIC S9(9) COMP-5     04600003
046100            VALUE IS 776 .                                        04610003
046200     05 SQLITE-REINDEX                       PIC S9(9) COMP-5     04620003
046300            VALUE IS 27 .                                         04630003
046400     05 SQLITE-REPLACE                       PIC S9(9) COMP-5     04640003
046500            VALUE IS 5 .                                          04650003
046600     05 SQLITE-ROLLBACK                      PIC S9(9) COMP-5     04660003
046700            VALUE IS 1 .                                          04670003
046800     05 SQLITE-ROW                           PIC S9(9) COMP-5     04680003
046900            VALUE IS 100 .                                        04690003
047000     05 SQLITE-SAVEPOINT                     PIC S9(9) COMP-5     04700003
047100            VALUE IS 32 .                                         04710003
047200     05 SQLITE-SCHEMA                        PIC S9(9) COMP-5     04720003
047300            VALUE IS 17 .                                         04730003
047400     05 SQLITE-SELECT                        PIC S9(9) COMP-5     04740003
047500            VALUE IS 21 .                                         04750003
047600     05 SQLITE-SET-LOCKPROXYFILE             PIC S9(9) COMP-5     04760003
047700            VALUE IS 3 .                                          04770003
047800     05 SQLITE-SHM-EXCLUSIVE                 PIC S9(9) COMP-5     04780003
047900            VALUE IS 8 .                                          04790003
048000     05 SQLITE-SHM-LOCK                      PIC S9(9) COMP-5     04800003
048100            VALUE IS 2 .                                          04810003
048200     05 SQLITE-SHM-NLOCK                     PIC S9(9) COMP-5     04820003
048300            VALUE IS 8 .                                          04830003
048400     05 SQLITE-SHM-SHARED                    PIC S9(9) COMP-5     04840003
048500            VALUE IS 4 .                                          04850003
048600     05 SQLITE-SHM-UNLOCK                    PIC S9(9) COMP-5     04860003
048700            VALUE IS 1 .                                          04870003
048800     05 SQLITE-STATIC                        PIC S9(9) COMP-5     04880003
048900            VALUE IS 0 .                                          04890003
049000     05 SQLITE-STATUS-MALLOC-COUNT           PIC S9(9) COMP-5     04900003
049100            VALUE IS 9 .                                          04910003
049200     05 SQLITE-STATUS-MALLOC-SIZE            PIC S9(9) COMP-5     04920003
049300            VALUE IS 5 .                                          04930003
049400     05 SQLITE-STATUS-MEMORY-USED            PIC S9(9) COMP-5     04940003
049500            VALUE IS 0 .                                          04950003
049600     05 SQLITE-STATUS-PGCACHE-OVERFLOW       PIC S9(9) COMP-5     04960003
049700            VALUE IS 2 .                                          04970003
049800     05 SQLITE-STATUS-PGCACHE-SIZE           PIC S9(9) COMP-5     04980003
049900            VALUE IS 7 .                                          04990003
050000     05 SQLITE-STATUS-PGCACHE-USED           PIC S9(9) COMP-5     05000003
050100            VALUE IS 1 .                                          05010003
050200     05 SQLITE-STATUS-PARSER-STACK           PIC S9(9) COMP-5     05020003
050300            VALUE IS 6 .                                          05030003
050400     05 SQLITE-STATUS-SCRATCH-OVERFLOW       PIC S9(9) COMP-5     05040003
050500            VALUE IS 4 .                                          05050003
050600     05 SQLITE-STATUS-SCRATCH-SIZE           PIC S9(9) COMP-5     05060003
050700            VALUE IS 8 .                                          05070003
050800     05 SQLITE-STATUS-SCRATCH-USED           PIC S9(9) COMP-5     05080003
050900            VALUE IS 3 .                                          05090003
051000     05 SQLITE-STMTSTATUS-AUTOINDEX          PIC S9(9) COMP-5     05100003
051100            VALUE IS 3 .                                          05110003
051200     05 SQLITE-STMTSTATUS-FULLSCAN-STP       PIC S9(9) COMP-5     05120003
051300            VALUE IS 1 .                                          05130003
051400     05 SQLITE-STMTSTATUS-SORT               PIC S9(9) COMP-5     05140003
051500            VALUE IS 2 .                                          05150003
051600     05 SQLITE-SYNC-DATAONLY                 PIC S9(9) COMP-5     05160003
051700            VALUE IS 16 .                                         05170003
051800     05 SQLITE-SYNC-FULL                     PIC S9(9) COMP-5     05180003
051900            VALUE IS 3 .                                          05190003
052000     05 SQLITE-SYNC-NORMAL                   PIC S9(9) COMP-5     05200003
052100            VALUE IS 2 .                                          05210003
052200     05 SQLITE-TESTCTRL-ALWAYS               PIC S9(9) COMP-5     05220003
052300            VALUE IS 13 .                                         05230003
052400     05 SQLITE-TESTCTRL-ASSERT               PIC S9(9) COMP-5     05240003
052500            VALUE IS 12 .                                         05250003
052600     05 SQLITE-TESTCTRL-B9-MALLOC-HKS        PIC S9(9) COMP-5     05260003
052700            VALUE IS 10 .                                         05270003
052800     05 SQLITE-TESTCTRL-BITVEC-TEST          PIC S9(9) COMP-5     05280003
052900            VALUE IS 8 .                                          05290003
053000     05 SQLITE-TESTCTRL-EXPLAIN-STMT         PIC S9(9) COMP-5     05300003
053100            VALUE IS 19 .                                         05310003
053200     05 SQLITE-TESTCTRL-FAULT-INSTALL        PIC S9(9) COMP-5     05320003
053300            VALUE IS 9 .                                          05330003
053400     05 SQLITE-TESTCTRL-FIRST                PIC S9(9) COMP-5     05340003
053500            VALUE IS 5 .                                          05350003
053600     05 SQLITE-TESTCTRL-ISKEYWORD            PIC S9(9) COMP-5     05360003
053700            VALUE IS 16 .                                         05370003
053800     05 SQLITE-TESTCTRL-LAST                 PIC S9(9) COMP-5     05380003
053900            VALUE IS 19 .                                         05390003
054000     05 SQLITE-TESTCTRL-LOCALTIME-FLT        PIC S9(9) COMP-5     05400003
054100            VALUE IS 18 .                                         05410003
054200     05 SQLITE-TESTCTRL-OPTIMIZATIONS        PIC S9(9) COMP-5     05420003
054300            VALUE IS 15 .                                         05430003
054400     05 SQLITE-TESTCTRL-PENDING-BYTE         PIC S9(9) COMP-5     05440003
054500            VALUE IS 11 .                                         05450003
054600     05 SQLITE-TESTCTRL-PRNG-RESET           PIC S9(9) COMP-5     05460003
054700            VALUE IS 7 .                                          05470003
054800     05 SQLITE-TESTCTRL-PRNG-RESTORE         PIC S9(9) COMP-5     05480003
054900            VALUE IS 6 .                                          05490003
055000     05 SQLITE-TESTCTRL-PRNG-SAVE            PIC S9(9) COMP-5     05500003
055100            VALUE IS 5 .                                          05510003
055200     05 SQLITE-TESTCTRL-RESERVE              PIC S9(9) COMP-5     05520003
055300            VALUE IS 14 .                                         05530003
055400     05 SQLITE-TESTCTRL-SCRATCHMALLOC        PIC S9(9) COMP-5     05540003
055500            VALUE IS 17 .                                         05550003
055600     05 SQLITE-TEXT                          PIC S9(9) COMP-5     05560003
055700            VALUE IS 3 .                                          05570003
055800     05 SQLITE-TOOBIG                        PIC S9(9) COMP-5     05580003
055900            VALUE IS 18 .                                         05590003
056000     05 SQLITE-TRANSACTION                   PIC S9(9) COMP-5     05600003
056100            VALUE IS 22 .                                         05610003
056200     05 SQLITE-TRANSIENT                     PIC S9(9) COMP-5     05620003
056300            VALUE IS -1 .                                         05630003
056400     05 SQLITE-UPDATE                        PIC S9(9) COMP-5     05640003
056500            VALUE IS 23 .                                         05650003
056600     05 SQLITE-UTF16                         PIC S9(9) COMP-5     05660003
056700            VALUE IS 4 .                                          05670003
056800     05 SQLITE-UTF16-ALIGNED                 PIC S9(9) COMP-5     05680003
056900            VALUE IS 8 .                                          05690003
057000     05 SQLITE-UTF16BE                       PIC S9(9) COMP-5     05700003
057100            VALUE IS 3 .                                          05710003
057200     05 SQLITE-UTF16LE                       PIC S9(9) COMP-5     05720003
057300            VALUE IS 2 .                                          05730003
057400     05 SQLITE-UTF8                          PIC S9(9) COMP-5     05740003
057500            VALUE IS 1 .                                          05750003
057600     05 SQLITE-VERSION                       PIC X(6)             05760003
057700            VALUE IS "3.7.17" .                                   05770003
057800     05 SQLITE-VERSION-NUMBER                PIC S9(9) COMP-5     05780003
057900            VALUE IS 3007017 .                                    05790003
058000     05 SQLITE-VTAB-CONSTRAINT-SUPPORT       PIC S9(9) COMP-5     05800003
058100            VALUE IS 1 .                                          05810003
058200     05 SQLITE-WARNING                       PIC S9(9) COMP-5     05820003
058300            VALUE IS 28 .                                         05830003
058400 01  SQLITE3-FUNCTION-NAMES.                                      05840003
058500*                                                                 05850008
058600     05 SQLITE3A                        PIC X(8)                  05860010
058700            VALUE IS 'SQLITE3A'.                                  05870010
058800*                                                                 05880008
058900     05 SQLITE3-AGGREGATE-CONTEXT       PIC X(26)                 05890002
059000            VALUE IS Z'sqlite3_aggregate_context'.                05900000
059100* This function returns a pointer which in COBOL                  05910009
059200* would be USAGE POINTER. You should assign                       05920009
059300* the result to the variable using the RETURNING clause           05930009
059400* CALL SQLITE3 USING BY REFERENCE SQLITE3-AGGREGATE-CONTEXT       05940009
059500*      <other call parameters as documented>                      05950009
059600*      RETURNING pointer-result-variable                          05960009
059700* END-CALL                                                        05970009
059800*                                                                 05980008
059900     05 SQLITE3-AGGREGATE-COUNT         PIC X(24)                 05990002
060000            VALUE IS Z'sqlite3_aggregate_count'.                  06000000
060100* This function returns an integer which in COBOL                 06010009
060200* would be PIC S9(9) COMP-5. You should assign                    06020009
060300* the result to the variable using the RETURNING clause           06030009
060400* CALL SQLITE3 USING BY REFERENCE SQLITE3-AGGREGATE-COUNT         06040009
060500*      <other call parameters as documented>                      06050009
060600*      RETURNING integer-result-variable                          06060009
060700* END-CALL                                                        06070009
060800*                                                                 06080008
060900     05 SQLITE3-AUTO-EXTENSION          PIC X(23)                 06090002
061000            VALUE IS Z'sqlite3_auto_extension'.                   06100000
061100* This function returns an integer which in COBOL                 06110009
061200* would be PIC S9(9) COMP-5. You should assign                    06120009
061300* the result to the variable using the RETURNING clause           06130009
061400* CALL SQLITE3 USING BY REFERENCE SQLITE3-AUTO-EXTENSION          06140009
061500*      <other call parameters as documented>                      06150009
061600*      RETURNING integer-result-variable                          06160009
061700* END-CALL                                                        06170009
061800*                                                                 06180008
061900     05 SQLITE3-BACKUP-FINISH           PIC X(22)                 06190002
062000            VALUE IS Z'sqlite3_backup_finish'.                    06200000
062100* This function returns an integer which in COBOL                 06210009
062200* would be PIC S9(9) COMP-5. You should assign                    06220009
062300* the result to the variable using the RETURNING clause           06230009
062400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BACKUP-FINISH           06240009
062500*      <other call parameters as documented>                      06250009
062600*      RETURNING integer-result-variable                          06260009
062700* END-CALL                                                        06270009
062800*                                                                 06280008
062900     05 SQLITE3-BACKUP-INIT             PIC X(20)                 06290002
063000            VALUE IS Z'sqlite3_backup_init'.                      06300000
063100* This function returns a pointer which in COBOL                  06310009
063200* would be USAGE POINTER. You should assign                       06320009
063300* the result to the variable using the RETURNING clause           06330009
063400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BACK-INIT               06340009
063500*      <other call parameters as documented>                      06350009
063600*      RETURNING pointer-result-variable                          06360009
063700* END-CALL                                                        06370009
063800*                                                                 06380008
063900     05 SQLITE3-BACKUP-PAGECOUNT        PIC X(25)                 06390002
064000            VALUE IS Z'sqlite3_backup_pagecount'.                 06400000
064100* This function returns an integer which in COBOL                 06410009
064200* would be PIC S9(9) COMP-5. You should assign                    06420009
064300* the result to the variable using the RETURNING clause           06430009
064400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BACKUP-PAGECOUNT        06440009
064500*      <other call parameters as documented>                      06450009
064600*      RETURNING integer-result-variable                          06460009
064700* END-CALL                                                        06470009
064800*                                                                 06480008
064900     05 SQLITE3-BACKUP-REMAINING        PIC X(25)                 06490002
065000            VALUE IS Z'sqlite3_backup_remaining'.                 06500000
065100* This function returns an integer which in COBOL                 06510009
065200* would be PIC S9(9) COMP-5. You should assign                    06520009
065300* the result to the variable using the RETURNING clause           06530009
065400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BACKUP-REMAINING        06540009
065500*      <other call parameters as documented>                      06550009
065600*      RETURNING integer-result-variable                          06560009
065700* END-CALL                                                        06570009
065800*                                                                 06580008
065900     05 SQLITE3-BACKUP-STEP             PIC X(20)                 06590002
066000            VALUE IS Z'sqlite3_backup_step'.                      06600000
066100* This function returns an integer which in COBOL                 06610009
066200* would be PIC S9(9) COMP-5. You should assign                    06620009
066300* the result to the variable using the RETURNING clause           06630009
066400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BACKUP-STEP             06640009
066500*      <other call parameters as documented>                      06650009
066600*      RETURNING integer-result-variable                          06660009
066700* END-CALL                                                        06670009
066800*                                                                 06680008
066900     05 SQLITE3-BIND-BLOB               PIC X(18)                 06690002
067000            VALUE IS Z'sqlite3_bind_blob'.                        06700000
067100* This function returns an integer which in COBOL                 06710009
067200* would be PIC S9(9) COMP-5. You should assign                    06720009
067300* the result to the variable using the RETURNING clause           06730009
067400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-BLOB               06740009
067500*      <other call parameters as documented>                      06750009
067600*      RETURNING integer-result-variable                          06760009
067700* END-CALL                                                        06770009
067800*                                                                 06780008
067900     05 SQLITE3-BIND-DOUBLE             PIC X(20)                 06790002
068000            VALUE IS Z'sqlite3_bind_double'.                      06800000
068100* This function returns an integer which in COBOL                 06810009
068200* would be PIC S9(9) COMP-5. You should assign                    06820009
068300* the result to the variable using the RETURNING clause           06830009
068400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-DOUBLE             06840009
068500*      <other call parameters as documented>                      06850009
068600*      RETURNING integer-result-variable                          06860009
068700* END-CALL                                                        06870009
068800*                                                                 06880008
068900     05 SQLITE3-BIND-INT                PIC X(17)                 06890002
069000            VALUE IS Z'sqlite3_bind_int'.                         06900000
069100* This function returns an integer which in COBOL                 06910009
069200* would be PIC S9(9) COMP-5. You should assign                    06920009
069300* the result to the variable using the RETURNING clause           06930009
069400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-INT                06940009
069500*      <other call parameters as documented>                      06950009
069600*      RETURNING integer-result-variable                          06960009
069700* END-CALL                                                        06970009
069800*                                                                 06980008
069900     05 SQLITE3-BIND-INT64              PIC X(19)                 06990002
070000            VALUE IS Z'sqlite3_bind_int64'.                       07000000
070100* This function returns an integer which in COBOL                 07010009
070200* would be PIC S9(9) COMP-5. You should assign                    07020009
070300* the result to the variable using the RETURNING clause           07030009
070400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-INT64              07040009
070500*      <other call parameters as documented>                      07050009
070600*      RETURNING integer-result-variable                          07060009
070700* END-CALL                                                        07070009
070800*                                                                 07080008
070900     05 SQLITE3-BIND-NULL               PIC X(18)                 07090002
071000            VALUE IS Z'sqlite3_bind_null'.                        07100000
071100* This function returns an integer which in COBOL                 07110009
071200* would be PIC S9(9) COMP-5. You should assign                    07120009
071300* the result to the variable using the RETURNING clause           07130009
071400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-NULL               07140009
071500*      <other call parameters as documented>                      07150009
071600*      RETURNING integer-result-variable                          07160009
071700* END-CALL                                                        07170009
071800*                                                                 07180008
071900     05 SQLITE3-BIND-PARAMETER-COUNT    PIC X(29)                 07190002
072000            VALUE IS Z'sqlite3_bind_parameter_count'.             07200000
072100* This function returns an integer which in COBOL                 07210009
072200* would be PIC S9(9) COMP-5. You should assign                    07220009
072300* the result to the variable using the RETURNING clause           07230009
072400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-PARAMETER-COUNT    07240009
072500*      <other call parameters as documented>                      07250009
072600*      RETURNING integer-result-variable                          07260009
072700* END-CALL                                                        07270009
072800*                                                                 07280008
072900     05 SQLITE3-BIND-PARAMETER-INDEX    PIC X(29)                 07290002
073000            VALUE IS Z'sqlite3_bind_parameter_index'.             07300000
073100* This function returns an integer which in COBOL                 07310009
073200* would be PIC S9(9) COMP-5. You should assign                    07320009
073300* the result to the variable using the RETURNING clause           07330009
073400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-PARAMETER-INDEX    07340009
073500*      <other call parameters as documented>                      07350009
073600*      RETURNING integer-result-variable                          07360009
073700* END-CALL                                                        07370009
073800*                                                                 07380008
073900     05 SQLITE3-BIND-PARAMETER-NAME     PIC X(28)                 07390002
074000            VALUE IS Z'sqlite3_bind_parameter_name'.              07400000
074100* This function returns a pointer which in COBOL                  07410009
074200* would be USAGE POINTER. You should assign                       07420009
074300* the result to the variable using the RETURNING clause           07430009
074400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-PARAMETER-NAME     07440009
074500*      <other call parameters as documented>                      07450009
074600*      RETURNING pointer-result-variable                          07460009
074700* END-CALL                                                        07470009
074800*                                                                 07480008
074900     05 SQLITE3-BIND-TEXT               PIC X(18)                 07490002
075000            VALUE IS Z'sqlite3_bind_text'.                        07500000
075100* This function returns an integer which in COBOL                 07510009
075200* would be PIC S9(9) COMP-5. You should assign                    07520009
075300* the result to the variable using the RETURNING clause           07530009
075400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-TEXT               07540009
075500*      <other call parameters as documented>                      07550009
075600*      RETURNING integer-result-variable                          07560009
075700* END-CALL                                                        07570009
075800*                                                                 07580008
075900     05 SQLITE3-BIND-TEXT16             PIC X(20)                 07590002
076000            VALUE IS Z'sqlite3_bind_text16'.                      07600000
076100* This function returns an integer which in COBOL                 07610009
076200* would be PIC S9(9) COMP-5. You should assign                    07620009
076300* the result to the variable using the RETURNING clause           07630009
076400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-TEXT16             07640009
076500*      <other call parameters as documented>                      07650009
076600*      RETURNING integer-result-variable                          07660009
076700* END-CALL                                                        07670009
076800*                                                                 07680008
076900     05 SQLITE3-BIND-VALUE              PIC X(19)                 07690002
077000            VALUE IS Z'sqlite3_bind_value'.                       07700000
077100* This function returns an integer which in COBOL                 07710009
077200* would be PIC S9(9) COMP-5. You should assign                    07720009
077300* the result to the variable using the RETURNING clause           07730009
077400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-VALUE              07740009
077500*      <other call parameters as documented>                      07750009
077600*      RETURNING integer-result-variable                          07760009
077700* END-CALL                                                        07770009
077800*                                                                 07780008
077900     05 SQLITE3-BIND-ZEROBLOB           PIC X(22)                 07790002
078000            VALUE IS Z'sqlite3_bind_zeroblob'.                    07800000
078100* This function returns an integer which in COBOL                 07810009
078200* would be PIC S9(9) COMP-5. You should assign                    07820009
078300* the result to the variable using the RETURNING clause           07830009
078400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BIND-ZEROBLOB           07840009
078500*      <other call parameters as documented>                      07850009
078600*      RETURNING integer-result-variable                          07860009
078700* END-CALL                                                        07870009
078800*                                                                 07880008
078900     05 SQLITE3-BLOB-BYTES              PIC X(19)                 07890002
079000            VALUE IS Z'sqlite3_blob_bytes'.                       07900000
079100* This function returns an integer which in COBOL                 07910009
079200* would be PIC S9(9) COMP-5. You should assign                    07920009
079300* the result to the variable using the RETURNING clause           07930009
079400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BLOB-BYTES              07940009
079500*      <other call parameters as documented>                      07950009
079600*      RETURNING integer-result-variable                          07960009
079700* END-CALL                                                        07970009
079800*                                                                 07980008
079900     05 SQLITE3-BLOB-CLOSE              PIC X(19)                 07990002
080000            VALUE IS Z'sqlite3_blob_close'.                       08000000
080100* This function returns an integer which in COBOL                 08010009
080200* would be PIC S9(9) COMP-5. You should assign                    08020009
080300* the result to the variable using the RETURNING clause           08030009
080400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BLOB-CLOSE              08040009
080500*      <other call parameters as documented>                      08050009
080600*      RETURNING integer-result-variable                          08060009
080700* END-CALL                                                        08070009
080800*                                                                 08080008
080900     05 SQLITE3-BLOB-OPEN               PIC X(18)                 08090002
081000            VALUE IS Z'sqlite3_blob_open'.                        08100000
081100* This function returns an integer which in COBOL                 08110009
081200* would be PIC S9(9) COMP-5. You should assign                    08120009
081300* the result to the variable using the RETURNING clause           08130009
081400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BLOB-OPEN               08140009
081500*      <other call parameters as documented>                      08150009
081600*      RETURNING integer-result-variable                          08160009
081700* END-CALL                                                        08170009
081800*                                                                 08180008
081900     05 SQLITE3-BLOB-READ               PIC X(18)                 08190002
082000            VALUE IS Z'sqlite3_blob_read'.                        08200000
082100* This function returns an integer which in COBOL                 08210009
082200* would be PIC S9(9) COMP-5. You should assign                    08220009
082300* the result to the variable using the RETURNING clause           08230009
082400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BLOB-READ               08240009
082500*      <other call parameters as documented>                      08250009
082600*      RETURNING integer-result-variable                          08260009
082700* END-CALL                                                        08270009
082800*                                                                 08280008
082900     05 SQLITE3-BLOB-REOPEN             PIC X(20)                 08290002
083000            VALUE IS Z'sqlite3_blob_reopen'.                      08300000
083100* This function returns an integer which in COBOL                 08310009
083200* would be PIC S9(9) COMP-5. You should assign                    08320009
083300* the result to the variable using the RETURNING clause           08330009
083400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BLOB-REOPEN             08340009
083500*      <other call parameters as documented>                      08350009
083600*      RETURNING integer-result-variable                          08360009
083700* END-CALL                                                        08370009
083800*                                                                 08380008
083900     05 SQLITE3-BLOB-WRITE              PIC X(19)                 08390002
084000            VALUE IS Z'sqlite3_blob_write'.                       08400000
084100* This function returns an integer which in COBOL                 08410009
084200* would be PIC S9(9) COMP-5. You should assign                    08420009
084300* the result to the variable using the RETURNING clause           08430009
084400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BLOB-WRITE              08440009
084500*      <other call parameters as documented>                      08450009
084600*      RETURNING integer-result-variable                          08460009
084700* END-CALL                                                        08470009
084800*                                                                 08480008
084900     05 SQLITE3-BUSY-HANDLER            PIC X(21)                 08490002
085000            VALUE IS Z'sqlite3_busy_handler'.                     08500000
085100* This function returns an integer which in COBOL                 08510009
085200* would be PIC S9(9) COMP-5. You should assign                    08520009
085300* the result to the variable using the RETURNING clause           08530009
085400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BUSY-HANDLER            08540009
085500*      <other call parameters as documented>                      08550009
085600*      RETURNING integer-result-variable                          08560009
085700* END-CALL                                                        08570009
085800*                                                                 08580008
085900     05 SQLITE3-BUSY-TIMEOUT            PIC X(21)                 08590002
086000            VALUE IS Z'sqlite3_busy_timeout'.                     08600000
086100* This function returns an integer which in COBOL                 08610009
086200* would be PIC S9(9) COMP-5. You should assign                    08620009
086300* the result to the variable using the RETURNING clause           08630009
086400* CALL SQLITE3 USING BY REFERENCE SQLITE3-BUSY-TIMEOUT            08640009
086500*      <other call parameters as documented>                      08650009
086600*      RETURNING integer-result-variable                          08660009
086700* END-CALL                                                        08670009
086800*                                                                 08680008
086900     05 SQLITE3-CANCEL-AUTO-EXTENSION   PIC X(30)                 08690006
087000            VALUE IS Z'sqlite3_cancel_auto_extension'.            08700006
087100* This function returns an integer which in COBOL                 08710009
087200* would be PIC S9(9) COMP-5. You should assign                    08720009
087300* the result to the variable using the RETURNING clause           08730009
087400* CALL SQLITE3 USING BY REFERENCE SQLITE3-CANCEL-AUTO-EXTENSION   08740009
087500*      <other call parameters as documented>                      08750009
087600*      RETURNING integer-result-variable                          08760009
087700* END-CALL                                                        08770009
087800*                                                                 08780008
087900     05 SQLITE3-CHANGES                 PIC X(16)                 08790002
088000            VALUE IS Z'sqlite3_changes'.                          08800000
088100* This function returns an integer which in COBOL                 08810009
088200* would be PIC S9(9) COMP-5. You should assign                    08820009
088300* the result to the variable using the RETURNING clause           08830009
088400* CALL SQLITE3 USING BY REFERENCE SQLITE3-CHANGES                 08840009
088500*      <other call parameters as documented>                      08850009
088600*      RETURNING integer-result-variable                          08860009
088700* END-CALL                                                        08870009
088800*                                                                 08880008
088900     05 SQLITE3-CLEAR-BINDINGS          PIC X(23)                 08890002
089000            VALUE IS Z'sqlite3_clear_bindings'.                   08900000
089100* This function returns an integer which in COBOL                 08910009
089200* would be PIC S9(9) COMP-5. You should assign                    08920009
089300* the result to the variable using the RETURNING clause           08930009
089400* CALL SQLITE3 USING BY REFERENCE SQLITE3-CLEAR-BINDINGS          08940009
089500*      <other call parameters as documented>                      08950009
089600*      RETURNING integer-result-variable                          08960009
089700* END-CALL                                                        08970009
089800*                                                                 08980008
089900     05 SQLITE3-CLOSE                   PIC X(14)                 08990002
090000            VALUE IS Z'sqlite3_close'.                            09000000
090100* This function returns an integer which in COBOL                 09010009
090200* would be PIC S9(9) COMP-5. You should assign                    09020009
090300* the result to the variable using the RETURNING clause           09030009
090400* CALL SQLITE3 USING BY REFERENCE SQLITE3-CLOSE                   09040009
090500*      <other call parameters as documented>                      09050009
090600*      RETURNING integer-result-variable                          09060009
090700* END-CALL                                                        09070009
090800*                                                                 09080008
090900     05 SQLITE3-CLOSE-V2                PIC X(17)                 09090002
091000            VALUE IS Z'sqlite3_close_v2'.                         09100000
091100* This function returns an integer which in COBOL                 09110009
091200* would be PIC S9(9) COMP-5. You should assign                    09120009
091300* the result to the variable using the RETURNING clause           09130009
091400* CALL SQLITE3 USING BY REFERENCE SQLITE3-CLOSE-VW                09140009
091500*      <other call parameters as documented>                      09150009
091600*      RETURNING integer-result-variable                          09160009
091700* END-CALL                                                        09170009
091800*                                                                 09180008
091900     05 SQLITE3-COLLATION-NEEDED        PIC X(25)                 09190002
092000            VALUE IS Z'sqlite3_collation_needed'.                 09200000
092100* This function returns an integer which in COBOL                 09210009
092200* would be PIC S9(9) COMP-5. You should assign                    09220009
092300* the result to the variable using the RETURNING clause           09230009
092400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLLATION-NEEDED        09240009
092500*      <other call parameters as documented>                      09250009
092600*      RETURNING integer-result-variable                          09260009
092700* END-CALL                                                        09270009
092800*                                                                 09280008
092900     05 SQLITE3-COLLATION-NEEDED16      PIC X(27)                 09290002
093000            VALUE IS Z'sqlite3_collation_needed16'.               09300000
093100* This function returns an integer which in COBOL                 09310009
093200* would be PIC S9(9) COMP-5. You should assign                    09320009
093300* the result to the variable using the RETURNING clause           09330009
093400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLLATION-NEEDED16      09340009
093500*      <other call parameters as documented>                      09350009
093600*      RETURNING integer-result-variable                          09360009
093700* END-CALL                                                        09370009
093800*                                                                 09380008
093900     05 SQLITE3-COLUMN-BLOB             PIC X(20)                 09390002
094000            VALUE IS Z'sqlite3_column_blob'.                      09400000
094100* This function returns a pointer which in COBOL                  09410009
094200* would be USAGE POINTER. You should assign                       09420009
094300* the result to the variable using the RETURNING clause           09430009
094400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-BLOB             09440009
094500*      <other call parameters as documented>                      09450009
094600*      RETURNING pointer-result-variable                          09460009
094700* END-CALL                                                        09470009
094800*                                                                 09480008
094900     05 SQLITE3-COLUMN-BYTES            PIC X(21)                 09490002
095000            VALUE IS Z'sqlite3_column_bytes'.                     09500000
095100* This function returns an integer which in COBOL                 09510009
095200* would be PIC S9(9) COMP-5. You should assign                    09520009
095300* the result to the variable using the RETURNING clause           09530009
095400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-BYTES            09540009
095500*      <other call parameters as documented>                      09550009
095600*      RETURNING integer-result-variable                          09560009
095700* END-CALL                                                        09570009
095800*                                                                 09580008
095900     05 SQLITE3-COLUMN-BYTES16          PIC X(23)                 09590002
096000            VALUE IS Z'sqlite3_column_bytes16'.                   09600000
096100* This function returns an integer which in COBOL                 09610009
096200* would be PIC S9(9) COMP-5. You should assign                    09620009
096300* the result to the variable using the RETURNING clause           09630009
096400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-BYTES16          09640009
096500*      <other call parameters as documented>                      09650009
096600*      RETURNING integer-result-variable                          09660009
096700* END-CALL                                                        09670009
096800*                                                                 09680008
096900     05 SQLITE3-COLUMN-COUNT            PIC X(21)                 09690002
097000            VALUE IS Z'sqlite3_column_count'.                     09700000
097100* This function returns an integer which in COBOL                 09710009
097200* would be PIC S9(9) COMP-5. You should assign                    09720009
097300* the result to the variable using the RETURNING clause           09730009
097400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-COUNT            09740009
097500*      <other call parameters as documented>                      09750009
097600*      RETURNING integer-result-variable                          09760009
097700* END-CALL                                                        09770009
097800*                                                                 09780008
097900     05 SQLITE3-COLUMN-DATABASE-NAME    PIC X(29)                 09790009
098000            VALUE IS Z'sqlite3_column_database_name'.             09800009
098100* This function returns a pointer which in COBOL                  09810009
098200* would be USAGE POINTER. You should assign                       09820009
098300* the result to the variable using the RETURNING clause           09830009
098400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-DATABASE-NAME    09840009
098500*      <other call parameters as documented>                      09850009
098600*      RETURNING pointer-result-variable                          09860009
098700* END-CALL                                                        09870009
098800*                                                                 09880009
098900     05 SQLITE3-COLUMN-DATABASE-NAME16  PIC X(32)                 09890009
099000            VALUE IS Z'sqlite3_column_database_name16'.           09900009
099100* This function returns a pointer which in COBOL                  09910009
099200* would be USAGE POINTER. You should assign                       09920009
099300* the result to the variable using the RETURNING clause           09930009
099400* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-DATABASE-NAME16  09940009
099500*      <other call parameters as documented>                      09950009
099600*      RETURNING pointer-result-variable                          09960009
099700* END-CALL                                                        09970009
099800*                                                                 09980009
099900     05 SQLITE3-COLUMN-DECLTYPE         PIC X(24)                 09990002
100000            VALUE IS Z'sqlite3_column_decltype'.                  10000000
100100* This function returns a pointer which in COBOL                  10010009
100200* would be USAGE POINTER. You should assign                       10020009
100300* the result to the variable using the RETURNING clause           10030009
100400* CALL SQLITE3 USING BY REFERENCE SQLITE3-DECLTYPE                10040009
100500*      <other call parameters as documented>                      10050009
100600*      RETURNING pointer-result-variable                          10060009
100700* END-CALL                                                        10070009
100800*                                                                 10080008
100900     05 SQLITE3-COLUMN-DECLTYPE16       PIC X(26)                 10090002
101000            VALUE IS Z'sqlite3_column_decltype16'.                10100000
101100* This function returns a pointer which in COBOL                  10110009
101200* would be USAGE POINTER. You should assign                       10120009
101300* the result to the variable using the RETURNING clause           10130009
101400* CALL SQLITE3 USING BY REFERENCE SQLITE3-DECLTYPE16              10140009
101500*      <other call parameters as documented>                      10150009
101600*      RETURNING pointer-result-variable                          10160009
101700* END-CALL                                                        10170009
101800*                                                                 10180008
101900     05 SQLITE3-COLUMN-DOUBLE           PIC X(22)                 10190002
102000            VALUE IS Z'sqlite3_column_double'.                    10200000
102100* Data is returned as a PIC USAGE COMP-2 in the second            10210011
102200* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  10220009
102300* inserted before the ones documented in the C API.               10230009
102400* CALL SQLITE3 USING                                              10240011
102500*              BY REFERENCE SQLITE3-COLUMN-DOUBLE                 10250011
102600*              BY REFERENCE double-result-variable                10260011
102700*              <other call parameters as documented>              10270011
102800* END-CALL                                                        10280009
102900* Before using this value, you must convert it from BFP to HFP.   10290009
103000* See the CONVERT-BFP-TO-HFP function below.                      10300009
103100*                                                                 10310005
103200     05 SQLITE3-COLUMN-INT              PIC X(19)                 10320002
103300            VALUE IS Z'sqlite3_column_int'.                       10330000
103400* This function returns an integer which in COBOL                 10340009
103500* would be PIC S9(9) COMP-5. You should assign                    10350009
103600* the result to the variable using the RETURNING clause           10360009
103700* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-INT              10370009
103800*      <other call parameters as documented>                      10380009
103900*      RETURNING integer-result-variable                          10390009
104000* END-CALL                                                        10400009
104100*                                                                 10410008
104200     05 SQLITE3-COLUMN-INT64            PIC X(21)                 10420002
104300            VALUE IS Z'sqlite3_column_int64'.                     10430000
104400* Data is returned as a PIC S9(18) COMP-5 in the second           10440009
104500* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  10450009
104600* inserted before the ones documented in the C API.               10460009
104700* This function returns a pointer which in COBOL                  10470009
104800* would be USAGE POINTER. You should assign                       10480009
104900* the result to the variable using the RETURNING clause           10490009
105000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-INT64            10500009
105100*      BY REFERENCE int64-returned-value                          10510009
105200*      <other call parameters as documented>                      10520009
105300* END-CALL                                                        10530009
105400*                                                                 10540005
105500     05 SQLITE3-COLUMN-NAME             PIC X(20)                 10550002
105600            VALUE IS Z'sqlite3_column_name'.                      10560000
105700* This function returns a pointer which in COBOL                  10570009
105800* would be USAGE POINTER. You should assign                       10580009
105900* the result to the variable using the RETURNING clause           10590009
106000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-NAME             10600009
106100*      <other call parameters as documented>                      10610009
106200*      RETURNING pointer-result-variable                          10620009
106300* END-CALL                                                        10630009
106400*                                                                 10640008
106500     05 SQLITE3-COLUMN-NAME16           PIC X(22)                 10650002
106600            VALUE IS Z'sqlite3_column_name16'.                    10660000
106700* This function returns a pointer which in COBOL                  10670009
106800* would be USAGE POINTER. You should assign                       10680009
106900* the result to the variable using the RETURNING clause           10690009
107000* CALL SQLITE3 USING BY REFERENCE SQLITE3- COLUMN-NAME16          10700009
107100*      <other call parameters as documented>                      10710009
107200*      RETURNING pointer-result-variable                          10720009
107300* END-CALL                                                        10730009
107400*                                                                 10740008
107500     05 SQLITE3-COLUMN-ORIGIN-NAME      PIC X(27)                 10750006
107600            VALUE IS Z'sqlite3_column_origin_name'.               10760006
107700* This function returns a pointer which in COBOL                  10770009
107800* would be USAGE POINTER. You should assign                       10780009
107900* the result to the variable using the RETURNING clause           10790009
108000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-ORIGIN-NAME      10800009
108100*      <other call parameters as documented>                      10810009
108200*      RETURNING pointer-result-variable                          10820009
108300* END-CALL                                                        10830009
108400*                                                                 10840008
108500     05 SQLITE3-COLUMN-ORIGIN-NAME16    PIC X(29)                 10850007
108600            VALUE IS Z'sqlite3_column_origin_name16'.             10860006
108700* This function returns a pointer which in COBOL                  10870009
108800* would be USAGE POINTER. You should assign                       10880009
108900* the result to the variable using the RETURNING clause           10890009
109000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-ORIGIN-NAME16    10900009
109100*      <other call parameters as documented>                      10910009
109200*      RETURNING pointer-result-variable                          10920009
109300* END-CALL                                                        10930009
109400*                                                                 10940008
109500     05 SQLITE3-COLUMN-TABLE-NAME       PIC X(26)                 10950004
109600            VALUE IS Z'sqlite3_column_table_name'.                10960004
109700* This function returns a pointer which in COBOL                  10970009
109800* would be USAGE POINTER. You should assign                       10980009
109900* the result to the variable using the RETURNING clause           10990009
110000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-TABLE-NAME       11000009
110100*      <other call parameters as documented>                      11010009
110200*      RETURNING pointer-result-variable                          11020009
110300* END-CALL                                                        11030009
110400*                                                                 11040008
110500     05 SQLITE3-COLUMN-TABLE-NAME16     PIC X(28)                 11050009
110600            VALUE IS Z'sqlite3_column_table_name16'.              11060004
110700* This function returns a pointer which in COBOL                  11070009
110800* would be USAGE POINTER. You should assign                       11080009
110900* the result to the variable using the RETURNING clause           11090009
111000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-TABLE-NAME16     11100009
111100*      <other call parameters as documented>                      11110009
111200*      RETURNING pointer-result-variable                          11120009
111300* END-CALL                                                        11130009
111400*                                                                 11140008
111500     05 SQLITE3-COLUMN-TEXT             PIC X(20)                 11150002
111600            VALUE IS Z'sqlite3_column_text'.                      11160000
111700* This function returns a pointer which in COBOL                  11170009
111800* would be USAGE POINTER. You should assign                       11180009
111900* the result to the variable using the RETURNING clause           11190009
112000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-TEXT             11200009
112100*      <other call parameters as documented>                      11210009
112200*      RETURNING pointer-result-variable                          11220009
112300* END-CALL                                                        11230009
112400*                                                                 11240008
112500     05 SQLITE3-COLUMN-TEXT16           PIC X(22)                 11250002
112600            VALUE IS Z'sqlite3_column_text16'.                    11260000
112700* This function returns a pointer which in COBOL                  11270009
112800* would be USAGE POINTER. You should assign                       11280009
112900* the result to the variable using the RETURNING clause           11290009
113000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-TEXT16           11300009
113100*      <other call parameters as documented>                      11310009
113200*      RETURNING pointer-result-variable                          11320009
113300* END-CALL                                                        11330009
113400*                                                                 11340008
113500     05 SQLITE3-COLUMN-TYPE             PIC X(20)                 11350002
113600            VALUE IS Z'sqlite3_column_type'.                      11360000
113700*                                                                 11370008
113800* This function returns an integer which in COBOL                 11380009
113900* would be PIC S9(9) COMP-5. You should assign                    11390009
114000* the result to the variable using the RETURNING clause           11400009
114100* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-TYPE             11410009
114200*      <other call parameters as documented>                      11420009
114300*      RETURNING integer-result-variable                          11430009
114400* END-CALL                                                        11440009
114500     05 SQLITE3-COLUMN-VALUE            PIC X(21)                 11450002
114600            VALUE IS Z'sqlite3_column_value'.                     11460000
114700* This function returns a pointer which in COBOL                  11470009
114800* would be USAGE POINTER. You should assign                       11480009
114900* the result to the variable using the RETURNING clause           11490009
115000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COLUMN-VALUE            11500009
115100*      <other call parameters as documented>                      11510009
115200*      RETURNING pointer-result-variable                          11520009
115300* END-CALL                                                        11530009
115400*                                                                 11540008
115500     05 SQLITE3-COMMIT-HOOK             PIC X(20)                 11550002
115600            VALUE IS Z'sqlite3_commit_hook'.                      11560000
115700* This function returns a pointer which in COBOL                  11570009
115800* would be USAGE POINTER. You should assign                       11580009
115900* the result to the variable using the RETURNING clause           11590009
116000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COMMIT-HOOK             11600009
116100*      <other call parameters as documented>                      11610009
116200*      RETURNING pointer-result-variable                          11620009
116300* END-CALL                                                        11630009
116400*                                                                 11640008
116500     05 SQLITE3-COMPILEOPTION-GET       PIC X(26)                 11650002
116600            VALUE IS Z'sqlite3_compileoption_get'.                11660000
116700* This function returns a pointer which in COBOL                  11670009
116800* would be USAGE POINTER. You should assign                       11680009
116900* the result to the variable using the RETURNING clause           11690009
117000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COMPILEOPTION-GET       11700009
117100*      <other call parameters as documented>                      11710009
117200*      RETURNING pointer-result-variable                          11720009
117300* END-CALL                                                        11730009
117400*                                                                 11740008
117500     05 SQLITE3-COMPILEOPTION-USED      PIC X(27)                 11750002
117600            VALUE IS Z'sqlite3_compileoption_used'.               11760000
117700*                                                                 11770008
117800* This function returns an integer which in COBOL                 11780009
117900* would be PIC S9(9) COMP-5. You should assign                    11790009
118000* the result to the variable using the RETURNING clause           11800009
118100* CALL SQLITE3 USING BY REFERENCE SQLITE3-COMPILEOPTION-USED      11810009
118200*      <other call parameters as documented>                      11820009
118300*      RETURNING integer-result-variable                          11830009
118400* END-CALL                                                        11840009
118500     05 SQLITE3-COMPLETE                PIC X(17)                 11850002
118600            VALUE IS Z'sqlite3_complete'.                         11860000
118700* This function returns an integer which in COBOL                 11870009
118800* would be PIC S9(9) COMP-5. You should assign                    11880009
118900* the result to the variable using the RETURNING clause           11890009
119000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COMPLETE                11900009
119100*      <other call parameters as documented>                      11910009
119200*      RETURNING integer-result-variable                          11920009
119300* END-CALL                                                        11930009
119400*                                                                 11940008
119500     05 SQLITE3-COMPLETE16              PIC X(19)                 11950002
119600            VALUE IS Z'sqlite3_complete16'.                       11960000
119700* This function returns an integer which in COBOL                 11970009
119800* would be PIC S9(9) COMP-5. You should assign                    11980009
119900* the result to the variable using the RETURNING clause           11990009
120000* CALL SQLITE3 USING BY REFERENCE SQLITE3-COMPLETE16              12000009
120100*      <other call parameters as documented>                      12010009
120200*      RETURNING integer-result-variable                          12020009
120300* END-CALL                                                        12030009
120400*                                                                 12040008
120500     05 SQLITE3-CONFIG                  PIC X(15)                 12050002
120600            VALUE IS Z'sqlite3_config'.                           12060000
120700* This function returns an integer which in COBOL                 12070009
120800* would be PIC S9(9) COMP-5. You should assign                    12080009
120900* the result to the variable using the RETURNING clause           12090009
121000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CONFIG                  12100009
121100*      <other call parameters as documented>                      12110009
121200*      RETURNING integer-result-variable                          12120009
121300* END-CALL                                                        12130009
121400*                                                                 12140008
121500     05 SQLITE3-CONTEXT-DB-HANDLE       PIC X(26)                 12150002
121600            VALUE IS Z'sqlite3_context_db_handle'.                12160000
121700* This function returns a pointer which in COBOL                  12170009
121800* would be USAGE POINTER. You should assign                       12180009
121900* the result to the variable using the RETURNING clause           12190009
122000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CONTEXT-DB-HANDLE       12200009
122100*      <other call parameters as documented>                      12210009
122200*      RETURNING pointer-result-variable                          12220009
122300* END-CALL                                                        12230009
122400*                                                                 12240008
122500     05 SQLITE3-CREATE-COLLATION        PIC X(25)                 12250002
122600            VALUE IS Z'sqlite3_create_collation'.                 12260000
122700* This function returns an integer which in COBOL                 12270009
122800* would be PIC S9(9) COMP-5. You should assign                    12280009
122900* the result to the variable using the RETURNING clause           12290009
123000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-COLLATION        12300009
123100*      <other call parameters as documented>                      12310009
123200*      RETURNING integer-result-variable                          12320009
123300* END-CALL                                                        12330009
123400*                                                                 12340008
123500     05 SQLITE3-CREATE-COLLATION-V2     PIC X(28)                 12350002
123600            VALUE IS Z'sqlite3_create_collation_v2'.              12360000
123700* This function returns an integer which in COBOL                 12370009
123800* would be PIC S9(9) COMP-5. You should assign                    12380009
123900* the result to the variable using the RETURNING clause           12390009
124000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-COLLATION-V2     12400009
124100*      <other call parameters as documented>                      12410009
124200*      RETURNING integer-result-variable                          12420009
124300* END-CALL                                                        12430009
124400*                                                                 12440008
124500     05 SQLITE3-CREATE-COLLATION16      PIC X(27)                 12450002
124600            VALUE IS Z'sqlite3_create_collation16'.               12460000
124700* This function returns an integer which in COBOL                 12470009
124800* would be PIC S9(9) COMP-5. You should assign                    12480009
124900* the result to the variable using the RETURNING clause           12490009
125000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-COLLATION16      12500009
125100*      <other call parameters as documented>                      12510009
125200*      RETURNING integer-result-variable                          12520009
125300* END-CALL                                                        12530009
125400*                                                                 12540008
125500     05 SQLITE3-CREATE-FUNCTION         PIC X(24)                 12550002
125600            VALUE IS Z'sqlite3_create_function'.                  12560000
125700* This function returns an integer which in COBOL                 12570009
125800* would be PIC S9(9) COMP-5. You should assign                    12580009
125900* the result to the variable using the RETURNING clause           12590009
126000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-FUNCTION         12600009
126100*      <other call parameters as documented>                      12610009
126200*      RETURNING integer-result-variable                          12620009
126300* END-CALL                                                        12630009
126400*                                                                 12640008
126500     05 SQLITE3-CREATE-FUNCTION-V2      PIC X(27)                 12650002
126600            VALUE IS Z'sqlite3_create_function_v2'.               12660000
126700* This function returns an integer which in COBOL                 12670009
126800* would be PIC S9(9) COMP-5. You should assign                    12680009
126900* the result to the variable using the RETURNING clause           12690009
127000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-FUNCTION-V2      12700009
127100*      <other call parameters as documented>                      12710009
127200*      RETURNING integer-result-variable                          12720009
127300* END-CALL                                                        12730009
127400*                                                                 12740008
127500     05 SQLITE3-CREATE-FUNCTION16       PIC X(26)                 12750002
127600            VALUE IS Z'sqlite3_create_function16'.                12760000
127700* This function returns an integer which in COBOL                 12770009
127800* would be PIC S9(9) COMP-5. You should assign                    12780009
127900* the result to the variable using the RETURNING clause           12790009
128000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-FUNCTION16       12800009
128100*      <other call parameters as documented>                      12810009
128200*      RETURNING integer-result-variable                          12820009
128300* END-CALL                                                        12830009
128400*                                                                 12840008
128500     05 SQLITE3-CREATE-MODULE           PIC X(22)                 12850002
128600            VALUE IS Z'sqlite3_create_module'.                    12860000
128700* This function returns an integer which in COBOL                 12870009
128800* would be PIC S9(9) COMP-5. You should assign                    12880009
128900* the result to the variable using the RETURNING clause           12890009
129000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-MODULE           12900009
129100*      <other call parameters as documented>                      12910009
129200*      RETURNING integer-result-variable                          12920009
129300* END-CALL                                                        12930009
129400*                                                                 12940008
129500     05 SQLITE3-CREATE-MODULE-V2        PIC X(25)                 12950002
129600            VALUE IS Z'sqlite3_create_module_v2'.                 12960000
129700* This function returns an integer which in COBOL                 12970009
129800* would be PIC S9(9) COMP-5. You should assign                    12980009
129900* the result to the variable using the RETURNING clause           12990009
130000* CALL SQLITE3 USING BY REFERENCE SQLITE3-CREATE-MODULE-V2        13000009
130100*      <other call parameters as documented>                      13010009
130200*      RETURNING integer-result-variable                          13020009
130300* END-CALL                                                        13030009
130400*                                                                 13040008
130500     05 SQLITE3-DATA-COUNT              PIC X(19)                 13050002
130600            VALUE IS Z'sqlite3_data_count'.                       13060000
130700* This function returns an integer which in COBOL                 13070009
130800* would be PIC S9(9) COMP-5. You should assign                    13080009
130900* the result to the variable using the RETURNING clause           13090009
131000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DATA-COUNT              13100009
131100*      <other call parameters as documented>                      13110009
131200*      RETURNING integer-result-variable                          13120009
131300* END-CALL                                                        13130009
131400*                                                                 13140008
131500     05 SQLITE3-DB-CONFIG               PIC X(18)                 13150002
131600            VALUE IS Z'sqlite3_db_config'.                        13160000
131700* This function returns an integer which in COBOL                 13170009
131800* would be PIC S9(9) COMP-5. You should assign                    13180009
131900* the result to the variable using the RETURNING clause           13190009
132000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-CONFIG               13200009
132100*      <other call parameters as documented>                      13210009
132200*      RETURNING integer-result-variable                          13220009
132300* END-CALL                                                        13230009
132400*                                                                 13240008
132500     05 SQLITE3-DB-FILENAME             PIC X(20)                 13250002
132600            VALUE IS Z'sqlite3_db_filename'.                      13260000
132700* This function returns a pointer which in COBOL                  13270009
132800* would be USAGE POINTER. You should assign                       13280009
132900* the result to the variable using the RETURNING clause           13290009
133000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-FILENAME             13300009
133100*      <other call parameters as documented>                      13310009
133200*      RETURNING pointer-result-variable                          13320009
133300* END-CALL                                                        13330009
133400*                                                                 13340008
133500     05 SQLITE3-DB-HANDLE               PIC X(18)                 13350002
133600            VALUE IS Z'sqlite3_db_handle'.                        13360000
133700* This function returns a pointer which in COBOL                  13370009
133800* would be USAGE POINTER. You should assign                       13380009
133900* the result to the variable using the RETURNING clause           13390009
134000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-HANDLE               13400009
134100*      <other call parameters as documented>                      13410009
134200*      RETURNING pointer-result-variable                          13420009
134300* END-CALL                                                        13430009
134400*                                                                 13440008
134500     05 SQLITE3-DB-MUTEX                PIC X(17)                 13450002
134600            VALUE IS Z'sqlite3_db_mutex'.                         13460000
134700* This function returns a pointer which in COBOL                  13470009
134800* would be USAGE POINTER. You should assign                       13480009
134900* the result to the variable using the RETURNING clause           13490009
135000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-MUTEX                13500009
135100*      <other call parameters as documented>                      13510009
135200*      RETURNING pointer-result-variable                          13520009
135300* END-CALL                                                        13530009
135400*                                                                 13540008
135500     05 SQLITE3-DB-READONLY             PIC X(20)                 13550002
135600            VALUE IS Z'sqlite3_db_readonly'.                      13560000
135700* This function returns an integer which in COBOL                 13570009
135800* would be PIC S9(9) COMP-5. You should assign                    13580009
135900* the result to the variable using the RETURNING clause           13590009
136000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-READONLY             13600009
136100*      <other call parameters as documented>                      13610009
136200*      RETURNING integer-result-variable                          13620009
136300* END-CALL                                                        13630009
136400*                                                                 13640008
136500     05 SQLITE3-DB-RELEASE-MEMORY       PIC X(26)                 13650002
136600            VALUE IS Z'sqlite3_db_release_memory'.                13660000
136700* This function returns an integer which in COBOL                 13670009
136800* would be PIC S9(9) COMP-5. You should assign                    13680009
136900* the result to the variable using the RETURNING clause           13690009
137000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-RELEASE-MEMORY       13700009
137100*      <other call parameters as documented>                      13710009
137200*      RETURNING integer-result-variable                          13720009
137300* END-CALL                                                        13730009
137400*                                                                 13740008
137500     05 SQLITE3-DB-STATUS               PIC X(18)                 13750002
137600            VALUE IS Z'sqlite3_db_status'.                        13760000
137700* This function returns an integer which in COBOL                 13770009
137800* would be PIC S9(9) COMP-5. You should assign                    13780009
137900* the result to the variable using the RETURNING clause           13790009
138000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DB-STATUS               13800009
138100*      <other call parameters as documented>                      13810009
138200*      RETURNING integer-result-variable                          13820009
138300* END-CALL                                                        13830009
138400*                                                                 13840008
138500     05 SQLITE3-DECLARE-VTAB            PIC X(21)                 13850002
138600            VALUE IS Z'sqlite3_declare_vtab'.                     13860000
138700* This function returns an integer which in COBOL                 13870009
138800* would be PIC S9(9) COMP-5. You should assign                    13880009
138900* the result to the variable using the RETURNING clause           13890009
139000* CALL SQLITE3 USING BY REFERENCE SQLITE3-DECLARE-VTAB            13900009
139100*      <other call parameters as documented>                      13910009
139200*      RETURNING integer-result-variable                          13920009
139300* END-CALL                                                        13930009
139400*                                                                 13940008
139500     05 SQLITE3-ENABLE-LOAD-EXTENSION   PIC X(30)                 13950002
139600            VALUE IS Z'sqlite3_enable_load_extension'.            13960000
139700* This function returns an integer which in COBOL                 13970009
139800* would be PIC S9(9) COMP-5. You should assign                    13980009
139900* the result to the variable using the RETURNING clause           13990009
140000* CALL SQLITE3 USING BY REFERENCE SQLITE3-ENABLE-LOAD-EXTENSION   14000009
140100*      <other call parameters as documented>                      14010009
140200*      RETURNING integer-result-variable                          14020009
140300* END-CALL                                                        14030009
140400*                                                                 14040008
140500     05 SQLITE3-ENABLE-SHARED-CACHE     PIC X(28)                 14050002
140600            VALUE IS Z'sqlite3_enable_shared_cache'.              14060000
140700* This function returns an integer which in COBOL                 14070009
140800* would be PIC S9(9) COMP-5. You should assign                    14080009
140900* the result to the variable using the RETURNING clause           14090009
141000* CALL SQLITE3 USING BY REFERENCE SQLITE3-ENABLE-SHARED-CACHE     14100009
141100*      <other call parameters as documented>                      14110009
141200*      RETURNING integer-result-variable                          14120009
141300* END-CALL                                                        14130009
141400*                                                                 14140008
141500     05 SQLITE3-ERRCODE                 PIC X(16)                 14150002
141600            VALUE IS Z'sqlite3_errcode'.                          14160000
141700* This function returns an integer which in COBOL                 14170009
141800* would be PIC S9(9) COMP-5. You should assign                    14180009
141900* the result to the variable using the RETURNING clause           14190009
142000* CALL SQLITE3 USING BY REFERENCE SQLITE3-ERRCODE                 14200009
142100*      <other call parameters as documented>                      14210009
142200*      RETURNING integer-result-variable                          14220009
142300* END-CALL                                                        14230009
142400*                                                                 14240008
142500     05 SQLITE3-ERRMSG                  PIC X(15)                 14250002
142600            VALUE IS Z'sqlite3_errmsg'.                           14260000
142700* This function returns a pointer which in COBOL                  14270009
142800* would be USAGE POINTER. You should assign                       14280009
142900* the result to the variable using the RETURNING clause           14290009
143000* CALL SQLITE3 USING BY REFERENCE SQLITE3-ERRMSG                  14300009
143100*      <other call parameters as documented>                      14310009
143200*      RETURNING pointer-result-variable                          14320009
143300* END-CALL                                                        14330009
143400*                                                                 14340008
143500     05 SQLITE3-ERRMSG16                PIC X(17)                 14350002
143600            VALUE IS Z'sqlite3_errmsg16'.                         14360000
143700* This function returns a pointer which in COBOL                  14370009
143800* would be USAGE POINTER. You should assign                       14380009
143900* the result to the variable using the RETURNING clause           14390009
144000* CALL SQLITE3 USING BY REFERENCE SQLITE3-ERRMSG16                14400009
144100*      <other call parameters as documented>                      14410009
144200*      RETURNING pointer-result-variable                          14420009
144300* END-CALL                                                        14430009
144400*                                                                 14440008
144500     05 SQLITE3-ERRSTR                  PIC X(15)                 14450002
144600            VALUE IS Z'sqlite3_errstr'.                           14460000
144700* This function returns a pointer which in COBOL                  14470009
144800* would be USAGE POINTER. You should assign                       14480009
144900* the result to the variable using the RETURNING clause           14490009
145000* CALL SQLITE3 USING BY REFERENCE SQLITE3-ERRSTR                  14500009
145100*      <other call parameters as documented>                      14510009
145200*      RETURNING pointer-result-variable                          14520009
145300* END-CALL                                                        14530009
145400*                                                                 14540008
145500     05 SQLITE3-EXEC                    PIC X(13)                 14550002
145600            VALUE IS Z'sqlite3_exec'.                             14560000
145700* This function returns an integer which in COBOL                 14570009
145800* would be PIC S9(9) COMP-5. You should assign                    14580009
145900* the result to the variable using the RETURNING clause           14590009
146000* CALL SQLITE3 USING BY REFERENCE SQLITE3-EXEC                    14600009
146100*      <other call parameters as documented>                      14610009
146200*      RETURNING integer-result-variable                          14620009
146300* END-CALL                                                        14630009
146400*                                                                 14640008
146500     05 SQLITE3-EXPIRED                 PIC X(16)                 14650002
146600            VALUE IS Z'sqlite3_expired'.                          14660000
146700* This function returns an integer which in COBOL                 14670009
146800* would be PIC S9(9) COMP-5. You should assign                    14680009
146900* the result to the variable using the RETURNING clause           14690009
147000* CALL SQLITE3 USING BY REFERENCE SQLITE3-EXPIRED                 14700009
147100*      <other call parameters as documented>                      14710009
147200*      RETURNING integer-result-variable                          14720009
147300* END-CALL                                                        14730009
147400*                                                                 14740008
147500     05 SQLITE3-EXTENDED-ERRCODE        PIC X(25)                 14750002
147600            VALUE IS Z'sqlite3_extended_errcode'.                 14760000
147700* This function returns an integer which in COBOL                 14770009
147800* would be PIC S9(9) COMP-5. You should assign                    14780009
147900* the result to the variable using the RETURNING clause           14790009
148000* CALL SQLITE3 USING BY REFERENCE SQLITE3-EXTENDED-ERRCODE        14800009
148100*      <other call parameters as documented>                      14810009
148200*      RETURNING integer-result-variable                          14820009
148300* END-CALL                                                        14830009
148400*                                                                 14840008
148500     05 SQLITE3-EXTENDED-RESULT-CODES   PIC X(30)                 14850002
148600            VALUE IS Z'sqlite3_extended_result_codes'.            14860000
148700* This function returns an integer which in COBOL                 14870009
148800* would be PIC S9(9) COMP-5. You should assign                    14880009
148900* the result to the variable using the RETURNING clause           14890009
149000* CALL SQLITE3 USING BY REFERENCE SQLITE3-EXTENDED-RESULT-CODES   14900009
149100*      <other call parameters as documented>                      14910009
149200*      RETURNING integer-result-variable                          14920009
149300* END-CALL                                                        14930009
149400*                                                                 14940008
149500     05 SQLITE3-FILE-CONTROL            PIC X(21)                 14950002
149600            VALUE IS Z'sqlite3_file_control'.                     14960000
149700* This function returns an integer which in COBOL                 14970009
149800* would be PIC S9(9) COMP-5. You should assign                    14980009
149900* the result to the variable using the RETURNING clause           14990009
150000* CALL SQLITE3 USING BY REFERENCE SQLITE3-FILE-CONTROL            15000009
150100*      <other call parameters as documented>                      15010009
150200*      RETURNING integer-result-variable                          15020009
150300* END-CALL                                                        15030009
150400*                                                                 15040008
150500     05 SQLITE3-FINALIZE                PIC X(17)                 15050002
150600            VALUE IS Z'sqlite3_finalize'.                         15060000
150700* This function returns an integer which in COBOL                 15070009
150800* would be PIC S9(9) COMP-5. You should assign                    15080009
150900* the result to the variable using the RETURNING clause           15090009
151000* CALL SQLITE3 USING BY REFERENCE SQLITE3-FINIALIZE               15100009
151100*      <other call parameters as documented>                      15110009
151200*      RETURNING integer-result-variable                          15120009
151300* END-CALL                                                        15130009
151400*                                                                 15140008
151500     05 SQLITE3-FREE                    PIC X(13)                 15150002
151600            VALUE IS Z'sqlite3_free'.                             15160000
151700* This function does not return anything at all.                  15170009
151800* CALL SQLITE3 USING BY REFERENCE SQLITE3-FREE                    15180009
151900*      <other call parameters as documented>                      15190009
152000* END-CALL                                                        15200009
152100*                                                                 15210008
152200     05 SQLITE3-FREE-TABLE              PIC X(19)                 15220002
152300            VALUE IS Z'sqlite3_free_table'.                       15230000
152400* This function does not return anything at all.                  15240009
152500* CALL SQLITE3 USING BY REFERENCE SQLITE3-FREE-TABLE              15250009
152600*      <other call parameters as documented>                      15260009
152700* END-CALL                                                        15270009
152800*                                                                 15280008
152900     05 SQLITE3-GET-AUTOCOMMIT          PIC X(23)                 15290002
153000            VALUE IS Z'sqlite3_get_autocommit'.                   15300000
153100* This function returns an integer which in COBOL                 15310009
153200* would be PIC S9(9) COMP-5. You should assign                    15320009
153300* the result to the variable using the RETURNING clause           15330009
153400* CALL SQLITE3 USING BY REFERENCE SQLITE3-GET-AUTOCOMMIT          15340009
153500*      <other call parameters as documented>                      15350009
153600*      RETURNING integer-result-variable                          15360009
153700* END-CALL                                                        15370009
153800*                                                                 15380008
153900     05 SQLITE3-GET-AUXDATA             PIC X(20)                 15390002
154000            VALUE IS Z'sqlite3_get_auxdata'.                      15400000
154100* This function returns a pointer which in COBOL                  15410009
154200* would be USAGE POINTER. You should assign                       15420009
154300* the result to the variable using the RETURNING clause           15430009
154400* CALL SQLITE3 USING BY REFERENCE SQLITE3-GET-AUXDATA             15440009
154500*      <other call parameters as documented>                      15450009
154600*      RETURNING pointer-result-variable                          15460009
154700* END-CALL                                                        15470009
154800*                                                                 15480008
154900     05 SQLITE3-GET-TABLE               PIC X(18)                 15490002
155000            VALUE IS Z'sqlite3_get_table'.                        15500000
155100* This function returns an integer which in COBOL                 15510009
155200* would be PIC S9(9) COMP-5. You should assign                    15520009
155300* the result to the variable using the RETURNING clause           15530009
155400* CALL SQLITE3 USING BY REFERENCE SQLITE3-GET-TABLE               15540009
155500*      <other call parameters as documented>                      15550009
155600*      RETURNING integer-result-variable                          15560009
155700* END-CALL                                                        15570009
155800*                                                                 15580008
155900     05 SQLITE3-GLOBAL-RECOVER          PIC X(23)                 15590002
156000            VALUE IS Z'sqlite3_global_recover'.                   15600000
156100* This function returns an integer which in COBOL                 15610009
156200* would be PIC S9(9) COMP-5. You should assign                    15620009
156300* the result to the variable using the RETURNING clause           15630009
156400* CALL SQLITE3 USING BY REFERENCE SQLITE3-GLOBAL-RECOVER          15640009
156500*      <other call parameters as documented>                      15650009
156600*      RETURNING integer-result-variable                          15660009
156700* END-CALL                                                        15670009
156800*                                                                 15680008
156900     05 SQLITE3-INITIALIZE              PIC X(19)                 15690002
157000            VALUE IS Z'sqlite3_initialize'.                       15700000
157100* This function returns an integer which in COBOL                 15710009
157200* would be PIC S9(9) COMP-5. You should assign                    15720009
157300* the result to the variable using the RETURNING clause           15730009
157400* CALL SQLITE3 USING BY REFERENCE SQLITE3-INITIALIZE              15740009
157500*      <other call parameters as documented>                      15750009
157600*      RETURNING integer-result-variable                          15760009
157700* END-CALL                                                        15770009
157800*                                                                 15780008
157900     05 SQLITE3-INTERRUPT               PIC X(18)                 15790002
158000            VALUE IS Z'sqlite3_interrupt'.                        15800000
158100* This function does not return anything at all.                  15810009
158200* CALL SQLITE3 USING BY REFERENCE SQLITE3-INTERRUPT               15820009
158300*      <other call parameters as documented>                      15830009
158400* END-CALL                                                        15840009
158500*                                                                 15850008
158600     05 SQLITE3-LAST-INSERT-ROWID       PIC X(26)                 15860002
158700            VALUE IS Z'sqlite3_last_insert_rowid'.                15870000
158800* Data is returned as a PIC S9(18) COMP-5 in the second           15880009
158900* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  15890009
159000* inserted before the ones documented in the C API.               15900009
159100* This function returns a pointer which in COBOL                  15910009
159200* would be USAGE POINTER. You should assign                       15920009
159300* the result to the variable using the RETURNING clause           15930009
159400* CALL SQLITE3 USING BY REFERENCE SQLITE3-LAST-INSERT-ROWID       15940009
159500*      BY REFERENCE int64-returned-value                          15950009
159600*      <other call parameters as documented>                      15960009
159700* END-CALL                                                        15970009
159800*                                                                 15980005
159900     05 SQLITE3-LIBVERSION              PIC X(19)                 15990002
160000            VALUE IS Z'sqlite3_libversion'.                       16000000
160100* This function returns a pointer which in COBOL                  16010009
160200* would be USAGE POINTER. You should assign                       16020009
160300* the result to the variable using the RETURNING clause           16030009
160400* CALL SQLITE3 USING BY REFERENCE SQLITE3-LIBVERSION              16040009
160500*      <other call parameters as documented>                      16050009
160600*      RETURNING pointer-result-variable                          16060009
160700* END-CALL                                                        16070009
160800*                                                                 16080008
160900     05 SQLITE3-LIBVERSION-NUMBER       PIC X(26)                 16090002
161000            VALUE IS Z'sqlite3_libversion_number'.                16100000
161100* This function returns an integer which in COBOL                 16110009
161200* would be PIC S9(9) COMP-5. You should assign                    16120009
161300* the result to the variable using the RETURNING clause           16130009
161400* CALL SQLITE3 USING BY REFERENCE SQLITE3-LIBVERSION-NUMBER       16140009
161500*      <other call parameters as documented>                      16150009
161600*      RETURNING integer-result-variable                          16160009
161700* END-CALL                                                        16170009
161800*                                                                 16180008
161900     05 SQLITE3-LIMIT                   PIC X(14)                 16190002
162000            VALUE IS Z'sqlite3_limit'.                            16200000
162100* This function returns an integer which in COBOL                 16210009
162200* would be PIC S9(9) COMP-5. You should assign                    16220009
162300* the result to the variable using the RETURNING clause           16230009
162400* CALL SQLITE3 USING BY REFERENCE SQLITE3-LIMIT                   16240009
162500*      <other call parameters as documented>                      16250009
162600*      RETURNING integer-result-variable                          16260009
162700* END-CALL                                                        16270009
162800*                                                                 16280008
162900     05 SQLITE3-LOAD-EXTENSION          PIC X(23)                 16290002
163000            VALUE IS Z'sqlite3_load_extension'.                   16300000
163100* This function returns an integer which in COBOL                 16310009
163200* would be PIC S9(9) COMP-5. You should assign                    16320009
163300* the result to the variable using the RETURNING clause           16330009
163400* CALL SQLITE3 USING BY REFERENCE SQLITE3-LOAD-EXTENSION          16340009
163500*      <other call parameters as documented>                      16350009
163600*      RETURNING integer-result-variable                          16360009
163700* END-CALL                                                        16370009
163800*                                                                 16380008
163900     05 SQLITE3-LOG                     PIC X(12)                 16390002
164000            VALUE IS Z'sqlite3_log'.                              16400000
164100* This function does not return anything at all.                  16410009
164200* CALL SQLITE3 USING BY REFERENCE SQLITE3-LOG                     16420009
164300*      <other call parameters as documented>                      16430009
164400* END-CALL                                                        16440009
164500*                                                                 16450008
164600     05 SQLITE3-MALLOC                  PIC X(15)                 16460002
164700            VALUE IS Z'sqlite3_malloc'.                           16470000
164800* This function returns a pointer which in COBOL                  16480009
164900* would be USAGE POINTER. You should assign                       16490009
165000* the result to the variable using the RETURNING clause           16500009
165100* CALL SQLITE3 USING BY REFERENCE SQLITE3-MALLOC                  16510009
165200*      <other call parameters as documented>                      16520009
165300*      RETURNING pointer-result-variable                          16530009
165400* END-CALL                                                        16540009
165500*                                                                 16550008
165600     05 SQLITE3-MEMORY-ALARM            PIC X(21)                 16560002
165700            VALUE IS Z'sqlite3_memory_alarm'.                     16570000
165800* This function returns an integer which in COBOL                 16580009
165900* would be PIC S9(9) COMP-5. You should assign                    16590009
166000* the result to the variable using the RETURNING clause           16600009
166100* CALL SQLITE3 USING BY REFERENCE SQLITE3-MEMORY-ALARM            16610009
166200*      <other call parameters as documented>                      16620009
166300*      RETURNING integer-result-variable                          16630009
166400* END-CALL                                                        16640009
166500*                                                                 16650008
166600     05 SQLITE3-MEMORY-HIGHWATER        PIC X(25)                 16660002
166700            VALUE IS Z'sqlite3_memory_highwater'.                 16670000
166800* Data is returned as a PIC S9(18) COMP-5 in the second           16680009
166900* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  16690009
167000* inserted before the ones documented in the C API.               16700009
167100* This function returns a pointer which in COBOL                  16710009
167200* would be USAGE POINTER. You should assign                       16720009
167300* the result to the variable using the RETURNING clause           16730009
167400* CALL SQLITE3 USING BY REFERENCE SQLITE3-MEMORY-HIGHWATER        16740009
167500*      BY REFERENCE int64-returned-value                          16750009
167600*      <other call parameters as documented>                      16760009
167700* END-CALL                                                        16770009
167800*                                                                 16780005
167900     05 SQLITE3-MEMORY-USED             PIC X(20)                 16790002
168000            VALUE IS Z'sqlite3_memory_used'.                      16800000
168100* Data is returned as a PIC S9(18) COMP-5 in the second           16810009
168200* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  16820009
168300* inserted before the ones documented in the C API.               16830009
168400* This function returns a pointer which in COBOL                  16840009
168500* would be USAGE POINTER. You should assign                       16850009
168600* the result to the variable using the RETURNING clause           16860009
168700* CALL SQLITE3 USING BY REFERENCE SQLITE3-MEMORY-USED             16870009
168800*      BY REFERENCE int64-returned-value                          16880009
168900*      <other call parameters as documented>                      16890009
169000* END-CALL                                                        16900009
169100*                                                                 16910005
169200     05 SQLITE3-MPRINTF                 PIC X(16)                 16920002
169300            VALUE IS Z'sqlite3_mprintf'.                          16930000
169400* This function returns a pointer which in COBOL                  16940009
169500* would be USAGE POINTER. You should assign                       16950009
169600* the result to the variable using the RETURNING clause           16960009
169700* CALL SQLITE3 USING BY REFERENCE SQLITE3-MPRINTF                 16970009
169800*      <other call parameters as documented>                      16980009
169900*      RETURNING pointer-result-variable                          16990009
170000* END-CALL                                                        17000009
170100*                                                                 17010008
170200     05 SQLITE3-MUTEX-ALLOC             PIC X(20)                 17020002
170300            VALUE IS Z'sqlite3_mutex_alloc'.                      17030000
170400* This function returns a pointer which in COBOL                  17040009
170500* would be USAGE POINTER. You should assign                       17050009
170600* the result to the variable using the RETURNING clause           17060009
170700* CALL SQLITE3 USING BY REFERENCE SQLITE3-MUTEX-ALLOC             17070009
170800*      <other call parameters as documented>                      17080009
170900*      RETURNING pointer-result-variable                          17090009
171000* END-CALL                                                        17100009
171100*                                                                 17110008
171200     05 SQLITE3-MUTEX-ENTER             PIC X(20)                 17120002
171300            VALUE IS Z'sqlite3_mutex_enter'.                      17130000
171400* This function does not return anything at all.                  17140009
171500* CALL SQLITE3 USING BY REFERENCE SQLITE3-MUTEX-ENTER             17150009
171600*      <other call parameters as documented>                      17160009
171700* END-CALL                                                        17170009
171800*                                                                 17180008
171900     05 SQLITE3-MUTEX-FREE              PIC X(19)                 17190002
172000            VALUE IS Z'sqlite3_mutex_free'.                       17200000
172100* This function does not return anything at all.                  17210009
172200* CALL SQLITE3 USING BY REFERENCE SQLITE3-MUTEX-FREE              17220009
172300*      <other call parameters as documented>                      17230009
172400* END-CALL                                                        17240009
172500*                                                                 17250008
172600     05 SQLITE3-MUTEX-LEAVE             PIC X(20)                 17260002
172700            VALUE IS Z'sqlite3_mutex_leave'.                      17270000
172800* This function does not return anything at all.                  17280009
172900* CALL SQLITE3 USING BY REFERENCE SQLITE3-MUTEX-LEAVE             17290009
173000*      <other call parameters as documented>                      17300009
173100* END-CALL                                                        17310009
173200*                                                                 17320008
173300     05 SQLITE3-MUTEX-TRY               PIC X(18)                 17330002
173400            VALUE IS Z'sqlite3_mutex_try'.                        17340000
173500* This function returns an integer which in COBOL                 17350009
173600* would be PIC S9(9) COMP-5. You should assign                    17360009
173700* the result to the variable using the RETURNING clause           17370009
173800* CALL SQLITE3 USING BY REFERENCE SQLITE3-MUTEX-TRY               17380009
173900*      <other call parameters as documented>                      17390009
174000*      RETURNING integer-result-variable                          17400009
174100* END-CALL                                                        17410009
174200*                                                                 17420008
174300     05 SQLITE3-NEXT-STMT               PIC X(18)                 17430002
174400            VALUE IS Z'sqlite3_next_stmt'.                        17440000
174500* This function returns a pointer which in COBOL                  17450009
174600* would be USAGE POINTER. You should assign                       17460009
174700* the result to the variable using the RETURNING clause           17470009
174800* CALL SQLITE3 USING BY REFERENCE SQLITE3-NEXT-STMT               17480009
174900*      <other call parameters as documented>                      17490009
175000*      RETURNING pointer-result-variable                          17500009
175100* END-CALL                                                        17510009
175200*                                                                 17520008
175300     05 SQLITE3-OPEN                    PIC X(13)                 17530002
175400            VALUE IS Z'sqlite3_open'.                             17540000
175500* This function returns an integer which in COBOL                 17550009
175600* would be PIC S9(9) COMP-5. You should assign                    17560009
175700* the result to the variable using the RETURNING clause           17570009
175800* CALL SQLITE3 USING BY REFERENCE SQLITE3-OPEN                    17580009
175900*      <other call parameters as documented>                      17590009
176000*      RETURNING integer-result-variable                          17600009
176100* END-CALL                                                        17610009
176200*                                                                 17620008
176300     05 SQLITE3-OPEN-V2                 PIC X(16)                 17630002
176400            VALUE IS Z'sqlite3_open_v2'.                          17640000
176500* This function returns an integer which in COBOL                 17650009
176600* would be PIC S9(9) COMP-5. You should assign                    17660009
176700* the result to the variable using the RETURNING clause           17670009
176800* CALL SQLITE3 USING BY REFERENCE SQLITE3-OPEN-V2                 17680009
176900*      <other call parameters as documented>                      17690009
177000*      RETURNING integer-result-variable                          17700009
177100* END-CALL                                                        17710009
177200*                                                                 17720008
177300     05 SQLITE3-OPEN16                  PIC X(15)                 17730002
177400            VALUE IS Z'sqlite3_open16'.                           17740000
177500* This function returns an integer which in COBOL                 17750009
177600* would be PIC S9(9) COMP-5. You should assign                    17760009
177700* the result to the variable using the RETURNING clause           17770009
177800* CALL SQLITE3 USING BY REFERENCE SQLITE3-OPEN16                  17780009
177900*      <other call parameters as documented>                      17790009
178000*      RETURNING integer-result-variable                          17800009
178100* END-CALL                                                        17810009
178200*                                                                 17820008
178300     05 SQLITE3-OS-END                  PIC X(15)                 17830002
178400            VALUE IS Z'sqlite3_os_end'.                           17840000
178500* This function returns an integer which in COBOL                 17850009
178600* would be PIC S9(9) COMP-5. You should assign                    17860009
178700* the result to the variable using the RETURNING clause           17870009
178800* CALL SQLITE3 USING BY REFERENCE SQLITE3-OS-END                  17880009
178900*      <other call parameters as documented>                      17890009
179000*      RETURNING integer-result-variable                          17900009
179100* END-CALL                                                        17910009
179200*                                                                 17920008
179300     05 SQLITE3-OS-INIT                 PIC X(16)                 17930002
179400            VALUE IS Z'sqlite3_os_init'.                          17940000
179500* This function returns an integer which in COBOL                 17950009
179600* would be PIC S9(9) COMP-5. You should assign                    17960009
179700* the result to the variable using the RETURNING clause           17970009
179800* CALL SQLITE3 USING BY REFERENCE SQLITE3-OS-INIT                 17980009
179900*      <other call parameters as documented>                      17990009
180000*      RETURNING integer-result-variable                          18000009
180100* END-CALL                                                        18010009
180200*                                                                 18020008
180300     05 SQLITE3-OVERLOAD-FUNCTION       PIC X(26)                 18030002
180400            VALUE IS Z'sqlite3_overload_function'.                18040000
180500* This function returns an integer which in COBOL                 18050009
180600* would be PIC S9(9) COMP-5. You should assign                    18060009
180700* the result to the variable using the RETURNING clause           18070009
180800* CALL SQLITE3 USING BY REFERENCE SQLITE3-OVERLOAD-FUNCTION       18080009
180900*      <other call parameters as documented>                      18090009
181000*      RETURNING integer-result-variable                          18100009
181100* END-CALL                                                        18110009
181200*                                                                 18120008
181300     05 SQLITE3-PREPARE                 PIC X(16)                 18130002
181400            VALUE IS Z'sqlite3_prepare'.                          18140000
181500* This function returns an integer which in COBOL                 18150009
181600* would be PIC S9(9) COMP-5. You should assign                    18160009
181700* the result to the variable using the RETURNING clause           18170009
181800* CALL SQLITE3 USING BY REFERENCE SQLITE3-PREPARE                 18180009
181900*      <other call parameters as documented>                      18190009
182000*      RETURNING integer-result-variable                          18200009
182100* END-CALL                                                        18210009
182200*                                                                 18220008
182300     05 SQLITE3-PREPARE-V2              PIC X(19)                 18230002
182400            VALUE IS Z'sqlite3_prepare_v2'.                       18240000
182500* This function returns an integer which in COBOL                 18250009
182600* would be PIC S9(9) COMP-5. You should assign                    18260009
182700* the result to the variable using the RETURNING clause           18270009
182800* CALL SQLITE3 USING BY REFERENCE SQLITE3-PREPARE-V2              18280009
182900*      <other call parameters as documented>                      18290009
183000*      RETURNING integer-result-variable                          18300009
183100* END-CALL                                                        18310009
183200*                                                                 18320008
183300     05 SQLITE3-PREPARE16               PIC X(18)                 18330002
183400            VALUE IS Z'sqlite3_prepare16'.                        18340000
183500* This function returns an integer which in COBOL                 18350009
183600* would be PIC S9(9) COMP-5. You should assign                    18360009
183700* the result to the variable using the RETURNING clause           18370009
183800* CALL SQLITE3 USING BY REFERENCE SQLITE3-PREPARE16               18380009
183900*      <other call parameters as documented>                      18390009
184000*      RETURNING integer-result-variable                          18400009
184100* END-CALL                                                        18410009
184200*                                                                 18420008
184300     05 SQLITE3-PREPARE16-V2            PIC X(21)                 18430002
184400            VALUE IS Z'sqlite3_prepare16_v2'.                     18440000
184500* This function returns an integer which in COBOL                 18450009
184600* would be PIC S9(9) COMP-5. You should assign                    18460009
184700* the result to the variable using the RETURNING clause           18470009
184800* CALL SQLITE3 USING BY REFERENCE SQLITE3-PREPARE16-V2            18480009
184900*      <other call parameters as documented>                      18490009
185000*      RETURNING integer-result-variable                          18500009
185100* END-CALL                                                        18510009
185200*                                                                 18520008
185300     05 SQLITE3-PROFILE                 PIC X(16)                 18530002
185400            VALUE IS Z'sqlite3_profile'.                          18540000
185500* This function returns a pointer which in COBOL                  18550009
185600* would be USAGE POINTER. You should assign                       18560009
185700* the result to the variable using the RETURNING clause           18570009
185800* CALL SQLITE3 USING BY REFERENCE SQLITE3-PROFILE                 18580009
185900*      <other call parameters as documented>                      18590009
186000*      RETURNING pointer-result-variable                          18600009
186100* END-CALL                                                        18610009
186200*                                                                 18620008
186300     05 SQLITE3-PROGRESS-HANDLER        PIC X(25)                 18630002
186400            VALUE IS Z'sqlite3_progress_handler'.                 18640000
186500* This function does not return anything at all.                  18650009
186600* CALL SQLITE3 USING BY REFERENCE SQLITE3-PROGRESS-HANDLER        18660009
186700*      <other call parameters as documented>                      18670009
186800* END-CALL                                                        18680009
186900*                                                                 18690008
187000     05 SQLITE3-RANDOMNESS              PIC X(19)                 18700002
187100            VALUE IS Z'sqlite3_randomness'.                       18710000
187200* This function returns an integer which in COBOL                 18720009
187300* would be PIC S9(9) COMP-5. You should assign                    18730009
187400* the result to the variable using the RETURNING clause           18740009
187500* CALL SQLITE3 USING BY REFERENCE SQLITE3-RANDOMNESS              18750009
187600*      <other call parameters as documented>                      18760009
187700*      RETURNING integer-result-variable                          18770009
187800* END-CALL                                                        18780009
187900*                                                                 18790008
188000     05 SQLITE3-REALLOC                 PIC X(16)                 18800002
188100            VALUE IS Z'sqlite3_realloc'.                          18810000
188200* This function returns a pointer which in COBOL                  18820009
188300* would be USAGE POINTER. You should assign                       18830009
188400* the result to the variable using the RETURNING clause           18840009
188500* CALL SQLITE3 USING BY REFERENCE SQLITE3-REALLOC                 18850009
188600*      <other call parameters as documented>                      18860009
188700*      RETURNING pointer-result-variable                          18870009
188800* END-CALL                                                        18880009
188900*                                                                 18890008
189000     05 SQLITE3-RELEASE-MEMORY          PIC X(23)                 18900002
189100            VALUE IS Z'sqlite3_release_memory'.                   18910000
189200* This function returns an integer which in COBOL                 18920009
189300* would be PIC S9(9) COMP-5. You should assign                    18930009
189400* the result to the variable using the RETURNING clause           18940009
189500* CALL SQLITE3 USING BY REFERENCE SQLITE3-RELEASE-MEMORY          18950009
189600*      <other call parameters as documented>                      18960009
189700*      RETURNING integer-result-variable                          18970009
189800* END-CALL                                                        18980009
189900*                                                                 18990008
190000     05 SQLITE3-RESET                   PIC X(14)                 19000002
190100            VALUE IS Z'sqlite3_reset'.                            19010000
190200* This function returns an integer which in COBOL                 19020009
190300* would be PIC S9(9) COMP-5. You should assign                    19030009
190400* the result to the variable using the RETURNING clause           19040009
190500* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESET                   19050009
190600*      <other call parameters as documented>                      19060009
190700*      RETURNING integer-result-variable                          19070009
190800* END-CALL                                                        19080009
190900*                                                                 19090008
191000     05 SQLITE3-RESET-AUTO-EXTENSION    PIC X(29)                 19100002
191100            VALUE IS Z'sqlite3_reset_auto_extension'.             19110000
191200* This function returns an integer which in COBOL                 19120009
191300* would be PIC S9(9) COMP-5. You should assign                    19130009
191400* the result to the variable using the RETURNING clause           19140009
191500* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESET-AUTO-EXTENSION    19150009
191600*      <other call parameters as documented>                      19160009
191700*      RETURNING integer-result-variable                          19170009
191800* END-CALL                                                        19180009
191900*                                                                 19190008
192000     05 SQLITE3-RESULT-BLOB             PIC X(20)                 19200002
192100            VALUE IS Z'sqlite3_result_blob'.                      19210000
192200* This function does not return anything at all.                  19220009
192300* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-BLOB             19230009
192400*      <other call parameters as documented>                      19240009
192500* END-CALL                                                        19250009
192600*                                                                 19260008
192700     05 SQLITE3-RESULT-DOUBLE           PIC X(22)                 19270002
192800            VALUE IS Z'sqlite3_result_double'.                    19280000
192900* This function does not return anything at all.                  19290009
193000* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-DOUBLE           19300009
193100*      <other call parameters as documented>                      19310009
193200* END-CALL                                                        19320009
193300*                                                                 19330008
193400     05 SQLITE3-RESULT-ERROR            PIC X(21)                 19340002
193500            VALUE IS Z'sqlite3_result_error'.                     19350000
193600* This function does not return anything at all.                  19360009
193700* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-ERROR            19370009
193800*      <other call parameters as documented>                      19380009
193900* END-CALL                                                        19390009
194000*                                                                 19400008
194100     05 SQLITE3-RESULT-ERROR-CODE       PIC X(26)                 19410002
194200            VALUE IS Z'sqlite3_result_error_code'.                19420000
194300* This function does not return anything at all.                  19430009
194400* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-ERROR-CODE       19440009
194500*      <other call parameters as documented>                      19450009
194600* END-CALL                                                        19460009
194700*                                                                 19470008
194800     05 SQLITE3-RESULT-ERROR-NOMEM      PIC X(27)                 19480002
194900            VALUE IS Z'sqlite3_result_error_nomem'.               19490000
195000* This function does not return anything at all.                  19500009
195100* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-ERROR-NOMEM      19510009
195200*      <other call parameters as documented>                      19520009
195300* END-CALL                                                        19530009
195400*                                                                 19540008
195500     05 SQLITE3-RESULT-ERROR-TOOBIG     PIC X(28)                 19550002
195600            VALUE IS Z'sqlite3_result_error_toobig'.              19560000
195700* This function does not return anything at all.                  19570009
195800* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-ERROR-TOOBIG     19580009
195900*      <other call parameters as documented>                      19590009
196000* END-CALL                                                        19600009
196100*                                                                 19610008
196200     05 SQLITE3-RESULT-ERROR16          PIC X(23)                 19620002
196300            VALUE IS Z'sqlite3_result_error16'.                   19630000
196400* This function does not return anything at all.                  19640009
196500* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-ERROR16          19650009
196600*      <other call parameters as documented>                      19660009
196700* END-CALL                                                        19670009
196800*                                                                 19680008
196900     05 SQLITE3-RESULT-INT              PIC X(19)                 19690002
197000            VALUE IS Z'sqlite3_result_int'.                       19700000
197100* This function does not return anything at all.                  19710009
197200* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-INT              19720009
197300*      <other call parameters as documented>                      19730009
197400* END-CALL                                                        19740009
197500*                                                                 19750008
197600     05 SQLITE3-RESULT-INT64            PIC X(21)                 19760002
197700            VALUE IS Z'sqlite3_result_int64'.                     19770000
197800* This function does not return anything at all.                  19780009
197900* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-INT64            19790009
198000*      <other call parameters as documented>                      19800009
198100* END-CALL                                                        19810009
198200*                                                                 19820008
198300     05 SQLITE3-RESULT-NULL             PIC X(20)                 19830002
198400            VALUE IS Z'sqlite3_result_null'.                      19840000
198500* This function does not return anything at all.                  19850009
198600* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-NULL             19860009
198700*      <other call parameters as documented>                      19870009
198800* END-CALL                                                        19880009
198900*                                                                 19890008
199000     05 SQLITE3-RESULT-TEXT             PIC X(20)                 19900002
199100            VALUE IS Z'sqlite3_result_text'.                      19910000
199200* This function does not return anything at all.                  19920009
199300* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-TEXT             19930009
199400*      <other call parameters as documented>                      19940009
199500* END-CALL                                                        19950009
199600*                                                                 19960008
199700     05 SQLITE3-RESULT-TEXT16           PIC X(22)                 19970002
199800            VALUE IS Z'sqlite3_result_text16'.                    19980000
199900* This function does not return anything at all.                  19990009
200000* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-TEXT16           20000009
200100*      <other call parameters as documented>                      20010009
200200* END-CALL                                                        20020009
200300*                                                                 20030008
200400     05 SQLITE3-RESULT-TEXT16BE         PIC X(24)                 20040002
200500            VALUE IS Z'sqlite3_result_text16be'.                  20050000
200600* This function does not return anything at all.                  20060009
200700* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-TEXT16BE         20070009
200800*      <other call parameters as documented>                      20080009
200900* END-CALL                                                        20090009
201000*                                                                 20100008
201100     05 SQLITE3-RESULT-TEXT16LE         PIC X(24)                 20110002
201200            VALUE IS Z'sqlite3_result_text16le'.                  20120000
201300* This function does not return anything at all.                  20130009
201400* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-TEXT16LE         20140009
201500*      <other call parameters as documented>                      20150009
201600* END-CALL                                                        20160009
201700*                                                                 20170008
201800     05 SQLITE3-RESULT-VALUE            PIC X(21)                 20180002
201900            VALUE IS Z'sqlite3_result_value'.                     20190000
202000* This function does not return anything at all.                  20200009
202100* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-VALUE            20210009
202200*      <other call parameters as documented>                      20220009
202300* END-CALL                                                        20230009
202400*                                                                 20240008
202500     05 SQLITE3-RESULT-ZEROBLOB         PIC X(24)                 20250002
202600            VALUE IS Z'sqlite3_result_zeroblob'.                  20260000
202700* This function does not return anything at all.                  20270009
202800* CALL SQLITE3 USING BY REFERENCE SQLITE3-RESULT-ZEROBLOB         20280009
202900*      <other call parameters as documented>                      20290009
203000* END-CALL                                                        20300009
203100*                                                                 20310008
203200     05 SQLITE3-ROLLBACK-HOOK           PIC X(22)                 20320002
203300            VALUE IS Z'sqlite3_rollback_hook'.                    20330000
203400* This function returns a pointer which in COBOL                  20340009
203500* would be USAGE POINTER. You should assign                       20350009
203600* the result to the variable using the RETURNING clause           20360009
203700* CALL SQLITE3 USING BY REFERENCE SQLITE3-ROLLBACK-HOOK           20370009
203800*      <other call parameters as documented>                      20380009
203900*      RETURNING pointer-result-variable                          20390009
204000* END-CALL                                                        20400009
204100*                                                                 20410008
204200     05 SQLITE3-RTREE-GEOMETRY-CALLBK   PIC X(32)                 20420003
204300            VALUE IS Z'sqlite3_rtree_geometry_callback'.          20430000
204400* This function returns an integer which in COBOL                 20440009
204500* would be PIC S9(9) COMP-5. You should assign                    20450009
204600* the result to the variable using the RETURNING clause           20460009
204700* CALL SQLITE3 USING BY REFERENCE SQLITE3-RTREE-GEOMETRY-CALLBK   20470009
204800*      <other call parameters as documented>                      20480009
204900*      RETURNING integer-result-variable                          20490009
205000* END-CALL                                                        20500009
205100*                                                                 20510008
205200     05 SQLITE3-SET-AUTHORIZER          PIC X(23)                 20520002
205300            VALUE IS Z'sqlite3_set_authorizer'.                   20530000
205400* This function returns an integer which in COBOL                 20540009
205500* would be PIC S9(9) COMP-5. You should assign                    20550009
205600* the result to the variable using the RETURNING clause           20560009
205700* CALL SQLITE3 USING BY REFERENCE SQLITE3-SET-AUTHORIZER          20570009
205800*      <other call parameters as documented>                      20580009
205900*      RETURNING integer-result-variable                          20590009
206000* END-CALL                                                        20600009
206100*                                                                 20610008
206200     05 SQLITE3-SET-AUXDATA             PIC X(20)                 20620002
206300            VALUE IS Z'sqlite3_set_auxdata'.                      20630000
206400* This function does not return anything at all.                  20640009
206500* CALL SQLITE3 USING BY REFERENCE SQLITE3-SET-AUXDATA             20650009
206600*      <other call parameters as documented>                      20660009
206700* END-CALL                                                        20670009
206800*                                                                 20680008
206900     05 SQLITE3-SHUTDOWN                PIC X(17)                 20690002
207000            VALUE IS Z'sqlite3_shutdown'.                         20700000
207100* This function returns an integer which in COBOL                 20710009
207200* would be PIC S9(9) COMP-5. You should assign                    20720009
207300* the result to the variable using the RETURNING clause           20730009
207400* CALL SQLITE3 USING BY REFERENCE SQLITE3-SHUTDOWN                20740009
207500*      <other call parameters as documented>                      20750009
207600*      RETURNING integer-result-variable                          20760009
207700* END-CALL                                                        20770009
207800*                                                                 20780008
207900     05 SQLITE3-SLEEP                   PIC X(14)                 20790002
208000            VALUE IS Z'sqlite3_sleep'.                            20800000
208100* This function returns an integer which in COBOL                 20810009
208200* would be PIC S9(9) COMP-5. You should assign                    20820009
208300* the result to the variable using the RETURNING clause           20830009
208400* CALL SQLITE3 USING BY REFERENCE SQLITE3-SLEEP                   20840009
208500*      <other call parameters as documented>                      20850009
208600*      RETURNING integer-result-variable                          20860009
208700* END-CALL                                                        20870009
208800*                                                                 20880008
208900     05 SQLITE3-SNPRINTF                PIC X(17)                 20890002
209000            VALUE IS Z'sqlite3_snprintf'.                         20900000
209100* This function returns a pointer which in COBOL                  20910009
209200* would be USAGE POINTER. You should assign                       20920009
209300* the result to the variable using the RETURNING clause           20930009
209400* CALL SQLITE3 USING BY REFERENCE SQLITE3-SNPRINTF                20940009
209500*      <other call parameters as documented>                      20950009
209600*      RETURNING pointer-result-variable                          20960009
209700* END-CALL                                                        20970009
209800*                                                                 20980008
209900     05 SQLITE3-SOFT-HEAP-LIMIT         PIC X(24)                 20990002
210000            VALUE IS Z'sqlite3_soft_heap_limit'.                  21000000
210100* This function does not return anything at all.                  21010009
210200* CALL SQLITE3 USING BY REFERENCE SQLITE3-SOFT-HEAP-LIMIT         21020009
210300*      <other call parameters as documented>                      21030009
210400* END-CALL                                                        21040009
210500*                                                                 21050008
210600     05 SQLITE3-SOFT-HEAP-LIMIT64       PIC X(26)                 21060002
210700            VALUE IS Z'sqlite3_soft_heap_limit64'.                21070000
210800* Data is returned as a PIC S9(18) COMP-5 in the second           21080009
210900* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  21090009
211000* inserted before the ones documented in the C API.               21100009
211100* This function returns a pointer which in COBOL                  21110009
211200* would be USAGE POINTER. You should assign                       21120009
211300* the result to the variable using the RETURNING clause           21130009
211400* CALL SQLITE3 USING BY REFERENCE SQLITE3-SOFT-HEAP-LIMIT64       21140009
211500*      BY REFERENCE int64-returned-value                          21150009
211600*      <other call parameters as documented>                      21160009
211700* END-CALL                                                        21170009
211800*                                                                 21180005
211900     05 SQLITE3-SOURCEID                PIC X(17)                 21190002
212000            VALUE IS Z'sqlite3_sourceid'.                         21200000
212100* This function returns a pointer which in COBOL                  21210009
212200* would be USAGE POINTER. You should assign                       21220009
212300* the result to the variable using the RETURNING clause           21230009
212400* CALL SQLITE3 USING BY REFERENCE SQLITE3-SOURCEID                21240009
212500*      <other call parameters as documented>                      21250009
212600*      RETURNING pointer-result-variable                          21260009
212700* END-CALL                                                        21270009
212800*                                                                 21280008
212900     05 SQLITE3-SQL                     PIC X(12)                 21290002
213000            VALUE IS Z'sqlite3_sql'.                              21300000
213100* This function returns a pointer which in COBOL                  21310009
213200* would be USAGE POINTER. You should assign                       21320009
213300* the result to the variable using the RETURNING clause           21330009
213400* CALL SQLITE3 USING BY REFERENCE SQLITE3-SQL                     21340009
213500*      <other call parameters as documented>                      21350009
213600*      RETURNING pointer-result-variable                          21360009
213700* END-CALL                                                        21370009
213800*                                                                 21380008
213900     05 SQLITE3-STATUS                  PIC X(15)                 21390002
214000            VALUE IS Z'sqlite3_status'.                           21400000
214100* This function returns an integer which in COBOL                 21410009
214200* would be PIC S9(9) COMP-5. You should assign                    21420009
214300* the result to the variable using the RETURNING clause           21430009
214400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STATUS                  21440009
214500*      <other call parameters as documented>                      21450009
214600*      RETURNING integer-result-variable                          21460009
214700* END-CALL                                                        21470009
214800*                                                                 21480008
214900     05 SQLITE3-STEP                    PIC X(13)                 21490002
215000            VALUE IS Z'sqlite3_step'.                             21500000
215100* This function returns an integer which in COBOL                 21510009
215200* would be PIC S9(9) COMP-5. You should assign                    21520009
215300* the result to the variable using the RETURNING clause           21530009
215400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STEP                    21540009
215500*      <other call parameters as documented>                      21550009
215600*      RETURNING integer-result-variable                          21560009
215700* END-CALL                                                        21570009
215800*                                                                 21580008
215900     05 SQLITE3-STMT-BUSY               PIC X(18)                 21590002
216000            VALUE IS Z'sqlite3_stmt_busy'.                        21600000
216100* This function returns an integer which in COBOL                 21610009
216200* would be PIC S9(9) COMP-5. You should assign                    21620009
216300* the result to the variable using the RETURNING clause           21630009
216400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STMT-BUSY               21640009
216500*      <other call parameters as documented>                      21650009
216600*      RETURNING integer-result-variable                          21660009
216700* END-CALL                                                        21670009
216800*                                                                 21680008
216900     05 SQLITE3-STMT-READONLY           PIC X(22)                 21690002
217000            VALUE IS Z'sqlite3_stmt_readonly'.                    21700000
217100* This function returns an integer which in COBOL                 21710009
217200* would be PIC S9(9) COMP-5. You should assign                    21720009
217300* the result to the variable using the RETURNING clause           21730009
217400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STMT-READONLY           21740009
217500*      <other call parameters as documented>                      21750009
217600*      RETURNING integer-result-variable                          21760009
217700* END-CALL                                                        21770009
217800*                                                                 21780008
217900     05 SQLITE3-STMT-STATUS             PIC X(20)                 21790002
218000            VALUE IS Z'sqlite3_stmt_status'.                      21800000
218100* This function returns an integer which in COBOL                 21810009
218200* would be PIC S9(9) COMP-5. You should assign                    21820009
218300* the result to the variable using the RETURNING clause           21830009
218400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STMT-STATUS             21840009
218500*      <other call parameters as documented>                      21850009
218600*      RETURNING integer-result-variable                          21860009
218700* END-CALL                                                        21870009
218800*                                                                 21880008
218900     05 SQLITE3-STRGLOB                 PIC X(16)                 21890002
219000            VALUE IS Z'sqlite3_strglob'.                          21900000
219100* This function returns an integer which in COBOL                 21910009
219200* would be PIC S9(9) COMP-5. You should assign                    21920009
219300* the result to the variable using the RETURNING clause           21930009
219400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STRGLOB                 21940009
219500*      <other call parameters as documented>                      21950009
219600*      RETURNING integer-result-variable                          21960009
219700* END-CALL                                                        21970009
219800*                                                                 21980008
219900     05 SQLITE3-STRICMP                 PIC X(16)                 21990002
220000            VALUE IS Z'sqlite3_stricmp'.                          22000000
220100* This function returns an integer which in COBOL                 22010009
220200* would be PIC S9(9) COMP-5. You should assign                    22020009
220300* the result to the variable using the RETURNING clause           22030009
220400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STRICMP                 22040009
220500*      <other call parameters as documented>                      22050009
220600*      RETURNING integer-result-variable                          22060009
220700* END-CALL                                                        22070009
220800*                                                                 22080008
220900     05 SQLITE3-STRNICMP                PIC X(17)                 22090002
221000            VALUE IS Z'sqlite3_strnicmp'.                         22100000
221100* This function returns an integer which in COBOL                 22110009
221200* would be PIC S9(9) COMP-5. You should assign                    22120009
221300* the result to the variable using the RETURNING clause           22130009
221400* CALL SQLITE3 USING BY REFERENCE SQLITE3-STRNICMP                22140009
221500*      <other call parameters as documented>                      22150009
221600*      RETURNING integer-result-variable                          22160009
221700* END-CALL                                                        22170009
221800*                                                                 22180008
221900     05 SQLITE3-TABLE-COLUMN-METADATA   PIC X(30)                 22190007
222000            VALUE IS Z'sqlite3_table_column_metadata'.            22200006
222100* This function returns an integer which in COBOL                 22210009
222200* would be PIC S9(9) COMP-5. You should assign                    22220009
222300* the result to the variable using the RETURNING clause           22230009
222400* CALL SQLITE3 USING BY REFERENCE SQLITE3-TABLE-COLUMN-METADATA   22240009
222500*      <other call parameters as documented>                      22250009
222600*      RETURNING integer-result-variable                          22260009
222700* END-CALL                                                        22270009
222800*                                                                 22280008
222900     05 SQLITE3-TEST-CONTROL            PIC X(21)                 22290002
223000            VALUE IS Z'sqlite3_test_control'.                     22300000
223100* This function returns an integer which in COBOL                 22310009
223200* would be PIC S9(9) COMP-5. You should assign                    22320009
223300* the result to the variable using the RETURNING clause           22330009
223400* CALL SQLITE3 USING BY REFERENCE SQLITE3-TEST-CONTROL            22340009
223500*      <other call parameters as documented>                      22350009
223600*      RETURNING integer-result-variable                          22360009
223700* END-CALL                                                        22370009
223800*                                                                 22380008
223900     05 SQLITE3-THREAD-CLEANUP          PIC X(23)                 22390002
224000            VALUE IS Z'sqlite3_thread_cleanup'.                   22400000
224100* This function does not return anything at all.                  22410009
224200* CALL SQLITE3 USING BY REFERENCE SQLITE3-THREAD-CLEANUP          22420009
224300*      <other call parameters as documented>                      22430009
224400* END-CALL                                                        22440009
224500*                                                                 22450008
224600     05 SQLITE3-THREADSAFE              PIC X(19)                 22460002
224700            VALUE IS Z'sqlite3_threadsafe'.                       22470000
224800* This function returns an integer which in COBOL                 22480009
224900* would be PIC S9(9) COMP-5. You should assign                    22490009
225000* the result to the variable using the RETURNING clause           22500009
225100* CALL SQLITE3 USING BY REFERENCE SQLITE3-THREADSAFE              22510009
225200*      <other call parameters as documented>                      22520009
225300*      RETURNING integer-result-variable                          22530009
225400* END-CALL                                                        22540009
225500*                                                                 22550008
225600     05 SQLITE3-TOTAL-CHANGES           PIC X(22)                 22560002
225700            VALUE IS Z'sqlite3_total_changes'.                    22570000
225800* This function returns an integer which in COBOL                 22580009
225900* would be PIC S9(9) COMP-5. You should assign                    22590009
226000* the result to the variable using the RETURNING clause           22600009
226100* CALL SQLITE3 USING BY REFERENCE SQLITE3-TOTAL-CHANGES           22610009
226200*      <other call parameters as documented>                      22620009
226300*      RETURNING integer-result-variable                          22630009
226400* END-CALL                                                        22640009
226500*                                                                 22650008
226600     05 SQLITE3-TRACE                   PIC X(14)                 22660002
226700            VALUE IS Z'sqlite3_trace'.                            22670000
226800* This function returns a pointer which in COBOL                  22680009
226900* would be USAGE POINTER. You should assign                       22690009
227000* the result to the variable using the RETURNING clause           22700009
227100* CALL SQLITE3 USING BY REFERENCE SQLITE3-TRACE                   22710009
227200*      <other call parameters as documented>                      22720009
227300*      RETURNING pointer-result-variable                          22730009
227400* END-CALL                                                        22740009
227500*                                                                 22750008
227600     05 SQLITE3-TRANSFER-BINDINGS       PIC X(26)                 22760002
227700            VALUE IS Z'sqlite3_transfer_bindings'.                22770000
227800* This function returns an integer which in COBOL                 22780009
227900* would be PIC S9(9) COMP-5. You should assign                    22790009
228000* the result to the variable using the RETURNING clause           22800009
228100* CALL SQLITE3 USING BY REFERENCE SQLITE3-TRANSFER-BINDINGS       22810009
228200*      <other call parameters as documented>                      22820009
228300*      RETURNING integer-result-variable                          22830009
228400* END-CALL                                                        22840009
228500*                                                                 22850008
228600     05 SQLITE3-UPDATE-HOOK             PIC X(20)                 22860002
228700            VALUE IS Z'sqlite3_update_hook'.                      22870000
228800* This function returns a pointer which in COBOL                  22880009
228900* would be USAGE POINTER. You should assign                       22890009
229000* the result to the variable using the RETURNING clause           22900009
229100* CALL SQLITE3 USING BY REFERENCE SQLITE3-UPDATE-HOOK             22910009
229200*      <other call parameters as documented>                      22920009
229300*      RETURNING pointer-result-variable                          22930009
229400* END-CALL                                                        22940009
229500*                                                                 22950008
229600     05 SQLITE3-URI-BOOLEAN             PIC X(20)                 22960002
229700            VALUE IS Z'sqlite3_uri_boolean'.                      22970000
229800* This function returns an integer which in COBOL                 22980009
229900* would be PIC S9(9) COMP-5. You should assign                    22990009
230000* the result to the variable using the RETURNING clause           23000009
230100* CALL SQLITE3 USING BY REFERENCE SQLITE3-URI-BOOLEAN             23010009
230200*      <other call parameters as documented>                      23020009
230300*      RETURNING integer-result-variable                          23030009
230400* END-CALL                                                        23040009
230500*                                                                 23050008
230600     05 SQLITE3-URI-INT64               PIC X(18)                 23060002
230700            VALUE IS Z'sqlite3_uri_int64'.                        23070000
230800* Data is returned as a PIC S9(18) COMP-5 in the second           23080009
230900* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  23090009
231000* inserted before the ones documented in the C API.               23100009
231100* This function returns a pointer which in COBOL                  23110009
231200* would be USAGE POINTER. You should assign                       23120009
231300* the result to the variable using the RETURNING clause           23130009
231400* CALL SQLITE3 USING BY REFERENCE SQLITE3-URI-INT64               23140009
231500*      BY REFERENCE int64-returned-value                          23150009
231600*      <other call parameters as documented>                      23160009
231700* END-CALL                                                        23170009
231800*                                                                 23180005
231900     05 SQLITE3-URI-PARAMETER           PIC X(22)                 23190002
232000            VALUE IS Z'sqlite3_uri_parameter'.                    23200000
232100* This function returns a pointer which in COBOL                  23210009
232200* would be USAGE POINTER. You should assign                       23220009
232300* the result to the variable using the RETURNING clause           23230009
232400* CALL SQLITE3 USING BY REFERENCE SQLITE3-URI-PARAMETER           23240009
232500*      <other call parameters as documented>                      23250009
232600*      RETURNING pointer-result-variable                          23260009
232700* END-CALL                                                        23270009
232800*                                                                 23280008
232900     05 SQLITE3-USER-DATA               PIC X(18)                 23290002
233000            VALUE IS Z'sqlite3_user_data'.                        23300000
233100* This function returns a pointer which in COBOL                  23310009
233200* would be USAGE POINTER. You should assign                       23320009
233300* the result to the variable using the RETURNING clause           23330009
233400* CALL SQLITE3 USING BY REFERENCE SQLITE3-USER-DATA               23340009
233500*      <other call parameters as documented>                      23350009
233600*      RETURNING pointer-result-variable                          23360009
233700* END-CALL                                                        23370009
233800*                                                                 23380008
233900     05 SQLITE3-VALUE-BLOB              PIC X(19)                 23390002
234000            VALUE IS Z'sqlite3_value_blob'.                       23400000
234100* This function returns a pointer which in COBOL                  23410009
234200* would be USAGE POINTER. You should assign                       23420009
234300* the result to the variable using the RETURNING clause           23430009
234400* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-BLOB              23440009
234500*      <other call parameters as documented>                      23450009
234600*      RETURNING pointer-result-variable                          23460009
234700* END-CALL                                                        23470009
234800*                                                                 23480008
234900     05 SQLITE3-VALUE-BYTES             PIC X(20)                 23490002
235000            VALUE IS Z'sqlite3_value_bytes'.                      23500000
235100* This function returns an integer which in COBOL                 23510009
235200* would be PIC S9(9) COMP-5. You should assign                    23520009
235300* the result to the variable using the RETURNING clause           23530009
235400* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-BYTES             23540009
235500*      <other call parameters as documented>                      23550009
235600*      RETURNING integer-result-variable                          23560009
235700* END-CALL                                                        23570009
235800*                                                                 23580008
235900     05 SQLITE3-VALUE-BYTES16           PIC X(22)                 23590002
236000            VALUE IS Z'sqlite3_value_bytes16'.                    23600000
236100* This function returns an integer which in COBOL                 23610009
236200* would be PIC S9(9) COMP-5. You should assign                    23620009
236300* the result to the variable using the RETURNING clause           23630009
236400* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-BYTES16           23640009
236500*      <other call parameters as documented>                      23650009
236600*      RETURNING integer-result-variable                          23660009
236700* END-CALL                                                        23670009
236800*                                                                 23680008
236900     05 SQLITE3-VALUE-DOUBLE            PIC X(21)                 23690002
237000            VALUE IS Z'sqlite3_value_double'.                     23700000
237100* Data is returned as a PIC S9(18) COMP-5 in the second           23710009
237200* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  23720009
237300* inserted before the ones documented in the C API.               23730009
237400* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-DOUBLE            23740009
237500*      BY REFERENCE double-result-variable                        23750009
237600*      <other call parameters as documented>                      23760009
237700* END-CALL                                                        23770009
237800* Before using this value, you must convert it from BFP to HFP.   23780009
237900* See the CONVERT-BFP-TO-HFP function below.                      23790009
238000*                                                                 23800005
238100     05 SQLITE3-VALUE-INT               PIC X(18)                 23810002
238200            VALUE IS Z'sqlite3_value_int'.                        23820000
238300* This function returns an integer which in COBOL                 23830009
238400* would be PIC S9(9) COMP-5. You should assign                    23840009
238500* the result to the variable using the RETURNING clause           23850009
238600* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-INT               23860009
238700*      <other call parameters as documented>                      23870009
238800*      RETURNING integer-result-variable                          23880009
238900* END-CALL                                                        23890009
239000*                                                                 23900008
239100     05 SQLITE3-VALUE-INT64             PIC X(20)                 23910002
239200            VALUE IS Z'sqlite3_value_int64'.                      23920000
239300* Data is returned as a PIC S9(18) COMP-5 in the second           23930009
239400* parameter passed, BY REFERENCE, to SQLITE3A. This parameter is  23940009
239500* inserted before the ones documented in the C API.               23950009
239600* This function returns a pointer which in COBOL                  23960009
239700* would be USAGE POINTER. You should assign                       23970009
239800* the result to the variable using the RETURNING clause           23980009
239900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-INT64             23990009
240000*      BY REFERENCE int64-returned-value                          24000009
240100*      <other call parameters as documented>                      24010009
240200* END-CALL                                                        24020009
240300*                                                                 24030005
240400     05 SQLITE3-VALUE-NUMERIC-TYPE      PIC X(27)                 24040002
240500            VALUE IS Z'sqlite3_value_numeric_type'.               24050000
240600* This function returns an integer which in COBOL                 24060009
240700* would be PIC S9(9) COMP-5. You should assign                    24070009
240800* the result to the variable using the RETURNING clause           24080009
240900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-NUMERIC-TYPE      24090009
241000*      <other call parameters as documented>                      24100009
241100*      RETURNING integer-result-variable                          24110009
241200* END-CALL                                                        24120009
241300*                                                                 24130008
241400     05 SQLITE3-VALUE-TEXT              PIC X(19)                 24140002
241500            VALUE IS Z'sqlite3_value_text'.                       24150000
241600* This function returns a pointer which in COBOL                  24160009
241700* would be USAGE POINTER. You should assign                       24170009
241800* the result to the variable using the RETURNING clause           24180009
241900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-TEXT              24190009
242000*      <other call parameters as documented>                      24200009
242100*      RETURNING pointer-result-variable                          24210009
242200* END-CALL                                                        24220009
242300*                                                                 24230008
242400     05 SQLITE3-VALUE-TEXT16            PIC X(21)                 24240002
242500            VALUE IS Z'sqlite3_value_text16'.                     24250000
242600* This function returns a pointer which in COBOL                  24260009
242700* would be USAGE POINTER. You should assign                       24270009
242800* the result to the variable using the RETURNING clause           24280009
242900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-TEXT16            24290009
243000*      <other call parameters as documented>                      24300009
243100*      RETURNING pointer-result-variable                          24310009
243200* END-CALL                                                        24320009
243300*                                                                 24330008
243400     05 SQLITE3-VALUE-TEXT16BE          PIC X(23)                 24340002
243500            VALUE IS Z'sqlite3_value_text16be'.                   24350000
243600* This function returns a pointer which in COBOL                  24360009
243700* would be USAGE POINTER. You should assign                       24370009
243800* the result to the variable using the RETURNING clause           24380009
243900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-TEXT16BE          24390009
244000*      <other call parameters as documented>                      24400009
244100*      RETURNING pointer-result-variable                          24410009
244200* END-CALL                                                        24420009
244300*                                                                 24430008
244400     05 SQLITE3-VALUE-TEXT16LE          PIC X(23)                 24440002
244500            VALUE IS Z'sqlite3_value_text16le'.                   24450000
244600* This function returns a pointer which in COBOL                  24460009
244700* would be USAGE POINTER. You should assign                       24470009
244800* the result to the variable using the RETURNING clause           24480009
244900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-TEXT16LE          24490009
245000*      <other call parameters as documented>                      24500009
245100*      RETURNING pointer-result-variable                          24510009
245200* END-CALL                                                        24520009
245300*                                                                 24530008
245400     05 SQLITE3-VALUE-TYPE              PIC X(19)                 24540002
245500            VALUE IS Z'sqlite3_value_type'.                       24550000
245600* This function returns an integer which in COBOL                 24560009
245700* would be PIC S9(9) COMP-5. You should assign                    24570009
245800* the result to the variable using the RETURNING clause           24580009
245900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VALUE-TYPE              24590009
246000*      <other call parameters as documented>                      24600009
246100*      RETURNING integer-result-variable                          24610009
246200* END-CALL                                                        24620009
246300*                                                                 24630008
246400     05 SQLITE3-VFS-FIND                PIC X(17)                 24640002
246500            VALUE IS Z'sqlite3_vfs_find'.                         24650000
246600* This function returns a pointer which in COBOL                  24660009
246700* would be USAGE POINTER. You should assign                       24670009
246800* the result to the variable using the RETURNING clause           24680009
246900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VFS-FIND                24690009
247000*      <other call parameters as documented>                      24700009
247100*      RETURNING pointer-result-variable                          24710009
247200* END-CALL                                                        24720009
247300*                                                                 24730008
247400     05 SQLITE3-VFS-REGISTER            PIC X(21)                 24740002
247500            VALUE IS Z'sqlite3_vfs_register'.                     24750000
247600* This function returns an integer which in COBOL                 24760009
247700* would be PIC S9(9) COMP-5. You should assign                    24770009
247800* the result to the variable using the RETURNING clause           24780009
247900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VFS-REGISTER            24790009
248000*      <other call parameters as documented>                      24800009
248100*      RETURNING integer-result-variable                          24810009
248200* END-CALL                                                        24820009
248300*                                                                 24830008
248400     05 SQLITE3-VFS-UNREGISTER          PIC X(23)                 24840002
248500            VALUE IS Z'sqlite3_vfs_unregister'.                   24850000
248600* This function returns an integer which in COBOL                 24860009
248700* would be PIC S9(9) COMP-5. You should assign                    24870009
248800* the result to the variable using the RETURNING clause           24880009
248900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VFS-UNREGISTER          24890009
249000*      <other call parameters as documented>                      24900009
249100*      RETURNING integer-result-variable                          24910009
249200* END-CALL                                                        24920009
249300*                                                                 24930008
249400     05 SQLITE3-VMPRINTF                PIC X(17)                 24940002
249500            VALUE IS Z'sqlite3_vmprintf'.                         24950000
249600* This function returns a pointer which in COBOL                  24960009
249700* would be USAGE POINTER. You should assign                       24970009
249800* the result to the variable using the RETURNING clause           24980009
249900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VMPRINTF                24990009
250000*      <other call parameters as documented>                      25000009
250100*      RETURNING pointer-result-variable                          25010009
250200* END-CALL                                                        25020009
250300*                                                                 25030008
250400     05 SQLITE3-VSNPRINTF               PIC X(18)                 25040002
250500            VALUE IS Z'sqlite3_vsnprintf'.                        25050000
250600* This function returns a pointer which in COBOL                  25060009
250700* would be USAGE POINTER. You should assign                       25070009
250800* the result to the variable using the RETURNING clause           25080009
250900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VSNPRINTF               25090009
251000*      <other call parameters as documented>                      25100009
251100*      RETURNING pointer-result-variable                          25110009
251200* END-CALL                                                        25120009
251300*                                                                 25130008
251400     05 SQLITE3-VTAB-CONFIG             PIC X(20)                 25140002
251500            VALUE IS Z'sqlite3_vtab_config'.                      25150000
251600* This function returns an integer which in COBOL                 25160009
251700* would be PIC S9(9) COMP-5. You should assign                    25170009
251800* the result to the variable using the RETURNING clause           25180009
251900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VTAB-CONFIG             25190009
252000*      <other call parameters as documented>                      25200009
252100*      RETURNING integer-result-variable                          25210009
252200* END-CALL                                                        25220009
252300*                                                                 25230008
252400     05 SQLITE3-VTAB-ON-CONFLICT        PIC X(25)                 25240002
252500            VALUE IS Z'sqlite3_vtab_on_conflict'.                 25250000
252600* This function returns an integer which in COBOL                 25260009
252700* would be PIC S9(9) COMP-5. You should assign                    25270009
252800* the result to the variable using the RETURNING clause           25280009
252900* CALL SQLITE3 USING BY REFERENCE SQLITE3-VTAB-ON-CONFLICT        25290009
253000*      <other call parameters as documented>                      25300009
253100*      RETURNING integer-result-variable                          25310009
253200* END-CALL                                                        25320009
253300*                                                                 25330008
253400     05 SQLITE3-WAL-AUTOCHECKPOINT      PIC X(27)                 25340002
253500            VALUE IS Z'sqlite3_wal_autocheckpoint'.               25350000
253600* This function returns an integer which in COBOL                 25360009
253700* would be PIC S9(9) COMP-5. You should assign                    25370009
253800* the result to the variable using the RETURNING clause           25380009
253900* CALL SQLITE3 USING BY REFERENCE SQLITE3-WAL-AUTOCHECKPOINT      25390009
254000*      <other call parameters as documented>                      25400009
254100*      RETURNING integer-result-variable                          25410009
254200* END-CALL                                                        25420009
254300*                                                                 25430008
254400     05 SQLITE3-WAL-CHECKPOINT          PIC X(23)                 25440002
254500            VALUE IS Z'sqlite3_wal_checkpoint'.                   25450000
254600* This function returns an integer which in COBOL                 25460009
254700* would be PIC S9(9) COMP-5. You should assign                    25470009
254800* the result to the variable using the RETURNING clause           25480009
254900* CALL SQLITE3 USING BY REFERENCE SQLITE3-WAL-CHECKPOINT          25490009
255000*      <other call parameters as documented>                      25500009
255100*      RETURNING integer-result-variable                          25510009
255200* END-CALL                                                        25520009
255300*                                                                 25530008
255400     05 SQLITE3-WAL-CHECKPOINT-V2       PIC X(26)                 25540002
255500            VALUE IS Z'sqlite3_wal_checkpoint_v2'.                25550000
255600* This function returns an integer which in COBOL                 25560009
255700* would be PIC S9(9) COMP-5. You should assign                    25570009
255800* the result to the variable using the RETURNING clause           25580009
255900* CALL SQLITE3 USING BY REFERENCE SQLITE3-WAL-CHECKPOINT-V2       25590009
256000*      <other call parameters as documented>                      25600009
256100*      RETURNING integer-result-variable                          25610009
256200* END-CALL                                                        25620009
256300*                                                                 25630008
256400     05 SQLITE3-WAL-HOOK                PIC X(17)                 25640002
256500            VALUE IS Z'sqlite3_wal_hook'.                         25650000
256600* This function returns a pointer which in COBOL                  25660009
256700* would be USAGE POINTER. You should assign                       25670009
256800* the result to the variable using the RETURNING clause           25680009
256900* CALL SQLITE3 USING BY REFERENCE SQLITE3-WAL-HOOK                25690009
257000*      <other call parameters as documented>                      25700009
257100*      RETURNING pointer-result-variable                          25710009
257200*                                                                 25720009
257300* END-CALL                                                        25730009
257400     05 CONVERT-BFP-TO-HFP              PIC X(19)                 25740009
257500            VALUE IS Z'convert_bfp_to_hfp'.                       25750009
257600* This function should be called after retrieving a double        25760009
257700* floating point result and before using it in COBOL.             25770009
257800* This is because COBOL uses HFP whereas sqlite stores in BFP.    25780009
257900* Data is returned as a PIC USAGE COMP-2 in the second            25790011
258000* parameter passed, BY REFERENCE, to SQLITE3A. This unusual       25800012
258100* calling sequence is used because it is how C works, and to be   25810012
258110* consistent with the other functions in this package.            25811012
258200* CALL SQLITE3 USING BY REFERENCE CONVERT-BFP-TO-HFP              25820009
258300*      BY REFERENCE double-result-variable                        25830012
258400*      <other call parameters as documented>                      25840009
258500* END-CALL                                                        25850009
258600*                                                                 25860009
258700     05 CONVERT-HFP-TO-BFP              PIC X(19)                 25870009
258800            VALUE IS Z'convert_hfp_to_bfp'.                       25880009
258900* This function should be called after using a COMP-2 in COBOL    25890009
259000* but before sending it to sqlite to store.                       25900009
259100* This is because COBOL uses HFP whereas sqlite stores in BFP.    25910009
259200* Data is returned as a PIC USAGE COMP-2 in the second            25920011
259210* parameter passed, BY REFERENCE, to SQLITE3A. This unusual       25921012
259220* calling sequence is used because it is how C works, and to be   25922012
259230* consistent with the other functions in this package.            25923012
259500* CALL SQLITE3 USING BY REFERENCE CONVERT-BFP-TO-HFP              25950009
259600*      BY REFERENCE double-result-variable                        25960013
259700*      <other call parameters as documented>                      25970009
259800* END-CALL                                                        25980009
259900*                                                                 25990009
