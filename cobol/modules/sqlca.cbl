       01 SQLCA.
          05 SQLCODE      PIC S9(9) COMP.
          05 SQLERRM.
             49 SQLERRML  PIC S9(4) COMP.
             49 SQLERRMC  PIC X(70).
          05 SQLERRP      PIC X(8).
          05 SQLERRD      PIC S9(9) COMP OCCURS 6.
          05 SQLWARN.
             10 SQLWARN0  PIC X.
             10 SQLWARN1  PIC X.
             10 SQLWARN2  PIC X.
             10 SQLWARN3  PIC X.
             10 SQLWARN4  PIC X.
             10 SQLWARN5  PIC X.
             10 SQLWARN6  PIC X.
             10 SQLWARN7  PIC X.
          05 SQLSTATE     PIC X(5).
