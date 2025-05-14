       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-ENTRIES.

       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-START-DATE   PIC X(10).
       01  WS-END-DATE     PIC X(10).
       01  WS-ENTRY-ID     PIC 9(9).
       01  WS-ENTRY-DATE   PIC X(10).
       01  WS-DEBIT        PIC X(50).
       01  WS-CREDIT       PIC X(50).
       01  WS-AMOUNT       PIC 9(9).
       01  WS-MEMO         PIC X(100).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       LINKAGE SECTION.
       01  L-START-DATE    PIC X(10).
       01  L-END-DATE      PIC X(10).

       PROCEDURE DIVISION USING L-START-DATE L-END-DATE.

       MAIN-LOGIC.
           MOVE L-START-DATE TO WS-START-DATE
           MOVE L-END-DATE   TO WS-END-DATE

           CALL "DB-UTIL" USING BY CONTENT "CONNECT".

           EXEC SQL
               DECLARE C1 CURSOR FOR
               SELECT id, entry_date, debit_account, credit_account, amount, memo
                 FROM journal_entry
                WHERE entry_date BETWEEN :WS-START-DATE AND :WS-END-DATE
                ORDER BY entry_date
           END-EXEC

           EXEC SQL OPEN C1 END-EXEC

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH C1 INTO
                       :WS-ENTRY-ID,
                       :WS-ENTRY-DATE,
                       :WS-DEBIT,
                       :WS-CREDIT,
                       :WS-AMOUNT,
                       :WS-MEMO
               END-EXEC
               IF SQLCODE = 0 THEN
                   DISPLAY
                       "{""id"":" WS-ENTRY-ID
                       ",""date"":""" WS-ENTRY-DATE
                       """,""debit"":""" WS-DEBIT
                       """,""credit"":""" WS-CREDIT
                       """,""amount"":" WS-AMOUNT
                       ",""memo"":""" WS-MEMO
                       """}"
               END-IF
           END-PERFORM

           EXEC SQL CLOSE C1 END-EXEC

           CALL "DB-UTIL" USING BY CONTENT "DISCONNECT".

           GOBACK.

       END PROGRAM READ-ENTRIES.
