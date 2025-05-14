       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-ENTRY.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           01  WS-DATE     PIC X(10).
           01  WS-DEBIT    PIC X(50).
           01  WS-CREDIT   PIC X(50).
           01  WS-AMOUNT   PIC 9(9).
           01  WS-MEMO     PIC X(100).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       LINKAGE SECTION.
           01  L-DATE     PIC X(10).
           01  L-DEBIT    PIC X(50).
           01  L-CREDIT   PIC X(50).
           01  L-AMOUNT   PIC 9(9).
           01  L-MEMO     PIC X(100).

       PROCEDURE DIVISION
           USING BY REFERENCE
               L-DATE
               L-DEBIT
               L-CREDIT
               L-AMOUNT
               L-MEMO.

       MAIN-LOGIC.
           MOVE L-DATE   TO WS-DATE
           MOVE L-DEBIT  TO WS-DEBIT
           MOVE L-CREDIT TO WS-CREDIT
           MOVE L-AMOUNT TO WS-AMOUNT
           MOVE L-MEMO   TO WS-MEMO

           CALL "DB-UTIL" USING BY CONTENT "CONNECT".

           EXEC SQL
               INSERT INTO journal_entry
                   (entry_date,
                    debit_account,
                    credit_account,
                    amount,
                    memo)
               VALUES
                   (:WS-DATE,
                    :WS-DEBIT,
                    :WS-CREDIT,
                    :WS-AMOUNT,
                    :WS-MEMO)
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY "CREATE ERROR, SQLCODE=" SQLCODE
           END-IF

           CALL "DB-UTIL" USING BY CONTENT "DISCONNECT".
           GOBACK.

       END PROGRAM CREATE-ENTRY.
