       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-LINE   PIC X(512).
       01  WS-ACTION       PIC X(20).
       01  WS-ACT-TRIM     PIC X(20).
       01  WS-P1           PIC X(100).
       01  WS-P2           PIC X(100).
       01  WS-P3           PIC 9(20).
       01  WS-P4           PIC X(100).
       01  WS-P5           PIC X(100).

       PROCEDURE DIVISION.
      *> cobol-lint CL002 main-logic
       MAIN-LOGIC.
           ACCEPT WS-INPUT-LINE.
           UNSTRING WS-INPUT-LINE
               DELIMITED BY "|"
               INTO WS-ACTION
                    WS-P1 WS-P2 WS-P3 WS-P4 WS-P5.

           MOVE FUNCTION TRIM(WS-ACTION TRAILING) TO WS-ACT-TRIM

           EVALUATE TRUE
             WHEN WS-ACT-TRIM = "CREATE-ENTRY"
                 CALL "CREATE-ENTRY"
                      USING WS-P1 WS-P2 WS-P3 WS-P4 WS-P5
             WHEN WS-ACT-TRIM = "READ-ENTRIES"
                 CALL "READ-ENTRIES"
                      USING WS-P1 WS-P2
             WHEN WS-ACT-TRIM = "UPDATE-ENTRY"
                 CALL "UPDATE-ENTRY"
                      USING WS-P1 WS-P2 WS-P3 WS-P4 WS-P5
             WHEN WS-ACT-TRIM = "DELETE-ENTRY"
                 CALL "DELETE-ENTRY"
                      USING WS-P1
             WHEN OTHER
                 DISPLAY "UNKNOWN ACTION: " WS-ACT-TRIM
           END-EVALUATE

           STOP RUN.

       END PROGRAM MAIN.
