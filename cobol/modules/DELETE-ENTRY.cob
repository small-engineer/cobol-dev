       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETE-ENTRY.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-ID       PIC 9(9).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       LINKAGE SECTION.
       01  L-ID        PIC 9(9).

       PROCEDURE DIVISION USING L-ID.

      *> cobol-lint CL002 main-logic
       MAIN-LOGIC.
           MOVE L-ID TO WS-ID

           CALL "DB-UTIL" USING BY CONTENT "CONNECT".

           EXEC SQL
               DELETE FROM journal_entry
               WHERE id = :WS-ID
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY "DELETE ERROR, SQLCODE=" SQLCODE
           END-IF

           CALL "DB-UTIL" USING BY CONTENT "DISCONNECT".
           GOBACK.

       END PROGRAM DELETE-ENTRY.
