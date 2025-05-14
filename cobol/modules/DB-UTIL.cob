       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB-UTIL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-CONN-STR     PIC X(256).
       01  WS-USER         PIC X(64).
       01  WS-PASS         PIC X(64).
       01  WS-DBNAME       PIC X(64).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       LINKAGE SECTION.
       01  L-CONNECT-FLAG  PIC X(10).

       PROCEDURE DIVISION USING L-CONNECT-FLAG.
      *> cobol-lint CL002 main-logic
       MAIN-LOGIC.
           IF L-CONNECT-FLAG = "CONNECT"
               PERFORM BUILD-CONN-STRING
               PERFORM CONNECT-DB
           ELSE
               PERFORM DISCONNECT-DB
           END-IF
           GOBACK.

       BUILD-CONN-STRING.
           ACCEPT WS-DBNAME FROM ENVIRONMENT "DB_NAME"
           ACCEPT WS-USER   FROM ENVIRONMENT "DB_USER"
           ACCEPT WS-PASS   FROM ENVIRONMENT "DB_PASS"

           STRING
               "DSN=PostgreSQL;"
               "DATABASE=" WS-DBNAME ";"
               "UID="      WS-USER   ";"
               "PWD="      WS-PASS   ";"
               INTO WS-CONN-STR
           END-STRING
           EXIT.

       CONNECT-DB.
           EXEC SQL
               CONNECT :WS-USER IDENTIFIED BY :WS-PASS
               USING :WS-DBNAME
           END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY "DB CONNECT ERROR, SQLCODE=" SQLCODE
               GOBACK
           END-IF
           EXIT.

       DISCONNECT-DB.
           EXEC SQL
               COMMIT WORK
           END-EXEC.
           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC.
           EXIT.

       END PROGRAM DB-UTIL.
