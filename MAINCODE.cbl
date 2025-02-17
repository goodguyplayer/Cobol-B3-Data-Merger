      ******************************************************************
      * Author: NATHAN BRITO DA SILVA
      * Date: 2025/02/17
      * Purpose: READ A GIVEN LIST OF FILES AND WRITE THE DATA IN ONE
      *        SINGULAR CSV
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCODE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DATAPATHS ASSIGN TO
               '.\..\DATA\FILEPATH.TXT'
               ORGANISATION        IS LINE SEQUENTIAL
               ACCESS MODE         IS SEQUENTIAL
               FILE STATUS         IS WS-FS-1.
       DATA DIVISION.
       FILE SECTION.
       FD DATAPATHS.
       01 FILE-PATH.
           03 FILE-PATH-INPUT          PIC X(128).
       WORKING-STORAGE SECTION.
       01 WS-FILE-PATH-FILES           PIC X(128).
       01 FILLER REDEFINES WS-FILE-PATH-FILES.
           03 WS-FILE-PATH-INPUT       PIC X(128).
       77 WS-START-TIME                PIC X(10)  VALUE SPACES.
       77 WS-END-TIME                  PIC X(10)  VALUE SPACES.
       77 WS-FILE-PATH-OUTPUT          PIC X(128) VALUE SPACES.
       77 WS-FS-1                      PIC 9(02).
           88 FS-OK                    VALUE 0.
       77 WS-EOF                       PIC X.
           88 EOF-OK                   VALUE 'S' FALSE 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-START THRU P100-END.
           PERFORM P200-START THRU P200-END.
           PERFORM P300-START THRU P300-END.
           PERFORM P999-EXIT.


      *>       INITIALISE VARIABLES
       P100-START.
            DISPLAY "PROGRAM START"
            MOVE ' ..\DATA\OUTPUTINPUT.TXT'
                 TO WS-FILE-PATH-OUTPUT
            SET EOF-OK TO FALSE
            SET FS-OK TO TRUE

            INITIALISE WS-START-TIME.
            INITIALISE WS-END-TIME.


            ACCEPT WS-START-TIME FROM TIME
            DISPLAY 'STARTING AT.: '
                   WS-START-TIME(01:02)
                   ':'
                   WS-START-TIME(03:02)
                   ':'
                   WS-START-TIME(05:06)


            .
       P100-END.


      *>       VALIDATE INPUT DATA FILE
       P200-START.
            OPEN INPUT DATAPATHS
            IF WS-FS-1 EQUAL 35 THEN
                DISPLAY 'FAILED TO LOAD INPUT DATA FILE, QUITTING...'
                PERFORM P999-EXIT
            END-IF
            CLOSE DATAPATHS
            .
       P200-END.


      *>       ITERATE LINES, PASS TO MODULE CALL
       P300-START.
            OPEN INPUT DATAPATHS
            PERFORM UNTIL EOF-OK
               IF FS-OK THEN
                   READ DATAPATHS INTO WS-FILE-PATH-FILES
                   AT END SET EOF-OK TO TRUE
                   NOT AT END
                       PERFORM P310-START THRU P310-END
                   END-READ
               ELSE
                   DISPLAY 'ERROR WHILE EXECUTING'
                   DISPLAY 'ERROR ' WS-FS-1
                   DISPLAY 'QUITTING...'
                   PERFORM P999-EXIT
               END-IF
            END-PERFORM
            .
       P300-END.


      *>       PASS LINE TO MODULE
       P310-START.
            DISPLAY WS-FILE-PATH-FILES
            CALL 'MODLRDWR' USING
                           WS-FILE-PATH-FILES
                           WS-FILE-PATH-OUTPUT
            .
       P310-END.



       P999-EXIT.

            CLOSE DATAPATHS
            ACCEPT WS-END-TIME FROM TIME
            DISPLAY '#################################################'
            DISPLAY ' CLOSING CODE.'
            DISPLAY 'START TIME.: '    WS-START-TIME(01:02)
                                       ':'
                                       WS-START-TIME(03:02)
                                       ':'
                                       WS-START-TIME(05:06)
            DISPLAY ' END  TIME.: '    WS-END-TIME(01:02)
                                       ':'
                                       WS-END-TIME(03:02)
                                       ':'
                                       WS-END-TIME(05:06)
            DISPLAY '#################################################'
            STOP RUN.
       END PROGRAM MAINCODE.
