      ******************************************************************
      * Author: NATHAN BRITO DA SILVA
      * Date: 2025/02/17
      * Purpose: READ A GIVEN LIST OF FILES AND WRITE THE DATA IN ONE
      *        SINGULAR CSV
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCODE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-FILE-PATH-INPUT           PIC X(128) VALUE SPACES.
       77 WS-FILE-PATH-OUTPUT          PIC X(128) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "PROGRAM START"


            MOVE ' ...\DATA\DemoCotacoesHistoricas12022003.TXT'
                   TO WS-FILE-PATH-INPUT

            MOVE ' ...\DATA\OUTPUTINPUT.TXT'
                   TO WS-FILE-PATH-OUTPUT

            CALL 'MODLRDWR' USING
                           WS-FILE-PATH-INPUT
                           WS-FILE-PATH-OUTPUT
            STOP RUN.
       END PROGRAM MAINCODE.
