      ******************************************************************
      * Author:
      * Date: 2025/01/30
      * Purpose: LIST / READ DATA FROM HISTORICAL QUOTATION FILE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HISTLIST.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT HISTDATA ASSIGN TO
      *>          INSERT PATH HERE
      *>          NEED TO FIND A WAY TO REMOVE THE HEADER AND FOOTER
      *>          '..\..\DATA\COTAHIST.A1986.TXT'
      *>          '..\..\DATA\COTAHIST-NH.A1986.TXT'
               '..\..\DATA\DEMO-NH.TXT'
               ORGANISATION    IS LINE SEQUENTIAL
               ACCESS MODE     IS SEQUENTIAL
      *>          RECORD KEY      IS HIST-TIPREG
               FILE STATUS     IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD HISTDATA.
           COPY HISTQUOT.
       WORKING-STORAGE SECTION.
       01 WS-REGISTER              PIC X(246).
       01 FILLER REDEFINES WS-REGISTER.
           03 WS-HIST-TIPREG       PIC 9(02).
           03 WS-HIST-DATEEX       PIC 9(08).
           03 WS-HIST-CODBDI       PIC X(02).
           03 WS-HIST-CODNEG       PIC X(12).
           03 WS-HIST-TPMERC       PIC 9(03).
           03 WS-HIST-NOMRES       PIC X(12).
           03 WS-HIST-ESPECI       PIC X(10).
           03 WS-HIST-PRAZOT       PIC X(03).
           03 WS-HIST-MODREF       PIC X(04).
           03 WS-HIST-PREABE       PIC 9(11)V99.
           03 WS-HIST-PREMAX       PIC 9(11)V99.
           03 WS-HIST-PREMIN       PIC 9(11)V99.
           03 WS-HIST-PREMED       PIC 9(11)V99.
           03 WS-HIST-PREULT       PIC 9(11)V99.
           03 WS-HIST-PREOFC       PIC 9(11)V99.
           03 WS-HIST-PREOFV       PIC 9(11)V99.
           03 WS-HIST-TOTNEG       PIC 9(05).
           03 WS-HIST-QUATOT       PIC 9(18).
           03 WS-HIST-VOLTOT       PIC 9(16)V99.
           03 WS-HIST-PREEXE       PIC 9(11)V99.
           03 WS-HIST-INDOPC       PIC X(01).
           03 WS-HIST-DATVEN       PIC 9(08).
           03 WS-HIST-FATCOT       PIC 9(07).
           03 WS-HIST-PTOEXE       PIC X(13).
           03 WS-HIST-CODISI       PIC X(12).
           03 WS-HIST-DISMES       PIC 9(03).
       77 WS-FS                    PIC 9(02).
           88 FS-OK                VALUE 0.
       77 WS-EOF                   PIC X.
           88 EOF-OK               VALUE 'S' FALSE 'N'.
       77 WS-COUNT                 PIC 9(03) VALUE ZEROES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'FILE READER'
            PERFORM P100-START THRU P100-END.
            PERFORM P200-START THRU P200-END.
            PERFORM P300-START THRU P300-END.
            PERFORM P999-EXIT.

      *>  INITIALIZER
       P100-START.
            SET EOF-OK TO FALSE
            SET FS-OK TO TRUE
            SET WS-COUNT TO 0
            .
       P100-END.

      *>  FILE VALIDATE
       P200-START.
            OPEN INPUT HISTDATA

            IF WS-FS EQUAL 35 THEN
                DISPLAY 'FAILED TO LOAD DATA FILE, QUITTING...'
                PERFORM P999-EXIT
            END-IF
            CLOSE HISTDATA
           .
       P200-END.

      *>  LINE READING
       P300-START.
            OPEN INPUT HISTDATA
            PERFORM UNTIL EOF-OK
               IF FS-OK THEN
                   READ HISTDATA INTO WS-REGISTER
                       AT END SET EOF-OK TO TRUE
                       NOT AT END
                           PERFORM P310-START THRU P310-END
                   END-READ
               ELSE
                   DISPLAY 'ERROR WHILE READING FILE'
                   DISPLAY 'ERROR CODE.: ' WS-FS
                   PERFORM P999-EXIT
               END-IF
            END-PERFORM

            .
       P300-END.

      *>  DATA DISPLAY
       P310-START.
            DISPLAY WS-REGISTER
            ADD 1 TO WS-COUNT
            DISPLAY '##################################################'
            DISPLAY '# COUNT.: ' WS-COUNT'                            #'
            DISPLAY '# TIPREG.: ' WS-HIST-TIPREG'                     #'
            DISPLAY '# DATEEX.: ' WS-HIST-DATEEX'                     #'
            DISPLAY '# DATEEX.: ' WS-HIST-DATEEX'                     #'
            DISPLAY '# CODBDI.: ' WS-HIST-CODBDI'                     #'
            DISPLAY '# CODNEG.: ' WS-HIST-CODNEG'                     #'
            DISPLAY '# TPMERC.: ' WS-HIST-TPMERC'                     #'
            DISPLAY '# NOMRES.: ' WS-HIST-NOMRES'                     #'
            DISPLAY '# ESPECI.: ' WS-HIST-ESPECI'                     #'
            DISPLAY '# PRAZOT.: ' WS-HIST-PRAZOT'                     #'
            DISPLAY '# MODREF.: ' WS-HIST-MODREF'                     #'
            DISPLAY '# PREABE.: ' WS-HIST-PREABE'                     #'
            DISPLAY '# PREMAX.: ' WS-HIST-PREMAX'                     #'
            DISPLAY '# PREMIN.: ' WS-HIST-PREMIN'                     #'
            DISPLAY '# PREMED.: ' WS-HIST-PREMED'                     #'
            DISPLAY '# PREULT.: ' WS-HIST-PREULT'                     #'
            DISPLAY '# PREOFC.: ' WS-HIST-PREOFC'                     #'
            DISPLAY '# PREOFV.: ' WS-HIST-PREOFV'                     #'
            DISPLAY '# TOTNEG.: ' WS-HIST-TOTNEG'                     #'
            DISPLAY '# QUATOT.: ' WS-HIST-QUATOT'                     #'
            DISPLAY '# VOLTOT.: ' WS-HIST-VOLTOT'                     #'
            DISPLAY '# PREEXE.: ' WS-HIST-PREEXE'                     #'
            DISPLAY '# INDOPC.: ' WS-HIST-INDOPC'                     #'
            DISPLAY '# DATVEN.: ' WS-HIST-DATVEN'                     #'
            DISPLAY '# FATCOT.: ' WS-HIST-FATCOT'                     #'
            DISPLAY '# PTOEXE.: ' WS-HIST-PTOEXE'                     #'
            DISPLAY '# CODISI.: ' WS-HIST-CODISI'                     #'
            DISPLAY '# DISMES.: ' WS-HIST-DISMES'                     #'
            DISPLAY '##################################################'
            .
       P310-END.


       P999-EXIT.
            CLOSE HISTDATA.
            STOP RUN.
       END PROGRAM HISTLIST.
