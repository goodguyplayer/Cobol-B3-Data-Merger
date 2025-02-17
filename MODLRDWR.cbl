      ******************************************************************
      * Author: NATHAN BRITO DA SILVA
      * Date: 2025/02/17
      * Purpose: READ A FILE WITH THE GIVEN FILEPATH AND WRITE THE DATA
      *        TO A SINGULAR CSV
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODLRDWR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT INPUTDATA ASSIGN TO
                   LS-FILE-PATH-INPUT
               ORGANISATION        IS LINE SEQUENTIAL
               ACCESS MODE         IS SEQUENTIAL
               FILE STATUS         IS WS-FS-1.
               SELECT OUTPUTDATA ASSIGN TO
                   LS-FILE-PATH-OUTPUT
               ORGANISATION        IS SEQUENTIAL
               ACCESS MODE         IS SEQUENTIAL
               FILE STATUS         IS WS-FS-2.
       DATA DIVISION.
       FILE SECTION.
       FD INPUTDATA.
           COPY HISTQUOT.
       FD OUTPUTDATA.
           COPY HISTOUTR.
       WORKING-STORAGE SECTION.
       01 WS-REGISTER              PIC X(245).
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
       77 WS-FILE-PATH                     PIC X(128).
       77 WS-TM-1                          PIC 99.
       77 WS-FS-1                          PIC 9(02).
           88 FS-OK                        VALUE 0.
       77 WS-FS-2                          PIC 9(02).
           88 FS-OK                        VALUE 0.
       77 WS-EOF                   PIC X.
           88 EOF-OK               VALUE 'S' FALSE 'N'.
       LINKAGE SECTION.
       01 PARAMETRES.
           02 LS-RETURN PIC 99 VALUE 0.
           02 LS-FILE-PATH-INPUT           PIC X(128).
           02 LS-FILE-PATH-OUTPUT          PIC X(128).
       PROCEDURE DIVISION USING PARAMETRES.
       MAIN-PROCEDURE.

           PERFORM P100-START THRU P100-END.
           PERFORM P200-START THRU P200-END.
           PERFORM P300-START THRU P300-END.
           PERFORM P999-EXIT.


      *>      MODULE INITIALIZER
       P100-START.
            DISPLAY 'RUNNING PROGRAM WITH FILE AT ' LS-FILE-PATH-INPUT
            INITIALISE WS-FILE-PATH
            INITIALISE WS-TM-1
            INITIALISE WS-FS-1
            INITIALISE HISTOUTR
                       WITH FILLER
                       REPLACING ALPHANUMERIC BY ','
            .
       P100-END.


      *>      FILE VALIDATE
       P200-START.
            DISPLAY '#####################'
            DISPLAY '#  TESTING FILE...  #'
            DISPLAY '#####################'
            PERFORM P210-START THRU P210-END
            PERFORM P220-START THRU P220-END
            .
       P200-END.


      *>      VALIDATE INPUT FILE
       P210-START.
            OPEN INPUT INPUTDATA
            IF WS-FS-1 EQUAL 35 THEN
               DISPLAY 'FAILED TO LOAD INPUT FILE, QUITTING...'
               MOVE 1 TO LS-RETURN
               PERFORM P999-EXIT
            END-IF
            DISPLAY 'INPUT DATA LOADED. PROCEEDING...'
            CLOSE INPUTDATA
            .
       P210-END.


      *>      VALIDATE/CREATE OUTPUT FILE
       P220-START.
            OPEN EXTEND OUTPUTDATA
            IF WS-FS-2 EQUAL 35 THEN
                DISPLAY 'FAILED TO LOAD OUTPUT FILE, CREATING NEW...'
                OPEN OUTPUT OUTPUTDATA
            END-IF
            DISPLAY 'OUTPUT FILE LOADED/CREATED. PROCEEDING...'
            CLOSE OUTPUTDATA
            .
       P220-END.


      *>       FILE READING AND OUTPUTTING
       P300-START.
            DISPLAY '###############################'
            DISPLAY '#  READING AND OUTPUTTING...  #'
            DISPLAY '###############################'
            PERFORM P310-START THRU P310-END
            .
       P300-END.


      *>       READING LINES FROM INPUTFILE
       P310-START.
            OPEN INPUT INPUTDATA
            PERFORM UNTIL EOF-OK
               IF FS-OK OF WS-FS-1 THEN
                   READ INPUTDATA INTO WS-REGISTER
                   AT END SET EOF-OK TO TRUE
                   NOT AT END
                       PERFORM P320-START THRU P320-END
                   END-READ
               ELSE
                   DISPLAY 'ERROR WHILE READING/STORING DATA'
                   DISPLAY 'ERROR CODE FS-1.: ' WS-FS-1
                   DISPLAY 'ERROR CODE FS-2.: ' WS-FS-2
                   PERFORM P999-EXIT
               END-IF
            END-PERFORM
            .
       P310-END.


      *>      REGISTRY VALIDATION
       P320-START.
            IF WS-HIST-TIPREG = 01 THEN
                PERFORM P330-START THRU P330-END
            END-IF.
       P320-END.


      *>       CONVERT REGISTRY AND WRITE IT
       P330-START.
            OPEN EXTEND OUTPUTDATA
               MOVE WS-HIST-TIPREG TO HIST-TIPREG OF OUTPUTDATA
               MOVE WS-HIST-DATEEX TO HIST-DATEEX OF OUTPUTDATA
               MOVE WS-HIST-CODBDI TO HIST-CODBDI OF OUTPUTDATA
               MOVE WS-HIST-CODNEG TO HIST-CODNEG OF OUTPUTDATA
               MOVE WS-HIST-TPMERC TO HIST-TPMERC OF OUTPUTDATA
               MOVE WS-HIST-NOMRES TO HIST-NOMRES OF OUTPUTDATA
               MOVE WS-HIST-ESPECI TO HIST-ESPECI OF OUTPUTDATA
               MOVE WS-HIST-PRAZOT TO HIST-PRAZOT OF OUTPUTDATA
               MOVE WS-HIST-MODREF TO HIST-MODREF OF OUTPUTDATA
               MOVE WS-HIST-PREABE TO HIST-PREABE OF OUTPUTDATA
               MOVE WS-HIST-PREMAX TO HIST-PREMAX OF OUTPUTDATA
               MOVE WS-HIST-PREMIN TO HIST-PREMIN OF OUTPUTDATA
               MOVE WS-HIST-PREMED TO HIST-PREMED OF OUTPUTDATA
               MOVE WS-HIST-PREULT TO HIST-PREULT OF OUTPUTDATA
               MOVE WS-HIST-PREOFC TO HIST-PREOFC OF OUTPUTDATA
               MOVE WS-HIST-PREOFV TO HIST-PREOFV OF OUTPUTDATA
               MOVE WS-HIST-TOTNEG TO HIST-TOTNEG OF OUTPUTDATA
               MOVE WS-HIST-QUATOT TO HIST-QUATOT OF OUTPUTDATA
               MOVE WS-HIST-VOLTOT TO HIST-VOLTOT OF OUTPUTDATA
               MOVE WS-HIST-PREEXE TO HIST-PREEXE OF OUTPUTDATA
               MOVE WS-HIST-INDOPC TO HIST-INDOPC OF OUTPUTDATA
               MOVE WS-HIST-DATVEN TO HIST-DATVEN OF OUTPUTDATA
               MOVE WS-HIST-FATCOT TO HIST-FATCOT OF OUTPUTDATA
               MOVE WS-HIST-PTOEXE TO HIST-PTOEXE OF OUTPUTDATA
               MOVE WS-HIST-CODISI TO HIST-CODISI OF OUTPUTDATA
               MOVE WS-HIST-DISMES TO HIST-DISMES OF OUTPUTDATA
               WRITE HISTOUTR OF OUTPUTDATA BEFORE ADVANCING 1 LINE
            CLOSE OUTPUTDATA
            .
       P330-END.


       P999-EXIT.
            CLOSE INPUTDATA
            CLOSE OUTPUTDATA
            GOBACK.
       END PROGRAM MODLRDWR.
