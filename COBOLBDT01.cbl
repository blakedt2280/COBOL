       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBBDT01.
       DATE-WRITTEN.   12/03/19.
       AUTHOR.         BLAKE TURNER.
       DATE-COMPILED.
      *THIS PROGRAM READS A FILE AND CREATES A PAINT COST REPORT.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 

           SELECT PAINT-EST
               ASSIGN TO 'C:\COBOL\PAINTEST.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOL\PJOBES.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD      PAINT-EST
               LABEL RECORD IS STANDARD
               DATA RECORD IS PAINT-REC
               RECORD CONTAINS 55 CHARACTERS.

       01      PAINT-REC.
               05  PAINT-EST-NO        PIC X(4).
               05  PAINT-DATE.
                   10  PAINT-YY        PIC 9(4).
                   10  PAINT-MM        PIC 99.
                   10  PAINT-DD        PIC 99.
               05  PAINT-WALL-SQ-FT    PIC 9(4).
               05  PAINT-DOOR-SQ-FT    PIC 9(3).
               05  PAINT-PRICE-GAL     PIC 99V99.

       FD      PRTOUT
               LABEL RECORD IS OMITTED
               RECORD CONTAINS 132 CHARACTERS
               DATA RECORD IS PRTLINE
               LINAGE IS 60 WITH FOOTING AT 56.

       01      PRTLINE                 PIC X(132).

       WORKING-STORAGE SECTION.
       01      WORK-AREA.
         05 PCTR                       PIC 99      VALUE 0.
         05 MORE-RECS                  PIC XXX     VALUE 'YES'.
         05 C-ECTR                     PIC 999     VALUE 0.
         05 C-PAINT-WALL-SQ-FT         PIC 9999V9.
         05 C-GALLONS                  PIC 99999V99.
         05 C-PAINT-COST               PIC 99999V99.
         05 C-LABOR-COST               PIC 99999V99.
         05 C-TOTAL-COST               PIC 999999V99.
         05 C-GT-GALLONS               PIC 99999V99.
         05 C-GT-PAINT-COST            PIC 99999999V99.
         05 C-GT-LABOR                 PIC 99999V99.
         05 C-GT-TTL                   PIC 999999999V99.
         






       01 CURRENT-DATE-AND-TIME.
         05    I-DATE.
               10  I-YY                PIC 9(4).
               10  I-MM                PIC 99.
               10  I-DD                PIC 99.

         05    I-TIME                  PIC X(11).

       01 COMPANY-TITLE.
           05 FILLER                   PIC X(6)    VALUE 'DATE:'.
           05 0-MM                     PIC 99.
           05 FILLER                   PIC X       VALUE '/'.
           05 0-DD                     PIC 99.
           05 FILLER                   PIC X       VALUE '/'.
           05 0-YY                     PIC 9(4).
           05 FILLER                   PIC X(37)   VALUE SPACES.
           05 FILLER                   PIC X(24)   VALUE 
           'WILSON''S PAINT ESTIMATOR'.
           05 FILLER                   PIC X(47)   VALUE SPACES.
           05 FILLER                   PIC X(6)    VALUE "PAGE:".
           05 0-PCTR                   PIC Z9.

       01 COL-HDG1.
         05 FILLER                     PIC X(8)    VALUE "ESTIMATE".
         05 FILLER                     PIC X(23)   VALUE SPACES.    
         05 FILLER                     PIC X(4)    VALUE "WALL".
         05 FILLER                     PIC X(7)    VALUE SPACES. 
         05 FILLER                     PIC X(4)    VALUE "DOOR".
         05 FILLER                     PIC X(6)    VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "TOTAL".
         05 FILLER                     PIC X(6)    VALUE SPACES. 
         05 FILLER                     PIC X(7)    VALUE "GALLONS".
         05 FILLER                     PIC X(6)    VALUE SPACES. 
         05 FILLER                     PIC X(6)    VALUE "PRICE/".
         05 FILLER                     PIC X(11)   VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "PAINT".
         05 FILLER                     PIC X(12)   VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "LABOR".
         05 FILLER                     PIC X(12)   VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "TOTAL".

       01 COL-HDG2.
         05 FILLER                     PIC X       VALUE SPACES.
         05 FILLER                     PIC X(6)    VALUE "NUMBER".
         05 FILLER                     PIC X(5)    VALUE SPACES. 
         05 FILLER                     PIC X(13)   VALUE 
         "ESTIMATE DATE".
         05 FILLER                     PIC X(5)    VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "SQ/FT".
         05 FILLER                     PIC X(6)    VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "SQ/FT".
         05 FILLER                     PIC X(6)    VALUE SPACES. 
         05 FILLER                     PIC X(5)    VALUE "SQ/FT".
         05 FILLER                     PIC X(7)    VALUE SPACES. 
         05 FILLER                     PIC X(6)    VALUE "NEEDED".
         05 FILLER                     PIC X(6)    VALUE SPACES.
         05 FILLER                     PIC X(6)    VALUE "GALLON".
         05 FILLER                     PIC X(8)    VALUE SPACES.
         05 FILLER                     PIC X(8)    VALUE "ESTIMATE".
         05 FILLER                     PIC X(9)    VALUE SPACES.
         05 FILLER                     PIC X(8)    VALUE "ESTIMATE".
         05 FILLER                     PIC X(9)    VALUE SPACES.
         05 FILLER                     PIC X(8)    VALUE "ESTIMATE".

       01 DETAIL-LINE.
         05 FILLER                     PIC XX      VALUE SPACES.
         05 0-PAINT-EST-NO             PIC X(11).
         05 0-PAINT-DATE.
               10  0-PAINT-MM                PIC 99.
               10  0-PAINT-DD                PIC 99.
               10  0-PAINT-YY                PIC 9(4).

         05 FILLER                     PIC X(9)    VALUE SPACES.
         05 0-PAINT-WALL-SQ-FT         PIC 9(12).
         05 0-PAINT-DOOR-SQ-FT         PIC 9(10).
         05 0-PAINT-TTL-SQ-FT          PIC 9(12).
         05 0-GAL-NEEDED               PIC 9(13).
         05 0-PAINT-PRICE-GAL          PIC 9(11).
         05 0-PAINT-EST                PIC 9(17).
         05 0-LBR-EST                  PIC 9(16).
         05 0-TTL-EST                  PIC 9(11).

       01 TTL-LINE. 
        05 FILLER                      PIC X(34)     VALUE 
        "GRAND TOTALS:".
        05 FILLER                      PIC X(17)     VALUE 
        "TOTAL ESTIMATES:".
        05 0-ECTR                      PIC 9(3).
        05 FILLER                      PIC X(7)      VALUE SPACES.
        05 0-TTL-GAL                   PIC 9(9).
        05 FILLER                      PIC X(15)     VALUE SPACES.
        05 0-TTL-PAINT-EST             PIC 9(13).
        05 FILLER                      PIC X(4)      VALUE SPACES.
        05 0-TTL-LBR-EST               PIC 9(13).
        05 FILLER                      PIC X(3)      VALUE SPACES.
        05 0-GT-EST                    PIC 9(14).

       

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT PAINT-EST.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.

           MOVE I-YY TO 0-YY.
           MOVE I-MM TO 0-MM.
           MOVE I-DD TO 0-DD.

           PERFORM 9000-READ.
           PERFORM 9100-HDGS. 

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           ADD 1 TO C-ECTR.
           SUBTRACT PAINT-DOOR-SQ-FT FROM PAINT-WALL-SQ-FT GIVING 
           C-PAINT-WALL-SQ-FT.
           COMPUTE C-GALLONS ROUNDED = C-PAINT-WALL-SQ-FT/115. 
           COMPUTE C-PAINT-COST = C-GALLONS * PAINT-PRICE-GAL.
           COMPUTE C-LABOR-COST = 23.55 * (C-GALLONS * 3).
           ADD C-PAINT-COST TO C-LABOR-COST GIVING C-TOTAL-COST.
           ADD C-GALLONS TO C-GT-GALLONS.
           ADD C-PAINT-COST TO C-GT-PAINT-COST.
           ADD C-LABOR-COST TO C-GT-LABOR.
           ADD C-TOTAL-COST TO C-GT-TTL.

       2200-OUTPUT.
           MOVE PAINT-EST-NO TO 0-PAINT-EST-NO.
           MOVE PAINT-DATE TO 0-PAINT-DATE.
           MOVE C-PAINT-WALL-SQ-FT TO 0-PAINT-WALL-SQ-FT.
           MOVE PAINT-DOOR-SQ-FT TO 0-PAINT-DOOR-SQ-FT.
           MOVE PAINT-WALL-SQ-FT TO 0-PAINT-TTL-SQ-FT.
           MOVE C-GALLONS TO 0-GAL-NEEDED.
           MOVE C-PAINT-COST TO 0-PAINT-PRICE-GAL.
           MOVE C-PAINT-COST TO 0-PAINT-EST.
           MOVE C-LABOR-COST TO 0-LBR-EST.
           MOVE C-TOTAL-COST TO 0-TTL-EST.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9100-HDGS.

       3000-CLOSING.
           PERFORM 9500-GRAND-TOTALS.
           CLOSE PAINT-EST.

       9000-READ.
           READ PAINT-EST
               AT END
                   MOVE 'NO' TO MORE-RECS.

       9100-HDGS.
           ADD 1 TO PCTR.
           MOVE PCTR TO 0-PCTR.
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COL-HDG1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COL-HDG2
               AFTER ADVANCING 1 LINE.

       9500-GRAND-TOTALS.
           MOVE C-ECTR TO 0-ECTR.
           MOVE C-GT-GALLONS TO 0-TTL-GAL.
           MOVE C-GT-PAINT-COST TO 0-TTL-PAINT-EST.
           MOVE C-GT-LABOR TO 0-TTL-LBR-EST.
           MOVE C-GT-TTL TO 0-GT-EST.

           WRITE PRTLINE FROM TTL-LINE. 
           




















