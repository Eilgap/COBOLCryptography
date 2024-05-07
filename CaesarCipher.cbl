IDENTIFICATION DIVISION.
       PROGRAM-ID.  CaesarEncrypt.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO "encryptedMonologue.txt" *>Set this to your input file
               ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OutputFile ASSIGN TO "decryptedMonologue.txt" *>Set this to your output file
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputLine    PIC X(100) VALUE ''.
       FD OutputFile.
       01 OutputLine   PIC X(100) VALUE ''.
       WORKING-STORAGE SECTION.
       01 EncOrDecMode PIC 9(1) VALUE 1. *>Set this to 0 for encryption, 1 for decryption
       01 ShiftAmount PIC S9(2) VALUE 3. *>Set this to the amount you'd like to shift by
       01 Counter PIC 9(2) VALUE 1.
       01 CurrentLetter PIC X(1).
       01 CurrentValue PIC S9(2) VALUE ZERO.
       01 TempValue PIC S9(2) VALUE ZERO.
       01 FirstSpaceFlag PIC X(1) VALUE 'F'.
       01 SecondSpaceFlag PIC X(1) VALUE 'F'.
       01 END-OF-FILE PIC X VALUE SPACE.
          88 END-OF-INPUT-FILE VALUE 'T'.
          88 NOT-END-OF-INPUT-FILE VALUE 'F'.

       PROCEDURE DIVISION.
       MainProgram SECTION.
         OPEN INPUT InputFile
         OPEN OUTPUT OutputFile

         SET NOT-END-OF-INPUT-FILE TO TRUE

         PERFORM UNTIL END-OF-INPUT-FILE
           READ InputFile
             NOT AT END
               IF EncOrDecMode = 0
                 PERFORM EncryptLettersInLine UNTIL SecondSpaceFlag = 'T'
               END-IF
               IF EncOrDecMode = 1
                 PERFORM DecryptLettersInLine UNTIL SecondSpaceFlag = 'T'
               END-IF
               MOVE 'F' to FirstSpaceFlag
               MOVE 'F' to SecondSpaceFlag
               WRITE OutputLine FROM InputLine
               SET Counter TO 1
             AT END
                SET END-OF-INPUT-FILE TO TRUE
           END-READ
         END-PERFORM
         CLOSE InputFile
         CLOSE OutputFile
         STOP RUN.

       EncryptLettersInLine SECTION.
         MOVE InputLine(Counter:1) TO CurrentLetter
         IF CurrentLetter = ' ' THEN
           IF FirstSpaceFlag = 'T' THEN
               MOVE 'T' TO SecondSpaceFlag
           ELSE
               MOVE 'T' TO FirstSpaceFlag
           END-IF
         ELSE
           MOVE 'F' TO FirstSpaceFlag
           PERFORM LetterToValue
           PERFORM AddShift
           PERFORM ValueToLetter
           MOVE CurrentLetter TO InputLine(Counter:1)
         END-IF
         ADD 1 TO Counter
         EXIT.

       DecryptLettersInLine SECTION.
         MOVE InputLine(Counter:1) TO CurrentLetter
         SET CurrentValue TO ZERO
         IF CurrentLetter = ' ' THEN
           IF FirstSpaceFlag = 'T' THEN
               MOVE 'T' TO SecondSpaceFlag
           ELSE
               MOVE 'T' TO FirstSpaceFlag
           END-IF
         ELSE
           MOVE 'F' TO FirstSpaceFlag
           PERFORM LetterToValue
           PERFORM SubShift
           PERFORM ValueToLetter
           MOVE CurrentLetter TO InputLine(Counter:1)
         END-IF
         ADD 1 TO Counter
         EXIT.

       LetterToValue SECTION.
         SET CurrentValue TO 0
         IF CurrentLetter = 'A' OR CurrentLetter = 'a'
           MOVE 1 TO CurrentValue
         END-IF
         IF CurrentLetter = 'B' OR CurrentLetter = 'b'
           MOVE 2 TO CurrentValue
         END-IF
         IF CurrentLetter = 'C' OR CurrentLetter = 'c'
           MOVE 3 TO CurrentValue
         END-IF
         IF CurrentLetter = 'D' OR CurrentLetter = 'd'
           MOVE 4 TO CurrentValue
         END-IF
         IF CurrentLetter = 'E' OR CurrentLetter = 'e'
           MOVE 5 TO CurrentValue
         END-IF
         IF CurrentLetter = 'F' OR CurrentLetter = 'f'
           MOVE 6 TO CurrentValue
         END-IF
         IF CurrentLetter = 'G' OR CurrentLetter = 'g'
           MOVE 7 TO CurrentValue
         END-IF
         IF CurrentLetter = 'H' OR CurrentLetter = 'h'
           MOVE 8 TO CurrentValue
         END-IF
         IF CurrentLetter = 'I' OR CurrentLetter = 'i'
           MOVE 9 TO CurrentValue
         END-IF
         IF CurrentLetter = 'J' OR CurrentLetter = 'j'
           MOVE 10 TO CurrentValue
         END-IF
         IF CurrentLetter = 'K' OR CurrentLetter = 'k'
           MOVE 11 TO CurrentValue
         END-IF
         IF CurrentLetter = 'L' OR CurrentLetter = 'l'
           MOVE 12 TO CurrentValue
         END-IF
         IF CurrentLetter = 'M' OR CurrentLetter = 'm'
           MOVE 13 TO CurrentValue
         END-IF
         IF CurrentLetter = 'N' OR CurrentLetter = 'n'
           MOVE 14 TO CurrentValue
         END-IF
         IF CurrentLetter = 'O' OR CurrentLetter = 'o'
           MOVE 15 TO CurrentValue
         END-IF
         IF CurrentLetter = 'P' OR CurrentLetter = 'p'
           MOVE 16 TO CurrentValue
         END-IF
         IF CurrentLetter = 'Q' OR CurrentLetter = 'q'
           MOVE 17 TO CurrentValue
         END-IF
         IF CurrentLetter = 'R' OR CurrentLetter = 'r'
           MOVE 18 TO CurrentValue
         END-IF
         IF CurrentLetter = 'S' OR CurrentLetter = 's'
           MOVE 19 TO CurrentValue
         END-IF
         IF CurrentLetter = 'T' OR CurrentLetter = 't'
           MOVE 20 TO CurrentValue
         END-IF
         IF CurrentLetter = 'U' OR CurrentLetter = 'u'
           MOVE 21 TO CurrentValue
         END-IF
         IF CurrentLetter = 'V' OR CurrentLetter = 'v'
           MOVE 22 TO CurrentValue
         END-IF
         IF CurrentLetter = 'W' OR CurrentLetter = 'w'
           MOVE 23 TO CurrentValue
         END-IF
         IF CurrentLetter = 'X' OR CurrentLetter = 'x'
           MOVE 24 TO CurrentValue
         END-IF
         IF CurrentLetter = 'Y' OR CurrentLetter = 'y'
           MOVE 25 TO CurrentValue
         END-IF
         IF CurrentLetter = 'Z' OR CurrentLetter = 'z'
           MOVE 26 TO CurrentValue
         END-IF
         EXIT.

       ValueToLetter SECTION.
         IF CurrentValue = 1
           MOVE 'A' TO CurrentLetter
         END-IF
         IF CurrentValue = 2
           Move 'B' TO CurrentLetter
         END-IF
         IF CurrentValue = 3
           MOVE 'C' TO CurrentLetter
         END-IF
         IF CurrentValue = 4
           Move 'D' TO CurrentLetter
         END-IF
         IF CurrentValue = 5
           MOVE 'E' TO CurrentLetter
         END-IF
         IF CurrentValue = 6
           Move 'F' TO CurrentLetter
         END-IF
         IF CurrentValue = 7
           MOVE 'G' TO CurrentLetter
         END-IF
         IF CurrentValue = 8
           Move 'H' TO CurrentLetter
         END-IF
         IF CurrentValue = 9
           MOVE 'I' TO CurrentLetter
         END-IF
         IF CurrentValue = 10
           Move 'J' TO CurrentLetter
         END-IF
         IF CurrentValue = 11
           MOVE 'K' TO CurrentLetter
         END-IF
         IF CurrentValue = 12
           Move 'L' TO CurrentLetter
         END-IF
         IF CurrentValue = 13
           MOVE 'M' TO CurrentLetter
         END-IF
         IF CurrentValue = 14
           Move 'N' TO CurrentLetter
         END-IF
         IF CurrentValue = 15
           MOVE 'O' TO CurrentLetter
         END-IF
         IF CurrentValue = 16
           Move 'P' TO CurrentLetter
         END-IF
         IF CurrentValue = 17
           Move 'Q' TO CurrentLetter
         END-IF
         IF CurrentValue = 18
           MOVE 'R' TO CurrentLetter
         END-IF
         IF CurrentValue = 19
           Move 'S' TO CurrentLetter
         END-IF
         IF CurrentValue = 20
           MOVE 'T' TO CurrentLetter
         END-IF
         IF CurrentValue = 21
           Move 'U' TO CurrentLetter
         END-IF
         IF CurrentValue = 22
           MOVE 'V' TO CurrentLetter
         END-IF
         IF CurrentValue = 23
           Move 'W' TO CurrentLetter
         END-IF
         IF CurrentValue = 24
           Move 'X' TO CurrentLetter
         END-IF
         IF CurrentValue = 25
           MOVE 'Y' TO CurrentLetter
         END-IF
         IF CurrentValue = 26
           Move 'Z' TO CurrentLetter
         END-IF
         EXIT.

       AddShift SECTION.
         IF CurrentValue IS NOT ZERO
           ADD ShiftAmount TO CurrentValue
           IF CurrentValue > 26
             SUBTRACT 26 FROM CurrentValue
           END-IF
         END-IF
         EXIT.

       SubShift SECTION.
         IF CurrentValue IS NOT ZERO
           SUBTRACT ShiftAmount FROM CurrentValue
           IF CurrentValue < 0
             ADD 26 TO CurrentValue
           END-IF
         END-IF
         EXIT.