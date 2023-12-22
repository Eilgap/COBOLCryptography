IDENTIFICATION DIVISION. 
PROGRAM-ID. fileIOTest. 
ENVIRONMENT DIVISION. 
DATA DIVISION. 
WORKING-STORAGE SECTION. 
    01 inFileName PIC X(12) VALUE 'test.txt'.
    01 outFileName PIC X(12) VALUE 'out'.
    01 inFileHandle PIC X(4).
    01 inOffset PIC X(8) comp-x VALUE ZERO. 
    01 outFileHandle PIC X(4).
    01 bufferSize  PIC 9(3) VALUE 256.
    *>01 outBuffer PIC X(256) VALUE 'This is hopefully some data that will be written to a new file'.
    01 inBuffer PIC X(256).
PROCEDURE DIVISION.
CONTROLFLOW SECTION.
      PERFORM str.
      PERFORM getBytes.
      PERFORM writeBytes.
      PERFORM fin.
str SECTION.
    
    call "CBL_OPEN_FILE" using     inFileName
                                   1
                                   0
                                   0
                                   inFileHandle.

    call "CBL_CREATE_FILE" using   outFileName
                                   3
                                   0
                                   0
                                   outFileHandle.
                                   
    EXIT.
writeBytes SECTION.
    call "CBL_WRITE_FILE" using    outFileHandle
                                   0
                                   bufferSize
                                   0
                                   inBuffer.
    EXIT.

getBytes SECTION.
    call "CBL_READ_FILE" using     inFileHandle
                                   inOffset
                                   30
                                   128
                                   inBuffer.
    DISPLAY "Check out.txt to see if it worked" inOffset
    *>DISPLAY inBuffer   
    EXIT.

fin SECTION.
    EXIT PROGRAM.
    STOP RUN.  
END PROGRAM fileIOTest.
