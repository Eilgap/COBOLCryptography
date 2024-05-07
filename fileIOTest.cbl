IDENTIFICATION DIVISION. 
PROGRAM-ID. fileIOTest. 
ENVIRONMENT DIVISION. 
DATA DIVISION. 
WORKING-STORAGE SECTION. 
    01 inFileName PIC X(12) VALUE 'in.txt'.
    01 outFileName PIC X(12) VALUE 'out.txt'.
    01 inFileHandle PIC X(4).
    01 inFileSize PIC X(8) comp-x VALUE ZERO .
    01 inOffset PIC X(8) comp-x VALUE ZERO. 
    01 outFileHandle PIC X(4).
    01 bufferSize  PIC 9(1) VALUE 8.
    01 outBuffer PIC X(8) VALUE '0'.
    01 inBuffer PIC X(8) VALUE '0'.
    01 buffer PIC X(8) VALUE '0'.
    01 initPerTest PIC X(64).
PROCEDURE DIVISION.
CONTROLFLOW SECTION.
      PERFORM str.
      PERFORM getByteCount.
      PERFORM getBytes.
      *>PERFORM initialPermutation.
      *>PERFORM writeBytes.
      *>PERFORM fin.
      
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

getByteCount SECTION.
    call "CBL_READ_FILE" using     inFileHandle
                                   inOffset
                                   0
                                   128
                                   inBuffer.
    move inOffset to inFileSize.
    DISPLAY inFileSize.   
    EXIT.

getBytes SECTION.
    call "CBL_READ_FILE" using     inFileHandle
                                   inOffset
                                   0
                                   0
                                   inBuffer.
    *>move inBuffer to buffer.
    DISPLAY inBuffer.   
    EXIT.

initialPermutation SECTION.
    call "BIT-OF" using            inBuffer.

fin SECTION.
    EXIT PROGRAM.
    STOP RUN.  
END PROGRAM fileIOTest.
