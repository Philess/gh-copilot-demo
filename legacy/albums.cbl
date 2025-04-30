$ SET SOURCEFORMAT"FREE"

IDENTIFICATION DIVISION.
PROGRAM-ID.  MonthTable.
AUTHOR.  Michael Coughlan.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT AlbumFile ASSIGN TO "ALBUMS.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD AlbumFile.
01 AlbumDetails.
   88  EndOfAlbumFile  VALUE HIGH-VALUES.
   02  AlbumId       PIC 9(7).
   02  AlbumName.
       03 Artist      PIC X(8).
       03 Title       PIC X(20).
   02  ReleaseDate.
       03 YORelease   PIC 9(4).
       03 MORelease   PIC 9(2).
       03 DORelease   PIC 9(2).
   02  Genre          PIC X(10).

WORKING-STORAGE SECTION.
01 MonthTable.
   02 TableValues.
      03 FILLER       PIC X(18) VALUE "January  February".
      03 FILLER       PIC X(18) VALUE "March    April".
      03 FILLER       PIC X(18) VALUE "May      June".
      03 FILLER       PIC X(18) VALUE "July     August".
      03 FILLER       PIC X(18) VALUE "SeptemberOctober".
      03 FILLER       PIC X(18) VALUE "November December".
   02 FILLER REDEFINES TableValues.
      03 Month OCCURS 12 TIMES PIC X(9).

01 MonthCount OCCURS 12 TIMES PIC 999 VALUE ZEROS.
01 MonthIdx           PIC 999.
01 HeadingLine          PIC X(19) VALUE " Month    AlbumCount".

PROCEDURE DIVISION.
    OPEN INPUT AlbumFile.
    PERFORM UNTIL EndOfAlbumFile
        READ AlbumFile INTO AlbumDetails.
        AT END SET EndOfAlbumFile TO TRUE.
        IF NOT EndOfAlbumFile
            COMPUTE MonthIdx = MORelease.
            ADD 1 TO MonthCount(MonthIdx).
        END-IF
    END-PERFORM

    DISPLAY HeadingLine.
    PERFORM VARYING MonthIdx FROM 1 BY 1 UNTIL MonthIdx > 12
        DISPLAY MonthTable(MonthIdx) " " MonthCount(MonthIdx).
    END-PERFORM

    CLOSE AlbumFile.
    STOP RUN.