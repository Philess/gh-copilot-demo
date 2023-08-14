$ SET SOURCEFORMAT"FREE"  * Set the source format to free format

IDENTIFICATION DIVISION.  * Start of the identification division
PROGRAM-ID.  MonthTable.  * Program name
AUTHOR.  Michael Coughlan.  * Author name

* This program counts the number of albums released in each month and displays the result.

ENVIRONMENT DIVISION.  * Start of the environment division
INPUT-OUTPUT SECTION.  * Input-output section
FILE-CONTROL.  * File control section
    SELECT AlbumFile ASSIGN TO "ALBUMS.DAT"  * Assign the file "ALBUMS.DAT" to AlbumFile
        ORGANIZATION IS LINE SEQUENTIAL.  * The file is organized as a sequence of lines

DATA DIVISION.  * Start of the data division
FILE SECTION.  * File section
FD AlbumFile.  * File description for AlbumFile
01 AlbumDetails.  * Record description for AlbumDetails
   88  EndOfAlbumFile  VALUE HIGH-VALUES.  * End of file marker
   02  AlbumId       PIC 9(7).  * Album ID
   02  AlbumName.  * Album name
       03 Artist      PIC X(8).  * Artist name
       03 Title       PIC X(20).  * Album title
   02  ReleaseDate.  * Release date
       03 YORelease   PIC 9(4).  * Year of release
       03 MORelease   PIC 9(2).  * Month of release
       03 DORelease   PIC 9(2).  * Day of release
   02  Genre          PIC X(10).  * Genre

WORKING-STORAGE SECTION.  * Start of the working-storage section
01 MonthTable.  * Table to store the month names
   02 TableValues.  * Values for the table
      03 FILLER       PIC X(18) VALUE "January  February".  * January and February
      03 FILLER       PIC X(18) VALUE "March    April".  * March and April
      03 FILLER       PIC X(18) VALUE "May      June".  * May and June
      03 FILLER       PIC X(18) VALUE "July     August".  * July and August
      03 FILLER       PIC X(18) VALUE "SeptemberOctober".  * September and October
      03 FILLER       PIC X(18) VALUE "November December".  * November and December
   02 FILLER REDEFINES TableValues.  * Redefine the table values
      03 Month OCCURS 12 TIMES PIC X(9).  * Array to store the month names

01 MonthCount OCCURS 12 TIMES PIC 999 VALUE ZEROS.  * Array to store the count of albums released in each month

01 MonthIdx           PIC 999.  * Index for the month array

01 HeadingLine          PIC X(19) VALUE " Month    AlbumCount".  * Heading for the output table

PROCEDURE DIVISION.  * Start of the procedure division
    OPEN INPUT AlbumFile.  * Open the input file
    PERFORM UNTIL EndOfAlbumFile  * Loop until end of file
        READ AlbumFile INTO AlbumDetails.  * Read a record from the file
        AT END SET EndOfAlbumFile TO TRUE.  * If end of file, set EndOfAlbumFile to true
        IF NOT EndOfAlbumFile  * If not end of file
            COMPUTE MonthIdx = MORelease.  * Compute the index for the month array
            ADD 1 TO MonthCount(MonthIdx).  * Increment the count for the month
        END-IF
    END-PERFORM

    DISPLAY HeadingLine.  * Display the heading for the output table
    PERFORM VARYING MonthIdx FROM 1 BY 1 UNTIL MonthIdx > 12  * Loop through the month array
        DISPLAY MonthTable(MonthIdx) " " MonthCount(MonthIdx).  * Display the month name and count
    END-PERFORM

    CLOSE AlbumFile.  * Close the input file
    STOP RUN.  * End the program