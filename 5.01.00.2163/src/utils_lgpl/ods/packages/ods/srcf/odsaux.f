* @begin@
*
* odsaux.f   -  Auxiliary FORTRAN ODS routines
*
* Copyright (C) 1998 Delft Hydraulics
*
*
* General information:
* This file contains the following routines:
* - ODS_CHECK_NEFIS : Check whether the NEFIS files exist
*
* $Author: Markus $
* $Date: 11/15/00 3:51p $
* $Source: /u/cvsroot/gpp/libsrc/ods/odsaux.f,v $
*
* @@------------------------------------------------------------------
*   Subroutine: ODS_CHECK_NEFIS
*   Author:     Arjen Markus
*   Purpose:    Check whether the NEFIS files exist
*   Context:    Used by TRISULA, MORSYS and PHIDIAS routines
*   Pseudo Code:
*               If the file names are incomplete (no .def file),
*               construct the proper filenames
*               Check if both files exist
*               If everything appears okay, return OK, otherwise
*               return an error code
*   Note:
*               Previously the code below was copied numerous times
*               into each individual routine
* --------------------------------------------------------------------
*
      SUBROUTINE ODS_CHECK_NEFIS
     &                  ( FILNAM , DEFEXT , IERROR )
* --------------------------------------------------------------------
*
* Arguments:
* Name    Type    Dimensions   I/O
* FILNAM  CH*256  3            I/O  name of the grid files:
*                                   1. NEFIS data file (extension: .dat)
*                                   2. NEFIS definition file (extension: .def)
*                                   3. (not used)
* DEFEXT  CH*6    -             I   extension of definition file
*                                   (including dot)
* IERROR  I*4     -             O   ODS error code
* --------------------------------------------------------------------
*
      INTEGER*4 LENODS
      PARAMETER ( LENODS = 256 )
      CHARACTER*(*) FILNAM
      CHARACTER*(*) DEFEXT
      INTEGER*4     IERROR
      DIMENSION     FILNAM(*)
*
      CHARACTER*256 FILNEF
      INTEGER*4     IND1   , IND2   , LAST   , LASTD  , IFOUND , I
      LOGICAL*4     EXIST1 , EXIST2
*
      INCLUDE 'ods.inc'
*
* -------- Initialise the error code
*
      IERROR = IEOK
*
* -------- Check if the files exist and construct the second file
*          name if necessary
*          We are a little overcautious perhaps
*
      IND1   = INDEX ( FILNAM(1), CHAR(0) )
      IF ( IND1   .EQ. 0 ) THEN
         FILNEF = FILNAM(1)
      ELSE
         IF ( IND1   .GT. 1 ) THEN
            FILNEF = FILNAM(1)(1:IND1-1)
         ELSE
            IERROR = IENOFI
            RETURN
         ENDIF
      ENDIF
*
* --------- Ensure that the second file name will be a well-formed
*           file name according to C!
*
      LASTD  = LEN( DEFEXT )
      DO 110 I = LEN(DEFEXT),1,-1
         IF ( DEFEXT(I:I) .NE. ' ' ) THEN
            LASTD  = I
            GOTO 120
         ENDIF
  110 CONTINUE
  120 CONTINUE
*
      INQUIRE ( FILE = FILNEF , EXIST=EXIST1 )
*
      IND2   = INDEX ( FILNAM(2), CHAR(0) )
      IF ( IND2   .EQ. 0 ) THEN
         FILNEF = FILNAM(2)
      ELSE
         IF ( IND2   .GT. 1 ) THEN
            FILNEF = FILNAM(2)(1:IND2-1)
         ELSE
            IFOUND = 0
            FILNEF = FILNAM(1)(1:IND1-1)
            DO 210 I = LENODS,1,-1
               IF ( FILNEF(I:I) .NE. ' ' ) THEN
                  LAST   = I
               ENDIF
               IF ( FILNEF(I:I) .EQ. '.' ) THEN
                  IF ( DEFEXT(1:1) .EQ. '.' ) THEN
                     FILNEF(I:)   = DEFEXT(1:LASTD) // CHAR(0)
                  ELSE
                     FILNEF(I+1:) = DEFEXT(1:LASTD) // CHAR(0)
                  ENDIF
                  IFOUND = 1
                  GOTO 220
               ENDIF
  210       CONTINUE
*
  220       CONTINUE
            IF ( IFOUND .EQ. 0 ) THEN
               FILNEF(LAST+1:) = DEFEXT(1:LASTD) // CHAR(0)
            ENDIF
*
* --------- Put the name in the array
*
            FILNAM(2) = FILNEF
         ENDIF
      ENDIF
*
      INQUIRE ( FILE = FILNEF , EXIST = EXIST2 )
*
      IF ( .NOT. EXIST1 .OR. .NOT. EXIST2 ) THEN
         IERROR = IENOFI
      ENDIF
*
      RETURN
      END
