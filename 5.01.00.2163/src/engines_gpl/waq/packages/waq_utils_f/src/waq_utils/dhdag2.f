!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      SUBROUTINE DHDAG2 ( NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +                    NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +                    NSYS  , IPGRID, IDATYP, ARRINP, WEIGHT,
     +                    ISWCUM, ARRHLP, ARROUT)
C
C     Deltares
C
C     Created             : June 1998 by Jan van Beek
C
C     Function            : Dis-aggregates value to finer grid
C
C     Subroutines called  : GETMLU, Get unit number report file
C                           SRSTOP, Stops execution
C                           ZERO  , Zero's a real array
C
C     Arguments           :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSEG1  INTEGER  1          INPUT   Number of segments on finer grid
C     NOSEG2  INTEGER  1          INPUT   Number of segments on coarser grid
C     IPGRID  INTEGER  NOSEG1     INPUT   Grid pointers to coarser grid
C     IDATYP  INTEGER  1          INPUT   Dis-aggregation type
C                                         1 = expansion of value
C                                         2 = distribute by WEIGHT
C                                         3 = distribute
C     ARRINP  REAL     NOSEG2     INPUT   Array to be dis-aggregated
C     WEIGHT  REAL     NOSEG1     INPUT   Weigth in dis-aggregation
C     ISWCUM  INTEGER  1          INPUT   Accummulaton in ARROUT switch (0=no/1=yes)
C     ARRHLP  REAL     NOSEG2     LOCAL   Local help array
C     ARROUT  REAL     NOSEG1     OUTPUT  Dis-aggregated array
C
C     Declaration of arguments
C
      INTEGER        NOSEG1, NOSEG2, IDATYP, ISWCUM
      INTEGER        IPGRID(NOSEG1)
      REAL           ARRINP(NOTOTI,NOSEG2) , WEIGHT(NOTOTW,NOSEG1) ,
     +               ARRHLP(NOTOTH,NOSEG2) , ARROUT(NOTOTO,NOSEG1)
C
C     Local declaration
C
C     ISEG1   INTEGER  1          LOCAL   Segment index finer grid
C     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
C     LUREP   INTEGER  1          LOCAL   Unit number report file
C
      INTEGER        ISEG1 , ISEG2 , LUREP
C
C     Zero arrays
C
      IF ( ISWCUM .EQ. 0 ) THEN
         DO ISEG1 = 1 , NOSEG1
            DO ISYS = 0 , NSYS - 1
               ARROUT(ISYSO+ISYS,ISEG1) = 0.0
            ENDDO
         ENDDO
      ENDIF
      IF ( IDATYP .EQ. 2 .OR. IDATYP .EQ. 2 ) THEN
         DO ISEG2 = 1 , NOSEG2
            DO ISYS = 0 , NSYS - 1
               ARRHLP(ISYSH+ISYS,ISEG2) = 0.0
            ENDDO
         ENDDO
      ENDIF
C
C     Accumulate WEIGHT in ARRHLP
C
      IF ( IDATYP .EQ. 3 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  ARRHLP(ISYSH+ISYS,ISEG2) = ARRHLP(ISYSH+ISYS,ISEG2) +
     +                                       1.0
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF ( IDATYP .EQ. 2 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  ARRHLP(ISYSH+ISYS,ISEG2) = ARRHLP(ISYSH+ISYS,ISEG2) +
     +                                       WEIGHT(ISYSW+ISYS,ISEG1)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C
C     Expand or distribute
C
      IF ( IDATYP .EQ. 1 ) THEN
C
C        Expand
C
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               IF ( ISWCUM .EQ. 0 ) THEN
                  DO ISYS = 0 , NSYS - 1
                     ARROUT(ISYSO+ISYS,ISEG1)=ARRINP(ISYSI+ISYS,ISEG2)
                  ENDDO
               ELSE
                  DO ISYS = 0 , NSYS - 1
                     ARROUT(ISYSO+ISYS,ISEG1)=ARROUT(ISYSO+ISYS,ISEG1)+
     +                                        ARRINP(ISYSI+ISYS,ISEG2)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ELSEIF ( IDATYP .EQ. 3 ) THEN
C
C        Distribute by weight
C
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  IF ( ABS(ARRHLP(ISYSH+ISYS,ISEG2)) .GT. 1.E-20 ) THEN
                     IF ( ISWCUM .EQ. 0 ) THEN
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARRINP(ISYSI+ISYS,ISEG2) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ELSE
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARROUT(ISYSO+ISYS,ISEG1) +
     +                                       ARRINP(ISYSI+ISYS,ISEG2) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ELSEIF ( IDATYP .EQ. 2 ) THEN
C
C        Distribute
C
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  IF ( ABS(ARRHLP(ISYSH+ISYS,ISEG2)) .GT. 1.E-20 ) THEN
                     IF ( ISWCUM .EQ. 0 ) THEN
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARRINP(ISYSI+ISYS,ISEG2) *
     +                                       WEIGHT(ISYSW+ISYS,ISEG1) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ELSE
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARROUT(ISYSO+ISYS,ISEG1) +
     +                                       ARRINP(ISYSI+ISYS,ISEG2) *
     +                                       WEIGHT(ISYSW+ISYS,ISEG1) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ELSE
C
C        ERROR , undefined dis-aggregation type
C
         CALL GETMLU(LUREP)
         WRITE(LUREP,2000) IDATYP
         CALL SRSTOP(1)
      ENDIF
C
      RETURN
 2000 FORMAT ( ' ERROR: undefined dis-aggregation type in DHDAGG :',I8 )
      END
