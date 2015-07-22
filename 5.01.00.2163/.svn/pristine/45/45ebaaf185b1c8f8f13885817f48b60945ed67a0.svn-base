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

      SUBROUTINE DLWQ10 ( IOUT   , IDUMP  , AMASS  , CONC   , AMASS2 ,
     *                    ITIME  , IDT    , IMSTRT , IMSTOP , IMSTEP ,
     *                    DNAME  , SNAME  , MNAME  , NODUMP , NOTOT  ,
     *                    NOSEG  , IP     , ISFLAG , IAFLAG , IMFLAG ,
     *                    ASMASS , INTOPT , NDMPAR , DANAM  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 4, 1988 by L.Postma
C
C     FUNCTION            : Writes monitoring results to IOUT in
C                                          blocks of 10 systems.
C
C     LOGICAL UNITNUMBERS : IOUT = number of monitoring output file
C
C     SUBROUTINES CALLED  : OUTMO1, print routine pointered
C                           OUTMO2, print routine not pointered
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     IOUT    INTEGER     1       INPUT   unit number output file
C     IDUMP   INTEGER  NODUMP     INPUT   segment numbers for dump
C     AMASS   REAL     NOTOT*?    INPUT   mass in the whole system
C     CONC    REAL     NOTOT*?    INPUT   concentration values
C     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance whole system
C     ITIME   INTEGER     1       INPUT   present time in clock units
C     IDT     INTEGER     1       INPUT   time step of simulation
C     IMSTRT  INTEGER     1       INPUT   start time of monitoring
C     IMSTOP  INTEGER     1       INPUT   stop time of monitoring
C     IMSTEP  INTEGER     1       INPUT   time step of monitoring
C     DNAME   CHAR*20   NODUMP    INPUT   names of monitoring stations
C     SNAME   CHAR*20   NOTOT     INPUT   names of substances
C     MNAME   CHAR*40     4       INPUT   model identification
C     NODUMP  INTEGER     1       INPUT   amount of dump segments
C     NOTOT   INTEGER     1       INPUT   total number of systems
C     NOSEG   INTEGER     1       INPUT   total number of segments
C     IP      INTEGER     4       IN/OUT  paging structure
C     ISFLAG  INTEGER     1       INPUT   if 1 then dd-hh:mm'ss"
C     IAFLAG  INTEGER     1       OUTPUT  if 1 then start accumulation
C     IMFLAG  LOGICAL     1       OUTPUT  TRUE if monitoring took place
C     ASMASS  REAL NOTOT*NDMPAR*? IN/OUT  Mass balance per segment
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C     NDMPAR  INTEGER     1       INPUT   Number of dump area's
C     DANAM   CHAR*20  NDMPAR     INPUT   Dump area names
C
C     Declaration of arguments
C
      use timers
      INTEGER      IOUT  , ITIME , IDT   , IMSTRT, IMSTOP,
     +             IMSTEP, NODUMP, NOTOT , NOSEG , ISFLAG,
     +             IAFLAG, INTOPT, NDMPAR
      INTEGER      IDUMP(*)       , IP(4)
      REAL         AMASS(NOTOT,*) , CONC(NOTOT,NOSEG)     ,
     *             AMASS2(NOTOT,5), ASMASS(NOTOT,NDMPAR,*)
      CHARACTER*20 DNAME(*) , SNAME(*) , DANAM(*)
      CHARACTER*40 MNAME(*)
      LOGICAL      IMFLAG
C
C     Local declaration
C
      CHARACTER*7  PADDER
      CHARACTER*20 SPACE
      CHARACTER*40 VNAME
      DATA         SPACE / ' ' /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq10", ithandl )
C
C           if mass balance per segment, fill asmass with mass
C
c     IF (MOD(INTOPT,16) .GE. 8 ) THEN
c        DO  5 I1 = 1,NOSEG
c        DO  5 I2 = 1,NOTOT
c   5    ASMASS(I2,I1,1) = AMASS(I2,I1)
c     ENDIF
C
C         initialise the paging, accumulation arrays and acumul flag
C
      IF ( IP(3) .EQ. 0 ) THEN
           IP(3) = MAX(1,IP(1)/(7+(NODUMP+7)*((NOTOT+IP(2)-1)/IP(2))))
           IP(4) = 0
           IF ( IAFLAG .EQ. 0 ) THEN
                DO 10 I1 = 1,    5
                DO 10 I2 = 1,NOTOT
   10           AMASS2(I2,I1) = 0.0
           ENDIF
           DO 20 I  = 1,NOTOT
           IF ( SNAME(I) .EQ. SPACE ) THEN
                SNAME(I)( 1:10) = ' SUBSTANCE'
                WRITE( PADDER,'(I7)' ) I
                SNAME(I)(11:17) = PADDER
           ENDIF
   20      CONTINUE
           IAFLAG = 0
      ENDIF
      IMFLAG = .FALSE.
      IF ( IMSTEP                   .LE. 0      ) goto 9999
      IF ( ITIME                    .LT. IMSTRT ) goto 9999
      IF ( ITIME-IDT                .GE. IMSTOP ) goto 9999
      IF ( MOD(ITIME-IMSTRT,IMSTEP) .GE. IDT    ) goto 9999
      IMFLAG = .TRUE.
C
C         set accumulation flag, compute total masses
C
      IAFLAG = 1
      DO 30 I1 = 1,NOSEG
      DO 30 I2 = 1,NOTOT
   30 AMASS2(I2,1) = AMASS2(I2,1) + AMASS(I2,I1)
C
C         start printing
C
      IF ( MOD(IP(4),IP(3)) .EQ. 0 ) THEN
           WRITE (IOUT,'(''1'')')
           WRITE (IOUT,2100 ) ( MNAME(K),K=1,4)
      ENDIF
      IP(4) = IP(4) + 1
      CALL REPTIM ( 6     , ITIME , ISFLAG, -999.)
      CALL REPTIM ( IOUT  , ITIME , ISFLAG, -999.)
C
      DO 50 ID = 1 , NOTOT , IP(2)
      NEND = MIN ( NOTOT , ID+IP(2)-1 )
      WRITE (IOUT,2030) (AMASS2(K,1)    ,K=ID,NEND)
      WRITE (IOUT,2040) (AMASS2(K,2)    ,K=ID,NEND)
      WRITE (IOUT,2050) (AMASS2(K,3)    ,K=ID,NEND)
      WRITE (IOUT,2060) (AMASS2(K,4)    ,K=ID,NEND)
      WRITE (IOUT,2070) (AMASS2(K,5)    ,K=ID,NEND)
      WRITE (IOUT,2020) (SNAME(K)( 1:10),K=ID,NEND)
      WRITE (IOUT,2020) (SNAME(K)(11:20),K=ID,NEND)
C
      VNAME = 'CONCENTRATION'
      CALL OUTMO1 ( IOUT   , IDUMP  , CONC   , VNAME  , DNAME  ,
     *              NODUMP , ID     , NEND   , NOTOT  )
      IF ( MOD(INTOPT,16) .GE. 8 ) THEN
      VNAME = 'MASS'
      CALL OUTMO2 ( IOUT   , ASMASS(1,1,1), VNAME  , DANAM  , NDMPAR ,
     +              ID     , NEND         , NOTOT  )
      VNAME = 'PROCESSES'
      CALL OUTMO2 ( IOUT   , ASMASS(1,1,2), VNAME  , DANAM  , NDMPAR ,
     +              ID     , NEND         , NOTOT  )
      VNAME = 'LOADS + BOUNDARIES ( IN )'
      CALL OUTMO2 ( IOUT   , ASMASS(1,1,3), VNAME  , DANAM  , NDMPAR ,
     +              ID     , NEND         , NOTOT  )
      VNAME = 'LOADS + BOUNDARIES ( OUT )'
      CALL OUTMO2 ( IOUT   , ASMASS(1,1,4), VNAME  , DANAM  , NDMPAR ,
     +              ID     , NEND         , NOTOT  )
      VNAME = 'INTERNAL TRANSPORT ( IN )'
      CALL OUTMO2 ( IOUT   , ASMASS(1,1,5), VNAME  , DANAM  , NDMPAR ,
     +              ID     , NEND         , NOTOT  )
      VNAME = 'INTERNAL TRANSPORT ( OUT )'
      CALL OUTMO2 ( IOUT   , ASMASS(1,1,6), VNAME  , DANAM  , NDMPAR ,
     +              ID     , NEND         , NOTOT  )
      ENDIF
C
      WRITE (IOUT,'('' '')')
   50 CONTINUE
C
C         zero the accumulation arrays
C
      DO 60 I1 = 1,    5
      DO 60 I2 = 1,NOTOT
   60 AMASS2(I2,I1) = 0.0
C
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
 2000 FORMAT (//' DUMP OF INTERMEDIATE RESULTS IN SELECTED SEGMENTS',
     *          ' AT TIME = ',I12,' .'//)
 2010 FORMAT (//' DUMP OF INTERMEDIATE RESULTS IN SELECTED SEGMENTS',
     *        ' AT TIME = ',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .'//)
 2020 FORMAT (22X,10(A10,' '))
 2030 FORMAT (  ' TOTAL MASS IN SYSTEM',10(1P,E11.4))
 2040 FORMAT (  ' CHANGES BY PROCESSES',10(1P,E11.4))
 2050 FORMAT (  ' CHANGES BY LOADS    ',10(1P,E11.4))
 2060 FORMAT (  ' BOUNDARY INFLOWS    ',10(1P,E11.4))
 2070 FORMAT (  ' BOUNDARY OUTFLOWS   ',10(1P,E11.4))
 2100 FORMAT (       45X, A40                       )
 2110 FORMAT ('  TIME = ',I12,' .')
 2120 FORMAT ('  TIME = ',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
C
      END
