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

      SUBROUTINE REAALG ( LUNREP , LUNBLM , MAXTYP , MAXCOF, NOTYP ,
     +                    NOCOF  , NOUTGRP, NOUTTYP, ALGGRP, ABRGRP,
     +                    ALGTYP , ABRTYP , ALGDSC , COFNAM, ALGCOF,
     +                    OUTGRP , OUTTYP , NOPROT , NAMPROT,NAMPACT,
     +                    NOPRALG,NAMPRALG)
C
C     Read the BLOOM-species database.
C
      use timers       !   performance timers

      INTEGER       LUNREP, LUNBLM
      INTEGER       MAXTYP, MAXCOF
      INTEGER       NOTYP , NOCOF , NOGRP , NOUTGRP, NOUTTYP
      CHARACTER*10  ALGGRP(MAXTYP), ALGTYP(MAXTYP)
      CHARACTER*5   ABRGRP(MAXTYP), ABRTYP(MAXTYP)
      CHARACTER*80  ALGDSC(MAXTYP)
      CHARACTER*10  COFNAM(MAXCOF)
      REAL          ALGCOF(MAXCOF,MAXTYP)
      CHARACTER*10  OUTGRP(MAXTYP), OUTTYP(MAXTYP)
      INTEGER       NOPROT , NOPRALG
      CHARACTER*10  NAMPROT(MAXTYP), NAMPACT(MAXTYP),
     +              NAMPRALG(MAXTYP)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "reaalg", ithndl )
C
      READ(LUNBLM,*,ERR=900) NOTYP
      READ(LUNBLM,*,ERR=901) NOCOF
      READ(LUNBLM,2000,ERR=902) (COFNAM(I),I=1,NOCOF)
      DO IATYP = 1 , NOTYP
         READ(LUNBLM,2010,ERR=903) ALGGRP(IATYP),ABRGRP(IATYP),
     +                     ALGTYP(IATYP),ABRTYP(IATYP),ALGDSC(IATYP),
     +                     (ALGCOF(I,IATYP),I=1,NOCOF)
      ENDDO
      READ(LUNBLM,*,ERR=904) NOUTGRP
      DO I = 1 , NOUTGRP
         READ(LUNBLM,2020,ERR=905) OUTGRP(I)
      ENDDO
      READ(LUNBLM,*,ERR=906) NOUTTYP
      DO I = 1 , NOUTTYP
         READ(LUNBLM,2020,ERR=907) OUTTYP(I)
      ENDDO
      READ(LUNBLM,*,ERR=908) NOPROT
      DO I = 1 , NOPROT
         READ(LUNBLM,2030,ERR=909) NAMPROT(I),NAMPACT(I)
      ENDDO
      READ(LUNBLM,*,ERR=910) NOPRALG
      DO I = 1 , NOPRALG
         READ(LUNBLM,2020,ERR=911) NAMPRALG(I)
      ENDDO
C
      if (timon) call timstop( ithndl )
      RETURN
C
 2000 FORMAT(10X,1X,5X,1X,10X,1X,5X,1X,30X,50(1X,A10))
 2010 FORMAT(A10,1X,A5,1X,A10,1X,A5,1X,A30,50(1X,F10.0))
 2020 FORMAT(A10)
 2030 FORMAT(A10,1X,A10)
C
  900 CONTINUE
      WRITE(LUNREP,3000)
      WRITE( *    ,3000)
      CALL SRSTOP(1)
 3000 FORMAT(' Error reading BLOOM database, number of types')
  901 CONTINUE
      WRITE(LUNREP,3001)
      WRITE( *    ,3001)
      CALL SRSTOP(1)
 3001 FORMAT(' Error reading BLOOM database, number of coefficients')
  902 CONTINUE
      WRITE(LUNREP,3002)
      WRITE( *    ,3002)
      CALL SRSTOP(1)
 3002 FORMAT(' Error reading BLOOM database, coefficient names')
  903 CONTINUE
      WRITE(LUNREP,3003)
      WRITE( *    ,3003)
      CALL SRSTOP(1)
 3003 FORMAT(' Error reading BLOOM database, types and coefficients')
  904 CONTINUE
      WRITE(LUNREP,3004)
      WRITE( *    ,3004)
      CALL SRSTOP(1)
 3004 FORMAT(' Error reading BLOOM database, no. of output per group')
  905 CONTINUE
      WRITE(LUNREP,3005)
      WRITE( *    ,3005)
      CALL SRSTOP(1)
 3005 FORMAT(' Error reading BLOOM database, output var. per group')
  906 CONTINUE
      WRITE(LUNREP,3006)
      WRITE( *    ,3006)
      CALL SRSTOP(1)
 3006 FORMAT(' Error reading BLOOM database, no. of output per type')
  907 CONTINUE
      WRITE(LUNREP,3007)
      WRITE( *    ,3007)
      CALL SRSTOP(1)
 3007 FORMAT(' Error reading BLOOM database, output var. per type')
  908 CONTINUE
      WRITE(LUNREP,3008)
      WRITE( *    ,3008)
      CALL SRSTOP(1)
 3008 FORMAT(' Error reading BLOOM database, no. of single processes')
  909 CONTINUE
      WRITE(LUNREP,3009)
      WRITE( *    ,3009)
      CALL SRSTOP(1)
 3009 FORMAT(' Error reading BLOOM database, single processes')
  910 CONTINUE
      WRITE(LUNREP,3010)
      WRITE( *    ,3010)
      CALL SRSTOP(1)
 3010 FORMAT(' Error reading BLOOM database, no. of processes per type')
  911 CONTINUE
      WRITE(LUNREP,3011)
      WRITE( *    ,3011)
      CALL SRSTOP(1)
 3011 FORMAT(' Error reading BLOOM database, processes per type')
C
      END
