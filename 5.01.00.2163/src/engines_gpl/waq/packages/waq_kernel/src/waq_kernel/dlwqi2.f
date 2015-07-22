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

      SUBROUTINE DLWQI2 ( LUN    , MODID  , SYSID  , IDUMP  , DUMPID ,
     &                    IDPNT  , IVPNT  , DISP   , IBPNT  , BNDID  ,
     &                    BNDNAM , BNDTYP , INWTYP , IWASTE , iwsknd ,
     &                    WASTID , WSTNAM , WSTTYP , ALENG  , CONST  ,
     &                    PARAM  , NRFTOT , NRHARM , CONAME , PANAME ,
     &                    FUNAME , SFNAME , DINAME , VENAME , IKNMRK ,
     &                    DANAM  , IPDMP  , IQDMP  , ISDMP  , RANAM  ,
     &                    IORAAI , NQRAAI , IQRAAI , GRDNOS , GRDREF ,
     &                    GRDSEG , GridPs , DMPBAL , dlwqd  )

!     Deltares Software Centre

!>\file
!>                          Reads the DelwaQ binary system file
!>
!>                          Initialises all fixed conditions and names
!>                          for the simulation from the binary system
!>                          file at LUN(2).

C     CREATED: may -1988 by L. Postma
C
C     LOGICAL UNITNUMBERS : LUN( 2) - system intermediate file
C                           LUN(19) - monitoring output file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C

      use timers
      use grids
      use delwaq2_data

C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER       *     INPUT   logical unitnumbers
C     MODID   CHAR*40       4     OUTPUT  Model and run-ID
C     SYSID   CHAR*20   NOTOT     OUTPUT  Systems ID
C     IDUMP   INTEGER  NODUMP     OUTPUT  Dump segment numbers
C     DUMPID  CHAR*20  NODUMP     OUTPUT  Dump-segment ID
C     IDPNT   INTEGER   NOSYS     OUTPUT  Pointers to dispersion array
C     IVPNT   INTEGER   NOSYS     OUTPUT  Pointers to velocity array
C     DISP    REAL          3     OUTPUT  dispersion in 3 directions
C     IBPNT   INTEGER  4*NOBND    OUTPUT  1,* = timelag
C                                         2,* = flow pointer
C                                         3,* = segment pointer
C                                         4,* = time on timelag
C     BNDID   CHAR*20   NOBND     OUTPUT  Open boundary ID's
C     BNDNAM  CHAR*40   NOBND     OUTPUT  Open boundary names
C     BNDTYP  CHAR*20   NOBND     OUTPUT  Open boundary types
C     INWTYP  INTEGER     *       OUTPUT  Types of items
C     IWASTE  INTEGER   NOWST     OUTPUT  waste load segment numbers
      integer  ( 4), intent(  out) :: iwsknd(*) !  wasteload processing
C     WASTID  CHAR*20   NOWST     OUTPUT  Waste location ID
C     WSTNAM  CHAR*40   NOWST     OUTPUT  Waste location names
C     WSTTYP  CHAR*20   NOWST     OUTPUT  Waste location types
C     ALENG   REAL        3       OUTPUT  Lengthes in 3 directions
C     CONST   REAL     NOCONS     OUTPUT  value of constants
C     PARAM   REAL    NOPA,NOSEG  OUTPUT  value of parameters
C     NRFTOT  INTEGER  NOITEM     OUTPUT  file lengthes per item
C     NRHARM  INTEGER  NOITEM     OUTPUT  nr of harmonics per item
C     CONAME  CHAR*20  NOCONS     OUTPUT  Constant names
C     PANAME  CHAR*20  NOPA       OUTPUT  Parameter names
C     FUNAME  CHAR*20  NOFUN      OUTPUT  Function names
C     SFNAME  CHAR*20  NOSFUN     OUTPUT  Segment function names
C     DINAME  CHAR*20  NODISP     OUTPUT  Dispersion array names
C     VENAME  CHAR*20  NOVELO     OUTPUT  Velocity array names
C     DANAM   CHAR*20  NDMPAR     OUTPUT  Dump-area    ID
C     IPDMP   INTEGER       *     OUTPUT  pointer structure dump area's
C     IQDMP   INTEGER       *     OUTPUT  Exchange to dumped exchange pointer
C     ISDMP   INTEGER       *     OUTPUT  Segment to dumped segment pointer
C     RANAM   CHAR*20       *     OUTPUT  Raaien names
C     IORAAI  INTEGER       *     OUTPUT  option output raaien
C     NQRAAI  INTEGER       *     OUTPUT  number of exch. per raai
C     IQRAAI  INTEGER       *     OUTPUT  exchange nunbers raaien
C
C
C     IN COMMON BLOCK     :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSEG   INTEGER       1     INPUT   Number of segments
C     NOSYS   INTEGER       1     INPUT   Number of active systems
C     NOTOT   INTEGER       1     INPUT   Number of systems
C     NODISP  INTEGER       1     INPUT   Number of dispersion array's
C     NOVELO  INTEGER       1     INPUT   Number of velocity array's
C     NOQ     INTEGER       1     INPUT   total number of exchanges
C     NODUMP  INTEGER       1     INPUT   Number of dump segments
C     NOBND   INTEGER       1     INPUT   Number of open boundaries
C     NOBTYP  INTEGER       1     INPUT   Number of boundarie types
C     NOWST   INTEGER       1     INPUT   Number of load locations
C     NOWTYP  INTEGER       1     INPUT   Number of waste load types
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     NOITEM  INTEGER       1     INPUT   Number possible functions
C     NDMPAR  INTEGER       1     INPUT   Number of dump area's
C     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
C     NTDMPS  INTEGER       1     INPUT   total number segments in dump area
C     NORAAI  INTEGER       1     INPUT   number of raaien
C     NTRAAQ  INTEGER       1     INPUT   total number of exch. in raaien
C
C

      INTEGER      IPDMP(*)  , IQDMP(*)   , ISDMP (*) , IORAAI(*) ,
     +             NQRAAI(*) , IQRAAI(*)  , GRDNOS(*) , GRDREF(*)
      INTEGER      GRDSEG(NOSEG,NOGRID)
      CHARACTER*40 MODID (4) , BNDNAM(*)  , WSTNAM(*)
      CHARACTER*20 SYSID (*) , DUMPID(*)  , BNDID (*) , BNDTYP(*) ,
     *             WASTID(*) , WSTTYP(*)  , CONAME(*) , PANAME(*) ,
     *             FUNAME(*) , SFNAME(*)  , DINAME(*) , VENAME(*)  ,
     *             DANAM (*) , RANAM (*)
      DIMENSION    IDUMP (*) , IDPNT (*)  , IVPNT (*) , DISP  (*) ,
     *             ALENG (*) , IBPNT (4,*), IWASTE(*) , CONST (*) ,
     *             PARAM (*) , NRFTOT(*)  , NRHARM(*) , LUN   (*) ,
     *             IKNMRK(*) , INWTYP(*)
      CHARACTER*40  FILLER
      type(GridPointerColl), intent(inout) :: GridPs     !< definitions of the grids
      type(delwaq_data),     intent(inout) :: dlwqd      !< derived type for persistent storage
      integer                              :: dmpbal(*)  !< indicates if dump area is included in the balance
      type(GridPointer)    :: aGrid      ! a single grid
C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI   /   Timer characteristics
C
      INCLUDE 'sysi.inc'

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqi2", ithandl )
C
C     Local
C
      IT = 0
C
C         read from the system file
C
      NOQTT = NOQ + NOQ4
      NOSSS = NOSEG + NSEG2
      IIN = LUN(2)
      READ ( IIN    , END=40, ERR=40)  MODID (1  ), MODID(2)
      READ ( IIN    , END=40, ERR=40)  MODID (3  ), MODID(4)
      READ ( IIN    , END=40, ERR=40) (SYSID (  K), K=1,NOTOTp )
      IF ( NODUMP .GT. 0 )
     &   READ ( IIN , END=40, ERR=40) (IDUMP(K), DUMPID(K), K=1,NODUMP)
      IF ( NDMPAR .GT. 0 )
     &   READ ( IIN , END=40, ERR=40) (DANAM(K), K=1,NDMPAR)
      IF ( NDMPAR .GT. 0 )
     &   READ ( IIN , END=40, ERR=40) (DMPBAL(K), K=1,NDMPAR)
      IF ( NORAAI .GT. 0 )
     &   READ ( IIN , END=40, ERR=40) (RANAM(K), K=1,NORAAI)
C
C     sub-grid
C
      DO IGRID = 1 , NOGRID
         READ ( IIN , END=40, ERR=40)  GRDNOS(IGRID), GRDREF(IGRID),
     +                                (GRDSEG(ISEG,IGRID),ISEG=1,NOSSS)
      ENDDO
c     the grid structures
      DO IGRID = 1 , NOGRID
         ierror = GridRead( iin, aGrid, nosss )
         if ( ierror .ne. 0 ) goto 40
         i_grid = GridPointerCollAdd(GridPs,aGrid)
      ENDDO
      READ ( IIN , END=40, ERR=40) (IDUMMY,ISYS=1,NOTOT)
      READ ( IIN , END=40, ERR=40) (IDUMMY,ISYS=1,NOTOT)
      READ ( IIN    , END=40, ERR=40) (IKNMRK(  K), K=1,NOSSS  )
      IF ( NODISP .GT. 0 )
     &   READ ( IIN , END=40, ERR=40) (DINAME(K)  , K=1,NODISP)
      IF ( NOVELO .GT. 0 )
     &   READ ( IIN , END=40, ERR=40) (VENAME(K)  , K=1,NOVELO)
      READ ( IIN    , END=40, ERR=40) (IDPNT (  K), K=1,NOSYS  )
      READ ( IIN    , END=40, ERR=40) (IVPNT (  K), K=1,NOSYS  )
      IF ( NOBND  .GT. 0 ) THEN
         READ ( IIN , END=40, ERR=40) (IBPNT (2,K), K=1,NOBND  )
         READ ( IIN , END=40, ERR=40) (IBPNT (3,K), K=1,NOBND  )
      ENDIF
      IF ( NDMPAR .GT. 0 ) THEN
         READ (IIN, END=40, ERR=40)  (IPDMP(K),K=1,NDMPAR+NTDMPQ)
         IX = NDMPAR+NTDMPQ
         READ (IIN, END=40, ERR=40)  (IPDMP(IX+K),K=1,NDMPAR+NTDMPS)
      ENDIF
      IF ( NORAAI .GT. 0 ) THEN
         READ (IIN, END=40, ERR=40)  (IORAAI(K),K=1,NORAAI)
         READ (IIN, END=40, ERR=40)  (NQRAAI(K),K=1,NORAAI)
         READ (IIN, END=40, ERR=40)  (IQRAAI(K),K=1,NTRAAQ)
      ENDIF
      IF ( NORAAI .GT. 0 .OR. NDMPAR .GT. 0 ) THEN
         READ (IIN, END=40, ERR=40)  (IQDMP(K),K=1,NOQTT)
      ENDIF
      IF ( NDMPAR .GT. 0 ) THEN
         READ (IIN, END=40, ERR=40)  (ISDMP(K),K=1,NOSSS)
      ENDIF
      READ ( IIN    , END=40, ERR=40) IDUMMY , (DISP  (K), K=1,3)
      READ ( IIN    , END=40, ERR=40) IDUMMY , (ALENG (K), K=1,3)
      IF ( NOBND  .GT. 0 ) THEN
         DO 10 I = 1 , NOBND
            READ ( IIN , END=40, ERR=40) BNDID(I),BNDNAM(I)
   10    CONTINUE
         READ ( IIN , END=40, ERR=40) ( BNDTYP(K)   , K=1,NOBTYP )
         READ ( IIN , END=40, ERR=40) ( INWTYP(K+IT), K=1,NOBND  )
         IT = IT + NOBND
C          read time lags
         READ ( IIN , END=40, ERR=40) ( IBPNT(1,K), K=1,NOBND  )
      ENDIF
      IF ( NOWST  .GT. 0 ) THEN
         DO 20 I = 1 , NOWST
            READ ( IIN , END=40, ERR=40) IWASTE(I), iwsknd(i),
     &                                   WASTID(I), WSTNAM(I)
   20    CONTINUE
         READ ( IIN , END=40, ERR=40) ( WSTTYP(K)    , K=1,NOWTYP )
         READ ( IIN , END=40, ERR=40) ( INWTYP(K+IT) , K=1,NOWST  )
         IT = IT + NOWST
      ENDIF
      IF ( NOCONS .GT. 0 ) THEN
         READ ( IIN , END=40, ERR=40) (CONAME(K),K=1,NOCONS )
      ENDIF
      IF ( NOPA   .GT. 0 ) THEN
         READ ( IIN , END=40, ERR=40) (PANAME(K),K=1,NOPA)
      ENDIF
      IF ( NOFUN  .GT. 0 ) THEN
         READ ( IIN , END=40, ERR=40) (FUNAME(K),K=1,NOFUN)
      ENDIF
      IF ( NOSFUN .GT. 0 ) THEN
         READ ( IIN , END=40, ERR=40) (SFNAME(K),K=1,NOSFUN)
      ENDIF
C
C     Time function info
C
      READ ( IIN    , END=40, ERR=40) (NRFTOT( K), K=1,NOITEM)
      READ ( IIN    , END=40, ERR=40) (NRHARM( K), K=1,NOITEM)
C
C         boundary timings greater then timelag
C
      DO 30 I=1,NOBND
      IBPNT(4,I)  = IBPNT(1,I) + 1
   30 CONTINUE
C
C         extract reference date and time
C
      CALL MODIFIED_JULIAN( MODID(4) )
      dlwqd%otime  = otime
      dlwqd%tscale = tscale
C
C         completion successful
C
      WRITE ( LUN(19) , 2000 ) (MODID(K),K=1,4)
      if ( timon ) call timstop ( ithandl )
      RETURN
C
C         unsuccessful read
C
   40 WRITE ( LUN(19) , 2010 )
      CALL SRSTOP(1)
C
C         output formats
C
 2000 FORMAT ( '1',20X,A40/21X,A40//21X,A40/21X,A40//
     &             21X,'Initialisation from system file completed.')
 2010 FORMAT ( '1  ERROR reading binary system file !!'/
     &         '   initialisation NOT successful    !!'/
     &         '   simulation impossible            !!')
C
      CONTAINS
      SUBROUTINE MODIFIED_JULIAN( T0STRING )
      CHARACTER(LEN=*) :: T0STRING

      INTEGER                :: IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE
      REAL(KIND=KIND(1.0D0)) :: TEMP1, TEMP2

      REAL(KIND=KIND(1.0D0)), PARAMETER :: MODIFICATION_OFFSET = 2400000.5D0

      OTIME  = MODIFICATION
      TSCALE = 1.0d0

      READ( T0STRING, '(4x,i4.4,x,i2.2,x,i2.2,x,i2.2,x,i2.2,x,i2.2,7x,i8)', IOSTAT = IERR )
     &    IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE

      IF ( IERR /= 0 ) THEN
         IYEAR  = 1900
         IMONTH = 1
         IDAY   = 1
         IHOUR  = 0
         IMIN   = 0
         ISEC   = 0
         ISCALE = 1
      ENDIF

      TEMP1  = INT (( IMONTH-14.0D0) / 12.0D0 )
      TEMP2  = IDAY - 32075.0D0 +
     1       INT ( 1461.0D0 * ( IYEAR + 4800.0D0 + TEMP1 ) / 4.0D0 ) +
     2       INT ( 367.0D0 * ( IMONTH - 2.0D0 - TEMP1 * 12.0D0 ) / 12.0D0 ) -
     3       INT ( 3.0D0 * INT ( ( IYEAR + 4900.0D0 + TEMP1 ) / 100.0D0 ) /
     4       4.0 )
      TEMP1  = FLOAT ( IHOUR ) * 3600.0 +
     1         FLOAT ( IMIN  ) *   60.0 +
     2         FLOAT ( ISEC  ) - 43200.0
      OTIME  = TEMP2 + ( TEMP1 / 86400.0 ) - MODIFICATION_OFFSET
      TSCALE = 86400.0D0 / ISCALE
      END SUBROUTINE MODIFIED_JULIAN

      END
