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

      SUBROUTINE DLWQ41 ( LUN    , ITIME  , ITIMEL , HARMAT , ARRAY  ,
     *                    IHARM  , NRHARM , NRFTOT , NOSEG  , VOLUME ,
     *                    IPOINT , LUNTXT , ftype  , ISFLAG , IVFLAG ,
     *                    UPDATV , INWSPC , ANWSPC , INWTYP , IWORK  ,
     *                    LSTREC , LREWIN , VOLLST , MYPART , dlwqd  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april- 8-1988 by L.Postma
C
C     FUNCTION            : Makes values at ITIME for volumes only.
C
C     LOGICAL UNITNUMBERS : LUN(..) -
C
C     SUBROUTINES CALLED  : DLWQT1, makes one time function
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ITIME   INTEGER       1     INPUT   Model timer
C     ITIMEL  INTEGER       1     INPUT   Model timer last step
C     HARMAT  REAL          ?     IN/OUT  matrices harmonic components
C     ARRAY   REAL          ?     LOCAL   set of double file buffers
C     IHARM   INTEGER       ?     INPUT   harmonics time space
C     NRHARM  INTEGER       ?     INPUT   set of nrs of harmonic records
C     NRFTOT  INTEGER       ?     INPUT   set of record lengthes
C     NOSEG   INTEGER       1     INPUT   nr of computational elements
C     VOLUME  REAL       NOSEG    OUTPUT  array of segment volumes
C     IDT     INTEGER       1     OUTPUT  integration time step size
C     IPOINT  INTEGER       ?     INPUT   set of pointers to destination
C     LUNTXT  CHAR*(*)      ?     INPUT   text with the unit numbers
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IVFLAG  INTEGER       1     INPUT   = 1 then computed volumes
C     UPDATV  LOGICAL       1     OUTPUT  set to T if volume is updated
C                                         else set to F
C     INWSPC  INTEGER       *     IN/OUT  Integer space new time funs
C     ANWSPC  REAL          *     IN/OUT  Real space new time functions
C     INWTYP  INTEGER       *     INPUT   Types of items
C     IWORK   INTEGER       *     LOCAL   Integer workspace
C     LSTREC  LOGICAL       1     INPUT   Switch last record on rewind wanted
C     LREWIN  LOGICAL       1     OUTPUT  Then rewind took place
C     VOLLST  REAL          *     OUTPUT  Last volume record before rewind
C     MYPART  INTEGER       1     INPUT   number of current part/subdomain
c
      use timers
      use m_couplib
      use delwaq2_data
C
C     DECLARATIONS        :
C
      integer, intent(in   )           :: ftype(*)     !< type of file to read
      type(delwaq_data), intent(inout) :: dlwqd        !< derived type for persistent storage

      DIMENSION    LUN   (*) , HARMAT(*) , ARRAY (*) , IHARM (*) ,
     *             NRHARM(*) , NRFTOT(*) , VOLUME(*) , IPOINT(*) ,
     *             INWSPC(*) , ANWSPC(*) , INWTYP(*) , IWORK (*) ,
     *             VOLLST(*)
      CHARACTER*(*)LUNTXT(*)
      LOGICAL      UPDATV    , LSTREC    , LREWIN
C
C     Local
C
      LOGICAL      UPDATE, LDUM(2)
C
C     COMMON  /  SYST   /   System time function flags
C
      INCLUDE 'syst.inc'
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq41", ithandl )
C
C         initialisation
C
      IPH = 1
      IPF = 1
      IPA = 1
      IPI = 1
      IFFLAG = 0
      UPDATV = .FALSE.
      LREWIN = .FALSE.
C
C         integration step size IDT
C
      IF ( NRFTOT( 1) .GT. 0 ) THEN
         IPA = IPA + 2
         IPI = IPI + 4
      ENDIF
C
C         volumes
C
      IF ( NRHARM( 2) .GE. 0 ) THEN
      IPA2 = IPA
      IPH2 = IPH
      IPF2 = IPF
      IPI2 = IPI
      IF ( IVFLAG     .EQ. 0 ) THEN
         IF (MYPART.EQ.1) THEN
            CALL DLWQT1 (LUN        , ITIME       , ITIMEL, IHARM(IPF2), HARMAT(IPH2),
     *                   ARRAY(IPA2), IPOINT(IPI2), VOLUME, 1          , NRHARM( 2)  ,
     *                   NOSEG      , NRFTOT( 2)  , IPA   , IPH        , IPF         ,
     *                   IPI        , LUNTXT      , 7     , ISFLAG     , IFFLAG      ,
     *                   UPDATE     , .FALSE.     , 0     , IWORK      , LSTREC      ,
     *                   LREWIN     , VOLLST      , ftype , dlwqd      )
            LDUM(1) = UPDATE
            LDUM(2) = LREWIN
         END IF

         CALL DISTRIBUTE_DATA(MYPART, LDUM, 2, IERR)
         UPDATE = LDUM(1)
         LREWIN = LDUM(2)

         IF ( UPDATE ) then
            UPDATV = .TRUE.
            CALL DISTRIBUTE_DATA(MYPART, VOLUME, 'noseg', 'distrib_itf', IERR)
         ENDIF
         IF ( LREWIN )
     *      CALL DISTRIBUTE_DATA(MYPART, VOLLST, 'noseg', 'distrib_itf', IERR)
      ENDIF
      ENDIF
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

