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

      MODULE STASUB
      INTERFACE
      SUBROUTINE SETDAY ( LUNREP     , NOKEY   ,
     +                    KEYNAM     , KEYVAL  ,
     +                    DTFLG1     , DTFLG3  ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
      USE ProcesSet
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , IPROC , IERR  , NOWARN
      LOGICAL       DTFLG1 , DTFLG3
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
C
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE
C
      SUBROUTINE SETDPT ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
      USE ProcesSet
C
      IMPLICIT NONE
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , IPROC , IERR  , NOWARN
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE
C
      SUBROUTINE SETDSC ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    PERNAM     , PERSFX     ,
     +                    PSTART     , PSTOP      ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
      USE ProcesSet
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , PSTART, PSTOP , IPROC ,
     +              IERR  , NOWARN
      CHARACTER*20  PERNAM, PERSFX
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE
C
      SUBROUTINE SETGEO ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    PERNAM     , PERSFX     ,
     +                    PSTART     , PSTOP      ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
      USE ProcesSet
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , PSTART, PSTOP , IPROC ,
     +              IERR  , NOWARN
      CHARACTER*20  PERNAM, PERSFX
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE
C
      SUBROUTINE SETPRC ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    PERNAM     , PERSFX     ,
     +                    PSTART     , PSTOP      ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
      USE ProcesSet
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , PSTART, PSTOP , IPROC ,
     +              IERR  , NOWARN
      CHARACTER*20  PERNAM, PERSFX
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE
C
      SUBROUTINE SETQTL ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    PERNAM     , PERSFX     ,
     +                    PSTART     , PSTOP      ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
      USE ProcesSet
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , PSTART, PSTOP , IPROC ,
     +              IERR  , NOWARN
      CHARACTER*20  PERNAM, PERSFX
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
      END SUBROUTINE
C
      SUBROUTINE RDSTAT ( LUNREP , IPOSR  , NPOS   , CCHAR  , VRSION ,
     +                    ILUN   , LCH    , LSTACK , IOUTPT , DTFLG1 ,
     +                    DTFLG3 , IERR   , NOSTAT , NKEY   , NOKEY  ,
     +                    KEYNAM , KEYVAL , NPERIOD, PERNAM , PERSFX ,
     +                    PSTART , PSTOP  )
C
      INTEGER       LUNREP , IPOSR  , NPOS   , LSTACK , IOUTPT ,
     +              IERR   , NOSTAT , NKEY
      LOGICAL       DTFLG1 , DTFLG3
      REAL          VRSION
      INTEGER       ILUN(*)
      CHARACTER*(*) LCH  (*)
      CHARACTER*1   CCHAR
      CHARACTER*20, POINTER :: KEYNAM(:)
      CHARACTER*20, POINTER :: KEYVAL(:)
      INTEGER     , POINTER :: NOKEY(:)
      INTEGER       NPERIOD
      CHARACTER*20, POINTER :: PERNAM(:)
      CHARACTER*20, POINTER :: PERSFX(:)
      INTEGER     , POINTER :: PSTART(:)
      INTEGER     , POINTER :: PSTOP(:)
      END SUBROUTINE
C
      END INTERFACE
C
      END MODULE
