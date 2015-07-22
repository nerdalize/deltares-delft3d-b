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

      SUBROUTINE SETDPT ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
C     Deltares
C
C     CREATED:            : februari 2002 by Jan van Beek
C
C     FUNCTION            : Sets io list for statistical routine STADPT
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C                           ZOEK  , finds string in character array
C
C
C     PARAMETERS          :
C
C     NAME    KIND      LENGTH  FUNCT.  DESCRIPTION
C     ----    -----     ------  ------- -----------
C     LUNREP  INTEGER        1  INPUT   unit number report file
C     NOKEY   INTEGER        1  INPUT   number of keywords for this process
C     KEYNAM  CHAR*20    NOKEY  INPUT   keyword name
C     KEYVAL  CHAR*20    NOKEY  INPUT   keyword value
C     IPROC   INTEGER        1  INPUT   index number proces
C     aProcesProp               OUTPUT  properties for this proces
C     AllItems                  INPUT   all items known to the proces system
C     IERR    INTEGER        1  IN/OUT  cummulative error count
C     NOWARN  INTEGER        1  IN/OUT  cummulative warning count
C
      USE ProcesSet
      use timers       !   performance timers
C
      IMPLICIT NONE
C
C     Declaration of arguments
C
      INTEGER       LUNREP, NOKEY , IPROC , IERR  , NOWARN
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
C
C     Local declarations
C
      INTEGER       IERR_ALLOC, IKEY  , ISTART, ISTOP , ISLEN ,
     +              IERR2     , IRET
      INTEGER,      ALLOCATABLE :: ISUSED(:)
      CHARACTER*20  KEY       , SUFFIX
      REAL          PERIOD
      type(ItemProp)        :: aItemProp            ! one item
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "setdpt", ithndl )
C
C     init
C
      ALLOCATE(ISUSED(NOKEY),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating buffer array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDPT_3, buffer length:',NOKEY
         WRITE(*,*) 'ERROR allocating buffer array:',IERR_ALLOC
         CALL SRSTOP(1)
      ENDIF
      ISUSED = 0
      KEY='OUTPUT-OPERATION'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .GT. 0 ) THEN
         ISUSED(IKEY) = 1
      ENDIF
C
C     Fill the Propces Properties
C
      aProcesProp%name       = 'STADPT'
      WRITE(aProcesProp%name(7:10),'(I4.4)') IPROC
      aProcesProp%routine    = 'STADPT'
      aProcesProp%text       = 'depth averaged output'
      aProcesProp%swtransp   = 123
      aProcesProp%type       = PROCESTYPE_OUTPUT
      aProcesProp%no_input      = 2
      aProcesProp%no_output     = 3
      aProcesProp%no_FluxOutput = 0
      aProcesProp%no_FluxStochi = 0
      aProcesProp%no_DispStochi = 0
      aProcesProp%no_VeloStochi = 0
      ALLOCATE(aProcesProp%input_item(aProcesProp%no_input),
     +         aProcesProp%output_item(aProcesProp%no_output),
     +         STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating IOitem array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDPT_1, array length:',aProcesProp%no_input,aProcesProp%no_output
         WRITE(*,*) 'ERROR allocating array:',IERR_ALLOC
         CALL SRSTOP(1)
      ENDIF
C
C     input on segments
C
      KEY='SUBSTANCE'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR no parameter specified for statistics'
         IERR = IERR + 1
      ELSE
         ISUSED(IKEY) = 1
         aProcesProp%input_item(1)%name=KEYVAL(IKEY)
         aProcesProp%input_item(1)%type=IOTYPE_SEGMENT_INPUT
         aProcesProp%input_item(1)%actdef=-999.
         aProcesProp%input_item(1)%indx  = 1
         aProcesProp%input_item(1)%ip_val= 0
         aItemProp%name = KEYVAL(IKEY)
         iret = ItemPropCollFind( AllItems, aItemProp )
         if ( iret .le. 0 ) then
            aItemProp%text    = 'input parameter for statistics'
            aItemProp%default = -999.
            aItemProp%waqtype = WAQTYPE_NONE
            iret = ItemPropCollAdd( AllItems, aItemProp )
         endif
         aProcesProp%input_item(1)%item=>AllItems%ItemPropPnts(iret)%pnt
      ENDIF
C
      aItemProp%name    = 'VOLUME'
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret .le. 0 ) then
         aItemProp%default = -999.
         aItemProp%text    = 'volume of segment'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd( AllItems, aItemProp )
      endif
      aProcesProp%input_item(2)%name=aItemProp%name
      aProcesProp%input_item(2)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(2)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(2)%actdef=-999.
      aProcesProp%input_item(2)%indx  = 2
      aProcesProp%input_item(2)%ip_val  = 0
C
      KEY = 'SUFFIX'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         SUFFIX = ' '
      ELSE
         SUFFIX = KEYVAL(IKEY)
         ISUSED(IKEY) = 1
      ENDIF
      CALL DHSLEN(SUFFIX,ISLEN)
C
      IF (SUFFIX(1:ISLEN) .NE. ' ' ) THEN
         aItemProp%name    = 'DPTAVG_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'DPTAVG_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'depth average '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(1)%name=aItemProp%name
      aProcesProp%output_item(1)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(1)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(1)%indx= 1
      aProcesProp%output_item(1)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name,
     +                   '] created with depth average from [',aProcesProp%input_item(1)%name,']'
C
      IF (SUFFIX(1:ISLEN) .NE. ' ' ) THEN
         aItemProp%name    = 'DPTMAX_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'DPTMAX_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'maximum over depth '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(2)%name=aItemProp%name
      aProcesProp%output_item(2)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(2)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(2)%indx= 2
      aProcesProp%output_item(2)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name,
     +                   '] created with maximum over depth from [',aProcesProp%input_item(1)%name,']'
C
      IF (SUFFIX(1:ISLEN) .NE. ' ' ) THEN
         aItemProp%name    = 'DPTMIN_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'DPTMIN_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'minimum over depth '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(3)%name=aItemProp%name
      aProcesProp%output_item(3)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(3)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(3)%indx= 3
      aProcesProp%output_item(3)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name,
     +                   '] created with minimum over depth from [',aProcesProp%input_item(1)%name,']'
C
C     check the use of the key words
C
      DO IKEY = 1 , NOKEY
         IF ( ISUSED(IKEY) .EQ. 0 ) THEN
            NOWARN = NOWARN + 1
            WRITE(LUNREP,*) 'WARNING: keyword not used'
            WRITE(LUNREP,*) 'key   :',KEYNAM(IKEY)
            WRITE(LUNREP,*) 'value :',KEYVAL(IKEY)
         ENDIF
      ENDDO
C
      DEALLOCATE (ISUSED)
C
      if (timon) call timstop( ithndl )
      RETURN
 2000 FORMAT(5A)
      END
