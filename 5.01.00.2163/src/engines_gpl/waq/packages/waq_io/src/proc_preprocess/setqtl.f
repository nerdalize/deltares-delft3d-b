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

      SUBROUTINE SETQTL ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    PERNAM     , PERSFX     ,
     +                    PSTART     , PSTOP      ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
C
C     Deltares
C
C     CREATED:            : februari 2002 by Jan van Beek
C
C     FUNCTION            : Sets io list for statistical routine STAQTL
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
C     PERNAM  CHAR*20        1  INPUT   period name
C     PERSFX  CHAR*20        1  INPUT   period suffix
C     PSTART  INTEGER        1  INPUT   period start
C     PSTOP   INTEGER        1  INPUT   period stop
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
      INTEGER       LUNREP, NOKEY , PSTART, PSTOP , IPROC ,
     +              IERR  , NOWARN
      CHARACTER*20  PERNAM, PERSFX
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
      CHARACTER*10  STANAM
      REAL          CLOBND    , CUPBND , CQLEV
      INTEGER       NOBUCK    , IBUCK
      type(ItemProp)        :: aItemProp            ! one item
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "setqtl", ithndl )
C
C     init
C
      ALLOCATE(ISUSED(NOKEY),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating buffer array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETQTL_3, buffer length:',NOKEY
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
      aProcesProp%name       = 'STAQTL'
      WRITE(aProcesProp%name(7:10),'(I4.4)') IPROC
      STANAM=aProcesProp%name(1:10)
      aProcesProp%routine    = 'STAQTL'
      aProcesProp%text       = 'quantiles (concentration percentiles)'
      aProcesProp%swtransp   = 123
      aProcesProp%type       = PROCESTYPE_STAT
C
C     get the number of buckets to determine the amount of IO items
C     actual number of buckets is one larger, bucket00 the rest bucket for values below CLOBND
C
      KEY = 'NOBUCK'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         NOBUCK = 10
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(I20.0)',IOSTAT=IERR2) NOBUCK
         IF ( IERR2 .NE. 0 ) THEN
            WRITE(LUNREP,*)'ERROR interpreting number of buckets:',
     +         KEYVAL(IKEY)
            IERR = IERR + 1
            NOBUCK = 10
         ENDIF
      ENDIF
C
      aProcesProp%no_input      = 10 + nobuck + 1
      aProcesProp%no_output     = 1 + nobuck + 1
      aProcesProp%no_FluxOutput = 0
      aProcesProp%no_FluxStochi = 0
      aProcesProp%no_DispStochi = 0
      aProcesProp%no_VeloStochi = 0
      ALLOCATE(aProcesProp%input_item(aProcesProp%no_input),
     +         aProcesProp%output_item(aProcesProp%no_output),
     +         STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating IOitem array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDAY_1, array length:',aProcesProp%no_input,aProcesProp%no_output
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
      aItemProp%name    = 'START     '//aProcesProp%name(1:10)
      aItemProp%default = PSTART
      aItemProp%text    = 'start of statistic output period'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(2)%name=aItemProp%name
      aProcesProp%input_item(2)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(2)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(2)%actdef=PSTART
      aProcesProp%input_item(2)%indx  = 2
      aProcesProp%input_item(2)%ip_val  = 0
C
      aItemProp%name    = 'STOP      '//aProcesProp%name(1:10)
      aItemProp%default = PSTOP
      aItemProp%text    = 'stop of statistic output period'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(3)%name=aItemProp%name
      aProcesProp%input_item(3)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(3)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(3)%actdef=PSTOP
      aProcesProp%input_item(3)%indx  = 3
      aProcesProp%input_item(3)%ip_val  = 0
C
      aItemProp%name    = 'ITIME'
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret .le. 0 ) then
         aItemProp%default = -999.
         aItemProp%text    = 'time in calculation'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd( AllItems, aItemProp )
      endif
      aProcesProp%input_item(4)%name=aItemProp%name
      aProcesProp%input_item(4)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(4)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(4)%actdef=-999.
      aProcesProp%input_item(4)%indx  = 4
      aProcesProp%input_item(4)%ip_val  = 0
C
      aItemProp%name    = 'IDT'
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret .le. 0 ) then
         aItemProp%default = -999.
         aItemProp%text    = 'time step'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd( AllItems, aItemProp )
      endif
      aProcesProp%input_item(5)%name=aItemProp%name
      aProcesProp%input_item(5)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(5)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(5)%actdef=-999.
      aProcesProp%input_item(5)%indx  = 5
      aProcesProp%input_item(5)%ip_val  = 0
C
      aItemProp%name    = 'NOBUCK    '//aProcesProp%name(1:10)
      aItemProp%default = NOBUCK
      aItemProp%text    = 'number of bucktes'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(6)%name=aItemProp%name
      aProcesProp%input_item(6)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(6)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(6)%actdef=NOBUCK
      aProcesProp%input_item(6)%indx  = 6
      aProcesProp%input_item(6)%ip_val  = 0
C
      KEY = 'CLOBND'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         CLOBND = 0.0
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(F20.0)',IOSTAT=IERR2) CLOBND
         IF ( IERR2 .NE. 0 ) THEN
            WRITE(LUNREP,*)'ERROR lower boundary:',
     +         KEYVAL(IKEY)
            IERR = IERR + 1
         ENDIF
      ENDIF
      aItemProp%name    = 'CLOBND    '//aProcesProp%name(1:10)
      aItemProp%default = CLOBND
      aItemProp%text    = 'lower boundary'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(7)%name=aItemProp%name
      aProcesProp%input_item(7)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(7)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(7)%actdef=CLOBND
      aProcesProp%input_item(7)%indx  = 7
      aProcesProp%input_item(7)%ip_val  = 0
C
      KEY = 'CUPBND'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         CUPBND = 0.0
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(F20.0)',IOSTAT=IERR2) CUPBND
         IF ( IERR2 .NE. 0 ) THEN
            WRITE(LUNREP,*)'ERROR upper boundary:',
     +         KEYVAL(IKEY)
            IERR = IERR + 1
         ENDIF
      ENDIF
      aItemProp%name    = 'CUPBND    '//aProcesProp%name(1:10)
      aItemProp%default = CUPBND
      aItemProp%text    = 'upper boundary'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(8)%name=aItemProp%name
      aProcesProp%input_item(8)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(8)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(8)%actdef=CUPBND
      aProcesProp%input_item(8)%indx  = 8
      aProcesProp%input_item(8)%ip_val  = 0
C
      KEY = 'CQLEV'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR quantile not specified'
         IERR = IERR + 1
         CQLEV = 0.0
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(F20.0)',IOSTAT=IERR2) CQLEV
         IF ( IERR2 .NE. 0 ) THEN
            WRITE(LUNREP,*)'ERROR quantile:',
     +         KEYVAL(IKEY)
            IERR = IERR + 1
         ENDIF
      ENDIF
      aItemProp%name    = 'CQLEV     '//aProcesProp%name(1:10)
      aItemProp%default = CQLEV
      aItemProp%text    = 'quantile'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(9)%name=aItemProp%name
      aProcesProp%input_item(9)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(9)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(9)%actdef=CQLEV
      aProcesProp%input_item(9)%indx  = 9
      aProcesProp%input_item(9)%ip_val  = 0
C
      aItemProp%name    = 'TCOUNT    '//aProcesProp%name(1:10)
      aItemProp%default = 0.0
      aItemProp%text    = 'time step counter'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(10)%name=aItemProp%name
      aProcesProp%input_item(10)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(10)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(10)%actdef=0.0
      aProcesProp%input_item(10)%indx  = 10
      aProcesProp%input_item(10)%ip_val  = 0
C
      KEY = 'SUFFIX'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
C        something involving pcount ??
         SUFFIX = ' '
      ELSE
         SUFFIX = KEYVAL(IKEY)
         ISUSED(IKEY) = 1
      ENDIF
      CALL DHSLEN(SUFFIX,ISLEN)
      IF (SUFFIX(1:ISLEN) .NE. ' ' ) THEN
         SUFFIX =SUFFIX(1:ISLEN)//'_'//PERSFX
      ELSE
         SUFFIX ='QUANT_'//PERSFX
      ENDIF
      CALL DHSLEN(SUFFIX,ISLEN)
C
      aItemProp%name    = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      aItemProp%default = -999.
      aItemProp%text    = 'Quantile '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(1)%name=aItemProp%name
      aProcesProp%output_item(1)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(1)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(1)%indx= 1
      aProcesProp%output_item(1)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name,
     +                   '] created with quantile average from [',aProcesProp%input_item(1)%name,']'
C
C     buckets input and output
C
      DO IBUCK = 1 , NOBUCK + 1
         aItemProp%name    = 'BUCKET    '//STANAM
         WRITE(aItemProp%name(7:10),'(I4.4)') IBUCK-1
         aItemProp%default = -999.
         aItemProp%text    = 'work array for quantile'
         aItemProp%waqtype = WAQTYPE_NONE
         iret = ItemPropCollAdd( AllItems, aItemProp )
         aProcesProp%input_item(10+ibuck)%name=aItemProp%name
         aProcesProp%input_item(10+ibuck)%type=IOTYPE_SEGMENT_WORK
         aProcesProp%input_item(10+ibuck)%item=>AllItems%ItemPropPnts(iret)%pnt
         aProcesProp%input_item(10+ibuck)%actdef=-999.
         aProcesProp%input_item(10+ibuck)%indx  = 10+ibuck
         aProcesProp%input_item(10+ibuck)%ip_val= 0
         aProcesProp%output_item(1+ibuck)%name=aItemProp%name
         aProcesProp%output_item(1+ibuck)%type=IOTYPE_SEGMENT_WORK
         aProcesProp%output_item(1+ibuck)%item=>AllItems%ItemPropPnts(iret)%pnt
         aProcesProp%output_item(1+ibuck)%indx=1+ibuck
         aProcesProp%output_item(1+ibuck)%ip_val=0
      ENDDO
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
