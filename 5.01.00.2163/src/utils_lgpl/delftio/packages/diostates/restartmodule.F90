!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: restartmodule.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/delftio/packages/diostates/restartmodule.F90 $
!
! This file was part of RR, restartmodule.F90
! It is an example of the usage of dio_states
!
! The subroutines not related to dio_states are removed
!
! adri.mourits@deltares.nl, 23 mei 2008
!


module Restart

  !use
  use Conf_fil
  use Conf_Arr
  use Network
  use Link
  use Paved
  use Unpaved
  use Greenhouse
  use Openwater
  use Structures
  use Boundary
  use NWRW
  use RWZI
  use Industry
  use Sacramento
  use RRRouting
  use Salts
  use Messages
  use Meteo
  use ReadLib

#if (!defined(HAVE_CONFIG_H))
  use dio_states
#endif

  implicit none

#if (!defined(HAVE_CONFIG_H))
 type(DioStates) :: myStates
#endif


 real LowestRestartGroundwaterLevel
 ! LowestRestartGroundwaterLevel = maximum value lowest gwl below ground level
 !  after reading a restart file the groundwater level may not be lower than this value
 Logical SkipBoundLevelFromRestartFile
 ! oktober 2001
 ! ARS 8471 Request by Siebe for optional skipping of the water levels on the boundaries from the restart file
 Logical RestartOrderStrict
 ! ARS xxxxx March 2002 option to allow the order in the restart file and the 3b_nod.Tp file to differ

 Integer MaxNodeIdLength, MaxLinkIdLength


 contains









! HarmonIT

    SUBROUTINE RRStatesCreate (MyStatesName)

    Character(Len=*) MyStatesName

#if (!defined(HAVE_CONFIG_H))
    logical success

    success = DioStatesCreate(myStates,MyStatesName)
    if (.not. success) then
         write(*,*)  ' Could not create States ', MyStatesName
         stop
    endif

#endif

    return
    End SubRoutine RRStatesCreate


    SUBROUTINE RRStatesDestroy(MyStatesName)

!    logical success
    Character(Len=*) MyStatesName

#if (!defined(HAVE_CONFIG_H))
    write(*,*) ' Destroy states ', MyStatesName
    Call DioStatesDestroy(MyStates)
#endif

    return
    End SubRoutine RRStatesDestroy



    SUBROUTINE RR_SaveState (Itmstp)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Save state of timestep Itmstp
! *********************************************************************

      Integer     itmstp

#if (!defined(HAVE_CONFIG_H))
      Integer     iDebug, iovh
      Character(CharIdLength) StateId
      Character*8 Ext
      Real*8      TempDoubleArray(NOvh)

      logical success
      type (DioState) CurrState

      iDebug = ConfFil_get_iDebug()

      IF (IDEBUG .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RR_SaveState')

! *********************************************************************
! *** Save state; only run-time, so no checks whether it is the same schematisation
! *********************************************************************

      Ext = IntCh8(Itmstp)
      StateId = 'RRStates_t='// Ext

      write(*,*)  ' Create State ',StateID(1:Len_trim(StateId))
      success = DioStateCreate(myStates, CurrState, StateId)
      if (.not. success) then
         write(*,*)  ' Could not create State '
         stop
      Endif
      write(*,*)  ' State ',StateID(1:Len_trim(StateId)), ' created successfully; now fill with data'
      success = .true.

! Paved
      success = success .and. DioStateAddItemVal(CurrState, ' BVSTR ', BVSTR)
      success = success .and. DioStateAddItemVal(CurrState, ' BVRL  ', BVRL)

      success = success .and. DioStateAddItemVal(CurrState, ' BVSTR0', BVSTR0)
      success = success .and. DioStateAddItemVal(CurrState, ' BVRL0 ', BVRL0)

! UnPaved
      success = success .and. DioStateAddItemVal(CurrState, ' BOLND ', BOLND)
      success = success .and. DioStateAddItemVal(CurrState, ' BOBD  ', BOBD )
      success = success .and. DioStateAddItemVal(CurrState, ' GWL   ', GWL  )
      TempDoubleArray = 0
      Do iovh=1,Ncovhg
         TempDoubleArray(iovh) = OnvZone(iovh)%Actual_Volume
      Enddo
      success = success .and. DioStateAddItemVal(CurrState, ' OnvZoneActualContent', TempDoubleArray)
      TempDoubleArray = 0
      Do iovh=1,Ncovhg
         TempDoubleArray(iovh) = OnvZone(iovh)%Actual_mm
      Enddo
      success = success .and. DioStateAddItemVal(CurrState, ' OnvZoneActualMm', TempDoubleArray)
      success = success .and. DioStateAddItemVal(CurrState, ' BergC   ', BergC)

      success = success .and. DioStateAddItemVal(CurrState, ' BOLND0 ', BOLND0)
      success = success .and. DioStateAddItemVal(CurrState, ' BOBD0  ', BOBD0 )
      success = success .and. DioStateAddItemVal(CurrState, ' GWL0   ', GWL0  )
      TempDoubleArray = 0
      Do iovh=1,Ncovhg
         TempDoubleArray(iovh) = OnvZone(iovh)%Init_Volume
      Enddo
      success = success .and. DioStateAddItemVal(CurrState, ' OnvZoneInitContent', TempDoubleArray)
      TempDoubleArray = 0
      Do iovh=1,Ncovhg
         TempDoubleArray(iovh) = OnvZone(iovh)%Init_mm
      Enddo
      success = success .and. DioStateAddItemVal(CurrState, ' OnvZoneInitMm', TempDoubleArray)

! Greenhouse
      success = success .and. DioStateAddItemVal(CurrState, ' BKASD  ', BKASD)
      success = success .and. DioStateAddItemVal(CurrState, ' BKAS   ', BKAS )
      success = success .and. DioStateAddItemVal(CurrState, ' SiloB  ', SiloB)

      success = success .and. DioStateAddItemVal(CurrState, ' BKASD0 ', BKASD0)
      success = success .and. DioStateAddItemVal(CurrState, ' BKAS0  ', BKAS0 )
      success = success .and. DioStateAddItemVal(CurrState, ' SiloB0 ', SiloB0)

! Open Water
      success = success .and. DioStateAddItemVal(CurrState, ' LvlOw  ', LvlOw)
      success = success .and. DioStateAddItemVal(CurrState, ' ArOw   ', ArOw)
      success = success .and. DioStateAddItemVal(CurrState, ' VolOw  ', VolOw)
      success = success .and. DioStateAddItemVal(CurrState, ' LvlOw0 ', LvlOw0)
      success = success .and. DioStateAddItemVal(CurrState, ' ArOw0  ', ArOw0)
      success = success .and. DioStateAddItemVal(CurrState, ' VolOw0 ', VolOw0)

! Structures
      success = success .and. DioStateAddItemVal(CurrState, ' StrSta ', StrSta)

! Boundary
      success = success .and. DioStateAddItemVal(CurrState, ' BndPar ', BndPar)

! NWRW
      success = success .and. DioStateAddItemVal(CurrState, ' VolOp  ', VolOp)
      success = success .and. DioStateAddItemVal(CurrState, ' VolDyn ', VolDyn)
      success = success .and. DioStateAddItemVal(CurrState, ' BVOP   ', BVOP)
      success = success .and. DioStateAddItemVal(CurrState, ' DT     ', DT)
      success = success .and. DioStateAddItemVal(CurrState, ' INFCP  ', INFCP)
      success = success .and. DioStateAddItemVal(CurrState, ' INFSTS ', INFSTS)
      success = success .and. DioStateAddItemVal(CurrState, ' NTRRST ', NTRRST)

      success = success .and. DioStateAddItemVal(CurrState, ' VolOp0 ', VolOp0)

      success = success .and. DioStateAddItemVal(CurrState, ' SpecialVolOp ', SpecialVolOp)
      success = success .and. DioStateAddItemVal(CurrState, ' SpecialVolDyn', SpecialVolDyn)
      success = success .and. DioStateAddItemVal(CurrState, ' SpecialBVOp  ', SpecialBVOP)
      success = success .and. DioStateAddItemVal(CurrState, ' SpecialDT    ', SpecialDT)
      success = success .and. DioStateAddItemVal(CurrState, ' SpecialINFCP ', SpecialINFCP)
      success = success .and. DioStateAddItemVal(CurrState, ' SpecialINFSTS', SpecialINFSTS)
      success = success .and. DioStateAddItemVal(CurrState, ' SpecialNTRRST', SpecialNTRRST)

      success = success .and. DioStateAddItemVal(CurrState, ' SpecialVolOp0', SpecialVolOp0)

! RWZI
!     success = success .and. DioStateAddItemVal(CurrState, ' SpecialVolOp0 ', SpecialVolOp0)

! Industry
!     success = success .and. DioStateAddItemVal(CurrState, ' SpecialVolOp0 ', SpecialVolOp0)

! Sacramento
      success = success .and. DioStateAddItemVal(CurrState, ' UZTWC', UZTWC )
      success = success .and. DioStateAddItemVal(CurrState, ' UZFWC', UZFWC )
      success = success .and. DioStateAddItemVal(CurrState, ' LZTWC', LZTWC )
      success = success .and. DioStateAddItemVal(CurrState, ' LZFPC', LZFPC )
      success = success .and. DioStateAddItemVal(CurrState, ' LZFSC', LZFSC )
      success = success .and. DioStateAddItemVal(CurrState, ' ADIMC', ADIMC )
      success = success .and. DioStateAddItemVal(CurrState, ' Sacr_QQ', Sacr_QQ )

! Salt
      success = success .and. DioStateAddItemVal(CurrState, ' SALTF', SaltF )

      if (.not. success) then
         write(*,*)  ' Could not store everything in state ',StateID(1:Len_trim(StateId))
      Endif

#endif
      RETURN
    END subroutine RR_SaveState




      SUBROUTINE RR_RestoreState (RestoreTimeStep)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR version 2.08.                Date: April 2003
! *********************************************************************
! *** Last update: April 2003; test for HarmonIT project
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Restore state of timestep Itmstp
! *********************************************************************

      Integer RestoreTimeStep
#if (!defined(HAVE_CONFIG_H))
      Integer iDebug, iovh

      Character(CharIdLength) StateId
      Character*8 Ext
      Real*8      TempDoubleArray(NOvh)

      logical success
      type (DioState) CurrState

      iDebug = ConfFil_get_iDebug()
!
      IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RR_restoreState ')
!
! *********************************************************************
! *** Restore state; only run-time, so no checks whether it is the same schematisation
! *********************************************************************

      Ext = IntCh8(RestoreTimeStep)
      StateId = 'RRStates_t='// Ext

      success = DioStateGet(myStates, StateId, CurrState)
      if (.not. success) then
         write(*,*)  ' Could not get state ',StateID(1:Len_trim(StateId))
         return
      endif
      write(*,*)  ' Get State ',StateID(1:Len_trim(StateId))

! Paved
      success = DioStateGetItemVal(CurrState, ' BVSTR ', BVSTR)
      if (.not. success) write(*,*)  ' Error getting BVSTR '
      success = DioStateGetItemVal(CurrState, ' BVRL  ', BVRL)
      if (.not. success) write(*,*)  ' Error getting BVRL '

      success = DioStateGetItemVal(CurrState, ' BVSTR0', BVSTR0)
      if (.not. success) write(*,*)  ' Error getting BVSTR0 '
      success = DioStateGetItemVal(CurrState, ' BVRL0 ', BVRL0)
      if (.not. success) write(*,*)  ' Error getting BVRL0 '

! UnPaved
      success = DioStateGetItemVal(CurrState, ' BOLND ', BOLND)
      if (.not. success) write(*,*)  ' Error getting BOLND '
      success = DioStateGetItemVal(CurrState, ' BOBD  ', BOBD )
      if (.not. success) write(*,*)  ' Error getting BOBD '
      success = DioStateGetItemVal(CurrState, ' GWL   ', GWL  )
      if (.not. success) write(*,*)  ' Error getting GWL '
      success = DioStateGetItemVal(CurrState, ' OnvZoneActualContent', TempDoubleArray)
      if (.not. success) write(*,*)  ' Error getting OnvZoneActualContent '
      Do iovh=1,Ncovhg
         OnvZone(iovh)%Actual_Volume = TempDoubleArray(iovh)
      Enddo
      success = DioStateGetItemVal(CurrState, ' OnvZoneActualMm', TempDoubleArray)
      if (.not. success) write(*,*)  ' Error getting OnvZoneActualMm '
      Do iovh=1,Ncovhg
         OnvZone(iovh)%Actual_Mm = TempDoubleArray(iovh)
      Enddo
      success = DioStateGetItemVal(CurrState, ' BergC   ', BergC)
      if (.not. success) write(*,*)  ' Error getting BERGC '

      success = DioStateGetItemVal(CurrState, ' BOLND0 ', BOLND0)
      if (.not. success) write(*,*)  ' Error getting BOLND0 '
      success = DioStateGetItemVal(CurrState, ' BOBD0  ', BOBD0 )
      if (.not. success) write(*,*)  ' Error getting BOBD0 '
      success = DioStateGetItemVal(CurrState, ' GWL0   ', GWL0  )
      if (.not. success) write(*,*)  ' Error getting GWL0 '
      success = DioStateGetItemVal(CurrState, ' OnvZoneInitContent', TempDoubleArray)
      if (.not. success) write(*,*)  ' Error getting OnvZoneInitContent '
      Do iovh=1,Ncovhg
         OnvZone(iovh)%Init_Volume = TempDoubleArray(iovh)
      Enddo
      success = DioStateGetItemVal(CurrState, ' OnvZoneInitMm', TempDoubleArray)
      if (.not. success) write(*,*)  ' Error getting OnvZoneInitMm '
      Do iovh=1,Ncovhg
         OnvZone(iovh)%Init_Mm = TempDoubleArray(iovh)
      Enddo

! Greenhouse
      success = DioStateGetItemVal(CurrState, ' BKASD  ', BKASD)
      if (.not. success) write(*,*)  ' Error getting BKASD'
      success = DioStateGetItemVal(CurrState, ' BKAS   ', BKAS )
      if (.not. success) write(*,*)  ' Error getting BLAS '
      success = DioStateGetItemVal(CurrState, ' SiloB  ', SiloB)
      if (.not. success) write(*,*)  ' Error getting SiloB '

      success = DioStateGetItemVal(CurrState, ' BKASD0 ', BKASD0)
      if (.not. success) write(*,*)  ' Error getting BKASD0 '
      success = DioStateGetItemVal(CurrState, ' BKAS0  ', BKAS0 )
      if (.not. success) write(*,*)  ' Error getting BKAS0 '
      success = DioStateGetItemVal(CurrState, ' SiloB0 ', SiloB0)
      if (.not. success) write(*,*)  ' Error getting SiloB0 '

! Open Water
! test other order!!
      success = DioStateGetItemVal(CurrState, ' LvlOw  ', LvlOw)
      if (.not. success) write(*,*)  ' Error getting LvlOw '
      success = DioStateGetItemVal(CurrState, ' VolOw  ', VolOw)
      if (.not. success) write(*,*)  ' Error getting VolOw '
      success = DioStateGetItemVal(CurrState, ' ArOw  ', ArOw)
      if (.not. success) write(*,*)  ' Error getting ArOw '
!
      success = DioStateGetItemVal(CurrState, ' LvlOw0 ', LvlOw0)
      if (.not. success) write(*,*)  ' Error getting LvlOw0 '
      success = DioStateGetItemVal(CurrState, ' ArOw0 ', ArOw0)
      if (.not. success) write(*,*)  ' Error getting ArOw0 '
      success = DioStateGetItemVal(CurrState, ' VolOw0 ', VolOw0)
      if (.not. success) write(*,*)  ' Error getting VolOw0 '

! Structures
      success = DioStateGetItemVal(CurrState, ' StrSta ', StrSta)
      if (.not. success) write(*,*)  ' Error getting StrSta '

! Boundary
      success = DioStateGetItemVal(CurrState, ' BndPar ', BndPar)
      if (.not. success) write(*,*)  ' Error getting BndPar '

! NWRW
      success = DioStateGetItemVal(CurrState, ' VolOp  ', VolOp )
      if (.not. success) write(*,*)  ' Error getting VolOp '
      success = DioStateGetItemVal(CurrState, ' VolDyn ', VolDyn)
      if (.not. success) write(*,*)  ' Error getting VolDyn '
      success = DioStateGetItemVal(CurrState, ' BVOP   ', BVOP  )
      if (.not. success) write(*,*)  ' Error getting BVOP '
      success = DioStateGetItemVal(CurrState, ' DT     ', DT    )
      if (.not. success) write(*,*)  ' Error getting DT '
      success = DioStateGetItemVal(CurrState, ' INFCP  ', INFCP )
      if (.not. success) write(*,*)  ' Error getting InfCp '
      success = DioStateGetItemVal(CurrState, ' INFSTS ', INFSTS)
      if (.not. success) write(*,*)  ' Error getting InfSts '
      success = DioStateGetItemVal(CurrState, ' NTRRST ', NTRRST)
      if (.not. success) write(*,*)  ' Error getting NtrRst '

      success = DioStateGetItemVal(CurrState, ' VolOp0 ', VolOp0)
      if (.not. success) write(*,*)  ' Error getting VolOp0 '

      success = DioStateGetItemVal(CurrState, ' SpecialVolOp  ', SpecialVolOp )
      if (.not. success) write(*,*)  ' Error getting SpecialVolOp '
      success = DioStateGetItemVal(CurrState, ' SpecialVolDyn ', SpecialVolDyn)
      if (.not. success) write(*,*)  ' Error getting SpecialVolDyn '
      success = DioStateGetItemVal(CurrState, ' SpecialBVOp   ', SpecialBVOP  )
      if (.not. success) write(*,*)  ' Error getting SpecialBVOp '
      success = DioStateGetItemVal(CurrState, ' SpecialDT     ', SpecialDT    )
      if (.not. success) write(*,*)  ' Error getting SpecialDT '
      success = DioStateGetItemVal(CurrState, ' SpecialINFCP  ', SpecialINFCP )
      if (.not. success) write(*,*)  ' Error getting SpecialInfCp '
      success = DioStateGetItemVal(CurrState, ' SpecialINFSTS ', SpecialINFSTS)
      if (.not. success) write(*,*)  ' Error getting SpecialInfSts '
      success = DioStateGetItemVal(CurrState, ' SpecialNTRRST ', SpecialNTRRST)
      if (.not. success) write(*,*)  ' Error getting SpecialNtrRst '

      success = DioStateGetItemVal(CurrState, ' SpecialVolOp0 ', SpecialVolOp0)
      if (.not. success) write(*,*)  ' Error getting SpecialVolop0 '

! RWZI
!     success = DioStateGetItemVal(CurrState, ' SpecialVolOp0 ', SpecialVolOp0)

! Industry
!     success = DioStateGetItemVal(CurrState, ' SpecialVolOp0 ', SpecialVolOp0)

! Sacramento
      success = DioStateGetItemVal(CurrState, ' UZTWC', UZTWC )
      if (.not. success) write(*,*)  ' Error getting UZTWC'
      success = DioStateGetItemVal(CurrState, ' UZFWC', UZFWC )
      if (.not. success) write(*,*)  ' Error getting UZFWC'
      success = DioStateGetItemVal(CurrState, ' LZTWC', LZTWC )
      if (.not. success) write(*,*)  ' Error getting LZTWC'
      success = DioStateGetItemVal(CurrState, ' LZFPC', LZFPC )
      if (.not. success) write(*,*)  ' Error getting LZFPC'
      success = DioStateGetItemVal(CurrState, ' LZFSC', LZFSC )
      if (.not. success) write(*,*)  ' Error getting LZFSC'
      success = DioStateGetItemVal(CurrState, ' ADIMC', ADIMC )
      if (.not. success) write(*,*)  ' Error getting ADIMC '
      success = DioStateGetItemVal(CurrState, ' Sacr_QQ', Sacr_QQ )
      if (.not. success) write(*,*)  ' Error getting SACR_QQ '

! Salt
      success = DioStateGetItemVal(CurrState, ' SALTF', SaltF )
      if (.not. success) write(*,*)  ' Error getting SALTF '

      write(*,*)  ' State ',StateID(1:Len_trim(StateId)), ' successfully got'


! Unpaved
!      DO iOVH = 1,NCOVHG
!         OnvZone(IOVH)%Actual_Volume = Rdum2
!         OnvZone(Iovh)%Actual_mm = OnvZone(Iovh)%Actual_Volume / AreaOh(Iovh) / mm2m
!         Read (InRest,Err=999,End=999) BERGC(IOVH)
! Initial conditions
!         OnvZone(IOVH)%Init_Volume = OnvZone(IOVH)%Actual_Volume
!         OnvZone(Iovh)%Init_mm     = OnvZone(Iovh)%Actual_mm
!      ENDDO


! Sacramento
!           Read (Inrest,Err=999,End=999) ReadAdimCInRestartFile
!           DO iSacr = 1,NcSacr
!              UZTWCInit(Isacr) = Rdummy(1)
!              UZFWCInit(Isacr) = Rdummy(2)
!              LZTWCInit(Isacr) = Rdummy(3)
!              LZFPCInit(Isacr) = Rdummy(4)
!              LZFSCInit(Isacr) = Rdummy(5)
!              if (ReadAdimCInRestartFile) AdimC (ISacr) = Rdummy(6)
!           ENDDO

#endif

      RETURN
      END Subroutine RR_RestoreState


end module Restart
