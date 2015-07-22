!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!  $Id: flow2d3d_openda.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/flow2d3d_openda/src/flow2d3d_openda.f90 $
!
!-------------------------------------------------------------------------------
!
! Running:
!  SE_SetElementSet
!  SE_GetCurrentTime
!  SE_GetValuesElementCount
!  SE_GetValues
!  SE_SetDiscreteInputTimes
!  SE_GetInputTimeCount
!  SE_GetInputTimes
!  SE_SetValuesAtTime
!  SE_PerformTimeStep 
!
! Finishing:
!  SE_Finalize
!
!!--pseudo code and references--------------------------------------------------
! NONE
!
! The DLL can only be used for single domain calculations
! On this top level, module gdp_entry is used to:
! - define the (unique) gdp pointer (with SAVE attribute)
! - define the (unique) odfData pointer corresponding with the unique 
!   meteodata%odfData (with SAVE attribute)
! - allocate/deallocate subroutines, to be called from the first/last 
!   wrapper-called DLL-subroutines 
! - getOdfData, assuming that the gdp has been defined and containing the runid
!
!
!======================================================================
!
function Initialize_openda(componentID, schemID) result(retVal)
    !
    implicit none
    !
    ! result
    integer :: retVal              ! retVal == 0 : success
    !
    ! externals
    integer, external :: Initialize
    !
    ! arguments
    character(*), intent(in) :: componentID  ! RR, RTC, etc.
    character(*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! local variables
    character(256) :: version_full ! by calling getfullversionstring_flow2d3d_openda, the version number is visible with the what command
    !
    ! body
    !
    call getfullversionstring_flow2d3d_openda(version_full)
    !
    ! initialize the FLOW2D3D part
    !
    retval = Initialize(componentID, schemID)
    !
    call allocate_d3d_states(1)
    !
end function Initialize_openda
!
!==============================================================================
!
function Finalize_openda(componentID, schemID) result(retVal)
    use m_d3dstate_2_openda
    !    
    implicit none
    !
    integer, external :: Finalize
    !
    ! result
    integer     :: retVal           ! retVal == 0 : success
    !
    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    ! finalize the FLOW2D3D part
    !
    retval = Finalize(componentID, schemID)
    !
    ! deallocate the d3d_state. Should be somewhere in model_free.
    !
    call deallocate_d3d_states(1)
    !
    ! close alle netcdf-corestatefiles
    !
    call d3da_close_cta_state_files(retVal)
    !
    ! reset number of instances etc
    call d3da_reset_all()
    !                  
    ! TODO: costa finalize?
    !
end function Finalize_openda
!
!==============================================================================
!
function getObservedLocalization(location_id_c, dist, nvals, values) result(retVal)
    use precision     ! pntrsize, used in fsm.i
    use gdp_entry 
    use m_openda_quantities
    use m_d3dstate_2_openda
    
    implicit none  

    ! arguments
    integer         , intent(in)                    :: nvals         ! size of values array
    double precision, intent(in)                    :: dist          ! the characteristic distance
    double precision, dimension(nvals), intent(out) :: values        ! returned values
    character(*)    , intent(in)                    :: location_id_c ! location name

    !
    ! result
    integer :: retVal

    include 'tri-dyn.igd'
    include 'fsm.i'
    
    integer , pointer :: nmax
    integer , pointer :: mmax
    integer , pointer :: nostat
    integer , dimension(:,:) , pointer :: mnstat
    integer(pntrsize) , pointer :: xz
    integer(pntrsize) , pointer :: yz
    integer(pntrsize) , pointer :: kcs
    integer , pointer :: nub
    integer , pointer :: nlb
    integer , pointer :: mub
    integer , pointer :: mlb
    integer :: ist, lenid, ilo, iup
    real(fp):: ms, ns                ! (m,n) value of station
    real(fp), dimension(:), allocatable :: locval 
    character(20), dimension(:)    , pointer :: namst
    
    lenid = len(location_id_c)
    lenid = min(20,lenid)

    ! body
    retVal = -1 ! indices not ok
       
    if ( dist <= 0.0 ) then
       write(*,*) 'problem with characteristic distance ', dist
       return
    endif  

    nmax       => gdp%d%nmax
    mmax       => gdp%d%mmax
    if (sub_core_offsets(9)-1 /= nvals) then
       write(*,*) 'compute localization weights: incompatible lenghts ', nvals, nmax*mmax
       return
    else
       nostat     => gdp%d%nostat
       mnstat     => gdp%gdstations%mnstat
       namst      => gdp%gdstations%namst
       xz         => gdp%gdr_i_ch%xz
       yz         => gdp%gdr_i_ch%yz
       kcs        => gdp%gdr_i_ch%kcs
       nlb        => gdp%d%nlb
       nub        => gdp%d%nub
       mlb        => gdp%d%mlb
       mub        => gdp%d%mub
    
       allocate(locval((nub-nlb+1)*(mub-mlb+1)))       
       do ist = 1, nostat
         if (location_id_c(1:lenid)==namst(ist)(1:lenid)) then
            call compute_localization_weights( ist, dist, nlb, nub, mlb, mub, nmax, mmax, &
                                               nostat, mnstat, i(kcs), r(xz), r(yz), locval )
            retVal = 0
         endif
      enddo

      !  initialize weights
      values = 0.0
   
      !  check size and copy localization weights per state variable
      do ist = 1, size(sub_core_offsets)-1
         ilo = sub_core_offsets(ist)
         iup = sub_core_offsets(ist+1)
         ! check size
         if (iup-ilo == size(locval)) then
            values(sub_core_offsets(ist):sub_core_offsets(ist+1)-1) = locval
         else
            ! sub-tree vector of an unused variable has length 1
            if (iup-ilo /= 1 ) then
               write(*,*) 'GetObservedLocalization:                    ', &
                                'Warning sub-treevector does not      ', &
                                'have length "mnmaxk" localization is ', &
                                'not (yet) supported for this kind of ', &
                                'vectors'
               retVal = -1                 
            end if 
         endif
      enddo  
      deallocate(locval)
  end if 

end function getObservedLocalization
!
