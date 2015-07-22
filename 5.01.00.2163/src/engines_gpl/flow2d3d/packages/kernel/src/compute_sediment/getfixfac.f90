subroutine getfixfac(bedcomp   ,nmlb      ,nmub      ,nval      ,nmmax     , &
                   & fixfac    ,ffthresh  ,srcmax    ,cdryb     ,dt)
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
!  $Id: getfixfac.f90 1294 2012-02-28 17:34:56Z ormondt $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/getfixfac.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Update underlayer bookkeeping system for erosion/sedimentation
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use bedcomposition_module
    !
    implicit none
!
! Global variables
!
    integer                                            , intent(in)  :: nmmax
    integer                                            , intent(in)  :: nmlb
    integer                                            , intent(in)  :: nmub
    integer                                            , intent(in)  :: nval
    type(bedcomp_data)                                 , intent(in)  :: bedcomp
    real(fp)                                           , intent(in)  :: ffthresh
    real(fp)                                           , intent(in)  :: dt
    real(fp), dimension(nval)                          , intent(in)  :: cdryb
    real(fp), dimension(nmlb:nmub, nval)               , intent(out) :: fixfac
    real(fp), dimension(nmlb:nmub, nval)               , intent(out) :: srcmax
!
! Local variables
!
    integer  :: l
    integer  :: nm
    real(fp) :: ffac
    real(fp) :: thresh
!
!! executable statements -------------------------------------------------------
!
    call getalluvthick(bedcomp, fixfac, nmlb, nmub, nval)
    !
    ! FIXFAC here is sediment thickness
    !
    thresh = max(1.0e-10_fp,ffthresh)
    do l = 1, nval
       do nm = max(nmlb,1), min(nmmax,nmub)
          !
          ! Compute SRCMAX (only used for cohesive sediments)
          !
          if (ffthresh<1.0e-10_fp) then
             !
             ! If user-specified THRESH is <= 0.0, the erosion flux is not limited by FIXFAC,
             ! but by the amount of sediment that is available
             !
             srcmax(nm, l) = fixfac(nm, l)*cdryb(l)/dt
          else
             !
             ! Otherwise, only use conventional FIXFAC approach and set SRCMAX to
             ! a very high number
             !
             srcmax(nm, l) = 1.0e+10_fp
          endif
          fixfac(nm, l) = min(max(fixfac(nm, l)/thresh, 0.0_fp), 1.0_fp)
       enddo
    enddo
end subroutine getfixfac
