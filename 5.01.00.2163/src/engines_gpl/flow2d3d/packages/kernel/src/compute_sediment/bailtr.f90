subroutine bailtr(h         ,hrms      ,tp        ,thetaw    ,w         , &
                & dzdx      ,dzdy      ,sbksi     ,sbeta     ,ssksi     , &
                & sseta     ,epssl     ,faca      ,facu      )
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
!  $Id: bailtr.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/bailtr.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
!
! Global variables
!
    real(fp), intent(in)               :: dzdx
    real(fp), intent(in)               :: dzdy
    real(fp)        :: h
    real(fp)        :: hrms !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(out)              :: sbeta
    real(fp), intent(out)              :: sbksi
    real(fp), intent(out)              :: sseta
    real(fp), intent(out)              :: ssksi
    real(fp), intent(in)               :: thetaw !  Description and declaration in rivpro.igs
    real(fp)        :: tp !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)               :: w
    real(fp), intent(in) :: epssl, faca, facu
!
!
! Local variables
!
    real(fp)                       :: cr
    real(fp)                       :: cs
    real(fp)                       :: even1b
    real(fp)                       :: even2b
    real(fp)                       :: even3b
    real(fp)                       :: even5b
    real(fp)                       :: facb
    real(fp)                       :: facs
    real(fp)                       :: g
    real(fp)                       :: odd2b
    real(fp)                       :: odd3
    real(fp)                       :: odd3b
    real(fp)                       :: odd4
    real(fp)                       :: odd4b
    real(fp)                       :: qbb
    real(fp)                       :: sn
    real(fp)                       :: tanpsi
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !     GLOBAL DATA
    !
    !     global data structure definition and access functions
    !
    !
    g = 9.81
    tanpsi = 0.63
    cr = 0.
    qbb = 0.
    !
    !     interpoleren momenten oscillerende snelheid uit tabel
    !     -----------------------------------------------------
    call osmom(hrms      ,h         ,tp        ,g         ,cr        , &
             & qbb       ,even1b    ,even2b    ,even3b    ,even5b    , &
             & odd2b     ,odd3b     ,odd4b     )
    !
    odd3 = odd3b*faca
    odd4 = odd4b*faca
    !
    !     berekening coefficienten
    !     ------------------------
    facb = .817E-4
    facs = 1.03E-5/w
    !
    !     berekening component golven t.o.v. ksi-richting
    !     -----------------------------------------------
    cs = cos(thetaw)
    sn = sin(thetaw)
    !
    !     berekening transporten
    !     ----------------------
    sbksi = facb*(odd3*cs + even3b*dzdx/tanpsi)
    ssksi = facs*(odd4*cs + even5b*dzdx*epssl/w)
    sbeta = facb*(odd3*sn + even3b*dzdy/tanpsi)
    sseta = facs*(odd4*sn + even5b*dzdy*epssl/w)
end subroutine bailtr
