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
!  $Id: d3dstate_2_d3d.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/flow2d3d_openda/src/d3dstate_2_d3d.f90 $
!
!--------------------    
  subroutine array_copy(arfrom,arto,d1_l,d1_u,d2_l,d2_u,d3_l,d3_u,d4)
    use precision   ! voor fp    
    implicit none
    integer :: d1_l,d1_u,d2_l,d2_u,d3_l,d3_u,d4
    real(fp), dimension(d1_l:d1_u,d2_l:d2_u,d3_l:d3_u,d4)  :: arfrom
    real(fp), dimension(d1_l:d1_u,d2_l:d2_u,d3_l:d3_u,d4)  :: arto

    arto = arfrom

  end subroutine array_copy

!--------------------    
  subroutine procbc_copy(d3dstate_procbc,procbc,d2_l,d2_u,d3_l,d3_u,d4)
    use precision   ! voor fp    
    implicit none
    integer :: d2_l,d2_u,d3_l,d3_u,d4
    real(fp), dimension(1:4,d2_l:d2_u,d3_l:d3_u,d4)  :: d3dstate_procbc
    real(fp), dimension(1:4,d2_l:d2_u,d3_l:d3_u,d4)  :: procbc

    procbc(1,:,:,:) = d3dstate_procbc(1,:,:,:)
    procbc(2,:,:,:) = d3dstate_procbc(2,:,:,:)    
!   do not adjust procbc(3:4 since these increments will be recomputed when timnow is outside bcc file pointer
!   range. 

  end subroutine procbc_copy

!--------------------    
  subroutine array_i_copy(arfrom,arto,d1_l,d1_u,d2_l,d2_u,d3_l,d3_u,d4)  
    implicit none
    integer :: d1_l,d1_u,d2_l,d2_u,d3_l,d3_u,d4
    integer, dimension(d1_l:d1_u,d2_l:d2_u,d3_l:d3_u,d4)  :: arfrom
    integer, dimension(d1_l:d1_u,d2_l:d2_u,d3_l:d3_u,d4)  :: arto

    arto = arfrom

  end subroutine array_i_copy

!----------------


  subroutine get_d3d_state_from_d3d(imode)

    use precision     ! pntrsize, used in fsm.i
    use gdp_entry
    use m_d3d_state
      
    implicit none

    include 'fsm.i'
    include 'tri-dyn.igd'
 
    integer :: imode
  
    integer(pntrsize)            , pointer :: u1
    integer(pntrsize)            , pointer :: v1
    integer(pntrsize)            , pointer :: w1
    integer(pntrsize)            , pointer :: vicuv
    integer(pntrsize)            , pointer :: vicww
    integer(pntrsize)            , pointer :: r1
    integer(pntrsize)            , pointer :: s1
    integer(pntrsize)            , pointer :: dp
    integer(pntrsize)            , pointer :: umnldf 
    integer(pntrsize)            , pointer :: vmnldf
    integer(pntrsize)            , pointer :: rtur1    

    integer(pntrsize)            , pointer :: kfs
    integer(pntrsize)            , pointer :: kfu   
    integer(pntrsize)            , pointer :: kfv


    
    integer :: ii
!    integer                      , pointer :: u0
!    integer                      , pointer :: v0

    !type(t_d3d_state) :: d3d_state
    ! get pointer to u1 and put it in d3d_state
    
    rtur1               => gdp%gdr_i_ch%rtur1
    s1                  => gdp%gdr_i_ch%s1
    r1                  => gdp%gdr_i_ch%r1
    umnldf              => gdp%gdr_i_ch%umnldf
    vmnldf              => gdp%gdr_i_ch%vmnldf               
    u1                  => gdp%gdr_i_ch%u1
    v1                  => gdp%gdr_i_ch%v1
    w1                  => gdp%gdr_i_ch%w1
    vicuv               => gdp%gdr_i_ch%vicuv
    vicww               => gdp%gdr_i_ch%vicww
    dp                  => gdp%gdr_i_ch%dp  
      
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv

     

!  ------- CORE SUBSTATE -------------------

    call array_copy(r(u1),d3d_state(imode)%core%u,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax,1) 
    call array_copy(r(v1),d3d_state(imode)%core%v,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax,1)

    call array_copy(r(umnldf),d3d_state(imode)%core%umnldf,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub,1, 1,1) 
    call array_copy(r(vmnldf),d3d_state(imode)%core%vmnldf,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub,1, 1,1)
    call array_copy(r(s1),d3d_state(imode)%core%sep,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    call array_copy(r(dp),d3d_state(imode)%core%dp,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1)

    call array_copy(r(r1),d3d_state(imode)%core%r1,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax,gdp%d%lstsci) 
    call array_copy(r(rtur1),d3d_state(imode)%core%rtur,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 0,gdp%d%kmax,gdp%d%ltur)
    

!  ------- PSEUDO SUBSTATE -------------------

!vicuv
  call array_copy(r(vicuv),d3d_state(imode)%pseudo%vicuv,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax+2,1)


! vicww
   if (gdp%d%kmax .gt. 1) then
      call array_copy(r(vicww),d3d_state(imode)%pseudo%vicww,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 0,gdp%d%kmax,1)
   endif
 
 !  kfs,kfu,kfv
    call array_i_copy(i(kfs),d3d_state(imode)%pseudo%kfs,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    call array_i_copy(i(kfu),d3d_state(imode)%pseudo%kfu,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    call array_i_copy(i(kfv),d3d_state(imode)%pseudo%kfv,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 

 

!     rint
   call array_copy(r(gdp%gdr_i_ch%rint), d3d_state(imode)%pseudo%rint, 1,gdp%d%lstsc,1,gdp%d%nsrc,1,1,1)

   call array_copy(r(gdp%gdr_i_ch%hydrbc), d3d_state(imode)%pseudo%hydrbc,  1,4, 1,gdp%d%nto, 1,gdp%d%kcd, 1)
   call array_copy(r(gdp%gdr_i_ch%procbc), d3d_state(imode)%pseudo%procbc,  1,4, 1,gdp%d%nto, 1,gdp%d%kmax, gdp%d%lstsc)
   call array_copy(r(gdp%gdr_i_ch%disch), d3d_state(imode)%pseudo%disch,  1,gdp%d%nsrc, 1,1, 1,1, 1)

    d3d_state(imode)%pseudo%runid = gdp%runid
    d3d_state(imode)%pseudo%trifil = gdp%gdtricom%trifil     
    d3d_state(imode)%pseudo%comfil = gdp%gdtricom%comfil 
        
    d3d_state(imode)%pseudo%timestep = nint(gdp%gdinttim%timnow)   ! this is allowed since timnow is whole-valued 
                                                            ! at start of timestep
    d3d_state(imode)%pseudo%trisol_ifirst = gdp%gdtrisol%ifirst

    d3d_state(imode)%pseudo%gdinttim_itstrt = gdp%gdinttim%itstrt
    d3d_state(imode)%pseudo%gdinttim_itstop = gdp%gdinttim%itstop
    d3d_state(imode)%pseudo%gdinttim_itinit = gdp%gdinttim%itinit
    d3d_state(imode)%pseudo%gdinttim_itfinish = gdp%gdinttim%itfinish
    d3d_state(imode)%pseudo%gdinttim_ntstep = gdp%gdinttim%ntstep
    d3d_state(imode)%pseudo%gdinttim_time_nodal_update_bnd  = gdp%gdinttim%time_nodal_update_bnd
    d3d_state(imode)%pseudo%gdinttim_time_nodal_update_tgf  = gdp%gdinttim%time_nodal_update_tgf   


     
     !  ------- OUTPUT SUBSTATE -------------------
     
    d3d_state(imode)%output%itcomc = gdp%gdtricom%itcomc
    d3d_state(imode)%output%ithisi = gdp%gdinttim%ithisi
    d3d_state(imode)%output%ithisc = gdp%gdtricom%ithisc
    d3d_state(imode)%output%iphisc = gdp%gdtricom%iphisc
    d3d_state(imode)%output%ipmapc = gdp%gdinttim%ipmap(1)            
    d3d_state(imode)%output%itmapi = gdp%gdinttim%itmapi
    d3d_state(imode)%output%itmapl = gdp%gdinttim%itmapl
    d3d_state(imode)%output%itmapc = gdp%gdtricom%itmapc
    d3d_state(imode)%output%itdroi = gdp%gdinttim%itdroi
    d3d_state(imode)%output%nofou = gdp%d%nofou
    d3d_state(imode)%output%itrstc = gdp%gdtricom%itrstc               

!  recompute w1
!  fast, untidy workaround: add w1 to the pseudostate
  ! print *,'allocating w ',gdp%d%nlb,gdp%d%nub,gdp%d%mlb,gdp%d%mub,gdp%d%kmax
   if (gdp%d%kmax .gt. 1) then
     call array_copy(r(w1),d3d_state(imode)%pseudo%w,gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 0,gdp%d%kmax,1)
   endif
     
  end subroutine get_d3d_state_from_d3d

!------------------

!----------------

  subroutine set_d3d_state_to_d3d(imode)

    use precision     ! pntrsize, used in fsm.i
    use gdp_entry
    use m_d3d_state
      
    implicit none

    include 'fsm.i'
    include 'tri-dyn.igd'
 
    integer :: imode
  
    integer(pntrsize)                      , pointer :: u1
    integer(pntrsize)                      , pointer :: v1
    integer(pntrsize)                      , pointer :: w1    
    
    integer(pntrsize)                      , pointer :: vicuv 
    integer(pntrsize)                      , pointer :: vicww 
    integer(pntrsize)                      , pointer :: r1
    integer(pntrsize)                      , pointer :: s1
    integer(pntrsize)                      , pointer :: dp
    integer(pntrsize)                      , pointer :: umnldf 
    integer(pntrsize)                      , pointer :: vmnldf
    integer(pntrsize)                      , pointer :: rtur1    
    integer(pntrsize)                      , pointer :: kfs
    integer(pntrsize)                      , pointer :: kfu   
    integer(pntrsize)                      , pointer :: kfv

    
    integer :: ii
    
    rtur1               => gdp%gdr_i_ch%rtur1
    s1                  => gdp%gdr_i_ch%s1
    r1                  => gdp%gdr_i_ch%r1
    umnldf              => gdp%gdr_i_ch%umnldf
    vmnldf              => gdp%gdr_i_ch%vmnldf               
    u1                  => gdp%gdr_i_ch%u1
    v1                  => gdp%gdr_i_ch%v1
    w1                  => gdp%gdr_i_ch%w1

    vicuv               => gdp%gdr_i_ch%vicuv
    vicww               => gdp%gdr_i_ch%vicww
    dp                  => gdp%gdr_i_ch%dp  
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv



!  ------- CORE SUBSTATE -------------------

    call array_copy(d3d_state(imode)%core%u,r(u1),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax,1) 
    call array_copy(d3d_state(imode)%core%v,r(v1),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax,1)

    call array_copy(d3d_state(imode)%core%umnldf,r(umnldf),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub,1, 1,1) 
    call array_copy(d3d_state(imode)%core%vmnldf,r(vmnldf),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub,1, 1,1)
    call array_copy(d3d_state(imode)%core%sep,r(s1),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    call array_copy(d3d_state(imode)%core%dp,r(dp),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1)

    call array_copy(d3d_state(imode)%core%r1,r(r1),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax,gdp%d%lstsci) 
    call array_copy(d3d_state(imode)%core%rtur,r(rtur1),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 0,gdp%d%kmax,gdp%d%ltur)
           
        

!  ------- PSEUDO SUBSTATE -------------------

   call array_copy(d3d_state(imode)%pseudo%vicuv,r(vicuv),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,gdp%d%kmax+2,1)

   if (gdp%d%kmax .gt. 1) then
     call array_copy(d3d_state(imode)%pseudo%vicww,r(vicww),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 0,gdp%d%kmax,1)
   endif

   call array_i_copy(d3d_state(imode)%pseudo%kfs,i(kfs),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
   call array_i_copy(d3d_state(imode)%pseudo%kfu,i(kfu),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
   call array_i_copy(d3d_state(imode)%pseudo%kfv,i(kfv),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 

    !call array_copy(d3d_state(imode)%pseudo%cfurou,r(cfurou),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,3,1) 
    !call array_copy(d3d_state(imode)%pseudo%cfvrou,r(cfvrou),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,3,1) 
    !call array_copy(d3d_state(imode)%pseudo%taubpu,r(taubpu),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    !call array_copy(d3d_state(imode)%pseudo%taubsu,r(taubsu),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1)
    !call array_copy(d3d_state(imode)%pseudo%z0urou,r(z0urou),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    !call array_copy(d3d_state(imode)%pseudo%z0vrou,r(z0vrou),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1)
    !call array_copy(d3d_state(imode)%pseudo%cvalu0,r(cvalu0),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1) 
    !call array_copy(d3d_state(imode)%pseudo%cvalv0,r(cvalv0),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 1,1,1)

   call array_copy(d3d_state(imode)%pseudo%rint,r(gdp%gdr_i_ch%rint),  1,gdp%d%lstsc,1,gdp%d%nsrc,1,1,1)
   call array_copy(d3d_state(imode)%pseudo%hydrbc, r(gdp%gdr_i_ch%hydrbc),  1,4, 1,gdp%d%nto, 1,gdp%d%kcd, 1)



!vortech: do not set the increment procbc(3:4,.. since that will be 
!recomputed if timnow and itbcc do not match (see incbc)
 !  call array_copy(d3d_state(imode)%pseudo%procbc, r(gdp%gdr_i_ch%procbc),  1,4, 1,gdp%d%nto, 1,gdp%d%kmax, gdp%d%lstsc)
   call procbc_copy(d3d_state(imode)%pseudo%procbc, r(gdp%gdr_i_ch%procbc),  1,gdp%d%nto, 1,gdp%d%kmax, gdp%d%lstsc)



   call array_copy(d3d_state(imode)%pseudo%disch, r(gdp%gdr_i_ch%disch),  1,gdp%d%nsrc, 1,1, 1,1, 1)

    gdp%runid  =   d3d_state(imode)%pseudo%runid 
    gdp%gdtricom%trifil =   d3d_state(imode)%pseudo%trifil      
    gdp%gdtricom%comfil =   d3d_state(imode)%pseudo%comfil  

           
    gdp%gdinttim%timnow = real(d3d_state(imode)%pseudo%timestep, fp)   ! 
    gdp%gdtrisol%ifirst = d3d_state(imode)%pseudo%trisol_ifirst
    gdp%gdinttim%itstrt = d3d_state(imode)%pseudo%gdinttim_itstrt
    gdp%gdinttim%itstop  =d3d_state(imode)%pseudo%gdinttim_itstop
    gdp%gdinttim%itfinish=d3d_state(imode)%pseudo%gdinttim_itfinish
    gdp%gdinttim%ntstep  =d3d_state(imode)%pseudo%gdinttim_ntstep
    gdp%gdinttim%itinit  =d3d_state(imode)%pseudo%gdinttim_itinit
    gdp%gdinttim%time_nodal_update_bnd  = d3d_state(imode)%pseudo%gdinttim_time_nodal_update_bnd
    gdp%gdinttim%time_nodal_update_tgf  = d3d_state(imode)%pseudo%gdinttim_time_nodal_update_tgf    

!  ------- OUTPUT SUBSTATE -------------------
    gdp%gdtricom%itcomc = d3d_state(imode)%output%itcomc 
    gdp%gdinttim%ithisi = d3d_state(imode)%output%ithisi
    gdp%gdtricom%ithisc = d3d_state(imode)%output%ithisc
    gdp%gdtricom%iphisc = d3d_state(imode)%output%iphisc 
    gdp%gdinttim%ipmap(1) = d3d_state(imode)%output%ipmapc            
    gdp%gdinttim%itmapi = d3d_state(imode)%output%itmapi 
    gdp%gdinttim%itmapl = d3d_state(imode)%output%itmapl
    gdp%gdtricom%itmapc = d3d_state(imode)%output%itmapc
    gdp%gdinttim%itdroi = d3d_state(imode)%output%itdroi 
    gdp%d%nofou  = d3d_state(imode)%output%nofou 
    gdp%gdtricom%itrstc = d3d_state(imode)%output%itrstc  

!------------------ now: recompute secundary variables ----------------
! w1 has to be recomputed, since it is used in uzd and computed in sud  

!  fast, untidy workaround: add w1 to the pseudostate
   if (gdp%d%kmax .gt. 1) then
     call array_copy(d3d_state(imode)%pseudo%w,r(w1),gdp%d%nlb,gdp%d%nub, gdp%d%mlb,gdp%d%mub, 0,gdp%d%kmax,1)
   endif
   
!------------------------------------------------
! compute secundary variables                   !
!------------------------------------------------
   !This routine is similar to inchkr_verify
  call compute_secundary_state(gdp)
   
 !-----------------------------------------------  
    
  end subroutine set_d3d_state_to_d3d



!--------------------------------
subroutine allocate_d3d_states(nmodes ) 
  use gdp_entry
  use m_d3d_state
      
  implicit none
 
 ! input arguments 
  integer :: nmodes

!local variables
  integer :: nlb,nub,mlb,mub
  integer :: kmax
  integer :: ltur
  integer :: lstsci, lstsc
  integer :: nsrc, nto, kcd
  integer :: ierr
  integer :: imode

  nlb    = gdp%d%nlb
  nub    = gdp%d%nub
  mlb    = gdp%d%mlb
  mub    = gdp%d%mub
  kmax   = gdp%d%kmax
  ltur   = gdp%d%ltur
  lstsci = gdp%d%lstsci
  lstsc  = gdp%d%lstsc
  nsrc   = gdp%d%nsrc
  nto    = gdp%d%nto
  kcd    = gdp%d%kcd

! first allocate a total o <nmodes> states (the allocatable array d3d_state exists
! in the module m_d3d_state)

  allocate(d3d_state(nmodes),stat=ierr)

  do imode = 1, nmodes
     call allocate_d3d_state(d3d_state(imode), nlb  ,nub   ,mlb   ,mub   ,kmax, &
  &                             ltur,      lstsci,lstsc ,nsrc  ,nto   ,kcd )
  enddo

  ! add dimensions for later use (grids)
  d3d_dims(1) = nlb
  d3d_dims(2) = nub
  d3d_dims(3) = mlb
  d3d_dims(4) = mub    
  d3d_dims(5) = kmax
  d3d_dims(6) = ltur 
  d3d_dims(7) = lstsci
  d3d_dims(8) = lstsc
  d3d_dims(9) = nsrc  
  d3d_dims(10) = nto
  d3d_dims(11) = kcd
      
 end subroutine allocate_d3d_states

!----------------------------------
subroutine deallocate_d3d_states(nmodes ) 
  !use gdp_entry
  use m_d3d_state
      
  implicit none

 ! input arguments 
  integer :: nmodes

!local variables
 
  integer :: ierr
  integer :: imode

  do imode = 1, nmodes
     call deallocate_d3d_state(d3d_state(imode))
  enddo
! finally de-allocate the module variable d3d_state
  deallocate(d3d_state,stat=ierr)
  
 end subroutine deallocate_d3d_states

!----------------

subroutine define_background_state(imode)

  use m_d3d_state
  implicit none

integer :: imode, color

color = background_state
call m_d3d_adjust_output_state(d3d_state(imode),background_state,imode)

end subroutine define_background_state

!----------------------------------
subroutine d3da_define_ordinary_state(imode)

use m_d3d_state

implicit none

integer :: imode, color

color = ordinary_state
call m_d3d_adjust_output_state(d3d_state(imode),color,imode)

end subroutine d3da_define_ordinary_state

!----------------

subroutine dump_d3d_state(imode,lun,fname)

  use m_d3d_state
  implicit none

  integer :: lun,imode
   
  character(len=*) :: fname
  call m_d3d_dump_d3d_state(d3d_state(imode),lun,fname)

end subroutine dump_d3d_state
