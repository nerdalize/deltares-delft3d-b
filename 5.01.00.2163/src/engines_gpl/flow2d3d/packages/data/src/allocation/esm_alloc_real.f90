subroutine esm_alloc_real(lundia, error, gdp)
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
!  $Id: esm_alloc_real.f90 2110 2013-01-17 15:32:14Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/allocation/esm_alloc_real.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Determines memory requirements for the
!              REAL ARRAY. In this subroutine the start indices
!              of all real arrays are calculated by using the
!              memory management functions MKRPNT, MKDPNT and MKFPNT.
!              The start adress of an array can be found by using
!              the function GTRPNT.
!              Function mk*pnt will when errors occure call an
!              errorroutine (ERRPNT). The function mk*pnt will
!              return with value 1 or for memory already
!              declared with correct length with value -1
!              Because the Delft3D-FLOW module can use static
!              array declaration the error messages will stay
!              at the end of the routine
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: ncmax
    integer                 , pointer :: nmax
    integer                 , pointer :: mmax
    integer                 , pointer :: nlb
    integer                 , pointer :: nub
    integer                 , pointer :: mlb
    integer                 , pointer :: mub
    integer                 , pointer :: ddbound
    integer                 , pointer :: nmaxus
    integer                 , pointer :: kmax
    integer                 , pointer :: nmaxd
    integer                 , pointer :: mmaxd
    integer                 , pointer :: lmax
    integer                 , pointer :: lsts
    integer                 , pointer :: lstsc
    integer                 , pointer :: lstsci
    integer                 , pointer :: lsed
    integer                 , pointer :: lsedtot
    integer                 , pointer :: ltur
    integer                 , pointer :: kmxdt
    integer                 , pointer :: npiwe
    integer                 , pointer :: nbub
    integer                 , pointer :: nlcest
    integer                 , pointer :: nto
    integer                 , pointer :: kc
    integer                 , pointer :: kcd
    integer                 , pointer :: nopest
    integer                 , pointer :: nsrc
    integer                 , pointer :: nostat
    integer                 , pointer :: ntruv
    integer                 , pointer :: ntru
    integer                 , pointer :: nofou
    integer                 , pointer :: ndro
    integer                 , pointer :: nsluv
    integer                 , pointer :: nrpntr
    logical                 , pointer :: wind
    logical                 , pointer :: salin
    logical                 , pointer :: temp
    logical                 , pointer :: const
    logical                 , pointer :: drogue
    logical                 , pointer :: wave
    logical                 , pointer :: struct
    logical                 , pointer :: cdwstruct
    logical                 , pointer :: sedim
    logical                 , pointer :: zmodel
    logical                 , pointer :: roller
    logical                 , pointer :: dpmveg
    logical                 , pointer :: bubble
    real(fp), dimension(:,:), pointer :: ustokes
    real(fp), dimension(:,:), pointer :: vstokes
!
! Global variables
!
    integer                   :: lundia !  Description and declaration in inout.igs
    logical    , intent(out)  :: error  !  TRUE if an error is encountered
!
! Local variables
!
    integer           :: ierr     ! Errorflag 
    integer           :: istat
    integer           :: kfacrl   ! Multiplication factor; 1 if ROLLER='Y', else 0 
    integer           :: kfaccdw  ! Multiplication factor; 1 if CDWSTRUCT='Y', else 0 
    integer           :: kfacdpmv ! Multiplication factor; 1 if DPMV='Y', else 0 
    integer           :: kfacwv   ! Multiplication factor; 1 if WAVE=TRUE, else 0
    integer           :: kfacz    ! Multiplication factor; 1 if ZMODEL=TRUE 1, else 0
    integer           :: laak     ! Length of aak array in combination with length of rbuff array, lmax <= 1 
    integer           :: laakl    ! Length of wrkc1 array in combination with length of rbuff array, lmax > 1 
    integer           :: lmaxsed  ! max(lmax,lsedtot)
    integer           :: lrbuff   ! Length of rbuff array for writing to nefis files maximum value of rbuff nmaxus * mmax * kmax * 2 or nmaxus * mmax * kmax * lmax or 2 * nostat or 4 * ntruv (the last 2 are only theoretical the largest) 
    integer           :: mmax3d
    integer           :: mmaxddb
    integer           :: nmax3d
    integer           :: nmaxddb
    integer, external :: mkdpnt
    integer, external :: mkfpnt
    integer, external :: mkrpnt
    character(6)      :: pntnam   ! Pointername
    character(256)    :: message
!
!! executable statements -------------------------------------------------------
!
    wind       => gdp%gdprocs%wind
    salin      => gdp%gdprocs%salin
    temp       => gdp%gdprocs%temp
    const      => gdp%gdprocs%const
    drogue     => gdp%gdprocs%drogue
    wave       => gdp%gdprocs%wave
    struct     => gdp%gdprocs%struct
    cdwstruct  => gdp%gdprocs%cdwstruct
    sedim      => gdp%gdprocs%sedim
    zmodel     => gdp%gdprocs%zmodel
    roller     => gdp%gdprocs%roller
    dpmveg     => gdp%gdprocs%dpmveg
    bubble     => gdp%gdprocs%bubble
    nrpntr     => gdp%gdpointrs%nrpntr
    ncmax      => gdp%d%ncmax
    nmax       => gdp%d%nmax
    mmax       => gdp%d%mmax
    nlb        => gdp%d%nlb
    nub        => gdp%d%nub
    mlb        => gdp%d%mlb
    mub        => gdp%d%mub
    ddbound    => gdp%d%ddbound
    nmaxus     => gdp%d%nmaxus
    kmax       => gdp%d%kmax
    nmaxd      => gdp%d%nmaxd
    mmaxd      => gdp%d%mmaxd
    lmax       => gdp%d%lmax
    lsts       => gdp%d%lsts
    lstsc      => gdp%d%lstsc
    lstsci     => gdp%d%lstsci
    lsed       => gdp%d%lsed
    lsedtot    => gdp%d%lsedtot
    ltur       => gdp%d%ltur
    kmxdt      => gdp%d%kmxdt
    npiwe      => gdp%d%npiwe
    nbub       => gdp%d%nbub
    nlcest     => gdp%d%nlcest
    nto        => gdp%d%nto
    kc         => gdp%d%kc
    kcd        => gdp%d%kcd
    nopest     => gdp%d%nopest
    nsrc       => gdp%d%nsrc
    nostat     => gdp%d%nostat
    ntruv      => gdp%d%ntruv
    ntru       => gdp%d%ntru
    nofou      => gdp%d%nofou
    ndro       => gdp%d%ndro
    nsluv      => gdp%d%nsluv
    !
    ! initialize array bounderies
    !
    nmaxddb = gdp%d%nub - gdp%d%nlb + 1
    mmaxddb = gdp%d%mub - gdp%d%mlb + 1
    nmax3d  = nmaxd + 2*gdp%d%ddbound
    mmax3d  = mmaxd + 2*gdp%d%ddbound
    !
    ! set multiplication factors
    !
    kfacwv = 0
    if (wave) then
       kfacwv = 1
    endif
    !
    kfacz = 0
    if (zmodel) then
       kfacz = 1
    endif
    !
    kfacrl = 0
    if (roller) kfacrl = 1
    !
    kfaccdw = 0
    if (cdwstruct) then
       kfaccdw = 1
    endif
    !
    kfacdpmv = 0
    if (dpmveg) then
       kfacdpmv = 1
    endif
    !
    ! check 32bit/64bit:
    ! pntrsize = 4 : 32bit
    ! pntrsize = 8 : 64bit
    !
    write (message,'(a,i2,a)') 'Executable for ', 8*pntrsize, '-bits platform.'
    call prterr(lundia, 'G051', trim(message))
    !
    ! check precision:
    ! fp = sp : single precision
    ! fp = hp : double precision
    ! else    : error
    !
    if (fp == hp) then
       write (message,'(a,i4)') 'Double precision computation using reals of kind ',fp
       call prterr(lundia, 'G051', trim(message))
    elseif (fp == sp) then
       write (message,'(a,i4)') 'Single precision computation using reals of kind ',fp
       call prterr(lundia, 'G051', trim(message))
    else
       write (message,'(a,i4)') 'Unable to allocate for reals of kind ',fp
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! check precision:
    ! prec = sp : single precision
    ! prec = hp : double precision
    ! else     : error
    ! create messages in case prec /= fp
    !
    if (prec == sp) then
       if (prec /= fp) then
          write (message,'(a)') 'Array DPS is in single precision'
          call prterr(lundia, 'G051', trim(message))
       endif
    elseif (prec == hp) then
       if (prec /= fp) then
          write (message,'(a)') 'Array DPS is in double precision'
          call prterr(lundia, 'G051', trim(message))
          write (message,'(a)') 'DPS can not be viewed with Online Visualisation'
          call prterr(lundia, 'Z013', trim(message))
          write (*,'(a,a)') '*** WARNING ',trim(message)
       endif
    else
       write (message,'(a,i4)') 'Unable to allocate for reals of kind ',prec
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! arrays for: 3d information
    !
    !                           thick   (kmax                )
    !                           sig     (kmax                ) in sigma model
    !                           sig     (0:kmax              ) = zk() in z model
    !                           zwork   (kmax*5              )
    pntnam = 'thick'         !  Relative layer thickness
    ierr = mkfpnt(pntnam, kmax, gdp)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sig'           !  Sigma [-1,0] levels of layer centers (in sigma model): kmax
                             !  or Z levels of layer interfaces (in z model): kmax+1
    ierr = mkfpnt(pntnam, kmax+1, gdp)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zwork'         !  Work array for Z layer model
    ierr = mkfpnt(pntnam, kmax*5, gdp)
    if (ierr<= - 9) goto 9999
    !
    !-----array for: x,y,z position of sources
    !
    !                           xyzsrc  (3     ,nsrc         )
    pntnam = 'XYZSRC'        !  Global data
    ierr = mkfpnt(pntnam, 3*nsrc, gdp)
                             !  Pointer of array XYZSRC
                             !  Array containing the coordinates in
                             !  the X,Y,Z plane for discharges
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: boundaries
    !
    !                           alpha   (nto                 )
    !                           omega   (kc                  )
    !                           hydrbc  (4     ,nto   ,kcd   )
    !                           procbc  (4     ,nto   ,kmax  ,lstsc )
    !                           rob     (kmax  ,nopest       )
    !                           zstep   (2     ,nto   ,lstsc )
    !                           circ3d  (kmax  ,2     ,nlcest)
    !                           circ2d  (4     ,nlcest       )
    !                           rbnd    (kmax  ,lstsc ,2     ,nlcest)
    !                           rthbnd  (kmax  ,lstsc ,2     ,nlcest)
    !                           thtim   (kmax  ,lstsc ,2     ,nlcest)
    !                           rettim  (nto   ,lstc  ,2     )
    !                           qtfrac  (nopest)
    !                           qtfrct  (nto   )
    !                           qtfrt2  (nto   )
    pntnam = 'alpha'         !  Global data
    ierr = mkfpnt(pntnam, nto, gdp)
                             !  Pointer of array ALPHA
                             !  HLES parameter; slope of the energy
                             !  density spectrum
                             !  Weakly reflective coefficient
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'omega'         !  Global data
    ierr = mkfpnt(pntnam, kc, gdp)
                             !  Pointer of array OMEGA
                             !  Frequencies of the Hydrod. forcings
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hydrbc'        !  Global data
    ierr = mkfpnt(pntnam, 4*nto*kcd, gdp)
                             !  Hydrodynamic bound. val. at MNBND sections.
                             !  The array is completely filled when the
                             !  H-type boundary is considered.
                             !  When time series is considered, the array is
                             !  only relevant for index K=1.
                             !               H-signal  t-signal
                             !               --------  --------
                             !  HYDRBC(1,N,K)= AmplA  AmplA
                             !                        at curr. time
                             !  HYDRBC(2,N,K)= AmplB  AmplB
                             !                        at curr. time
                             !  HYDRBC(3,N,K)= FaseA  AmplA incr. at
                             !                        subeseq. time
                             !  HYDRBC(4,N,K)= FaseB  AmplB incr. at
                             !                        subeseq. time
                             !  K = 1,.,KC & N = 1,.,NTOF (H)
                             !  K = 1,.,KMAX & N = NTOF+1,.,NTO (T)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'procbc'        !  Global data
    ierr = mkfpnt(pntnam, 4*nto*kmax*lstsc, gdp)
                             ! Boundary val. for constituents at
                             !  opening sections (time series).
                             !  1,N,K,L = value at a
                             !  2,N,K,L = value at b
                             !  3,N,K,L = time incr. at a
                             !  4,N,K,L = time incr. at b
                             !  N=1,.,NTO; K=1,.,KMAX & L=1,.,LSTSC;
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rob'           !  Global data
    ierr = mkfpnt(pntnam, kmax*nopest, gdp)
                             !  Hulp array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zstep'         !  Global data
    ierr = mkfpnt(pntnam, 2*nto*lstsc, gdp)
                             !  Pointer of array ZSTEP
                             !  Time varying location of disconti-
                             !  nuity for the 3D BC for constituents
                             !  old and new time in index 1 and 2
                             !  Time varying location of disconti-
                             !  nuity for the 3D BC for constituents
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'circ3d'        !  Global data
    ierr = mkfpnt(pntnam, kmax*2*nlcest, gdp)
                             !  Pointer of array CIRC3D
                             !  Array with 3D Boundary Conditions
                             ! 
                             !  Array with 3D Boundary Conditions
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'circ2d'        !  Global data
    ierr = mkfpnt(pntnam, 4*nlcest, gdp)
                             !  Pointer of array CIRC2D
                             !  Array with Boundary Conditions and
                             !  Reflection coefficients hydrodynamics
    if (ierr<= - 9) goto 9999
    !
    !    MAX(lstsc,1) is applied to avoid undefined rbnd pointer in tritra when calling difu*, when lstsc is 0.
    pntnam = 'rbnd'          !  Global data
    ierr = mkfpnt(pntnam, kmax*max(lstsc, 1)*2*nlcest, gdp)
                             !  Pointer of array RBND
                             ! 
                             !  Array containing the actual Boundary
                             !  Conditions for constituents
                             !  Array containing the return time for
                             !  constituents at each bnd. point
                             !  n,1; return time at surface
                             !  n,2; return time at bottom
                             ! 
                             !  Array with Boundary Conditions for
                             !  constituents
                             ! 
                             !  Boundary values for constituents at
                             !  each open boundary point (after in-
                             !  terpolation in space has been
                             !  carried out). 2-nd and 3-rd index in
                             !  the array points to IROCOL).
                             !  RBND(K,L,I,N) = AMPL
                             !  L=1,.,LSTSC & I=1/2 & N=1,.,NOROCO
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rthbnd'        !  Global data
    ierr = mkfpnt(pntnam, kmax*lstsc*2*nlcest, gdp)
                             !  Pointer of array RTHBND
                             ! 
                             !  Array containing the last outflow
                             !  (Th.Harleman) value of Boundary
                             !  Conditions for constituents
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'thtim'         !  Actual Thatcher Harleman time
    ierr = mkfpnt(pntnam, kmax*lstsc*2*nlcest, gdp)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rettim'        !  User defined delay for Thatcher Harleman return time
    ierr = mkfpnt(pntnam, 2*nto*lstsc, gdp)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qtfrac'        !  Global data
    ierr = mkfpnt(pntnam, nopest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qtfrct'        !  Global data
    ierr = mkfpnt(pntnam, nto, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qtfrt2'        !  Global data
    ierr = mkfpnt(pntnam, nto, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! for Riemann-Van Dongeren-Svendsen left bc incoming signal (begin)
    !
    ! left boundary
    !
    pntnam = 'ctif'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'stif'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zetabf'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ctbf'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'stbf'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'encgf'         !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cgdghf'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wenf'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wenfm'         !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zbmnf'         !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zetaif'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ctrf'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umeanf'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zmeanf'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! right boundary
    !
    pntnam = 'ctil'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'stil'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zetabl'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ctbl'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'stbl'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'encgl'         !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cgdghl'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wenl'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wenlm'         !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zbmnl'         !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zetail'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ctrl'          !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umeanl'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zmeanl'        !  Global data
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'crbc'          !  Global data
    ierr = mkfpnt(pntnam, 24*nlcest, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! for Riemann-Van Dongeren-Svendsen bc incoming signal (end)
    !
    !
    !-----arrays for: depths
    !
    !                           dp    (nmaxddb  ,mmaxddb)
    !                           dpu   (nmaxddb  ,mmaxddb)
    !                           dpv   (nmaxddb  ,mmaxddb)
    !                           dps   (nmaxddb  ,mmaxddb)
    !                           hkru  (nmaxddb  ,mmaxddb)
    !                           hkrv  (nmaxddb  ,mmaxddb)
    !                           hu    (nmaxddb  ,mmaxddb)
    !                           hu0   (nmaxddb  ,mmaxddb)
    !                           hv    (nmaxddb  ,mmaxddb)
    !                           hv0   (nmaxddb  ,mmaxddb)
    pntnam = 'dp'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DP
                             !  Depth value at depth points
                             !  only if NOUI = .true. real value
                             !  Depth value at depth points
                             !  depth value at depth points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dpu'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DPU
                             !  Depth value at u-points incl. crest
                             !  height (only for general 3D weir)
                             !  Depth value at u-points incl. crest
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dpv'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DPV
                             !  Depth value at v-points incl. crest
                             !  height (only for general 3D weir)
                             !  Depth value at v-points incl. crest
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dps'           !  Global data
    if (prec == sp) then
       ierr = mkrpnt(pntnam, nmaxddb*mmaxddb, gdp)
    elseif (prec == hp) then
       ierr = mkdpnt(pntnam, nmaxddb*mmaxddb, gdp)
    else
       ! catched at top of esm_alloc_real.f90
    endif
                             !  Pointer of array DPS
                             !  Depth at waterlevel points
                             !
                             !  Depth value at zeta points
                             !  Depth value at zeta point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hkru'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array HKRU
                             !  Crest height in u points
                             !  Vertical position crest of U-weir
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hkrv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array HKRV
                             !  Crest height in v points
                             !  Vertical position crest of U-weir(2D)
                             !  Vertical position crest of V-weir(2D)
                             !  Vertical position crest of V-weir
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hu'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Total water depth in u-point [m]
                             !  Upwind approach in
                             !  a. points with HU below 5*dryflc
                             !  b. barrier points.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hu0'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Total water depth in u-point [m] in previous time step
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hv'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array HV
                             !  Total water depth in v-point [m]
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hv0'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Total water depth in v-point [m] in previous time step
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: initial field values
    !
    !                           s1    (nmaxddb ,mmaxddb)
    !                           s0    (nmaxddb ,mmaxddb)
    !                           u1    (nmaxddb ,mmaxddb,kmax  )
    !                           u0    (nmaxddb ,mmaxddb,kmax  )
    !                           umean (nmaxddb ,mmaxddb)
    !                           v1    (nmaxddb ,mmaxddb,kmax  )
    !                           v0    (nmaxddb ,mmaxddb,kmax  )
    !                           vmean (nmaxddb ,mmaxddb)
    !                           w1    (nmax3d,-1:mmax3d+2,0:kmax)
    !                           r1    (nmaxddb ,mmaxddb,kmax ,lstsci)
    !                           r0    (nmaxddb ,mmaxddb,kmax ,lstsci)
    !                           rtur1 (nmaxddb ,mmaxddb,0:kmax,ltur )
    !                           rtur0 (nmaxddb ,mmaxddb,0:kmax,ltur )
    !                           decay (lstsc )
    pntnam = 's1'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array S1
                             !  Zeta at NEW time level
                             !  Zeta at new time level
                             !  Zeta at old time level
                             !  Zeta
    if (ierr<= - 9) goto 9999
    !
    pntnam = 's0'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array S0
                             !  Zeta at old time level
                             !  Zeta in old time level
                             !  Depth (output) value at vel. points
                             !  zeta at old time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'u1'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array U1
                             !  U-velocities at new time level
                             !  U-velocities at old time level
                             !  U-velocities new time step
                             ! 
                             !  U-velocities at new time level
                             ! called as WRKB3(4)
                             !  U-velocities at new time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'u0'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array U0
                             !  Horizontal u-velocity component on
                             !  hydrodynamic grid (zeta point)
                             !  U-velocities at old time level
                             !  fluid mud layer
                             !  U-velocities at old time level
                             !  U-velocities mud at old time level
                             ! 
                             !  U-velocities at old time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umean'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array UMEAN
                             !  Depth-average of u-velocity component
                             !  Depth-averaged U-velocities
                             !  Mean horizontal velocity
                             !  depth-averaged U-velocities
                             !  at old time level
                             !  depth-averaged U-velocities
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'v1'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array V1
                             !  V-velocities at new time level
                             !  V-velocities at new time level
                             !  V-velocities at new time level
                             !  V-velocities at old time level
                             !  V-velocities new time step
                             ! 
                             !  V-velocities at new time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'v0'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array V0
                             !  Horizontal v-velocity component on
                             !  hydrodynamic grid (zeta point)
                             !  V-velocities at old time level
                             !  fluid mud layer
                             !  V-velocities at old time level
                             !  V-velocities mud at old time level
                             ! 
                             !  V-velocities at old time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vmean'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array VMEAN
                             !  Depth-average of v-velocity component
                             !  Depth-averaged V-velocities
                             !  Mean horizontal velocity
                             !  depth-averaged U-velocities
                             !  at old time level
                             !  depth-averaged V-velocities
                             !  at old time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'w1'            !  Global data
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
                             !  Pointer of array W1
                             !  = WSTCOF(1)
                             ! 
                             !  Omega-velocity at new time level
                             !  (velocity relative to sigma-plane)
                             ! 
                             !  Omega-velocity at old time level
                             !  (velocity relative to sigma-plane)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'r1'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
                             !  Pointer of array R1
                             ! 
                             !  Concentrations at new time level
                             !  (before and after filter)
                             ! 
                             !  Concentrations at new time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'r0'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
                             !  Pointer of array R0
                             ! 
                             !  Array for concentration (including
                             !  sediment)
                             ! 
                             !  Concentrations at old time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rtur1'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*ltur, gdp)
                             !  Pointer of array RTUR1
                             ! 
                             !  Concentrations turbulent energy and
                             !  dissipation at new time level
                             ! 
                             !  Concentrations turbulent energy and
                             !  dissipation at old time level
                             ! 
                             !  Turbulence at new time level
                             !  (before and after filter)
    if (ierr== - 1) goto 9999
    !
    pntnam = 'rtur0'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*ltur, gdp)
                             !  Pointer of array RTUR0
                             !  Turbulent kinetic energy (0:kmax,1)
                             !  and its dissipation rate (0:kmax,2)
                             !  Turbulent kinetic energy and its dis-
                             !  sipation rate
                             ! 
                             !  Concentrations turbulent energy and
                             !  dissipation at old time level
                             ! 
                             !  Turbulence at old time level
    if (ierr== - 1) goto 9999
    !
    pntnam = 'decay'         !  Global data
    ierr = mkfpnt(pntnam, lstsc, gdp)
                             !  Pointer of array DECAY
                             !  Decay rates per day as input
                             !  divided by 86400. in RDIC
    if (ierr<= - 9) goto 9999
    !
    !
    !-----arrays for: subgrid viscosity model
    !
    !                           umnldf(nmaxddb  ,mmaxddb)
    !                           umnflc(nmaxddb  ,mmaxddb)
    !                           vmnldf(nmaxddb  ,mmaxddb)
    !                           vmnflc(nmaxddb  ,mmaxddb)
    !                           vortic(nmaxddb  ,mmaxddb,kmax)
    !                           enstro(nmaxddb  ,mmaxddb,kmax)
    pntnam = 'umnldf'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Depth-averaged low-pass filtered
                             !  U-velocities
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umnflc'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Depth-averaged fluctuating
                             !  U-velocities
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vmnldf'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Depth-averaged low-pass filtered
                             !  V-velocities
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vmnflc'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Depth-averaged fluctuating
                             !  V-velocities
    !
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vortic'        !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'enstro'        !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: roughness, viscosity and diffusity field values
    !
    !                           cfurou(nmaxddb  ,mmaxddb,3)
    !                           cfvrou(nmaxddb  ,mmaxddb,3)
    !                           cvalu0(nmaxddb  ,mmaxddb)
    !                           cvalv0(nmaxddb  ,mmaxddb)
    !                           vicuv (nmaxddb  ,mmaxddb,kmax+2)
    !                           vicww (nmax3d ,-1:mmax3d+2,0:kmax(>1))
    !                           dicuv (nmaxddb  ,mmaxddb,kmax+2)
    !                           dicww (nmax3d ,-1:mmax3d+2,0:kmax(>1))
    pntnam = 'cfurou'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*3, gdp)
                             !  2DH : array containing space depen-
                             !        dent coefficients in roughness
                             !        method in U-point
                             !        Manning         for ROUFLO=MANN
                             !        Chezy           for ROUFLO=CHEZ
                             !        White Colebrook for ROUFLO=WHIT
                             !  3D  : array containing
                             !        ratio magnitude velocity/ustar
                             !        in U-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cfvrou'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*3, gdp)
                             !  2DH : array containing space depen-
                             !        dent coefficient in roughness
                             !        method in V-point
                             !        Manning         for ROUFLO=MANN
                             !        Chezy           for ROUFLO=CHEZ
                             !        White Colebrook for ROUFLO=WHIT
                             !  3D  : array containing
                             !        ratio magnitude velocity/ustar
                             !        in V-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cvalu0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cvalv0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vicuv'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 2), gdp)
                             !  Horizontal eddy visc. coefficient
                             !  [m2/s] (in density point)
                             !  Background value will be set in layer kmax+1
                             !  Horizontal eddy value (HLES or roller) will be set in layer kmax+2
  if (ierr<= - 9) goto 9999
    !
    pntnam = 'vicww'         !  Global data
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
                             !  Vertial eddy viscosity coefficient
                             !  [m2/s] (at interface between layer k
                             !  and k+1)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dicuv'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 2), gdp)
                             !  Horizontal Diffusion coeff. [m2/s]
                             !  Background value will be set in layer kmax+1
                             !  Horizontal eddy value (HLES or roller) will be set in layer kmax+2
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dicww'         !  Global data
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
                             !  Pointer of array DICWW
                             !  Vertical   Diffusion coeff. [m2/s]
                             !  Used to calculate Z0
                             ! 
                             !  Vertical   Diffusion coeff. [m2/s]
                             ! Vertical   Diffusion coeff. [m2/s]
                             !  at layer interfaces
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: grid information
    !
    !                           guu   (nmaxddb  ,mmaxddb)
    !                           gvv   (nmaxddb  ,mmaxddb)
    !                           gsqs  (nmaxddb  ,mmaxddb)
    !                           gsqd  (nmaxddb  ,mmaxddb)
    !                           alfas (nmaxddb  ,mmaxddb)
    !                           guv   (nmaxddb  ,mmaxddb)
    !                           gvu   (nmaxddb  ,mmaxddb)
    !                           xcor  (nmaxddb  ,mmaxddb)
    !                           ycor  (nmaxddb  ,mmaxddb)
    !                           xz    (nmaxddb  ,mmaxddb)
    !                           yz    (nmaxddb  ,mmaxddb)
    !                           wphy  (nmaxddb  ,mmaxddb,kmax    )
    !                           guz   (nmaxddb  ,mmaxddb)
    !                           gvz   (nmaxddb  ,mmaxddb)
    !                           gud   (nmaxddb  ,mmaxddb)
    !                           gvd   (nmaxddb  ,mmaxddb)
    !                           gsqiu (nmaxddb  ,mmaxddb)
    !                           gsqiv (nmaxddb  ,mmaxddb)
    !
    pntnam = 'guu'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GUU
                             !  Grid distance in the ETA-/Y-direction
                             !  at U-velocity point
                             !  Grid distance in the eta-/y-dir. at
                             !  u-velocity point
                             !  Grid distance in the eta-/y-direction
                             !  at u-velocity point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gvv'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GVV
                             !  Grid distance in the KSI-/X-direction
                             !  at V-velocity point
                             !  Grid distance in the ksi-/x-dir. at
                             !  v-velocity point
                             !  Grid distance in the ksi-/x-direction
                             !  at v-velocity point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gsqs'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GSQS
                             !  Area of computational cell defined at
                             !  zeta point
                             !  Area of computational cell defined
                             !  at zeta point.
                             !  Reduced in case of rigid lid
                             !  approximation.
                             !  Area of computational cell defined
                             !  at zeta point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gsqd'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GSQD
                             !  Area of a cell defined at the depth
                             !  point (Not yet used)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'alfas'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array ALFAS
                             !  Array containing the orientation
                             !  of each computational cell defined
                             !  as the angle formed by the line
                             !  spanned by the u-velocity points
                             !  around the zeta point and the x-axis
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'guv'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GUV
                             !  Grid distance in the eta-/y-direction
                             !  at v-velocity point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gvu'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GVU
                             !  Grid distance in the ksi-/x-direction
                             !  at u-velocity point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'xcor'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array XCOR
                             !  X-coord. of the depth point
                             !  x-coord. of the depth point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ycor'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array YCOR
                             !  Y-coord. of the depth point
                             !  Current only Z0 values (for use in EROSED)
                             !  Y-coord. of the depth point
                             !  y-coord. of the depth point
                             !
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'xz'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array XZ
                             !  X-coord. of the water elevation pnt.
                             !  X-coord. of the water-elevation pnt.
                             !  X-coord. of the zeta  point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'yz'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array YZ
                             !  X-coord. of the zeta  point
                             !  Y-coord. of the water elevation pnt.
                             !  Y-coord. of the water-elevation pnt.
                             !  Y-coord. of the zeta  point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wphy'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array WPHY
                             !  Physical w-velocity at new time level
                             !  (in fixed coordinate system)
                             !  Physical w-velocity at new time level
                             ! 
                             !  Physical w-velocity at new time level
                             !  (in fixed coordinate system)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'guz'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gvz'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gud'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gvd'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gsqiu'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gsqiv'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: flows
    !
    !                           qxk   (nmaxddb  ,mmaxddb ,kmax  )
    !                           qyk   (nmaxddb  ,mmaxddb ,kmax  )
    !                           qzk   (nmax3d ,-1:mmax3d+2,0:kmax)
    pntnam = 'qxk'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array QXK
                             !  Discharges for layer k in the X-dir.
                             !  in U-velocity point at following time
                             !  integration continuity equation
                             ! 
                             !  Discharges for layer k in the X-dir.
                             !  in U-velocity point at following time
                             !  integration continuity equation
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qyk'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array QYK
                             !  Discharges for layer k in the Y-dir.
                             !  in V-velocity point at following time
                             !  integration continuity equation
                             ! 
                             !  Discharges for layer k in the Y-dir.
                             !  in V-velocity point at following time
                             !  integration continuity equation
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qzk'           !  Global data
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
                             !  Pointer of array QZK
                             ! 
                             !  Discharges at layer interface k+1/2
                             !  in the Z-direction following time in-
                             !  tegration continuity equation
                             ! 
                             !  Discharges at layer interface k+1/2
                             !  in the Z-direction following
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: Coriolis
    !
    !                           fcorio(nmaxddb  ,mmaxddb)
    pntnam = 'fcorio'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array FCORIO
                             !  Coriolis Force (currently uniform in space)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for drogues; dxydro and xydro
    !
    !                           dxydro(2,ndro  )
    !                           xydro (2,ndro  )
    pntnam = 'dxydro'        !  Global data
    ierr = mkfpnt(pntnam, 2*ndro, gdp)
                             !  Pointer of array DXYDRO
                             !  Delta X(1)Y(2)-coordinate correspon-
                             !  ding to drogue starting point
                             !  0. := left/lower cell boundary
                             !  1. := right/upper cell boundary
                             !  Delta X(1)Y(2)-coordinate correspon-
                             !  ding to drogue track starting point
                             !  0. := left/lower cell boundary
                             !  1. := right/upper cell boundary
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'xydro'         !  Global data
    ierr = mkfpnt(pntnam, 2*ndro, gdp)
                             !  Pointer of array XYDRO
                             !   X(1)Y(2)-coordinate corresponding to
                             !  drogue starting point if
                             !  track is calculated else 999.999
                             !   X(1)Y(2)-coordinate corresponding to
                             !  drogue track starting point if
                             !  track is calculated else 999.999
                             !  X(1)Y(2)-coordinate corresponding to
                             !  drogue starting point if
                             !  track is calculated else 999.999
    if (ierr<= - 9) goto 9999
    pntnam = 'drodep'        !  Global data
    ierr = mkfpnt(pntnam, ndro, gdp)
                             !  Depth of each drogue under water level
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for barriers;
    !
    !                           cbuv  (2,nsluv   )
    !                           cbuvrt(2,nsluv   )
    pntnam = 'cbuv'          !  Global data
    ierr = mkfpnt(pntnam, 4*nsluv, gdp)
                             !  Pointer of array CBUV
                             !  Pointer of array CBUV
                             !  Pointer of array CBUV
                             !  Real barrier data:
                             !  CBUV(1,*) = Loss coefficient
                             !  CBUV(2,*) = Initial gate height
    if (ierr<= - 9) goto 9999
    pntnam = 'cbuvrt'        !  Global data
    ierr = mkfpnt(pntnam, 2*nsluv, gdp)
                             !  Pointer of array CBUVRT
                             !  Run time barrier data:
                             !  CBUVRT(1,*) = Return status from RTC
                             !              > 0 : OK
                             !              < 0 : Not OK/Found
                             !  CBUVRT(2,*) = Value from RTC
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: structure information
    !
    !                           pship    (nmaxddb  ,mmaxddb)
    !                           ubrlsu   (nmaxddb  ,mmaxddb,kmax)
    !                           ubrlsv   (nmaxddb  ,mmaxddb,kmax)
    !                           uwtypu   (nmaxddb  ,mmaxddb)
    !                           uwtypv   (nmaxddb  ,mmaxddb)
    !                           dteu     (nmaxddb  ,mmaxddb)
    !                           dtev     (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwlsu   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwlsv   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwztu   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwzbu   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwztv   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwzbv   (nmaxddb  ,mmaxddb)
    !                           porosu   (nmaxddb  ,mmaxddb, kmax)
    !                           porosv   (nmaxddb  ,mmaxddb, kmax)
    !                           areau    (nmaxddb  ,mmaxddb, kmax)
    !                           areav    (nmaxddb  ,mmaxddb, kmax)
    pntnam = 'pship'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array PSHIP
                             !  Depth value (input),pressure (output)
                             !  Depth value at depth points
                             !  only if NOUI = .true. real value
                             !  Depth value floating structure
                             !  Later re-defined as pressure
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ubrlsu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array UBRLSU
                             !  Roughness coefficient in x-dir.
                             !  Wall roughness in x-dir.
                             ! 
                             !  Roughness coefficient in x-dir.
                             ! 
                             !  Wall roughness in x-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ubrlsv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array UBRLSV
                             !  Roughness coefficient in y-dir.
                             !  Wall roughness in y-dir.
                             ! 
                             !  Roughness coefficient in y-dir.
                             ! 
                             !  Wall roughness in y-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'uwtypu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array UWTYPU
                             !  Type of weir in x-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'uwtypv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array UWTYPV
                             !  Type of weir in x-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dteu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DTEU
                             !  Subgrid energy loss due to 2D weir
                             !  in U-points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dtev'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DTEV
                             !  Subgrid energy loss due to 2D weir
                             !  in V-points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cdwztu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
                               !  z co-ordinate of the top point of the fixed gate/cdw
                               !  in u point of the cell
                               !  [m]
                               !  Only used in cells with KSPU(NM,0) = 10
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cdwzbu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
                               !  z co-ordinate of the bottom point of the fixed gate/cdw
                               !  in u point of the cell
                               !  [m]
                               !  Only used in cells with KSPU(NM,0) = 10
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cdwztv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
                               !  z co-ordinate of the top point of the fixed gate/cdw
                               !  in v point of the cell
                               !  [m]
                               !  Only used in cells with KSPV(NM,0) = 10
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cdwzbv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
                               !  z co-ordinate of the bottom point of the fixed gate/cdw
                               !  in v point of the cell
                               !  [m]
                               !  Only used in cells with KSPV(NM,0) = 10
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cdwlsu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
                               !  loss coefficient of the fixed gate/cdw
                               !  in u point of the cell
                               !  [???]
                               !  Only used in cells with KSPU(NM,0) = 10
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'cdwlsv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
                               !  loss coefficient of the fixed gate/cdw
                               !  in v point of the cell
                               !  [???]
                               !  Only used in cells with KSPV(NM,0) = 10
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'porosu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                               !  porosity of cell in u point
                               !  [-]
                               !  interval [0.0 - 1.0] 
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'porosv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                               !  porosity of cell in v point
                               !  [-]
                               !  interval [0.0 - 1.0] 
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'areau'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                               !  area of cell in u point
                               !  [m2]
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'areav'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                               !  area of cell in v point
                               !  [m2]
    if (ierr<= - 9) goto 9999
    !
    !-----additional arrays for sediment transport
    !
    !                           rsedeq(nmaxddb  ,mmaxddb,kmax  ,lsed  )
    !                           ws    (nmaxddb  ,mmaxddb,kmax+1,lsed  )
    !                           rhowat(nmaxddb  ,mmaxddb,kmax)
    !                           depchg(nmaxddb  ,mmaxddb)
    !                           sbu   (nmaxddb  ,mmaxddb,lsedtot)
    !                           sbv   (nmaxddb  ,mmaxddb,lsedtot)
    !                           epscur(0:kmax)
    !                           epswav(0:kmax)
    !                          aks   (nmaxddb  ,mmaxddb)
    !                          sbuu  (nmaxddb  ,mmaxddb,lsedtot)
    !                          sbvv  (nmaxddb  ,mmaxddb,lsedtot)
    !                          ssuu  (nmaxddb  ,mmaxddb,lsed)
    !                          ssvv  (nmaxddb  ,mmaxddb,lsed)
    !
    pntnam = 'rsedeq'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lsed, gdp)
                             !  Pointer of array RSEDEQ
                             ! 
                             !  Equilibrium sediment concentration
                             ! 
                             !  Equilibrium sediment concentration
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ws'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*lsed, gdp)
                             !  Pointer of array WS
                             !  Fall velocity
                             !
                             !  Fall velocity
                             !  Wave stresses (in the x-/y-direction)
                             ! 
                             !  Fall velocity (dependent on sediment
                             !  type)
                             ! 
                             !  Settling velocity (concentration
                             !  dependent)
                             ! Settling velocity (concentration
                             !  dependent)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rhowat'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array RHOWAT
                             !  Array with water densities [kg/m3]
                             !  Pointer of array RHOWAT
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'seddif'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*lmax, gdp)
                             !  Vertical sediment diffusion coeff.
                             !  Pointer of array SEDDIF
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'depchg'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'epscur'        !  Global data
    ierr = mkfpnt(pntnam, kmax + 1, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'epswav'        !  Global data
    ierr = mkfpnt(pntnam, kmax + 1, gdp)
                             !  dissipation at surface (breaking waves)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'aks'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sbuu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsedtot, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sbvv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsedtot, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ssuu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ssvv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rca'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zrca'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dss'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'facdss'        !  Global data
    ierr = mkfpnt(pntnam, lsed, gdp)
                             !  ratio of suspended sediment diameter and
                             !  diameter specified in sed-file
    if (ierr<= - 9) goto 9999
    !
    !
    !     Arrays for: fluid mud; driving forces for fluid mud layer
    !                   usus, vsus, czusus, czvsus, wssus, entr, rsed
    !                   excbed and soumud
    !
    !
    pntnam = 'usus'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  U-velocities at old time level
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     usus_mud = u0(kmax)_water
    if (ierr<= - 9) goto 9999
    pntnam = 'vsus'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  V-velocities at old time level
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     vsus_mud = v0(kmax)_water
    if (ierr<= - 9) goto 9999
    pntnam = 'czusus'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  U-chezy coefficients
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     czusus_mud = cfurou(2)_water
    if (ierr<= - 9) goto 9999
    pntnam = 'czvsus'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  V-chezy coefficients
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     czvsus_mud = cfvrou(2)_water
    if (ierr<= - 9) goto 9999
    pntnam = 'wssus'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Fall velocity
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     wssus_mud = wstau_water
    if (ierr<= - 9) goto 9999
    pntnam = 'wstau'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Fall velocity at k=kmax
    if (ierr<= - 9) goto 9999
    pntnam = 'entr'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Entrainment mud --> suspension layer
                             !  Only used in water fraction of fluid mud calculation
    if (ierr<= - 9) goto 9999
    pntnam = 'rsed'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Concentration of sediment(1) at old time level
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     rsed_mud = r0(kmax,l1)_water
                             !  WARNING: only the first sediment type is communicated!!!
    if (ierr<= - 9) goto 9999
    pntnam = 'soumud'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Growth speed mud layer (m/s)
                             !  >0 source(>0) or sink(<0) for mud layer
                             !  source term for combined effect of
                             !  erosion/dewater (exchange) and settling/entrainment
                             !  Only used in mud fraction of fluid mud calculation
    if (ierr<= - 9) goto 9999
    pntnam = 'excbed'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Exchange bed / mud layer
                             !  Only used in mud fraction of fluid mud calculation
    if (ierr<= - 9) goto 9999
    pntnam = 'sepsus'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Water level  at old time level of water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     sepsus_mud = s0_water
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: time dependent discharges
    !
    !                        disch (nsrc  )
    !                        disch0(nsrc  )
    !                        disch1(nsrc  )
    !                        rint  (lstsc ,nsrc      )
    !                        rint0 (lstsc ,nsrc      )
    !                        rint1 (lstsc ,nsrc      )
    !                        umdis (nsrc  )
    !                        umdis0(nsrc  )
    !                        umdis1(nsrc  )
    !                        vmdis (nsrc  )
    !                        vmdis0(nsrc  )
    !                        vmdis1(nsrc  )
    !                        sink  (nmaxddb  ,mmaxddb,kmax ,lstsci)
    !                        sour  (nmaxddb  ,mmaxddb,kmax ,lstsci)
    pntnam = 'disch'         !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array DISCH
                             !  Array with discharge values [m3/sec]
                             !  At most MXDIST time varying dis-
                             !  charges at discharge points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'disinp'
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Copy of array DISCH
                             !  Contains input values (required for bubble screens)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'disnf'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DISNF
                             !  Array with discharge values [m3/sec]
                             !  Following from near field model
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'voldis'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array VOLDIS
                             !  Array with total volume discharged by discharge [m3]
    if (ierr<= - 9) goto 9999    
    !
    pntnam = 'disch0'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array DISCH1
                             !  Old discharge value
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'disch1'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array DISCH1
                             !  New discharge value
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rint'          !  Global data
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
                             !  Pointer of array RINT
                             !  Concentration at discharge points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rint0'         !  Global data
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
                             !  Pointer of array RINT0
                             !  Old concentration value at discharge points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rint1'         !  Global data
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
                             !  Pointer of array RINT1
                             !  New concentration value at discharge points
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umdis'         !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array UMDIS
                             !  Velocity for discharges (in the
                             !  x-dir)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umdis0'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array UMDIS0
                             !  Old time velocity for
                             !  discharges (in the x-dir)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'umdis1'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array UMDIS1
                             !  New time velocity for
                             !  discharges (in the x-dir)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vmdis'         !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array VMDIS
                             !  Velocity for discharges (in the
                             !  y-dir)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vmdis0'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array VMDIS0
                             !  Old time velocity for
                             !  discharges (in the y-dir)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'vmdis1'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array VMDIS1
                             !  New time velocity for
                             !  discharges (in the y-dir)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sink'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
                             !  Pointer of array SINK
                             ! 
                             !  Array with sink   terms
                             !  (added to main diagonal   BBKL)
                             ! 
                             !  Array with sink   terms
                             !  (added to main diagonal   bbkl)
                             ! 
                             !  Array with sink   terms
                             ! 
                             !  Array with sink terms sediment
                             !  (added to main diagonal   BBKL)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sour'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
                             !  Pointer of array SOUR
                             ! 
                             !  Array with source terms sediment
                             !  (added to Right Hand Side DDKL)
                             ! 
                             !  Array with source terms
                             !  (added to Right Hand Side DDKL)
                             !  Array with source terms for sediment
                             ! 
                             !  Array with source terms
                             !  (added to Right Hand Side DDKL)
                             ! 
                             !  Array with source terms
                             !  (added to right hand side ddkl)
                             ! 
                             !  Array with source terms
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sournf'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
                             !  Pointer of array SOURNF
                             !
                             !  Sources for near field model
                             !  (added to Right Hand Side DDKL)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: time dependent wind fields
    !
    !                        windu (nmaxddb  ,mmaxddb)
    !                        windsu(nmaxddb  ,mmaxddb)
    !                        windv (nmaxddb  ,mmaxddb)
    !                        windsv(nmaxddb  ,mmaxddb)
    !                        patm  (nmaxddb  ,mmaxddb)
    !                        w10mag(nmaxddb  ,mmaxddb)
    !                        evap  (nmaxddb  ,mmaxddb)
    pntnam = 'windu'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WINDU
                             !  Current wind velocity in the ksi-dir.
                             !  Interface Stress in the x-dir.
                             !  Wind force in the x-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'windsu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WINDSU
                             !  Wind force in the x-dir.
                             !  Wind stress in the ksi-dir.
                             !  Wind stress in the x-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'windv'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WINDV
                             !  Current wind velocity in the eta-dir.
                             !  Wind force in the y-dir.
                             !  Wind velocity in the eta-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'windsv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WINDSV
                             !  Wind force in the y-dir.
                             !  Wind stress in the eta-dir.
                             !  Wind stress in the y-dir.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'patm'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array PATM
                             !  Atmosferic pressure in N/m2
                             !  Current atmosferic pressure in N/m2
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'w10mag'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array W10MAG
                             !  Magnitude of Wind at 10 M
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'evap'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array EVAP
                             !  Evaporation in kg/m2s (*RHOW)
                             !  Evaporation in m/s
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'precip'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array EVAP
                             !  Precipitation in m/s
    if (ierr<= - 9) goto 9999
    !
    !
    !-----arrays for: diffusion, concentrations
    !
    !                        sigdif(lstsci )
    !                        sigmol(lstsci )
    !                        rmneg (lstsci )
    pntnam = 'sigdif'        !  Global data
    ierr = mkfpnt(pntnam, lstsci, gdp)
                             !  Pointer of array SIGDIF
                             !  Prandtl/schmidt-numbers for const.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sigmol'        !  Global data
    ierr = mkfpnt(pntnam, lstsci, gdp)
                             !  Pointer of array SIGMOL
                             !  Moleculair Prandtl numbers for const.
                             !  Moleculair Prandtl-numbers for const.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rmneg'         !  Global data
    ierr = mkfpnt(pntnam, lstsci, gdp)
                             !  Pointer of array RMNEG
                             !  Criterion for conc. above which fil-
                             !  ter procedure is applied
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: diffusion, concentrations
    !
    !                        rho   (nmaxddb  ,mmaxddb,kmax  )
    !                        sumrho(nmaxddb  ,mmaxddb,kmax  )
    pntnam = 'rho'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array RHO
                             !  Array with densities [kg/m3]
                             !  Array with mud-water densities
                             !  [kg/m3]
                             ! 
                             !  Array with densities [kg/m3]
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sumrho'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array SUMRHO
                             !  Integral of density from layer k
                             !  until free surface [kg/m3]
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: tau
    !
    !                        taubpu(nmaxddb  ,mmaxddb)
    !                        taubpv(nmaxddb  ,mmaxddb)
    !                        taubsu(nmaxddb  ,mmaxddb)
    !                        taubsv(nmaxddb  ,mmaxddb)
    !                        taubmx(nmaxddb  ,mmaxddb)
    !                        rxx   (nmaxddb  ,mmaxddb,kmax  )
    !                        rxy   (nmaxddb  ,mmaxddb,kmax  )
    !                        ryy   (nmaxddb  ,mmaxddb,kmax  )
    !                        rxz   (nmaxddb  ,mmaxddb,kmax  )
    !                        ryz   (nmaxddb  ,mmaxddb,kmax  )
    pntnam = 'taubpu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array TAUBPU
                             !  Primary   bottom friction term in the
                             !  x-dir. (velocity dependent)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'taubpv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array TAUBPV
                             !  Primary   bottom friction term in the
                             !  y-dir. (velocity dependent)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'taubsu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array TAUBSU
                             !  Pointer of array TAUBSV
                             !  Secondary bottom friction term in the
                             !  x-dir. (velocity dependent)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'taubsv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array TAUBSV
                             !  Secondary bottom friction term in the
                             !  y-dir. (velocity dependent)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'taubmx'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array TAUBMX
                             !  Primary maximal bottom friction term
                             !  (scalar entity)
                             !  Primary maximal bottom friction term
                             !  in the x-dir. (velocity dependent)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rxx'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array RXX
                             !  Spiral motion stresses
                             !  Turbulent stresses
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rxy'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array RXY
                             !  Spiral motion stresses
                             !  Turbulent stresses
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ryy'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array RYY
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rxz'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ryz'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: waves
    !
    !                        dis   (nmaxddb  ,mmaxddb) * kfacwv * 4
    !                        discom(nmaxus,mmax   ,2) * kfacwv
    !                        rlabda(nmaxddb  ,mmaxddb) * kfacwv
    !                        teta  (nmaxddb  ,mmaxddb) * kfacwv
    !                        dircom(nmaxus,mmax   ,2) * kfacwv
    !                        dircos(nmaxus,mmax   ,2) * kfacwv
    !                        dirsin(nmaxus,mmax   ,2) * kfacwv
    !                        uorb  (nmaxddb  ,mmaxddb) * kfacwv
    !                        ubot  (nmaxddb  ,mmaxddb) * kfacwv
    !                        ubcom (nmaxddb  ,mmaxddb) * kfacwv
    !                        hrms  (nmaxddb  ,mmaxddb) * kfacwv
    !                        hrmcom(nmaxus,mmax   ,2) * kfacwv
    !                        tp    (nmaxddb  ,mmaxddb) * kfacwv
    !                        tpcom (nmaxus,mmax   ,2) * kfacwv
    !                        grmasu(nmaxddb  ,mmaxddb)
    !                        msucom(nmaxus,mmax   ,2) * kfacwv
    !                        grmasv(nmaxddb  ,mmaxddb)
    !                        msvcom(nmaxus,mmax   ,2) * kfacwv
    !                        wsu   (nmaxddb  ,mmaxddb)
    !                        wsucom(nmaxus,mmax   ,2) * kfacwv
    !                        wsv   (nmaxddb  ,mmaxddb)
    !                        wsvcom(nmaxus,mmax   ,2) * kfacwv
    !                        wsbu  (nmaxddb  ,mmaxddb)
    !                        wsbuc (nmaxus,mmax   ,2) * kfacwv
    !                        wsbv  (nmaxddb  ,mmaxddb)
    !                        wsbvc (nmaxus,mmax   ,2) * kfacwv
    !                        wlen  (nmaxus,mmax   ,2) * kfacwv
    !                        wlcom (nmaxus,mmax   ,2) * kfacwv
    !                        qxkw  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        qykw  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        cgc   (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        c     (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        qxkr  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        qykr  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewabr0(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewabr1(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewave0(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewave1(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        eroll0(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        eroll1(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sinkw (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sourw (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sinkr (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sourr (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        fxw   (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        fyw   (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        dfu   (nmaxddb  ,mmaxddb)
    !                        dfv   (nmaxddb  ,mmaxddb)
    !                        deltau(nmaxddb  ,mmaxddb)
    !                        deltav(nmaxddb  ,mmaxddb)
    pntnam = 'dis'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*4, gdp)
                             !  Pointer of array DIS
                             !  Dissipation waves
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'df'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array DF
                             !  Dissipation waves in wave boundary layer
    if (ierr<= - 9) goto 9999
    pntnam = 'discom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  dissipation waves
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rlabda'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array RLABDA
                             !  Wavelength
                             !  wavelength
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'teta'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array TETA
                             !  Angle waves
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dircom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  angle waves
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dircos'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dirsin'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'uorb'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array UORB
                             !  Orbital velocity at the bottom layer
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ubot'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array UBOT
                             !  Peak orbital velocity at the bottom layer
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ubcom'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array UBCOM
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  Peak orbital velocity at the bottom layer
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hrms'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array HRMCOM
                             !  RMS Wave Height
                             !  Wave heights
                             !  Wave hights
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hrmcom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  orbital velocity at the bottom layer
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'tp'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  Pointer of array TP
                             !  Period Waves
                             !  Period waves
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'tpcom'         !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  period waves
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'grmasu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GRMASU
                             !  Mass flux waves (in the x direction)
                             !  Mass flux waves (in the x-dir.)
                             !  Mass flux waves (in the x-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'grmasv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GRMASV
                             !  Mass flux waves (in the y direction)
                             !  Mass flux waves (in the y-dir.)
                             !  Mass flux waves (in the y-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'grmsur'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GRMSUR
                             !  Mass flux Roller (in the x direction)

    if (ierr<= - 9) goto 9999
    !
    pntnam = 'grmsvr'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GRMSVR
                             !  Mass flux Roller (in the y direction)

    if (ierr<= - 9) goto 9999
    !
    pntnam = 'grfacu'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GRFACU
                             !  Breaker Delay Adjustment (in the x direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'grfacv'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array GRFACV
                             !  Breaker Delay Adjustment (in the y direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'msucom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  mass flux waves (in the x-dir.)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'msvcom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  mass flux waves (in the y-dir.)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsu'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WSU
                             !  Local x-component of the flow-driving
                             !  force due to wave breaking (:= WSU)
                             !  Wave stresses (in the x-direction)
                             ! 
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsucom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  wave stresses (in the x-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsv'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WSV
                             !  Local y-component of the flow-driving
                             !  force due to wave breaking (:= WSV)
                             !  Wave stresses (in the y-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsvcom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  wave stresses (in the y-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsbu'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WSBODYU
                             !  Local x-component of the flow-driving
                             !  force due to waves in the water column (:= WSBODYU)
                             !  Wave stresses in the water column (in the x-direction)
                             ! 
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsbuc'         !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  wave stresses in the water column (in the x-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsbv'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WSBODYV
                             !  Local y-component of the flow-driving
                             !  force due to waves in the water column (:= WSBODYV)
                             !  Wave stresses in the water column (in the y-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wsbvc'         !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  wave stresses in the water column (in the y-direction)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wlen'          !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Pointer of array WLEN
                             !  Mean wave length
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wlcom'        !  Global data
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
                             !  Pointer of array WLCOM
                             !  Help array to interpolate between
                             !  to consecutive timesteps for
                             !  Mean wave length
    if (ierr<= - 9) goto 9999
    !roller
    pntnam = 'qxkw'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'qykw'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'cgc'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'c'             !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  Complete char. array (mempool.inc)
                             !  Internal work array, tridiagonal
                             !  matrix water levels upper diagonal
                             ! 
    if (ierr<= - 9) goto 9999
    pntnam = 'qxkr'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'qykr'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'ewabr0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'ewabr1'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'ewave0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'ewave1'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'eroll0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'eroll1'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'sinkw'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'sourw'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'sinkr'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'sourr'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'fxw'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'fyw'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'ampbc'         !  Global data
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'ombc'          !  Global data
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'phibc'         !  Global data
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    pntnam = 'thetbc'        !  Global data
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dfu'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DFU
                             !  Bottom wave dissipation in u-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dfv'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DFV
                             !  Bottom wave dissipation in v-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'deltau'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DELTAU
                             !  Boundary layer thickness in u-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'deltav'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DELTAV
                             !  Boundary layer thickness in v-point
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: derivatives
    !
    !                        dddksi(nmaxddb  ,mmaxddb)
    !                        dddeta(nmaxddb  ,mmaxddb)
    !                        dzdksi(nmaxddb  ,mmaxddb)
    !                        dzdeta(nmaxddb  ,mmaxddb)
    pntnam = 'dddksi'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DDDKSI
                             !  Ddepth/deta in u-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dddeta'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DDDETA
                             !  Ddepth/dksi in v-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzdksi'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DZDKSI
                             !  Dzeta /deta in v-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzdeta'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array DZDETA
                             !  Dzeta /dksi in u-point
    if (ierr<= - 9) goto 9999
    !
    !      Export HDT to shared mem
    !
    pntnam = 'hdt'           !  Global data
    ierr = mkfpnt(pntnam, 1, gdp)
                             !  Half Integration time step [seconds]
                             !  Half integration time step [seconds]
    if (ierr<= - 9) goto 9999
    !
    ! overview workarrays for version v240:
    ! 16 2D arrays (r 80-88 + 88a-88e)
    ! 17 3D arrays (r 89-109)
    !  4 3D arrays (r 94-97)
    !
    !                wrka1    (nmaxddb  ,mmaxddb) or (nmaxwind*mmaxwind)
    !                wrka2    (nmaxddb  ,mmaxddb) or (nmaxwind*mmaxwind)
    !                wrka3    (nmaxddb  ,mmaxddb) or (nmaxwind*mmaxwind)
    !                wrka4    (nmaxddb  ,mmaxddb)
    !                wrka5    (nmaxddb  ,mmaxddb)
    !                wrka6    (nmaxddb  ,mmaxddb)
    !                wrka7    (nmaxddb  ,mmaxddb)
    !                wrka8    (nmaxddb  ,mmaxddb)
    !                wrka9    (nmaxddb  ,mmaxddb)
    !                wrka12   (nmaxddb  ,mmaxddb)
    !                wrka13   (nmaxddb  ,mmaxddb)
    !                wrka14   (nmaxddb  ,mmaxddb)
    !                wrka15   (nmaxddb  ,mmaxddb)
    !                wrka16   (nmaxddb  ,mmaxddb)
    pntnam = 'wrka1'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WRKA1
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka2'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array WRKA2
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka3'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka4'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka5'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka6'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka7'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka8'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka9'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrka12'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr== - 1) goto 9999
    !
    pntnam = 'wrka13'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr== - 1) goto 9999
    !
    pntnam = 'wrka14'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr== - 1) goto 9999
    !
    pntnam = 'wrka15'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Internal work array
    if (ierr== - 1) goto 9999
    !
    pntnam = 'wrka16'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  no description (yet)
    if (ierr== - 1) goto 9999
    !
    !-----arrays for: coefficient matrices (double kmax dimension)
    !     for the array rbuff we use wrkb1 or wrkc1 dependent on the value
    !     of lmax
    ! NB. arrays wrkb1, wrkb2, wrkb3, wrkb4, wrkb5 and wrkb6 have 1 extra layer
    ! due to new turbulence model and fully non-hydrostatic module
    !
    !                        wrkb1   (nmaxddb  ,mmaxddb,0:kmax  )
    !                      := rbuff max ((nmaxus,mmax  ,0:kmax,lmax  ),
    !                                    (nmaxus,mmax  ,kmax  ,2     ))
    !                              for lmax > 1 declared with wrkc1
    !                        wrkb2   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb3   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb4   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb5   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb6   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb7   (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb8   (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb9   (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb10  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb11  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb12  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb13  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb14  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb15  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb16  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb17  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb18  (nmaxddb  ,mmaxddb,kmax  )
    !
    lrbuff = nmaxus*mmax*kmax*2
    if (lmax>1) lrbuff = 0
    laak = max(nmaxddb*mmaxddb*(kmax + 1), lrbuff)
    !
    pntnam = 'wrkb1'         !  Global data
    ierr = mkfpnt(pntnam, laak, gdp)
                             !  Pointer of array WRKB1
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb2'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array WRKB2
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb3'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array WRKB3
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb4'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array WRKB4
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb5'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb6'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax+1), gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb7'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb8'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb9'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb10'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb11'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb12'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb13'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb14'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb15'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb16'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Internal work array
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb17'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkb18'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for: coefficient matrices (double kmax dimension)
    !
    !                        wrkc1  (nmaxddb  ,mmaxddb,kmax,lmax)
    !                      := rbuff max ((nmaxus,mmax  ,0:kmax,lmax  ),
    !                                    (nmaxus,mmax  ,kmax  ,2     ))
    !                              for lmax > 1 it fits always in wrkc1
    !                              for lmax <= 1 declared in wrkb1
    !                        wrkc2  (nmaxddb  ,mmaxddb,kmax,lmax)
    !                        wrkc3  (nmaxddb  ,mmaxddb,kmax,lmax)
    !                        wrkc4  (nmaxddb  ,mmaxddb,kmax,lmax)
    lmaxsed = max(lmax,lsedtot)
    lrbuff  = nmaxus*mmax*(kmax + 1)*lmaxsed
    if (lmaxsed<=1) lrbuff = 0
    laakl = max(nmaxddb*mmaxddb*kmax*lmaxsed, lrbuff)
    !
    pntnam = 'wrkc1'         !  Global data
    ierr = mkfpnt(pntnam, laakl, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkc2'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*max(1, lmax), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkc3'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'wrkc4'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*max(1, lmax), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! RBUFF is used only outside TRISOL. So we use a work array, which
    ! is initialized each timestep in TRISOL, here WRKB1 or WRKC1
    ! The maximum needed storage space for RBUFF is defined in esm_alloc_real
    ! and taken in consideration in the declaration of WRKB1 and WRKC1
    !
    ! As a result RBUFF does not need to be allocated
    ! RBUFF is going to point to WRKB1 or WRKC1, depending on their size
    ! See subroutine gtptrs.f90
    !
    !
    !
    !-----arrays for: curvature coefficients of streakline
    !
    !                        x3    (nmaxddb  ,mmaxddb)
    !                        x2y   (nmaxddb  ,mmaxddb)
    !                        xy2   (nmaxddb  ,mmaxddb)
    !                        y3    (nmaxddb  ,mmaxddb)
    pntnam = 'x3'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array X3
                             !  Coef. of u**3 used for curvature
                             !  Coefficient of U**3
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'x2y'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array X2Y
                             !  Coef. of u**2*v used for curvature
                             !  Coefficient of U**2*V
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'xy2'           !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array XY2
                             !  Coef. of u*v**2 used for curvature
                             !  Coefficient of U*V**2
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'y3'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array Y3
                             !  Coef. of v**3  used for curvature
                             !  Coefficient of V**3
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for anti creep algorithm
    !
    !                        dpdksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dpdeta(nmaxddb  ,mmaxddb,1:kmax )
    !                        dsdksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dsdeta(nmaxddb  ,mmaxddb,1:kmax )
    !                        dtdksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dtdeta(nmaxddb  ,mmaxddb,1:kmax )
    !                        dldksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dldeta(nmaxddb  ,mmaxddb,1:kmax )
    pntnam = 'dpdksi'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DPDKSI
                             !  Horizontal gradient baroclinic part
                             !  pressure, strictly horizontal
                             !  Horizontal gradient baroclinic
                             !  pressure in ksi-direction
                             !  Strictly horizontal gradient density
                             !  in pressure term in ksi-direction
                             !  strictly horizontal gradient density
                             !  in pressure term in ksi direction,
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dpdeta'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DPDETA
                             !  Horizontal gradient baroclinic
                             !  pressure in eta-direction
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dsdksi'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DSDKSI
                             !  Horizontal gradient salinity
                             !  in ksi-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
                             !  Horizontal gradient salinity
                             !  in ksi-direction
                             !  Horizontal gradient salinity,
                             !  strictly horizontal in ksi-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dsdeta'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DSDETA
                             !  Horizontal gradient salinity
                             !  in eta-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
                             !  Horizontal gradient salinity
                             !  in eta-direction
                             !  Horizontal gradient salinity,
                             !  strictly horizontal in eta-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dtdksi'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DTDKSI
                             !  Horizontal gradient temperature
                             !  in ksi-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
                             !  Horizontal gradient temperature
                             !  in ksi-direction
                             !  Horizontal gradient temperature,
                             !  strictly horizontal in ksi-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dtdeta'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DTDETA
                             !  Horizontal gradient temperature
                             !  in eta-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
                             !  Horizontal gradient temperature
                             !  in eta-direction
                             !  Horizontal gradient temperature,
                             !  strictly horizontal in eta-direction
                             !  For Anti Creep: Contribution diffu-
                             !  sive flux interpolated on Cartesian
                             !  grid to cell NM
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dldksi'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DLDKSI
                             !  Horizontal gradient sediment,
                             !  strictly horizontal in ksi-direction
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dldeta'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array DLDETA
    if (ierr== - 1) goto 9999
    !
    !-----arrays for: matrices for turbulence model
    !
    !                        bruvai(nmaxddb  ,mmaxddb,0:kmax )
    !                        rich  (nmaxddb  ,mmaxddb,0:kmax )
    !                        dudz  (nmaxddb  ,mmaxddb,0:kmax )
    !                        dvdz  (nmaxddb  ,mmaxddb,0:kmax )
    pntnam = 'bruvai'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array BRUVAI
                             !  Bouyancy frequency, squared, defined
                             !  on hydrodyn. grid
                             !  Bouyancy frequency, squared, defined
                             !  on hydrodynamic grid
                             ! 
                             !  Brunt vaisly frequency
                             ! BRUNT VAISLY FREQUENCY
                             ! Brunt vaisly frequency
                             !  -AG*DRHODZ/RHO
    if (ierr== - 1) goto 9999
    !
    pntnam = 'rich'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array RICH
                             !  Gradient Richardson number on hydro-
                             !  dynamic grid
                             !  Richardson numbers at layer
                             !  interfaces
                             ! 
                             !  Array with Richardson numbers
                             ! 
                             !  Richardson numbers at layer
                             !  interfaces
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dudz'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array DUDZ
                             !  Vertical gradient of u-velocity com-
                             !  ponent of hydrodynamic module
                             !  Vertical gradient of u-velocity
                             !  component
                             ! 
                             !  Vertical velocity gradients at layer
                             !  interface
                             ! Vertical velocity gradients
                             !  at layer interface
    if (ierr== - 1) goto 9999
    !
    pntnam = 'dvdz'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
                             !  Pointer of array DVDZ
                             !  Vertical gradient of v-velocity com-
                             !  ponent of hydrodynamic module
                             !  Vertical gradient of v-velocity
                             !  component
                             ! 
                             !  Vertical velocity gradients at layer
                             !  interface
                             ! Vertical velocity gradients
                             !  at layer interface
    if (ierr== - 1) goto 9999
    !
    !-----arrays for DELWAQ
    !
    !                        volum0(nmaxddb  ,mmaxddb,kmax  )
    !                        volum1(nmaxddb  ,mmaxddb,kmax  )
    !                        z0ucur(nmaxddb  ,mmaxddb       )
    !                        z0vcur(nmaxddb  ,mmaxddb       )
    !                        z0urou(nmaxddb  ,mmaxddb       )
    !                        z0vrou(nmaxddb  ,mmaxddb       )
    !                        qu    (nmaxddb  ,mmaxddb,kmax  )
    !                        qv    (nmaxddb  ,mmaxddb,kmax  )
    pntnam = 'volum0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array VOLUM0
                             !  Volume of a cell at old time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'volum1'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array VOLUM1
                             !  Volume of a cell at new time level
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'z0ucur'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Z0 roughness related to currents in U-point
    if (ierr== - 1) goto 9999
    !
    pntnam = 'z0vcur'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Z0 roughness related to currents in V-point
    if (ierr== - 1) goto 9999
    !
    pntnam = 'z0urou'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Wave enhanced Z0 roughness in U-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'z0vrou'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Wave enhanced Z0 roughness in V-point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qu'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array QU
                             ! 
                             !  Cummulative discharge for layer k
                             !  in the X-dir. in U-velocity point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'qv'            !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Pointer of array QV
                             ! 
                             !  Cummulative discharge for layer k
                             !  in the Y-dir. in V-velocity point
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for post processing HIS file
    !
    !                        zwl   (nostat       )
    !                        zalfas(nostat       )
    !                        zcuru (nostat,kmax  )
    !                        zcurv (nostat,kmax  )
    !                        zcurw (nostat,kmax  )
    !                        zqxk  (nostat,kmax  )
    !                        zqyk  (nostat,kmax  )
    !                        ztauks(nostat       )
    !                        ztauet(nostat       )
    !                        zvicww(nostat,0:kmax)
    !                        zdicww(nostat,0:kmax)
    !                        zrich (nostat,0:kmax)
    !                        zrho  (nostat,kmax  )
    !                        fltr  (ntruv        )
    !                        ctr   (ntruv        )
    !                        gro   (nostat,kmax  ,lstsci)
    !                        ztur  (nostat,0:kmax,ltur  )
    !                        atr   (ntruv ,lstsci)
    !                        dtr   (ntruv ,lstsci)
    !                      a sbtr  (ntruv ,lsedtot  )
    !                      b sstr  (ntruv ,lsed  )
    !                      c sbtrc (ntruv ,lsedtot  )
    !                      d sstrc (ntruv ,lsed  )
    !                        discum(nsrc         )
    !                        zws   (nostat,0:kmax,lsed  )
    !                        zrsdeq(nostat,kmax  ,lsed  )
    !                        zbdsed(nostat       ,lsedtot  )
    !                        zdpsed(nostat       )
    !                        zdps  (nostat       )
    !                      d zsbu  (nostat       ,lsedtot  )
    !                      e zsbv  (nostat       ,lsedtot  )
    !                      f zssu  (nostat       ,lsed  )
    !                      g zssv  (nostat       ,lsed  )
    !                        zvort (nostat,kmax  )
    !                        zenst (nostat,kmax  )
    !                       hydprs(nostat,kmax  )
    pntnam = 'zwl'           !  Global data
    ierr = mkfpnt(pntnam, nostat, gdp)
                             !  Pointer of array ZWL
                             !  Water elevation at the monitoring
                             !  stations
                             !  Flag for activation of Z-MODEL
                             !  Water elevation at the monitoring
                             !  stations
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zalfas'        !  Global data
    ierr = mkfpnt(pntnam, nostat, gdp)
                             !  Pointer of array ZALFAS
                             !  Orientation at the monitoring sta-
                             !  tions (defined at the water eleva-
                             !  tion point) as the angle formed by
                             !  the line spanned by the u-velocity
                             !  points around the zeta point and the
                             !  x-axis
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zcuru'         !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  Pointer of array ZCURU
                             !  U-velocity at the monitoring sta-
                             !  tions (defined at the water eleva-
                             !  tion point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zcurv'         !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  Pointer of array ZCURV
                             !  V-velocity at the monitoring sta-
                             !  tions (defined at the water eleva-
                             !  tion point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zcurw'         !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  Pointer of array ZCURW
                             !  W-velocity at the monitoring sta-
                             !  tions (defined at the water eleva-
                             !  tion point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zqxk'          !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  Pointer of array ZQXK
                             !  Flow in the x-dir. at the monito-
                             !  ring stations (defined at the water
                             !  elevation point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zqyk'          !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  Pointer of array ZQYK
                             !  Flow in the y-dir. at the monito-
                             !  ring stations (defined at the water
                             !  elevation point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ztauks'        !  Global data
    ierr = mkfpnt(pntnam, nostat, gdp)
                             !  Pointer of array ZTAUKS
                             !  Bottom friction in KSI direction at
                             !  the monitoring stations defined at
                             !  the water elevation point
                             !  Density at the monitoring stations
                             !  defined at the water elevation point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ztauet'        !  Global data
    ierr = mkfpnt(pntnam, nostat, gdp)
                             !  Pointer of array ZTAUET
                             !  Bottom friction in ETA direction at
                             !  the monitoring stations defined at
                             !  the water elevation point
                             !  Density at the monitoring stations
                             !  defined at the water elevation point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zvicww'        !  Global data
    ierr = mkfpnt(pntnam, nostat*(kmax + 1), gdp)
                             !  Pointer of array ZVICWW
                             !  Vertical Eddy viscosity-3D at the
                             !  monitoring stations (defined at
                             !  the water elevation point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zdicww'        !  Global data
    ierr = mkfpnt(pntnam, nostat*(kmax + 1), gdp)
                             !  Pointer of array ZDICWW
                             !  Vertical Eddy diffusivity-3D at the
                             !  monitoring stations (defined at
                             !  the water elevation point)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zrich'         !  Global data
    ierr = mkfpnt(pntnam, nostat*(kmax + 1), gdp)
                             !  Pointer of array ZRICH
                             !  Richardson numbers at the monitoring
                             !  stations defined at the water ele-
                             !  vation point
                             !  Richsardson numbers at the monitoring
                             !  stations defined at the water ele-
                             !  vation point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zrho'          !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  Pointer of array ZRHO
                             !  Density at the monitoring stations
                             !  defined at the water elevation point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'fltr'          !  Global data
    ierr = mkfpnt(pntnam, ntruv, gdp)
                             !  Pointer of array FLTR
                             !  Array for the cumulative volume
                             !  transport for water along the
                             !  cross section.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ctr'           !  Global data
    ierr = mkfpnt(pntnam, ntruv, gdp)
                             !  Array for the total flow along the
                             !  cross section.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'gro'           !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax*lstsci, gdp)
                             !  Pointer of array GRO
                             ! 
                             !  Array containing the concentration
                             !  value at the monitoring stations
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'ztur'          !  Global data
    ierr = mkfpnt(pntnam, nostat*(kmax + 1)*ltur, gdp)
                             !  Pointer of array ZTUR
                             ! 
                             !  Concentrations turbulent energy and
                             !  dissipation at the monitoring station
                             !  defined at the water elevation point
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'atr'           !  Global data
    ierr = mkfpnt(pntnam, ntruv*lstsci, gdp)
                             !  Pointer of array ATR
                             !  Array for the cumulative advective
                             !  transport for constituents along
                             !  the cross section.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dtr'           !  Global data
    ierr = mkfpnt(pntnam, ntruv*lstsci, gdp)
                             !  Pointer of array CTR
                             !  Pointer of array DTR
                             !  Array for the cumulative diffusive
                             !  transport for constituents along
                             !  the cross section.
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sbtr'          !  Global data
    ierr = mkfpnt(pntnam, ntruv*lsedtot, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sstr'          !  Global data
    ierr = mkfpnt(pntnam, ntruv*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sbtrc'         !  Global data
    ierr = mkfpnt(pntnam, ntruv*lsedtot, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'sstrc'         !  Global data
    ierr = mkfpnt(pntnam, ntruv*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'discum'        !  Global data
    ierr = mkfpnt(pntnam, nsrc, gdp)
                             !  Pointer of array DISCUM
                             !  Array with discharge values [m3/sec]
                             !  cummulalive in time
                             !  Array with discharge values [m3/sec]
                             !  cummulative in time
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zws'           !  Global data
    ierr = mkfpnt(pntnam, nostat*(kmax + 1)*lsed, gdp)
                             !  Fall velocity (dependent on sediment
                             !  type)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zrsdeq'        !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax*lsed, gdp)
                             !  Equilibrium sediemnt concentration
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zbdsed'        !  Global data
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
                             !  Array with total sediment in NM
                             !  units : kg /m2
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zdpsed'        !  Global data
    ierr = mkfpnt(pntnam, nostat, gdp)
                             !  Total depth sediment layer
                             !  (not yet used)
                             !  type)
                             !  Total depth sediment layer
                             !  (not yet used)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zdps'          !  Global data
    ierr = mkfpnt(pntnam, nostat, gdp)
                             !  Depth at monitoring station
                             !
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zsbu'          !  Global data
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zsbv'          !  Global data
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zssu'          !  Global data
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zssv'          !  Global data
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zvort'         !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zenst'         !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'hydprs'        !  Global data
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for Internal wave energy
    !
    !                        tgarkx(15   * (kmax +1))
    !                        tgarkt(24   * (kmxdt+1))
    !                        tgarnp(4    * (npiwe+1))
    !                        tkepro(nmaxddb  ,mmaxddb,kmax  )
    !                        tkedis(nmaxddb  ,mmaxddb,kmax  )
    !                        fuiwe (nmaxddb  ,mmaxddb,kmax  )
    !                        fviwe (nmaxddb  ,mmaxddb,kmax  )
    !
    pntnam = 'tgarkx'        !  Global data
    ierr = mkfpnt(pntnam, 15*(kmax + 1), gdp)
                             !  Delft3D flow arrays for point (N,M)
                             !  for all layers  0:KMAX
    if (ierr<= - 9) goto 9999
    pntnam = 'tgarkt'        !  Global data
    ierr = mkfpnt(pntnam, 24*(kmxdt + 1), gdp)
                             !  All work arrays for IWE for point
                             !  (N,M) for all KMXT layers
    if (ierr<= - 9) goto 9999
    pntnam = 'tgarnp'        !  Global data
    ierr = mkfpnt(pntnam, 4*(npiwe + 1), gdp)
                             !  All work arrays for IWE for point
                             !  (N,M) for all NFREQS frequencies
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'tkepro'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Transfer rate of IWE to TKE, on
                             !  hydrodynamic grid, derived from TIWTK
                             !  Turbulence production term
                             !  due to IWE
                             ! 
                             !  Transfer rate of IWE to TKE, on
                             !  hydrodynamic grid, derived from TIWTK
    if (ierr<= - 9) goto 9999
    pntnam = 'tkedis'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Transfer rate of TKE to IWE, on
                             !  hydrodynamic grid, derived from TTKIW
                             !  Turbulence dissipation term
                             !  due to IWE
                             ! 
                             !  Transfer rate of TKE to IWE, on
                             !  hydrodynamic grid, derived from TTKIW
    if (ierr<= - 9) goto 9999
    pntnam = 'fuiwe'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Force/momentum flux in u-direction
                             !  due to critical-layer formation, on
                             !  hydrodynamic grid (not yet installed)
                             ! 
                             !  Force/momentum flux in u-direction
                             !  due to critical-layer formation, on
                             !  hydrodynamic grid (not yet installed)
    if (ierr<= - 9) goto 9999
    pntnam = 'fviwe'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
                             !  Force/momentum flux in v-direction
                             !  due to critical-layer formation, on
                             !  hydrodynamic grid (not yet installed)
                             ! 
                             !  Force/momentum flux in v-direction
                             !  due to critical-layer formation, on
                             !  hydrodynamic grid (not yet installed)
    if (ierr<= - 9) goto 9999
    !
    !-----arrays for 2D Turbulence model
    !
    !                        vnu2d (nmaxddb  ,mmaxddb       )
    !                        vnu3d (nmaxddb  ,mmaxddb       )
    !                        rtu2d0(nmaxddb  ,mmaxddb,   2  )
    !                        rtu2d1(nmaxddb  ,mmaxddb,   2  )
    !                        rtubnd(nmaxddb  ,mmaxddb,   2  )
    !
    pntnam = 'vnu2d'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array VNU2D
                             !  Horizontal eddy visc. coefficient
                             !  [m2/s] (in depth point), due to
                             !  2D-turbulence
    if (ierr<= - 9) goto 9999
    pntnam = 'vnu3d'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             !  Pointer of array VNU3D
                             !  Depth-averaged vertical
                             !  eddy visc. coefficient
                             !  [m2/s] (in depth point), due to
                             !  3D-turbulence
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rtu2d0'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*2, gdp)
                             !  Concentrations turbulent energy and
                             !  enstrophy at old time level
    if (ierr<= - 9) goto 9999
    pntnam = 'rtu2d1'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*2, gdp)
                             !  Pointer of array RTU2D1
                             !  Concentrations turbulent energy and
                             !  dissipation at new time level
                             ! 
                             !  Concentrations turbulent energy and
                             !  enstrophy at new time level
    if (ierr<= - 9) goto 9999
    pntnam = 'rtubnd'        !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*2, gdp)
                             !  Array with boundary conditions for
                             !  turbulent quantities
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: vertical grid information for Online Visualisation
    !
    !                        uvdist(max(nmax,mmax),kmax+2)
    !                        huvw  (max(nmax,mmax),kmax+2)
    !                        zdist (max(nmax,mmax),kmax+2)
    !                        dpc   (max(nmax,mmax),kmax+2)
    !
    pntnam = 'uvdist'        !  Global data
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'huvw'          !  Global data
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'zdist'         !  Global data
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dpc'           !  Global data
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! BEGIN arrays for: z-model (fixed layer)
    !
    !
    ! arrays for: vertical grid
    !
    !            * kfacz     dzs0  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzs1  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzu0  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzu1  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzv0  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzv1  (nmaxddb,mmaxddb,kmax)
    !
    pntnam = 'dzs0'          !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzs1'          !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             ! 
                             !  Layer thickness at Z-points
                             !  (new time level)
    if (ierr<= - 9) goto 9999
    pntnam = 'dzu0'          !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzu1'          !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzv0'          !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzv1'          !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! arrays for: baroclinic pressure gradient
    !
    !                        drhodx(nmaxddb,mmaxddb,kmax+2)
    !                        drhody(nmaxddb,mmaxddb,kmax+2)
    pntnam = 'drhodx'        !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'drhody'        !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    !
    ! END arrays for: fixed layer approach
    !
    !
    ! BEGIN arrays for: non-hydrostatic pressure
    !
    !
    pntnam = 'p1'            !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'p0'            !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'p00'           !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'pnhcor'        !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'w0'            !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*(kmax + 1)*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 's00'           !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzs00'         !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzu00'         !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'dzv00'         !  Global data
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
                             !  no description (yet)
    if (ierr<= - 9) goto 9999
    !
    ! END arrays for: non-hydrostatic pressure (NOT verified)
    !
    !
    ! BEGIN arrays for: Directional Point Model for Vegetation
    !
    !
    pntnam = 'diapl'         !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*kfacdpmv, gdp)
                             !  plant stem diameter (m)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'rnpl'          !  Global data
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*kfacdpmv, gdp)
                             !  number of stems per horizontal unit area (1/m2)
    if (ierr<= - 9) goto 9999
    !
    ! END arrays for: Directional Point Model for Vegetation
    !
    !
    ! The following list of scalar reals have to be placed in shared memory,
    ! because then the mapper can read them.
    ! BE CAREFUL:
    !    These reals are allocated TWICE inside FLOW:
    !    in esm_alloc_real.f90 as part of the shared memory block, allocated via esm/fsm, and
    !    in *.igs-files as part of the GDP structure (e.g. ag) or
    !    locally on a high level (see scalar integers in esm_alloc_int.f90)
    !
    !    FLOW uses the instance in the GDP-structure (or the
    !    local definition respectively) and place a copy of these parameters in
    !    the shared memory block. The mapper uses this copy and is assumed not to
    !    change the values.
    !
    ! TO DO: Clean implementation:
    !    These parameters should be allocated in shared memory only (since the
    !    mapper needs them). The GDP-structure and local allocation variants
    !    must be replaced by pointers to the shared memory instance.
    !
    pntnam = 'AG'            !  Global data
    ierr = mkfpnt(pntnam, 1, gdp)
                             !  Gravity acceleration [m2/s]
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'RHOW'          !  Global data
    ierr = mkfpnt(pntnam, 1, gdp)
                             !  Density of water [kg/m3]
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'DT'            !  Global data
    ierr = mkfpnt(pntnam, 1, gdp)
                             !  Integration time step in tunits (tunit is 60.0 sec by default)
    if (ierr<= - 9) goto 9999
    !
    pntnam = 'TSCALE'        !  Global data
    ierr = mkfpnt(pntnam, 1, gdp)
                             !  Integration time step in seconds
                             !  TSCALE = dt*tunit
    if (ierr<= - 9) goto 9999
    !
    !
    !
    ! GDP arrays
    !
    if (.not. associated(gdp%gdtrisol%ustokes)) then
       allocate (gdp%gdtrisol%ustokes(gdp%d%nmlb:gdp%d%nmub,kmax), stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'esm_alloc_real: memory alloc error')
          call d3stop(1, gdp)
       endif
       gdp%gdtrisol%ustokes = 0.0_fp
    endif
    if (.not. associated(gdp%gdtrisol%vstokes)) then
       allocate (gdp%gdtrisol%vstokes(gdp%d%nmlb:gdp%d%nmub,kmax), stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'esm_alloc_real: memory alloc error')
          call d3stop(1, gdp)
       endif
       gdp%gdtrisol%vstokes = 0.0_fp
    endif
    !
    !
    !
    ! Test if pointer declaration outside declaration in POINTRS.INC
    !
    if (ierr== - 3) then
       error = .true.
       call prterr(lundia    ,'G005'    ,' '       )
       write (lundia, *) '         Parameter MXRPNT to small, add ',            &
                       & nrpntr - mxrpnt
    endif
    !
    !
    ! Test exit code which are not allowed (in theory not possible)
    !
 9999 continue
    if (ierr<= - 9) then
       error = .true.
       call prterr(lundia    ,'G920'    ,'esm_alloc_real'   )
    endif
end subroutine esm_alloc_real
