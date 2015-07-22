!nesthd1
      program nesthd1
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
!  $Id: nesthd1.f90 1379 2012-04-02 15:51:31Z mooiman $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/src/nesthd1.f90 $
!****&******************************************************************
! Deltares                         marine and coastal management
!
! program            : nesthd1
! version            : v1.0
! date               : June 1997
! programmer         : Lamber Hulsen/Theo van der Kaaij
! version            : 1.50.01 - AVUNDF for PC underflow
!                                SECURE without license test
!
! function           :    determine nest stations, weight factors and
!                         angles in an overall model to generate
!                         boundary conditions (with NESTHD2) at boundary
!                         support points in a nested model
! notes              : 1. the nest characteristics are determined both
!                         for a nesting at water level points as well as
!                         a nesting at velocity points. In this
!                         way you are free to choose the kind of
!                         boundary forcing when running NESTHD2 without
!                         re-running NESTHD1.
!                      2. NESTHD1 requires that the overall and nested
!                         models are defined in the same co-ordinate
!                         system.
!                      3. This version of NESTHD1 will NOT compose a
!                         grid for spherical or rectilinear models.
!                         For these models a user-defined grid is
!                         required.
!****&******************************************************************

      use precision
      use nesthd1_version_module
      include      'fsm.i'
      include      'tri-dyn.inc'
!
!     Obsolete Fortran 77 code for fixed dimensions
!     (only for comparison)
!
!     integer       lun   (  7   ),
!    &              ipx   (maxnrp), ipy   (maxnrp),
!    &              itotpx(maxnrp), itotpy(maxnrp)
!
!     integer       icom1 ( mmax  , nmax  )       ,
!    &              icom2 ( mmax  , nmax  )       ,
!    &              mcbsp ( maxbnd, 2     )       ,
!    &              ncbsp ( maxbnd, 2     )
!
!     integer       mcnes ( maxbnd, 2, 4     )    ,
!    &              ncnes ( maxbnd, 2, 4     )
!
!     real          angle ( maxbnd)
!
!     real          x1    ( mmax  , nmax  )       ,
!    &              x2    ( mmax  , nmax  )       ,
!    &              y1    ( mmax  , nmax  )       ,
!    &              y2    ( mmax  , nmax  )       ,
!    &              xbnd  ( maxbnd, 2     )       ,
!    &              ybnd  ( maxbnd, 2     )
!     real          weight( maxbnd, 2, 4     )
!     character*  1 typbnd (maxbnd)
!     character* 20 nambnd (maxbnd)

      integer       lun   (  7   )
      character*80  filnam (7)
      character*80  CIDENT
      logical       fout
      logical       spher_crs, spher_det


!     FMM pointers
!
      integer x1    ,y1    ,icom1
      integer x2    ,y2    ,icom2
      integer typbnd,nambnd
      integer angle ,mcbsp ,ncbsp
      integer xbnd  ,ybnd  ,mcnes ,ncnes
      integer weight
      integer ipx   ,ipy
      integer itotpx,itotpy
      integer :: fsmstatus
      integer :: fsm_flags
      integer :: mcid
!
!     Version string
!
    cident = ' '
    call getfullversionstring_nesthd1(cident)
!-----------------------------------------------------------------------
!---- 0. Initialisation
!-----------------------------------------------------------------------
      lunscr = 6

      mcid = 0
      fsm_flags = FSM_SILENT
      fsmstatus = fsmini(mcid, fsm_flags)

!-----------------------------------------------------------------------
!---- 1. Open all files
!-----------------------------------------------------------------------
      write (*     ,'(/, 2a)') ' ',trim(cident)
      call opnfl1(lun   , filnam, CIDENT)

!-----------------------------------------------------------------------
!---- 2. Dynamic memory allocation (FMM)
!-----------------------------------------------------------------------
!
!     Get dimensions for allocation
      call getdim(lun   , mmax1 , nmax1  , &
     &            mmax2 , nmax2 , maxnrp , maxbnd )
!
!     Make FMM pointers
!
      call mkpoint(lun(6) , mmax1 , nmax1  , &
     &             mmax2  , nmax2 , maxnrp , maxbnd )
!
!     Get FMM pointers
!
      call gtpoint(x1     ,y1     ,icom1 , &
     &             x2     ,y2     ,icom2 , &
     &             typbnd ,nambnd , &
     &             angle  ,mcbsp  ,ncbsp , &
     &             xbnd   ,ybnd   ,mcnes ,ncnes, &
     &             weight , &
     &             ipx    ,ipy    ,itotpx,itotpy)

!-----------------------------------------------------------------------
!---- 3. Zeroise arrays
!-----------------------------------------------------------------------

      call zeroi1 (i(mcbsp) ,maxbnd*2     )
      call zeroi1 (i(ncbsp) ,maxbnd*2     )
      call zeroi1 (i(icom1) ,mmax1  *nmax1 )
      call zeroi1 (i(icom2) ,mmax2  *nmax2 )

      call zeroi1 (i(mcnes) ,maxbnd*2     *4     )
      call zeroi1 (i(ncnes) ,maxbnd*2     *4     )

      call zeror1 (r(angle) ,maxbnd,0.    )

      call zeror1 (r(x1)    ,mmax1  *nmax1  ,0.    )
      call zeror1 (r(x2)    ,mmax2  *nmax2  ,0.    )
      call zeror1 (r(y1)    ,mmax1  *nmax1  ,0.    )
      call zeror1 (r(y2)    ,mmax2  *nmax2  ,0.    )

      call zeror1 (r(weight),maxbnd*2     *4     ,0.    )

!-----------------------------------------------------------------------
!---- 4. Read grid and boundary data models
!-----------------------------------------------------------------------

      call reargf(lun(1)    ,r(x1)    ,r(y1)    ,mmax1  ,nmax1  ,mc1   ,nc1, &
                & spher_crs )

      call zeroi1(i(ipx)   ,maxnrp)
      call zeroi1(i(ipy)   ,maxnrp)
      call zeroi1(i(itotpx),maxnrp)
      call zeroi1(i(itotpy),maxnrp)
      call inigrd(lun(2),ifout ,nmax1 ,mmax1 ,i(icom1),i(ipx) ,i(ipy) , &
     &            i(itotpx),i(itotpy),idum                             )

      if (ifout .ne. 0) stop 'Error in grid enclosure overall model'

      call reargf(lun(3)   ,r(x2)  ,r(y2) ,mmax2  ,nmax2  ,mc2   ,nc2   , &
               &  spher_det)
      if (spher_crs .neqv. spher_det) stop 'Coarse and detailled model should be in the same coordinate system'

      call zeroi1(i(ipx)   ,maxnrp)
      call zeroi1(i(ipy)   ,maxnrp)
      call zeroi1(i(itotpx),maxnrp)
      call zeroi1(i(itotpy),maxnrp)
      call inigrd(lun(4),ifout ,nmax2 ,mmax2 ,i(icom2),i(ipx) ,i(ipy) , &
     &            i(itotpx),i(itotpy),idum                          )
      if (ifout .ne. 0) stop 'Error in grid enclosure detailed model'

      call dimbnd(lun(5), nobnd )
      if (nobnd .gt. maxbnd) stop 'Enlarge maxbnd'
      call reabnd(lun(5),i(mcbsp), i(ncbsp), ch(typbnd), ch(nambnd), &
     &            nobnd)

!-----------------------------------------------------------------------
!---- 5. Determine weight factors water level boundary
!-----------------------------------------------------------------------

      call detxy (i(mcbsp) , i(ncbsp) , r(x2) , r(y2) , i(icom2) , &
     &            r(xbnd)  , r(ybnd)  , mmax2  , nmax2 , maxbnd, &
     &            nobnd , 'WL'                          )

      call detnst(r(x1)   , r(y1)   , i(icom1), r(xbnd) , r(ybnd)  , &
     &            i(mcbsp), i(ncbsp), i(mcnes), i(ncnes), r(weight), &
     &            mmax1   , nmax1   , mc1     , nc1     , maxbnd, &
     &            nobnd   , spher_crs                   )

!-----------------------------------------------------------------------
!---- 6. Write weight factors water level boundary to administration
!        file and write stations to trisula input file
!-----------------------------------------------------------------------

      call wrinst(lun(6)   , i(mcbsp) , i(ncbsp) , i(mcnes) , i(ncnes), &
     &            r(weight), r(angle) , maxbnd   , nobnd    , 'WL'    )

      call wrista(lun(7), i(mcnes) , i(ncnes) , maxbnd)

!-----------------------------------------------------------------------
!---- 7. Determine weight factors velocity boundary but first
!        zeroise arrays
!-----------------------------------------------------------------------

      call zeroi1(i(mcnes) ,maxbnd*2     *4     )
      call zeroi1(i(ncnes) ,maxbnd*2     *4     )

      call zeror1(r(weight),maxbnd*2     *4     ,0.    )

      call detxy  (i(mcbsp) ,i(ncbsp) ,r(x2) ,r(y2) ,i(icom2) , &
     &             r(xbnd)  ,r(ybnd)  ,mmax2 ,nmax2 , maxbnd, &
     &             nobnd    , 'UV'                          )

      call detnst (r(x1)   ,r(y1)   ,i(icom1) ,r(xbnd) ,r(ybnd)  , &
     &             i(mcbsp),i(ncbsp),i(mcnes) ,i(ncnes),r(weight), &
     &             mmax1   , nmax1  ,mc1      ,nc1     ,maxbnd   , &
     &             nobnd   , spher_crs                           )

!-----------------------------------------------------------------------
!---- 8. Determine angles velocity boundary
!-----------------------------------------------------------------------

      call detang (r(xbnd)  ,r(ybnd),r(angle) ,i(mcbsp) ,i(ncbsp) , &
     &             i(icom2) ,mmax2  ,nmax2    ,maxbnd   ,nobnd )

!-----------------------------------------------------------------------
!---- 9. Write weight factors/angles velocity boundary
!-----------------------------------------------------------------------

      call wrinst(lun(6)   ,i(mcbsp) ,i(ncbsp) ,i(mcnes) ,i(ncnes) ,    &
     &            r(weight),r(angle) ,maxbnd   ,nobnd    ,'UV'        )

      call wrista(lun(7), i(mcnes) , i(ncnes) , maxbnd)

!-----------------------------------------------------------------------
!---- end program
!-----------------------------------------------------------------------

      call clsfil(lun   ,7     )
      !call fsmend()

 9000 continue
      endprogram nesthd1
