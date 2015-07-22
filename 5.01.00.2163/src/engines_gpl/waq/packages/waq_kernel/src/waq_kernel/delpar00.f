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

      subroutine delpar00 ( mdpfile, noseg  , noq    , dwqvol , dwqflo )

      use partmem      !   for PARTicle tracking
      use alloc_mod    !   for PARTicle tracking
!      use rdhydr_mod   !   explicit interface
      use timers

      implicit none

!         Particle tracking


!     Arguments

!     kind            function         name                      description

      character( * ), intent(in   ) :: mdpfile                 !< file name mdp-file
      integer  ( ip), intent(in   ) :: noseg                   !< delwaq noseg
      integer  ( ip), intent(in   ) :: noq                     !< delwaq noq
      real     ( rp), intent(in   ) :: dwqvol (noseg)          !< delwaq volumes
      real     ( rp), intent(in   ) :: dwqflo (noq)            !< delwaq flows

!     Locals

      integer(ip) i, i2, itime
      integer     lunut         ! report file
      real   (rp) depmin
      logical update

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "delpar00", ithandl )

      alone = .true.
      if ( mdpfile .ne. ' ' ) then
         alone = .false.
         call rdfnam ( lunitp    , mdpfile   , fnamep    , nfilesp  , 2       ,
     &                 1         , .false.   )
         lunut = lunitp(2)
         ftypep = 'binary    '
         call report_date_time   ( lunut     )
         call rdlgri ( nfilesp   , lunitp    , fnamep    , ftypep   )
         call rdccol ( nmaxp     , mmaxp     , lunitp(5) , fnamep(5), ftypep  ,
     &                 lgrid2    , xb        , yb        , lunut    )
         call part01 ( lgrid     , lgrid2    , xb        , yb       , dx      ,
     &                 dy        , area      , angle     , nmaxp    , mmaxp   )
         nolayp =  layt

!           this replaces the call to rdhydr

         if ( lunitp(20) .gt. 0 .and. fnamep(20) .ne. 'none' ) then
            write ( lunut, * ) ' Opening the vdf    file:', fnamep(20)(1:len_trim(fnamep(20)))
            open ( lunitp(20), file = fnamep(20), form = 'binary' )
            if ( lunitp(20) .eq. 0 ) write ( lunut, * ) ' Warning the vdf file does not exist !'
         else
            lunitp(20) = 0
         endif
         if ( lunitp(20) .eq. 0 ) vdiff = 0.0
         if ( lunitp(21) .gt. 0 .and. fnamep(21) .ne. 'none' ) then
            write ( lunut, * ) ' Opening the tau    file:', fnamep(21)(1:len_trim(fnamep(21)))
            open ( lunitp(21), file = fnamep(21), form = 'binary' )
            if ( lunitp(21) .eq. 0 ) write ( lunut, * ) ' Warning the tau file does not exist !'
         else
            lunitp(21) = 0
         endif
         caltau = .false.
         if ( lunitp(21) .eq. 0 ) caltau = .true.
         volumep(cellpntp(:)) = dwqvol(:)
         do i = 1, noqp
            if ( flowpntp(i,1) .gt. 0 ) flow(flowpntp(i,1)) = flow(flowpntp(i,1)) + dwqflo(i)
            if ( flowpntp(i,2) .gt. 0 ) flow(flowpntp(i,2)) = flow(flowpntp(i,2)) + dwqflo(i)
         enddo
         depmin = (0.05*nmaxp*mmaxp)/mnmaxk
         depmin = max(depmin,0.001)
         do i = 1, mnmaxk        !       limit volume to depmin
            i2 = mod(i-1,nmaxp*mmaxp) + 1
            volumep(i) = max(volumep(i), area(i2) * depmin)
         enddo

         call rdpart ( lunitp(1) , lunut     , fnamep(1) )
         call plotgrp( npgrid    , pg        , nmaxp     , mmaxp    , lgrid    ,
     &                 lgrid2    , xb        , yb        )
         call part08 ( lunut     , nodye     , nocont    , ictmax   , amassd   ,
     &                 ictime    , amassc    , aconc     , tmass    , tmassc   ,
     &                 nosubs    , ndprt     , tmassu    , ftime    , linear   ,
     &                 substi    , nmdyer    , nmconr    )
         call part06 ( lunut     , lgrid     , lgrid2    , nmaxp    , mmaxp    ,
     &                 xb        , yb        , nodye     , nocont   , xwaste   ,
     &                 ywaste    , nwaste    , mwaste    )
         call getdps ( lunut     , lunitp(17), fnamep(17), dpsp     , ltrack   )
         call part03 ( lgrid     , volumep   , flow      , dx       , dy       ,
     &                 nmaxp     , mmaxp     , mnmaxk    , lgrid2   , velo     ,
     &                 layt      , area      , depth     , dpsp     , locdep   ,
     &                 zlevel    , tcktot    , ltrack)
         if (ltrack) then
            call part11 ( lgrid    , xb       , yb        , nmaxp   , npart    ,
     &                    mpart    , xpart    , ypart     , xa      , ya       ,
     &                    nopart   , npwndw   , lgrid2    , kpart   , zpart    ,
     &                    za       , locdep   , dpsp      , layt    , mmaxp    ,
     &                    tcktot   )
            call wrttrk ( lunut    , fout     , fnamep(16), itrakc  , nopart   ,
     &                    xa       , ya       , za        , xyztrk  , npmax    )
         endif
         oil    = modtyp == 4
         if ( ini_opt .eq. 1 .and. oil ) then
            call inipart( lgrid    , lgrid2   , nmaxp     , mmaxp   , xb       ,
     &                    yb       , nopart   , nosubs    , substi  , ini_file ,
     &                    xpol     , ypol     , npolmax   , wpart   , xpart    ,
     &                    ypart    , zpart    , npart     , mpart   , kpart    ,
     &                    iptime   , lunut    )
         endif
         nopart   = 0
         npwndw   = 1
         npwndn   = 0
         acomp  = .false.
         accrjv = 1.0e-9_sp
         ltrack = notrak  /=  0
         oil2dh = oil .and. layt == 1
         oil3d  = oil .and. layt  > 1
         if (oil2dh) hmin = const(noconsp ) ! 2dh: last par        =hmin
         if (oil3d) then
            hmin     = const(noconsp-1)
            defang   = const(noconsp)
         endif
         if (modtyp == 1.or.modtyp >= 3) then
            pblay = 0.0
         elseif(modtyp==2) then
            pblay = 0.7
         else
            write(*,*) 'This model type has not been implemented yet '
            call srstop(1)
         endif
         ptlay  = 1.0 - pblay
         nstep = 1 + (itstopp - itstrtp)/idelt
         itrakc = 0
         itraki = 1
         if ( ltrack) itrakc = itrakc + itraki
         call exit_alloc ( i2 )
      endif

      if ( timon ) call timstop ( ithandl )
      return
      end
