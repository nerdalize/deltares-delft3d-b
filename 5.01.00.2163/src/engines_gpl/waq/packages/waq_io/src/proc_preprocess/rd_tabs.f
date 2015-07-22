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

      subroutine rd_tabs( pdffil, lunrep, versio, serial, noinfo,
     +                    nowarn, nerror)

!     Deltares Software Centre

!>/File
!>                read process definition tables from nefis format

!     Created   : June  1999 by Jan van Beek

!     Modified  : Aug   2012 by Jan van Beek : just read the tables

      use timers         !< performance timers
      implicit none

      ! arguments

      character(len=*)                  :: pdffil                 !< proces defintion file
      integer           , intent(in   ) :: lunrep                 !< report file
      real              , intent(  out) :: versio                 !< version number proces defintion file
      integer           , intent(  out) :: serial                 !< serial number proces defintion file
      integer           , intent(inout) :: noinfo                 !< cummulative information count
      integer           , intent(inout) :: nowarn                 !< cummulative warning count
      integer           , intent(inout) :: nerror                 !< cummulative error count

      ! common declarations

      include 'data.inc'
c
c     declaration of file identification group
c
      real          vfform
      character*20  rundat
      character*40  fform      , conten      ,
     +              source
      character*40  remark(4)
c
c     local variables
c
      integer       iconf           , ilen            ,
     +              iend            , i               ,
     +              ierror
      integer       deffds
      character*1   coding, access
      logical       lexi
      character*256 fildef, fildat
      character*256 filext
      integer       extpos, extlen
c
c     external nefis functions
c
      integer   clsnef
     +         ,crenef
      external  clsnef
     +         ,crenef
      integer(4)                :: ithndl = 0      ! handle for performance timer
      if (timon) call timstrt( "rd_tabs", ithndl )
c
c     initialize proces definition file
c
      call dhfext (pdffil, filext, extpos, extlen)
      if ( filext .ne. ' ' ) then

         ! files with extension, assume nefis file made out of one file, fildat equals fildef

         fildat = pdffil
         fildef = pdffil

      else

         ! no extension assume nefis dat and def file, but check existence def file

         ilen = len(pdffil)
         do i = ilen , 1 , -1
            iend = i
            if ( pdffil(i:i) .ne. ' ' ) goto 10
         enddo
         iend = 0
   10    continue
         if ( iend .eq. ilen ) iend = max(0,ilen-4)

         pdffil(iend+1:) = '.dat'
         fildat = pdffil
         pdffil(iend+1:) = '.def'
         fildef = pdffil
         inquire ( file=fildef , exist = lexi )
         if ( .not. lexi ) then
            fildef = fildat
         endif

      endif


      ! open nefis file

      inquire ( file=fildat , exist = lexi )
      if ( lexi ) then
         access = 'r'
         coding = 'n'
         ierror = crenef(deffds, fildat, fildef, coding, access)
         if ( ierror .ne. 0 ) then
            nerror = nerror + 1
            call dhpfil(lunrep,' error opening nefis file(s):',pdffil(1:iend))
            write(lunrep,*) 'error number:',ierror
            goto 900
         endif
      else
         nerror = nerror + 1
         call dhpfil(lunrep,'error opening nefis file(s):',pdffil(1:iend))
         write(lunrep,*) 'files do not exist'
         goto 900
      endif
c
c     last file identification group
c
      call rd_filid ( deffds,         fform , vfform, conten,
     +                versio, serial, rundat, source, remark,
     +                lunrep, ierror)
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading file identification group'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table p1 (substance groups)
c
      call rd_tabp1 ( deffds      ,
     +                nsgrpm      , nsgrp       ,
     +                sgrpid      , sgrpnm      ,
     +                lunrep      , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table p1'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table p2 (items)
c
      call rd_tabp2 ( deffds      ,
     +                nitemm      , nitem       ,
     +                itemid      , itemnm      ,
     +                itemun      , itemde      ,
     +                itemag      , itemda      ,
     +                itemgr      , itemse      ,
cjvb +                itemgr      , itemsx      ,
     +                itemwk      , lunrep      ,
     +                ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table p2'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table p3 (process modules)
c
      call rd_tabp3 ( deffds      ,
     +                nfortm      , nfort       ,
     +                fortid      , lunrep      ,
     +                ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table p3'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table p4 (processes)
c
      call rd_tabp4 ( deffds      ,
     +                nprocm      , nproc       ,
     +                procid      , procnm      ,
     +                procfo      , procco      ,
     +                lunrep      , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table p4'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table p5 (configurations)
c
      call rd_tabp5 ( deffds      ,
     +                nconfm      , nconf       ,
     +                confid      , confnm      ,
     +                lunrep      , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table p5'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r1 (configurations-processes)
c
      call rd_tabr1 ( deffds       ,
     +                nconfm*nprocm, nconf       ,
     +                nproc        , icnpro      ,
     +                lunrep       , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r1'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r2 (configurations-substances)
c
      call rd_tabr2 ( deffds      ,
     +                ncnsbm      , ncnsb       ,
     +                r2_cid      , r2_sid      ,
     +                lunrep      , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r2'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r3 (input items)
c
      call rd_tabr3 ( deffds      ,
     +                ninpum      , ninpu       ,
     +                inpupr      , inpuit      ,
     +                inpunm      , inpude      ,
     +                inpudo      , inpusx      ,
     +                lunrep      , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r3'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r4 (output items)
c
      call rd_tabr4 ( deffds      ,
     +                noutpm      , noutp       ,
     +                outppr      , outpit      ,
     +                outpnm      , outpdo      ,
     +                outpsx      , lunrep      ,
     +                ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r4'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r5 (output fluxes)
c
      call rd_tabr5 ( deffds      ,
     +                noutfm      , noutf       ,
     +                outfpr      , outffl      ,
     +                outfnm      , outfdo      ,
     +                lunrep      , ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r5'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r6 (flux-substance)
c
      call rd_tabr6 ( deffds      ,
     +                nstocm      , nstoc       ,
     +                stocfl      , stocsu      ,
     +                stocsc      , lunrep      ,
     +                ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r6'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r7 (velocity-substance)
c
      call rd_tabr7 ( deffds      ,
     +                nvelom      , nvelo       ,
     +                veloit      , velosu      ,
     +                velosc      , lunrep      ,
     +                ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r7'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table r8 (dispersion-substance)
c
      call rd_tabr8 ( deffds      ,
     +                ndispm      , ndisp       ,
     +                dispit      , dispsu      ,
     +                dispsc      , lunrep      ,
     +                ierror      )
      if ( ierror .ne. 0 ) then
         nerror = nerror + 1
         write(lunrep,*) 'error reading table r8'
         write(lunrep,*) 'error number:',ierror
         goto 900
      endif
c
c     table m1 (old-items)
c
      if ( vfform .gt. 1.99 ) then
         call rd_tabm1 ( deffds      ,
     +                   n_old_items_max,
     +                   n_old_items,
     +                   old_items_old_name,
     +                   old_items_new_name,
     +                   old_items_old_default,
     +                   old_items_configuration,
     +                   old_items_serial,
     +                   old_items_action_type,
     +                   lunrep      ,
     +                   ierror      )
         if ( ierror .ne. 0 ) then
            nerror = nerror + 1
            write(lunrep,*) 'error reading table m1'
            write(lunrep,*) 'error number:',ierror
            goto 900
         endif
      endif
c
c     close files
c
      ierror = clsnef(deffds)
      if ( ierror .ne. 0 ) then
         write(lunrep,*) 'error closing nefis process defintion file'
         write(lunrep,*) 'error number:',ierror
         nerror = nerror + 1
      endif

  900 continue
      if (timon) call timstop( ithndl )
      return

      end
