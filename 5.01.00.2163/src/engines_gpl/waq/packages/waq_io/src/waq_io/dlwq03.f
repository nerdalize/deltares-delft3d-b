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

      subroutine dlwq03 ( lun    , lchar  , filtype, nrftot , nrharm ,
     &                    ivflag , dtflg1 , iwidth , dtflg3 , vrsion ,
     &                    ioutpt , gridps , syname , ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>              Reads grid layout; attributes and the computational volumes
!>
!>              This routine reads:
!>                 - the number of computational volumes
!>                 - the number of layers (optional)
!>                 - the number and info of additional grids (optional)
!>                 - any wish for a printed grid layout
!>                 - constant attribute arrays
!>                 - time varying attribute arrays
!>                 - information on the time series of volumes

!       Created           : April '88  BY M.E. Sileon / L. Postma

!       Modified          : ???????    by Jan van Beek  : Fixed and time varying attributes
!                           April 1996 by Leo Postma    : Version support
!                         : April 1997 by Rinze Bruinsma: Tokenized input data file reading added
!                           July  2002 by Leo Postma    : Call to Opt1 changed.
!                           May   2011 by Leo Postma    : Modernized and merged with layered bed

!       Subroutines called: grid    read grid structures
!                           opt0    read constant/time-variable block
!                           opt1    get & open ( include ) file
!                           dhopnf  open file
!                           DHKMRK  get an attribute from an attribute integer
!                           srstop  stop with error code
!                           check   end of block

!       Logical units     : LUN(40) = unit number to read attributes from binary file
!                           LUN( 2) = unit intermediate file (system)
!                           LUN( 6) = unit intermediate file (grid)
!                           LUN( 7) = unit intermediate file (volumes)

      use grids        !   for the storage of contraction grids
      use rd_token     !   for the reading of tokens
      use partmem      !   for PARTicle tracking
      use timers       !   performance timers

      implicit none

!     Parameters

!     kind           function         name                Descriptipon

      integer  ( 4), intent(in   ) :: lun    (*)        !< array with unit numbers
      character( *), intent(inout) :: lchar  (*)        !< array with file names of the files
      integer  ( 4), intent(inout) :: filtype(*)        !< type of binary file
      integer  ( 4), intent(inout) :: nrftot (*)        !< number of function items
      integer  ( 4), intent(inout) :: nrharm (*)        !< number of harmonic items
      integer  ( 4), intent(  out) :: ivflag            !< computed volumes ?
      logical      , intent(in   ) :: dtflg1            !< 'date'-format 1st timescale
      integer  ( 4), intent(in   ) :: iwidth            !< width of the output file
      logical      , intent(in   ) :: dtflg3            !< 'date'-format (F;ddmmhhss,T;yydddhh)
      real     ( 4), intent(in   ) :: vrsion            !< version number of this input
      integer  ( 4), intent(in   ) :: ioutpt            !< flag for more or less output
      character(20), intent(in   ) :: syname (*)        !< array with substance names
      integer  ( 4), intent(inout) :: ierr              !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar              !< cumulative warning count
      type(GridPointerColl)           GridPs            !< Collection of grid pointers

      include 'sysn.inc'        !     common  /  sysn  /    System dimensions

!     local decalations

      character*255           cdummy            !  workspace to read a string
      integer                 idummy            !  location to read an integer
      logical                 disper            !  is opt0 called for dispersions ?
      logical                 volume            !  is opt0 called for volumes ?
      integer                 ifact             !  needed for call to opt0
      integer                 itype             !  type of token that is returned
      integer                 ierr2             !  local error indicator
      integer                 iwar2             !  local warning indicator
      integer, allocatable :: ikmerge(:)        !  array with indicators whether attributes are already set
      integer, allocatable :: iamerge(:)        !  composite attribute array
      integer, allocatable :: ikenm  (:)        !  array with attributes of an input block
      integer, allocatable :: iread  (:)        !  array to read attributes
      integer, allocatable :: pgrid  (:,:)      !  workspace for matrix with printed grids
      integer                 imopt1            !  first option for grid layout
      integer                 i, j, k           !  loop counters
      integer                 ikerr             !  error indicator attributes
      integer                 nkopt             !  number of attribute blocks
      integer                 nopt              !  that many attributes in this block
      integer                 ikopt1            !  first (file) option attributes
      integer                 ikopt2            !  second (default/nondefault) option attributes
      integer                 ikdef             !  default value attributes
      integer                 nover             !  number of overridings
      integer                 iover             !  overriding cell number
      integer                 iseg              !  loop counter computational volumes
      integer                 iknm1, iknm2      !  help variables for attributes
      integer                 iknmrk            !  help variables merged attributes
      integer                 ivalk             !  return value dhknmrk
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq03", ithndl )

      disper = .false.
      volume = .true.
      ifact  = 1
      ivflag = 0
      ierr2  = 0
      iwar2  = 0
      iposr  = 0

!       Read number of computational volumes

      if ( gettoken( noseg, ierr2 ) .gt. 0 ) goto 240
      if ( noseg .gt. 0 ) then
         write ( lunut , 2000 ) noseg
      else
         write ( lunut , 2010 ) noseg
         ierr = ierr+1
      endif
      if ( .not. alone ) then
         if ( noseg .ne. nosegp ) then
            write ( lunut , 2015 ) nosegp
            ierr = ierr + 1
         endif
      endif

!       Read optional multiple grids

      call grid ( lun    , noseg  , notot  , nolay  , gridps ,
     &            nseg2  , nogrid , syname , ierr2  , iwar2  )
      if ( ierr2 .gt. 0 ) goto 240

!       Read grid-layout for visually printed output

      if ( gettoken( cdummy, imopt1, itype, ierr2 ) .gt. 0 ) goto 240
      if ( itype .eq. 1 ) then
         if ( cdummy .eq. 'NONE' ) then
            write ( lunut , 2050 )
            nx = 0
            ny = 0
         else
            write ( lunut, 2045 ) cdummy
            ierr = ierr + 1
         endif
      else
         write ( lunut , 2030 ) imopt1
         select case ( imopt1 )

            case ( :-2 , 3: )
               write ( lunut, 2040 )
               ierr = ierr + 1
            case ( 2 )
               write ( lunut , 2050 )
               nx = 0
               ny = 0
            case default
!                   call with record length 0 => IMOPT1 of -4 not allowed
               call opt1 ( imopt1  , lun     , 6       , lchar   , filtype ,
     &                     dtflg1  , dtflg3  , 0       , ierr2   , iwar2   )
               if ( ierr2 .gt. 0 ) goto 240
               if ( gettoken( nx, ierr2 ) .gt. 0 ) goto 240
               if ( gettoken( ny, ierr2 ) .gt. 0 ) goto 240
               write ( lunut , 2060 ) nx, ny
               if ( imopt1 .ne. 0 ) then      !  else an adequate file was given
                  allocate ( pgrid(nx,ny) )
                  do j = 1 , ny
                     do i = 1 , nx
                        if ( gettoken( pgrid(i,j), ierr2 ) .gt. 0 ) goto 240
                        if ( pgrid(i,j) .gt. noseg+nseg2 ) then
                           write ( lunut , 2070 ) pgrid(i,j)
                           ierr = ierr+1
                        endif
                     enddo
                  enddo
                  if ( ioutpt .lt. 2 ) then
                     write ( lunut , 2080 )
                  else
                     do i = 1 , nx , 2*iwidth
                        write ( lunut , 2090 ) (k,k=i,min(nx,i+2*iwidth-1))
                        do j = 1, ny
                           write ( lunut , 2100 )
     &                             j, (pgrid(k,j),k=i,min(nx,i+2*iwidth-1))
                        enddo
                     enddo
                     if ( nx*ny .gt. 0 ) then
                        call dhopnf  ( lun(6), lchar(6), 6    , 1   , ierr2 )
                        write ( lun(6) ) pgrid
                        close ( lun(6) )
                     else
                        write ( lunut , 2050 )
                     endif
                  endif
                  deallocate ( pgrid )
               endif

         end select

      endif

!     Attribute array

      ikerr   = 0
      allocate ( iamerge(noseg+nseg2) )                        !   composite attribute array
      allocate ( ikmerge(    10     ) )                        !   flags of filles attributes
      allocate ( iread  (noseg+nseg2) )                        !   work array to read the attributes
      iamerge = 0
      ikmerge = 0

      if ( vrsion .lt. 4.20 ) then                             !   attributes not supported
         nkopt = 0
      else
         if ( gettoken( nkopt, ierr2 ) .gt. 0 ) goto 240
         write ( lunut , 2110 ) nkopt                          !   so many blocks of input are provided
      endif

      do 20 i = 1 , nkopt                                      !   read those blocks

         if ( gettoken( nopt, ierr2 ) .gt. 0 ) goto 240
         write ( lunut , 2120 ) nopt                           !   number of attributes in this block
         allocate ( ikenm(nopt) )
         do j = 1, nopt                                        !   get the attribute numbers
            if ( gettoken( ikenm(j), ierr2 ) .gt. 0 ) goto 240
         enddo

         if ( gettoken( ikopt1, ierr2 ) .gt. 0 ) goto 240      !   the file option for this info
         write ( lunut , 2130 ) ikopt1
         call opt1 ( ikopt1  , lun     , 40      , lchar   , filtype ,
     &               dtflg1  , dtflg3  , 0       , ierr2   , iwar2   )
         if ( ierr2  .gt. 0 ) goto 240
         if ( ikopt1 .eq. 0 ) then                             !   binary file
            call dhopnf  ( lun(40) , lchar(40) , 40 , 2 , ierr2 )
            read  ( lun(40) , end=250 , err=260 ) ( iread(j), j=1, noseg )
            close ( lun(40) )
         else
            if ( gettoken( ikopt2, ierr2 ) .gt. 0 ) goto 240   !   second option
            write ( lunut , 2140 ) ikopt2

            select case ( ikopt2 )

               case ( 1 )                                      !   no defaults
                  write ( lunut , 2150 )
                  if ( ioutpt .ge. 5 ) then
                     write ( lunut , 2160 )
                  else
                     write ( lunut , 2170 )
                  endif
                  do j = 1, noseg
                     if ( gettoken( iread(j), ierr2 ) .gt. 0 ) goto 240
                     if ( ioutpt .ge. 5 ) write ( lunut , 2180 ) j , iread(j)
                  enddo

               case ( 2 )                                      !   default with overridings
                  write ( lunut , 2190 )
                  if ( gettoken( ikdef, ierr2 ) .gt. 0 ) goto 240
                  if ( gettoken( nover, ierr2 ) .gt. 0 ) goto 240
                  write ( lunut , 2200 )ikdef, nover
                  if ( ikerr .eq. 0 ) then                     !   only assign if no previous error
                     do iseg = 1 , noseg
                        iread(iseg) = ikdef
                     enddo
                  endif
                  if ( nover .gt. 0 ) then
                     if ( ioutpt .ge. 3 ) then
                        write ( lunut , 2210 )
                     else
                        write ( lunut , 2220 )
                     endif
                     do j = 1, nover
                        if ( gettoken( iover , ierr2 ) .gt. 0 ) goto 240
                        if ( gettoken( idummy, ierr2 ) .gt. 0 ) goto 240
                        if ( iover .lt. 1 .or. iover .gt. noseg ) then
                           write ( lunut , 2230 ) j, iover
                           ierr = ierr + 1
                        else
                           if ( ioutpt .ge. 3 ) write ( lunut , 2240 ) j, iover, idummy
                           iread(iover) = idummy
                        endif
                     enddo
                  endif

               case default
                  write ( lunut , 2250 )
                  ierr = ierr + 1

            end select

         endif

!        Merge file buffer with attributes array in memory

         do 10 iknm2 = 1 , nopt
            iknm1 = ikenm(iknm2)

!                 see if merged already

            if ( ikmerge(iknm1) .ne. 0  ) then
               write ( lunut , 2260 ) iknm2, iknm1
               ierr  = ierr + 1
               ikerr = 1
               exit
            endif

!                 see if valid

            if ( iknm1 .le. 0 .or. iknm1 .gt. 10 ) then
               if ( iknm1 .eq. 0 ) then
                  write ( lunut , 2270 ) iknm2
                  iwar2 = iwar2 + 1
               else
                  write ( lunut , 2280 ) iknm1,iknm2
                  iwar2 = iwar2 + 1
               endif
               cycle                    !  skip
            endif

!              Merge for this attribute

            write ( lunut , 2290 ) iknm2, iknm1
            ikmerge(iknm1) = 1
            iknmrk = 10**(iknm1-1)
            do iseg = 1 , noseg
               call DHKMRK( iknm2, iread(iseg), ivalk )
               iamerge(iseg) = iamerge(iseg) + iknmrk*ivalk
            enddo
   10    continue
         deallocate ( ikenm )
   20 continue

!     Time dependent attributes

      if ( vrsion .lt. 4.20 ) then                             !   attributes not supported
         ikopt2 = 0
      else
         if ( gettoken( ikopt2, ierr2 ) .gt. 0 ) goto 240
         write ( lunut , 2300 ) ikopt2
      endif
      if ( ikopt2 .eq. 1 ) then                                !   this file
         write ( lunut, 2310 )
         if ( gettoken( nopt, ierr2 ) .gt. 0 ) goto 240
         write ( lunut, 2120 ) nopt
         do j = 1, nopt
            if ( gettoken( iknm1, ierr2 ) .gt. 0 ) goto 240
            if ( iknm1 .le. 0 .or. iknm1 .gt. 10 ) then
               if ( iknm1 .eq. 0 ) then
                  write ( lunut , 2270 ) j,iknm1
                  iwar2 = iwar2 + 1
               else
                  write ( lunut , 2280 ) iknm2,iknm1
                  iwar2 = iwar2 + 1
               endif
               cycle
            endif

!              Merge for this attribute is performed in DELWAQ2

            if ( ikmerge(iknm1) .eq. 0  ) then
               write ( lunut , 2290 ) iknm2,iknm1
               ikmerge(iknm1) = 1
            else
               write ( lunut , 2260 ) iknm2,iknm1
               ierr  = ierr + 1
               ikerr = 1
            endif
         enddo

         ifiopk = 0
         if ( gettoken( ikopt1, ierr2 ) .gt. 0 ) goto 240
         write ( lunut , 2130 ) ikopt1
         call opt1 ( ikopt1  , lun     , 40      , lchar   , filtype ,
     &               dtflg1  , dtflg3  , 0       , ierr2   , iwar2   )
         if ( ierr2 .gt. 0 ) goto 240
         if ( ikopt1 .eq. 0 ) then
            write ( lunut , 2320 )
            ifiopk = 1
         elseif(ikopt1 .eq. -2 ) then
            write ( lunut , 2330 )
            ifiopk = 2
         else
            write ( lunut , 2340 )
            ierr = ierr + 1
         endif

      else

         write ( lunut , 2350 )

      endif

!     Set default behaviour if not specified

      if ( ikmerge(1) .eq. 0  .and. ikerr .eq. 0 ) then
         write ( lunut , 2360 )
         iamerge = iamerge + 1
      endif
      if ( ikmerge(2) .eq. 0 ) write ( lunut, 2370 )
      if ( nseg2      .gt. 0 ) write ( lunut, 2380 ) nseg2
      do i = noseg+1, noseg+nseg2      ! bottom segments are 3 - always active!
         iamerge(i) = (iamerge(i)/10)*10 + 3
      enddo

!     Write to file

      if ( ikerr .eq. 0 ) write( lun(2) ) iamerge
      deallocate ( ikmerge, iamerge )

!       read segment volumes

      write ( lunut , 2390 )
      ierr2 = 0
      call opt0   ( lun    , 7      , 0        , 0        , noseg  ,
     &              1      , 1      , nrftot(2), nrharm(2), ifact  ,
     &              dtflg1 , disper , volume   , iwidth   , lchar  ,
     &              filtype, dtflg3 , vrsion   , ioutpt   , ierr2   ,
     &              iwar2  )
      if ( .not. alone ) then
         if ( lchar(7) .ne. fnamep(6) ) then
            write ( lunut , 2395 ) fnamep(6)
            ierr = ierr + 1
         endif
      endif
      if ( .not. volume ) ivflag = 1
      ierr  = ierr + ierr2
      ierr2 = 0
  240 continue
      if ( ierr2 .gt. 0 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      goto 270

!     error processing

  250 write ( lunut , 2400 ) lun(40), lchar(40)
      ierr = ierr+1
      goto 270

  260 write ( lunut , 2410 ) lun(40), lchar(40)
      ierr = ierr+1

  270 call check  ( cdummy , iwidth , 3      , ierr2  , ierr   )
      iwar = iwar + iwar2
      if ( timon ) call timstop( ithndl )
      return

!       Output formats

 2000 format ( //' Number of segments :',I15 )
 2010 format ( / ' ERROR, invalid number of segments:',I10 )
 2015 format (   ' ERROR, nr of volumes in Delwaq not equal to nr of volumes in Delpar:',I10 )
 2020 format (   ' Number of layers in base grid :',I10)
 2030 format ( / ' option selected for grid layout :' , I2 )
 2040 format ( / ' ERROR, option for grid layout not implemented !!!!!')
 2045 format ( / ' ERROR, invalid keyword for grid layout: ',A20 )
 2050 format (   ' Grid layout not used.')
 2060 format ( / ' Grid layout, width :',I4,' depth :',I4  )
 2070 format ( / ' ERROR, invalid number in grid layout:',I6 )
 2080 format (   ' Information on gridlayout will be printed for output option 2 and higher ' )
 2090 format ( 10X  ,20I6,/)
 2100 format ( I6,4X,20I6 )
 2110 format ( /,I3,' blocks of time independent attribute contribution')
 2120 format ( /,I3,' attributes in this block')
 2130 format ( / ' Selected file option is ',I3)
 2140 format ( / ' second selected option',I3)
 2150 format ( / ' constant values without defaults')
 2160 format (   '        segment   value ')
 2170 format (   ' Information will be printed for output option is ',
     &                                                '5 or higher !' )
 2180 format (   '       ',I6,1X,I9.9)
 2190 format ( / ' constant values with defaults and overridings')
 2200 format ( / ' Default value :',I9.9,
     &         / ' number of overridings :',I6)
 2210 format (   '        number   segment   value ')
 2220 format (   ' Information will be printed for output option is ',
     &                                                '3 or higher !' )
 2230 format (   ' ERROR ',I6,' ',I6,' segment number out of range')
 2240 format (   '       ',I6,' ',I6,' ',I9.9)
 2250 format ( / ' ERROR, option out of range.')
 2260 format ( / ' ERROR, Pointer of contribution ',I6,' mapped to',
     &           ' attribute',I6,' already specified.')
 2270 format ( / ' WARNING, Pointer of contribution ',I6,' zero,',
     &           ' attribute skipped.')
 2280 format ( / ' WARNING, Pointer of contribution ',I6,'=',I6,
     &           ' is out of range(1-10), attribute skipped')
 2290 format ( / ' Pointer of contribution ',I6,' mapped to',
     &           ' attribute',I6)
 2300 format ( / ' option for time dependent attributes ',I3)
 2310 format (   ' time dependent attributes in binary file')
 2320 format (   ' Binary file with one record per time step.')
 2330 format (   ' Block data with breakpoints in binary file.')
 2340 format (   ' ERROR, file option not allowed for this item.')
 2350 format (   ' NO time dependent attributes')
 2360 format (   ' No first attribute set, using default 1')
 2370 format (   ' No second attribute set, using default 0')
 2380 format (   ' Attribute of',I7,' bottom segments set to 2' )
 2390 format ( / ' Volumes:' )
 2395 format ( / ' ERROR, volumes for Delpar from different file : ',A20 )
 2400 format ( / ' ERROR, end of file on unit:' ,I3, / ' Filename: ',A20 )
 2410 format ( / ' ERROR, reading file on unit:',I3, / ' Filename: ',A20 )

      end
