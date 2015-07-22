      subroutine set_active(constants, no_act_max, no_act, actlst)

!     Deltares Software Centre

!>/File
!>                makes list of active processes

!     Created   : Aug   2012 by Jan van Beek

      use timers         !< performance timers
      use dlwq_data      !< data definitions
      use processet      !< use processet definitions
      implicit none

      ! arguments

      type(t_dlwq_item) , intent(inout) :: constants              !< delwaq constants list
      integer           , intent(in   ) :: no_act_max             !< number of activated processes max
      integer           , intent(inout) :: no_act                 !< number of activated processes
      character(len=*)  , intent(inout) :: actlst(*)              !< list of activated processes

      ! local declarations

      integer                           :: nocons                 !  number of constants
      integer                           :: ico                    !  loop counter constants
      integer                           :: i_act                  !  loop counter active
      integer                           :: ix_act                 !  index active
      integer                           :: ix_dbl                 !  index double
      character(len=10)                 :: name10                 !  local process name
      character(len=80)                 :: line                   !  line buffer for report file
      integer                           :: i_old_item             !  loopcounter old_items
      integer(4)                        :: ithndl = 0             !  handle for performance timer
      if (timon) call timstrt( "set_active", ithndl )

      ! check the actives in the constant names

      nocons = constants%no_item
      do ico = 1 , nocons
         call zoek('active', 1, constants%name(ico), 6, ix_act )
         if ( ix_act .gt. 0 ) then

            ! check if double in the list

            name10 = constants%name(ico)(8:17)
            call zoek(name10, no_act, actlst, 10, ix_dbl )
            if ( ix_dbl .le. 0 ) then
               no_act = no_act + 1
               if ( no_act .gt. no_act_max ) then
                  write(line,2130)
                  call monsys(line,1)
                  write(line,2110) no_act,no_act_max
                  call monsys(line,1)
                  call srstop(1)
               endif
               actlst(no_act) = name10
            endif
         endif
      enddo

      ! if bloom then also phy_blo

      name10 = 'bloom'
      call zoek(name10, no_act, actlst, 10, ix_dbl )
      if ( ix_dbl .gt. 0 ) then
         name10 = 'phy_blo'
         call zoek(name10, no_act, actlst, 10, ix_dbl )
         if ( ix_dbl .le. 0 ) then
            write(line,2140)
            call monsys(line,1)
            no_act = no_act + 1
            if ( no_act .gt. no_act_max ) then
               write(line,2130)
               call monsys(line,1)
               write(line,2110) no_act,no_act_max
               call monsys(line,1)
               call srstop(1)
            endif
            actlst(no_act) = name10
         endif
      endif

      if (timon) call timstop( ithndl )
      return
 2110 format ( ' in input :',I6,' maximum :',I6)
 2130 format ( ' ERROR: Local dimension to small for active processes')
 2140 format ( ' Automatic activation of BLOOM ouput process Phy_Blo')
 2150 format ( ' Process name [',a10,'] in input replaced with new name [',a10,']')
 2160 format ( ' Process [',a10,'] added because of active process [',a10,']')
      end
