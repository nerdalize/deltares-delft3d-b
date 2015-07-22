      subroutine fill_old_items(old_items)

!     Deltares Software Centre

!>/File
!>      add the old items to the collection

      use timers         !< performance timers
      use processet      !< use processet definitions
      implicit none

      ! decalaration of arguments

      type(old_item_coll)                :: old_items         !< the old_items table to be filled

      ! common declarations

      include 'data.inc'                           ! tables read from proces definition file

      ! local declaration

      type(old_item)            :: a_old_item        ! single process
      integer                   :: i                 ! loop counter old items
      integer                   :: i2                ! index in collection
      integer(4)                :: ithndl = 0        ! handle for performance timer
      if (timon) call timstrt( "fill_old_items", ithndl )

      do i = 1, n_old_items

         a_old_item%old_name      = old_items_old_name(i)
         a_old_item%new_name      = old_items_new_name(i)
         a_old_item%old_default   = old_items_old_default(i)
         a_old_item%configuration = old_items_configuration(i)
         a_old_item%serial        = old_items_serial(i)
         a_old_item%action_type   = old_items_action_type(i)

         i2 = old_item_coll_add( old_items, a_old_item)

      enddo

      if (timon) call timstop( ithndl )
      return
      end
