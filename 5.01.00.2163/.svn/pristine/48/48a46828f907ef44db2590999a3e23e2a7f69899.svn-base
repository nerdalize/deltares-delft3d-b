module dealloc_mod
!
!  Generic module for memory de-allocation, 
!  including proper error handling 
!
!  Notice that de-allocating pointers does not work always, e.g. 
!  de-allocation pointers to array sections is invalid,
!  so proper error handling is required.
!
!
!  data definition module(s)
!
use precision              ! single and double precision
use fileinfo , lun=>lunit  ! file information for input/output files
!
implicit none   ! force explicit typing
!
interface dealloc
    module procedure dealloc_int_1D     ! for deallocating 1D integer arrays
    module procedure dealloc_int_2D     ! for deallocating 2D integer arrays
    module procedure dealloc_int_3D     ! for deallocating 3D integer arrays
    module procedure dealloc_int_4D     ! for deallocating 4D integer arrays    
    module procedure dealloc_real_1D    ! for deallocating 1D real    arrays
    module procedure dealloc_real_2D    ! for deallocating 2D real    arrays
    module procedure dealloc_real_3D    ! for deallocating 3D real    arrays    
    module procedure dealloc_real_4D    ! for deallocating 4D real    arrays  
    module procedure dealloc_double_1D  ! for deallocating 1D double  arrays      
    module procedure dealloc_char_1D    ! for deallocating 1D char.   arrays
    module procedure dealloc_char_2D    ! for deallocating 2D char.   arrays    
end interface   
contains
! ----------------------------------------------------
!     INTEGER ARRAYS
! ----------------------------------------------------
      subroutine dealloc_int_1D(arr) 
      integer, dimension(:), pointer :: arr
      integer                        :: stat
      logical                        :: dealloc_ok
      if (associated(arr)) then 
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_1D
! ----------------------------------------------------
      subroutine dealloc_int_2D(arr) 
      integer, dimension(:,:), pointer :: arr
      integer                          :: stat
      logical                          :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_2D
! ----------------------------------------------------      
      subroutine dealloc_int_3D(arr) 
      integer, dimension(:,:,:), pointer :: arr
      integer                            :: stat
      logical                            :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_3D
! ----------------------------------------------------
      subroutine dealloc_int_4D(arr) 
      integer, dimension(:,:,:,:), pointer :: arr
      integer                              :: stat
      logical                              :: dealloc_ok
      if (associated(arr)) then 
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_4D
! ----------------------------------------------------
!     REAL ARRAYS
! ----------------------------------------------------
      subroutine dealloc_real_1D(arr) 
      real(sp), dimension(:), pointer :: arr
      integer                        :: stat
      logical                        :: dealloc_ok
      if (associated(arr)) then 
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_1D
! ----------------------------------------------------      
      subroutine dealloc_double_1D(arr) 
      real(dp), dimension(:), pointer :: arr
      integer                         :: stat
      logical                         :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_double_1D
! ----------------------------------------------------
      subroutine dealloc_real_2D(arr) 
      real(sp), dimension(:,:), pointer :: arr
      integer                           :: stat
      logical                           :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_2D
! ----------------------------------------------------      
      subroutine dealloc_real_3D(arr) 
      real(sp), dimension(:,:,:), pointer :: arr
      integer                             :: stat
      logical                             :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif   
      return         
      end subroutine dealloc_real_3D
! ----------------------------------------------------
      subroutine dealloc_real_4D(arr) 
      real(sp), dimension(:,:,:,:), pointer :: arr
      integer                               :: stat
      logical                               :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_4D
! ----------------------------------------------------
!     CHARACTER ARRAYS
! ----------------------------------------------------
      subroutine dealloc_char_1D(arr) 
      character (len=*), dimension(:), pointer :: arr
      integer                                  :: stat
      logical                                  :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_char_1D
! ----------------------------------------------------
      subroutine dealloc_char_2D(arr) 
      character (len=*), dimension(:,:), pointer :: arr
      integer                                    :: stat
      logical                                    :: dealloc_ok
      if (associated(arr)) then       
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_char_2D
! ----------------------------------------------------
      subroutine dealloc_error()
         write(lun(2),'(//a)') ' * Part Memory Error '      
         write(lun(2),'(  a)') ' *   Could not deallocate memory'      
         write(lun(2),'(a//)') ' *   Please contact Delft3D Support    '               
         write(*     ,'(//a)') ' * Part Memory Error '      
         write(*     ,'(  a)') ' *   Could not deallocate memory'      
         write(*     ,'(a//)') ' *   Please contact Delft3D Support    '               
         STOP ' Part aborted ! '
      end subroutine dealloc_error          
end module dealloc_mod