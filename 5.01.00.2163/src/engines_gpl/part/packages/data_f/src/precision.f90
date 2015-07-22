module precision
!
!  module declarations
!
      implicit none
      integer, parameter :: ip = 4            ! precision of normal integers
      integer, parameter :: rp = 4            ! precision of normal reals
      integer, parameter :: sp = kind(1.0e0)  ! single precision
      integer, parameter :: dp = kind(1.0d0)  ! double precision
      integer, parameter :: fp = sp
      integer, parameter :: hp = dp
end module
