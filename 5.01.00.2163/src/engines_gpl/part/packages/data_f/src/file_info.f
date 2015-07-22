module fileinfo
!
!     Unit numbers, file names and file types for the (20) input and output files
!     See also file name : filename.dat
!     The 18th file is the .hyd file.
!     The 19th file is the .poi from-to pointer file
!     The 20th file is the .vdf vertical diffusion file.
!
      use precision                                       ! single and double precision

      integer(ip)  , parameter          :: nfiles =  23
      integer(ip)                       :: lunit(nfiles) = 0    ! logical unit numbers for in-/output files
      character(len=256)                :: fname(nfiles) = ' '  ! file names for in-/output files
      character(len=20) , dimension(2)  :: ftype         = ' '  ! file types, i.e. unformatted or binary
      save
end module
