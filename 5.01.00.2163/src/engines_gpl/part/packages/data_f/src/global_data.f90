module global_pointers

!     Declarations for global pointer arrays

      use precision      ! single and double precision

!     Global character arrays

      character     (len=16   ) ,  pointer, dimension(:       ) :: elt_names
      character     (len=16   ) ,  pointer, dimension(:       ) :: elt_types
      character     (len=20   ) ,  pointer, dimension(:       ) :: cbuff
      character     (len=20   ) ,  pointer, dimension(:       ) :: nmconr
      character     (len=20   ) ,  pointer, dimension(:       ) :: nmdyer
      character     (len=20   ) ,  pointer, dimension(:       ) :: nmstat
      character     (len=20   ) ,  pointer, dimension(:       ) :: subst
      character     (len=20   ) ,  pointer, dimension(:       ) :: subst2
      character     (len=20   ) ,  pointer, dimension(:       ) :: substi
      character     (len=20   ) ,  pointer, dimension(:       ) :: subsud
      character     (len=40   ) ,  pointer, dimension(:       ) :: title
      character     (len=256  ) ,  pointer, dimension(:       ) :: finud

!     Global integer arrays

!     1D integer arrays

      integer       (sp       ) ,  pointer, dimension(:       ) :: floil
      integer       (sp       ) ,  pointer, dimension(:       ) :: ictmax
      integer       (sp       ) ,  pointer, dimension(:       ) :: idtime
      integer       (sp       ) ,  pointer, dimension(:       ) :: ifopt
      integer       (sp       ) ,  pointer, dimension(:       ) :: iftime
      integer       (sp       ) ,  pointer, dimension(:       ) :: ihplot
      integer       (sp       ) ,  pointer, dimension(:       ) :: ioptrad
      integer       (sp       ) ,  pointer, dimension(:       ) :: ipnt
      integer       (sp       ) ,  pointer, dimension(:       ) :: ipset
      integer       (sp       ) ,  pointer, dimension(:       ) :: iptime
      integer       (sp       ) ,  pointer, dimension(:       ) :: isfile
      integer       (sp       ) ,  pointer, dimension(:       ) :: isfud
      integer       (sp       ) ,  pointer, dimension(:       ) :: isub
      integer       (sp       ) ,  pointer, dimension(:       ) :: iutime
      integer       (sp       ) ,  pointer, dimension(:       ) :: ivtime
      integer       (sp       ) ,  pointer, dimension(:       ) :: iwndtm
      integer       (sp       ) ,  pointer, dimension(:       ) :: iwtime
      integer       (sp       ) ,  pointer, dimension(:       ) :: kpart
      integer       (sp       ) ,  pointer, dimension(:       ) :: kwaste
      integer       (sp       ) ,  pointer, dimension(:       ) :: linear
      integer       (sp       ) ,  pointer, dimension(:       ) :: mapsub
      integer       (sp       ) ,  pointer, dimension(:       ) :: mpart
      integer       (sp       ) ,  pointer, dimension(:       ) :: mpart0
      integer       (sp       ) ,  pointer, dimension(:       ) :: mplsta
      integer       (sp       ) ,  pointer, dimension(:       ) :: mstat
      integer       (sp       ) ,  pointer, dimension(:       ) :: mstick
      integer       (sp       ) ,  pointer, dimension(:       ) :: mwaste
      integer       (sp       ) ,  pointer, dimension(:       ) :: elt_bytes
      integer       (sp       ) ,  pointer, dimension(:       ) :: ncheck
      integer       (sp       ) ,  pointer, dimension(:       ) :: ndprt
      integer       (sp       ) ,  pointer, dimension(:       ) :: nosud
      integer       (sp       ) ,  pointer, dimension(:       ) :: nosys
      integer       (sp       ) ,  pointer, dimension(:       ) :: npart
      integer       (sp       ) ,  pointer, dimension(:       ) :: npart0
      integer       (sp       ) ,  pointer, dimension(:       ) :: nplay
      integer       (sp       ) ,  pointer, dimension(:       ) :: nplot
      integer       (sp       ) ,  pointer, dimension(:       ) :: nplsta
      integer       (sp       ) ,  pointer, dimension(:       ) :: nstat
      integer       (sp       ) ,  pointer, dimension(:       ) :: nwaste
      integer       (sp       ) ,  pointer, dimension(:       ) :: stoil
      integer       (sp       ) ,  pointer, dimension(:       ) :: cellpnt
      integer       (sp       ) ,  pointer, dimension(:       ) :: flowpnt

!     2D integer arrays

      integer       (sp       ) ,  pointer, dimension(:,:     ) :: elt_dims
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: ibuff
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: ictime
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: imap
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: imask
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: lgrid
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: lgrid2
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: lgrid3
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: mcell
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: ncell

!     3D integer array

      integer       (sp       ) ,  pointer, dimension(:,:,:   ) :: nbin

!     Global real arrays

!     1D real arrays

      real          (sp       ) ,  pointer, dimension(:       ) :: abuoy
      real          (sp       ) ,  pointer, dimension(:       ) :: acf
      real          (sp       ) ,  pointer, dimension(:       ) :: angle
      real          (sp       ) ,  pointer, dimension(:       ) :: area
      real          (sp       ) ,  pointer, dimension(:       ) :: const
      real          (sp       ) ,  pointer, dimension(:       ) :: decays
      real          (sp       ) ,  pointer, dimension(:       ) :: depth
      real          (sp       ) ,  pointer, dimension(:       ) :: dfact
      real          (sp       ) ,  pointer, dimension(:       ) :: dps
      real          (sp       ) ,  pointer, dimension(:       ) :: drand
      real          (sp       ) ,  pointer, dimension(:       ) :: dx
      real          (sp       ) ,  pointer, dimension(:       ) :: dy
      real          (sp       ) ,  pointer, dimension(:       ) :: flow
      real          (sp       ) ,  pointer, dimension(:       ) :: flow1
      real          (sp       ) ,  pointer, dimension(:       ) :: fstick
      real          (sp       ) ,  pointer, dimension(:       ) :: radius
      real          (sp       ) ,  pointer, dimension(:       ) :: rbuffr
      real          (sp       ) ,  pointer, dimension(:       ) :: recovr
      real          (sp       ) ,  pointer, dimension(:       ) :: rem
      real          (sp       ) ,  pointer, dimension(:       ) :: t0buoy
      real          (sp       ) ,  pointer, dimension(:       ) :: t0cf
      real          (sp       ) ,  pointer, dimension(:       ) :: tcktot
      real          (sp       ) ,  pointer, dimension(:       ) :: tmass
      real          (sp       ) ,  pointer, dimension(:       ) :: tmassu
      real          (sp       ) ,  pointer, dimension(:       ) :: uscal
      real          (sp       ) ,  pointer, dimension(:       ) :: vdiff
      real          (sp       ) ,  pointer, dimension(:       ) :: velo
      real          (sp       ) ,  pointer, dimension(:       ) :: vol1
      real          (sp       ) ,  pointer, dimension(:       ) :: vol2
      real          (sp       ) ,  pointer, dimension(:       ) :: volfracw
      real          (sp       ) ,  pointer, dimension(:       ) :: volume
      real          (sp       ) ,  pointer, dimension(:       ) :: wdira
      real          (sp       ) ,  pointer, dimension(:       ) :: window
      real          (dp       ) ,  pointer, dimension(:       ) :: wevap      ! double precision
      real          (sp       ) ,  pointer, dimension(:       ) :: wparm
      real          (sp       ) ,  pointer, dimension(:       ) :: wsettl
      real          (sp       ) ,  pointer, dimension(:       ) :: wveloa
      real          (sp       ) ,  pointer, dimension(:       ) :: xa
      real          (sp       ) ,  pointer, dimension(:       ) :: xa0
      real          (sp       ) ,  pointer, dimension(:       ) :: xb
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart0
      real          (sp       ) ,  pointer, dimension(:       ) :: xpol
      real          (sp       ) ,  pointer, dimension(:       ) :: xstat
      real          (sp       ) ,  pointer, dimension(:       ) :: xwaste
      real          (sp       ) ,  pointer, dimension(:       ) :: ya
      real          (sp       ) ,  pointer, dimension(:       ) :: ya0
      real          (sp       ) ,  pointer, dimension(:       ) :: yb
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart0
      real          (sp       ) ,  pointer, dimension(:       ) :: ypol
      real          (sp       ) ,  pointer, dimension(:       ) :: ystat
      real          (sp       ) ,  pointer, dimension(:       ) :: ywaste
      real          (sp       ) ,  pointer, dimension(:       ) :: za
      real          (sp       ) ,  pointer, dimension(:       ) :: zlevel
      real          (sp       ) ,  pointer, dimension(:       ) :: zpart
      real          (sp       ) ,  pointer, dimension(:       ) :: zwaste

!     2D real arrays

      real          (sp       ) ,  pointer, dimension(:,:     ) :: aconc
      real          (sp       ) ,  pointer, dimension(:,:     ) :: aconud
      real          (sp       ) ,  pointer, dimension(:,:     ) :: adepth
      real          (sp       ) ,  pointer, dimension(:,:     ) :: amassd
      real          (sp       ) ,  pointer, dimension(:,:     ) :: apeak
      real          (sp       ) ,  pointer, dimension(:,:     ) :: atotal
      real          (sp       ) ,  pointer, dimension(:,:     ) :: c2
      real          (sp       ) ,  pointer, dimension(:,:     ) :: cdelv
      real          (sp       ) ,  pointer, dimension(:,:     ) :: conc
      real          (sp       ) ,  pointer, dimension(:,:     ) :: constev
      real          (sp       ) ,  pointer, dimension(:,:     ) :: decay
      real          (sp       ) ,  pointer, dimension(:,:     ) :: flres
      real          (sp       ) ,  pointer, dimension(:,:     ) :: fractd
      real          (sp       ) ,  pointer, dimension(:,:     ) :: fracte
      real          (sp       ) ,  pointer, dimension(:,:     ) :: ftime
      real          (sp       ) ,  pointer, dimension(:,:     ) :: fwatoil
      real          (sp       ) ,  pointer, dimension(:,:     ) :: locdep
      real          (sp       ) ,  pointer, dimension(:,:     ) :: qentr
      real          (sp       ) ,  pointer, dimension(:,:     ) :: rbuff
      real          (sp       ) ,  pointer, dimension(:,:     ) :: rhooilv
      real          (sp       ) ,  pointer, dimension(:,:     ) :: stoch
      real          (sp       ) ,  pointer, dimension(:,:     ) :: tmassc
      real          (sp       ) ,  pointer, dimension(:,:     ) :: tmasud
      real          (sp       ) ,  pointer, dimension(:,:     ) :: totfe
      real          (sp       ) ,  pointer, dimension(:,:     ) :: track
      real          (sp       ) ,  pointer, dimension(:,:     ) :: viso
      real          (sp       ) ,  pointer, dimension(:,:     ) :: visowat
      real          (sp       ) ,  pointer, dimension(:,:     ) :: vrtdsp
      real          (sp       ) ,  pointer, dimension(:,:     ) :: vsfact
      real          (sp       ) ,  pointer, dimension(:,:     ) :: wpart
      real          (sp       ) ,  pointer, dimension(:,:     ) :: xyztrk

!     3D real arrays

      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: amassc
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: atrack
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: chismp
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: chispl
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: vsfour

!     4D real arrays

      real          (sp       ) ,  pointer, dimension(:,:,:,: ) :: amap
      real          (sp       ) ,  pointer, dimension(:,:,:,: ) :: amapsett

end module global_pointers
