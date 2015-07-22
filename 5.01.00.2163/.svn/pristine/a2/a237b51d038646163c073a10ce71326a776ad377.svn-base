
subroutine compute_localization_weights( istat, dist, nlb, nub, mlb, mub, nmax, mmax, nostat, mnstat, kcs, xz, yz, locval ) 

    use globaldata
    
    implicit none

    integer, intent(in) :: istat   ! current station number
    integer, intent(in) :: nostat  ! number of stations
    integer, intent(in) :: nlb, nub, nmax
    integer, intent(in) :: mlb, mub, mmax
    integer, dimension(nmax,mmax), intent(in) :: kcs
    integer, dimension(2,nostat) , intent(in) :: mnstat
    
    double precision, intent(in) :: dist
    real(fp) , dimension(nlb:nub, mlb:mub) , intent(in) :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(nlb:nub, mlb:mub) , intent(in) :: yz     !  Description and declaration in esm_alloc_real.f90

    double precision, dimension(nlb:nub, mlb:mub), intent(out) :: locval ! computed weights

    ! local variables
    integer :: m, n
    ! mn values of station
    integer :: ms, ns
    ! xy coordinates of station
    double precision :: sx, sy
    real :: weight
    real, external :: cohn
       
    ! intialize weight values
    locval = 0.0
      
    weight = 0.
    ms = mnstat(1,istat)
    ns = mnstat(2,istat)
    sx = xz(ns,ms)
    sy = yz(ns,ms)
    do n = 1, nmax
       do m = 1, mmax
          if (kcs(n,m) .gt. 0) then
             weight = cohn(sx,sy,xz(n,m),yz(n,m),dist)
          endif
          locval(n,m) = weight
       enddo
    enddo
   
end subroutine compute_localization_weights
!
!-------------------------------------------------------------------------------
!
function cohn(sm,sn,rm,rn,dist) result(weight)
    
    implicit none

    real :: weight ! localization weight according to Cohn's formula

    ! coordinates to compute localization weight for
    double precision :: sm, sn 
    
    ! treshold distance for Cohn's formula. all points at a distance
    ! more than 2*dist have weight 0
    double precision :: dist
    double precision :: rm, rn    

    ! local parameters
    real :: zc1 ! value of (z/c)
    real :: zc2 ! value of (z/c)^2
    real :: zc3 ! value of (z/c)^3
    real :: zc4 ! value of (z/c)^4
    real :: zc5 ! value of (z/c)^5
    real :: z   ! Eucledean distance between (m,n) and (rm, rn)
    real :: dm  ! distance between rm and sm
    real :: dn  ! distance between rn and sn
    
    real p1_4, p1_2, p2_3, p5_8, p5_3, p1_12
    parameter( p1_4  = 1.0/4.0  ) ! value of 1/4
    parameter( p1_2  = 1.0/2.0  ) ! value of 1/2
    parameter( p2_3  = 2.0/3.0  ) ! value of 2/3
    parameter( p5_8  = 5.0/8.0  ) ! value of 5/8
    parameter( p5_3  = 5.0/3.0  ) ! value of 5/3
    parameter( p1_12 = 1.0/12.0 ) ! value of 1/12

!     Compute distance per direction
!
      dn=abs(rn-sn)
      dm=abs(rm-sm)
!
!     Initialize weight
      weight=0.0
!
!     If point is likely to be within localization region
!
      if (dn<2.0*dist .and. dm<2.0*dist) then
!
!        compute euclidian distance
!
         z=sqrt(dn*dn+dm*dm)
!
!        compute weight according to Cohn's formula
!
         if (z .le. 2.0*dist) then
            zc1=z/dist
            zc2=zc1*zc1
            zc3=zc2*zc1
            zc4=zc3*zc1
            zc5=zc4*zc1
            if (z .le. dist) then
               weight=-p1_4 *zc5+p1_2*zc4+p5_8*zc3-p5_3*zc2+1.0
            else
               weight=p1_12*zc5-p1_2*zc4+p5_8*zc3+p5_3*zc2-5.0*zc1+4-p2_3*(dist/z)
            endif
         endif
      endif
         
      end function cohn

