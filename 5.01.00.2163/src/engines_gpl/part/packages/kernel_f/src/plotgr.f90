      subroutine plotgrp( npgrid , pg     , nmax   , mmax   , lgrid  ,               &
                          lgrid2 , xb     , yb     )

!     Deltares Software Centre

!>\file
!>             Makes a pointer from the plotgrid cells to the gridmap to easy access depth
!>
!>             The local depth variable is also included in the plot grid results file.\n
!>             To avoid making look-ups every time step, a back-pointer is made once here.\n
!>             The backpointer is stored in pg(i)%nmcell(:,:).

!     Created           : July   2003 by antoon koster

!     Modified          : August 2011 by Leo Postma : allow for multiple plot grids
!     Modified          : January 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     Subroutines called: findcell to get a grid-cell from an x,y value

!     Functions called  : none

!     Logical units     : * standard output

      use precision        ! single/double precision
      use typos           ! the derived types

      implicit none

!     Arguments

!     kind            function         name                description

      integer  ( ip), intent(in   ) :: npgrid            !< number of plot grids
      type(PlotGrid)                   pg    (npgrid)    !< collection with plot grid information
      integer  ( ip), intent(in   ) :: nmax              !< 1st dimension of the flow grid
      integer  ( ip), intent(in   ) :: mmax              !< 2nd dimension of the flow grid
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax) !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax) !< total grid matrix
      real     ( rp), intent(in   ) :: xb    (nmax*mmax) !< x-values of the grid cell corners
      real     ( rp), intent(in   ) :: yb    (nmax*mmax) !< y-values of the grid cell corners

      return
      end subroutine
