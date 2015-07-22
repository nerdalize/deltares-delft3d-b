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

      subroutine difhor(nm        ,nmu       ,j         ,nmmaxj    ,
     *                  kmax      ,lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kadu      ,
     *                  sepnm     ,sepnmu    ,dpnm      ,dpnmu     ,
     *                  guu       ,gvu       ,
     *                  r0        ,ddkl      ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dflux                                      )

c-----------------------------------------------------------------------
c         D e l f t      H y d r a u l i c s   -   section    M C M
c
c             Module: SUBROUTINE DIFHOR
c           Function: computes horizontal diffusion along strict
c                     horizontal planes, using a special limiter,
c                     to avoid 'artificial vertical diffusion'.
c                     Recently the limiter was improved.The new
c                     limiter is following Van Leer's scheme for
c                     advection and preserves monotony.
c                     explicit in u-direction, explicit in v-
c                     direction. Fluxes computed in subroutine
c                     dengra are used.
c        Method used: Reference : On the approximation of horizontal
c                     gradients in sigma co-ordinates for bathymetry
c                     with steep bottom slopes (G.S. Stelling and J.
c                     van Kester - International Journal for Methods
c                     in Fluids, Vol. 18 1994) and the discussion
c                     in "A comparison of two 3D shallow
c                     water models using sigma-coordinates and
c                     z-coordinates in the vertical direction."
c                     (M.D. Bijvelds, J. van Kester and G.S. Stelling -
c                      Proceedings 6-th International Conference on
c                      estuarine and coastal modelling, New Orleans,
c                      september 1999)
c               Date: 29-11-2001
c         Programmer: G.S. Stelling, J.A.T.M. van Kester, M.D. Bijvelds
c         CVS header
c            $Author: Beek_j $
c              $Date: 22-09-03 15:46 $
c            $Source: /u/trisula/cvsroot/trisula/reken/difhor.f,v $
c          $Revision: 1 $
c-----------------------------------------------------------------------
c   Calling routines:                DIFACR
c-----------------------------------------------------------------------
c   Called  routines:                NONE
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c DDKL    IO  R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Internal work array, right hand side
c                                  at (n,m,k,l)
c DFLUX   IO  R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Work array for horizontal diffusion flux
c DICUV   I   R*4  J:NMMAXJ,KMAX   Horizontal diffusion coeff. [m2/s]
c GUU     I   R*4  J:NMMAXJ        Grid distance in the eta-/y-direction
c                                  at u-velocity point
c GVU     I   R*4  J:NMMAXJ        Grid distance in the ksi-/x-direction
c                                  at u-velocity point
c J       I   I*4                  Begin pointer for arrays which have
c                                  been transformed into 1d arrays.
c                                  due to the shift in the 2nd (m-)
c                                  index, j = -2*nmax + 1
c KADU    I   I*4  J:NMMAXJ,KMAX   Mask array for adv. term adjustment
c                                  for structures in U-points
c                                  = 1 no structure (HYD)
c                                  = 1 no gate (TRA)
c                                  = 0 structure
c                                  = 0 gate (KSPU(NM,0)*KSPU(NM,K)=4)
c KCS     I   I*4  J:NMMAXJ        Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KMAX    I   I*4                  Number of layers in the z-dir.
c LSAL    I   I*4                  Pointer for salinity in array r
c                                  for constituents (if used, always 1)
c LSTSCI  I   I*4                  Number of Constituents (Salinity,
c                                  Temperature, Sediment, Conservative
c                                  Constituents and Secondary Flow)
c LTEM    I   I*4                  Pointer for temperature in array r
c                                  for constituents (lsal+1)
c NMMAX   I   I*4                  Total number of grid pts. (nmax*mmax)
c NMMAXJ  I   I*4                  End   pointer for arrays which have
c                                  been transformed into 1d arrays.
c                                  due to the shift in the 2nd (m-)
c                                  index, nmmaxj = nmmax + 2 * nmax
c R0      I   R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Concentrations at old time level
c SIGDIF  I   R*4  LSTSCI          Prandtl/schmidt-numbers for const.
c THICK   I   R*4  KMAX            Relative layer thickness
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c AREA        R*4
c CL          R*4
c CR          R*4
c DIFL        R*4
c DIFR        R*4
c FLUX        R*4
c K           I*4
c KICOL       I*4  2*MXKMAX+1      K-index concentration point left
c                                  for flux kf
c KICOR       I*4  2*MXKMAX+1      K-index concentration point right
c                                  for flux kf
c L           I*4                  Constituent number
c NM          I*4                  Index gridpoint
c NMU         I*4                  Index neighbour
c NUM         I*4
c POCOL       R*4  0:MXKMAX        Z-coordinate concentr. point nm ,k
c POCOR       R*4  0:MXKMAX        Z-coordinate concentr. point nmu,k
c POFLU       R*4  2*MXKMAX+1       Z-coordinate gradient flux
c POINT       R*4  0:2*MXKMAX+1    Merge arrays polal and polar
c POLAL       R*4  0:MXKMAX        Z-coordinate horizontal layers in nm
c POLAR       R*4  0:MXKMAX        Z-coordinate horizontal layers in nmu
c-----------------------------------------------------------------------
c
c declaration
c
c-----------------------------------------------------------------------
c     GLOBAL DATA
c
c     global data structure definition and access functions
c      include 'globdat.igd'
c     global data structure definition and access functions
c      include    'pardef.igd'
c-----------------------------------------------------------------------
c
      use timers

      dimension   kcs   (j:nmmaxj),
     *            kadu  (j:nmmaxj,  kmax)
c
      dimension   guu   (j:nmmaxj),gvu   (j:nmmaxj),
     *            r0    (j:nmmaxj,kmax,lstsci),
     *            ddkl  (j:nmmaxj,kmax,lstsci),
     *            thick (  kmax  ),sig   (  kmax  ),
     *            dicuv (j:nmmaxj,kmax),sigdif(lstsci)

      dimension   dflux (j:nmmaxj,kmax,lstsci)
c
      real   , allocatable, save :: polal (:),polar (:),pocol (:),pocor (:),
     *                              poflu (:),point (:)
      integer, allocatable, save :: kicol (:),kicor (:)
      logical, save              :: first = .true.
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "difhor", ithandl )
c
      if ( first ) then
         first = .false.
         allocate ( polal (0:kmax  ),polar (0:  kmax  ),
     *              pocol (  kmax  ),pocor (    kmax  ),
     *              poflu (2*kmax+1),point (0:2*kmax+1)   )
         allocate ( kicol (2*kmax+1),kicor (  2*kmax+1)   )
      endif
c
c***initialisation
c
c***position horizontal interfaces left
c
      polal (0)=sepnm
      h0       =MAX (sepnm+dpnm,0.01)
c$dir scalar
      do 20 k=1,kmax
         polal (k)=(sig(k)-0.5*thick(k))*h0+sepnm
         pocol (k)=0.5*(polal(k-1)+polal(k))
 20   continue
c
c***position horizontal interfaces right
c
      polar (0)=sepnmu
      h0       =MAX (sepnmu+dpnmu,0.01)
c$dir scalar
      do 30 k=1,kmax
         polar (k)=(sig(k)-0.5*thick(k))*h0+sepnmu
         pocor (k)=0.5*(polar(k-1)+polar(k))
 30   continue
c
c***merge polal and polar
c
      kll=0
      krr=0
c$dir scalar
      do 40 k=0,2*kmax+1
         if (polal(kll).gt.polar(krr)) then
            point(k)=polal(kll)
            kll     =kll+1
            if (kll.gt.kmax) then
               kpoint=k+1
               point(kpoint)=polar(krr)
               goto 41
            endif
         else
            point(k)=polar(krr)
            krr   =krr +1
            if (krr.gt.kmax) then
               kpoint=k+1
               point(kpoint)=polal(kll)
               goto 41
            endif
         endif
  40  continue
      kpoint=2*kmax+1
  41  continue
c
c***position flux points
c
      kflux=kpoint
c$dir scalar
      do 51 k=1,kflux
         poflu(k)=0.5*(point(k)+point(k-1))
  51  continue
c
c***k-index concentration points left and right for flux point
c
      kll=1
      krr=1
c$dir scalar
      do 80 kf=1,kflux
         kicol(kf)=0
         kicor(kf)=0
c$dir scalar
         do 60 k=kll,kmax
            if (poflu(kf).le.polal(k-1).and.poflu(kf).ge.polal(k)) then
               kicol(kf)=k
               kll      =k
               goto 61
            endif
  60     continue
  61     continue
c$dir scalar
         do 70 k=krr,kmax
            if (poflu(kf).le.polar(k-1).and.poflu(kf).ge.polar(k)) then
               kicor(kf)=k
               krr      =k
               goto 71
            endif
  70     continue
  71     continue
  80  continue
c
c***computation diffusive flux using
c   limiter
c
c$dir scalar
      do 200 l= MAX (lsal,ltem)+1,lstsci
c
c***computation of flux
c
c$dir scalar
      do 100 kf=1,kflux
        kll =kicol(kf)
        krr =kicor(kf)
        if (kll.ne.0.and.krr.ne.0) then
          if (kadu(nm,kll)*kadu(nm,krr) .ne. 0) then
c
c***interpolation left point
c
            k2=kll
            if (pocor(krr).gt.pocol(kll)) then
              k1=k2-1
              if (k1.lt.1) then
                cl=r0(nm,k2,l)
              else
                cl=((pocor(krr)-pocol(k2))/(pocol(k1)-pocol(k2)))*
     *               r0(nm,k1,l)+
     *             ((pocor(krr)-pocol(k1))/(pocol(k2)-pocol(k1)))*
     *               r0(nm,k2,l)
              endif
            else
              k1=k2+1
              if (k1.gt.kmax) then
                cl=r0(nm,k2,l)
              else
                cl=((pocor(krr)-pocol(k2))/(pocol(k1)-pocol(k2)))*
     *               r0(nm,k1,l) +
     *             ((pocor(krr)-pocol(k1))/(pocol(k2)-pocol(k1)))*
     *               r0(nm,k2,l)
              endif
            endif
c
c***interpolation left point
c
            k2=krr
            if (pocol(kll).gt.pocor(k2)) then
              k1=k2-1
              if (k1.lt.1) then
                cr=r0(nmu,k2,l)
              else
                cr=((pocol(kll)-pocor(k2))/(pocor(k1)-pocor(k2)))*
     *               r0(nmu,k1,l)+
     *             ((pocol(kll)-pocor(k1))/(pocor(k2)-pocor(k1)))*
     *               r0(nmu,k2,l)
              endif
            else
              k1=k2+1
              if (k1.gt.kmax) then
                cr=r0(nmu,k2,l)
              else
                cr=((pocol(kll)-pocor(k2))/(pocor(k1)-pocor(k2)))*
     *               r0(nmu,k1,l)+
     *             ((pocol(kll)-pocor(k1))/(pocor(k2)-pocor(k1)))*
     *               r0(nmu,k2,l)
              endif
            endif
            grad1=r0(nmu,krr,l)-cl
            grad2=cr-r0(nm,kll,l)
            grmax=max(grad1,grad2)
            grmin=min(grad1,grad2)
c
c***new limiter following Van Leer
c
            if (grmax.ge.0.0.and.grmin.le.0.0) then
               grad=0.0
            else
               grad = 2.* grad1*grad2/(grad1+grad2)
            endif
c
c***flux
c
            difl=dicuv(nm ,kll)
            difr=dicuv(nmu,krr)
            flux=0.5*(point(kf-1)-point(kf))*grad*guu(nm)*
     *               (difl+difr)/sigdif(l)/gvu(nm)
            dflux(nm ,kll,l)=dflux(nm ,kll,l)+flux*MAX(0,2-kcs(nmu))
            dflux(nmu,krr,l)=dflux(nmu,krr,l)-flux*MAX(0,2-kcs(nm ))
            ddkl(nm ,kll,l)=ddkl(nm ,kll,l)+flux*MAX(0,2-kcs(nmu))
            ddkl(nmu,krr,l)=ddkl(nmu,krr,l)-flux*MAX(0,2-kcs(nm ))
          endif
        endif
  100 continue
  200 continue
c
      if ( timon ) call timstop ( ithandl )
      return
      end
