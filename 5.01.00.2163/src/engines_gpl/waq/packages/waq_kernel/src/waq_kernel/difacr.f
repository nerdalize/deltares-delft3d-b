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

      subroutine difacr(icx       ,icy       ,j         ,nmmaxj    ,
     *                  nmmax     ,kmax      ,
     *                  lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  kadu      ,kadv      ,
     *                  s0        ,dps       ,r0        ,ddkl      ,
     *                  guu       ,gvv       ,guv       ,gvu       ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dsdksi    ,dtdksi    ,dsdeta    ,dtdeta    ,
     *                  dfluxx    ,dfluxy                          )
c-----------------------------------------------------------------------
c         D e l f t      H y d r a u l i c s   -   section    M C M
c
c             Module: SUBROUTINE DIFACR
c           Function: Computes horizontal diffusion along Z-planes.
c                     Explicit in u-direction, explicit in v-
c                     direction.
c                     Horizontal gradients salinity and temperature
c                     are computed in subroutine dengra.
c                     Only if KMAX > 1 and Anti Creep
c        Method used: Reference : On the approximation of horizontal
c                     gradients in sigma co-ordinates for bathymetry
c                     with steep bottom slopes (G.S. Stelling and J.
c                     van Kester - International Journal for Methods
c                     in Fluids, Vol. 18 1994)
c               Date: 19-07-2000
c         Programmer: G.S. Stelling, J. v. Kester, A.J. Mourits
c         CVS header
c            $Author: Beek_j $
c              $Date: 22-09-03 15:46 $
c            $Source: /u/trisula/cvsroot/trisula/reken/difacr.f,v $
c          $Revision: 1 $
c-----------------------------------------------------------------------
c   Calling routines:              DIFU
c                                  DIFUEX
c                                  DIFUIM
c                                  DIFUVL
c-----------------------------------------------------------------------
c   Called  routines:              DIFHOR
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c DDKL    --  R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Internal work array, diagonal space
c                                  at (n,m,k,l)
c DFLUXX  IO  R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Work array for horizontal diffusion flux
c DFLUXY  IO  R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Work array for horizontal diffusion flux
c DICUV   --  R*4  J:NMMAXJ,KMAX   Horizontal diffusion coeff. [m2/s]
c DPS     I   R*4  J:NMMAXJ        Depth value at zeta points
c DSDETA  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient salinity,
c                                  strictly horizontal in eta-direction
c                                  For Anti Creep: Contribution diffu-
c                                  sive flux interpolated on Cartesian
c                                  grid to cell NM
c DSDKSI  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient salinity,
c                                  strictly horizontal in ksi-direction
c                                  For Anti Creep: Contribution diffu-
c                                  sive flux interpolated on Cartesian
c                                  grid to cell NM
c DTDETA  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient temperature,
c                                  strictly horizontal in eta-direction
c                                  For Anti Creep: Contribution diffu-
c                                  sive flux interpolated on Cartesian
c                                  grid to cell NM
c DTDKSI  I   R*4  J:NMMAXJ,KMAX   Horizontal gradient temperature,
c                                  strictly horizontal in ksi-direction
c                                  For Anti Creep: Contribution diffu-
c                                  sive flux interpolated on Cartesian
c                                  grid to cell NM
c GUU     --  R*4  J:NMMAXJ        Grid distance in the eta-/y-direction
c                                  at u-velocity point
c GUV     --  R*4  J:NMMAXJ        Grid distance in the eta-/y-direction
c                                  at v-velocity point
c GVU     --  R*4  J:NMMAXJ        Grid distance in the ksi-/x-direction
c                                  at u-velocity point
c GVV     --  R*4  J:NMMAXJ        Grid distance in the ksi-/x-direction
c                                  at v-velocity point
c ICX     I   I*4                  Increment in the x-dir., if icx= nmax
c                                  then computation proceeds in the x-
c                                  dir. if icx=1 then computation pro-
c                                  ceeds in the y-dir.
c ICY     I   I*4                  Increment in the y-dir. (see icx)
c J       I   I*4                  Begin pointer for arrays which have
c                                  been transformed into 1d arrays.
c                                  due to the shift in the 2nd (m-)
c                                  index, j = -2*nmax + 1
c KADU    --  I*4  J:NMMAXJ,KMAX   Mask array for adv. term adjustment
c                                  for structures in U-points
c                                  = 1 no structure (HYD)
c                                  = 1 no gate (TRA)
c                                  = 0 structure
c                                  = 0 gate (KSPU(NM,0)*KSPU(NM,K)=4)
c KADV    --  I*4  J:NMMAXJ,KMAX   Mask array for adv. term adjustment
c                                  for structures in V-points
c                                  = 1 no structure (HYD)
c                                  = 1 no gate (TRA)
c                                  = 0 structure
c                                  = 0 gate (KSPV(NM,0)*KSPV(NM,K)=4)
c KCS     --  I*4  J:NMMAXJ        Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  J:NMMAXJ        Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  J:NMMAXJ        Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers in the z-dir.
c LSAL    --  I*4                  Pointer for salinity in array r
c                                  for constituents (if used, always 1)
c LSTSCI  I   I*4                  Number of Constituents (Salinity,
c                                  Temperature, Sediment, Conservative
c                                  Constituents and Secondary Flow)
c LTEM    --  I*4                  Pointer for temperature in array r
c                                  for constituents (lsal+1)
c NMMAX   --  I*4                  Total number of grid pts. (nmax*mmax)
c NMMAXJ  I   I*4                  End   pointer for arrays which have
c                                  been transformed into 1d arrays.
c                                  due to the shift in the 2nd (m-)
c                                  index, nmmaxj = nmmax + 2 * nmax
c R0      --  R*4  J:NMMAXJ,KMAX,LSTSCI
c                                  Concentrations at old time level
c S0      I   R*4  J:NMMAXJ        Zeta at old time level
c SIG     --  R*4  KMAX            Sigma coordinate density points.
c SIGDIF  --  R*4  LSTSCI          Prandtl/schmidt-numbers for const.
c THICK   --  R*4  KMAX            Relative layer thickness
c-----------------------------------------------------------------------
c    local variables:
c    ----------------
c
c   var.      type dimensions
c   -------------------------
c
c AREA        R*4
c CL          R*4
c CR          R*4
c DIFL        R*4
c DIFR        R*4
c FLUX        R*4
c K           I*4
c KENU        I*4
c KENV        I*4
c L           I*4
c NM          I*4
c NMU         I*4
c NUM         I*4
c-----------------------------------------------------------------------
c
c declaration
c
c-----------------------------------------------------------------------
c     GLOBAL DATA
c
c     global data structure definition and access functions
c     include 'globdat.igd'
c-----------------------------------------------------------------------
      use timers

      dimension   s0    (j:nmmaxj),dps   (j:nmmaxj),
     *            guu   (j:nmmaxj),gvv   (j:nmmaxj),
     *            guv   (j:nmmaxj),gvu   (j:nmmaxj)
c
      dimension   kcs   (j:nmmaxj),
     *            kfu   (j:nmmaxj),kfv   (j:nmmaxj),
     *            kadu  (j:nmmaxj,  kmax),
     *            kadv  (j:nmmaxj,  kmax)
c
      dimension   dicuv (j:nmmaxj,kmax),
     *            r0    (j:nmmaxj,kmax,lstsci),
     *            dsdksi(j:nmmaxj,kmax),dsdeta(j:nmmaxj,kmax  ),
     *            dtdksi(j:nmmaxj,kmax),dtdeta(j:nmmaxj,kmax  ),
     *            ddkl  (j:nmmaxj,kmax,lstsci),
     *            thick (kmax),sigdif(lstsci) ,
     *            sig   (kmax)

      dimension   dfluxx(j:nmmaxj,kmax,lstsci),
     *            dfluxy(j:nmmaxj,kmax,lstsci)

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "difacr", ithandl )

      dfluxx = 0.0
      dfluxy = 0.0

c
c***horizontal diffusion in both u- and v-diffusion
c   artificial creep is avoided by use
c   of a special limiter
c
c***calibration for SAL / TEMP in routine DENGRA
c
c$dir scalar
      if (lsal   .ne. 0) then
        do 10 nm=1,nmmax
          do 20 k=1,kmax
            ddkl(nm ,k,lsal)=ddkl(nm ,k,lsal)+dsdksi(nm,k)+
     *                                        dsdeta(nm,k)
 20       continue
 10     continue
      endif
      if (ltem   .ne. 0) then
        do 50 nm=1,nmmax
          do 60 k=1,kmax
            ddkl(nm ,k,ltem)=ddkl(nm ,k,ltem)+dtdksi(nm,k)+
     *                                        dtdeta(nm,k)
 60       continue
 50     continue
      endif
c
      if (lstsci.eq.MAX (lsal,ltem)) goto 1000
c
c***calibration for all other constituents
c   horizontal diffusion in u-diffusion
c   artificial creep is avoided by use
c   of a special limiter
c
      nmu = icx
      do 150 nm=1,nmmax
        nmu =nmu+1
        if (kfu(nm).ne.0) then
          sepnm =s0 (nm)
          sepnmu=s0 (nmu)
          dpnm  =dps(nm)
          dpnmu =dps(nmu)
          call   DIFHOR(nm        ,nmu       ,j         ,nmmaxj    ,
     *                  kmax      ,lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kadu      ,
     *                  sepnm     ,sepnmu    ,dpnm      ,dpnmu     ,
     *                  guu       ,gvu       ,
     *                  r0        ,ddkl      ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dfluxx                                     )

        endif
 150  continue
c
c***calibration for all other constituents
c   horizontal diffusion in v-direction
c   artificial creep is avoided by use
c   of a special limiter
c
      num = icy
      do 250 nm=1,nmmax
        num  =num+1
        if (kfv(nm).ne.0) then
          sepnm =s0 (nm)
          sepnum=s0 (num)
          dpnm  =dps(nm)
          dpnum =dps(num)
          call   DIFHOR(nm        ,num       ,j         ,nmmaxj    ,
     *                  kmax      ,lstsci    ,lsal      ,ltem      ,
     *                  kcs       ,kadv      ,
     *                  sepnm     ,sepnum    ,dpnm      ,dpnum     ,
     *                  gvv       ,guv       ,
     *                  r0        ,ddkl      ,
     *                  thick     ,sig       ,dicuv     ,sigdif    ,
     *                  dfluxy                                     )

        endif
 250  continue
c
 1000 continue
      if ( timon ) call timstop ( ithandl )
      return
      end
