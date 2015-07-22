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

      SUBROUTINE DLTEST(
c trisula parameters:
     *                  lundia    ,timest    ,
     *                  icx       ,icy       ,jstart    ,nmmaxj    ,
     *                  nmmax     ,kmax      ,
     *                  qxk       ,qyk       ,qzk       ,
     *                  kcs       ,kfs       ,kfu       ,kfv       ,
     *                  gvu       ,guv       ,gzz       ,
     *                  r1        ,
     *                  aak       ,bbk       ,cck       ,
     *                  bdddx     ,bddx      ,bdx       ,
     *                  buuux     ,buux      ,bux       ,
     *                  uvdwk     ,vvdwk     ,
     *                  aakl      ,bbkl      ,cckl      ,ddkl      ,
c delwaq parameters:
     *                  nosys     ,notot     ,vol0      ,vol1      ,
     *                  intsrt    ,
     *                  difx      ,dify      ,difz      )
C
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : August 1996 by E. de Goede
C
C     FUNCTION            : TRISULA transport scheme implemented in DELWAQ
C                           A choice between central and upwind differences
C                           in the vertical has been implemented.
C                           (INTSRT = 19 OR 20)
C                           (this option is not available in TRISULA)
C
C     PARAMETERS          :
C
C     NAME    KIND      FUNCT.  DESCRIPTION
C     ----    -----     ------  -----------
C     DIFX    REAL      INPUT   flux in x-direction due to diffusive transport
C     DIFY    REAL      INPUT   flux in y-direction due to diffusive transport
C     DIFZ    REAL      INPUT   flux in z-direction due to diffusive transport
C     GUV     REAL      INPUT   Grid distance in the eta-/y-direction
C                               at v-velocity point
C     GVU     REAL      INPUT   Grid distance in the ksi-/x-direction
C                               at u-velocity point
C     GZZ     REAL      INPUT   Grid distance in the z-direction
C                               at w-velocity point
C       remark: GZZ is not an original TRISULA array
C     ICX     INTEGER   INPUT   Increment in the X-dir., if ICX= NMAX
C                               then computation proceeds in the X-
C                               dir. If icx=1 then computation pro-
C                               ceeds in the Y-dir.
C     ICY     INTEGER   INPUT   Increment in the Y-dir. (see ICX)
C     INTSRT  INTEGER   INPUT   integration option number
C     JSTART  INTEGER   INPUT   start pointer   (jstart=1-2*nmax)
C       remark: in TRISULA j instead of jstart
C     KCS     INTEGER   INPUT   Mask array for the zeta points
C                                      (time independent)
C                                      =0 inactive point
C                                      =1 active   point
C                                      =2 open boundary point
C     KFS     INTEGER   INPUT   Mask array for the zeta points
C                                  (time dependent)
C                                  =0 dry      point
C                                  =1 active   point
C     KFU     INTEGER   INPUT   Mask array for the u-velocity point
C                                      (time dependent)
C                                      =0 dry      point
C                                      =1 active   point
C     KFV     INTEGER   INPUT   Mask array for the v-velocity point
C                                      (time dependent)
C                                      =0 dry      point
C                                      =1 active   point
C     KMAX    INTEGER   INPUT   nr of layers in third dimension (=noq3)
C     LUNDIA  INTEGER   INPUT   integer number for monitoring file
C     NMMAX   INTEGER   INPUT   nmax * mmax (= noq2 * noq1)
C     NMMAXJ  INTEGER   INPUT   end pointer   (nmmaxj=(mmax+2)*nmax)
C     NOSYS   INTEGER   INPUT   number of active substances
C     NOTOT   INTEGER   INPUT   number of total substances
C     QXK     REAL      INPUT   flux in x-direction due to advective transport
C     QYK     REAL      INPUT   flux in y-direction due to advective transport
C     QZK     REAl      INPUT   flux in z-direction due to advective transport
C     R1      REAL      IN/OUT  concentration array
C     TIMEST  REAL      INPUT   time step
C     VOL0    REAL      INPUT   volumes at old time level
C     VOL1    REAL      INPUT   volumes at new time level
C work arrays:
C     AAK     real      INPUT   work array for (n,m,k+1)
C     AAKL    real      INPUT   work array for (n,m,k+1,l)
C     BBK     real      INPUT   work array for (n,m,k)
C     BBKL    real      INPUT   work array for (n,m,k,l)
C     CCK     real      INPUT   work array for (n,m,k-1)
C     CCKL    real      INPUT   work array for (n,m,k-1,l)
C     on input AAKLK/BBKL/CCKL contain additional dispersions + velocities
C     for the vertical direction
C     DDKL    real      INPUT   work array for (n,m,k,l)
C     on input DDKL contains processes and waste loads and
C     additional dispersions and velocities in horizontal direction
C     BDDDX   real      INPUT   work array for (n,m-3,k)
C     BDDX    real      INPUT   work array for (n,m-2,k)
C     BDX     real      INPUT   work array for (n,m-1,k)
C     BUX     real      INPUT   work array for (n,m+1,k)
C     BUUX    real      INPUT   work array for (n,m+2,k)
C     BUUUX   real      INPUT   work array for (n,m+3,k)
C     UVDWK   real      INPUT   work array
C     VVDWK   real      INPUT   work array
C
      INTEGER   kfu    (jstart:nmmaxj),
     *          kfv    (jstart:nmmaxj),
     *          kfs    (jstart:nmmaxj),
     *          kcs    (jstart:nmmaxj)
c
      DIMENSION  qxk    (jstart:nmmaxj, kmax),
     *           qyk    (jstart:nmmaxj, kmax),
     *           qzk    (jstart:nmmaxj, 0:kmax),
     *           difx   (jstart:nmmaxj, kmax),
     *           dify   (jstart:nmmaxj, kmax),
     *           difz   (jstart:nmmaxj, 0:kmax),
     *           r1     (jstart:nmmaxj, kmax, notot),
     *           vol0   (jstart:nmmaxj, kmax),
     *           vol1   (jstart:nmmaxj, kmax),
     *           guv    (jstart:nmmaxj),
     *           gvu    (jstart:nmmaxj),
     *           gzz    (jstart:nmmaxj, 0:kmax)
c
c work arrays:
      DIMENSION  bdddx  (jstart:nmmaxj, kmax),
     *           bddx   (jstart:nmmaxj, kmax),
     *           bdx    (jstart:nmmaxj, kmax),
     *           bux    (jstart:nmmaxj, kmax),
     *           buux   (jstart:nmmaxj, kmax),
     *           buuux  (jstart:nmmaxj, kmax),
     *           uvdwk  (jstart:nmmaxj, kmax),
     *           vvdwk  (jstart:nmmaxj, kmax),
     *           aak    (jstart:nmmaxj, kmax),
     *           bbk    (jstart:nmmaxj, kmax),
     *           cck    (jstart:nmmaxj, kmax),
     *           aakl   (jstart:nmmaxj, kmax, nosys),
     *           bbkl   (jstart:nmmaxj, kmax, nosys),
     *           cckl   (jstart:nmmaxj, kmax, nosys),
     *           ddkl   (jstart:nmmaxj, kmax, nosys)
      write(lundia, * )       timest,icx,icy,jstart,nmmaxj,kmax
      write(lundia, * )       notot,nosys,intsrt,nmmax
      write(lundia,'(A)') 'kfu'
      write(lundia,'(10i10)') (kfu(i),i=jstart,nmmaxj)
      write(lundia,'(A)') 'kfv'
      write(lundia,'(10i10)') (kfv(i),i=jstart,nmmaxj)
      write(lundia,'(A)') 'kfs'
      write(lundia,'(10i10)') (kfs(i),i=jstart,nmmaxj)
      write(lundia,'(A)') 'kcs'
      write(lundia,'(10i10)') (kcs(i),i=jstart,nmmaxj)
      write(lundia,'(A)') 'qxk'
      write(lundia,'(10e12.6)') ((qxk(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'qyk'
      write(lundia,'(10e12.6)') ((qyk(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'qzk'
      write(lundia,'(10e12.6)') ((qzk(i,k),i=jstart,nmmaxj),k=0,kmax)
      write(lundia,'(A)') 'difx'
      write(lundia,'(10e12.6)') ((difx(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'dify'
      write(lundia,'(10e12.6)') ((dify(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'difz'
      write(lundia,'(10e12.6)') ((difz(i,k),i=jstart,nmmaxj),k=0,kmax)
      write(lundia,'(A)') 'r1'
      write(lundia,'(10e12.6)') (((r1(i,k,j),i=jstart,nmmaxj),k=1,kmax),
     +                             j=1,notot)
      write(lundia,'(A)') 'vol0'
      write(lundia,'(10e12.6)') ((vol0(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'vol1'
      write(lundia,'(10e12.6)') ((vol1(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'guv'
      write(lundia,'(10e12.6)') (guv(i),i=jstart,nmmaxj)
      write(lundia,'(A)') 'gvu'
      write(lundia,'(10e12.6)') (gvu(i),i=jstart,nmmaxj)
      write(lundia,'(A)') 'gzz'
      write(lundia,'(10e12.6)') ((gzz(i,k),i=jstart,nmmaxj),k=0,kmax)
      write(lundia,'(A)') 'bdddx'
      write(lundia,'(10e12.6)') ((bdddx(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'bddx'
      write(lundia,'(10e12.6)') ((bddx (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'bdx'
      write(lundia,'(10e12.6)') ((bdx  (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'bux'
      write(lundia,'(10e12.6)') ((bux  (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'buux'
      write(lundia,'(10e12.6)') ((buux (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'buuux'
      write(lundia,'(10e12.6)') ((buuux(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'uvdwk'
      write(lundia,'(10e12.6)') ((uvdwk(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'vvdwk'
      write(lundia,'(10e12.6)') ((vvdwk(i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'aak'
      write(lundia,'(10e12.6)') ((aak  (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'bbk'
      write(lundia,'(10e12.6)') ((bbk  (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'cck'
      write(lundia,'(10e12.6)') ((cck  (i,k),i=jstart,nmmaxj),k=1,kmax)
      write(lundia,'(A)') 'aakl'
      write(lundia,'(10e12.6)') (((aakl(i,k,j),i=jstart,nmmaxj),
     +                            k=1,kmax),j=1,nosys)
      write(lundia,'(A)') 'bbkl'
      write(lundia,'(10e12.6)') (((bbkl(i,k,j),i=jstart,nmmaxj),
     +                            k=1,kmax),j=1,nosys)
      write(lundia,'(A)') 'cckl'
      write(lundia,'(10e12.6)') (((cckl(i,k,j),i=jstart,nmmaxj),
     +                            k=1,kmax),j=1,nosys)
      write(lundia,'(A)') 'ddkl'
      write(lundia,'(10e12.6)') (((ddkl(i,k,j),i=jstart,nmmaxj),
     +                            k=1,kmax),j=1,nosys)
      stop
      return
      end
