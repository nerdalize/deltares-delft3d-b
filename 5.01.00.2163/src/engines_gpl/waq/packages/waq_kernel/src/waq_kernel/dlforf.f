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

      SUBROUTINE DLFORF(
c trisula parameters:
     *                  lundia    ,
     *                  icx       ,icy       ,jstart    ,nmmaxj    ,
     *                  nmmax     ,kmax      ,
     *                  kcs       ,kfs       ,kfu       ,kfv       ,
     *                  r1        ,rwork     ,value     ,
c delwaq parameters:
     *                  nosys     ,notot     ,volum1               )
C
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : August 1996 by E. de Goede
C
C     FUNCTION            : TRISULA transport discretization + solver
C                           implemented in DELWAQ code
C
C     PARAMETERS          :
C
C     NAME    KIND      FUNCT.  DESCRIPTION
C     ----    -----     ------  -----------
C     ICX     INTEGER   INPUT   Increment in the X-dir., if ICX= NMAX
C                               then computation proceeds in the X-
C                               dir. If icx=1 then computation pro-
C                               ceeds in the Y-dir.
C     ICY     INTEGER   INPUT   Increment in the Y-dir. (see ICX)
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
C     KMAX    INTEGER   INPUT   nr of layers in third  dimension (=noq3)
C     LUNDIA  INTEGER   INPUT   integer number for monitoring file
C     NMMAX   INTEGER   INPUT   nmax * mmax (= noq2 * noq1)
C     NMMAXJ  INTEGER   INPUT   end pointer   (nmmaxj=(mmax+2)*nmax)
C     NOSYS   INTEGER   INPUT   number of active substances
C     NOTOT   INTEGER   INPUT   number of total substances
C     R1      REAL      IN/OUT  concentration array
C     RMNEG   INTEGER   INPUT   Criterion for conc. above which filter
C                               procedure is applied
C     RWORK   REAL      INPUT   real work array
C     VALUE   REAL      INPUT   real work array
C     VOLUM1  REAL      INPUT   volumes at new time level
C
C

      use timers

      INTEGER   kfu    (jstart:nmmaxj),
     *          kfv    (jstart:nmmaxj),
     *          kfs    (jstart:nmmaxj),
     *          kcs    (jstart:nmmaxj)
c
      DIMENSION r1     (jstart:nmmaxj,kmax, notot),
     *          rwork  (jstart:nmmaxj,kmax),
     *          volum1 (jstart:nmmaxj,kmax),
     *          value  (jstart:nmmaxj),
     *          rmneg  (100)

      save            ifirst, rmneg

      data            maxfil /   10/
      data            ifirst / 1 /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlforf", ithandl )


      if (ifirst .eq. 1) then
         ifirst = 0

         if (nosys .gt. 100) then
            write (lundia,*)  ' INCREASE DIMENSION OF ARRAY RMNEG'
            write (lundia,*)  ' ASK FOR A NEW VERSION'
            stop
         endif
c
c-----------------------------------------------------------------------
c-----define value for 'smoothing', for turbulence model this must be 0.
c     for all other constituents are -1.e-2 this small enough
c-----------------------------------------------------------------------
         do 10 l=1,nosys
           rmneg(l) = -1.0e-2
  10     continue
c
c ******************************************
c ******************************************
c ad hoc oplossing:
c        open (unit=77, file='filter.bnd')
c        read (77,*) nofil
c        do 20 l=1,nofil
c          read (77,*) i,rmneg(i)
c 20     continue
c        close (unit=77)
c ******************************************
c ******************************************
      endif
c
c-----------------------------------------------------------------------
c-----loop over the constituents
c-----------------------------------------------------------------------
c$DIR SCALAR
      do 3000 l=1,nosys
c-----------------------------------------------------------------------
c-------iteration loop over computational grid
c-----------------------------------------------------------------------
c$DIR SCALAR
        do 2000 k=1,kmax
c-----------------------------------------------------------------------
c---------Forester filter initialisation
c-----------------------------------------------------------------------
          do 900 nm=1,nmmax
            value(nm)=0.0
  900     continue
c-----------------------------------------------------------------------
c---------Forester filter number of iterations
c-----------------------------------------------------------------------
c$DIR SCALAR
          do 1200 itfil=1,maxfil
            ifil=0
c
            do 1000 nm=1,nmmax
              rwork(nm,k)=r1(nm,k,l)
              if (rwork(nm ,k).lt.rmneg(l).and.
     *            kcs(nm)*kfs(nm).eq.1) then
                value(nm )=1.0
                ifil      =1
              endif
 1000       continue
c
            if (ifil.eq.0) goto 1299
            nmd=-icx
            ndm=-icy
            nmu= icx
            num= icy
            do 1150 nm=1,nmmax
              nmd=nmd+1
              ndm=ndm+1
              nmu=nmu+1
              num=num+1
              if (kcs(nm)*kfs(nm).eq.1) then
                volnmu = MIN (1.0,volum1(nmu,k)/volum1(nm ,k))
                cofnmu = 0.125*(value(nmu)+value(nm))*kfu(nm)  *
     *                             volnmu
                volnmd = MIN (1.0,volum1(nmd,k)/volum1(nm ,k))
                cofnmd = 0.125*(value(nmd)+value(nm))*kfu(nmd) *
     *                             volnmd
                volnum = MIN (1.0,volum1(num,k)/volum1(nm ,k))
                cofnum = 0.125*(value(num)+value(nm))*kfv(num) *
     *                             volnum
                volndm = MIN (1.0,volum1(ndm,k)/volum1(nm ,k))
                cofndm = 0.125*(value(ndm)+value(nm))*kfv(ndm) *
     *                             volndm
                r1(nm,k,l)=rwork(nm ,k) *
     *                     (1 - cofnmu - cofnmd - cofndm - cofnum) +
     *                     rwork(nmu,k) * cofnmu +
     *                     rwork(nmd,k) * cofnmd +
     *                     rwork(num,k) * cofnum +
     *                     rwork(ndm,k) * cofndm
              endif
 1150       continue
c-----------------------------------------------------------------------
c-----------test if number of iteration steps for filtering is exceeded
c-----------------------------------------------------------------------
            if (itfil.eq.maxfil) then
               write (lundia,*)
     *         'FORESTER FILTER: MAXIMUM NUMBER OF ITERATIONS EXCEEDED'
            endif
 1200     continue
c
 1299     continue
 2000   continue
c       if (itfil.gt.1)
c    *  write (lundia,*) 'FOR CONST. NO. ',l,
c    *                   'THE NUMBER OF FORESTER ITERATIONS=: ',itfil
c-----------------------------------------------------------------------
 3000 continue
6      if ( timon ) call timstop ( ithandl )
      return
      end

