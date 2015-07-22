      subroutine ININFS(lundia,fout  ,extnef,mnstat,thick ,namcon,
     *                  grdang,tstart,dtmin ,notims,nostat,kmax  ,
     *                  lstci ,itdate                            )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: nefis.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/src/nefis.f $
!***********************************************************************
! Deltares                         marine and coastal management
!
!     Function: 1) Read general model information from the TRISULA
!                  (NEFIS) history file for hydrodynamics
!
!               Theo van der Kaaij
!               April 1993
!
!     Modified: Adjusted for nesthd2
!               Theo van der Kaaij
!               July 1993
!***********************************************************************
!
      integer       itdate

      integer       tyden (2), date(2)

      integer       mnstat(2     ,nostat)

      real          thick (kmax  )

      character*20  namcon(lstci + 2)
!
!-----For nefis
!
      integer*4     fdNef,
     *              uindex(     15), ! 3x5
     *              buflen,
     *              usrord(5)
      integer*4     error
      integer*4     getels, getelt, crenef, clsnef
!
      character*1   coding, access
      character*(*) extnef
      character*256 fildat, fildef
      character*16  elmnam, celnam, grpnam
!
      logical       fout
      external      getels, getelt, crenef, clsnef
!
!              ****************************************
!              *     read dimensions from NEFIS files *
!              ****************************************
!
!     +-----------------------------------------------------------------
!     | Initialize
!     +-----------------------------------------------------------------
      coding    = ' '
      access    = 'r'
      uindex   = 0
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 0
      usrord(1) = 1
!     +-----------------------------------------------------------------
!     | Ophalen dimensies datagroep 'his-info-series'
!     +-----------------------------------------------------------------
      fildef = 'trih-' // trim(extnef) // '.def'
!
      fildat = 'trih-' // trim(extnef) // '.dat'
!
      error  = CRENEF (fdNef, fildat, fildef, coding, access)
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: opening FLOW his files'
         fout   = .true.
         goto 999
      endif
!
      celnam = 'his-info-series'
      grpnam = 'his-info-series'

      elmnam     = 'ITHISC'
      buflen     = 4 * notims
      uindex (2) = 2
      uindex (3) = 1
      error      = GETELT (fdNef, grpnam, elmnam, uindex,
     *                     usrord, buflen, tyden                 )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (ITHISC)'
         fout   = .true.
         goto 999
      endif
!
      grpnam     = 'his-const'
!
      elmnam    = 'ITDATE'
      buflen    = 8
      uindex(2) = 1
      uindex(3) = 1
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,date                )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (ITDATE)'
         fout   = .true.
         goto 999
      endif

      itdate = date(1)

      elmnam     = 'TUNIT'
      buflen     = 4
      uindex (2) = 1
      error      = getelt (fdNef, grpnam, elmnam, uindex,
     *                     usrord, buflen, tunit                 )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (TUNIT)'
         fout   = .true.
         goto 999
      endif
!
      elmnam     = 'DT'
      buflen     = 4
      uindex (2) = 1
      error      = getelt (fdNef, grpnam, elmnam, uindex,
     *                     usrord, buflen, dt                    )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (DT)'
         fout   = .true.
         goto 999
      endif
!----------------------------------------------------------------------
!     time frame
!----------------------------------------------------------------------
      dtmin = (real(tyden(2)) - real(tyden(1))) * dt * tunit/60.
!
      tstart = real (tyden(1)) * dt * tunit
      tstart = tstart / 60.0
!
      if (nostat .gt. 0) then
         elmnam    = 'MNSTAT'
         buflen    = 2*nostat*4
         uindex(2) = 1
         error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                      usrord,buflen,mnstat              )
         if (error  .ne. 0) then
            write(lundia,'(a)')
     *           '***ERROR: reading DELFT3D-FLOW his data file (MNSTAT)'
            fout   = .true.
            goto 999
         endif
      endif
!
      elmnam    = 'THICK'
      buflen    = 4*kmax
      uindex(2) = 1
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,thick               )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (THICK)'
         fout   = .true.
         goto 999
      endif

      elmnam    = 'GRDANG'
      buflen    = 4
      uindex(2) = 1
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,grdang              )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (GRDANG)'
         fout   = .true.
         goto 999
      endif

      elmnam    = 'LTUR'
      error     = GETELT(fdNef,grpnam    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (LTUR)'
         fout   = .true.
         goto 999
      endif

      if ((lstci + ltur) .gt. 0) then

         buflen    = 20 * (lstci + ltur)
         elmnam    = 'NAMCON'
         error     = GETELS(fdNef,grpnam    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,NAMCON    )
         if (error  .ne. 0) then
            write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (NAMCON)'
            fout   = .true.
            goto 999
        endif

         do 10 l = 1, lstci + ltur
            call small (namcon( l),20        )
   10    continue

      endif
!
!-----close NEFIS files
!
  999 continue

      error     = CLSNEF(fdNef)
!
      return

      end
!
!==============================================================================
!
      subroutine SIMHSH(lundia, fout, extnef, kfs, wl, uu, vv,
     *                  alfas ,grdang, notims, nostat, kmax)
!
!-----------------------------------------------------------------------
!     Function: 1) Read time series (hydrodynamics) from the NEFIS
!                  history file
!
!               2) Converts u- and v-velocities to north and south
!                  velocities
!----------------------------------------------------------------------
      integer*4     notims
!
      real*4        zcurut,zcurvt,mag   ,dir   ,eps   ,
     *              pi    ,deg2rd
!
      real*4        ALFAS (nostat)
!
      real          wl  (nostat,notims),
     *              uu  (nostat,kmax  ,notims),
     *              vv  (nostat,kmax  ,notims)
      integer       kfs(nostat,notims)

!----------------------------------------------------------------------
!     Nefis declarations
!----------------------------------------------------------------------
      integer*4     fdNef,
     *              uindex(    15), ! 3x5
     *              buflen,
     *              usrord(5)
      integer*4     error
      integer*4     getelt, crenef, clsnef
!
      character*1   coding, access
      character*(*) extnef
      character*256 fildat,fildef
      character*16  elmnam,grpnam
      character*1024 error_string
!
      logical       fout
      external      getelt, crenef, clsnef
!----------------------------------------------------------------------
!     initialisation
!----------------------------------------------------------------------
      eps    = 1.0e-6
      pi     = 4.*atan(1.)
      deg2rd = pi/180.
      rd2deg = 180./pi
      write (*     ,'('' >>> Reading hydrodynamic data from history '',
     *                  ''file <<<'')')
      write (lundia,'('' >>> Reading hydrodynamic data from history '',
     *                  ''file <<<'')')
!----------------------------------------------------------------------
!     open NEFIS files
!----------------------------------------------------------------------
      coding    = 'N'
      access    = 'R'
      fildef    = 'trih-'//trim(extnef)//'.def'
!
      fildat    = 'trih-'//trim(extnef)//'.dat'
      error  = CRENEF (fdNef, fildat, fildef, coding, access)
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: opening FLOW his file(s)'
         fout   = .true.
         goto 999
      endif
!
      usrord    = 0
      usrord(1) = 1
!----------------------------------------------------------------------
!     get ALFAS
!----------------------------------------------------------------------
      grpnam    = 'his-const'
!
      uindex    = 0    
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      buflen    = 4 * nostat
      elmnam    = 'ALFAS'
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,alfas     )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (ALFAS)'
         fout   = .true.
         goto 999
      endif
!----------------------------------------------------------------------
!     cycle through hisfile
!----------------------------------------------------------------------
      grpnam    = 'his-series'
!
!----------- mask array for wl-point (active (1), inactive (0))
      elmnam    = 'ZKFS'
      buflen    = 4*nostat*notims
      kfs       = 0
      uindex    = 0    
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,kfs )
      if (error  .ne. 0) then
         error = neferr(fdnef, error_string)
         write(lundia,'(a)') trim(error_string)
         kfs = 1
      endif
!
!----------- water levels
      elmnam    = 'ZWL'
      buflen    = 4*nostat*notims
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,wl                  )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (ZWL)'
         fout   = .true.
         goto 999
      endif
!
!------- u and v-velocities to get north and south vel
!
      elmnam    = 'ZCURU'
      buflen    = 4*nostat*kmax*notims
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,uu                  )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (ZCURU)'
         fout   = .true.
         goto 999
      endif
!
      elmnam    = 'ZCURV'
      buflen    = 4*nostat*kmax*notims
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
      write(*,'(a,a)') 
     * '     Reading hydrodynamic data, element ', trim(elmnam)
      error     = GETELT(fdNef,grpnam,elmnam,uindex,
     *                   usrord,buflen,vv                  )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading DELFT3D-FLOW his data file (ZCURV)'
         fout   = .true.
         goto 999
      endif
!-------------------------------------------------------------------
!     Translate to south and north velocities
!-------------------------------------------------------------------
      do 10 itim = 1, notims
         do 10 istat = 1, nostat
            do 10 k = 1, kmax
               zcurut = uu   (istat ,k     ,itim  )*
     *                        cos (alfas (istat) * deg2rd) -
     *                  vv   (istat ,k     ,itim  )*
     *                        sin (alfas (istat) * deg2rd)
               zcurvt = uu   (istat ,k     ,itim  )*
     *                         sin (alfas (istat) * deg2rd) +
     *                  vv   (istat ,k     ,itim  )*
     *                         cos (alfas (istat) * deg2rd)
!
             mag    = sqrt(zcurut*zcurut + zcurvt*zcurvt)
             if (abs(zcurut) .lt. eps) zcurut = eps
             if (abs(zcurvt) .lt. eps) zcurvt = eps
             hulp   = 90. -atan2(zcurvt,zcurut)*rd2deg +
     *                grdang
             dir    = amod (hulp + 360.,360.)

             uu (istat,k,itim ) = mag*sin(dir*deg2rd)
             vv (istat,k,itim ) = mag*cos(dir*deg2rd)
   10 continue
!----------------------------------------------------------------------
!     close NEFIS files
!----------------------------------------------------------------------
  999 continue
!
      error     = CLSNEF(fdNef)
!
      return
      end

      subroutine SIMHSC(lundia,fout  ,extnef,
     *                  notims,nostat,kmax  ,lstci ,
     *                  conc                       )
!
!-----------------------------------------------------------------------
!     Function: 1) Read time series (constituents) from the NEFIS
!                  history file
!----------------------------------------------------------------------

      integer       notims

      real          conc   (nostat,kmax  ,lstci ,notims)
!
!----------------------------------------------------------------------
!     Nefis declarations
!----------------------------------------------------------------------
      integer*4     fdNef,
     *              uindex(    15), ! 3x5
     *              buflen,
     *              usrord(5)
      integer*4     error
      integer*4     getelt, crenef, clsnef
!
      character*1   coding, access
      character*(*) extnef
      character*256 fildat,fildef
      character*16  elmnam,grpnam
!
      logical       fout
      external      getelt, crenef, clsnef
!----------------------------------------------------------------------
!     open NEFIS files
!----------------------------------------------------------------------
      write (*     ,'(/,'' >>> Reading transport data from history '',
     *                  ''file <<<'')')
      write (lundia,'(/,'' >>> Reading transport data from history '',
     *                  ''file <<<'')')

      coding    = ' '
      access    = 'r'
      fildef    = 'trih-'//trim(extnef)//'.def'
      fildat    = 'trih-'//trim(extnef)//'.dat'
      error  = CRENEF (fdNef, fildat, fildef, coding, access)
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: opening Delft3D-FLOW his file(s)'
         fout   = .true.
         goto 999
      endif
!
      usrord    = 0
      usrord(1) = 1
!----------------------------------------------------------------------
!     get concentrations
!----------------------------------------------------------------------

      grpnam = 'his-series'
      usrord    = 0
      usrord(1) = 1
!
      buflen = 4 * nostat * kmax * lstci*notims
      elmnam = 'GRO'
      uindex    = 0
      uindex(1) = 1
      uindex(2) = notims
      uindex(3) = 1
!-----------------------------------------------------------------------
!--------Read from group 3 GRO
!-----------------------------------------------------------------------
      write(*,'(a,a)') 
     * '     Reading transport data, element ', trim(elmnam)
      error    = GETELT(fdNef,grpnam    ,elmnam    ,
     *                  uindex,usrord ,buflen    ,conc      )
      if (error  .ne. 0) then
         write(lundia,'(a)')
     *        '***ERROR: reading Delft3D-FLOW his data file (GRO)'
         fout   = .true.
         goto 999
      endif
!----------------------------------------------------------------------
!     close NEFIS files
!----------------------------------------------------------------------
  999 continue
!
      error     = CLSNEF(fdNef)
!
      return
      end
