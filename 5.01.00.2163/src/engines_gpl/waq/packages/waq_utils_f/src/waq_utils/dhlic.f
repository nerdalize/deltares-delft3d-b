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

      SUBROUTINE DHLIC ( lunrep, verwaq, verd3d, versbk, verrur,
     +                   verrib, nfeat , feat  , usernm, yfeat ,
     +                   l3dmod, nolic)
c
      USE Timers
      implicit none
c-----------------------------------------------------------------------
c
c     Deltares
c
c     Created             : April 19, 2000
c     Programmer          : Jan Mooiman
c     Function            : Checks license file
c
c-----------------------------------------------------------------------
c
c     Subroutines called  : AUTHMD, check license
c                           AUTHER, error message authorization modue
c                           AUTCFG, read consome contents of lic. file
c
c-----------------------------------------------------------------------
c
c     Arguments           :
c
c     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
c     ----    -----    ------     ------- -----------
c     lunrep  integer  1          input   Unit number report file
c     verwaq  ch*(*)   1          input   Version number DELWAQ
c     verd3d  ch*(*)   1          input   Version number Delft3D
c     versbk  ch*(*)   1          input   Version number SOBEK(river)
c     verrur  ch*(*)   1          input   Version number SOBEK(rural+urban)
c     verrib  ch*(*)   1          input   Version number RIBASIM
c     nfeat   integer  1          input   Number of defined features
c     feat    ch*(*)   1          input   Names of defined features
c     usernm  ch*(*)   1          output  User name
c     yfeat   integer  1          output  Licence of defined features
c     l3dmod  logical  1          input   check 3d features
c     nolic   logical  1          output  No valid license found
c                                         (for SOBEK with limited number
c                                         of segments)
c
c-----------------------------------------------------------------------
c
c     Declaration of arguments
c
      integer       lunrep, nfeat
      character*(*) verwaq, verd3d, versbk, verrur, verrib
      character*(*) usernm
      integer       yfeat(nfeat)
      character*(*) feat (nfeat)
      logical       l3dmod, nolic
c
c     Declaration of externals
c
      integer       authmd, auther, autcfg
      external      authmd, auther, autcfg
c
c     Declaration of local variables
c
      integer       i, iusnm, jusnm, ierror,
     +              idummy, ierr   , ilun  ,
     +              lu, i_delft3d, i_sobek,
     +              i_ribasim, ifeat,
     +              length, iend
c
      real          rdummy
c
      logical       permit, sbklic, d3dlic, riblic,
     +              lfound, lexist, lopen , permit_3d
c
      character*256 vendor
      character*256 errmsg
      character*30  featur
      character*30  featur_3d
      character*256 licfil
      character*256 modelsuite
      character*256 conf
      character*256 inifil
      character*25  group
      character*25  keywrd
      character*1   sbkriv
      integer(4) ithndl /0/
      if ( timon ) call timstrt( "dhlic", ithndl )
c-----------------------------------------------------------------------
c     Initialization of local variables
c-----------------------------------------------------------------------
      licfil  = ' '
      vendor  = ' '

!      yfeat   = 1
!      write(*,*) 'NO LICENCE CHECK'
!      if ( timon ) call timstop( ithndl )
!      return

      nolic  = .false.
      usernm = '(unknown)'

c-----------------------------------------------------------------------
c     Check license file and context on command line direct or through ini file
c-----------------------------------------------------------------------
      CALL GETCOM ( '-MODELSUITE', 3    , LFOUND, IDUMMY, RDUMMY,
     +              MODELSUITE   , IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .NE. 0 ) THEN
            MODELSUITE = ' '
         ENDIF
      ELSE
         MODELSUITE = ' '
      ENDIF
      CALL GETCOM ( '-CONF', 3    , LFOUND, IDUMMY, RDUMMY,
     +              CONF   , IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .NE. 0 ) THEN
            CONF = ' '
         ENDIF
      ELSE
         CONF = ' '
      ENDIF
      CALL GETCOM ( '-L'  , 3    , LFOUND, IDUMMY, RDUMMY,
     +              LICFIL, IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .NE. 0 ) THEN
            LICFIL = ' '
         ENDIF
      ELSE
         LICFIL = ' '
      ENDIF
      CALL GETCOM ( '-I'  , 3    , LFOUND, IDUMMY, RDUMMY,
     +              INIFIL, IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .NE. 0 ) THEN
            INIFIL = ' '
         ENDIF
      ELSE
         INIFIL = 'delwaq.ini'
      ENDIF

      IF ( INIFIL .NE. ' ' ) THEN
         INQUIRE ( FILE=INIFIL, EXIST = LEXIST )
         IF ( LEXIST ) THEN
            LU = 0
            DO ILUN = 10, 99
               INQUIRE ( UNIT=ILUN, OPENED = LOPEN )
               IF ( .NOT. LOPEN ) THEN
                  LU = ILUN
                  EXIT
               ENDIF
            ENDDO
            OPEN(LU,FILE=INIFIL,ACTION='READ')
            IF ( MODELSUITE .EQ. ' ' ) THEN
               GROUP  = 'GENERAL'
               KEYWRD = 'MODELSUITE'
               CALL GKWINI ( LU    , GROUP , KEYWRD, MODELSUITE)
            ENDIF
            IF ( CONF .EQ. ' ' ) THEN
               GROUP  = 'GENERAL'
               KEYWRD = 'CONF'
               CALL GKWINI ( LU    , GROUP , KEYWRD, CONF)
            ENDIF
            GROUP  = 'GENERAL'
            KEYWRD = 'SOBEKRIVER'
            CALL GKWINI ( LU    , GROUP , KEYWRD, SBKRIV)
            IF ( SBKRIV .EQ. '0' ) THEN
               VERSBK = VERRUR
            ENDIF
            IF ( LICFIL .EQ. ' ' ) THEN
               GROUP  = 'SYSTEM'
               KEYWRD = 'LM_LICENSE_FILE'
               CALL GKWINI ( LU    , GROUP , KEYWRD, LICFIL)
            ENDIF
            CLOSE(LU)
         ENDIF
      ENDIF
c-----------------------------------------------------------------------
c     Check authorisation to use Delft3D program verd3d number, or SOBEK
c-----------------------------------------------------------------------

      d3dlic = .false.
      sbklic = .false.
      riblic = .false.
      call zoek(modelsuite,1,'Delft3D ',8,i_delft3d)
      call zoek(modelsuite,1,'SOBEK ',6,i_sobek)
      call zoek(modelsuite,1,'RIBASIM ',8,i_ribasim)
      if ( i_delft3d .eq. 1 ) then
         d3dlic = 0 .eq. authmd ('DHS_Delft3D', verd3d, licfil )
         if (.NOT. d3dlic ) then
            ierror = authmd ('DHS_Delft3D', verd3d, licfil )
            ierror = auther (0, errmsg)
            write(*,'(//a)') 'Error: Authorization problem DHS_Delft3D'
            write(*,'(a,a)') 'version :',verd3d
            write(*,'(a)')  errmsg
            write(lunrep,'(a)') 'Error: Authorization problem DHS_Delft3D:'
            write(lunrep,'(a,a)') 'version :',verd3d
            write(lunrep,'(a)') errmsg
            call srstop(1)
         endif
         d3dlic = 0 .eq. authmd ('DHS_Delft3D_WAQ' , verwaq, licfil )
         if (.NOT. d3dlic ) then
            ierror = authmd ('DHS_Delft3D_WAQ', verwaq, licfil )
            ierror = auther (0, errmsg)
            write(*,'(//a)') 'Error: Authorization problem DHS_Delft3D_WAQ'
            write(*,'(a,a)') 'version :',verwaq
            write(*,'(a)')  errmsg
            write(lunrep,'(//a)') 'Error: Authorization problem DHS_Delft3D_WAQ'
            write(lunrep,'(a,a)') 'version :',verwaq
            write(lunrep,'(a)')  errmsg
            call srstop(1)
         endif
         usernm='Delft3D licensee'
         permit = 0 .eq. autcfg(8,'Delft3D',vendor)
         if (permit) then

!           Determine argument behind USERNAME out of vendor
!           Username is between single quotes ( char(39) )

            iusnm = index( vendor, "USERNAME" )
            if ( iusnm .ne. 0) then
               iusnm = index( vendor(iusnm:), char(39) )
               jusnm = iusnm+1
               jusnm = index( vendor(jusnm:), char(39) )
               length = len(usernm)
               iend   = min(length, jusnm-1)
               usernm = vendor(iusnm+1:iusnm+iend)
            endif
         endif

!        any 3D license

         if ( l3dmod ) then
            if ( conf .eq. ' ' ) then
               permit_3d = .false.
               do i = 1,nfeat
                  featur = ' '
                  featur = 'DHS_Delft3D_'//trim(feat(i))//'_3D'
                  permit = 0 .eq. authmd (featur, verwaq,licfil )
                  if ( permit ) then
                     permit_3d = permit
                  endif
               enddo
               if ( .not. permit_3d ) then
                  write(*,'(//a)') 'Error: Authorization problem 3D simulation'
                  write(*,'(a)') 'no valid license found'
                  write(lunrep,'(a)') 'Error: Authorization problem 3D simulation'
                  write(lunrep,'(a)') 'no valid license found'
                  call srstop(1)
               endif
            else
               yfeat = 0
               do i = 1 , 24
                  call zoek(conf((i-1)*10+1:i*10),nfeat,feat,10,ifeat)
                  if ( ifeat .gt. 0 ) then
                     featur = ' '
                     featur = 'DHS_Delft3D_'//trim(feat(i))//'_3D'
                     permit = 0 .eq. authmd (featur, verwaq,licfil )
                     if ( .not. permit ) then
                        write(*,'(//a)') 'Error: Authorization problem 3D simulation'
                        write(*,'(a,a)') 'no valid license found:',trim(featur)
                        write(lunrep,'(a)') 'Error: Authorization problem 3D simulation'
                        write(lunrep,'(a,a)') 'no valid license found:',trim(featur)
                        call srstop(1)
                     endif
                  endif
               enddo
            endif
         endif

!        Get authorization for features

         if ( conf .eq. ' ' ) then
            do i = 1,nfeat
               featur = ' '
               featur = 'DHS_PROC_LIB_'//feat(i)
               permit = 0 .eq. authmd (featur, verwaq,licfil )
               if ( l3dmod ) then
                  featur = ' '
                  featur = 'DHS_Delft3D_'//trim(feat(i))//'_3D'
                  permit_3d = 0 .eq. authmd (featur, verwaq,licfil )
               else
                  permit_3d = .true.
               endif
               if ( permit .and. permit_3d ) then
                  yfeat(i) = 1
               else
                  yfeat(i) = 0
               endif
            enddo
         else
            yfeat = 0
            do i = 1 , 24
               call zoek(conf((i-1)*10+1:i*10),nfeat,feat,10,ifeat)
               if ( ifeat .gt. 0 ) then
                  featur = ' '
                  featur = 'DHS_PROC_LIB_'//feat(ifeat)
                  permit = 0 .eq. authmd (featur, verwaq,licfil )
                  if ( l3dmod ) then
                     featur_3d = ' '
                     featur_3d = 'DHS_Delft3D_'//trim(feat(i))//'_3D'
                     permit_3d = 0 .eq. authmd (featur_3d, verwaq,licfil )
                  else
                     permit_3d = .true.
                  endif
                  if ( permit .and. permit_3d) then
                     yfeat(ifeat) = 1
                  else
                     if ( .not. permit ) then
                        write(lunrep,'(2a)') 'WARNING no license for configuration:',trim(featur)
                     endif
                     if ( .not. permit_3d ) then
                        write(lunrep,'(2a)') 'WARNING no license for configuration:',trim(featur_3d)
                     endif
                  endif
               endif
            enddo
         endif

      elseif( i_sobek .eq. 1 ) then
         sbklic = 0 .eq. authmd ('SOBEK', versbk, licfil )
         if (.NOT. sbklic ) then
            nolic = .true.
            usernm = 'SOBEK user'
            yfeat(1) = 1 ! Allow "WAQ"
            yfeat(6) = 1 ! Allow "WAQS"
         endif
!           ierror = authmd ('SOBEK', versbk, licfil )
!           ierror = auther (0, errmsg)
!           write(*,'(//a)') 'Error: Authorization problem SOBEK'
!           write(*,'(a,a)') 'version :',versbk
!           write(*,'(a)')  errmsg
!           write(lunrep,'(a)') 'Error: Authorization problem SOBEK:'
!           write(lunrep,'(a,a)') 'version :',versbk
!           write(lunrep,'(a)') errmsg
!           call srstop(1)
!        endif
         sbklic = 0 .eq. authmd ('SOBEK_WQ', versbk, licfil )
         if ( .NOT. sbklic ) then
            nolic = .true.
            usernm = 'SOBEK user'
         endif
!           ierror = authmd ('SOBEK_WQ', versbk, licfil )
!           ierror = auther (0, errmsg)
!           write(*,'(//a)') 'Error: Authorization problem SOBEK_WQ'
!           write(*,'(a,a)') 'version :',versbk
!           write(*,'(a)')  errmsg
!           write(lunrep,'(//a)') 'Error: Authorization problem SOBEK_WQ'
!           write(lunrep,'(a,a)') 'version :',versbk
!           write(lunrep,'(a)')  errmsg
!           call srstop(1)
!        endif
         usernm='SOBEK licensee'

!        Get authorization for features
         yfeat = 0
         do i = 1 , 24
            call zoek(conf((i-1)*10+1:i*10),nfeat,feat,10,ifeat)
            if ( ifeat .gt. 0 ) then
               featur = ' '
               featur = feat(ifeat)
               permit = 0 .eq. authmd (featur, versbk,licfil )
               if ( permit ) then
                  yfeat(ifeat) = 1
               else
                  write(lunrep,'(2a)') 'WARNING no license for configuration:',trim(featur)
               endif
            endif
         enddo

         if ( nolic ) then
            yfeat(1) = 1 ! Allow "WAQ"
            yfeat(6) = 1 ! Allow "WAQS"
         endif

      elseif( i_ribasim .eq. 1 ) then
         riblic = 0 .eq. authmd ('RIBASIM', verrib, licfil )
         if (.NOT. riblic ) then
            ierror = authmd ('RIBASIM', verrib, licfil )
            ierror = auther (0, errmsg)
            write(*,'(//a)') 'Error: Authorization problem RIBASIM'
            write(*,'(a,a)') 'version :',verrib
            write(*,'(a)')  errmsg
            write(lunrep,'(a)') 'Error: Authorization problem RIBASIM'
            write(lunrep,'(a,a)') 'version :',verrib
            write(lunrep,'(a)') errmsg
            call srstop(1)
         endif
         usernm='RIBASIM licensee'

      else  ! like previously, take what you can get

         d3dlic = 0 .eq. authmd ('DHS_Delft3D', verd3d, licfil )
         sbklic = 0 .eq. authmd ('SOBEK', versbk, licfil )
         riblic = 0 .eq. authmd ('RIBASIM', verrib, licfil )
         if (.NOT. (d3dlic .or. sbklic .or. riblic) ) then
            !
            ! Go into demo mode - skip the rest of the checks
            !
            if ( .not. nolic ) then
                nolic  = .true.
                usernm = 'SOBEK user'
                yfeat(1) = 1 ! Allow "WAQ"
                yfeat(6) = 1 ! Allow "WAQS"
                if ( timon ) call timstop( ithndl )
                return
            endif

            ierror = authmd ('DHS_Delft3D', verd3d, licfil )
            ierror = auther (0, errmsg)
            write(*,'(//a)') 'Error: Authorization problem DHS_Delft3D'
            write(*,'(a)')  errmsg
            write(lunrep,'(a)') 'Authorization error:'
            write(lunrep,'(a)') errmsg
            ierror = authmd ('SOBEK', versbk, licfil )
            ierror = auther (0, errmsg)
            write(*,'(//a)') 'Error: Authorization problem SOBEK'
            write(*,'(a)')  errmsg
            write(lunrep,'(a)') 'Authorization error:'
            write(lunrep,'(a)') errmsg
            ierror = authmd ('RIBASIM', verrib, licfil )
            ierror = auther (0, errmsg)
            write(*,'(//a)') 'Error: Authorization problem RIBASIM'
            write(*,'(a)')  errmsg
            write(lunrep,'(a)') 'Authorization error:'
            write(lunrep,'(a)') errmsg
            call srstop(1)
         endif

!        Check water quality license, not for RIBASIM always license without processes

         PERMIT = riblic
         if ( .not. PERMIT .and. d3dlic) PERMIT = 0 .eq. authmd ('DHS_Delft3D_WAQ' , verwaq, licfil )
         if ( .not. PERMIT .and. sbklic) PERMIT = 0 .eq. authmd ('SOBEK_WQ', versbk, licfil )
         if (.NOT.PERMIT) then
            if ( d3dlic ) then
               ierror = authmd ('DHS_Delft3D_WAQ', verwaq, licfil )
               ierror = auther (0, errmsg)
               write(*,'(//a)') 'Error: Authorization problem'
               write(*,'(a)')  errmsg
               write(lunrep,'(//a)') 'Error: Authorization problem'
               write(lunrep,'(a)')  errmsg
            endif
            if ( sbklic ) then
               ierror = authmd ('SOBEK_WQ', versbk, licfil )
               ierror = auther (0, errmsg)
               write(*,'(//a)') 'Error: Authorization problem'
               write(*,'(a)')  errmsg
               write(lunrep,'(//a)') 'Error: Authorization problem'
               write(lunrep,'(a)')  errmsg
            endif
            call srstop(1)
         endif

!        Get info on USERNAME

         permit = 0 .eq. autcfg(8,'DHS_Delft3D',vendor)
         if (permit) then

!        Determine argument behind USERNAME out of vendor
!        Username is between single quotes ( char(39) )

           iusnm = index( vendor, "USERNAME" )
           if ( iusnm .ne. 0) then
             iusnm = index( vendor(iusnm:), char(39) )
             jusnm = iusnm+1
             jusnm = index( vendor(jusnm:), char(39) )
             length = len(usernm)
             iend   = min(length, jusnm-1)
             usernm = vendor(iusnm+1:iusnm+iend)
           endif
         else
             if ( riblic ) usernm='RIBASIM licensee'
             if ( sbklic ) usernm='SOBEK licensee'
         endif

!        get any 3d licence

         if ( l3dmod ) then
            permit_3d = .false.
            do i = 1,nfeat
               featur = ' '
               featur = 'DHS_Delft3D_'//trim(feat(i))//'_3D'
               permit = 0 .eq. authmd (featur, verwaq,licfil )
               if ( permit ) then
                  permit_3d = permit
               endif
            enddo
            if ( .not. permit_3d ) then
               write(*,'(//a)') 'Error: Authorization problem 3D simulation'
               write(*,'(a)') 'no valid license found'
               write(lunrep,'(a)') 'Error: Authorization problem 3D simulation'
               write(lunrep,'(a)') 'no valid license found'
               call srstop(1)
            endif
         endif

!        Get authorization for features

         do i = 1,nfeat
           featur = ' '
           featur = 'DHS_PROC_LIB_'//feat(i)
           PERMIT = 0 .eq. authmd (featur, verwaq,licfil )
           if ( .not. PERMIT .and. sbklic) then
              featur = feat(i)
              PERMIT = 0 .eq. authmd (featur, versbk,licfil )
           endif
           if ( l3dmod ) then
              featur = ' '
              featur = 'DHS_Delft3D_'//trim(feat(i))//'_3D'
              permit_3d = 0 .eq. authmd (featur, verwaq,licfil )
           else
              permit_3d = .true.
           endif
           if ( permit .and. permit_3d ) then
             YFEAT(i) = 1
           else
             YFEAT(i) = 0
           endif
         enddo

      endif
c
      if ( timon ) call timstop( ithndl )
      return
      end
