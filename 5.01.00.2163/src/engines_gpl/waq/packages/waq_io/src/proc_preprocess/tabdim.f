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

      SUBROUTINE TABDIM ( LUREP , NSVICH, NSVOCH, NFLCH , NSTCH ,
     +                    LASWI , ICONF ,         NO_ACT, ACTLST,
     +                    ACTUSE, NBPR  , NO_INS, NO_INE, NO_OUS,
     +                    NO_OUE, NO_FLU, NO_STO, NO_DIS, NO_VEL,
     +                    NOINFO, NOWARN, IERROR)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: nov -1992 by Jan van Beek
C
C     FUNCTION            : Reads the binary proces definition file
C
C     LOGICAL UNITNUMBERS : LUREP   - report file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUREP   INTEGER       1     INPUT   Report file
C     NSVICH  INTEGER       1     INPUT   No of extra input variables for charon
C     NSVOCH  INTEGER       1     INPUT   No of extra output variables for charon
C     NFLCH   INTEGER       1     INPUT   No of fluxes for charon
C     NSTCH   INTEGER       1     INPUT   No of stochiometric terms for charon
C     LASWI   LOGICAL       1     INPUT   Active processes only switch
C     ICONF   INTEGER       1     INPUT   Requested configuration index
C     NO_ACT  INTEGER       1     INPUT   Number of activated processes
C     ACTLST  CHARACTER     *     INPUT   List of activated processes
C     ACTUSE  INTEGER       *     INPUT   activated processes status indicator
C     NBPR    INTEGER       1     OUTPUT  number of active processes
C     NO_INS  INTEGER       1     OUTPUT  Number of input var on segment
C     NO_INE  INTEGER       1     OUTPUT  Number of input var on exchange
C     NO_OUS  INTEGER       1     OUTPUT  Number of output var on segment
C     NO_OUE  INTEGER       1     OUTPUT  Number of output var on exchange
C     NO_FLU  INTEGER       1     OUTPUT  Number of fluxes
C     NO_STO  INTEGER       1     OUTPUT  Number of stochis
C     NO_DIS  INTEGER       1     OUTPUT  Number of dispersions
C     NO_VEL  INTEGER       1     OUTPUT  Number of velocities
C     NOINFO  INTEGER       1     IN/OUT  Cummulative information count
C     NOWARN  INTEGER       1     IN/OUT  Cummulative warning count
C     IERROR  INTEGER       1     OUTPUT  Error indicatior
C
C     IMPLICIT NONE for extra compiler checks
C
      use timers       !   performance timers

      IMPLICIT NONE
C
C     Declaration of arguments
C
      INTEGER        LUREP           , NSVICH          ,
     +               NSVOCH          , NFLCH           ,
     +               NSTCH           , ICONF           ,
     +               NO_ACT          , NBPR            ,
     +               NO_INS          , NO_INE          ,
     +               NO_OUS          , NO_OUE          ,
     +               NO_FLU          , NO_STO          ,
     +               NO_DIS          , NO_VEL          ,
     +               NOINFO          , NOWARN          ,
     +               IERROR
      INTEGER        ACTUSE(*)
      LOGICAL        LASWI
      CHARACTER*(*)          :: ACTLST(*)
C
C     Common declarations
C
      INCLUDE 'data.inc'
C
C     Local declarations
C
      INTEGER        INS   , INE   , OUS   , OUE   , FLU   ,
     +               STO   , DIS   , VEL   , IPROC , IOFFSE,
     +               NAANTA, IINPU , IITEM , JNDEX , IOUTP ,
     +               IOFFS2, NAANT2, IDISP , IVELO , IOUTF ,
     +               ISTOC , IFLUX , IPRCNF, IGET  , IACT  ,
     +               ILIC  , IC
      LOGICAL        SWIT2D
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "tabdim", ithndl )
C
C     Some init
C
      SWIT2D = .FALSE.

c
c     first loop, determine maximal dimensions
c
      NO_INS = 0
      NO_INE = 0
      NO_OUS = 0
      NO_OUE = 0
      NO_FLU = 0
      NO_STO = 0
      NO_DIS = 0
      NO_VEL = 0
      NBPR = 0
      do iproc=1,nproc
C
C        Check if process is requested and licensed
C
         IF ( .NOT. LASWI ) THEN
            IPRCNF = (IPROC-1)*NCONF + ICONF
            IGET   = ICNPRO(IPRCNF)
         ELSE
            CALL ZOEK ( PROCID(IPROC), NO_ACT, ACTLST, 10    , IACT  )
            IF ( IACT .GT. 0 ) THEN
!              ILIC = 0
!              DO IC = 1 , NCONF
!                 IPRCNF = (IPROC-1)*NCONF + IC
!                 IF ( ICNPRO(IPRCNF).GT.0 .AND. LICONF(IC).GT.0 ) THEN
!                    ILIC = 1
!                 ENDIF
!              ENDDO
!              IF ( ILIC .EQ. 0 ) THEN
!                 WRITE(LUREP,*)
!    +               ' ERROR: no valid license for activated process:',
!    +               PROCID(IPROC)
!                 CALL SRSTOP(1)
!              ENDIF
               IGET = 1
               ACTUSE(IACT) = 1
            ELSE
               IGET = 0
            ENDIF
         ENDIF
         IF ( IGET .GT. 0 ) THEN
            ins = 0
            ine = 0
            ous = 0
            oue = 0
            flu = 0
            sto = 0
            dis = 0
            vel = 0
            NBPR = NBPR+1

C           write (LUREP,'(''Process: '',a10)') procid(iproc)
C           write (*,'(''+Process: '',a10)') procid(iproc)

c------------------------------------------------------------------------c
c           CONSTRUCT PROCESS
c------------------------------------------------------------------------c

c           Fill PDF structure, only dimensions

C           INPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

c           scan input items table for FIRST occurence of proces

            do iinpu = 1 , ninpu
               call zoek ( procid(iproc), 1, inpupr(iinpu), 10, jndex)
               if ( jndex .gt.0 ) then
                  if ( inpusx(iinpu) .eq. 1 ) then
!                     ins = ins + 1
                      ins = max(ins,inpunm(iinpu)) ! this is incorrect, because ins and ine are mixed, resulting in overestimation
                  else
!                     ine = ine + 1
                      ine = max(ine,inpunm(iinpu)) ! this is incorrect, because ins and ine are mixed, resulting in overestimation
                  endif
               endif
            enddo

C           OUTPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

            do ioutp = 1 , noutp
               call zoek ( procid(iproc), 1, outppr(ioutp),10 , jndex)
               if ( jndex .gt.0 ) then
                  call zoek ( outpit(ioutp), nitem, itemid, 10, iitem)
                  if ( iitem .le. 0 ) stop 'unknown ITEM'

c                 Find item properties and store in PDF structure

                  if ( outpsx(ioutp) .eq. 1 ) then
!                     ous = ous + 1
                      ous = max(ous,outpnm(ioutp)) ! this is incorrect, because ous and oue are mixed, resulting in overestimation of almost a factor 2
                  else
!                     oue = oue + 1
                      oue = max(oue,outpnm(ioutp)) ! this is incorrect, because ous and oue are mixed, resulting in overestimation of almost a factor 2

c                     SCAN VELO and DISP TABLES FOR LINES ASSOCIATED WITH CURRENT OUTPUT ITEM ON EXCHANGE LEVEL

                      if ( .not. swit2d ) then
                         do idisp = 1 , ndisp
                            call zoek ( itemid(iitem)  , 1, dispit(idisp), 10, jndex)
                            if ( jndex .gt. 0 ) then
                               dis = dis + 1
                            endif
                         enddo
                      endif

c                     scan velocity lines table

                      if ( .not. swit2d ) then
                         do ivelo = 1 , nvelo
                            call zoek ( itemid(iitem)  , 1, veloit(ivelo), 10, jndex)
                            if ( jndex .gt. 0 ) then
                               vel = vel + 1
                            endif
                         enddo
                      endif

c                     END of processing output item on exchange level!

                  endif

               endif

            enddo

C           FLUXES

            do ioutf = 1 , noutf
               call zoek ( procid(iproc), 1, outfpr(ioutf), 10, jndex)
               if ( jndex .gt.0 ) then

!                 flu = flu + 1
                  flu = max(flu,outfnm(ioutf))

c                 Process current row

c                 Lookup flux in item  table

                  call zoek ( outffl(ioutf), nitem, itemid, 10, iflux)
                  if ( iflux .le. 0 ) THEN
                     WRITE(LUREP,*) 'ERROR: unknown flux:',OUTFFL(IOUTF)
                     IERROR = 3
                     GOTO 900
                  endif

                  ! scan stochi table for lines associated with present flux

                  do istoc = 1 , nstoc
                     call zoek ( itemid(iflux), 1, stocfl(istoc), 10, jndex)
                     if ( jndex .gt.0 ) then
                        sto = sto + 1
                     endif
                  enddo

               endif
            enddo

c           reserve for extra input/output if process is charon

            if ( procfo(iproc) .eq. 'D40CHA' ) then
               ins = ins + nsvich
               ous = ous + nsvoch
               flu = flu + nflch
               sto = sto + nstch
            endif

            NO_INS = NO_INS + INS
            NO_INE = NO_INE + INE
            NO_OUS = NO_OUS + OUS
            NO_OUE = NO_OUE + OUE
            NO_FLU = NO_FLU + FLU
            NO_STO = NO_STO + STO
            NO_DIS = NO_DIS + DIS
            NO_VEL = NO_VEL + VEL

         ENDIF
C
      enddo
C
  900 CONTINUE
C
C
      if (timon) call timstop( ithndl )
      RETURN
      END
