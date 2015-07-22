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

      SUBROUTINE DLWQIV ( LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR , VARARR, VARIDX, VARTDA,
     +                    VARDAG, VARTAG, VARAGG, NOGRID, VGRSET)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : Jan van Beek
C
C     FUNCTION            : Initialisation of Variables structure
C
C     SUBROUTINES CALLED  :
C
C     FILES               :
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C
C     Declaration of arguments
C

      use timers

      INTEGER             LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR
      INTEGER             VARARR(NOVAR) , VARIDX(NOVAR) ,
     +                    VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +                    VARTAG(NOVAR) , VARAGG(NOVAR)
      INTEGER             VGRSET(NOVAR,NOGRID)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqiv", ithandl )
C
C     Just take the used array's in the right order
C
      IIVOL  =  1
      IIAREA =  2
      IIFLOW =  3
      IILENG =  4
      IIDISP =  5
      IICONC =  6
      IIMASS =  7
      IIDERV =  8
      IIBOUN =  9
      IIBSET = 10
      IIBSAV = 11
      IIWSTE = 12
      IICONS = 13
      IIPARM = 14
      IIFUNC = 15
      IISFUN = 16
      IIDNEW = 17
      IIDIFF = 18
      IIVNEW = 19
      IIVELO = 20
      IIHARM = 21
      IIFARR = 22
      IIMAS2 = 23
      IITIMR = 24
      IIVOL2 = 25
      IITRAC = 26
      IIGWRK = 27
      IIGHES = 28
      IIGSOL = 29
      IIGDIA = 30
      IIGTRI = 31
      IISMAS = 32
      IIPLOC = 33
      IIDEFA = 34
      IIFLUX = 35
      IISTOC = 36
      IIFLXD = 37
      IIFLXI = 38
      IIRIOB = 39
      IIDSPX = 40
      IIVELX = 41
      IILOCX = 42
      IIDSTO = 43
      IIVSTO = 44
      IIDMPQ = 45
      IIDMPS = 46
      IITRRA = 47
      IINRSP = 48
      IIVOLL = 49
      IIVOL3 = 50
      IIR1   = 51
      IIQXK  = 52
      IIQYK  = 53
      IIQZK  = 54
      IIDIFX = 55
      IIDIFY = 56
      IIDIFZ = 57
      IIVOLA = 58
      IIVOLB = 59
      IIGUV  = 60
      IIGVU  = 61
      IIGZZ  = 62
      IIAAK  = 63
      IIBBK  = 64
      IICCK  = 65
      IIBD3X = 66
      IIBDDX = 67
      IIBDX  = 68
      IIBU3X = 69
      IIBUUX = 70
      IIBUX  = 71
      IIWRK1 = 72
      IIWRK2 = 73
      IIAAKL = 74
      IIBBKL = 75
      IICCKL = 76
      IIDDKL = 77
C
      IVVOL = 1
      IVARE = IVVOL + 1
      IVFLO = IVARE + 1
      IVLEN = IVFLO + 1
      IVCNS = IVLEN + 2
      IVPAR = IVCNS + NOCONS
      IVFUN = IVPAR + NOPA
      IVSFU = IVFUN + NOFUN
      IVCNC = IVSFU + NOSFUN
      IVMAS = IVCNC + NOTOT
      IVDER = IVMAS + NOTOT
      IVDSP = IVDER + NOTOT
      IVVEL = IVDSP + NODISP
      IVDEF = IVVEL + NOVELO
      IVLOC = IVDEF + NODEF
      IVDSX = IVLOC + NOLOC
      IVVLX = IVDSX + NDSPX
      IVLCX = IVVLX + NVELX
      IVFLX = IVLCX + NLOCX
C
C
C
      CALL DHZERI(VGRSET,NOVAR*NOGRID)
C
C     Volume
C
      IVAR = 1
c     VARARR(IVAR) = IIVOL
c     VARIDX(IVAR) = 1
c     VARTDA(IVAR) = 0
c     VARDAG(IVAR) = 0
c     VARTAG(IVAR) = 1
c     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
C
C     Area
C
      IVAR = IVAR + 1
c     VARARR(IVAR) = IIAREA
c     VARIDX(IVAR) = 1
c     VARTDA(IVAR) = 0
c     VARDAG(IVAR) = 0
c     VARTAG(IVAR) = 0
c     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
C
C     Flow
C
      IVAR = IVAR + 1
c     VARARR(IVAR) = IIFLOW
c     VARIDX(IVAR) = 1
c     VARTDA(IVAR) = 0
c     VARDAG(IVAR) = 0
c     VARTAG(IVAR) = 0
c     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
C
C     Length , two length
C
      IVAR = IVAR + 1
c     VARARR(IVAR) = IILENG
c     VARIDX(IVAR) = 1
c     VARTDA(IVAR) = 0
c     VARDAG(IVAR) = 0
c     VARTAG(IVAR) = 0
c     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
      IVAR = IVAR + 1
c     VARARR(IVAR) = IILENG
c     VARIDX(IVAR) = 2
c     VARTDA(IVAR) = 0
c     VARDAG(IVAR) = 0
c     VARTAG(IVAR) = 0
c     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
C
C     Cons
C
      DO ICONS = 1 , NOCONS
         IVAR = IVAR + 1
c        VARARR(IVAR) = IICONS
c        VARIDX(IVAR) = ICONS
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Param
C
      DO IPA = 1 , NOPA
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIPARM
c        VARIDX(IVAR) = IPA
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 3
c        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Func
C
      DO IFUN = 1 , NOFUN
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIFUNC
c        VARIDX(IVAR) = IFUN
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Seg Func
C
      DO ISFUN = 1 , NOSFUN
         IVAR = IVAR + 1
c        VARARR(IVAR) = IISFUN
c        VARIDX(IVAR) = ISFUN
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 3
c        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Conc
C
      DO ISYS = 1 , NOSYS
         IVAR = IVAR + 1
c        VARARR(IVAR) = IICONC
c        VARIDX(IVAR) = ISYS
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 3
c        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
      DO ISYS = NOSYS + 1 , NOTOT
         IVAR = IVAR + 1
c        VARARR(IVAR) = IICONC
c        VARIDX(IVAR) = ISYS
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 1
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Mass
C
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIMASS
c        VARIDX(IVAR) = ISYS
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 1
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Deriv
C
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIDERV
c        VARIDX(IVAR) = ISYS
c        VARTDA(IVAR) = 2
c        VARDAG(IVAR) = IVMAS + ISYS - 1
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
      ENDDO
C
C     Disp
C
      DO IDSP = 1 , NODISP
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIDISP
c        VARIDX(IVAR) = IDSP
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Velo
C
      DO IVEL = 1 , NOVELO
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIVELO
c        VARIDX(IVAR) = IVEL
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Default
C
      DO IDEF = 1 , NODEF
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIDEFA
c        VARIDX(IVAR) = IDEF
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
C
C     Local
C
      DO ILOC = 1 , NOLOC
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIPLOC
c        VARIDX(IVAR) = ILOC
c        VARTDA(IVAR) = 1
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 3
c        VARAGG(IVAR) = 1
      ENDDO
C
C     DSPX
C
      DO IDSX = 1 , NDSPX
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIDSPX
c        VARIDX(IVAR) = IDSX
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
      ENDDO
C
C     VELX
C
      DO IVLX = 1 , NVELX
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIVELX
c        VARIDX(IVAR) = IVLX
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
      ENDDO
C
C     LOCX
C
      DO ILCX = 1 , NLOCX
         IVAR = IVAR + 1
c        VARARR(IVAR) = IILOCX
c        VARIDX(IVAR) = ILCX
c        VARTDA(IVAR) = 0
c        VARDAG(IVAR) = 0
c        VARTAG(IVAR) = 0
c        VARAGG(IVAR) = 0
      ENDDO
C
C     FLUX
C
      DO IFLX = 1 , NFLUX
         IVAR = IVAR + 1
c        VARARR(IVAR) = IIFLUX
c        VARIDX(IVAR) = IFLX
c        VARTDA(IVAR) = 2
c        VARDAG(IVAR) = IVVOL
c        VARTAG(IVAR) = 1
c        VARAGG(IVAR) = 0
      ENDDO
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
