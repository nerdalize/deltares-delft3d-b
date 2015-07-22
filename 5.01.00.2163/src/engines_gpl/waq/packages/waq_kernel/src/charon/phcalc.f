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

C    Date:       22 Oct 1992
C    Time:       23:54
C    Program:    PHCALC.FOR
C    Version:    6.00.00
C    Programmer: Nicolaas M de Rooij
C
C
      subroutine phcalc
c        4-69
c        phcalc uses double precision version of
c           alog
      include  'char1.inc'
c        aliter is the constant used in the ph calculation
      mta = kl(1)
      mtb = kl(2) - 1
      do 10 j=mta,mtb
         if (kn(j).eq.hplus) go to 40
   10 continue
   20 phk = -0.0d+00
   30 return
   40 xfj = x(j)/xbar(1)
      if (xfj.eq.0.0d+00) go to 20
      bliter = dexp(gamma(j))
      phk = -dlog10(xfj*aliter*bliter)
      go to 30
      end
