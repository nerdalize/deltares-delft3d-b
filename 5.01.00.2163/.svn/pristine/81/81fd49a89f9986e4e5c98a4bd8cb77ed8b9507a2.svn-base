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

      implicit real*8 (a-h,o-z)
c note:::: change dimensions in matinv to value of maxmd if changed
      integer pf
      character*6 ka, kb, nr, kn, nam, end, blank, h2o, hplus
      parameter (maxm=  40,
     2          maxmd=  60,
     3           maxp=  25,
     4          maxn = 200,
     5        maxaij = 600, maxn2 = maxn+1, maxp2 = maxp+1)
      common /real8/ aij(maxaij), bbb(maxm,5), phk, t(20),
     2      bmult(5), b(maxm), pie(maxmd), v1(maxmd), v2(maxmd),
     3      v3(maxmd), v4(maxmd), x(maxn), xmf(maxn), c(maxn),
     4      x1(maxn), x2(maxn), x3(maxn), xbar(maxp),
     5      r(maxmd,maxmd), fe, fe2, ermb, xemb, erma, xema,
     6      tol1, tol2, xmin, xstart, barmin, slacks, aliter, rt
      common /hol6/ ka(12), kb(12), nr(maxm,2), kn(maxn),
     2      nam(maxp,2), end, blank, h2o, hplus
      common /intgr/ irow(maxaij), jcol(maxaij), jcomp(maxn),
     2      ijfind(maxn2), kl(maxp2), m, mend, ncomp, n, nit, not, pf,
     3      iter, itmax, ierror, lastcp, ke, ncycle, nbstar, kpf,
     4      naij, iopt, mtog, marith, nemb, nema, iarith, ifind,
     5      intitl, insolv, ijohn, ilp, imatrx, ipush, irows, iscale
      equivalence (n,ntot)
      common /water/ c1(maxn), c2(maxn), diam(maxn), gamma(maxn),
     2      delth(maxn), ch(maxn), gfw(maxn), ehk, ceck,
     3      apt(maxn), bpt(maxn), compr(maxn), delv(maxn), delv2(maxn),
     4      adt, bdt, bdot, temp, tt, press, str, str1,
     5      bready,bactiv,bactwa,bconc,btemp,bpress,bppm,brt,bnew
      logical  bready,bactiv,bactwa,bconc,btemp,bpress,bppm,brt,bnew
