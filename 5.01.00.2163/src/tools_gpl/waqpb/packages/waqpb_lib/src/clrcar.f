      subroutine clrcar (ndim, indarr, array )
      integer ndim
      integer indarr(ndim)
      character*(*) array(ndim)

c     Remove all elements for which the index = 0
c     Without actually updating the index array

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              array(ndiml) = array(idim)
          endif
   10 continue

      return
      end

      subroutine clriar (ndim, indarr, array )
      integer ndim
      integer indarr(ndim)
      integer array(ndim)

c     Remove all elements for which the index = 0
c     Without actually updating the index array

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              array(ndiml) = array(idim)
          endif
   10 continue

      return
      end

      subroutine clrrar (ndim, indarr, array )
      integer ndim
      integer indarr(ndim)
      real array(ndim)

c     Remove all elements for which the index = 0
c     Without actually updating the index array

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              array(ndiml) = array(idim)
          endif
   10 continue

      return
      end

      subroutine updind (ndim, indarr)
      integer ndim
      integer indarr(ndim)

c     Remove all elements for which the index = 0
c     And update length of table!!

      integer ndiml, idim

      ndiml = 0
      do 10 idim = 1,ndim
          if (indarr(idim) .gt. 0 ) then
              ndiml = ndiml + 1
              indarr(ndiml) = indarr(idim)
          endif
   10 continue
      ndim = ndiml

      return
      end

