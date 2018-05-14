      integer function lnsig(title)

      implicit none
c
c     This function returns the singificant length of a character string
c
      character*(*) title
      INTEGER ILEN,I
c
      ilen=len(title)
      do i=ilen,1,-1
         if (title(i:i).ne.' ') go to 10
      end do
10    continue
      lnsig=i
      if (i.gt.0) then
         if (ichar(title(i:i)).eq.0) lnsig=i-1
      end if
c
      end
