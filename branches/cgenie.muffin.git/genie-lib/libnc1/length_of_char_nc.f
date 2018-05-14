!========================================
! Marc 26/3/03
! Returns the length in characters of cname
! I've been using it to find the length of netcdf file names which 
! is why it's part of the *_nc suite.   
!========================================
      function length_of_char_nc(cname)
      implicit none
!----------------------------------------
! Define variables 
!----------------------------------------
      character(*), intent(in) :: cname      ! name of string 
      integer :: length_of_char_nc           ! length of string
      integer :: j                           ! loop variable
!----------------------------------------
! find length of string 
!----------------------------------------
      j=len(cname)
      do while (cname(j:j).eq.' ')
         j=j-1
      enddo
      length_of_char_nc=j

      end
