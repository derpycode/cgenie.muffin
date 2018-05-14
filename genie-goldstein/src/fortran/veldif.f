c 
c veldif.f  reordered to sensible order
c
      program veldif 
      use genie_util, ONLY: check_unit, check_iostat
      character lin*30
      real tmp(6,40,40,40), u(6,40,40,40)
      integer ios
      print*,'lmax imax jmax kmax'
      read(5,*)lmax,imax,jmax,kmax
      print*,'1st filename for input?'
      read(5,'(a)')lin
      write(6,'(a)')lin
      call check_unit(1,__LINE__,__FILE__)
      open(1,file=lin,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(1,*,iostat=ios)((((tmp(l,i,j,k),l=1,lmax),i=1,imax),
     &     j=1,jmax),k=1,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(1,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      print*,'2nd filename for input?'
      read(5,'(a)')lin
      write(6,'(a)')lin
      open(1,file=lin)
      read(1,*,iostat=ios)((((u(l,i,j,k),l=1,lmax),i=1,imax),j=1,jmax),
     &     k=1,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(1,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      print*,'filename for output (2-1)'
      read(5,'(a)')lin
      open(1,file=lin,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      write(1,10,iostat=ios)((((u(l,i,j,k)-tmp(l,i,j,k),l=1,lmax),i=1,
     &     imax),j=1,jmax),k=1,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
c    1 ,i=10,10 ),j=10,10 )
      close(1,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
  10  format(e16.9)
c 10  format(6e15.5)
      end
