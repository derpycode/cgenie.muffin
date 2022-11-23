!     =============
!     MODULE FFTMOD
!     =============

      module fftmod
      implicit none
      integer NRES
      parameter(NRES = 13)
      integer :: nallowed(NRES)=(/8,16,32,48,64,96,128,256,384,512,1024,2048,4096/)
!     T5    - N16   : 8-2
!     T10   - N32   : 8-2-2
!     T15   - N48   : 8-3-2
!     T21   - N64   : 8-4-2
!     T31   - N96   : 8-4-3
!     T42   - N128  : 8-4-4
!     T85   - N256  : 8-4-4-2
!     T127  - N384  : 8-4-4-3
!     T170  - N512  : 8-4-4-4
!     T341  - N1024 : 8-4-4-4-2
!     T682  - N2048 : 8-4-4-4-4
!     T1365 - N4096 : 8-4-4-4-4-2

      integer :: lastn = 0
      real,allocatable :: trigs(:)
      end module fftmod

!     ================
!     SUBROUTINE GP2FC
!     ================

      subroutine gp2fc(a,n,lot)
      use fftmod
      implicit none
      integer n,lot,la,l
      real a(n,lot)

      if (n /= lastn) then
         if (allocated(trigs)) deallocate(trigs)
         allocate(trigs(n))
         lastn = n
         call fftini(n)
      endif

      call dfft8(a,a,n,lot)
      la = n / 8
      do while (la >= 4)
         call dfft4(a,trigs,n,lot,la)
      enddo

      if (la == 3) then
         do l = 1 , lot
            call dfft3(a(1,l),trigs,n)
         enddo
      endif

      if (la == 2) then
         do l = 1 , lot
            call dfft2(a(1,l),trigs,n)
         enddo
      endif
      return
      end subroutine gp2fc

!     ================
!     SUBROUTINE FC2GP
!     ================

      subroutine fc2gp(a,n,lot)
      use fftmod
      implicit none
      integer n,lot,nf,la
      real a(n,lot)

      if (n /= lastn) then
         if (allocated(trigs)) deallocate(trigs)
         allocate(trigs(n))
         lastn = n
         call fftini(n)
      endif

      nf = n/8
      do while (nf >= 4)
         nf = nf/4
      enddo
      la = 1
      if (nf == 2) call ifft2(a,trigs,n,lot,la)
      if (nf == 3) call ifft3(a,trigs,n,lot,la)
      do while (la < n/8)
         call ifft4(a,trigs,n,lot,la)
      enddo
      call ifft8(a,a,n,lot)
      return
      end subroutine fc2gp

!     =================
!     SUBROUTINE FFTINI
!     =================

      subroutine fftini(n)
      use fftmod
      implicit none
      logical labort
      integer n,j,k
      real del,angle

!     check for allowed values of n

      labort = .true.
      do j = 1 , NRES
         if (n == nallowed(j)) labort = .false.
      enddo

      if (labort) then
         write (*,*) '*** FFT does not support n = ',n,' ***'
         write (*,*) 'Following resolutions may be used:'
         write (*,*) '----------------------------------'
         do j = 1 , NRES
            write (*,1000) nallowed(j), nallowed(j)/2, nallowed(j)/3
         enddo
         stop
      endif
 1000 format(' NLON=',I5,'  NLAT=',I5,'  NTRU=',I5)

      del = 4.0 * asin(1.0) / n
      do k=0,n/2-1
        angle = k * del
        trigs(2*k+1) = cos(angle)
        trigs(2*k+2) = sin(angle)
      enddo
      return
      end subroutine fftini

!     ================
!     SUBROUTINE DFFT2
!     ================

      subroutine dfft2(a,trigs,n)
      implicit none
      integer n,ja,jb,i
      real a,c,trigs,c1,s1,a1p3,a3m1
      dimension a(n),c(n),trigs(n)

      c(1) = a(1) + a(2)
      c(2) = 0.0

      ja = 3
      jb = n - 1

      do i=3,n-5,4
         c1 = trigs(ja  )
         s1 = trigs(ja+1)
         a1p3 = c1 * a(i+1) + s1 * a(i+3)
         a3m1 = c1 * a(i+3) - s1 * a(i+1)
         c(ja  ) = a(i) + a1p3
         c(jb  ) = a(i) - a1p3
         c(ja+1) = a3m1 + a(i+2)
         c(jb+1) = a3m1 - a(i+2)
         ja = ja + 2
         jb = jb - 2
      enddo

      c(ja  ) =  a(n-1)
      c(ja+1) = -a(n  )

      a = c
      return
      end subroutine dfft2

!     ================
!     SUBROUTINE DFFT3
!     ================

      subroutine dfft3(a,trigs,n)
      implicit none
      integer n,ja,jb,jc,i
      real a,trigs,c,c1,s1,c2,s2,a1,b1,a2,b2,a3,b3
      real*8 SIN60
      parameter(SIN60 = 0.866025403784438D0)
      dimension a(n),c(n),trigs(n)

      ja = 1              !  1
      jb = 2 * (n/3)  + 1 ! 65
      jc = jb             ! 65

      c(ja  ) = a(1) + a(2) + a(3)
      c(ja+1) = 0.0
      c(jb  ) = a(1) - 0.5 * (a(2) + a(3))
      c(jb+1) =      SIN60 * (a(3) - a(2))

      ja = 3         !  3, 5, 7, ... ,31
      jb = jb + 2    ! 67,69,71, ... ,95
      jc = jc - 2    ! 63,61,59, ... ,35

      do i = 4 , n-8 , 6 ! 88
         c1 = trigs(ja  )
         s1 = trigs(ja+1)
         c2 = trigs(ja+ja-1)
         s2 = trigs(ja+ja  )
         a1 = (c1*a(i+1)+s1*a(i+4))+(c2*a(i+2)+s2*a(i+5))
         b1 = (c1*a(i+4)-s1*a(i+1))+(c2*a(i+5)-s2*a(i+2))
         a2 = a(i  ) - 0.5 * a1
         b2 = a(i+3) - 0.5 * b1
         a3 = SIN60*((c1*a(i+1)+s1*a(i+4))-(c2*a(i+2)+s2*a(i+5)))
         b3 = SIN60*((c1*a(i+4)-s1*A(i+1))-(c2*a(i+5)-s2*a(i+2)))
         c(ja  ) = a(i  ) + a1
         c(ja+1) = a(i+3) + b1
         c(jb  ) = a2 + b3
         c(jb+1) = b2 - a3
         c(jc  ) = a2 - b3
         c(jc+1) =-b2 - a3
         ja = ja + 2
         jb = jb + 2
         jc = jc - 2
      enddo

      if (ja <= jc) then ! ja=33  jc=33
         c(ja  ) = a(n-2) + 0.5 * (a(n-1) - a(n)) ! 33
         c(ja+1) =       -SIN60 * (a(n-1) + a(n)) ! 34
      endif
      a(:) = c(:)
      return
      end subroutine dfft3

!     ================
!     SUBROUTINE DFFT4
!     ================

      subroutine dfft4(a,trigs,n,lot,la)
      implicit none
      integer n,lot,la,i,j,l,i1,i2,i3,i4,i5,i6,i7,j1,j2,j3,j4,j5,j6,j7
      integer k,kb,kc,kd,jink,j0,ibase
      real a,c,trigs,c1,s1,c2,s2,c3,s3
      real a0p2,a1p3,a1p5,a2p6,a3p7,a1m3,a5m1,a6m2,a7m3
      real a0,a1,a2,a3,b0,b1,b2,b3
      real sin45
      dimension a(n,lot),c(n,lot),trigs(n)
      la = la / 4

      i1 = la
      i2 = la + i1
      i3 = la + i2
      i4 = la + i3
      i5 = la + i4
      i6 = la + i5
      i7 = la + i6

      j1 = n/2 - la
      j2 = n - la
      j3 = j1
      j5 = j1 + la

      do i=1,la
         do l=1,lot
         a0p2 = a(i   ,l) + a(i2+i,l)
         a1p3 = a(i1+i,l) + a(i3+i,l)
         c(   i,l) = a0p2 + a1p3
         c(j2+i,l) = a0p2 - a1p3
         c(j1+i,l) = a(   i,l) - a(i2+i,l)
         c(j5+i,l) = a(i3+i,l) - a(i1+i,l)
         enddo
      enddo

      jink = 2 * la
      j0 = la
      j1 = j1 + jink
      j2 = j2 - jink
      j3 = j3 - jink
      j4 = j0 + la
      j5 = j1 + la
      j6 = j2 + la
      j7 = j3 + la

      ibase=4*la

      do 450 k=la,(n-4)/8,la
         kb=k+k
         kc=kb+kb
         kd=kc+kb
         c1=trigs(kb+1)
         s1=trigs(kb+2)
         c2=trigs(kc+1)
         s2=trigs(kc+2)
         c3=trigs(kd+1)
         s3=trigs(kd+2)

         i=ibase+1
         do j=1,la
            do l=1,lot
            a1p5 = c1 * a(i1+i,l) + s1 * a(i5+i,l)
            a2p6 = c2 * a(i2+i,l) + s2 * a(i6+i,l)
            a3p7 = c3 * a(i3+i,l) + s3 * a(i7+i,l)
            a5m1 = c1 * a(i5+i,l) - s1 * a(i1+i,l)
            a6m2 = c2 * a(i6+i,l) - s2 * a(i2+i,l)
            a7m3 = c3 * a(i7+i,l) - s3 * a(i3+i,l)
            a0 = a(i,l) + a2p6
            a2 = a(i,l) - a2p6
            a1 = a1p5 + a3p7
            a3 = a3p7 - a1p5
            b0 = a(i4+i,l) + a6m2
            b2 = a(i4+i,l) - a6m2
            b1 = a5m1 + a7m3
            b3 = a5m1 - a7m3
            c(j0+j,l) = a0+a1
            c(j2+j,l) = a0-a1
            c(j4+j,l) = b0+b1
            c(j6+j,l) = b1-b0
            c(j1+j,l) = a2+b3
            c(j3+j,l) = a2-b3
            c(j5+j,l) = a3+b2
            c(j7+j,l) = a3-b2
            enddo
            i=i+1
         enddo

        ibase=ibase+8*la
        j0 = j0 + jink
        j1 = j1 + jink
        j2 = j2 - jink
        j3 = j3 - jink
        j4 = j0 + la
        j5 = j1 + la
        j6 = j2 + la
        j7 = j3 + la
450   continue
      if (j1 <= j2) then
         sin45=sqrt(0.5)
         i=ibase+1
         do j=1,la
            do l=1,lot
            a1p3 = sin45 * (a(i1+i,l) + a(i3+i,l))
            a1m3 = sin45 * (a(i1+i,l) - a(i3+i,l))
            c(j0+j,l) =  a(   i,l) + a1m3
            c(j1+j,l) =  a(   i,l) - a1m3
            c(j4+j,l) = -a(i2+i,l) - a1p3
            c(j5+j,l) =  a(i2+i,l) - a1p3
            enddo
            i=i+1
         enddo
      endif
      if (la == 1) then
         do l=1,lot
         a(1,l) = c(1,l)
         a(2,l) = 0.0
         a(3:n,l) = c(2:n-1,l)
         enddo
      else
         a = c
      endif
      return
      end subroutine dfft4

!     ================
!     SUBROUTINE DFFT8
!     ================

      subroutine dfft8(a,c,n,lot)
      implicit none
      integer n,lot,la,i
      integer i0,i1,i2,i3,i4,i5,i6,i7
      real a0p4,a1p5,a2p6,a3p7,a5m1,a7m3,a0m4,a6m2
      real a0p4p2p6,a1p5p3p7,a7m3p5m1,a7m3m5m1
      real a(n*lot),c(n*lot)
      real z,zsin45

      la = n / 8
      z  = 1.0 / n
      zsin45 = z * sqrt(0.5)

      do i=0,la*lot-1
         i0 = (i/la) * n + mod(i,la) + 1
         i1 = i0 + la
         i2 = i1 + la
         i3 = i2 + la
         i4 = i3 + la
         i5 = i4 + la
         i6 = i5 + la
         i7 = i6 + la

         a0p4 =  a(i0) + a(i4)
         a1p5 =  a(i1) + a(i5)
         a2p6 =  a(i2) + a(i6)
         a3p7 =  a(i3) + a(i7)
         a5m1 =  a(i5) - a(i1)
         a7m3 =  a(i7) - a(i3)
         a0m4 = (a(i0) - a(i4)) * z
         a6m2 = (a(i6) - a(i2)) * z

         a0p4p2p6 = a0p4 + a2p6
         a1p5p3p7 = a1p5 + a3p7
         a7m3p5m1 = (a7m3 + a5m1) * zsin45
         a7m3m5m1 = (a7m3 - a5m1) * zsin45

         c(i0) = z * (a0p4p2p6 + a1p5p3p7)
         c(i7) = z * (a0p4p2p6 - a1p5p3p7)
         c(i3) = z * (a0p4 - a2p6)
         c(i4) = z * (a3p7 - a1p5)
         c(i1) = a0m4 + a7m3m5m1
         c(i5) = a0m4 - a7m3m5m1
         c(i2) = a7m3p5m1 + a6m2
         c(i6) = a7m3p5m1 - a6m2
      enddo
      return
      end subroutine dfft8

!     ================
!     SUBROUTINE IFFT4
!     ================

      subroutine ifft4(c,trigs,n,lot,la)
      implicit none
      real a,c,trigs
      real c1,s1,c2,s2,c3,s3
      real a0p2,a0m2,a1p3,a1m3,a4p6,a4m6,a5p7,a5m7
      real a0p2m1p3,a4m6m5m7
      real sin45
      integer n,la,m,kstop,lot,i,j,l
      integer i0,i1,i2,i3,i4,i5,i6,i7
      integer j1,j2,j3,j4,j5,j6,j7
      integer iink,jbase
      integer k,kb,kc,kd
      dimension a(n,lot),c(n,lot),trigs(n)

      if (la == 1) then
         a(1,:) = 0.5 * c(1,:)
         a(n,:) = 0.0
         a(2:n-1,:) = c(3:n,:)
      else
         a = c
      endif

      m=n/4
      kstop=(n-4)/8

      i1 = n/2 - la
      i2 = n   - la
      i5 = i1  + la

      j1 = la
      j2 = la+j1
      j3 = la+j2
      j4 = la+j3
      j5 = la+j4
      j6 = la+j5
      j7 = la+j6

      do i=1,la
      do l=1,lot
         c(   i,l) = a(i,l) + a(i2+i,l) + a(i1+i,l)
         c(j1+i,l) = a(i,l) - a(i2+i,l) - a(i5+i,l)
         c(j2+i,l) = a(i,l) + a(i2+i,l) - a(i1+i,l)
         c(j3+i,l) = a(i,l) - a(i2+i,l) + a(i5+i,l)
      enddo
      enddo

      iink  = 2 * la
      jbase = 4 * la + 1
      i0    = la
      i1    = i0 + n/2
      i2    = n - 3 * la
      i3    = i2 - n/2
      i4    = i0 + la
      i5    = i1 + la
      i6    = i2 + la
      i7    = i3 + la

      do 450 k=la,kstop,la
        kb=k+k
        kc=kb+kb
        kd=kc+kb
        c1=trigs(kb+1)
        s1=trigs(kb+2)
        c2=trigs(kc+1)
        s2=trigs(kc+2)
        c3=trigs(kd+1)
        s3=trigs(kd+2)
        do i = 1 , la
           j = jbase
           do l=1,lot
           a0p2 = a(i0+i,l) + a(i2+i,l)
           a0m2 = a(i0+i,l) - a(i2+i,l)
           a1p3 = a(i1+i,l) + a(i3+i,l)
           a1m3 = a(i1+i,l) - a(i3+i,l)
           a4p6 = a(i4+i,l) + a(i6+i,l)
           a4m6 = a(i4+i,l) - a(i6+i,l)
           a5p7 = a(i5+i,l) + a(i7+i,l)
           a5m7 = a(i5+i,l) - a(i7+i,l)

           a0p2m1p3 = a0p2 - a1p3
           a4m6m5m7 = a4m6 - a5m7

           c(   j,l) = a0p2 + a1p3
           c(j4+j,l) = a4m6 + a5m7
           c(j2+j,l) = c2 * a0p2m1p3 - s2 * a4m6m5m7
           c(j6+j,l) = s2 * a0p2m1p3 + c2 * a4m6m5m7
           c(j1+j,l) = c1*(a0m2-a5p7)-s1*(a4p6+a1m3)
           c(j5+j,l) = s1*(a0m2-a5p7)+c1*(a4p6+a1m3)
           c(j3+j,l) = c3*(a0m2+a5p7)-s3*(a4p6-a1m3)
           c(j7+j,l) = s3*(a0m2+a5p7)+c3*(a4p6-a1m3)
           enddo
           jbase=jbase+1
        enddo
        i0 = i0 + iink
        i1 = i1 + iink
        i2 = i2 - iink
        i3 = i3 - iink
        i4 = i4 + iink
        i5 = i5 + iink
        i6 = i6 - iink
        i7 = i7 - iink
        jbase=jbase+7*la
450     continue

      if (i1 <= i2) then
         sin45=sqrt(0.5)
         do i=1,la
            j=jbase
            do l=1,lot
            c(   j,l)=a(i0+i,l)+a(i1+i,l)
            c(j1+j,l)=sin45*((a(i0+i,l)-a(i1+i,l))-(a(la+i0+i,l)+a(la+i1+i,l)))
            c(j2+j,l)=a(la+i1+i,l)-a(la+i0+i,l)
            c(j3+j,l)=-sin45*((a(i0+i,l)-a(i1+i,l))+(a(la+i0+i,l)+a(la+i1+i,l)))
            enddo
            jbase=jbase+1
         enddo
      endif
      la = la * 4
      return
      end subroutine ifft4

!     ================
!     SUBROUTINE IFFT2
!     ================

      subroutine ifft2(a,trigs,n,lot,la)
      implicit none
      real a,c,trigs
      integer n,lot,la,ia,ib,j,l
      real c1,s1,amb,apb
      dimension a(n,lot),c(n,lot),trigs(n)

      c(1,:) = 0.5 * a(1,:)
      c(2,:) = c(1,:)

      ia    =   3
      ib    = n-1

      do j = 3 , n-5 , 4
         c1 = trigs(ia  )
         s1 = trigs(ia+1)
         do l=1,lot
         amb = a(ia  ,l) - a(ib  ,l)
         apb = a(ia+1,l) + a(ib+1,l)
         c(j  ,l) = a(ia  ,l) + a(ib  ,l)
         c(j+2,l) = a(ia+1,l) - a(ib+1,l)
         c(j+1,l) = c1 * amb - s1 * apb
         c(j+3,l) = s1 * amb + c1 * apb
         enddo
         ia = ia + 2
         ib = ib - 2
      enddo
      c(n-1,:) =  a(ia  ,:)
      c(n  ,:) = -a(ia+1,:)

      a(:,:) = c(:,:)
      la = 2
      return
      end subroutine ifft2

!     ================
!     SUBROUTINE IFFT3
!     ================

      subroutine ifft3(a,trigs,n,lot,la)
      implicit none
      real a,c,trigs,sin60,c1,s1,c2,s2,hbpc,hbmc,sbmc,sbpc
      integer n,lot,la,ia,ib,ic,j,l
      dimension a(n,lot),c(n,lot),trigs(n)
      parameter(SIN60 = 0.866025403784438D0)

      ib = 2 * (n/3) + 1

      c(1,:) = 0.5 * a(1,:) + a(ib,:)
      c(2,:) = 0.5 * a(1,:) - 0.5 * a(ib,:) - SIN60 * a(ib+1,:)
      c(3,:) = 0.5 * a(1,:) - 0.5 * a(ib,:) + SIN60 * a(ib+1,:)

      ia = 3
      ic = ib - 2
      ib = ib + 2

      do j = 4 , n-8 , 6
         c1 = trigs(ia  )
         s1 = trigs(ia+1)
         c2 = trigs(ia+ia-1)
         s2 = trigs(ia+ia  )

         do l = 1 , lot
            hbpc = a(ia  ,l) - 0.5 * (a(ib  ,l) + a(ic  ,l))
            hbmc = a(ia+1,l) - 0.5 * (a(ib+1,l) - a(ic+1,l))
            sbmc = SIN60 * (a(ib  ,l) - a(ic  ,l))
            sbpc = SIN60 * (a(ib+1,l) + a(ic+1,l))

            c(j  ,l) = a(ia  ,l) + a(ib  ,l) + a(ic  ,l)
            c(j+3,l) = a(ia+1,l) + a(ib+1,l) - a(ic+1,l)
            c(j+1,l) = c1 * (hbpc-sbpc) - s1 * (hbmc+sbmc)
            c(j+4,l) = s1 * (hbpc-sbpc) + c1 * (hbmc+sbmc)
            c(j+2,l) = c2 * (hbpc+sbpc) - s2 * (hbmc-sbmc)
            c(j+5,l) = s2 * (hbpc+sbpc) + c2 * (hbmc-sbmc)
         enddo
         ia = ia + 2
         ib = ib + 2
         ic = ic - 2
      enddo

      c(n-2,:) = a(ia,:)
      c(n-1,:) =   0.5 * a(ia,:) - SIN60 * a(ia+1,:)
      c(n  ,:) = - 0.5 * a(ia,:) - SIN60 * a(ia+1,:)

      a(:,:)  = c(:,:)
      la = 3
      return
      end subroutine ifft3

!     ================
!     SUBROUTINE IFFT8
!     ================

      subroutine ifft8(a,c,n,lot)
      implicit none
      real SQRT2,a,c
      real a0p7,a0m7,a1p5,a1m5,a2p6,a2m6
      real a0p7p3,a0p7m3,a0m7p4,a0m7m4
      real a1m5p2p6,a1m5m2p6
      integer n,lot,la
      integer i,i0,i1,i2,i3,i4,i5,i6,i7
      parameter(SQRT2 = 1.414213562373095D0)
      dimension a(n*lot),c(n*lot)
      la = n / 8

      do i=0,la*lot-1
         i0 = (i/la) * n + mod(i,la) + 1
         i1 = i0 + la
         i2 = i1 + la
         i3 = i2 + la
         i4 = i3 + la
         i5 = i4 + la
         i6 = i5 + la
         i7 = i6 + la

         a0p7 = a(i0) + a(i7)
         a0m7 = a(i0) - a(i7)
         a1p5 = a(i1) + a(i5)
         a1m5 = a(i1) - a(i5)
         a2p6 = a(i2) + a(i6)
         a2m6 = a(i2) - a(i6)

         a0p7p3   = a0p7 + a(i3)
         a0p7m3   = a0p7 - a(i3)
         a0m7p4   = 2.0 * (a0m7 + a(i4))
         a0m7m4   = 2.0 * (a0m7 - a(i4))
         a1m5p2p6 = SQRT2 * (a1m5 + a2p6)
         a1m5m2p6 = SQRT2 * (a1m5 - a2p6)

         c(i0)  = 2.0 * (a0p7p3 + a1p5)
         c(i2)  = 2.0 * (a0p7m3 - a2m6)
         c(i4)  = 2.0 * (a0p7p3 - a1p5)
         c(i6)  = 2.0 * (a0p7m3 + a2m6)

         c(i1)  = a0m7m4 + a1m5m2p6
         c(i3)  = a0m7p4 - a1m5p2p6
         c(i5)  = a0m7m4 - a1m5m2p6
         c(i7)  = a0m7p4 + a1m5p2p6
      enddo
      return
      end
