c
c co.f convection code simplified form for program goldstein
c      suitable for arbitrary functions rho(T,S) variable depth
c cost counts occurences of mixing at each point not including the point
c at the top of each mixed region
c if cost is 2-d it counts the average number of convecting points at 
c each horizontal point (divide by nsteps in mains.f)
c 22/5/2 lmax.gt.2 allowed 
c 10/6/2 ts array passed as argument
c 16/5/5 coshuffle from Simon Mueller's Bern version added with edits nre
c 1/8/06 old/new convection scheme option added (rma)
c 04/08/08 KICO modified to handle thermobaricity
c 05/08/08 KICO merging of duplicated code

      subroutine co(tv,mldpk)

#include "ocean.cmn"

      real dzm(maxk), sum(maxl), tv(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
      integer mldpk(2,maxi,maxj)

      integer i, j, k(0:maxk), l, lastmix, m, n, ni, icond

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      icond = 0
c ------------------------------------------------------------ c

      if(iconv.eq.1) then
c Mueller convection scheme:
c first apply coshuffle to mix directly down to density level
c would be faster to inline this call
c don't comment out the call without ensuring icosd is initialised
         call coshuffle(tv,mldpk)
      endif

      do 210 j=1,jmax
         do 210 i=1,imax

c initialize the index array k and mixed region sizes dzm
c wet points only
           if(k1(i,j).le.kmax)then
           if(iconv.eq.1)icond = 0

            k(k1(i,j)-1) = 0
            do 220 m=k1(i,j),kmax
               k(m) = m
               dzm(m) = dz(m)
  220       continue

            m = kmax
            lastmix = 0

c main loop 'normally' decreasing in m

            do 230 while(k(m-1).gt.0.or.(lastmix.ne.0.and.k(m).ne.kmax))

c KICO added code for thermobaricity
               if(ieos.ne.0)then
                  call eos(ec,tv(1,i,j,k(m)),
     1             tv(2,i,j,k(m)),zw(k(m-1)),
     2             ieos,rho(i,j,k(m)))
                  call eos(ec,tv(1,i,j,k(m-1)),
     1             tv(2,i,j,k(m-1)),zw(k(m-1)),
     2             ieos,rho(i,j,k(m-1)))
               endif

               if(rho(i,j,k(m)).lt.rho(i,j,k(m-1)).or.k(m-1).eq.0)then
c this may need changing, unless as rho(i,j,0) dimensioned
                  if(lastmix.eq.0.or.k(m).eq.kmax)then
                     m = m-1
                  else
                     m = m+1
                  endif
                  lastmix = 0
               else
                  lastmix = 1

c look for instability before mixing

                  n = m-1

c KICO added code for thermobaricity
                  if(ieos.ne.0)then
                    call eos(ec,tv(1,i,j,k(n)),
     1                 tv(2,i,j,k(n)),zw(k(n-1)),
     2                 ieos,rho(i,j,k(n)))
                    call eos(ec,tv(1,i,j,k(n-1)),
     1                 tv(2,i,j,k(n-1)),zw(k(n-1)),
     2                 ieos,rho(i,j,k(n-1)))
                  endif

                  do 240 while (k(n-1).gt.0.and.
     1                         rho(i,j,k(n)).ge.rho(i,j,k(n-1)))
                     n = n-1
c KICO added code for thermobaricity
                     if(ieos.ne.0)then
                       call eos(ec,tv(1,i,j,k(n)),
     1                    tv(2,i,j,k(n)),zw(k(n-1)),
     2                    ieos,rho(i,j,k(n)))
                       call eos(ec,tv(1,i,j,k(n-1)),
     1                    tv(2,i,j,k(n-1)),
     2                    zw(k(n-1)),ieos,rho(i,j,k(n-1)))
                     endif
  240             enddo
                  do 280 l=1,lmax
                  sum(l) = tv(l,i,j,k(m))*dzm(k(m))
  280             continue
                  do 260 ni=1,m-n
                     do 270 l=1,lmax
                     sum(l) = sum(l) + tv(l,i,j,k(m-ni))*dzm(k(m-ni))
  270                continue
                     dzm(k(m)) = dzm(k(m)) + dzm(k(m-ni))
  260             continue
                  do 290 l=1,lmax
                     tv(l,i,j,k(m)) = sum(l)/dzm(k(m))
  290             continue
                  call eos(ec,tv(1,i,j,k(m)),
     1               tv(2,i,j,k(m)),zw(k(m-1)),
     2               ieos,rho(i,j,k(m)))
c reindex k(m)
                  ni = m-1
                  do 250 while (k(ni+1).gt.0)
                     k(ni) = k(ni-m+n)
                     ni = ni-1
  250             enddo
               endif
  230       enddo

c fill in T,S values in mixed regions

            m = kmax-1
            do 300 n=kmax-1,k1(i,j),-1
               if(n.gt.k(m))then
                  do 310 l=1,lmax
                     tv(l,i,j,n) = tv(l,i,j,k(m+1))
 310              continue
                  call eos(ec,tv(1,i,j,n),tv(2,i,j,n),zw(k(n-1)),
     1               ieos,rho(i,j,n))
                  if(iconv.eq.1)then
                    icond = icond + 1
                  else
                    cost(i,j) = cost(i,j) + 1.0
                  endif
               else
                  m = m-1
               endif
 300        continue

            if(iconv.eq.1)then
              icosd(i,j) = max(icosd(i,j),icond)
crma fix for convection diagnostic needed by biogem (rma, 26/10/05)
              cost(i,j) = dsc*zw(kmax - 1 - icosd(i,j))
            endif

c KICO 18/09/07
            mldpk(1,i,j)=k(m+1)
c wet points only
        endif
 210  continue

      end
c
c $Id: co.f 2964 2006-08-01 16:20:25Z cvs-gw $
c
c coshuffle.f - Bern3D+C
c
c Original Version            : Simon Mueller
c Adaption for Bern3D+C model : Simon Mueller
c modified                    : Neil Edwards
c
c $Log$
c Revision 1.5  2006/08/01 16:20:25  cvs-gw
c removed unused variables:
c 'n': line 246
c 'temp': line 248
c
c Revision 1.4  2006/08/01 13:06:59  cvs-rma
c choice of two convection schemes (original or Mueller)
c
c Revision 1.3  2006/07/05 10:56:31  cvs-rma
c New convection scheme, originally "coshuffle" by S. Mueller
c
c modifications 2005/5/17 nre
c icosd is now the maximum integer depth of convection at this timestep
c number of passes limited (initially to kmax)
c
c Revision 1.1  2004/08/19 12:09:11  sam
c updated physical model to version used in paper 'mueller et al., 
c ventilation time scales in an efficient 3-d ocean model'
c
c 2003-09-17 sam
c
      subroutine coshuffle(tv,mldpk)

#include "ocean.cmn"
      
      real tv(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
      real tv_temp
      integer mldpk(2,maxi,maxj)

      integer i, j, k, k0, l, ipass, maxpass
c    +       , icosd(maxi,maxj)

      maxpass = kmax

      do 10 j=1,jmax
         do 10 i=1,imax
            if(k1(i,j).le.kmax)then

               icosd(i,j) = 0
               k0 = 0
               ipass = 0
               do 5 while (k0.lt.kmax.and.ipass.lt.maxpass)
                  ipass = ipass + 1
                  k = kmax-1

c KICO added code for thermobaricity
                  if(ieos.ne.0)then
                    call eos(ec,tv(1,i,j,kmax),tv(2,i,j,kmax),zw(k),
     1                 ieos,rho(i,j,kmax))
                    call eos(ec,tv(1,i,j,k),tv(2,i,j,k),zw(k),
     1                 ieos,rho(i,j,k))
                  endif

                  do 20 while((rho(i,j,kmax).gt.rho(i,j,k))
     +                 .and.(k.ge.k1(i,j)))
                     k=k-1
c KICO added code for thermobaricity
                     if(ieos.ne.0)then
                       call eos(ec,tv(1,i,j,kmax),tv(2,i,j,kmax),zw(k),
     1                    ieos,rho(i,j,kmax))
                       call eos(ec,tv(1,i,j,k),tv(2,i,j,k),zw(k),
     1                    ieos,rho(i,j,k))
                     endif
 20               continue
c KICO 18/09/07
                  mldpk(2,i,j)=k0
                  k0 = k+1
                  if (k0.lt.kmax) then
                     do 50 l = 1,lmax
                        tv_temp = tv(l,i,j,kmax)
                        do 30 k = kmax,k0+1,-1
                           tv(l,i,j,k) = ((dz(k)-dz(kmax))*tv(l,i,j,k)
     +                          +dz(kmax)*tv(l,i,j,k-1))
     +                          *rdz(k)
 30                     continue
                        tv(l,i,j,k0) = ((dz(k0)-dz(kmax))*tv(l,i,j,k0)
     +                       +dz(kmax) *tv_temp)
     +                       *rdz(k0)
 50                  continue
                     do k = k0,kmax
                       call eos(ec,tv(1,i,j,k),tv(2,i,j,k),zw(kmax-1),
     1                    ieos,rho(i,j,k))
                     enddo
                     icosd(i,j) = max(icosd(i,j),kmax-k0)
                  endif
 5             continue
c              if(icosd(i,j).gt.0)print*,'coshuff',i,j,icosd(i,j),ipass
            endif
 10   continue

      end
