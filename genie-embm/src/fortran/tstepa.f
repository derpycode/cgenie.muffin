*
* subroutine tstepa.f for program goldstein, introduced 8/2/02
* transports tair, qair meridionally and vertically
* updates tair, qair in lower atmosphere
* 
* flux version fully explicit one step second order variable depth

      subroutine tstepa

#include "embm.cmn"

      real fe(2), fw(2), fn(2), fs(2,maxi), fa(2), fb(2,maxi,maxj)
     +    ,fwsave(2)

      real diffextra

      integer i, j, l

* 2nd order explicit step

c lower boundary fluxes

      do 220 j=1,jmax
         do 220 i=1,imax
            do 220 l=1,2   
               fb(l,i,j) = 0
  220 continue

c southern boundary fluxes

         j = 1
         do 230 i=1,imax
            do 230 l=1,2   
               fs(l,i) = 0
  230    continue

         do 100 j=1,jmax
c western boundary fluxes
            i = 1
            do 210 l=1,2   
c western doorway
               fw(l) = betaz(l)*uatm(1,imax,j)*rc(j)*(tq1(l,1,j) +
     1                       tq1(l,imax,j))*0.5
c add zonal heat diffusion
               diffextra = (2-l)*diffmod0*max(0.0,min(1.0,
     1            (pptn(i,j)-ppmin)/(ppmax-ppmin)))
               fw(l) = fw(l) - (tq1(l,1,j) - tq1(l,imax,j))
     1                    *rc(j)*rc(j)*rdphi*(diffa(l,1,j)+diffextra)
               fwsave(l) = fw(l)
210         continue

            do 100 i=1,imax
               do 120 l=1,2   
c flux to east
                  if(i.eq.imax)then
c eastern edge (doorway or wall)
                     fe(l) = fwsave(l)
                  else
                     fe(l) = betaz(l)*uatm(1,i,j)*rc(j)*(tq1(l,i+1,j) 
     1                       + tq1(l,i,j))*0.5
c add zonal heat diffusion
                     diffextra = (2-l)*diffmod0*max(0.0,min(1.0,
     1                  (pptn(i,j)-ppmin)/(ppmax-ppmin)))
                     fe(l) = fe(l) - (tq1(l,i+1,j) - tq1(l,i,j))
     1                    *rc(j)*rc(j)*rdphi*(diffa(l,1,j)+diffextra)
                  endif
c flux to north
                  if(j.ne.jmax)then
c except northermost gridpoints

                     fn(l) = cv(j)*betam(l)*uatm(2,i,j)*(tq1(l,i,j+1)
     1                       + tq1(l,i,j))*0.5
c add meridional heat diffusion
                     diffextra = (2-l)*diffmod0*max(0.0,min(1.0,
     1                  (pptn(i,j)-ppmin)/(ppmax-ppmin)))
                     fn(l) = fn(l) - cv(j)*cv(j)*(diffa(l,2,j)
     1                     + diffextra)
     1                     *(tq1(l,i,j+1) - tq1(l,i,j))*rdsv(j)
                  else
                     fn(l) = 0.
                  endif

c                 if(m.eq.2)then
c zero flux through top of upper layer
c                    fa(l) = 0
c                 else
c surface flux
                     fb(l,i,j) = tqa(l,i,j)
c flux through interface between lower and upper atmosphere
                     fa(l) = 0
c                 endif

                     tq(l,i,j) = tq1(l,i,j) - dtatm*(
     1                           (fe(l) - fw(l))*rdphi
     2                           + (fn(l) - fs(l,i))*rds(j)
     3                           + fa(l) - fb(l,i,j))
c    3                           + (fa(l) - fb(l,i,j))/(hatmbl(l)/dsc))

c nre dimless height of atm set to 1, implies factor of h in fluxsc

                  fw(l) = fe(l)
                  fs(l,i) = fn(l)
                  fb(l,i,j) = fa(l)
  120          continue

  100 continue

      do 10 j=1,jmax
         do 10 i=1,imax
            do 10 l=1,2   
               tq1(l,i,j) = tq(l,i,j)
   10 continue

      end
