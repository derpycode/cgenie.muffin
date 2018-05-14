
c **********************************************************************
c *** ocean flux calculation -- SINGLE *********************************
c **********************************************************************

      subroutine tstepo_flux

#include "ocean.cmn"
           
      real tv, ups(3), ups0, pec(3)

      real fe(maxl), fw(maxl), fn(maxl), fs(maxl,maxi), fa(maxl)
     1   , fb(maxl,maxi,maxj), fwsave(maxl)
      integer i, j, k, l
      parameter(ups0 = 0.)

c KICO ediff calc needs
      real diffv

      real tec, scc, dzrho, rdzrho, slim, tv1
      real dxrho(4), dxts(maxl,4), dyrho(4), dyts(maxl,4), dzts(maxl)
      integer ina, nnp, knp

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      scc = 0.0
      rdzrho = 0.0
c ------------------------------------------------------------ c

      if(diso)then
         scc = ec(2)
         limps = 0
      endif

c KICO initialise diffv
      diffv=diff(2)
      dmax = 0

* 2nd order explicit step

c lower boundary fluxes
      do 220 j=1,jmax
         do 220 i=1,imax
            do 220 l=1,lmax
               fb(l,i,j) = 0
  220 continue

      do 100 k=1,kmax
c southern boundary fluxes
         j = 1
         do 230 i=1,imax
            do 230 l=1,lmax
               fs(l,i) = 0
  230    continue
         do 100 j=1,jmax
c western boundary fluxes
            i = 1
            pec(1) = u(1,imax,j,k)*dphi/diff(1)
            ups(1) = pec(1) / (2.0 + abs(pec(1)))
            do 210 l=1,lmax
               if (k.ge.max(k1(imax,j),k1(1,j)))then
c western doorway
                  fw(l) = u(1,imax,j,k)*rc(j)*((1.-ups(1))*ts1(l,1,j,k)
     1                  + (1.+ups(1))*ts1(l,imax,j,k))*0.5
                  fw(l) = fw(l) - (ts1(l,1,j,k) - ts1(l,imax,j,k))
     1                       *rc2(j)*diff(1)
               else
                  fw(l) = 0
               endif
               fwsave(l) = fw(l)
  210       continue
            do 100 i=1,imax

c KICO calculate local vertical diffusivity
              if(k.ge.k1(i,j).and.k.lt.kmax)then
c First get vertical density gradient (also needed for isopycnal diff)
                call eosd(ec,ts1(1,i,j,k:k+1),ts1(2,i,j,k:k+1),zw(k),
     1             rdza(k),ieos,dzrho,tec)
                if(dzrho.lt.-1e-12)then
                  rdzrho = 1.0/dzrho
                else
                  rdzrho = -1e12
                endif
                dzrho_test(i,j,k) = dzrho
                if((iediff.gt.0).AND.(iediff.lt.3))then
c Value of diffv fine for applying diffusivity, but peclet number calc
c is a 1st order approximation if diffusivity is variable.
                   if(ediffpow2i.eq.0)then
                     diffv=ediff0+ediff1(i,j,k)
                   elseif(ediffpow2i.eq.1)then
                     diffv=ediff0+ediff1(i,j,k)*(-rdzrho)
                   elseif(ediffpow2i.eq.2)then
                     diffv=ediff0+ediff1(i,j,k)*sqrt(-rdzrho)
                   else
                     diffv=ediff0+ediff1(i,j,k)*((-rdzrho)**ediffpow2)
                   endif
                   if(diffv.gt.diffmax(k+1)) diffv=diffmax(k+1)
                   diffv_test(i,j,k) = diffv
                 endif
               endif
               pec(1) = u(1,i,j,k)*dphi/diff(1)
               ups(1) = pec(1) / (2.0 + abs(pec(1)))
c rather untidy mask to avoid undefined dsv at jmax nre
               pec(2) = u(2,i,j,k)*dsv(min(j,jmax-1))/diff(1)
               ups(2) = pec(2) / (2.0 + abs(pec(2)))
               pec(3) = u(3,i,j,k)*dza(k)/diffv
               ups(3) = pec(3) / (2.0 + abs(pec(3)))
               do l=1,lmax
c flux to east
                  if(i.eq.imax)then
c eastern edge(doorway or wall)
                     fe(l) = fwsave(l)
                  elseif(k.lt.max(k1(i,j),k1(i+1,j)))then
                     fe(l) = 0
                  else
                     fe(l) = u(1,i,j,k)*rc(j)*((1.-ups(1))
     &                   *ts1(l,i+1,j,k) + (1.+ups(1))*ts1(l,i,j,k))*0.5
                     fe(l) = fe(l) - (ts1(l,i+1,j,k) - ts1(l,i,j,k))
     1                       *rc2(j)*diff(1)
                  endif
c flux to north
                  if(k.lt.max(k1(i,j),k1(i,j+1)))then
                     fn(l) = 0
                  else
                     fn(l) = cv(j)*u(2,i,j,k)*((1.-ups(2))
     &                   *ts1(l,i,j+1,k) + (1.+ups(2))*ts1(l,i,j,k))*0.5
                     fn(l) = fn(l) - cv2(j)*(ts1(l,i,j+1,k) -
     1                       ts1(l,i,j,k))*diff(1)
                  endif
c flux above
                  if(k.lt.k1(i,j))then
                     fa(l) = 0
                  elseif(k.eq.kmax)then
                     fa(l) = ts(l,i,j,kmax+1) 
                  else
                     fa(l) = u(3,i,j,k)*((1.-ups(3))*ts1(l,i,j,k+1) +
     1                       (1.+ups(3))*ts1(l,i,j,k))*0.5
                     fa(l) = fa(l) - (ts1(l,i,j,k+1) - ts1(l,i,j,k))
     1                       *rdza(k)*diffv
                  endif
               enddo
               if(diso)then
c isoneutral diffusion
                  if(k.ge.k1(i,j).and.k.lt.kmax)then
                     if(dzrho.lt.-1e-12)then
                        tv1 = 0.0
c tracer loop          
                        do knp=0,1
                           do nnp=0,1
                              ina = 1+nnp + 2*knp
c phi derivatives
                              do l=1,lmax
                                 if(k+knp.ge.k1(i-1+2*nnp,j))then
                                    dxts(l,ina) = (ts1(l,i+nnp,j,k+knp)
     2                                   - ts1(l,i+nnp-1,j,k+knp))
     3                                   *rc(j)*rdphi
                                 else
                                    dxts(l,ina) = 0.
                                 endif
c s-derivatives
                                 if(k+knp.ge.k1(i,j-1+2*nnp))then
                                    dyts(l,ina) = (ts1(l,i,j+nnp,k+knp)
     2                                   - ts1(l,i,j+nnp-1,k+knp))
     &                                   *cv(j-1+nnp)*rdsv(j+nnp-1)
                                 else
                                    dyts(l,ina) = 0.
                                 endif
                              enddo
                              dxrho(ina) = scc*dxts(2,ina)
     1                             -tec*dxts(1,ina)
                              dyrho(ina) = scc*dyts(2,ina)
     1                             -tec*dyts(1,ina)
c calculate diagonal part
                              tv1 = tv1 + dxrho(ina)*dxrho(ina)
     1                             + dyrho(ina)*dyrho(ina)
                           enddo
                        enddo
                        tv1 = 0.25*tv1*rdzrho*rdzrho
c limit flux by factor slim for large slope
                        if(tv1.gt.ssmax(k))then
                           slim = ssmax(k)*ssmax(k)/(tv1*tv1)
c count flux-limited points
                           limps = limps + 1
                        else
                           slim = 1.0
                        endif
                        tv1 = tv1*slim*diff(1)*rdza(k)
c test vertical diffusion number
                        tv = tv1*dt(k)*rdza(k)
                        if(tv.gt.dmax)then
                           dmax = tv
                        endif
                        do l=1,lmax
                           dzts(l) = (ts1(l,i,j,k+1) 
     1                          - ts1(l,i,j,k))*rdza(k)
c add isoneutral vertical flux
                           tv = 0
                           do ina=1,4
                              tv = tv + (2*dzrho*dxts(l,ina)
     1                             - dxrho(ina)*dzts(l))*dxrho(ina) 
     2                             + (2*dzrho*dyts(l,ina)
     3                             - dyrho(ina)*dzts(l))*dyrho(ina)
                           enddo
                           tv = 0.25*slim*diff(1)*tv/(dzrho*dzrho)
                           fa(l) = fa(l) + tv
                        enddo
                     endif
                  endif
               endif
               do l=1,lmax
                  tv = 0
                  if(k.ge.k1(i,j))then
                     ts(l,i,j,k) = ts1(l,i,j,k) - dt(k)*( - tv +
     1                           (fe(l) - fw(l))*rdphi
     2                           + (fn(l) - fs(l,i))*rds(j)
     3                           + (fa(l) - fb(l,i,j))*rdz(k))
                  endif
                  fw(l) = fe(l)
                  fs(l,i) = fn(l)
                  fb(l,i,j) = fa(l)
               enddo

               call eos(ec,ts(1,i,j,k),ts(2,i,j,k),zro(k),
     1            ieos,rho(i,j,k))
  100 continue

      end

c **********************************************************************
c *** ocean flux calculation -- THREAD #1 *********************************
c **********************************************************************

      subroutine tstepo_flux_t(ts_,ts1_,rho_)

#include "ocean.cmn"
           
      real ts_(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
      real ts1_(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
      real rho_(0:maxi+1,0:maxj+1,0:maxk)

      real tv, ups(3), ups0, pec(3)

      real fe(maxl), fw(maxl), fn(maxl), fs(maxl,maxi), fa(maxl)
     1   , fb(maxl,maxi,maxj), fwsave(maxl)
      integer i, j, k, l
      parameter(ups0 = 0.)

c KICO ediff calc needs
      real diffv

      real tec, scc, dzrho, rdzrho, slim, tv1
      real dxrho(4), dxts(maxl,4), dyrho(4), dyts(maxl,4), dzts(maxl)
      integer ina, nnp, knp

      print*,'@@@ tstepo_flux_t'

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      scc = 0.0
      rdzrho = 0.0
c ------------------------------------------------------------ c

      if(diso)then
         scc = ec(2)
         limps = 0
      endif

c KICO initialise diffv
      diffv=diff(2)
      dmax = 0

* 2nd order explicit step

c lower boundary fluxes
      do 220 j=1,jmax
         do 220 i=1,imax
            do 220 l=1,lmax
               fb(l,i,j) = 0
  220 continue

      do 100 k=1,kmax
c southern boundary fluxes
         j = 1
         do 230 i=1,imax
            do 230 l=1,lmax
               fs(l,i) = 0
  230    continue
         do 100 j=1,jmax
c western boundary fluxes
            i = 1
            pec(1) = u(1,imax,j,k)*dphi/diff(1)
            ups(1) = pec(1) / (2.0 + abs(pec(1)))
            do 210 l=1,lmax
               if (k.ge.max(k1(imax,j),k1(1,j)))then
c western doorway
                  fw(l) = u(1,imax,j,k)*rc(j)*((1.-ups(1))*ts1(l,1,j,k)
     1                  + (1.+ups(1))*ts1(l,imax,j,k))*0.5
                  fw(l) = fw(l) - (ts1_(l,1,j,k) - ts1_(l,imax,j,k))
     1                       *rc2(j)*diff(1)
               else
                  fw(l) = 0
               endif
               fwsave(l) = fw(l)
  210       continue
            do 100 i=1,imax

c KICO calculate local vertical diffusivity
              if(k.ge.k1(i,j).and.k.lt.kmax)then
c First get vertical density gradient (also needed for isopycnal diff)
                call eosd(ec,ts1_(1,i,j,k:k+1),ts1_(2,i,j,k:k+1),zw(k),
     1             rdza(k),ieos,dzrho,tec)
                if(dzrho.lt.-1e-12)then
                  rdzrho = 1.0/dzrho
                else
                  rdzrho = -1e12
                endif
                if((iediff.gt.0).AND.(iediff.lt.3))then
c Value of diffv fine for applying diffusivity, but peclet number calc
c is a 1st order approximation if diffusivity is variable.
                   if(ediffpow2i.eq.0)then
                     diffv=ediff0+ediff1(i,j,k)
                   elseif(ediffpow2i.eq.1)then
                     diffv=ediff0+ediff1(i,j,k)*(-rdzrho)
                   elseif(ediffpow2i.eq.2)then
                     diffv=ediff0+ediff1(i,j,k)*sqrt(-rdzrho)
                   else
                     diffv=ediff0+ediff1(i,j,k)*((-rdzrho)**ediffpow2)
                   endif
                   if(diffv.gt.diffmax(k+1)) diffv=diffmax(k+1)
                 endif
               endif
               pec(1) = u(1,i,j,k)*dphi/diff(1)
               ups(1) = pec(1) / (2.0 + abs(pec(1)))
c rather untidy mask to avoid undefined dsv at jmax nre
               pec(2) = u(2,i,j,k)*dsv(min(j,jmax-1))/diff(1)
               ups(2) = pec(2) / (2.0 + abs(pec(2)))
               pec(3) = u(3,i,j,k)*dza(k)/diffv
               ups(3) = pec(3) / (2.0 + abs(pec(3)))
               do l=1,lmax
c flux to east
                  if(i.eq.imax)then
c eastern edge(doorway or wall)
                     fe(l) = fwsave(l)
                  elseif(k.lt.max(k1(i,j),k1(i+1,j)))then
                     fe(l) = 0
                  else
                     fe(l) = u(1,i,j,k)*rc(j)*((1.-ups(1))
     &                   *ts1_(l,i+1,j,k) + (1.+ups(1))*ts1_(l,i,j,k))*0.5
                     fe(l) = fe(l) - (ts1_(l,i+1,j,k) - ts1_(l,i,j,k))
     1                       *rc2(j)*diff(1)
                  endif
c flux to north
                  if(k.lt.max(k1(i,j),k1(i,j+1)))then
                     fn(l) = 0
                  else
                     fn(l) = cv(j)*u(2,i,j,k)*((1.-ups(2))
     &                   *ts1_(l,i,j+1,k) + (1.+ups(2))*ts1_(l,i,j,k))*0.5
                     fn(l) = fn(l) - cv2(j)*(ts1_(l,i,j+1,k) -
     1                       ts1_(l,i,j,k))*diff(1)
                  endif
c flux above
                  if(k.lt.k1(i,j))then
                     fa(l) = 0
                  elseif(k.eq.kmax)then
                     fa(l) = ts(l,i,j,kmax+1) 
                  else
                     fa(l) = u(3,i,j,k)*((1.-ups(3))*ts1_(l,i,j,k+1) +
     1                       (1.+ups(3))*ts1_(l,i,j,k))*0.5
                     fa(l) = fa(l) - (ts1_(l,i,j,k+1) - ts1_(l,i,j,k))
     1                       *rdza(k)*diffv
                  endif
               enddo
               if(diso)then
c isoneutral diffusion
                  if(k.ge.k1(i,j).and.k.lt.kmax)then
                     if(dzrho.lt.-1e-12)then
                        tv1 = 0.0
c tracer loop          
                        do knp=0,1
                           do nnp=0,1
                              ina = 1+nnp + 2*knp
c phi derivatives
                              do l=1,lmax
                                 if(k+knp.ge.k1(i-1+2*nnp,j))then
                                    dxts(l,ina) = (ts1_(l,i+nnp,j,k+knp)
     2                                   - ts1_(l,i+nnp-1,j,k+knp))
     3                                   *rc(j)*rdphi
                                 else
                                    dxts(l,ina) = 0.
                                 endif
c s-derivatives
                                 if(k+knp.ge.k1(i,j-1+2*nnp))then
                                    dyts(l,ina) = (ts1_(l,i,j+nnp,k+knp)
     2                                   - ts1_(l,i,j+nnp-1,k+knp))
     &                                   *cv(j-1+nnp)*rdsv(j+nnp-1)
                                 else
                                    dyts(l,ina) = 0.
                                 endif
                              enddo
                              dxrho(ina) = scc*dxts(2,ina)
     1                             -tec*dxts(1,ina)
                              dyrho(ina) = scc*dyts(2,ina)
     1                             -tec*dyts(1,ina)
c calculate diagonal part
                              tv1 = tv1 + dxrho(ina)*dxrho(ina)
     1                             + dyrho(ina)*dyrho(ina)
                           enddo
                        enddo
                        tv1 = 0.25*tv1*rdzrho*rdzrho
c limit flux by factor slim for large slope
                        if(tv1.gt.ssmax(k))then
                           slim = ssmax(k)*ssmax(k)/(tv1*tv1)
c count flux-limited points
                           limps = limps + 1
                        else
                           slim = 1.0
                        endif
                        tv1 = tv1*slim*diff(1)*rdza(k)
c test vertical diffusion number
                        tv = tv1*dt(k)*rdza(k)
                        if(tv.gt.dmax)then
                           dmax = tv
                        endif
                        do l=1,lmax
                           dzts(l) = (ts1_(l,i,j,k+1) 
     1                          - ts1_(l,i,j,k))*rdza(k)
c add isoneutral vertical flux
                           tv = 0
                           do ina=1,4
                              tv = tv + (2*dzrho*dxts(l,ina)
     1                             - dxrho(ina)*dzts(l))*dxrho(ina) 
     2                             + (2*dzrho*dyts(l,ina)
     3                             - dyrho(ina)*dzts(l))*dyrho(ina)
                           enddo
                           tv = 0.25*slim*diff(1)*tv/(dzrho*dzrho)
                           fa(l) = fa(l) + tv
                        enddo
                     endif
                  endif
               endif
               do l=1,lmax
                  tv = 0
                  if(k.ge.k1(i,j))then
                     ts_(l,i,j,k) = ts1_(l,i,j,k) - dt(k)*( - tv +
     1                           (fe(l) - fw(l))*rdphi
     2                           + (fn(l) - fs(l,i))*rds(j)
     3                           + (fa(l) - fb(l,i,j))*rdz(k))
                  endif
                  fw(l) = fe(l)
                  fs(l,i) = fn(l)
                  fb(l,i,j) = fa(l)
               enddo

               call eos(ec,ts_(1,i,j,k),ts_(2,i,j,k),zro(k),
     1            ieos,rho_(i,j,k))
  100 continue

      end
