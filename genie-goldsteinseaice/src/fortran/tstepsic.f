*
* subroutine tstepsic.f for program goldstein introduced 18/9/02 
* updates sea-ice height and area
* 
      subroutine tstepsic 

#include "seaice.cmn"

      integer i, j, l

      real fe(2), fw(2), fn(2), fs(2,maxi)
     +    ,fwsave(2)

c 2nd order explicit transport code using upper level ocean velocities

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
            if (kmax.ge.max(k1(imax,j),k1(1,j)))then
c western doorway
               fw(l) = u(1,imax,j)*rc(j)*(varice1(l,1,j) +
     1                    varice1(l,imax,j))*0.5
               if (u(1,imax,j).ge.0.0) then
                  if (varice1(2,1,j).gt.par_sica_thresh) then
                     fw(l) = 0
                  endif
                  if (varice1(1,1,j).gt.par_sich_thresh) then
                     fw(l) = 0
                  endif
               else
                  if (varice1(2,imax,j).gt.par_sica_thresh) then
                     fw(l) = 0
                  endif
                  if (varice1(1,imax,j).gt.par_sich_thresh) then
                     fw(l) = 0
                  endif
               endif
               fw(l) = fw(l) - (varice1(l,1,j) - varice1(l,imax,j))
     1                    *rc(j)*rc(j)*rdphi*diffsic
            else
               fw(l) = 0
            endif
            fwsave(l) = fw(l)
  210       continue

         do 100 i=1,imax
            do 120 l=1,2
c flux to east
               if(i.eq.imax)then
c eastern edge(doorway or wall)
                  fe(l) = fwsave(l)
               elseif(kmax.lt.max(k1(i,j),k1(i+1,j)))then
                  fe(l) = 0
               else
                  fe(l) = u(1,i,j)*rc(j)*(varice1(l,i+1,j) + 
     1                    varice1(l,i,j))*0.5
                  if (u(1,i,j).ge.0.0) then
                     if (varice1(2,i+1,j).gt.par_sica_thresh) then
                        fe(l) = 0
                     endif
                     if (varice1(1,i+1,j).gt.par_sich_thresh) then
                        fe(l) = 0
                     endif
                  else
                     if (varice1(2,i,j).gt.par_sica_thresh) then
                        fe(l) = 0
                     endif
                     if (varice1(1,i,j).gt.par_sich_thresh) then
                        fe(l) = 0
                     endif
                  endif
                  fe(l) = fe(l) - (varice1(l,i+1,j) - varice1(l,i,j))
     1                    *rc(j)*rc(j)*rdphi*diffsic
               endif
c flux to north
               if(kmax.lt.max(k1(i,j),k1(i,j+1)))then
                  fn(l) = 0
               else
                  fn(l) = cv(j)*u(2,i,j)*(varice1(l,i,j+1) +
     1                    varice1(l,i,j))*0.5
                  if (u(2,i,j).ge.0.0) then
                     if (varice1(2,i,j+1).gt.par_sica_thresh) then
                        fn(l) = 0
                     endif
                     if (varice1(1,i,j+1).gt.par_sich_thresh) then
                        fn(l) = 0
                     endif
                  else
                     if (varice1(2,i,j).gt.par_sica_thresh) then
                        fn(l) = 0
                     endif
                     if (varice1(1,i,j).gt.par_sich_thresh) then
                        fn(l) = 0
                     endif
                  endif
                  fn(l) = fn(l) - cv(j)*cv(j)*(varice1(l,i,j+1) -
     1                       varice1(l,i,j))*rdsv(j)*diffsic
               endif
c
               if(kmax.ge.k1(i,j))then
                  varice(l,i,j) = varice1(l,i,j) - dtsic*(
     1                        (fe(l) - fw(l))*rdphi
     2                        + (fn(l) - fs(l,i))*rds(j))
     1                          + tsc*dtsic*dtha(l,i,j)

               endif
               fw(l) = fe(l)
               fs(l,i) = fn(l)

  120       continue

  100 continue

      end
