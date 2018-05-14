*
* subroutine tstipa.f atmospheric timestep for c-goldstein
* JGS iterative implicit version 2/10/00
* NRE 2-step version, 2nd attempt 2/11/01
* to recover old explicit code change the lines indicated
* coefficient of implicitness cimp included 27/5/2 
* cimp=1 fully implicit, cimp=0 explicit
* coeffs for iterative implicit scheme are defined at cell faces. 
* eg flux across east face = cie(i)*T(i+1) + ciw(i)*T(i)
* converted from ocean to atmosphere 23/8/2
*
      subroutine tstipa

#include "embm.cmn"

crma extra delaration (22/12/03)
c      integer tnow

      real tv, ups, ups0, pec, diffpp, cimp, centre, dtloc

      real cie(0:maxi,0:maxj),ciw(0:maxi,0:maxj),
     +     cin(0:maxi,0:maxj),cis(0:maxi,0:maxj)
      real tq2(0:maxi+1,0:maxj+1)

c iterations to solve timestep

      integer iits, nii

c implicit
      parameter (nii=4, ups0=999, cimp=0.5)
c     parameter (nii=4, ups0=0.8, cimp=1.0)
c recover old explicit 
c     parameter (nii=8, ups0=0.0, cimp=0.0)

      integer i, j, l

      logical correct

      parameter(correct=.true. )

      dtloc = dtatm

c set b.c's on local variables

      do i=0,imax
         cin(i,0) = 0.
         cis(i,0) = 0.
         tq2(i,0) = 0.
         cin(i,jmax) = 0.
         cis(i,jmax) = 0.
         tq2(i,jmax+1) = 0.
      enddo

      do l=1,2    
         do j=1,jmax
            do i=1,imax
c flux to east
               cie(i,j) = betaz(l)*uatm(1,i,j)*rc(j)*0.5*rdphi
               diffpp = diffa(l,1,j) +
     1            (2-l)*diffmod0*max(0.0,min(1.0,
     1            (pptn(i,j)-ppmin)/(ppmax-ppmin)))

               tv = rc(j)*rc(j)*rdphi*diffpp*rdphi
c recover old explicit 
c              ups = sign(ups0, uatm(1,i,j))
               pec = betaz(l)*uatm(1,i,j)*dphi/diffpp
               ups = pec / (2.0 + abs(pec))
               ciw(i,j) = cie(i,j)*(1+ups) + tv
               cie(i,j) = cie(i,j)*(1-ups) - tv
c flux to north
               cin(i,j) = cv(j)*betam(l)*uatm(2,i,j)*0.5
               diffpp = diffa(l,2,j) +
     1            (2-l)*diffmod0*max(0.0,min(1.0,
     1            (pptn(i,j)-ppmin)/(ppmax-ppmin)))
c cv(jmax) = 0 but dsv not defined so mask needed
               if(j.lt.jmax)then
                  tv = cv(j)*cv(j)*rdsv(j)*diffa(l,2,j)
c recover old explicit 
c                 ups = sign(ups0, uatm(2,i,j))
                  pec = betam(l)*uatm(2,i,j)*dsv(j)/diffpp
                  ups = pec / (2.0 + abs(pec))
               else
                  tv = 0.
                  ups = 0.
               endif
               cis(i,j) = cin(i,j)*(1+ups) + tv
               cin(i,j) = cin(i,j)*(1-ups) - tv
            enddo
         enddo
         do j=1,jmax
            cie(0,j) = cie(imax,j)
            ciw(0,j) = ciw(imax,j)
         enddo

c iterate to solve timestep

         do iits=1,nii
            do j=1,jmax
               do i=1,imax
                  tq2(i,j) = cimp*tq(l,i,j)
     1                         + (1.0 - cimp)*tq1(l,i,j)
               enddo
            enddo
            do j=1,jmax
               tq2(0,j) = tq2(imax,j)
               tq2(imax+1,j) = tq2(1,j)
            enddo
            do j=1,jmax
               do i=1,imax
                  centre = dtloc*(ciw(i,j) - cie(i-1,j)
     1                   + (cis(i,j) - cin(i,j-1))*rds(j))
                  tq(l,i,j) = (tq1(l,i,j)*(1.0 - (1.0-cimp)
     1                        *centre) - dtloc*(-tqa(l,i,j)
     2                        + cie(i,j)  *tq2(i+1,j)
     3                        - ciw(i-1,j)*tq2(i-1,j)
     4                       + (cin(i,j)  *tq2(i,j+1)
     5                        - cis(i,j-1)*tq2(i,j-1))*rds(j)))/
     8                        (1 + cimp*centre)
               enddo
            enddo
         enddo
         if(correct)then
            do j=1,jmax
               do i=1,imax
                  tq2(i,j) = 0.5*(tq2(i,j) + cimp*tq(l,i,j)
     1                            + (1.0 - cimp)*tq1(l,i,j))
               enddo
            enddo
            do j=1,jmax
               tq2(0,j) = tq2(imax,j)
               tq2(imax+1,j) = tq2(1,j)
            enddo
            do j=1,jmax
               do i=1,imax
c
c explicit and conservative corrector step 
c
                     tq(l,i,j) =  tq1(l,i,j) - dtloc*(-tqa(l,i,j)
     1                           + cie(i,j)  *tq2(i+1,j)
     2                           - ciw(i-1,j)*tq2(i-1,j)
     3                          + (cin(i,j)  *tq2(i,j+1)
     4                           - cis(i,j-1)*tq2(i,j-1))*rds(j))
     7                                - dtloc*tq2(i,j)*(
     8                           ciw(i,j) - cie(i-1,j) 
     9                           + (cis(i,j) - cin(i,j-1))*rds(j) )
               enddo
            enddo
         endif
      enddo

c update tq1

      do j=1,jmax
         do i=1,imax
            do l=1,2
c              tv = abs(tq1(l,i,j) - tq(l,i,j))
               tq1(l,i,j) = tq(l,i,j)
            enddo
         enddo
      enddo

      end
