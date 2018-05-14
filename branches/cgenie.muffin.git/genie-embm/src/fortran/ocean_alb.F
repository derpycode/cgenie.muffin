cmsw
cmsw Used to calculate mean daily ocean albedo zenith angle dependence
cmsw Calculation performed at initialisation stage for every latitude and
cmsw istep.
cmsw
      subroutine ocean_alb(oscss,osccc,oscday,j,istep)
     
c      include '../genie-cgoldstein/var.cmn'
c      include '../genie-simpleland/var_ents.cmn'
#include "embm.cmn"
 
      real oscss,osccc,oscday
      real h
      real a,b,sum,tol

      real old,int,radout
      real xp

      integer n,p,j,x,istep

      parameter(tol=1.e-5)

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      sum = 0.0
      p = 0
c ------------------------------------------------------------ c

cmsw Integration loop (adaptive extended trapezium rule)
cmsw returns the integrated value when converges to a
cmsw specified tolerance. See Numerical Recipes Ch. 4

cmsw Initial values
      old=-1.e30
      int=1.e30
cmsw Integration limits     
      a=0.
      b=oscday

      if(b.le.0.)then
         albo(j,istep)=1.
      else
         do n=1,15 
cmsw Initial guess based on end points
            if(n.eq.1)then
               p=1
               call rad_out(radout,a,oscss,osccc)
               sum=0.5*radout
               call rad_out(radout,b,oscss,osccc)
               sum=sum+(0.5*radout)
               old=(b-a)*sum
            else
               old=int
            endif
cmsw Interval size doubles with each iteration
            h=(b-a)/(2*p) 
cmsw Calculate for new points only then add to the running sum
            do x=1,p
               xp=a+h+(2*h*(x-1))
               call rad_out(radout,xp,oscss,osccc)
               sum=sum+radout
            enddo
cmsw Calculate new value of integral
            int=h*sum
cmsw Check tolerance
            if(abs(int-old).lt.tol*abs(old)) exit
cmsw Double number of points to evaluate
            p=2*p
         enddo

cmsw Work out ocean albedo (outgoing radiation/incoming radiation)
         albo(j,istep)=int/(oscss*(oscday-tan(oscday)))
      endif

      end
c*************************************************************
cmsw Function to work out instantaneous outgoing radiation   *
c*************************************************************
      subroutine rad_out(radout,h,oscss,osccc)
      
      real oscss,osccc
      real czsol,h,rspec,rtot,radout
      real rdiff

      parameter(rdiff=0.06)

cmsw Cosine of zenith angle
      czsol=oscss+(osccc*cos(h))
      if(czsol.gt.1.)then
         czsol=1.
      endif
      if(czsol.lt.0.)then
         czsol=0.
      endif

cmsw specular reflectance according to Briegleb et al. (1986)
      rspec=(0.026/((czsol**1.7)+0.065))+
     &        (0.15*(czsol-0.1)*(czsol-0.5)*(czsol-1.0))

cmsw total reflectance
      rtot=rspec+rdiff
      
cmsw Instantaneous outgoing radiation (but without the constants)
      radout=rtot*czsol

      end

