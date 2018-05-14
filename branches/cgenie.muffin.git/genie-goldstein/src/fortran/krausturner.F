c krausturner.f
c Simplest Kraus-Turner mixed layer scheme, carried out after
c buoyancy-driven convection. The PE release from convection is
c calculated in co.f (to be multiplied by an efficiency here).
c The KE input from wind is in the field kewind, pre-multiplied
c by an efficiency, which is not a function of mixed layer depth
c (mld). Internal KE sources are neglected. It is not necessary
c to determine mld, because the scheme works by partially mixing
c the last layer. However mld is output each time-step as a
c diagnostic for use by (e.g.) biogem. KICO 18/09/07
c Inputs:
c   tv (ts) - tracers including T and S
c   empe    - pe energy input
c   emke    - ke energy input
c   mldpk   - deepest layer to be mixed from surface by buoyancy
c             forcing alone
c Outputs:
c   tv (ts) - tracers including T and S
c   mldt    - mixed layer depth (-ve value) for use by biogem
c   mldkt   - index of layer containing mld
c
      subroutine krausturner(tv,pebuoy,ketau,mldpk,mldt,mldkt,tvkl)

#include "ocean.cmn"

      real em, empe, emke, eneed, emr
      real smix, tmix, qmix(1:maxl), tv(maxl,1,1,1:maxk)
      real rhou, rhol, rhomix
      real mldt, mlda, mldb, mldtadd
      real pebuoy, ketau

      integer mldkt, mldpk, k, l, n, partmix, tvkl 

c KICO delete following line (more efficient) once mldpk calc sorted
      mldpk=kmax
c initialise variables
      k=mldpk
      empe = pebuoy
      emke = ketau*mlddec(k)
      em = empe + emke
      eneed=0.
      tmix=tv(1,1,1,k)
      smix=tv(2,1,1,k)
      do l=1,lmax
        qmix(l) = tv(l,1,1,k)
      enddo
      call eos(ec,tmix,smix,zw(k),ieos,
     1   rhomix)
      partmix=1
c find layer to be only partially mixed
      do while((eneed.lt.em).AND.(em.gt.0))
         if (k.lt.mldpk)then
c apply mixing from last step if complete
           qmix(1)=tmix
           qmix(2)=smix
           do l=3,lmax
             qmix(l) = rdzg(kmax,k)*(qmix(l)*dzg(kmax,k+1)
     1             + tv(l,1,1,k)*dz(k))
           enddo
         endif
         if (k.eq.tvkl)then
c Mixed layer reaches bottom
           mldkt = k
           mldt = zw(k-1)
           partmix=0
           em=-1.d-8
           if (k.lt.kmax) then
           do l=1,lmax
             do n=k,kmax
               tv(l,1,1,n)=qmix(l)
             enddo
           enddo
           endif
         else
c Try to completely mix next layer
           k = k-1
           emr = (em - eneed)/em
           empe = empe*emr
           emke = emke*emr*mlddecd(k)
           em = empe + emke
c T and S and rho
           if(ieos.ne.0)then
             call eos(ec,tmix,smix,zw(k),ieos,
     1          rhou)
           else
             rhou = rhomix
           endif
           tmix = rdzg(kmax,k)*(tmix*dzg(kmax,k+1)
     1           + tv(1,1,1,k)*dz(k)) 
           smix = rdzg(kmax,k)*(smix*dzg(kmax,k+1)
     1           + tv(2,1,1,k)*dz(k))
           call eos(ec,tv(1,1,1,k),tv(2,1,1,k),zw(k),ieos,
     1        rhol)
           call eos(ec,tmix,smix,zw(k),ieos,
     1        rhomix)
c Energy needed to completely mix layer
           eneed = z2dzg(kmax,k+1)*rhou+z2dzg(k,k)*rhol -
     1            z2dzg(kmax,k)*rhomix
         endif
      enddo

      if((partmix.eq.1).AND.(em.gt.0))then
c Partially mix next layer. This scheme is taken from Unified
c Model documentation paper No41. Ocean model mixed layer
c formulation (S. J. Foreman, 17 Sept 1990). Model Vers <2.0.
c (24) is the key equation, but it should read
c a=(h_(n-1)/h_n)*(M/E_mix).
        mldkt=k
        mlda=dzg(kmax,k+1)*rdzg(kmax,k)*em/eneed
        mldb=(dzg(kmax,k)*rdzg(kmax,k+1)-1)*mlda
        do l=1,lmax
          tv(l,1,1,kmax)=(1-mldb)*qmix(l)+mldb*tv(l,1,1,k)
          do n=k+1,kmax-1
            tv(l,1,1,n)=tv(l,1,1,kmax)
          enddo
          tv(l,1,1,k)=mlda*qmix(l)+(1-mlda)*tv(l,1,1,k)
        enddo
c Actual mixed layer depth. The key equation is (26), but 
c again the eqn is wrong (see above ref). It should read
c d = 2M / (g h_(n-1) (rho_n - rho_mix) )
        if(k.lt.kmax)then
          mldtadd=em/(zw(k)*(rhol-rhou))
c          if(-mldtadd.gt.dzg(k,k))then
c            mldtadd=-dzg(k,k)
cc Shouldn't happen, delete this line when confident it doesn't
c          print*,'Warning: inconsistent result in mixed layer
c     1 scheme partmixed layer: see krausturner.f'
c          endif
          mldt=zw(k)+mldtadd
        else
c Shouldn't happen, delete this line when confident it doesn't
          mldt=0.
          print*,'Warning: inconsistent result in mixed layer
     1 scheme surface layer partmixed: see krausturner.f'
        endif
      elseif((partmix.eq.1).AND.(k.lt.kmax))then
        mldkt=k+1
        mldt=zw(k) 
      endif 
       
      end
