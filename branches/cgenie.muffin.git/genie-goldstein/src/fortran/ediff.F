c ediff.F
c KICO 06/07/08. Scheme for stratification-dependent
c and/or spatially variable diapycnal mixing. The standard
c option of horizontally uniform diapycnal mixing can
c be recovered (except for rounding differences), by iediff=1,
c ediff0=diff(2), or by iediff=1, ediffpow1=0.0, ediffpow2=0.0,
c ediffvar=0.0
c
c If iediff=1, a very simple stratification-dependent mixing
c scheme is applied:
c
c ediff1p = ediff0 +
c   (diff(2)-ediff0)*ediffk0^ediffpow1*(dzrho_lev/dzrho)^ediffpow2
c
c where ediff is implemented diffusivity; ediff0 (tunable)
c is "minimum diffusivity" applied everywhere; ediffk0 is a
c reference diffusivity profile, exponentially growing with
c depth and equal to 1 at a depth of 2500 m;
c ediffpow1 (tunable) effectively modifies the efolding
c scale - ediffpow1=0 yields vertically uniform ediff for the
c "standard" density profile, ediffpow1=1 yields an e-folding
c scale of 700 m (but effectively slightly greater due to
c limitation at high diffusivities); dzrho is density gradient;
c dzrho_lev is a density gradient profile obtained by fitting an
c exponential decay profile to globally averaged Levitus data,
c ediffpow2 (tunable) is the stratification dependent mixing
c paramter - ediffpow2=0 yields no stratification dependence,
c ediffpow2=1 yields constant PE production in diapycnal mixing
c (excluding background mixing in ediff0 and diapycnal leakage).
c
c For all values of ediff0, ediffpow1, and ediffpow2, diff(2) is
c the diffusivity at 2500 m when dzrho=dzrho_lev.
c
c Additional spatial variability may be added by creating an
c imax*jmax*(kmax-1) file ediffvargrid.dat. If ediffvar is
c different from zero, ediff is then modified according to:
c
c ediff1=ediff0+(ediff1p-ediff0)*(1+ediffvar*(ediffvargrid-1))
c
c Note that there is a maximum threshold value of ediff applied
c in tstepo.F, needed because dzrho can be small or zero.
c
c iediff=2 is identical to iediff=1, except that ediffk0 is
c a Bryan & Lewis profile (where diffusivity at a positive
c infinite depth is ediff0 and diffusivity at 2500 m is
c diff(2)). Not recommended.
c
c Future mixing schemes may be added using iediff=3 etc....

      subroutine ediff

#include "ocean.cmn"

      real ediff10, dzrho_lev(kmax), ediffk0(kmax), ediff1p(kmax)
      real ediffvargrid(imax,jmax,kmax)
      real ediffvartemp(imax)
      real ediffklim

      integer i, j, k

      if((iediff.gt.0).AND.(iediff.lt.3))then

c ediff0 units are initially m^2/s, other paramters are dimensionless
c convert ediff0 to dimensionless number
        ediff0=ediff0*rsc/(usc*dsc*dsc)

        ediff10=diff(2)-ediff0
        if(ediff10.lt.0.0)then
          print*,'****Warning: risk of negative diffusivities, increase
     1       diff(2) or decrease ediff0****'
        endif
     
c Create Levitus global mean density profile, dzrho_lev
        do k=1,kmax-1
          dzrho_lev(k)=(-5.5e-3/rhosc*dsc)
     1                   *exp(zw(k)*(dsc/650.0))
        enddo

c Create standard mixing profile
        if(iediff.eq.1)then
c Use exponential growth profile, roughly consistent with observations
c (see papers by e.g. Polzin, Naveira Garabato).
          do k=1,kmax-1
            ediffk0(k)=exp(-(zw(k)+2500.0/dsc)*(dsc/700.0))
c 08/07/08 However, the profile is limited to a maximum that is
c 1/ediffklim=3 times greater than the diffusivity at 2500 m.
c Note that this limit is raised to the power of ediffpow1.
            ediffklim=1/3.0e0
            ediffk0(k)=1/((1-ediffklim)/ediffk0(k)+ediffklim) 
          enddo
        elseif(iediff.eq.2)then
c Use Bryan & Lewis (1979) type profile instead. Not recommended. The
c exact profile used by Bryan & Lewis is recovered using
c diff(2)=0.8e-4 and ediff0=0.275e-4, with ediffpow2 and ediffvar set
c to zero and ediffpow1=1.
          do k=1,kmax-1
            ediffk0(k)=1+(2/pi)*atan(-(zw(k)+2500.0/dsc)*(4.5e-3*dsc))
          enddo
        endif

c Create precursor (before any manual mods from ediffvargrid)
c of profile to be used in tstepo.F
        do k=1,kmax-1
          ediff1p(k)=ediff10*(ediffk0(k)**ediffpow1)
     1                   *((-dzrho_lev(k))**ediffpow2)
        enddo

c Make any manual mods from ediffvargrid
        if((ediffvar.lt.-1e-7).OR.(ediffvar.gt.1e-7))then
c Read in var file. The default file is a copy of ediffvar_36_36_08.dat;
c to use files for different grids or with different variation, copy
c (e.g.) ~/genie/genie-goldstein/data/input/ediffvar_36_36_16.dat to
c ~/genie/genie-goldstein/data/input/ediffvargrid.dat, or create new
c file.
          open(unit=87,file=indir_name(1:lenin)//'ediffvargrid.dat'
     1            ,status='old')
          do k=1,kmax-1
            do j=1,jmax
              read(87,*) ediffvartemp
              do i=1,imax
                ediffvargrid(i,j,k)=ediffvartemp(i)
              enddo
            enddo
          enddo
          close(unit=87)
c Apply mods
          do i=1,imax
            do j=1,jmax
              do k=1,kmax-1
                ediff1(i,j,k)=ediff1p(k)*(1+
     1               ediffvar*(ediffvargrid(i,j,k)-1))
              enddo
            enddo
          enddo
        else
c No mods needed....
          do i=1,imax
            do j=1,jmax
              do k=1,kmax-1
                ediff1(i,j,k)=ediff1p(k)
              enddo
            enddo
          enddo
        endif

c Make code more efficient by checking whether ediffpow2 is
c very close to 0, 1/2, or 1. If so, tstepo.F will not need to
c raise 1/dzrho to the power of a real number
        if((ediffpow2.gt.-1e-7).AND.(ediffpow2.lt.1e-7))then
          ediffpow2i=0
        elseif((ediffpow2.gt.(1.0-1e-7)).AND.
     1           (ediffpow2.lt.(1.0+1e-7)))then
          ediffpow2i=1
        elseif((ediffpow2.gt.(0.5-1e-7)).AND.
     1           (ediffpow2.lt.(0.5+1e-7)))then 
          ediffpow2i=2
        else
c In this eventuality, every w point at every timestep will
c involve raising 1/dzrho to the power of a real number
          ediffpow2i=-999
        endif
        
c Set up grid of maximum diffusivity (theoretical limit is
c 0.125*dz^2/dt). Note that this means max diffusivity will be
c a function of the number of vertical levels.
        do k=1,kmax
          diffmax(k)=0.5*0.125*dz(k)*dz(k)/dt(k)
        enddo
      endif
      
      end
