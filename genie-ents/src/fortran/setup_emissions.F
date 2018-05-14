cmsw
cmsw Setup emissions scenario
cnre nre version 11/10/04
cmsw Reads in an emissions timeseries, mains interpolates and sets value to
cnre that at start of timestep, consistent with forward timestep scheme
cmsw
      subroutine setup_emissions(nsteps,maxemit,lenemit,tts,ets)

      include '../genie-cgoldstein/var.cmn'
      include '../genie-simpleland/var_ents.cmn'

      integer nsteps, i, maxemit, lenemit
     
      real tts(maxemit),ets(maxemit),conv
 
cnre put the global emissions into a single grid cell

      asurf=rsc*rsc*ds*dphi               ! grid box area (m-2)
      conv=1.e15/(12.0*asurf*syr)         ! GtC to moles C all to one cell

c read timeseries of emissions and convert

      open(2,file='../genie-simpleland/data/emissions_timeseries.dat')

      do i=1,maxemit
         read(2,*,end=10)tts(i),ets(i)
         print*,'emissions time (yr) value (GtC)',tts(i),ets(i)
         ets(i) = ets(i)*conv
         tts(i) = tts(i)*syr/tsc
      enddo
  10  lenemit = i-1
      print*,lenemit,' rows read from timeseries '
      close(2)

      if(tts(lenemit).lt.nsteps*dt(kmax)-1e-10.or.abs(tts(1)).gt.1e-10)
     &    then
         print*,nsteps*dt(kmax),tts(lenemit),
     & 'emissions must be defined from start to end of run in 
     & ../genie-simpleland/data/emissions_timeseries.dat'
         stop 
      endif

      end
