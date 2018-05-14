c SG File created from diagosc.F for ENTS diagnostics
c SG Comments below are from original diagosc.F file
c
c diagosc.f extra diagnostic routine for c-goldstein v2 with seasonal cycle
c calculate average over nyear timesteps. Extra diagnostics to be included
c WHERE INDICATED 
c file created 18/6/3 Neil Edwards
c
      subroutine entsdiagosc(nyear,istep,iout,
#ifdef hfoutput
     &     istep0,
#endif
     :     albs_lnd,                         !< surface albedo
     :     land_snow_lnd                     !< land snow cover
     :     )
c #ifdef dosc
#include "genie_ents.cmn"
#include "var_ents.cmn"

#ifdef hfoutput
      character datestring*10
#endif

ccccc FOR NETCDF
        include 'netcdf.inc'
ccccc

      real rnyear

      integer istep,i,j,iout
c     integer l
#ifdef hfoutput
      integer istep0
#endif
      integer nyear

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

ccccccccccccccccccccccccc FOR ENTS NETCDF
        character fname*200, label*200
        real, allocatable :: var_data(:,:,:)
c        character :: outname*6
        integer myyear,mymonth,myday
        logical fexist

        interface

         character(len=10) function ConvertFunc(innumber,flag) result(outname)
         integer::innumber, flag
         end function ConvertFunc

         subroutine netcdf_ents(a,b,c,d)
          character*200 a,c
          real b(:,:,:)
          integer d
         end subroutine netcdf_ents

        end interface
cccccccccccccccccccccccc

      if (dosc) then
      rnyear = 1.0/nyear

      do j=1,jmax
         do i=1,imax
c            do l=1,2
c               tqavg(l,i,j) = tqavg(l,i,j) + tq(l,i,j)*rnyear
c               haavg(l,i,j) = haavg(l,i,j) + varice(l,i,j)*rnyear
c            enddo
c            relhavg(i,j) = relhavg(i,j) + relh(i,j)*rnyear
c            ticeavg(i,j) = ticeavg(i,j) + tice(i,j)*rnyear
            tqldavg(1,i,j) = tqldavg(1,i,j) + (tqld(1,i,j)
     1                       *rnyear)
            tqldavg(2,i,j) = tqldavg(2,i,j) + (tqld(2,i,j)
     1                       *rnyear)
            snowavg(i,j) = snowavg(i,j)
     &           + (real(land_snow_lnd(i,j))*rnyear)
            albsavg(i,j) = albsavg(i,j) + (albs_lnd(i,j)*rnyear)
c            palbavg(i,j) = palbavg(i,j) + (palb(i,j)*rnyear)
c            evapavg(i,j) = evapavg(i,j) + (evap(i,j)*rnyear)
c            pptnavg(i,j) = pptnavg(i,j) + (pptn(i,j)*rnyear)
c            runavg(i,j)  = runavg(i,j)  + (runoff(i,j)*rnyear)
            bcapavg(i,j) = bcapavg(i,j) + (bcap(i,j)*rnyear)
            z0avg(i,j)   = z0avg(i,j)   + (z0(i,j)*rnyear)
c            fxavg(1,i,j) = fxavg(1,i,j) + (fxsw(i,j)*rnyear)
c            fxavg(2,i,j) = fxavg(2,i,j) + (fxplw(i,j)*rnyear)
c            fxavg(3,i,j) = fxavg(3,i,j) + (fxlw(i,j)*rnyear)
c            fxavg(4,i,j) = fxavg(4,i,j) + (fxsen(i,j)*rnyear)
c            fxavg(5,i,j) = fxavg(5,i,j) + (fxlata(i,j)*rnyear)
c            fxavg(6,i,j) = fxavg(6,i,j) + (fx0a(i,j)*rnyear)
c            fxavg(7,i,j) = fxavg(7,i,j) + (fx0o(i,j)*rnyear)
         enddo
      enddo
      

      if((iout.eq.1).and.(mod(istep,ents_ianav).eq.0
     &   .and.(istep.ge.ents_ianav)))then
         print*,'writing averaged data at istep ',istep
c
c write averaged data (a near-copy of outm.f) not a restart
c as such, therefore can write less accurate, more economical output
c
c dynamic tracers and velocity only
c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(1,file=trim(outdir_name)//trim(ents_out_name)//
c     1       '.hfavg_'//datestring)
c#else
c         open(1,file=trim(outdir_name)//trim(ents_out_name)
c     1       //'.avg')
c#endif
cc EMBM
c         do j=1,jmax
c            do i=1,imax
c               do l=1,2
c                  write(1,10)tqavg(l,i,j)
c               enddo
ccmsw Calculate global mean air temperature
c               gmairttot=gmairttot+tqavg(1,i,j)
c            enddo
c         enddo
cc sea ice
c         do j=1,jmax
c            do i=1,imax
c               do l=1,2
c                  write(1,10)haavg(l,i,j)
c               enddo
c            enddo
c         enddo
c         do j=1,jmax
c            do i=1,imax
c               write(1,10)ticeavg(i,j)
c            enddo
c         enddo
c
c         close(1)
#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     :         //'.hfltavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.ltavg')
#endif

ccccccccccccccccc  FOR NETCDF
ccc	if (mod(istep,ents_ianav).eq.0.and.(istep.ge.ents_ianav)) then
ccc	mytime=int(istep/ents_ianav)*istep/ents_ianav

        myyear=int(istep/ents_nyear)
        mymonth=int(12*mod(istep,ents_nyear)/ents_nyear)
        myday=int(360*istep/ents_nyear-mymonth*30-(myyear-1)*360)

        fname=trim(outdir_name)//trim(ents_out_name)//'_yearav_'//
     : trim(ConvertFunc(myyear,10))//'.nc'
ccc     : trim(ConvertFunc(myyear,10))//'_2Davg.nc'

        inquire(file=fname,exist=fexist)
        if (fexist.eqv..true.) then
                open(8,file=fname,status='old')
                close(8,status='delete')
        end if
cccccccccccccccccccccccccccccccccc
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)tqldavg(1,i,j)
            var_data(1,j,i)=tqldavg(1,i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='ltavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccc	end if
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     1         //'.hflqavg_'//datestring)
#else
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     1         //'.lqavg')
#endif

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)tqldavg(2,i,j)
            var_data(1,j,i)=tqldavg(2,i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
        close(15)
        label='lqavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
cccccccccccccccccccccccccccccccccccccc

c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1        //'.hffluxavg_'//datestring)
c#else
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1        //'.fluxavg')
c#endif
c         do l=1,7
c            do j=1,jmax
c               do i=1,imax
c                  write(15,10)fxavg(l,i,j)
c               enddo
c            enddo
c         enddo
c         close(15)
c
#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1        //'.hfsnowavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1        //'.snowavg')
#endif
ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)snowavg(i,j)
            var_data(1,j,i)=snowavg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='snowavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1       //'.hfz0avg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1       //'.z0avg')
#endif
ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)z0avg(i,j)
            var_data(1,j,i)=z0avg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='z0avg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file=trim(outdir_name)//trim(ents_out_name)
     1         //'.hfalbsavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.albsavg')
#endif
ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)albsavg(i,j)
            var_data(1,j,i)=albsavg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='albsavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.hfpalbavg_'//datestring)
c#else
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.palbavg')
c#endif
c         do j=1,jmax
c            do i=1,imax
c               write(15,10)palbavg(i,j)
c            enddo
c         enddo
c         close(15)
c
c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1        //'.hfrelhavg_'//datestring)
c#else
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1        //'.relhavg')
c#endif
c         do j=1,jmax
c            do i=1,imax
c               write(15,10)relhavg(i,j)
c            enddo
c         enddo
c         close(15)
c
c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.hfpptnavg_'//datestring)
c#else
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.pptnavg')
c#endif
c         do j=1,jmax
c            do i=1,imax
c               write(15,10)pptnavg(i,j)
c            enddo
c         enddo
c         close(15)
c
c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.hfrunavg_'//datestring)
c#else
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.runavg')
c#endif
c         do j=1,jmax
c            do i=1,imax
c               write(15,10)runavg(i,j)
c            enddo
c         enddo
c         close(15)
c
c#ifdef hfoutput
c         write(datestring,'(i10.10)') istep+istep0
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.hfevapavg_'//datestring)
c#else
c         open(15,file= trim(outdir_name)//trim(ents_out_name)
c     1         //'.evaplavg')
c#endif
c         do j=1,jmax
c            do i=1,imax
c               write(15,10)evapavg(i,j)
c            enddo
c         enddo
c         close(15)
c
#ifdef hfoutput
         write(datestring,'(i10.10)') istep+istep0
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.hfbcapavg_'//datestring)
#else
         open(15,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.bcapavg')
#endif

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        allocate(var_data(1,jmax,imax))
         do j=1,jmax
            do i=1,imax
cccccccccccccc OLD LINE
               write(15,10)bcapavg(i,j)
            var_data(1,j,i)=bcapavg(i,j)
            enddo
         enddo
cccccccccccccc  OLD LINE
         close(15)
        label='bcapavg'

        call netcdf_ents(fname,var_data,label,myday)
        deallocate(var_data)
ccccccccccccccccccccccccccccccccccccccc

         open(45,file= trim(outdir_name)//trim(ents_out_name)
     1         //'.'//'gmairt')
         write(45,'(2e18.7)')real(istep/nyear)-0.5,
     1                       gmairttot/(real(imax*jmax))

c 	CHECK THIS VARIABLE!!!!!!!!!!!!!

c 
c perform diagnostics on averaged data, either by rewriting other diag 
c routines to accept data as argument, or by simply copying code,
c otherwise diagnose by integrating one (short) step from .avg file.
c
c diagnostic code to be inserted here
c

            print*,'resetting averaged data arrays at step',istep
            do j=1,jmax
               do i=1,imax
c                  do l=1,2
c                     tqavg(l,i,j) = 0.
c                     haavg(l,i,j) = 0. 
c                  enddo
c                  relhavg(i,j) = 0.
c                  ticeavg(i,j) = 0.
                  tqldavg(1,i,j) = 0.
                  tqldavg(2,i,j) = 0.
                  snowavg(i,j) = 0.
                  albsavg(i,j) = 0.
c                  palbavg(i,j) = 0.
c                  pptnavg(i,j) = 0.
c                  runavg(i,j)  = 0.
                  bcapavg(i,j) = 0.
                  z0avg(i,j)   = 0.
c                  evapavg(i,j) = 0.
c                  do l=1,7
c                     fxavg(l,i,j) = 0.
c                  enddo
c                  gmairttot = 0.
               enddo
            enddo
      endif
c #endif
      endif
  10  format(e14.7)
      end
