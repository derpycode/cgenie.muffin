cmsw
cmsw sealevel.f calculates change in sealevel height 
cmsw relative to a reference average density specified
cmsw in goin_ents
cnre dsc/ec(2) error corrected NRE 20/6/6
cmsw
      subroutine sealevel(istep,
     :     dum_rh0sc,dum_rhosc,dum_rsc,dum_ds,dum_dphi,
     :     dum_dsc,dum_saln0,dum_dz,dum_ec,dum_rho)

#include "genie_ents.cmn"
#include "var_ents.cmn"
      
      real vol,sumrho,avrho,mass,rho1,deltah
      real diagtime,vol1,areas
      integer pts,i,j,k,istep

c SG > Variables passed from other modules
      character filename*200
      real dum_rh0sc
      real dum_rhosc
      real dum_rsc
      real ,dimension(maxj)::dum_ds
      real dum_dphi
      real dum_dsc
      real dum_saln0
      real ,dimension(maxj)::dum_dz
      real ,dimension(4)::dum_ec
      real ,dimension(maxi,maxj,ents_kmax)::dum_rho

ccccccccccccccccccccccccc FOR NETCDF
        character fname*200, label*200
        real var_data

ccc	'diagtime1'
        character*6, dimension(2) :: labels=(/'deltah','avrho '/)
        integer kk
ccc        integer mymonth,myyear
        integer myday

        interface

         character(len=10) function ConvertFunc(innumber,flag) result(outname)
         integer::innumber, flag
         end function ConvertFunc

         subroutine netcdf_ts_ents(a,b,c,d)
          character*200 a,c
          real b
          integer d
         end subroutine netcdf_ts_ents

        end interface
cccccccccccccccccccccccc


c SG <

      diagtime=real(istep)/real(ents_nyear)

      vol=0.
      sumrho=0.
      pts=0

c SG > Open sealevel file for disgnostics
      filename = trim(outdir_name)//trim(ents_out_name)//
     1   '.'//'sealevel'
      open(44,file=trim(filename),POSITION='APPEND')
c      print*,trim(filename)
c SG <

cmsw Sealevel rise due to thermal expansion

      do i=1,imax
         do j=1,jmax
            do k=1,ents_kmax
               if(k.ge.ents_k1(i,j))then
cnre dsc error   rho1=(rho(i,j,k)*1.18376)+1000.+(34.9*0.7968)
cnre dsc error   vol1=rsc*rsc*dphi*ds*dz(k)*5000.
                 rho1=(dum_rho(i,j,k) + dum_saln0*dum_ec(2))*dum_rhosc
     1            + dum_rh0sc
                 vol1=dum_rsc*dum_rsc*dum_dphi*dum_ds(1)*dum_dz(k)
     1           *dum_dsc
                 vol=vol+vol1
                 sumrho=sumrho+(rho1*vol1)
               endif
            enddo
            if(ents_k1(i,j).le.ents_kmax)then
               pts=pts+1
            endif
         enddo
      enddo

      avrho=sumrho/vol
      mass=sumrho

      areas=dum_rsc*dum_rsc*dum_dphi*dum_ds(1)*pts
 
      deltah=(vol/areas)*((rhoref/avrho)-1.)

#ifdef icemelt

cmsw If Greenland melt added to ocean then correct ocean density

      deltah=(1./areas)*((((rhoref*vol)+(rho0*areas*isslfwf))/avrho)
     1           -vol)

cmsw Calculate change in sealevel due to thermal expansion only

      deltah=deltah-isslfwf

      write(44,'(6e24.16)')diagtime,deltah,avrho,
     &                       issl,glairt,deltah+issl
#else
      write(44,'(3e24.16)')diagtime,deltah,avrho
#endif

      close (44)

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
ccc	myyear=int(istep/ents_nyear)+1
ccc	mymonth=int(12*mod(istep,ents_nyear)/ents_nyear)
ccc	myday=int(360*istep/ents_nyear-mymonth*30-(myyear-1)*360)
        myday=int(360*istep/ents_nyear)

        fname=trim(outdir_name)//trim(ents_out_name)//'_TS.nc'
cccccccccccccccccccccc 
       do kk=1,2
          label=labels(kk)
                select case (kk)
                        case (1)
                        var_data=deltah
                        case (2)
                        var_data=avrho
                end select
        call netcdf_ts_ents(fname,var_data,label,myday)

        enddo
cccccccccccccccccccccccccccccccc

      end
