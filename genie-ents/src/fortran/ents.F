* ======================================================================
* ents.F
* ======================================================================
* 
* SG (20/03/2K7)
* conversion of simpleland into geniefied ents (henceforth genie-ents)

      subroutine ents(istep,
     :     nyear,
     :     torog_atm,                        !< surface air temperature adjusted for orography
     :     dum_co2_out,
     :     dum_rh0sc,dum_rhosc,dum_rsc,dum_ds,dum_dphi,
     :     dum_dsc,dum_saln0,dum_dz,dum_ec,dum_rho,
     :     dum_fx0a,dum_fx0o,dum_fxsen,dum_fxlw,
     :     dum_evap,dum_pptn,dum_relh,dum_istep0,
     :     dum_el_photo,dum_el_respveg,dum_el_respsoil,dum_el_leaf,
     :     landice_slicemask_lic,            !< land ice sheet mask
     :     albs_lnd,                         !< surface albedo
     :     land_albs_snow_lnd,               !< albedo over land for snowy conditions
     :     land_albs_nosnow_lnd,             !< albedo over land for snow-free conditions
     :     land_snow_lnd,                    !< land snow cover
     :     land_bcap_lnd,                    !< bucket capacity
     :     land_z0_lnd,                      !< roughness length (m)
     :     land_temp_lnd,                    !< land temperataure
     :     land_moisture_lnd,                !< land moisture content
     :     ntrac_atm,                        !< number of atmospheric tracer
     :     sfcatm_lnd,                       !< atmospheric tracer concentrations
     :     sfxatm_lnd                        !< land-atmosphere exchange fluxes
     :     )
     
#include "genie_ents.cmn"
#include "var_ents.cmn"
        include 'netcdf.inc'

c SG > Argument list Variable declaration
      integer istep,nyear
      real ,dimension(maxi,maxj)::torog_atm
      real ,dimension(maxi,maxj)::dum_co2_out

c SG  Variables passed from other modules
      integer dum_istep0
      integer itv
      integer iout
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
      
      real ,dimension(maxi,maxj)::dum_fx0a
      real ,dimension(maxi,maxj)::dum_fx0o
      real ,dimension(maxi,maxj)::dum_fxsen
      real ,dimension(maxi,maxj)::dum_fxlw
      real ,dimension(maxi,maxj)::dum_evap
      real ,dimension(maxi,maxj)::dum_pptn
      real ,dimension(maxi,maxj)::dum_relh
c SG <

c GHC  Variables passed from genie_global
      real ,dimension(maxi,maxj)::dum_el_photo
      real ,dimension(maxi,maxj)::dum_el_respveg
      real ,dimension(maxi,maxj)::dum_el_respsoil
      real ,dimension(maxi,maxj)::dum_el_leaf
c GHC

c land ice mask
      real,dimension(maxi,maxj),
     &     intent(in)::landice_slicemask_lic

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
      real,dimension(maxi,maxj),intent(out)::land_albs_snow_lnd
      real,dimension(maxi,maxj),intent(out)::land_albs_nosnow_lnd

c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

c bucket capacity
      real,dimension(maxi,maxj),intent(inout)::land_bcap_lnd

c roughness length
      real,dimension(maxi,maxj),intent(inout)::land_z0_lnd

c land temperature
      real,dimension(maxi,maxj),intent(inout)::land_temp_lnd

c land moisture content
      real,dimension(maxi,maxj),intent(inout)::land_moisture_lnd

c atmospheric tracer concentrations and land-atmosphere fluxes
      integer,intent(in)::ntrac_atm
      real,intent(in),dimension(ntrac_atm,maxi,maxj)::sfcatm_lnd
      real,intent(inout),
     &     dimension(ntrac_atm,maxi,maxj)::sfxatm_lnd

c Local variable declaration
      integer i,j,istot
      character filename*200

      real::asoil !< surface albedo of soil

ccccccccccccccccccccccccc FOR NETCDF
        character fname*200, label*200
        real, allocatable :: var_data(:,:,:)
        character*8, dimension(10) :: labels=(/'photo   ','respveg ',
     : 'leaf    ','respsoil','Cveg    ','Csoil   ','fv      ',
     : 'tqld1   ','tqld2   ','snow    '/)
        integer kk,myyear,var_id,vardim_id,ncid,status
        integer mymonth,myday,inistep
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

#ifndef hfoutput
c     dummy statement      
        dum_istep0 = 0
#endif

      istot=0;

      do i=1,imax
         do j=1,jmax
            bcap(i,j)=real(land_bcap_lnd(i,j),kind(bcap))
            z0(i,j)=real(land_z0_lnd(i,j),kind(z0))
            tqld(1,i,j)=real(land_temp_lnd(i,j),kind(tqld))
            tqld(2,i,j)=real(land_moisture_lnd(i,j),kind(tqld))
         enddo
      enddo
                              
c          itv = mod(istep+nyear-1,nyear)
          itv = mod(istep+nyear-1,ents_ianav)
          if(itv.lt.nyear)then
            if(istep.ge.nyear.and.itv.eq.nyear-1)then
               iout = 1
            else
               iout = 0
            endif
#ifdef icemelt
            call greenland_melt(iout)
#endif
            call entsdiagosc(nyear,istep,iout,
#ifdef hfoutput
     :           ,dum_istep0,
#endif
     :           albs_lnd,
     :           land_snow_lnd
     :           )

            call annav_diags (istep,iout,
#ifdef hfoutput
     :       dum_istep0,
#endif
     :       dum_fx0a,dum_fx0o,dum_fxsen,dum_fxlw,
     :       dum_evap,dum_pptn,dum_relh,
     :           albs_lnd,
     :           land_snow_lnd
     :           )

         endif

         if(mod(istep,ents_npstp).lt.1) call screen_diags

         if(mod(istep+istot,msimpleland).eq.0)then
c Note, the index for the atmospheric pCO2 tracer is 3
c TODO: define this index globally
            if (atchem_fert) then
               call carbon(torog_atm,sfcatm_lnd(3,:,:),
     &              landice_slicemask_lic,sfxatm_lnd(3,:,:))
            else
               call carbon(torog_atm,dum_co2_out,landice_slicemask_lic
     &              ,sfxatm_lnd(3,:,:))
         endif
         endif
         
c SG > Write ENTS restarts

         if(mod(istep,ents_iwstp).eq.0)then
           if (ents_ascout.eq.'y'.or.ents_ascout.eq.'Y') then
            print*,'Writing ENTS restart file'
            filename = trim(outdir_name)//trim(ents_out_name)//'.'
     1               //'sland'
           open(3,file=trim(filename))
c	   open(3,file='../results/'//trim(lout)//'.'//trim(ext)//
c    1             '.sland')
            rewind 3
            call out_ents(3,land_snow_lnd)
            close(3)
           endif

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
        if (ents_netout.eq.'y'.or.ents_netout.eq.'Y') then

       if (ents_netin.eq.'y'.or.ents_netin.eq.'Y') then
                inistep=int(ents_nyear*iniday/ents_yearlen)+istep
                myyear=int(inistep/ents_nyear)
                mymonth=int(12*mod(inistep,ents_nyear)/ents_nyear)
                myday=int(ents_yearlen*inistep/ents_nyear-
     :          mymonth*(ents_yearlen/12)-myyear*ents_yearlen)
        else
                myyear=int(istep/ents_nyear)
                mymonth=int(12*mod(istep,ents_nyear)/ents_nyear)
                myday=int(ents_yearlen*istep/ents_nyear-
     :          mymonth*(ents_yearlen/12)-myyear*ents_yearlen)
        endif

        if (mod(ents_iwstp,ents_nyear).eq.0) then
                fname=trim(outdir_name)//trim(ents_out_name)//'_restart_'//
     :  trim(ConvertFunc(myyear-1,10))//'_12_30.nc'
        else

                fname=trim(outdir_name)//trim(ents_out_name)//'_restart_'//
     :  trim(ConvertFunc(myyear,10))//'_'//
     :          trim(ConvertFunc(mymonth,2))//'_'//
     :          trim(ConvertFunc(myday,2))//'.nc'
        end if

        inquire(file=fname,exist=fexist)
        if (fexist.eqv..true.) then
                open(8,file=fname,status='old')
                close(8,status='delete')
        end if
ccccccccccccccccccccccccccccccccccccccccccccccc
       do kk=1,10
          allocate(var_data(1,jmax,imax))
          label=labels(kk)
          do j=1,jmax
                do i=1,imax
                select case (kk)
                        case (1)
                        var_data(1,j,i)=photo(i,j)
                        case (2)
                        var_data(1,j,i)=respveg(i,j)
                        case (3)
                        var_data(1,j,i)=leaf(i,j)
                        case (4)
                        var_data(1,j,i)=respsoil(i,j)
                        case (5)
                        var_data(1,j,i)=Cveg(i,j)
                        case (6)
                        var_data(1,j,i)=Csoil(i,j)
                        case (7)
                        var_data(1,j,i)=fv(i,j)
                        case (8)
                        var_data(1,j,i)=tqld(1,i,j)
                        case (9)
                        var_data(1,j,i)=tqld(2,i,j)
                        case (10)
                        var_data(1,j,i)=land_snow_lnd(i,j)
                end select
           enddo
       enddo

        call netcdf_ents(fname,var_data,label,myday)

        deallocate(var_data)
        enddo
cccccccccccccccccccccccccccccccccccccc
ccc	ADDING FINAL RESTART VALUE (SINGLE)
        status = nf_open(fname, nf_write, ncid)
        if (status .ne. nf_noerr) call her(status)

        status = nf_redef(ncid)
        if (status .ne. nf_noerr) call her(status)

        status=nf_def_dim(ncid,'pco2ld',1,vardim_id)
        if (status .ne. nf_noerr) call her(status)

        status=nf_def_var(ncid,'pco2ld',nf_float,1,vardim_id,var_id)
        if (status .ne. nf_noerr) call her(status)

        status=nf_put_att_text(ncid,var_id,'long_name',6,'pco2ld')
        if (status .ne. nf_noerr) call her(status)

        status=nf_enddef(ncid)
        if (status .ne. nf_noerr) call her(status)

        status=nf_put_var_double(ncid,var_id,pco2ld)
        if (status .ne. nf_noerr) call her(status)

        status=nf_close(ncid)
        if (status .ne. nf_noerr) call her(status)

cccccccccccccccccccccccccccccccccccccc
         endif
         endif

         if(mod(istep,ents_itstp).eq.0)then
           call carbt_diags (istep)
         
           call sealevel (istep,
     :       dum_rh0sc,dum_rhosc,dum_rsc,dum_ds,dum_dphi,
     :       dum_dsc,dum_saln0,dum_dz,dum_ec,dum_rho)

           call physt_diags(istep,dum_fx0a,dum_fx0o,dum_fxsen,
     :       dum_fxlw,dum_evap,dum_pptn,dum_relh,
     :          albs_lnd,
     :          land_snow_lnd)
     
         endif

c ghc > copy carbon arrays into genie_global arrays for rokgem
          do j=1,jmax
                do i=1,imax
                       dum_el_leaf(i,j) = leaf(i,j)
                       dum_el_photo(i,j) = photo(i,j)
                       dum_el_respveg(i,j) = respveg(i,j)
                       dum_el_respsoil(i,j) = respsoil(i,j)
                enddo
            enddo

c albedo
            do j=1,jmax
               do i=1,imax
                  asoil=max(apeat,((apeat-asand)*k10*Csoil(i,j)
     1                 /(k8-k9))+asand)
                  land_albs_nosnow_lnd(i,j)=(fv(i,j)*aveg)
     &                 +((1.-fv(i,j))*asoil)
                  albs_lnd(i,j)=land_albs_nosnow_lnd(i,j)
                  if (snowswitch.eq.1) then
                     land_albs_snow_lnd(i,j)=((asnow-asnowv)
     &                    *exp(-k7*Cveg(i,j)))+asnowv
                  else
                     land_albs_snow_lnd(i,j)=land_albs_nosnow_lnd(i,j)
                  endif
                  if(land_snow_lnd(i,j).eq.1.and.snowswitch.eq.1)then
                     albs_lnd(i,j)=land_albs_snow_lnd(i,j)
                  endif
                  if (ents_k1(i,j).gt.ents_kmax) then
c field capacity
                     bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))
c roughness length
                     z0(i,j)=max(0.001,kz0*Cveg(i,j))
                  endif
                  land_bcap_lnd(i,j)=real(bcap(i,j)
     &                   ,kind(land_bcap_lnd))
                  land_z0_lnd(i,j)=real(z0(i,j),kind(land_z0_lnd))
                  land_temp_lnd(i,j)=real(tqld(1,i,j)
     &                   ,kind(land_temp_lnd))
                  land_moisture_lnd(i,j)=real(tqld(2,i,j)
     &                   ,kind(land_moisture_lnd))
               enddo
            enddo

      return
      end
