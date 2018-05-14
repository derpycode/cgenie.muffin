cmsw
cmsw Makes a file averaged over the last
cmsw year for ENTS model. 
cmsw Files written to every ianav timesteps.
cmsw
      subroutine annav_diags(istep,iout,
#ifdef hfoutput
     : dum_istep0,
#endif
     : dum_fx0a,dum_fx0o,dum_fxsen,dum_fxlw,
     : dum_evap,dum_pptn,dum_relh,
     :     albs_lnd,                         !< surface albedo
     :     land_snow_lnd                     !< land snow cover
     :     )
c #ifdef dosc

#include "genie_ents.cmn"
#include "var_ents.cmn"

ccccc FOR NETCDF
        include 'netcdf.inc'
ccccc

      integer l,i,j,istep,iout,yearosc
#ifdef biogem
      integer k
#endif

      real rnyear
#ifdef biogem
      real vol,dens,mass_c
#endif
      real cfavg(8,maxi,maxj),sumavg(8),avgsl(8)
      real Gtatm,Gtocn
      real pfavg(13,maxi,maxj),sumavg2(13),avgsl2(13)
c SG > Variables for geniefication
#ifdef hfoutput
      integer dum_istep0
#endif
      character filename*200
      real ,dimension(maxi,maxj)::dum_fx0a
      real ,dimension(maxi,maxj)::dum_fx0o
      real ,dimension(maxi,maxj)::dum_fxsen
      real ,dimension(maxi,maxj)::dum_fxlw
      real ,dimension(maxi,maxj)::dum_evap
      real ,dimension(maxi,maxj)::dum_pptn
      real ,dimension(maxi,maxj)::dum_relh
c SG <

#ifdef hfoutput
      character datestring*10
#endif

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

ccccccccccccccccccccccccc FOR NETCDF
        character fname*200, label*200, refname*200
        real, allocatable :: var_data(:,:,:)

        character*10, dimension(8) :: labels=(/'sphoto   ','srespveg ',
     : 'sleaf    ',
     : 'srespsoil','sCveg    ','sCsoil   ','sfv      ','sepsv    '/)

        character*10, dimension(9) :: avlabels=(/'avgsl1','avgsl2','avgsl3',
     :  'avgsl4','avgsl5','avgsl6','avgsl7','avgsl8','Gtatm2'/)

        character*10, dimension(13) :: avlabels2=(/'1avgs1  ','2avgs2  ',
     : '3avgs3  ','4avgs4  ','5avgs5  ','6avgs6  ','7avgs7  ',
     : '8avgs8  ',
     : '9avgs9  ','10avgs10','11avgs11','12avgs12','13avgs13'/)

        integer kk, myyear,ncid,var_id,status,myday,timedim_id
        logical fexist
        real var_data1

        interface

         character(len=10) function ConvertFunc(innumber,flag) result(outname)
         integer::innumber, flag
         end function ConvertFunc

         subroutine netcdf_ents(a,b,c,d)
          character*200 a,c
          real b(:,:,:)
          integer d
         end subroutine netcdf_ents

         subroutine netcdf_ts_ents(a,b,c,d)
          character*200 a,c
          real b
          integer d
         end subroutine netcdf_ts_ents

        end interface
ccccccccccccccccccccccccccccc

      if (dosc) then
c SG > Files are opened locally
       filename = trim(outdir_name)//trim(ents_out_name)//'.slavgt'
       open(46,file=trim(filename),POSITION='APPEND')
       filename = trim(outdir_name)//trim(ents_out_name)//'.pslavgt'
       open(47,file=trim(filename),POSITION='APPEND')
c SG <

cmsw Sum up quantities since last .avg calculation

      pco2ld_tot=pco2ld_tot+pco2ld 
      do i=1,imax
         do j=1,jmax
            if(ents_k1(i,j).gt.ents_kmax)then
cmsw Carbon diagnostics
               sphoto(i,j)=sphoto(i,j)+photo(i,j)
               srveg(i,j)=srveg(i,j)+respveg(i,j)
               sleaf(i,j)=sleaf(i,j)+leaf(i,j)
               srsoil(i,j)=srsoil(i,j)+respsoil(i,j)

               sCveg1(i,j)=sCveg1(i,j)+Cveg(i,j)
               sCsoil1(i,j)=sCsoil1(i,j)+Csoil(i,j)
               sfv1(i,j)=sfv1(i,j)+fv(i,j)
               sepsv1(i,j)=sepsv1(i,j)+epsv(i,j)
cmsw Physical diagnostics
cmsw temp and water
               stqld(1,i,j)=stqld(1,i,j)+tqld(1,i,j)
               stqld(2,i,j)=stqld(2,i,j)+tqld(2,i,j)
cmsw heat fluxes
               sfx0a(i,j)=sfx0a(i,j)+dum_fx0a(i,j)
               sfx0o(i,j)=sfx0o(i,j)+dum_fx0o(i,j)
               sfxsens(i,j)=sfxsens(i,j)+dum_fxsen(i,j)
               sfxlw(i,j)=sfxlw(i,j)+dum_fxlw(i,j)
cmsw water fluxes
               sevap(i,j)=sevap(i,j)+dum_evap(i,j)
               spptn(i,j)=spptn(i,j)+dum_pptn(i,j)
               srelh(i,j)=srelh(i,j)+dum_relh(i,j)
cmsw other quantities
               sbcap(i,j)=sbcap(i,j)+bcap(i,j)
               salbs(i,j)=salbs(i,j)+albs_lnd(i,j)
               ssnow(i,j)=ssnow(i,j)+land_snow_lnd(i,j)
               sz0(i,j)=sz0(i,j)+z0(i,j)
            endif
#ifdef biogem
cmsw Ocean stuff
             do k=1,ents_kmax
                if(k.ge.ents_k1(i,j))then
cmsw Volume of ocean box in m^3
                  vol=asurfrea*dz(k)*5000.
cmsw Density of water in kg/m^3
                  dens=(rho(i,j,k)*1.18376)+1000.+(34.9*0.7968)
cmsw Mass of DIC in kg in ocean box
                  mass_c=ts(3,i,j,k)*vol*dens*mu
                  tot_mass_ocn_c=tot_mass_ocn_c+mass_c
                endif
             enddo
#endif
          enddo 
       enddo

cmsw If istep divisable by ianav then write
cmsw .sland.avg file

       rnyear=1./ents_nyear
         
       if(iout.eq.1)then

         pco2ld_tot=pco2ld_tot+pco2ld

         do l=1,8
            sumavg(l)=0.
         enddo

         do l=1,13
            sumavg2(l)=0.
         enddo


         do i=1,imax
            do j=1,jmax
               if(ents_k1(i,j).gt.ents_kmax)then
cmsw Average over time
                 cfavg(1,i,j)=sphoto(i,j)*rnyear
                 cfavg(2,i,j)=srveg(i,j)*rnyear
                 cfavg(3,i,j)=sleaf(i,j)*rnyear
                 cfavg(4,i,j)=srsoil(i,j)*rnyear

                 cfavg(5,i,j)=sCveg1(i,j)*rnyear
                 cfavg(6,i,j)=sCsoil1(i,j)*rnyear
                 cfavg(7,i,j)=sfv1(i,j)*rnyear
                 cfavg(8,i,j)=sepsv1(i,j)*rnyear

                 pfavg(1,i,j)=stqld(1,i,j)*rnyear
                 pfavg(2,i,j)=stqld(2,i,j)*rnyear

                 pfavg(3,i,j)=sfx0a(i,j)*rnyear
                 pfavg(4,i,j)=sfx0o(i,j)*rnyear
                 pfavg(5,i,j)=sfxsens(i,j)*rnyear
                 pfavg(6,i,j)=sfxlw(i,j)*rnyear

                 pfavg(7,i,j)=sevap(i,j)*rnyear
                 pfavg(8,i,j)=spptn(i,j)*rnyear
                 pfavg(9,i,j)=srelh(i,j)*rnyear

                 pfavg(10,i,j)=sbcap(i,j)*rnyear
                 pfavg(11,i,j)=salbs(i,j)*rnyear
                 pfavg(12,i,j)=ssnow(i,j)*rnyear
                 pfavg(13,i,j)=sz0(i,j)*rnyear

cmsw Add code for summed annual average timeseries
cmsw Land fluxes
                 sumavg(1)=sumavg(1)+cfavg(1,i,j)
                 sumavg(2)=sumavg(2)+cfavg(2,i,j)
                 sumavg(3)=sumavg(3)+cfavg(3,i,j)
                 sumavg(4)=sumavg(4)+cfavg(4,i,j)
cmsw Carbon reservoirs
                 sumavg(5)=sumavg(5)+cfavg(5,i,j)
                 sumavg(6)=sumavg(6)+cfavg(6,i,j)
                 sumavg(7)=sumavg(7)+cfavg(7,i,j)
                 sumavg(8)=sumavg(8)+cfavg(8,i,j)

cmsw Physical quantities
                 sumavg2(1)=sumavg2(1)+pfavg(1,i,j)
                 sumavg2(2)=sumavg2(2)+pfavg(2,i,j)

                 sumavg2(3)=sumavg2(3)+pfavg(3,i,j)
                 sumavg2(4)=sumavg2(4)+pfavg(4,i,j)
                 sumavg2(5)=sumavg2(5)+pfavg(5,i,j)
                 sumavg2(6)=sumavg2(6)+pfavg(6,i,j)

                 sumavg2(7)=sumavg2(7)+pfavg(7,i,j)
                 sumavg2(8)=sumavg2(8)+pfavg(8,i,j)
                 sumavg2(9)=sumavg2(9)+pfavg(9,i,j)
                 sumavg2(10)=sumavg2(10)+pfavg(10,i,j)
                 sumavg2(11)=sumavg2(11)+pfavg(11,i,j)

                 sumavg2(12)=sumavg2(12)+pfavg(12,i,j)
                 sumavg2(13)=sumavg2(13)+pfavg(13,i,j)

               else

                 cfavg(1,i,j)=-99999.
                 cfavg(2,i,j)=-99999.
                 cfavg(3,i,j)=-99999.
                 cfavg(4,i,j)=-99999.

                 cfavg(5,i,j)=-99999.
                 cfavg(6,i,j)=-99999.
                 cfavg(7,i,j)=-99999.
                 cfavg(8,i,j)=-99999.

               endif

             enddo
          enddo

         yearosc=istep/ents_nyear

cmsw Convert to GtC
cmsw land
          do l=1,6
             avgsl(l)=sumavg(l)*rgtk*asurfrea
          enddo
          avgsl(7)=sumavg(7)/real(land_pts_ents)
          avgsl(8)=sumavg(8)/real(land_pts_ents)
cmsw atmosphere
          Gtatm=(pco2ld_tot*k_a)*rgtm*mtp*rnyear
cmsw ocean
          Gtocn=tot_mass_ocn_c*rgtk*rnyear

cmsw Write to a timeseries file

         write(46,'(11e24.16)')real(yearosc)-0.5,avgsl(1),avgsl(2),
     1         avgsl(3),avgsl(4),avgsl(5),avgsl(6),avgsl(7),avgsl(8),
     2         Gtatm
#ifdef biogem
     3        ,Gtocn
#endif

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
ccc	day since the beginning of the run
        myday=int(ents_yearlen*yearosc)

        fname=trim(outdir_name)//trim(ents_out_name)//'_TSannual.nc'

cccccccccccccccccccccc 
       do kk=1,8
                label=avlabels(kk)
                var_data1=avgsl(kk)
                call netcdf_ts_ents(fname,var_data1,label,myday)
        enddo
        var_data1=Gtatm
        label=avlabels(9)
        call netcdf_ts_ents(fname,var_data1,label,myday)
cccccccccccccccccccccccccccccccc

cmsw For physical timeseries average down

          do l=1,13
             avgsl2(l)=sumavg2(l)/real(land_pts_ents)
          enddo

cmsw write to file

          write(47,'(14e24.16)')real(yearosc)-0.5,avgsl2(1),avgsl2(2),
     1         avgsl2(3),avgsl2(4),avgsl2(5),avgsl2(6),avgsl2(7),
     2         avgsl2(8),avgsl2(9),avgsl2(10),avgsl2(11),avgsl2(12),
     3         avgsl2(13)

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
       do kk=1,13
                label=avlabels2(kk)
                var_data1=avgsl2(kk)
                call netcdf_ts_ents(fname,var_data1,label,myday)
        enddo
cccccccccccccccccccccccccccccccc

cmsw Spatial average file
#ifdef hfoutput
         write(datestring,'(i10.10)') istep+dum_istep0
         open(1,file='../results/'//trim(ents_out_name)//'.sland.hfavg_'//
     1        datestring)
#else
         filename = trim(outdir_name)//trim(ents_out_name)//'.'
     1              //'sland.avg'
         open(1,file=trim(filename))
#endif
      
c         print*,'Printing averaged land restart on istep=',istep
c         print*,'Averaged over the last',ents_nyear,'isteps'

cmsw Write to file

         do l=1,8 
            do i=1,imax
               do j=1,jmax 
                   write(1,10)cfavg(l,i,j)
               enddo
            enddo
         enddo
         write(1,10)real(yearosc)

c   10 format(e14.4)  
   10 format(e14.4e3)   

      close(1)

ccccccccccccccccccccccccccccccccccccccc NETCDF
        if ((mod(istep,ents_ianav).eq.0).and.(istep.ge.ents_ianav)) then

        myyear=int(yearosc)
        refname=trim(outdir_name)//trim(ents_out_name)//'_restartav_'//
     :  trim(ConvertFunc(myyear,10))//'.nc'
        inquire(file=refname,exist=fexist)
        if (fexist.eqv..true.) then
                open(8,file=refname,status='old')
                close(8,status='delete')
        end if
cccccccccccccccccccccccccccccccccccccccccccccccc
       do kk=1,8
          allocate(var_data(1,jmax,imax))
          label=labels(kk)
          do j=1,jmax
            do i=1,imax
                select case (kk)
                        case (1)
                        var_data(1,j,i)=cfavg(1,i,j)
                        case (2)
                        var_data(1,j,i)=cfavg(2,i,j)
                        case (3)
                        var_data(1,j,i)=cfavg(3,i,j)
                        case (4)
                        var_data(1,j,i)=cfavg(4,i,j)
                        case (5)
                        var_data(1,j,i)=cfavg(5,i,j)
                        case (6)
                        var_data(1,j,i)=cfavg(6,i,j)
                        case (7)
                        var_data(1,j,i)=cfavg(7,i,j)
                        case (8)
                        var_data(1,j,i)=cfavg(8,i,j)
                end select
            enddo
         enddo

        call netcdf_ents(refname,var_data,label,myday)
        deallocate(var_data)
        enddo

ccc     ADDING FINAL RESTART VALUE (SINGLE)

       status = nf_open(refname, nf_write, ncid)
        if (status .ne. nf_noerr) call her(status)

       status = nf_redef(ncid)
       if (status .ne. nf_noerr) call her(status)

       status=nf_inq_dimid(ncid,'time',timedim_id)
       if (status .ne. nf_noerr) call her(status)

       status=nf_def_var(ncid,'yearosc',nf_float,1,timedim_id,var_id)
        if (status .ne. nf_noerr) call her(status)

       status=nf_put_att_text(ncid,var_id,'long_name',7,'yearosc')
       if (status .ne. nf_noerr) call her(status)

       status=nf_enddef(ncid)
       if (status .ne. nf_noerr) call her(status)

       status=nf_put_var_double(ncid,var_id,real(yearosc))
       if (status .ne. nf_noerr) call her(status)

       status=nf_close(ncid)
       if (status .ne. nf_noerr) call her(status)

        end if
ccccccccccccccccccccccccccccccccccccccc

cmsw Zero arrays ready for next average

         do i=1,imax
            do j=1,jmax
               sphoto(i,j)=0.
               srveg(i,j)=0.
               sleaf(i,j)=0.
               srsoil(i,j)=0.

               sCveg1(i,j)=0.
               sCsoil1(i,j)=0.
               sfv1(i,j)=0.
               sepsv1(i,j)=0.
 
               do l=1,8
                  cfavg(l,i,j)=0.
               enddo

               stqld(1,i,j)=0.
               stqld(2,i,j)=0.

               sfx0a(i,j)=0.
               sfx0o(i,j)=0.
               sfxsens(i,j)=0.
               sfxlw(i,j)=0.

               sevap(i,j)=0.
               spptn(i,j)=0.
               srelh(i,j)=0.

               sbcap(i,j)=0.
               salbs(i,j)=0.
               ssnow(i,j)=0.
               sz0(i,j)=0.

               do l=1,13
                  pfavg(l,i,j)=0.
               enddo

            enddo
          enddo
          tot_mass_ocn_c=0.
          pco2ld_tot=0.
       endif 
c #endif
      endif
      end
