cmsw
cmsw Instantaneous globally averaged values.
cmsw Produces file *.plandt of exactly same quantities
cmsw as *.pslavgt but at a snap shot in time rather
cmsw than annually averaged.
cmsw
      subroutine physt_diags(istep,dum_fx0a,dum_fx0o,
     :  dum_fxsen,dum_fxlw,dum_evap,dum_pptn,dum_relh,
     :     albs_lnd,                         !< surface albedo
     :     land_snow_lnd                     !< land snow cover
     :     )

#include "genie_ents.cmn"
#include "var_ents.cmn"

      real diagtime,sumavg2(13),avgsl2(13)
 
      integer i,j,istep,l

      character filename*200
      real ,dimension(maxi,maxj)::dum_fx0a
      real ,dimension(maxi,maxj)::dum_fx0o
      real ,dimension(maxi,maxj)::dum_fxsen
      real ,dimension(maxi,maxj)::dum_fxlw
      real ,dimension(maxi,maxj)::dum_evap
      real ,dimension(maxi,maxj)::dum_pptn
      real ,dimension(maxi,maxj)::dum_relh

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

ccccccccccccccccccccccccc FOR NETCDF
        character fname*200, label*200
        real var_data

ccc	'diagtime1'
        character*6, dimension(13) :: labels=(/'avgs1 ','avgs2 ',
     :  'avgs3 ','avgs4 ','avgs5 ','avgs6 ','avgs7 ','avgs8 ','avgs9 ',
     :  'avgs10','avgs11','avgs12','avgs13'/)
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

      
c SG > Open physt_diags file for disgnostics
      filename = trim(outdir_name)//trim(ents_out_name)//
     1   '.'//'pslandt'
      open(48,file=trim(filename),POSITION='APPEND')
c      print*,trim(filename)
c SG <

      diagtime=real(istep)/real(ents_nyear)

      do l=1,13
         sumavg2(l)=0.
      enddo
 
      do i=1,imax
         do j=1,jmax
            if(ents_k1(i,j).gt.ents_kmax)then

            sumavg2(1)=sumavg2(1)+tqld(1,i,j)
            sumavg2(2)=sumavg2(2)+tqld(2,i,j)

            sumavg2(3)=sumavg2(3)+dum_fx0a(i,j)
            sumavg2(4)=sumavg2(4)+dum_fx0o(i,j)
            sumavg2(5)=sumavg2(5)+dum_fxsen(i,j)
            sumavg2(6)=sumavg2(6)+dum_fxlw(i,j)

            sumavg2(7)=sumavg2(7)+dum_evap(i,j)
            sumavg2(8)=sumavg2(8)+dum_pptn(i,j)
            sumavg2(9)=sumavg2(9)+dum_relh(i,j)

            sumavg2(10)=sumavg2(10)+bcap(i,j)
            sumavg2(11)=sumavg2(11)+albs_lnd(i,j)
            sumavg2(12)=sumavg2(12)+land_snow_lnd(i,j)
            sumavg2(13)=sumavg2(13)+z0(i,j)
 
            endif
         enddo
      enddo


cmsw For physical timeseries average down

      do l=1,13
         avgsl2(l)=sumavg2(l)/real(land_pts_ents)
      enddo

cmsw write to file

      write(48,'(15e24.16)')diagtime,avgsl2(1),avgsl2(2),
     1         avgsl2(3),avgsl2(4),avgsl2(5),avgsl2(6),avgsl2(7),
     2         avgsl2(8),avgsl2(9),avgsl2(10),avgsl2(11),avgsl2(12),
     3         avgsl2(13)

       close(48)

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
ccc	myyear=int(istep/ents_nyear)+1
ccc	mymonth=int(12*mod(istep,ents_nyear)/ents_nyear)
ccc	myday=int(360*istep/ents_nyear-mymonth*30-(myyear-1)*360)
        myday=int(360*istep/ents_nyear)

        fname=trim(outdir_name)//trim(ents_out_name)//'_TS.nc'

cccccccccccccccccccccc 
       do kk=1,13
          label=labels(kk)
          var_data=avgsl2(kk)
          call netcdf_ts_ents(fname,var_data,label,myday)
        enddo
cccccccccccccccccccccccccccccccc

        end
