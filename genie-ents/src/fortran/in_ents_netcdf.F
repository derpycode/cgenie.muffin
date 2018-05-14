cmsw
cmsw Reads in restarts from netcdf
cmsw

         subroutine in_ents_netcdf(fname,myday,
     :     land_snow_lnd                     !< land snow cover
     :     )

#include "genie_ents.cmn"
#include "var_ents.cmn"
        include 'netcdf.inc'

      integer i,j
  
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

        character*(*) fname

        character*20, dimension(11) :: labels=(/'photosynthesis',
     : 'veg_resp      ','leaf          ','soil_resp     ',
     : 'vegC          ','soilC         ','fv            ',
     : 'land_temp     ','land_water    ','snow          ',
     : 'pco2ld        '/)

        integer kk,myday,var_id,ncid,status,
     :  time_id

        character var_name*200

        real tempdata1, tempdata(maxients,maxjents)

        do kk=1,11

        var_name=labels(kk)

       status = nf_open(fname, nf_nowrite, ncid)
        if (status .ne. nf_noerr) call her(status)

       status=nf_inq_varid(ncid,var_name,var_id)
       if (status .ne. nf_noerr) call her(status)

        if (kk.ne.11) then

               status=nf_get_var_double(ncid,var_id,tempdata)
               if (status .ne. nf_noerr) call her(status)
        else
               status=nf_get_var_double(ncid,var_id,tempdata1)
               if (status .ne. nf_noerr) call her(status)
        end if

       status=nf_inq_varid(ncid,'time',time_id)
        if (status .ne. nf_noerr) call her(status)

       status=nf_get_var_int(ncid,time_id,myday)
       if (status .ne. nf_noerr) call her(status)

       status=nf_close(ncid)
       if (status .ne. nf_noerr) call her(status)

          do j=1,maxjents
                do i=1,maxients
                select case (kk)
                        case (1)
                        photo(j,i)=tempdata(i,j)

                        case (2)
                        respveg(j,i)=tempdata(i,j)

                        case (3)
                        leaf(j,i)=tempdata(i,j)

                        case (4)
                        respsoil(j,i)=tempdata(i,j)

                        case (5)
                        Cveg(j,i)=tempdata(i,j)

                        case (6)
                        Csoil(j,i)=tempdata(i,j)

                        case (7)
                        fv(j,i)=tempdata(i,j)

                        case (8)
                        tqld(1,j,i)=tempdata(i,j)

                        case (9)
                        tqld(2,j,i)=tempdata(i,j)
        
                        case (10)
                        land_snow_lnd(j,i)=tempdata(i,j)

                        case (11)
                        pco2ld=tempdata1
                end select
        end do
        end do
        end do

cmsw Initialise water bucket capacity

      do i=1,imax
        do j=1,jmax
          if(ents_k1(i,j).gt.ents_kmax)then
             bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))
cmsw initial roughness length
             z0(i,j)=max(0.001,kz0*Cveg(i,j))
          endif
        enddo
      enddo

      end
