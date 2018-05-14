cmsw
cmsw Subroutine writes out reservoir sizes every
cmsw itstp timesteps
cmsw
      subroutine carbt_diags(istep)

c      include '../genie-cgoldstein/var.cmn'
#include "genie_ents.cmn"
#include "var_ents.cmn"
        include 'netcdf.inc'
  
      integer i,j,istep

      real diagtime
      real sumveg,sumsoil,sumfv
      real sumphoto,sumrveg,sumrsoil,sumleaf
      real Gtveg,Gtsoil,Gtatm,Gfv
      real Gtphoto,Gtrveg,Gtrsoil,Gtleaf
      
      character filename*200

ccccccccccccccccccccccccc FOR NETCDF
        character fname*200, label*200
        real var_data

        character*8, dimension(9) :: labels=(/'Gtveg  ','Gtsoil ',
     :  'Gtatm  ','Gfv    ','pco2ld ','Gtphoto','Gtrveg ','Gtleaf ',
     : 'Gtrsoil'/)
        integer kk
ccc        integer mymonth,myyear
        integer myday
        logical fexist

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

c SG > Open slandt file for disgnostics
      filename = trim(outdir_name)//trim(ents_out_name)//'.'//'slandt'
      open(43,file=trim(filename),POSITION='APPEND')
c      print*,trim(filename)
c SG <

      diagtime=real(istep)/real(ents_nyear)

      sumveg=0.
      sumsoil=0.
      sumfv=0.

      sumphoto=0.
      sumrveg=0.
      sumrsoil=0.
      sumleaf=0. 

cmsw Sum up all carbon spatially in each reservoir

      do i=1,imax
         do j=1,jmax
            if(ents_k1(i,j).gt.ents_kmax)then
               sumveg=sumveg+Cveg(i,j)
               sumsoil=sumsoil+Csoil(i,j)
               sumfv=sumfv+fv(i,j)
cmsw Sum up fluxes
               sumphoto=sumphoto+photo(i,j)
               sumrveg=sumrveg+respveg(i,j)
               sumrsoil=sumrsoil+respsoil(i,j)
               sumleaf=sumleaf+leaf(i,j)
             endif
          enddo
      enddo

cmsw Convert back to GtC

      Gtveg=sumveg*rgtk*asurfrea
      Gtsoil=sumsoil*rgtk*asurfrea
      Gtatm=(pco2ld*k_a)*rgtm*mtp

cmsw Covert back to average fraction

      Gfv=sumfv/land_pts_ents

cmsw Convert to GtC/yr
   
      Gtphoto=sumphoto*rgtk*asurfrea
      Gtrveg=sumrveg*rgtk*asurfrea
      Gtrsoil=sumrsoil*rgtk*asurfrea
      Gtleaf=sumleaf*rgtk*asurfrea

cmsw Write to file

      write(43,'(10e24.16)')diagtime,Gtveg,Gtsoil,Gtatm,Gfv,pco2ld,
     &      Gtphoto,Gtrveg,Gtleaf,Gtrsoil

       close (43)

ccccccccccccccccccccccccccccccccccccccc ncdf-replacement
ccc	myyear=int(istep/ents_nyear)+1
ccc	mymonth=int(12*mod(istep,ents_nyear)/ents_nyear)
ccc	myday=int(360*istep/ents_nyear-mymonth*30-(myyear-1)*360)
        myday=int(360*istep/ents_nyear)

        fname=trim(outdir_name)//trim(ents_out_name)//'_TS.nc'
        if (istep.eq.ents_itstp) then
        inquire(file=fname,exist=fexist)
        if (fexist.eqv..true.) then
                open(8,file=fname,status='old')
                close(8,status='delete')
        end if
        end if
cccccccccccccccccccccc 
       do kk=1,9
          label=labels(kk)
                select case (kk)
                        case (1)
                        var_data=Gtveg
                        case (2)
                        var_data=Gtsoil
                        case (3)
                        var_data=Gtatm
                        case (4)
                        var_data=Gfv
                        case (5)
                        var_data=pco2ld
                        case (6)
                        var_data=Gtphoto
                        case (7)
                        var_data=Gtrveg
                        case (8)
                        var_data=Gtleaf
                        case (9)
                        var_data=Gtrsoil
                end select
        call netcdf_ts_ents(fname,var_data,label,myday)
        enddo
cccccccccccccccccccccccccccccccc

      end        
