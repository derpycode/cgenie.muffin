ccc
ccc	Subroutine for writing ENTS NetCDF with 2D fields
ccc

        subroutine netcdf_ents(fname,var_data,label,myday)

#include "genie_ents.cmn"
        include 'netcdf.inc'

        character*(*) fname, label
        character dim1_name*200, dim2_name*200, dim3_name*200, 
     : var_name*200, var_att(2,2)*200
        integer ncid, status, var_id, dims(3),jj,myday,
     : vardim_id1,vardim_id2,vardim_id3,time_id,lon_id,lat_id
        logical fexist
        real var_data(:,:,:), londata(jmax)

        call netcdf_db_ents(label,var_name,var_att)

        dim1_name='time'
        dim2_name='latitude'
        dim3_name='longitude'

        inquire(file=fname,exist=fexist)

        if (fexist.eqv..true.) then
               status = nf_open(fname, nf_write, ncid)
        if (status .ne. nf_noerr) call her(status)

                status = nf_redef(ncid)
               if (status .ne. nf_noerr) call her(status)

               status=nf_inq_dimid(ncid,dim1_name,vardim_id1)
        if (status .ne. nf_noerr) call her(status)

               status=nf_inq_dimid(ncid,dim2_name,vardim_id2)
        if (status .ne. nf_noerr) call her(status)

               status=nf_inq_dimid(ncid,dim3_name,vardim_id3)
        if (status .ne. nf_noerr) call her(status)

                dims(1)=vardim_id1
                dims(2)=vardim_id2
                dims(3)=vardim_id3

        else
               status = nf_create(fname, nf_clobber,ncid)
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_dim(ncid,dim1_name,1,vardim_id1)
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_dim(ncid,dim2_name,jmax,vardim_id2)
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_dim(ncid,dim3_name,imax,vardim_id3)
        if (status .ne. nf_noerr) call her(status)

                dims(1)=vardim_id1
                dims(2)=vardim_id2
                dims(3)=vardim_id3

               status=nf_def_var(ncid,dim1_name,nf_int,1,dims(1),time_id)
        if (status .ne. nf_noerr) call her(status)

               status=nf_put_att_text(ncid,time_id,'units',29,
     :          'day from the start of the run')
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_var(ncid,dim2_name,nf_float,1,dims(2),lat_id)
        if (status .ne. nf_noerr) call her(status)
        
               status=nf_put_att_text(ncid,lat_id,'units',13,'degrees_north')
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_var(ncid,dim3_name,nf_float,1,dims(3),lon_id)
        if (status .ne. nf_noerr) call her(status)

               status=nf_put_att_text(ncid,lon_id,'units',12,'degrees_east')
        if (status .ne. nf_noerr) call her(status)

        end if

ccc	print*,var_name

cccccccccccccccccccc

       status=nf_inq_varid(ncid,var_name,var_id)
       if (status .eq. nf_noerr) then
                continue
        else
               status=nf_def_var(ncid,var_name,nf_double,3,dims,var_id)
        if (status .ne. nf_noerr) call her(status)

               status=nf_put_att_text(ncid,var_id,trim(var_att(1,1)),
     :  len(trim(var_att(1,2))),trim(var_att(1,2)))
               if (status .ne. nf_noerr) call her(status)

        status=nf_put_att_text(ncid,var_id,trim(var_att(2,1)),
     :  len(trim(var_att(2,2))),trim(var_att(2,2)))
              if (status .ne. nf_noerr) call her(status)
        end if

cccccccccccccccccccccccc

       status=nf_enddef(ncid)
       if (status .ne. nf_noerr) call her(status)

cccccccccccccccccccccccc

       status=nf_inq_varid(ncid,dim1_name,time_id)
       if (status .ne. nf_noerr) call her(status)

       status=nf_put_var_int(ncid,time_id,myday)

cccccccccccccccccccccccccc

       status=nf_inq_varid(ncid,dim2_name,lat_id)
       if (status .ne. nf_noerr) call her(status)

       status=nf_put_var_double(ncid,lat_id,ents_lat)

       status=nf_inq_varid(ncid,dim3_name,lon_id)
       if (status .ne. nf_noerr) call her(status)
        do jj=1,jmax
                londata(jj)=-255+(jj-1)*(360/jmax)
        end do

       status=nf_put_var_double(ncid,lon_id,londata)

       status=nf_put_var_double(ncid,var_id,var_data)
       if (status .ne. nf_noerr) call her(status)

       status=nf_close(ncid)
       if (status .ne. nf_noerr) call her(status)

       end

c	SUBROUTINE FOR ERROR PROCESSING 
       subroutine her(err)
        implicit none
       include 'netcdf.inc'
       integer err
       print *, 'Error: ', nf_strerror(err)
       stop 2
       end subroutine her
