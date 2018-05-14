ccc
ccc	Subroutine for writing ENTS NetCDF with 1D fields (time series)
ccc

        subroutine netcdf_ts_ents(fname,var_value,label,myday)

#include "genie_ents.cmn"
#include "var_ents.cmn"
        include 'netcdf.inc'

        character*(*) fname,label
        real var_value
        integer var_id, myday,ncid,status,time_dim,time_id
       character var_name*200, var_att(2,2)*100
        logical fexist
        integer mystart, timedim_id, timedim_len
        integer, allocatable :: temptime(:)

        call netcdf_db_ents(label,var_name,var_att)

        inquire(file=fname,exist=fexist)

ccccccccccccccc

        if (fexist.eqv..true.) then
               status = nf_open(fname, nf_write, ncid)
        if (status .ne. nf_noerr) call her(status)

                status = nf_redef(ncid)
               if (status .ne. nf_noerr) call her(status)

               status=nf_inq_dimid(ncid,'time',time_dim)
        if (status .ne. nf_noerr) call her(status)

        else

               status = nf_create(fname, nf_clobber, ncid)
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_dim(ncid,'time',
     :          nf_unlimited,time_dim)
        if (status .ne. nf_noerr) call her(status)

               status=nf_def_var(ncid,'time',nf_int,1,
     :          time_dim,time_id)
        if (status .ne. nf_noerr) call her(status)

        status=nf_put_att_text(ncid,time_id,'long_name',
     :  30,"day from the start of the run")
                if (status .ne. nf_noerr) call her(status)

        end if

ccccccccccccccc

       status=nf_inq_varid(ncid,var_name,var_id)
        if (status .ne. nf_noerr) then
               status=nf_def_var(ncid,var_name,nf_double,1,
     :          time_dim,var_id)
                if (status .ne. nf_noerr) call her(status)

        status=nf_put_att_text(ncid,var_id,trim(var_att(1,1)),
     :  len(trim(var_att(1,2))),trim(var_att(1,2)))
                if (status .ne. nf_noerr) call her(status)

                status=nf_put_att_text(ncid,var_id,trim(var_att(2,1)),
     :  len(trim(var_att(2,2))),trim(var_att(2,2)))
                if (status .ne. nf_noerr) call her(status)

        end if

ccccccccccccccc

        status=nf_enddef(ncid)
       if (status .ne. nf_noerr) call her(status)

       status=nf_inq_varid(ncid,var_name,var_id)
        if (status .ne. nf_noerr) call her(status)

       status=nf_inq_varid(ncid,'time',time_id)
       if (status .ne. nf_noerr) call her(status)

       status=nf_inq_dimid(ncid,'time',timedim_id)
       if (status .ne. nf_noerr) call her(status)
        
       status=nf_inq_dimlen(ncid,timedim_id,timedim_len)
       if (status .ne. nf_noerr) call her(status)

        allocate(temptime(timedim_len))
       status=nf_get_var_int(ncid,time_id,temptime)
       if (status .ne. nf_noerr) call her(status)

        if ((timedim_len.ne.0).and.(myday.eq.temptime(timedim_len))) then
                mystart=timedim_len
        else
                mystart=timedim_len+1
        end if
        deallocate(temptime)

       status=nf_put_var1_int(ncid,time_id,mystart,myday)
        if (status .ne. nf_noerr) call her(status)

        status=nf_put_var1_double(ncid,var_id,mystart,var_value)
        if (status .ne. nf_noerr) call her(status)

        status=nf_close(ncid)
        if (status .ne. nf_noerr) call her(status)

        end
