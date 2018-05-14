      subroutine ininc(fname,nmaxdims,ndim,nvar,natts,nattsvar,
     :                 vdims,vadims,ndims,
     :                 dimname,varname,
     :                 attdimname,attvarname,
     :                 ncid,iddim,idvar)

      implicit none

      INTEGER IFLEN,LNSIG,NDIM,II,ITLEN,NVAR,III,ITLEN1,ITLEN2
c
      character*(*) fname
      include 'netcdf.inc'
c
* error status return
      integer  iret,idebug
* netCDF id
      integer  ncid,nmaxdims
* attribute vectors
c AY (24/03/04) : realval declared as real*4 (otherwise messes up missing
c                 data flag in GOLDSTEIN netCDF files)
c                 note : in the future, this should be dealt with via 
c                 precision arguments similar to those used in GOLDSTEIN
c (DJL) THIS HAS NOW BEEN FIXED!! NO NEED FOR *4 NOW.
      real realval(1)
      integer tempdims(200),dimdim(200)
c
      integer iddim(*),idvar(*),natts(*),nattsvar(*)
      integer vdims(*),vadims(nmaxdims,*),ndims(*)
      character*200 dimname(*),varname(*),
     :              attdimname(2,nmaxdims,*),
     :              attvarname(2,nmaxdims,*)
      character*200 tname,tname1,tname2
       
      integer realkind
c
      realkind=kind(realval)

      idebug=0
c
      iflen=lnsig(fname)
*
* enter define mode
*
      if (idebug.eq.1) then
         print*,' opening file = ',fname(1:iflen)
      end if 
      iret = nf_create(fname(1:iflen), NF_CLOBBER, ncid)
      call check_err(iret)
*
* define dimensions
*
      if (idebug.eq.1) then
         print*,' defining dimensions ',ndim
      end if
      do ii=1,ndim
         tname=dimname(ii)
         itlen=lnsig(tname)
         iret = nf_def_dim(ncid,tname(1:itlen),ndims(ii),dimdim(ii))
      end do
*
* define coordinates etc
*
      if (idebug.eq.1) then
         print*,' defining coordinates ',ndim
      end if
      do ii=1,ndim
         tname=dimname(ii)
         itlen=lnsig(tname)
         tempdims(1) = dimdim(ii)
         iret = nf_def_var(ncid,tname(1:itlen), NF_REAL, 1,
     :                     tempdims,iddim(ii))
         call check_err(iret)
      end do
*
* and the real data
*
      if (idebug.eq.1) then
         print*,' defining variables ',nvar
      end if
      do ii=1,nvar 
         tname=varname(ii)
         itlen=lnsig(tname)
         if (idebug.eq.1) then
            print*,'       variables ',ii,vdims(ii),tname(1:itlen)
         end if
         do iii=1,vdims(ii) 
            tempdims(iii)=dimdim(vadims(iii,ii))
            if (idebug.eq.1) then
               print*,'       variables ',ii,iii,tempdims(iii)
            end if
         end do
         iret = nf_def_var(ncid,tname(1:itlen), NF_REAL,vdims(ii), 
     :                     tempdims,idvar(ii))  
         call check_err(iret)
      end do
*
* assign attributes for coordinates
*
      if (idebug.eq.1) then
         print*,' Defining attibutes of coordinates ',ndim  
      end if
      do ii=1,ndim
         if (idebug.eq.1) then
            print*,' Number of attributes = ',ii,natts(ii) 
         end if
         do iii=1,natts(ii)
            tname1=attdimname(1,iii,ii)
            tname2=attdimname(2,iii,ii)
            itlen1=lnsig(tname1)
            itlen2=lnsig(tname2)
            if (idebug.eq.1) then
               print*,ii,iii,tname1(1:itlen1) 
               print*,ii,iii,tname2(1:itlen2) 
            end if
            iret = nf_put_att_text(ncid,iddim(ii),
     :                             tname1(1:itlen1),itlen2, 
     :                             tname2(1:itlen2))
            call check_err(iret)
         end do
         realval(1) = -99999.0
         if (realkind.eq.4) then
           iret = nf_put_att_real(ncid,iddim(ii), 'missing_value', 
     :                          NF_REAL, 1, realval)
         elseif (realkind.eq.8) then
           iret = nf_put_att_double(ncid,iddim(ii), 'missing_value', 
     :                          NF_REAL, 1, realval)
         else
           print*,'precision problem in writenc6'
           stop
         endif
         call check_err(iret)
      end do
*
*  assign attributes for variables
*
      if (idebug.eq.1) then
        print*,' Defining attibutes of variables ',nvar  
      end if
      do ii=1,nvar
         if (idebug.eq.1) then
            print*,' Number of attributes = ',ii,nattsvar(ii) 
         end if
         do iii=1,nattsvar(ii)
            tname1=attvarname(1,iii,ii)
            tname2=attvarname(2,iii,ii)
            itlen1=lnsig(tname1)
            itlen2=lnsig(tname2)
            if (idebug.eq.1) then
               print*,ii,iii,tname1(1:itlen1)   
               print*,ii,iii,tname2(1:itlen2)   
            end if
            iret = nf_put_att_text(ncid,idvar(ii),
     :                             tname1(1:itlen1),itlen2, 
     :                             tname2(1:itlen2))
            call check_err(iret)
         end do
         realval(1) = -99999.0
         if (realkind.eq.4) then
           iret = nf_put_att_real(ncid,idvar(ii), 'missing_value', 
     :                          NF_REAL, 1, realval)
         elseif (realkind.eq.8) then
           iret = nf_put_att_double(ncid,idvar(ii), 'missing_value', 
     :                          NF_REAL, 1, realval)
         else
           print*,'precision problem in writenc6'
           stop
         endif
         call check_err(iret)
      end do 
*
* global attribute
*
      if (idebug.eq.1) then
         print*,' Writing global attribute '
      end if
      tname='Produced using writenc6 program by PJV'
      itlen=lnsig(tname)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'title',
     :                       itlen,tname(1:itlen))
      call check_err(iret)
*
* leave define mode
*
      iret = nf_enddef(ncid)
      call check_err(iret)
c
      if (idebug.eq.1) then
         print*,' Finished ininc '
      end if
*
      return
      end
*
      subroutine writedim(ncid,iddim,data)
      implicit none
      include 'netcdf.inc'
      integer ncid,iddim
      real data(*)
c
      integer iret 
*
      integer realkind

      realkind=kind(data(1))

* write coordinates
*
      if (realkind.eq.4) then
        iret = nf_put_var_real(ncid,iddim,data)
      else if (realkind.eq.8) then
        iret = nf_put_var_double(ncid,iddim,data)
      else
        print*,'precision problem in writedim'
        stop
      endif
      call check_err(iret)
*
      return
      end
*
      subroutine writevar(ncid,idvar,data)
      implicit none
      include 'netcdf.inc'
      integer ncid,idvar
      real data(*)
c
      integer iret 
*
      integer realkind
*
* write data
      realkind=kind(data(1))

*
      if (realkind.eq.4) then
        iret = nf_put_var_real(ncid,idvar,data)
      else if (realkind.eq.8) then
        iret = nf_put_var_double(ncid,idvar,data)
      else
        print*,'precision problem in writevar'
        stop
      endif
      call check_err(iret)
*
      return
      end
c
      subroutine writevar2(ncid,idvar,data,
     :                     ix1,ix2,iy1,iy2,iz1,iz2,it1,it2)
      implicit none
      include 'netcdf.inc'
      integer ncid,idvar
      integer ix1,ix2,iy1,iy2,iz1,iz2,it1,it2
      real data(*)
      integer start(4),count(4)
c
      integer iret 
*
      integer realkind
*
* write data
      realkind=kind(data(1))
*
      start(1)=ix1
      start(2)=iy1
      start(3)=iz1
      if (it1.gt.0) then
         start(4)=it1
      end if
      start(4)=it1
      count(1)=ix2-ix1+1
      count(2)=iy2-iy1+1
      count(3)=iz2-iz1+1
      if (it1.gt.0) then
         count(4)=it2-it1+1
      end if
c      

      if (realkind.eq.4) then
        iret = nf_put_vara_real(ncid,idvar,start,count,data)
      else if (realkind.eq.8) then
        iret = nf_put_vara_double(ncid,idvar,start,count,data)
      else
        print*,'precision problem in writevar2'
        stop
      endif

      call check_err(iret)
*
      return
      end
c
      subroutine closenc(ncid)
      implicit none
      include 'netcdf.inc'
      integer ncid,iret
c
*
* close file
*
      iret = nf_close(ncid)
      call check_err(iret)

      return
      end
