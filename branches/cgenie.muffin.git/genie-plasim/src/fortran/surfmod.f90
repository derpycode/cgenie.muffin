      module surfmod
      use pumamod
      implicit none

      character(len=80) :: version = '23.07.2009 by edi'

      integer, parameter :: nsurdim  = 100           ! max # of variables
      integer, parameter :: nsurunit =  35           ! read unit
      integer, parameter :: nlocunit =  36           ! read unit
      integer, parameter :: ncodunit =  37           ! write unit
      integer            :: nsurnum  =   0           ! total # of records
      integer            :: nfreefo  =   1           ! 1: free format
      character (len=16) :: ysfile   = 'surface.sra' ! surface file name
      character (len=16) :: ysurnam(nsurdim) = ' '   ! Name
      integer            :: nsurcod(nsurdim) =  -1   ! Code

!     namelist parameter

      integer :: nsurf = 1
      integer :: noromax = NTRU

      real :: oroscale = 1.   ! scaling factor for orography
      real :: doro(NHOR)      ! orography

      end module surfmod


!     ===================
!     SUBROUTINE SURFCODE
!     ===================

      subroutine surfcode(kcode,yn)
      use surfmod
      implicit none
      integer :: kcode
      character (len=*) :: yn

!     Build a table of codes and corresponding names
!     After that variables may be read from <surface.sra> or <surface.txt>
!     referencing their name

      nsurnum = nsurnum + 1
      if (nsurnum >= nsurdim) then
          call mpabort('NSURDIM in surfmod too low')
      endif
      nsurcod(nsurnum) = kcode ! Code
      ysurnam(nsurnum) = yn    ! Name
      return
      end subroutine surfcode


!     ===================
!     SUBROUTINE SURFNAME
!     ===================

      subroutine surfname(kcode,yn)
      use surfmod
      implicit none
      integer :: kcode
      integer j
      character (len=*) :: yn

!     Get name from code

      do j = 1 , nsurnum
         if (kcode == nsurcod(j)) then
            yn = ysurnam(j)
            return
         endif
      enddo
      yn = 'UNKNOWN'
      return
      end subroutine surfname


!     ==================
!     SUBROUTINE CHECKIO
!     ==================

      subroutine checkio(kstat,ym)
      implicit none
      integer :: kstat               ! status of last i/o
      character (len=* ) :: ym       ! calling routine
      character (len=60) :: ymessage ! complete abort message

      if (kstat /= 0) then
         write(ymessage,'(2A)') 'I/O error in ',ym
         call mpabort(ymessage)
      endif
      return
      end


!     =========================
!     SUBROUTINE GET_SURF_ARRAY
!     =========================

      subroutine get_surf_array(yn,pa,kdim,klot,kread)
      use surfmod
      integer j,jlot

      character (len=*) :: yn  ! Array name
      integer :: icode         ! code to read
      integer :: kdim          ! dim of array
      integer :: klot          ! number of arrays
      integer :: kread         ! flag to indicate success
      integer :: ilot          ! arrays read
      integer :: io            ! iostat
      integer :: ih(8)         ! header
      character (len=200) :: yf ! filename
      character (len=16) :: yc ! formatted array name
      logical :: lex           ! exists
      real :: pa(kdim,klot)    ! array to receive values

      kread = 0 ! Initialize as "not read"
      icode = 0
      ilot  = 0
      do j = 1 , nsurnum
         if (yn == trim(ysurnam(j))) then
            icode = nsurcod(j)
            yc = ysurnam(j)
            exit
         endif
      enddo

      if (icode == 0) then
         write (*,'(" *** unknown array [",A,"] in get_surf_array")') yn
         return
      endif

      call code_surf_file(icode,yf)
      inquire(file=yf,exist=lex)
      if (lex) then
         open (ncodunit,file=yf,form='formatted')
         if (klot == 1) then ! single array (no annual cycle)
            read (ncodunit,*,iostat=io) ih(:)
            call checkio(io,'get_surf_array 1')
            read (ncodunit,*,iostat=io) pa(:,1)
            call checkio(io,'get_surf_array 2')
            ilot = 1
         else                ! annual cycle (12 or 14 months)
            do jlot = 1 , klot
               read (ncodunit,*,iostat=io) ih(:)
               if (io /= 0) exit ! end-of-file
               read (ncodunit,*,iostat=io) pa(:,jlot)
               call checkio(io,'get_surf_array 3')
               ilot = jlot ! arrays read so far
            enddo
            if (ilot < klot) then ! read less arrays than expected
               if (ilot == 12 .and. klot == 14) then ! do cyclic expansion
                  do jlot = 12,1,-1
                     pa(:,jlot+1) = pa(:,jlot) ! Shift year to indices 2-13
                  enddo
                  pa(:,14) = pa(:, 2) ! Copy January
                  pa(:, 1) = pa(:,13) ! Copy December
               elseif (ilot == 1 .and. klot == 14) then ! copy mean to all months
                  do jlot = 2,14
                     pa(:,jlot) = pa(:,1)
                  enddo
               else
                  write(*,*) 'Error reading file ',trim(yf)
                  write(*,*) 'Expected ',klot,' arrays'
                  write(*,*) 'Found    ',ilot,' arrays'
                  call mpabort('Wrong surface file')
               endif
            endif
         endif ! (ilot < klot) 
         close(ncodunit)
         kread = 1
         write(*,'(" * Read ",A10," from <",A,"> *")') yc,yf
         if (ilot < klot) then
          write(*,'(" * Expanded ",A10,"  from",I3," to",I3" months *")') yc,ilot,klot
         endif
      else
         write(*,'(" * Init ",A10," [code = ",I5,"] internally *")') yc,icode
      endif
      return
      end subroutine get_surf_array


!     =========================
!     SUBROUTINE CODE_SURF_FILE
!     =========================

      subroutine code_surf_file(kcode,yfn)
      use pumamod
      implicit none
      integer :: kcode
      character (len=200) :: yfn

!PBH pathname indir introduced for easy setup of different geographies etc    
      write(yfn,'("/N",I3.3,"_surf_",I4.4,".sra")') NLAT,kcode
      yfn = trim(indir_name)//trim(yfn)
      print*,yfn

!      write(yfn,'("N",I3.3,"_surf_",I4.4,".sra")') NLAT,kcode
      return
      end

      
!     ======================
!     SUBROUTINE SURFACE_INI
!     ======================

      subroutine surface_ini
      use surfmod
      implicit none
      integer io,imon,jmon

      integer :: ih(8)         ! current header
      integer :: il(8)         ! last    header
      integer :: icmon
      character (len=200) :: yf ! file name
      real    :: zy(NUGP,0:13) ! code with annual cycle
      logical :: lm(0:13)      ! month read flag
      logical :: lex           ! file exists

!     Define code - arrayname relationship
 
      call surfcode( 129,'doro'    )
      call surfcode( 172,'dls'     )

!     miscmod arrays

      call surfcode( 130,'dtnudge' )
      call surfcode(  22,'dfnudge' )

!     oceanmod arrays

      call surfcode( 169,'yclsst'  )
      call surfcode( 172,'yls'     )  ! same as dls
      call surfcode( 903,'yfsst'   )

!     icemod arrays

      call surfcode( 169,'xclsst'  )  ! same as zclsst
      call surfcode( 172,'xls'     )  ! same as dls
      call surfcode( 210,'xclicec' )
      call surfcode( 211,'xcliced' )
      call surfcode( 709,'xflxice' )

!     simba arrays

      call surfcode( 200,'dlai'    ) ! leaf area index
      call surfcode( 304,'dcveg'   ) ! carbon in biomass
      call surfcode( 305,'dcsoil'  ) ! carbon in soil
      call surfcode(1604,'pgrow'   ) ! biomass growth
      call surfcode(1605,'plai'    ) ! above ground growth
      call surfcode(1606,'pgs'     ) ! stomatal conductance
      call surfcode(1607,'pz0_max' ) ! max. roughness

!     landmod arrays

      call surfcode( 173,'dz0clim' )
      call surfcode(1730,'dz0climo')
      call surfcode( 174,'dalbcl'  )   ! background albedo
      call surfcode(1740,'dalbcls' )   ! albedo for bare soil
      call surfcode(1741,'dalbclv' )   ! albedo for vegetation
      call surfcode( 232,'dglac'   )   
      call surfcode( 212,'dforest' )
      call surfcode( 229,'dwmax'   )
      call surfcode( 209,'dtclsoil')
      call surfcode( 169,'dtcl'    )   ! same as xclsst
      call surfcode( 140,'dwcl'    )

!     radmod arrays

      call surfcode( 237,'dqo3cl'  )   ! climatological ozone

!     Scan start_data for codes and store sequence number
!     Write surf-code files if not existent

      icmon = 0
      ih(:) = 0
      il(:) = 0
      lm(:) = .false.

!     Look first for 'surface.sra' written in free format
!     otherwise  for 'surface.txt' written in (8E12.6)

      open(nsurunit,file=ysfile,form='formatted',status='old',iostat=io)
      if (io /= 0) then ! Compatibility mode for old format surface file
         close(nsurunit)
         ysfile  = 'surface.txt'
         nfreefo = 0
         open(nsurunit,file=ysfile,form='formatted',status='old',iostat=io)
         if (io /= 0) return ! Neither <surface.sra> nor <surface.txt>
      endif
      do
         read (nsurunit,*,IOSTAT=io) ih(:)
         if (io /= 0) ih(:) = 0

!        write surface code file

         if (ih(1) /= il(1) .and. il(1) > 0) then
            call code_surf_file(il(1),yf)
            inquire(file=yf,exist=lex)
            if (.not. lex) then
               if (icmon == 12 .and. lm(12) .and. .not. lm(0)) then
                  zy(:,0) = zy(:,12)
                  lm(0) = .true.
               endif
               if (icmon == 12 .and. lm(1) .and. .not. lm(13)) then
                  zy(:,13) = zy(:,1)
                  lm(13) = .true.
               endif
               open(ncodunit,file=yf,form='formatted')
               do jmon = 0 , 13
                  if (lm(jmon)) then
                     write (ncodunit,'(8i10)') il(1:2),jmon*100,il(4:8)
                     write (ncodunit,'(4e16.6)') zy(:,jmon)
                  endif
               enddo ! jmon
               close(ncodunit)
               write (*,'(" * Created <",A,"> from <",A,">  *")') & 
                     yf,trim(ysfile)
            endif ! lex
            icmon = 0
            lm(:) = .false.
         endif ! ih(1)

         if (io /= 0) exit ! end-of-file
         imon = ih(3)
         if (imon >= 100) imon = mod(imon/100,100)
         if (imon >= 0 .and. imon <= 13) then
            if (nfreefo == 1) then 
               read (nsurunit,*,IOSTAT=io) zy(:,imon)
            else
               read (nsurunit,'(8E12.6)',IOSTAT=io) zy(:,imon)
            endif
            call checkio(io,'surface_ini')
            icmon = icmon + 1
            lm(imon) = .true.
            il(:) = ih(:)
         else
            write (*,'(A)') "*** Illegal month in header ***"
            write (*,'(8I10)') ih(:)
         endif
      enddo
      close(nsurunit)

      return
      end subroutine surface_ini


!     ==================
!     SUBROUTINE SURFINI
!     ==================

      subroutine surfini
      use surfmod
      implicit none
      integer jr,jm,jn,ji
!
!     initialize surface parameter
!

      namelist/surfpar/nsurf,noromax
      namelist/surfpar_nml/oroscale

      if (mypid == NROOT) then
         read(11,surfpar)
         write (*,'(/," *****************************************")')
         write (*,'(" * SURFMOD ",a29," *")') trim(version)
         write (*,'(" *****************************************")')
         write (*,'(" * Namelist SURFPAR from <puma_namelist> *")')
         write (*,'(" *****************************************")')
         write(*,surfpar)
      endif
      open(unit=56,file='data_PLASIM')
      read(unit=56,NML=surfpar_nml)
      write(*,surfpar_nml)
      close(unit=56)

      call mpbci(nsurf)

      if (nrestart == 0) then ! need to read start data
         call mpsurfgp('doro',doro,NHOR,1)
         call mpsurfgp('dls' ,dls ,NHOR,1)
         doro(:) = doro(:) * oroscale  ! Scale orography

!        Compute spectral orography

         call gp2fc(doro,NLON,NLPP)
         call fc2sp(doro,so)
         call mpsum(so,1)
         if (mypid == NROOT) then
            so(:) = so(:) / (cv*cv)
            if (noromax < NTRU) then
             jr=-1
             do jm=0,NTRU
              do jn=jm,NTRU
               jr=jr+2
               ji=jr+1
               if(jn > noromax) then
                so(jr)=0.
                so(ji)=0.
               endif
              enddo
             enddo
            endif ! (noromax < NTRU)
         endif ! (mypid == NROOT)

!*       adjust land sea mask  (workaround)

         dls(:)=AMAX1(dls(:),0.)
         dls(:)=AMIN1(dls(:),1.)

      endif ! (nrestart == 0)

      call landini                      ! land module
      call vegini                       ! vegetation module
      if (n_sea_points > 0) call seaini ! sea module

      return
      end subroutine surfini

!     ===================
!     SUBROUTINE SURFSTEP
!     ===================

      subroutine surfstep
      use surfmod
      implicit none

      call landstep
      if (n_sea_points > 0) call seastep

      return
      end subroutine surfstep

!     ===================
!     SUBROUTINE SURFSTOP
!     ===================

      subroutine surfstop
      use surfmod
      implicit none

      call landstop
      call vegstop
      if (n_sea_points > 0) call seastop

      return
      end subroutine surfstop

!     ===================
!     SUBROUTINE SURF_REST
!     ===================

      subroutine surf_rest
      use surfmod
      implicit none

      call land_rest
      call veg_rest
      if (n_sea_points > 0) call sea_rest

      return
      end subroutine surf_rest
