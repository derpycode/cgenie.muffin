!     ==================
!     SUBROUTINE OUTDIAG
!     ==================

      subroutine outdiag_plasim
      use pumamod
      implicit none
      integer jdiag,jcode,jlev

!     *****************************************
!     * 2-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiagsp2d > 0) then
       do jdiag=1,ndiagsp2d
        jcode=40+jdiag
        call writesp(40,dsp2d(1,jdiag),jcode,0,1.,0.0)
       enddo
      end if

!     *****************************************
!     * 3-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiagsp3d > 0) then
       do jdiag=1,ndiagsp3d
        jcode=60+jdiag
        do jlev=1,NLEV
         call writesp(40,dsp3d(1,jlev,jdiag),jcode,jlev,1.,0.0)
        enddo
       enddo
      end if

!     *****************************************
!     * 2-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiaggp2d > 0) then
       do jdiag=1,ndiaggp2d
        jcode=jdiag
        call writegp(40,dgp2d(1,jdiag),jcode,0)
       enddo
      end if

!     *****************************************
!     * 3-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiaggp3d > 0) then
       do jdiag=1,ndiaggp3d
        jcode=20+jdiag
        do jlev=1,NLEV
         call writegp(40,dgp3d(1,jlev,jdiag),jcode,jlev)
        enddo
       enddo
      end if

!     ************************************************
!     * cloud forcing (clear sky fluxes) diagnostics *
!     ************************************************

      if(ndiagcf > 0) then
       call writegp(40,dclforc(1,1),101,0)
       call writegp(40,dclforc(1,2),102,0)
       call writegp(40,dclforc(1,3),103,0)
       call writegp(40,dclforc(1,4),104,0)
       call writegp(40,dclforc(1,5),105,0)
       call writegp(40,dclforc(1,6),106,0)
       call writegp(40,dclforc(1,7),107,0)
      end if

      return
      end

