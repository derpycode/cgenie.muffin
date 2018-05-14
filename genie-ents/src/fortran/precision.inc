c     This is a file to determine the precision of the genie,libnc1 
c       modules...

      integer rk_libnc1
      integer rk_in


#ifdef real4
      parameter(rk_in=4)
#endif
#ifdef real8
      parameter(rk_in=8)
#endif


#ifdef ncreal4
      parameter(rk_libnc1=4)
#endif
#ifdef ncreal8
      parameter(rk_libnc1=8)
#endif


#ifdef utreal4
      integer rk_libutil1
      parameter(rk_libutil1=4)
#endif
#ifdef utreal8
      integer rk_libutil1
      parameter(rk_libutil1=8)
#endif
