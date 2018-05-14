
#Makefile to compile source files common to biogem, atchem, and sedgem

#SRCS=gem_cmn.f90 gem_util.f90 gem_carbchem.f90 gem_netcdf.f90
SRCS=gem_cmn.f90 gem_util.f90 gem_geochem.f90 gem_carbchem.f90 gem_netcdf.f90 gem_data.f90 initialise_gem.f90 end_gem.f90
OBJS=$(patsubst %.f90, %.$(OBJ_EXT), $(SRCS))
LIBNAME=libcmngem
LOCFLAGS= $(FLAGR8)

$(LIBNAME).$(LIB_EXT): $(OBJS)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJS)

$(OBJS): %.$(OBJ_EXT) : %.f90
	$(F77) $(COMPILEONLY) $(LOCFLAGS) $(FFLAGS) $< $(OBJ_FLAG)$@

.PHONY: clean
clean:
	\rm -f $(OBJS) $(LIBNAME).$(LIB_EXT) *.mod
