#

#

LOCALFLAGS=$(FFLAGS) $(ROKGEMPREC) $(BOUNDS_FLAGS) $(FLAGR8)
ifeq ($(MACHINE),WIN32)
	CMNSG = ..\\..\\..\\genie-main\\src\\fortran\\cmngem\\
	MAIN_DIR = ..\\..\\..\\genie-main\\
else
	CMNSG = ../../../genie-main/src/fortran/cmngem/
	MAIN_DIR = ../../../genie-main/
endif
LOCINCPATH = $(MOD_INC_FLAG)$(CMNSG) $(MOD_INC_FLAG)$(MAIN_DIR)
LIBNAME = librokgem
INCLUDE_FILES = 
OTHER_FILES=../../../genie-main/makefile.arc Makefile

OBJECTS = rokgem_lib.$(OBJ_EXT) \
	rokgem_data.$(OBJ_EXT) \
          rokgem_data_netCDF.$(OBJ_EXT) \
          rokgem_box.$(OBJ_EXT) \
	initialise_rokgem.$(OBJ_EXT) \
	cpl_flux_rokatm.$(OBJ_EXT) \
	cpl_flux_rokocn.$(OBJ_EXT) cpl_comp_rokocn.$(OBJ_EXT) \
	rokgem.$(OBJ_EXT) end_rokgem.$(OBJ_EXT)

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: all clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.dep *.mod *__genmod.f90

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

# Pattern rules to make object files
# One rule for each compilation permutation
$(OBJS1): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(LOCINCPATH) $(F77FLAGS) \
	$< $(OBJ_FLAG)$@

$(OBJECTS): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCINCPATH) $(LOCALFLAGS) $(F90FLAGS) \
	$(FPPFLAGS) $< $(OBJ_FLAG)$@

# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = $(SHELL) -ec 'which $(PYTHON); $(PYTHON) $(MAIN_DIR)/finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.F
	$(MAKE_DEPS)

%.dep: %.f90
	$(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
-include $(OBJECTS:.$(OBJ_EXT)=.dep)
endif
