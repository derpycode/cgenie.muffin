
LOCALFLAGS=$(FFLAGS) $(BOUNDS_FLAGS) $(FLAGR8)
ifeq ($(MACHINE),WIN32)
	CMNSG = ..\\..\\..\\genie-main\\src\\fortran\\cmngem\\
	MAIN_DIR = ..\\..\\..\\genie-main\\
#	BIODIC = ..\\..\\..\\genie-biogem\\src\\fortran\\
#	ATCDIC = ..\\..\\..\\genie-atchem\\src\\fortran\\
else
	CMNSG = ../../../genie-main/src/fortran/cmngem/
	MAIN_DIR = ../../../genie-main/
#	BIODIR = ../../../genie-biogem/src/fortran/
#	ATCDIR = ../../../genie-biogem/src/fortran/
endif
LOCINCPATH = $(MOD_INC_FLAG)$(CMNSG) $(MOD_INC_FLAG)$(MAIN_DIR)
LIBNAME = libgemlite
INCLUDE_FILES = 
OTHER_FILES=../../../genie-main/makefile.arc Makefile

OBJECTS = gemlite_lib.$(OBJ_EXT) \
	gemlite_box.$(OBJ_EXT) gemlite_data.$(OBJ_EXT) initialise_gemlite.$(OBJ_EXT) \
	cpl_comp_gemlite.$(OBJ_EXT) \
	gemlite.$(OBJ_EXT) end_gemlite.$(OBJ_EXT)

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.dep *.mod

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
