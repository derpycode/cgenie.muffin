#
# $Id$
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(BOUNDS_FLAGS) $(FLAGR8)

LIBNAME = libnc1

OBJECTS= close_file_nc.$(OBJ_EXT) find_dim_nc.$(OBJ_EXT) \
	get1d_data_nc.$(OBJ_EXT) get1d_data_point_nc.$(OBJ_EXT) \
	get2d_data_nc.$(OBJ_EXT) get2d_data_slice_nc.$(OBJ_EXT) \
	get1d_slice_comp_nc.$(OBJ_EXT) get2d_slice_comp_nc.$(OBJ_EXT) \
	get3d_data_nc.$(OBJ_EXT) get3d_data_slice_nc.$(OBJ_EXT) \
	get3d_slice_comp_nc.$(OBJ_EXT) get4d_data_nc.$(OBJ_EXT) \
	length_of_char_nc.$(OBJ_EXT) open_file_nc.$(OBJ_EXT) \
	read1d_comp_nc.$(OBJ_EXT) read2d_nc.$(OBJ_EXT) \
	read3d_slice_nc.$(OBJ_EXT) writenc6.$(OBJ_EXT) extras.$(OBJ_EXT)

OTHER_FILES=netcdf.inc makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

close_file_nc.$(OBJ_EXT)		: close_file_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) close_file_nc.f

find_dim_nc.$(OBJ_EXT)		: find_dim_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) find_dim_nc.f

get1d_data_nc.$(OBJ_EXT)		: get1d_data_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get1d_data_nc.f

get1d_data_point_nc.$(OBJ_EXT)		: get1d_data_point_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get1d_data_point_nc.f

get2d_data_nc.$(OBJ_EXT)		: get2d_data_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get2d_data_nc.f

get2d_data_slice_nc.$(OBJ_EXT)	: get2d_data_slice_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get2d_data_slice_nc.f

get1d_slice_comp_nc.$(OBJ_EXT)	: get1d_slice_comp_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get1d_slice_comp_nc.f

get2d_slice_comp_nc.$(OBJ_EXT)	: get2d_slice_comp_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get2d_slice_comp_nc.f

get3d_data_nc.$(OBJ_EXT)		: get3d_data_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get3d_data_nc.f

get3d_data_slice_nc.$(OBJ_EXT)	: get3d_data_slice_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get3d_data_slice_nc.f

get3d_slice_comp_nc.$(OBJ_EXT)	: get3d_slice_comp_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get3d_slice_comp_nc.f

get4d_data_nc.$(OBJ_EXT)		: get4d_data_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) get4d_data_nc.f

length_of_char_nc.$(OBJ_EXT)	: length_of_char_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) length_of_char_nc.f

open_file_nc.$(OBJ_EXT)		: open_file_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) open_file_nc.f

read1d_comp_nc.$(OBJ_EXT)	: read1d_comp_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) read1d_comp_nc.f

read2d_nc.$(OBJ_EXT)		: read2d_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) read2d_nc.f

read3d_slice_nc.$(OBJ_EXT)	: read3d_slice_nc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) read3d_slice_nc.f

writenc6.$(OBJ_EXT)	: writenc6.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) writenc6.f

extras.$(OBJ_EXT)	: extras.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) extras.f
