include ../../makefile.arc

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif

LOCALFLAGS = $(FFLAGS) $(GENIEPREC) $(BOUNDS_FLAGS) $(LOC_INC_FLAGS)

MODULE_NAMES=embm goldstein goldsteinseaice

# Construct the linking params
ifeq ($(MACHINE),WIN32) 

  # construct lib names for linking
  LINK_LIBRARIES = $(MODULE_NAMES:%=/LIBPATH:$(GENIE_ROOT_WIN)\\genie-%\\lib)
  LINK_LIBRARIES += $(MODULE_NAMES:%=lib%.$(LIB_EXT))

  # include the netcdf library
  LINK_LIBRARIES += /LIBPATH:$(GENIE_ROOT_WIN)\\genie-lib\\libnc1 libnc1.lib

  # include the util library
  LINK_LIBRARIES += /LIBPATH:$(GENIE_ROOT_WIN)\\genie-lib\\libutil1 libutil1.lib
  
else
  # construct lib names for linking
  LINK_LIBRARIES = $(MODULE_NAMES:%=-L$(GENIE_ROOT)/genie-%/lib)
  LINK_LIBRARIES += $(MODULE_NAMES:%=-l%)

  # include the netcdf library
  LINK_LIBRARIES += -L$(GENIE_ROOT)/genie-lib/libnc1 -lnc1

  # include the util library
  LINK_LIBRARIES += -L$(GENIE_ROOT)/genie-lib/libutil1 -lutil1

endif


EA_GO_GS_OBJS=errfn_ea_go_gs.$(OBJ_EXT)
EA_GO_GS_AC_BG_OBJS=errfn_ea_go_gs_ac_bg.$(OBJ_EXT)
IG_FI_FI_OBJS=errfn_ig_fi_fi_ml.$(OBJ_EXT)

EXES=errfn_ea_go_gs.exe errfn_ea_go_gs_ac_bg.exe errfn_ig_fi_fi_ml.exe costfn.exe

# force linking step, as currently not all dependencies are checked
all: clean_exe $(EXES) Makefile

$(EA_GO_GS_OBJS): %.$(OBJ_EXT) : %.f90
	$(F77) $(F90FLAGS) $(FPPFLAGS) $(LOCALFLAGS) $(COMPILEONLY) $(DOPTS) \
	$(GOLDSTEINNLONSOPTS) $(GOLDSTEINNLATSOPTS) $(GOLDSTEINNLEVSOPTS) \
	$< $(OBJ_FLAG)$@

$(EA_GO_GS_AC_BG_OBJS): %.$(OBJ_EXT) : %.f90
	$(F77) $(F90FLAGS) $(FPPFLAGS) $(LOCALFLAGS) $(COMPILEONLY) $(DOPTS) \
	$(GOLDSTEINNLONSOPTS) $(GOLDSTEINNLATSOPTS) $(GOLDSTEINNLEVSOPTS) \
	$< $(OBJ_FLAG)$@

$(IG_FI_FI_OBJS): %.$(OBJ_EXT) : %.f90
	$(F77) $(F90FLAGS) $(FPPFLAGS) $(LOCALFLAGS) $(COMPILEONLY) $(DOPTS) $< $(OBJ_FLAG)$@

costfn.exe: $(EA_GO_GS_OBJS)
	$(F77_LD) $(OUT_FLAG)$@ $(EA_GO_GS_OBJS) $(LDFLAGS) $(LINK_LIBRARIES) $(NETCDF) ../../genie_util.$(OBJ_EXT) ../../genie_global.$(OBJ_EXT) ../../genie_control.$(OBJ_EXT) ../../local_netcdf.$(OBJ_EXT)

errfn_ea_go_gs.exe: $(EA_GO_GS_OBJS)
	$(F77_LD) $(OUT_FLAG)$@ $(EA_GO_GS_OBJS) $(LDFLAGS) $(LINK_LIBRARIES) $(NETCDF) ../../genie_util.$(OBJ_EXT) ../../genie_global.$(OBJ_EXT) ../../genie_control.$(OBJ_EXT) ../../local_netcdf.$(OBJ_EXT)

errfn_ea_go_gs_ac_bg.exe: $(EA_GO_GS_AC_BG_OBJS)
	$(F77_LD) $(OUT_FLAG)$@ $(EA_GO_GS_AC_BG_OBJS) $(LDFLAGS) $(LINK_LIBRARIES) $(NETCDF)

errfn_ig_fi_fi_ml.exe: $(IG_FI_FI_OBJS)
	$(F77_LD) $(OUT_FLAG)$@ $(IG_FI_FI_OBJS) $(LDFLAGS) $(LINK_LIBRARIES) $(NETCDF)

.PHONY: clean clean_exe
clean:
	rm -rf $(EA_GO_GS_OBJS) $(EA_GO_GS_AC_BG_OBJS) $(IG_FI_FI_OBJS) $(EXES) 

clean_exe:
	rm -rf $(EXES) 
