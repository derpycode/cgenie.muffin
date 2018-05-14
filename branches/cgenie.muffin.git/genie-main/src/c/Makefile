# $Id$

ifneq ($(MACHINE),WIN32)
	CXXFLAGS += -I$(NETCDF_DIR)/include
	LDFLAGSNC  += -L$(NETCDF_DIR)/lib -lnetcdf_c++ -lnetcdf -lm
else
	CXXFLAGS += /I$(NETCDF_DIR)\\include /EHsc /DWIN32 /TP
	LDFLAGSNC  += /link /LIBPATH:$(NETCDF_DIR)\\lib netcdf_c++.lib netcdf.lib
endif

SRCS=compare.cpp
# Windows compilers append .exe regardless <sigh>
# Rather than fight the tide, we'll append this always
EXES=nccompare.exe

ifeq ($(MACHINE),WIN32)
	SRCS += getopt.cpp
endif

OBJS=$(patsubst %.cpp, %.$(OBJ_EXT), $(SRCS))

all: $(EXES)

$(OBJS): %.$(OBJ_EXT) : %.cpp
ifeq ($(MACHINE),WIN32)
	cp $(NETCDF_DIR)\\src\\win32\\NET\\getopt.c getopt.c
	sed 's/strchr/(char *) strchr/g' getopt.c > getopt.cpp
endif
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(EXES): $(OBJS)
	$(CXX) -o $@ $(OBJS) $(LDFLAGSNC)

.PHONY: clean
clean:
	rm -rf $(OBJS) $(EXES) 
