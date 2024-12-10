# ====================================================================
#
#          === PLEASE MAKE YOUR EDITS IN THIS FILE ===
#
# These edits may include the name of your Fortran or C compiler, 
# the location of your version of the NetCDF libraries.
#
# Please note that in addition to editing this file, variables can be
# overridden on the command line, e.g.:
#       make BUILD=SHIP F77=pgf90 MACHINE=SGI
#
# Note also, however that to _ADD_ to compiler flags, such as
# LDFLAGS, one should set GENIE_LDFLAGS to your desired
# additions, e.g.:
#       make GENIE_LDFLAGS=-L/path/to/mylibs
# LDFLAGS is then seeded with GENIE_LDFLAGS--this is done
# because command line overrides are immutable. 
#
# (genie_example.job uses the command-line override machanism to
# pass compile-time configuration options through to makefile.arc
# from a .config file)
#
# ====================================================================

## === Compre the values below to those in user.sh ===
GENIE_ROOT        = $(HOME)/cgenie.cookie
OUT_DIR           = $(HOME)/cgenie_output
RUNTIME_ROOT      = ../../cgenie.cookie

# === Fortran compiler (ifc/ifort/f90/pgf90) ===
F77=gfortran
#F77=ifort
#F77=ifc
#F77=f90
#F77=f95
#F77=pgf90
#F77=pathf90
#F77=g95
#F77=f90.exe
#F77=ifort.exe
#F77=gfc.exe
#F77=g95.exe

# === C Compiler (gcc/cc) ===
CC=gcc
CXX=g++
#CXX=cl.exe
#CC=cc

# === Build type (NORMAL/SHIP/DEBUG/PROFILE/BOUNDS) ===
#BUILD=NORMAL
BUILD=SHIP
#BUILD=DEBUG
#BUILD=PROFILE
#BUILD=BOUNDS
# === Machine type (LINUX/SLOARIS/SGI) ===
MACHINE=LINUX
#MACHINE=SGI
#MACHINE=SOLARIS
#MACHINE=WIN32
#MACHINE=OSX	# Intel processor
#MACHINE=OSX_M	# Apple silicon (M1, M2 etc.)

# === Extension for module files ===
MODEXT=mod
#MODEXT=d

# === NetCDF library ===
# Please edit 'NETCDF_DIR' to point to the top-level location
# of the NetCDF library. 'NETCDF_NAME' is the name of the library file
# (minus lib prefix and .a suffix for Unix/Linux)
# The locations of the library file and the .mod file for the
# f90 interface are then determined by appending the lower-level lib
# and include directories onto this stem.
# (http://www.unidata.ucar.edu/packages/netcdf/index.html)

FNETCDF=$(shell hostname -s)
ifeq ($(FNETCDF),sterling)
  ### sterling cluster ###
  NETCDF_DIR=/share/apps
else ifeq ($(FNETCDF),eevee)
  ### eevee cluster ###
  NETCDF_DIR=/share/apps/netcdf
else
  ### DEFAULT ###
  NETCDF_DIR=/usr/local
  ### Mac (example) ###
  #NETCDF_DIR=/usr/local/Cellar/netcdf/4.9.2_1
endif

NETCDF_NAME=netcdf
