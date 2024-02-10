
EXE = nccompare.exe
SRC = nccompare.f90
OBJ = $(SRC:.f90=.o)

$(EXE): $(OBJ)
	$(F77) $(OBJ) $(NETCDF) -o $(EXE) 
	
%.o: %.f90
	$(F77) $(F90FLAGS) $(NETCDF_INC) -c $<

all: $(EXE)

.PHONY: clean

clean:
	rm -f *.o *.mod *.MOD $(EXE)

