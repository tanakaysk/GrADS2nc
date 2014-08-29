# makefile for ncmake (conversion of plain binary data to netCDF)

# system environmente
include env.mk
# compiler settings
include compiler.mk
# tool specific macros
include ncmake.mk

vpath %.f90 $(CPPFD)

# List of executables
EXECS = ncmake_CF16

.PHONY : all clean

all : $(addprefix $(EXECD)/,$(EXECS))

# Linker
$(EXECD)/ncmake_CF16 : ncmake_CF16.o nclib.o cnst.o
	$(FC) $(NC_INC) $(LDFLAGS) $(NC_LD) -o $@ $^ $(NC_LIB)

# Main program
ncmake_CF16.o : ncmake_CF16.f90 nclib.o cnst.o
	$(FC) $(NC_INC) $(FFLAGS) -o $@ $<

# common development platforms
include $(CPPFD)/CPPF.mk

clean :
	rm -f $(addprefix $(EXECD)/,$(EXECS)) *.o *.mod *~ \#*
