#
#
PROG =	emepctm
###################################################

include Makefile.SRCS

###################################################

# prefered netCDF 4.2.1.1 or later
LIBS = -lnetcdff
#use explicit pathes if nc-config does not work
INCL = -I/global/hds/software/cpu/eb3/netCDF-Fortran/4.4.4-foss-2017a-HDF5-1.8.18/include
LLIB = -L/global/hds/software/cpu/eb3/netCDF-Fortran/4.4.4-foss-2017a-HDF5-1.8.18/lib

# options by nc-config/nf-config utility
INCL = $(shell nc-config --fflags)
LLIB = $(shell nc-config --flibs)

F90 = mpif90

# Intel ifort compiler
F90FLAGS = -shared-intel -r8 -recursive -O2


# GNU gfortran compiler (version 4.4.3 or later)
#comment out if you use intel compiler
F90FLAGS = -ffree-line-length-none -fdefault-real-8 -fdefault-double-8 -O3

###################################################


LDFLAGS = $(F90FLAGS) $(LLIB) -o $(PROG) $(FOBJ) $(INCL) $(LIBS)


.SUFFIXES: $(SUFFIXES)  .f90

.f90.o:
	$(F90) $(F90FLAGS) $(INCL) -c $<


all:  $(PROG)


# Include the dependency-list (created by makedepf90)
include dependencies

$(PROG): $(FOBJ)
	 $(F90) $(LDFLAGS)
#

clean: diskclean

diskclean:
	rm -f $(PROG) *.o *.mod

##########################################################
