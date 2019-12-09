gfortran -c prak.f90 tests.f90 assertions.f90 utils.f90 print_funcs.f90
gfortran -o executable prak.o tests.o assertions.o utils.o print_funcs.f90
rm *.o
./executable
