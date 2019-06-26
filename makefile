files=engine.f90 example.f90

rmcommand=rm
ifeq ($(OS),Windows_NT)
	rmcommand=del
endif

all:
	gcc -c macros.c
	gfortran -c ncurses.f90 -fno-range-check
	gfortran $(files) ncurses.o macros.o -lncurses
	$(rmcommand) *.o *.mod
