files=engine.f90 example.f90

rmcommand=rm
ifeq ($(OS),Windows_NT)
	rmcommand=del
endif

all:
	gcc -c macros.c
	gfortran -c ncurses.f90 -fno-range-check
	gfortran -O3 $(files) ncurses.o macros.o -lncurses -Wall -fcheck=all
	$(rmcommand) *.o *.mod
