CF = gfortran
CFLAGS= -Wall
LFLAGS= -Wall

precision.o: precision.f90
	$(CF) $(CFLAGS) -c $<

options.o: options.f90
	$(CF) $(CFLAGS) -c $<

lecture.o: lecture.f90
	$(CF) $(CFLAGS) -c $<

ecriture.o: ecriture.f90
	$(CF) $(CFLAGS) -c $<

algebre.o: algebre.f90
	$(CF) $(CFLAGS) -c $<

variablesAjustables.o: variablesAjustables.f90
	$(CF) $(CFLAGS) -c $<

spectres.o: spectres.f90
	$(CF) $(CFLAGS) -c $<

main: main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o
	$(CF) $(LFLAGS) -o $@  main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o
	
clean:
	rm  *.o
