CF = gfortran
CFLAGS= -Wall
LFLAGS= -Wall


main: main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o habillage.o
	$(CF) $(LFLAGS) -o $@  main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o

precision.o: precision.f90
	$(CF) $(CFLAGS) -c precision.f90

connex.o: connex.f90 precision.o
	$(CF) $(CFLAGS) -c connex.f90 precision.o
	
options.o: options.f90 precision.o
	$(CF) $(CFLAGS) -c options.f90  precision.o

lecture.o: lecture.f90  precision.o
	$(CF) $(CFLAGS) -c lecture.f90  precision.o

ecriture.o: ecriture.f90  precision.o  options.o
	$(CF) $(CFLAGS) -c ecriture.f90 precision.o options.o

algebre.o: algebre.f90 precision.o
	$(CF) $(CFLAGS) -c algebre.f90 precision.o

variablesAjustables.o: variablesAjustables.f90  precision.o connex.o
	$(CF) $(CFLAGS) -c variablesAjustables.f90 precision.o connex.o

spectres.o: spectres.f90 precision.o
	$(CF) $(CFLAGS) -c spectres.f90 precision.o

habillage.o: habillage.f90  precision.o
	$(CF) $(CFLAGS) -c habillage.f90 precision.o
	
test_lecture_ecriture.exec: test_lecture_ecriture.f90 ecriture.o lecture.o variablesAjustables.o precision.o options.o
	$(CF) $(LFLAGS) -o $@ test_lecture_ecriture.f90 ecriture.o lecture.o variablesAjustables.o precision.o options.o
	
test_minv.exec: test_minv.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  test_minv.f90 precision.o algebre.o
	
test_alsb.exec: test_alsb.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  test_alsb.f90 precision.o algebre.o

test_cegren.exec: test_cegren.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  test_cegren.f90 precision.o algebre.o
	
clean:
	rm  *.o *.mod
