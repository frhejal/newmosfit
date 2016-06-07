CF = gfortran
CFLAGS= -Wall 
LFLAGS= -Wall


main: main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o habillage.o hamiltonien.o ajustement.o connex.o
	$(CF) $(LFLAGS) -o $@  main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o habillage.o hamiltonien.o ajustement.o connex.o

precision.o: precision.f90
	$(CF) $(CFLAGS) -c precision.f90

connex.o: connex.f90 precision.o
	$(CF) $(CFLAGS) -c connex.f90 
	
options.o: options.f90 precision.o
	$(CF) $(CFLAGS) -c options.f90  

lecture.o: lecture.f90  precision.o
	$(CF) $(CFLAGS) -c lecture.f90  

ecriture.o: ecriture.f90  precision.o  options.o
	$(CF) $(CFLAGS) -c ecriture.f90

algebre.o: algebre.f90 precision.o
	$(CF) $(CFLAGS) -c algebre.f90 

variablesAjustables.o: variablesAjustables.f90  precision.o connex.o options.o
	$(CF) $(CFLAGS) -c variablesAjustables.f90

spectres.o: spectres.f90 precision.o habillage.o algebre.o hamiltonien.o connex.o
	$(CF) $(CFLAGS) -c spectres.f90

habillage.o: habillage.f90  precision.o options.o variablesAjustables.o
	$(CF) $(CFLAGS) -c habillage.f90
	
ajustement.o: ajustement.f90 precision.o options.o variablesAjustables.o spectres.o ecriture.o algebre.o
	$(CF) $(CFLAGS) -c ajustement.f90
	
hamiltonien.o: hamiltonien.f90 precision.o options.o
	$(CF) $(CFLAGS) -c hamiltonien.f90

old_cegren.o: old_cegren.f90 
	$(CF) $(CFLAGS) -c old_cegren.f90
	
test_lecture_ecriture.exec: test_lecture_ecriture.f90 ecriture.o lecture.o variablesAjustables.o precision.o options.o
	$(CF) $(LFLAGS) -o $@ test_lecture_ecriture.f90 ecriture.o lecture.o variablesAjustables.o precision.o options.o
	
test_minv.exec: test_minv.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  test_minv.f90 precision.o algebre.o
	
test_alsb.exec: test_alsb.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  test_alsb.f90 precision.o algebre.o

test_cegren.exec: test_cegren.f90 precision.o algebre.o old_cegren.o
	$(CF) $(LFLAGS) -o $@  test_cegren.f90 precision.o algebre.o old_cegren.o
	
clean:
	rm  *.o *.mod
