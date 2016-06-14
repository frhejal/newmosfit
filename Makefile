CF = gfortran
CFLAGS= -Wall 
LFLAGS=
EXEC=main

all: main test_minv.exec test_alsb.exec test_lecture_ecriture.exec

main: main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o habillage.o hamiltonien.o ajustement.o connex.o variablesFixes.o
	$(CF) $(LDLAGS) -o $@  $^

precision.o: precision.f90

connex.o: connex.f90 precision.o
	
options.o: options.f90 precision.o

lecture.o: lecture.f90  precision.o

ecriture.o: ecriture.f90  precision.o  options.o

algebre.o: algebre.f90 precision.o

variablesFixes.o: variablesFixes.f90 precision.o

variablesAjustables.o: variablesAjustables.f90  precision.o connex.o options.o

spectres.o: spectres.f90 precision.o habillage.o algebre.o hamiltonien.o connex.o variablesFixes.o

habillage.o: habillage.f90  precision.o options.o variablesAjustables.o
	
ajustement.o: ajustement.f90 precision.o options.o variablesAjustables.o spectres.o ecriture.o algebre.o
	
hamiltonien.o: hamiltonien.f90 precision.o options.o

old_cegren.o: old_cegren.f90 
	
%.o: %.f90
	$(CF) $(CFLAGS) -o $@ -c $<
		
test_lecture_ecriture.exec: test_lecture_ecriture.f90 ecriture.o lecture.o variablesAjustables.o precision.o options.o connex.o
	$(CF) $(LFLAGS) -o $@ $^
	
test_minv.exec: test_minv.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  $^
	
test_alsb.exec: test_alsb.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  $^

test_cegren.exec: test_cegren.f90 precision.o algebre.o old_cegren.o
	$(CF) $(LFLAGS) -o $@  $^
	
clean:
	rm  *.o *.mod
	
mrproper: clean
	rm  main
