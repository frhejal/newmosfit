DEBUG=no
CF = gfortran
ifeq ($(DEBUG),yes)
	CFLAGS= -Wall -W -g
	LFLAGS=
else
	CFLAGS= 
	LFLAGS=
endif

EXEC=main

all: main test_minv.exec test_alsb.exec test_lecture_ecriture.exec
ifeq ($(DEBUG),yes)
	@echo "Génération en mode debug"
endif

main: main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o habillage.o hamiltonien.o ajustement.o connex.o variablesFixes.o
	$(CF) $(LDLAGS) -o $@  $^

connex.o: precision.o

options.o: precision.o

lecture.o: precision.o

ecriture.o: precision.o  options.o

algebre.o: precision.o

variablesFixes.o: precision.o

variablesAjustables.o: precision.o connex.o options.o

spectres.o: precision.o habillage.o algebre.o hamiltonien.o connex.o variablesFixes.o

habillage.o: precision.o options.o variablesAjustables.o

ajustement.o: precision.o options.o variablesAjustables.o spectres.o ecriture.o algebre.o

hamiltonien.o: precision.o options.o

%.o: %.f90
	$(CF) $(CFLAGS) -o $@ -c $<

test_lecture_ecriture.exec: test_lecture_ecriture.f90 ecriture.o lecture.o variablesAjustables.o precision.o options.o connex.o variablesFixes.o
	$(CF) $(LFLAGS) -o $@ $^

test_minv.exec: test_minv.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  $^

test_alsb.exec: test_alsb.f90 precision.o algebre.o
	$(CF) $(LFLAGS) -o $@  $^

test_cegren.exec: test_cegren.f90 precision.o algebre.o old_cegren.o
	$(CF) $(LFLAGS) -o $@  $^

.PHONY: clean mrproper

clean:
	rm  *.o *.mod

mrproper: clean
	rm  main
