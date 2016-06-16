DEBUG = no
COMPILER_UNIX = gfortran
COMPILER_WIN = gfortran

ifeq ($(DEBUG),yes)
	CFLAGS = -Wall -W -g
	LFLAGS =
else
	CFLAGS = 
	LFLAGS =
endif
# Pour faire fonctionner clean et mrproper dans windows :
ifeq ($(OS),Windows_NT)
	 CF = $(COMPILER_WIN)
   RM = del /Q
   #remplacement des slash par des antislash dans les chemins
   FixPath = $(subst /,\,$1)
   BIN = mosfit2016.exe
else
   ifeq ($(shell uname), Linux)
			CF = $(COMPILER_UNIX)
      RM = rm -f
      FixPath = $1
      BIN = mosfit2016
   endif
endif

EXEC=main

all: mosfit test_minv.exec test_alsb.exec test_lecture_ecriture.exec
ifeq ($(DEBUG),yes)
	@echo "Génération en mode debug"
endif

mosfit: main.f90 precision.o ecriture.o lecture.o options.o algebre.o variablesAjustables.o spectres.o habillage.o hamiltonien.o ajustement.o connex.o variablesFixes.o
	$(CF) $(LDLAGS) -o $(BIN)  $^

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
	 $(RM) $(call FixPath,*.o *.mod)

mrproper: clean
	$(RM)  $(BIN)
