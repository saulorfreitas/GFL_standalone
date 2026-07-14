# Novo Makefile, preparado para compilar com gfortran e intel:
#
# Usage: make usage, ou apenas make
#
#			make gnu		:: para compilar com gfortran
#			make intel  :: para compilar com ifort
#
# Carlos Renato
# 2023/mar
#-------------------------------------------------------------------

BIN=./
SRC=../src

BASE=gfl
EXE=$(BASE).x
ARC=$(BASE).a

# Opcoes para o compilador gfortran:
#GNU_OPTS=-O0 -ffree-form -ffree-line-length-none #-fdefault-real-8
GNU_OPTS=-O0 -ffree-form -fno-range-check -ffree-line-length-none -fallow-argument-mismatch \
 -Wall -fbacktrace -g -fcheck=bounds -fcheck=all -finit-real=nan -unused

# Opcoes para o compilador ifort:
INTEL_OPTS=-O0 -g -check bounds -traceback -check uninit -r8 -free -Wl,-ld_classic
INTEL_OPTS=-O0 -g -check bounds -traceback -check uninit     -free -Wl,-ld_classic

PGI_OPTS=-O0
dummy:
	@$(MAKE) usage --no-print-directory

gnu:
	@( $(MAKE) all --no-print-directory \
	"COMP = gfortran" \
	"F_OPTS = $(GNU_OPTS)" )

intel:
	@( $(MAKE) all --no-print-directory \
	"COMP = ifort" \
	"F_OPTS = $(INTEL_OPTS)" )

pgi:
	@( $(MAKE) all --no-print-directory \
	"COMP = pgf90" \
	"F_OPTS = $(PGI_OPTS)" )

all:	$(BIN)/$(EXE)

$(BIN)/$(EXE):	$(BIN)/$(ARC)
	$(COMP) $(F_OPTS) $< -o $@

#$(BIN)/$(ARC):	$(BIN)/1d-gf-test.o $(BIN)/modConvParGF.o $(BIN)/modConstants.o $(BIN)/modGate.o $(BIN)/modHenrysLawCts.o $(BIN)/modVector.o 
#	ar rs $@ $(BIN)/*.o

$(BIN)/$(ARC):	$(BIN)/main_1d_gfl.o $(BIN)/modConstants.o $(BIN)/modGate.o $(BIN)/modHenrysLawCts.o $(BIN)/modVector.o $(BIN)/module_cu_gf.monan.o
	ar rs $@ $(BIN)/*.o

$(BIN)/main_1d_gfl.o: $(SRC)/main_1d_gfl.f90 $(BIN)/modGate.o $(BIN)/modConstants.o $(BIN)/module_cu_gf.monan.o
	$(COMP) $(F_OPTS) -c $< -o $@	

$(BIN)/module_cu_gf.monan.o: $(SRC)/module_cu_gf.monan.F $(BIN)/modConstants.o $(BIN)/modGate.o $(BIN)/modHenrysLawCts.o $(BIN)/modVector.o
	$(COMP) $(F_OPTS) -c $< -o $@

$(BIN)/modConstants.o:	$(SRC)/modConstants.F
	$(COMP) $(F_OPTS) -c $< -o $@

$(BIN)/modGate.o:	$(SRC)/modGate.F
	$(COMP) $(F_OPTS) -c $< -o $@

$(BIN)/modHenrysLawCts.o:	$(SRC)/modHenrysLawCts.F
	$(COMP) $(F_OPTS) -c $< -o $@

$(BIN)/modVector.o:	$(SRC)/modVector.F
	$(COMP) $(F_OPTS) -c $< -o $@

clean:
	rm -f $(BIN)/*.x $(BIN)/*.o $(BIN)/*.a $(BIN)/*.mod

usage:
	@echo ""
	@echo "Usage: $(MAKE) <target> "
	@echo ""
	@echo "Example targets:"
	@echo "                 gnu"
	@echo "                 intel"
	@echo ""
