
ifeq ($(SRC), )
SRC=.
endif

ifeq ($(BUILD), )
BUILD=$(ARCH)
endif

all: exe

include Makefile.$(ARCH)

$(BUILD)/main.o: $(BOOT) $(SRC)/main.F90 
	$(FC) -o $(BUILD)/main.o -c $(SRC)/main.F90

$(BUILD)/main.x: $(BOOT) $(BUILD)/main.o 
	$(FC) -o $(BUILD)/main.x $(BUILD)/main.o  $(LIBS)


exe: $(BUILD)/main.x

subclean:
	\rm -f $(BUILD)/main.o

clean: 
	\rm -f $(BUILD)/*.o $(BUILD)/*.xml $(BUILD)/*.a $(BUILD)/*.x $(BUILD)/*.mod $(BUILD)/*.optrpt $(BUILD)/*.smod $(BUILD)/*.ok $(BUILD)/*.optrpt

tidy:
	\rm -f $(BUILD)/*.xml $(BUILD)/*.optrpt
