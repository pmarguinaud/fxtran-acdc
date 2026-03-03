
FLAGS=-fopenmp -fPIC -O1 -DFXTRAN_ACDC_VENDOR=FXTRAN_ACDC_AMDROCM 

FFLAGS=$(FLAGS) -fconvert=big-endian -Qunused-arguments
FC=amdflang -I $(BUILD)/fxtran-acdc/include $(FFLAGS)
CC=amdclang $(FLAGS)
CXX=amdclang++ $(FLAGS)

LIBS=-L $(BUILD)/fxtran-acdc/lib -lFxtranACDC -L/lib/x86_64-linux-gnu -lblas -lrocprofiler-sdk-roctx
