
FLAGS=-fopenmp --offload-arch=gfx942 -fPIC -O1 -DFXTRAN_ACDC_VENDOR=FXTRAN_ACDC_AMDROCM -DFXTRAN_ACDC_PRAGMA=FXTRAN_ACDC_OPENMPTARGET -I$(ROCM_PATH)/include/hipfort/amdgcn

FFLAGS=$(FLAGS) -fconvert=big-endian -Qunused-arguments
FC=amdflang -I $(BUILD)/fxtran-acdc/include $(FFLAGS)
CC=amdclang $(FLAGS)
CXX=amdclang++ $(FLAGS)

LIBS=-L $(BUILD)/fxtran-acdc/lib -lFxtranACDC -L/lib/x86_64-linux-gnu -lblas -lrocblas -lamdhip64 -lhipfort-amdgcn


