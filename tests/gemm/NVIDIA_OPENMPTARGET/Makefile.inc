
FLAGS=-mp=gpu -fPIC -Minfo -gopt -Mlist -Mnofma -cuda -O1 -DFXTRAN_ACDC_VENDOR=FXTRAN_ACDC_NVIDIA -DFXTRAN_ACDC_PRAGMA=FXTRAN_ACDC_OPENMPTARGET

FFLAGS=$(FLAGS) -Minfo=accel,all,ccff -Mlarge_arrays -traceback -Mbyteswapio -Mbackslash -Mstack_arrays 
FC=pgf90 -I $(BUILD)/fxtran-acdc/include $(FFLAGS)
CC=pgcc $(FLAGS)
CXX=pgc++ $(FLAGS)

LIBS=-L $(BUILD)/fxtran-acdc/lib -lFxtranACDC -lblas -L$(NVHPC_ROOT)/math_libs/$(CUDA)/lib64 -cudalib=cublas $(NVTXLIBS)

