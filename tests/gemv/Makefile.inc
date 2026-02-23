
FLAGS=-mp -fPIC -Minfo -gopt -Mlist -Mnofma -cuda -acc=gpu -O1

FFLAGS=$(FLAGS) -Minfo=accel,all,ccff -Mlarge_arrays -traceback -Mbyteswapio -Mbackslash -Mstack_arrays
FC=pgf90 -I fxtran-acdc/include $(FFLAGS)
CC=pgcc $(FLAGS)
CXX=pgc++ $(FLAGS)

LIBS=-L fxtran-acdc/lib -lFxtranACDC -lblas -L$(NVHPC_ROOT)/math_libs/12.5/lib64 -cudalib=cublas

boot:
	../../bin/fxtran-boot --prefix fxtran-acdc -CXX "$(CXX)" -CC "$(CC)" -F90 "$(FC)"

