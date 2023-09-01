# hpc-1-Porting-scientific-code-to-GPU

###SOR_unroll_GPU
The SOR_unroll_GPU folder is the original Fortran code provided by the tutor. It contains src/ (this is the reference code) and unroll_2/, unroll_3/, unroll_4/ (these three are memory reduction code codes). 
To compile this code, you need to use the gfortran compiler and the scons compilation system.


###SOR_unroll_GPU_CDUA
SOR_unroll_GPU_CDUA folder is the folder where the project is ported to GPU.
The folders in it correspond to the folders in SOR_unroll_GPU.
To compile this code, you need to install WSL, CUDA Toolkit 12.2 and the corresponding NVIDIA HPC SDK, and use an NVIDIA GPU.
Also, the nvfortran compiler is required.

For the reference code src folder, the compile command is:
nvfortran -cuda -gpu=cuda12.2 -o output sor_params.f95 sor_routines.f95 test_sor_unroll_time.f95;

For the memory reduction code unroll_2, unroll_3 and unroll_4 folders, the compile command is:
nvfortran -cuda -gpu=cuda12.2 -c stage_kernel_1.cuf;
nvfortran -cuda -gpu=cuda12.2 -c sor_superkernel.cuf;
nvfortran -cuda -gpu=cuda12.2 -o output gen_sor_superkernel_time.f95 sor_superkernel.o stage_kernel_1.o;

The UNROLL value in the src folder can be modified to a multiple of 12.
The values of im, jm, km in each folder can be modified.
