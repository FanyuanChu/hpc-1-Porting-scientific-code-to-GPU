#!/usr/bin/env perl
use strict;
use warnings;
my @files_to_delete = qw(
./refactored-src_4/src_4_postcpp/sor.o
./refactored-src_4/src_4_postcpp/test_sor_unroll.o
./refactored-src_4/src_4_postcpp/sor_routines.o
./refactored-src_4/src_4_postcpp/sor_params.o
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/module_gen_sor_superkernel.o
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/gen_sor_superkernel.o
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Scalarized/sor_map_47.o
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Scalarized/sor_map_95.o
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Scalarized/sor_map_71.o
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Scalarized/sor_map_23.o
./refactored-src_4/src_4_postcpp/sor_superkernel.o
./refactored-src_4/src_4_postcpp/sor_kernel.o
./refactored-src_3/src_3_postcpp/sor.o
./refactored-src_3/src_3_postcpp/test_sor_unroll.o
./refactored-src_3/src_3_postcpp/sor_routines.o
./refactored-src_3/src_3_postcpp/sor_params.o
./refactored-src_3/src_3_postcpp/sor_superkernel.o
./refactored-src_3/src_3_postcpp/sor_kernel.o
./src_4_postcpp/test_sor_unroll.o
./src_4_postcpp/sor_routines.o
./src_4_postcpp/sor_params.o
./patched_autopar_2/mem_reduced_inlined/Generated/Patched/sor_superkernel.o
./patched_autopar_2/mem_reduced_inlined/Generated/Patched/stage_kernel_1.o
./patched_autopar_2/mem_reduced_inlined/Generated/Patched/gen_sor_superkernel.o
./patched_autopar_4/mem_reduced_inlined/Generated/Patched/sor_superkernel.o
./patched_autopar_4/mem_reduced_inlined/Generated/Patched/stage_kernel_1.o
./patched_autopar_4/mem_reduced_inlined/Generated/Patched/gen_sor_superkernel.o
./patched_autopar_3/MemoryReduction/Generated/module_gen_sor_superkernel.o
./patched_autopar_3/MemoryReduction/Generated/gen_sor_superkernel.o
./patched_autopar_3/MemoryReduction/Scalarized/sor_map_22.o
./patched_autopar_3/MemoryReduction/Scalarized/sor_map_70.o
./patched_autopar_3/MemoryReduction/Scalarized/sor_map_46.o
./patched_autopar_3/mem_reduced_inlined/Generated/Patched/sor_superkernel.o
./patched_autopar_3/mem_reduced_inlined/Generated/Patched/stage_kernel_1.o
./patched_autopar_3/mem_reduced_inlined/Generated/Patched/gen_sor_superkernel.o
./src/test_sor_unroll.o
./src/sor_routines.o
./src/sor_params.o
./refactored-src_4/src_4_postcpp/singleton_module_src_4_postcpp_sor_superkernel.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/module_sor_superkernel_superkernel.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/Scalarized/singleton_module_sor_map_23.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/module_sor_superkernel_superkernel.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/singleton_module_sor_map_47.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/singleton_module_sor_map_95.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/singleton_module_sor_map_23.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/module_sor_superkernel.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Generated/singleton_module_sor_map_71.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Scalarized/singleton_module_sor_map_95.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/MemoryReduction/Scalarized/singleton_module_sor_map_23.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/mem_reduced_inlined/Generated/singleton_module_sor_superkernel.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/mem_reduced_inlined/Scalarized/singleton_module_sor_map_23.mod
./refactored-src_4/src_4_postcpp/Autopar_GPU/Patched/module_sor_superkernel.mod
./refactored-src_4/src_4_postcpp/singleton_module_sor_kernel.mod
./refactored-src_4/src_4_postcpp/sor_routines.mod
./refactored-src_4/src_4_postcpp/singleton_module_sor.mod
./refactored-src_4/src_4_postcpp/sor_params.mod
./refactored-src_3/src_3_postcpp/singleton_module_src_3_postcpp_sor_superkernel.mod
./refactored-src_3/src_3_postcpp/singleton_module_sor_kernel.mod
./refactored-src_3/src_3_postcpp/sor_routines.mod
./refactored-src_3/src_3_postcpp/singleton_module_sor.mod
./refactored-src_3/src_3_postcpp/sor_params.mod
./src_4_postcpp/sor_routines.mod
./src_4_postcpp/sor_params.mod
./patched_autopar_2/mem_reduced_inlined/Generated/Patched/singleton_module_stage_kernel_1.mod
./patched_autopar_2/mem_reduced_inlined/Generated/Patched/singleton_module_sor_superkernel.mod
./patched_autopar_4/mem_reduced_inlined/Generated/Patched/singleton_module_stage_kernel_1.mod
./patched_autopar_4/mem_reduced_inlined/Generated/Patched/singleton_module_sor_superkernel.mod
./patched_autopar_3/MemoryReduction/Generated/singleton_module_sor_map_46.mod
./patched_autopar_3/MemoryReduction/Generated/singleton_module_sor_map_22.mod
./patched_autopar_3/MemoryReduction/Generated/singleton_module_sor_map_70.mod
./patched_autopar_3/MemoryReduction/Generated/module_sor_superkernel.mod
./patched_autopar_3/mem_reduced_inlined/Generated/Patched/singleton_module_stage_kernel_1.mod
./patched_autopar_3/mem_reduced_inlined/Generated/Patched/singleton_module_sor_superkernel.mod
./src/sor_routines.mod
./src/sor_params.mod
./patched_autopar_2/mem_reduced_inlined/Generated/Patched/prog.exe
./patched_autopar_4/mem_reduced_inlined/Generated/Patched/prog.exe
./patched_autopar_3/mem_reduced_inlined/Generated/Patched/prog.exe
);

for my $file (@files_to_delete) {
unlink $file;
}