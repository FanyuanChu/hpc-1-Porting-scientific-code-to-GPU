module module_sor_superkernel
     use singleton_module_sor_map_21 ! _split_module_per_subroutine
     use singleton_module_sor_map_45 ! _split_module_per_subroutine
     use singleton_module_sor_superkernel ! _split_module_per_subroutine
     interface sor_map_21
       module procedure sor_map_21
     end interface sor_map_21
     interface sor_map_45
       module procedure sor_map_45
     end interface sor_map_45
     interface sor_superkernel
       module procedure sor_superkernel
     end interface sor_superkernel
end module module_sor_superkernel
