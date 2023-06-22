module module_sor_superkernel
     use singleton_module_sor_map_22 ! _split_module_per_subroutine
     use singleton_module_sor_map_46 ! _split_module_per_subroutine
     use singleton_module_sor_map_70 ! _split_module_per_subroutine
     use singleton_module_sor_superkernel ! _split_module_per_subroutine
     interface sor_map_22
       module procedure sor_map_22
     end interface sor_map_22
     interface sor_map_46
       module procedure sor_map_46
     end interface sor_map_46
     interface sor_map_70
       module procedure sor_map_70
     end interface sor_map_70
     interface sor_superkernel
       module procedure sor_superkernel
     end interface sor_superkernel
end module module_sor_superkernel
