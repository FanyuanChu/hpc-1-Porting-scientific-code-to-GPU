module module_sor_superkernel
     use singleton_module_sor_map_23 ! _split_module_per_subroutine
     use singleton_module_sor_map_47 ! _split_module_per_subroutine
     use singleton_module_sor_map_71 ! _split_module_per_subroutine
     use singleton_module_sor_map_95 ! _split_module_per_subroutine
     use singleton_module_sor_superkernel ! _split_module_per_subroutine
     interface sor_map_23
       module procedure sor_map_23
     end interface sor_map_23
     interface sor_map_47
       module procedure sor_map_47
     end interface sor_map_47
     interface sor_map_71
       module procedure sor_map_71
     end interface sor_map_71
     interface sor_map_95
       module procedure sor_map_95
     end interface sor_map_95
     interface sor_superkernel
       module procedure sor_superkernel
     end interface sor_superkernel
end module module_sor_superkernel
