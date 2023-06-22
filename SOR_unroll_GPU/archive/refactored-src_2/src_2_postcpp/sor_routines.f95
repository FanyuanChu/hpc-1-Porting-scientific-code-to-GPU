module sor_routines
     use singleton_module_sor ! _split_module_per_subroutine
     use singleton_module_sor_kernel ! _split_module_per_subroutine
     interface sor
       module procedure sor
     end interface sor
     interface sor_kernel
       module procedure sor_kernel
     end interface sor_kernel
end module sor_routines
