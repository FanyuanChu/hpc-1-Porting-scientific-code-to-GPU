! sor_params.f95
module sor_params
integer, parameter :: im=1000
integer, parameter :: jm=1000
integer, parameter :: km=320
integer, parameter :: blockSize = 256  ! Block size for CUDA kernel
end module sor_params
