! sor_params.f95
module sor_params
integer, parameter :: im=1000
integer, parameter :: jm=1000
integer, parameter :: km=320
! New parameters to define the block size
integer, parameter :: tx = 16
integer, parameter :: ty = 16
integer, parameter :: tz = 16
end module sor_params                                                                                                                                                                  ! sor_routines.f95
module sor_routines
    use sor_params
    contains

    attributes(global) subroutine sor(p0_d, p1_d, rhs_d)
        use sor_params
        real, dimension(:,:,:), device :: p0_d, p1_d, rhs_d
        integer :: i,j,k
        integer :: index
        real(kind=4), parameter :: cn1 = 1.0/3.0
        real(kind=4), parameter :: cn2l = 0.5
        real(kind=4), parameter :: cn2s = 0.5
        real(kind=4), parameter :: cn3l = 0.5
        real(kind=4), parameter :: cn3s = 0.5
        real(kind=4), parameter :: cn4l = 0.5
        real(kind=4), parameter :: cn4s = 0.5
        real, parameter  :: omega = 1.0
        real :: reltmp

        index = blockDim%x * blockIdx%x + threadIdx%x

        if (index > (im+1) * (jm+1) * (km+1)) return

        k = index / ((im+1) * (jm+1))
        j = (index - k * (im+1) * (jm+1)) / (im+1)
        i = index - k * (im+1) * (jm+1) - j * (im+1)

        if (i > 0 .and. i < im+1 .and. j > 0 .and. j < jm+1 .and. k > 0 .and. k < km+1) then
            reltmp = omega*(cn1 *(cn2l*p0_d(i+1,j,k) + cn2s*p0_d(i-1,j,k) + cn3l*p0_d(i,j+1,k) + cn3s*p0_d(i,j-1,k) + cn4l*p0_d(i,j,k+1) + cn4s*p0_d(i,j,k-1) -rhs_d(i,j,k))-p0_d(i,j,k))
            p1_d(i,j,k) = p0_d(i,j,k) + reltmp
        end if
    end subroutine sor

end module sor_routines
