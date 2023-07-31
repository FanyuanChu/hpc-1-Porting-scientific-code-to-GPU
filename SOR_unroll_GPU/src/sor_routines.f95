module sor_routines
    use cudafor
    use sor_params

    contains

    attributes(global) subroutine sor_kernel(p0,p1,rhs) 
        use sor_params
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: p0
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: p1
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: rhs 
        integer :: i,j,k
        real(kind=4), parameter :: cn1 = 1.0/3.0
        real(kind=4), parameter :: cn2l = 0.5
        real(kind=4), parameter :: cn2s = 0.5
        real(kind=4), parameter :: cn3l = 0.5
        real(kind=4), parameter :: cn3s = 0.5
        real(kind=4), parameter :: cn4l = 0.5
        real(kind=4), parameter :: cn4s = 0.5
        real, parameter  :: omega = 1.0
        real :: reltmp

        i = threadIdx%x + (blockIdx%x - 1) * blockDim%x
        j = threadIdx%y + (blockIdx%y - 1) * blockDim%y
        k = threadIdx%z + (blockIdx%z - 1) * blockDim%z

        ! ... rest of your code here ...
    end subroutine sor_kernel

    subroutine sor (p0,p1,rhs)
        use sor_params
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: p0
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: p1
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: rhs 
        type(dim3) :: threads, blocks

        threads%x = 16
        threads%y = 16
        threads%z = 4
        blocks%x = (im + threads%x - 1) / threads%x
        blocks%y = (jm + threads%y - 1) / threads%y
        blocks%z = (km + threads%z - 1) / threads%z

        call sor_kernel<<<blocks,threads>>>(p0, p1, rhs)
        call cudaDeviceSynchronize()
    end subroutine sor
end module sor_routines
