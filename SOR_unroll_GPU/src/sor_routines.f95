module sor_routines
    use cudafor
    use sor_params

    contains

    attributes(global) subroutine sor_kernel(p0,p1,rhs) 
        real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(In) :: p0
        real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(Out) :: p1
        real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(In) :: rhs 
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

        if (i > 0 .and. i < im+1 .and. j > 0 .and. j < jm+1 .and. k > 0 .and. k < km+1) then
            ! the core
            ! The actual SOR expression
            reltmp = omega*(cn1 *(cn2l*p0(i+1,j,k) + &
                cn2s*p0(i-1,j,k) +cn3l*p0(i,j+1,k) + &
                cn3s*p0(i,j-1,k) +cn4l*p0(i,j,k+1) + &
                cn4s*p0(i,j,k-1) -rhs(i,j,k))-p0(i,j,k))
            p1(i,j,k) = p0(i,j,k) +reltmp 
        else
            p1(i,j,k) = p0(i,j,k)
        end if
    end subroutine sor_kernel
    
    subroutine sor (p0,p1,rhs)
        use cudafor
        use sor_params
        implicit none
        real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(In) :: p0
        real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(Out) :: p1
        real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(In) :: rhs
        type(dim3) :: threads, blocks

        threads = dim3(16,16,4)  ! adjust this to your needs
        blocks = dim3((im+2+threads%x-1)/threads%x, (jm+2+threads%y-1)/threads%y, (km+2+threads%z-1)/threads%z)

        call sor_kernel<<<blocks, threads>>>(p0, p1, rhs)
        cudaDeviceSynchronize()
    end subroutine sor


end module sor_routines
