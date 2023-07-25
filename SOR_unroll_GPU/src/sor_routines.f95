module sor_routines
    use sor_params
    contains

    attributes(global) subroutine sor(p0_d,p1_d,rhs_d)
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

        if (i==im+1) then
            p1_d(i,j,k) = p0_d(i-im,j,k)
        else if (i==0) then
            p1_d(i,j,k) = p0_d(i+im,j,k)
        else if (j==jm+1) then
            p1_d(i,j,k)=p0_d(i-1,j,k)
        else if (j==0) then
            p1_d(i,j,k)=p0_d(i,j,k)
        else if (k==0) then
            p1_d(i,j,k)=p0_d(i,j,k)
        else if (k==km+1) then
            p1_d(i,j,k)=p0_d(i,j,k)
        else
            reltmp = omega*(cn1 *(cn2l*p0_d(i+1,j,k) + cn2s*p0_d(i-1,j,k) + cn3l*p0_d(i,j+1,k) + cn3s*p0_d(i,j-1,k) + cn4l*p0_d(i,j,k+1) + cn4s*p0_d(i,j,k-1) -rhs_d(i,j,k))-p0_d(i,j,k))
            p1_d(i,j,k) = p0_d(i,j,k) +reltmp    
        end if
    end subroutine sor

    subroutine swap(A, B)
        real, dimension(:,:,:), device :: A, B
        real, dimension(:,:,:), device, allocatable :: temp

        allocate(temp(0:im+1,0:jm+1,0:km+1))

        temp = A
        A = B
        B = temp

        deallocate(temp)
    end subroutine swap

end module sor_routines
