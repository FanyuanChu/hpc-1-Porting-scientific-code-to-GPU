module sor_routines
use sor_params
contains

type dim3
    integer :: x, y, z
end type dim3

attributes(global) subroutine sor_kernel(p0,p1,rhs) 
    use sor_params
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: p0
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out), device :: p1
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: rhs 
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

    i = (blockIdx%x - 1) * blockDim%x + threadIdx%x
    j = (blockIdx%y - 1) * blockDim%y + threadIdx%y
    k = (blockIdx%z - 1) * blockDim%z + threadIdx%z

    if (i > 0 .and. i <= im+1 .and. j > 0 .and. j <= jm+1 .and. k > 0 .and. k <= km+1) then
        if (i==im+1) then
            p1(i,j,k) = p0(i-im,j,k)
        else if (i==0) then
            p1(i,j,k) = p0(i+im,j,k)
        else if (j==jm+1) then
            p1(i,j,k)=p0(i-1,j,k)
        else if (j==0) then
            p1(i,j,k)=p0(i,j,k)
        else if (k==0) then
            p1(i,j,k)=p0(i,j,k)
        else if (k==km+1) then
            p1(i,j,k)=p0(i,j,k)
        else
            reltmp = omega*(cn1 *(cn2l*p0(i+1,j,k) + &
                cn2s*p0(i-1,j,k) +cn3l*p0(i,j+1,k) + &
                cn3s*p0(i,j-1,k) +cn4l*p0(i,j,k+1) + &
                cn4s*p0(i,j,k-1) -rhs(i,j,k))-p0(i,j,k))
            p1(i,j,k) = p0(i,j,k) +reltmp    
        end if
    end if
end subroutine sor_kernel

subroutine sor (p0,p1,rhs)
    use sor_params
    integer, parameter :: bdim = 16
    type(dim3) :: grid, block
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: p0
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out), device :: p1
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: rhs 

    block = dim3(bdim, bdim, bdim)
    grid = dim3((im+1)/bdim + 1, (jm+1)/bdim + 1, (km+1)/bdim + 1)

    call sor_kernel<<<grid, block>>>(p0, p1, rhs)
end subroutine sor
end module sor_routines
