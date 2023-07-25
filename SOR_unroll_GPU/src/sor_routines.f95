module sor_routines
use sor_params
contains

subroutine sor (p0,p1,rhs)
    use sor_params
    real, dimension(:,:,:), intent(In) :: p0
    real, dimension(:,:,:), intent(Out) :: p1
    real, dimension(:,:,:), intent(In) :: rhs 
    integer :: i,j,k

    ! Modified part
    integer, parameter :: threadsPerBlock = 256
    integer :: blocks

    ! Declare device copies of the arrays
    real, dimension(:,:,:), device, allocatable :: p0_d, p1_d, rhs_d

    ! Calculate the number of blocks
    blocks = ((im+1) * (jm+1) * (km+1) + threadsPerBlock - 1) / threadsPerBlock

    ! Allocate memory on the device
    allocate(p0_d(0:im+1,0:jm+1,0:km+1))
    allocate(p1_d(0:im+1,0:jm+1,0:km+1))
    allocate(rhs_d(0:im+1,0:jm+1,0:km+1))

    ! Copy data from host to device
    p0_d = p0
    p1_d = p1
    rhs_d = rhs

    ! Launch the kernel
    call sor_kernel<<<blocks, threadsPerBlock>>>(p0_d, p1_d, rhs_d)

    ! Make sure all threads have finished before continuing
    call cudaDeviceSynchronize()

    ! Copy results from device to host
    p1 = p1_d

    ! Free device memory
    deallocate(p0_d, p1_d, rhs_d)

end subroutine sor

attributes(global) subroutine sor_kernel(p0_d,p1_d,rhs_d)
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
end subroutine sor_kernel

end module sor_routines
