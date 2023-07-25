module sor_routines
use sor_params
contains

subroutine sor (p0,p1,rhs)
    use sor_params    
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: p0
real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: p1
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: rhs 
integer :: i,j,k
! Modified part
integer, parameter :: threadsPerBlock = 256
integer :: blocks

! Calculate the number of blocks
! Modified part
blocks = ((im+1) * (jm+1) * (km+1) + threadsPerBlock - 1) / threadsPerBlock

! Launch the kernel
! Modified part
call sor_kernel<<<blocks, threadsPerBlock>>>(p0, p1, rhs)

! Make sure all threads have finished before continuing
! Modified part
call cudaDeviceSynchronize()

end subroutine sor
! Modified part
attributes(global) subroutine sor_kernel(p0,p1,rhs)
    use sor_params
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: p0
real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: p1
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: rhs 
integer :: i,j,k
! Modified part
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
!Modified part
index = blockDim%x * blockIdx%x + threadIdx%x
!Modified part
if (index > (im+1) * (jm+1) * (km+1)) return

!Modified part
k = index / ((im+1) * (jm+1))
j = (index - k * (im+1) * (jm+1)) / (im+1)
i = index - k * (im+1) * (jm+1) - j * (im+1)

! Rest of the code
if (i==im+1) then
! circular
! i=im+1
p1(i,j,k) = p0(i-im,j,k)
else if (i==0) then
! i=0
! circular
p1(i,j,k) = p0(i+im,j,k)
else if (j==jm+1) then
! open
! j = jm+1
p1(i,j,k)=p0(i-1,j,k)
else if (j==0) then
! fixed
! j = 0
! We keep the original values
p1(i,j,k)=p0(i,j,k)
else if (k==0) then
p1(i,j,k)=p0(i,j,k)
else if (k==km+1) then
p1(i,j,k)=p0(i,j,k)
else
! the core
! The actual SOR expression
reltmp = omega*(cn1 *(cn2l*p0(i+1,j,k) + &
cn2s*p0(i-1,j,k) +cn3l*p0(i,j+1,k) + &
cn3s*p0(i,j-1,k) +cn4l*p0(i,j,k+1) + &
cn4s*p0(i,j,k-1) -rhs(i,j,k))-p0(i,j,k))
p1(i,j,k) = p0(i,j,k) +reltmp    
end if

end subroutine sor_kernel

end module sor_routines
