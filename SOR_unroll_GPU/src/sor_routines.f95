module sor_routines
use sor_params

contains

attributes(global) subroutine sor (d_p0,d_p1,d_rhs)
    use sor_params    
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: d_p0
real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out), device :: d_p1
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: d_rhs
integer :: i,j,k

i = (blockIdx%x-1)*blockDim%x + threadIdx%x
j = (blockIdx%y-1)*blockDim%y + threadIdx%y
k = (blockIdx%z-1)*blockDim%z + threadIdx%z

if(i<=im+1 .and. j<=jm+1 .and. k<=km+1) then
  call sor_kernel(d_p0,d_p1,d_rhs,i,j,k)
endif

end subroutine sor

attributes(device) subroutine sor_kernel(d_p0,d_p1,d_rhs,i,j,k) 
    use sor_params
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: d_p0
real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: d_p1
real, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: d_rhs 
integer, intent(In) :: i,j,k
real(kind=4), parameter :: cn1 = 1.0/3.0
real(kind=4), parameter :: cn2l = 0.5
real(kind=4), parameter :: cn2s = 0.5
real(kind=4), parameter :: cn3l = 0.5
real(kind=4), parameter :: cn3s = 0.5
real(kind=4), parameter :: cn4l = 0.5
real(kind=4), parameter :: cn4s = 0.5
real, parameter  :: omega = 1.0
real :: reltmp

if(i > 0 .and. i < im+1 .and. j > 0 .and. j < jm+1 .and. k > 0 .and. k < km+1) then
    reltmp = d_rhs(i,j,k) - cn4l*(d_p0(i+1,j,k) + d_p0(i-1,j,k)) - cn4s*(d_p0(i+2,j,k) + d_p0(i-2,j,k)) &
        - cn2l*(d_p0(i,j+1,k) + d_p0(i,j-1,k)) - cn2s*(d_p0(i,j+2,k) + d_p0(i,j-2,k)) &
        - cn3l*(d_p0(i,j,k+1) + d_p0(i,j,k-1)) - cn3s*(d_p0(i,j,k+2) + d_p0(i,j,k-2)) &
        - cn1*d_p0(i,j,k)
    d_p1(i,j,k) = d_p0(i,j,k) + omega*reltmp
endif

end subroutine sor_kernel

end module sor_routines
