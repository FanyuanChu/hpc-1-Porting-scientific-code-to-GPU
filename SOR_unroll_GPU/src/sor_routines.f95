module sor_routines
use sor_params
contains

attributes(global) subroutine sor_kernel(p0,p1,rhs,i,j,k) 
    use sor_params
real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(In) :: p0
real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(Out) :: p1
real, dimension(0:im+1,0:jm+1,0:km+1), device, intent(In) :: rhs 
integer, value, intent(In) :: i,j,k
real(kind=4), parameter :: cn1 = 1.0/3.0
real(kind=4), parameter :: cn2l = 0.5
real(kind=4), parameter :: cn2s = 0.5
real(kind=4), parameter :: cn3l = 0.5
real(kind=4), parameter :: cn3s = 0.5
real(kind=4), parameter :: cn4l = 0.5
real(kind=4), parameter :: cn4s = 0.5
real, parameter  :: omega = 1.0
real :: reltmp

! the core
! The actual SOR expression
    reltmp = omega*(cn1 *(cn2l*p0(i+1,j,k) + &
        cn2s*p0(i-1,j,k) +cn3l*p0(i,j+1,k) + &
        cn3s*p0(i,j-1,k) +cn4l*p0(i,j,k+1) + &
        cn4s*p0(i,j,k-1) -rhs(i,j,k))-p0(i,j,k))
    p1(i,j,k) = p0(i,j,k) +reltmp    

end subroutine sor_kernel

end module sor_routines
