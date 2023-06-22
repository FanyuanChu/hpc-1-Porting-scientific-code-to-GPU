module singleton_module_sor

contains

subroutine sor(p0,p1,rhs)
use sor_params ! emit_AnnLines(sor_kernel)

 implicit none
!      BEGIN ex-sub decls sor_kernel
 real( 4), parameter :: cn1 = 0.333333333333333
 real( 4), parameter :: cn2l = 0.5
 real( 4), parameter :: cn2s = 0.5
 real( 4), parameter :: cn3l = 0.5
 real( 4), parameter :: cn3s = 0.5
 real( 4), parameter :: cn4l = 0.5
 real( 4), parameter :: cn4s = 0.5
 real, parameter :: omega = 1
real :: reltmp
!      END ex-sub decls sor_kernel
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(out) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: rhs
 integer :: i
 integer :: j
 integer :: k
 do i = 0, 101
 do j = 0, 101
 do k = 0, 81
!      BEGIN inlined call to sor_kernel
!  assume i=x = west to east , y=j=south to north, k=z = vertical
if ( i == 100 + 1 ) then
!  circular
!  i=im+1
p1(i,j,k) = p0(i - 100,j,k)
else if ( i == 0 ) then
!  i=0
!  circular
p1(i,j,k) = p0(i + 100,j,k)
else if ( j == 100 + 1 ) then
!  open
!  j = jm+1
p1(i,j,k) = p0(i - 1,j,k)
else if ( j == 0 ) then
!  fixed
!  j = 0
!  We keep the original values
 p1(i,j,k) = p0(i,j,k)
 else if ( k == 0 ) then
 p1(i,j,k) = p0(i,j,k)
 else if ( k == 80 + 1 ) then
 p1(i,j,k) = p0(i,j,k)
 else
!  the core
!  The actual SOR expression
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p0(i + 1,j,k) + 0.5 * p0(i - 1,j,k) + 0.5 * p0(i,j + 1, &
      k) + 0.5 * p0(i,j - 1,k) + 0.5 * p0(i,j,k + 1) + 0.5 * p0(i,j,k - 1) - rhs(i,j,k)) - p0(i,j, &
      k))
 p1(i,j,k) = p0(i,j,k) + reltmp
 end if 
!      END inlined call to sor_kernel
 end do 
 end do 
 end do 
 end subroutine sor

end module singleton_module_sor

