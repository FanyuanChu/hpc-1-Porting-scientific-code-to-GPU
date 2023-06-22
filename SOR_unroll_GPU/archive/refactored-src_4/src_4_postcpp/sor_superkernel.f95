module singleton_module_src_4_postcpp_sor_superkernel

contains

 subroutine sor_superkernel(p0,p4,rhs)
use sor_params ! emit_AnnLines(sor_kernel)

 implicit none
 real( 4), parameter :: cn1 = 0.333333333333333
 real( 4), parameter :: cn2l = 0.5
 real( 4), parameter :: cn2s = 0.5
 real( 4), parameter :: cn3l = 0.5
 real( 4), parameter :: cn3s = 0.5
 real( 4), parameter :: cn4l = 0.5
 real( 4), parameter :: cn4s = 0.5
 real, parameter :: omega = 1
real :: reltmp
 integer :: i
 integer :: j
 integer :: k
 real, dimension(0:101,0:101,0:81), intent(In) :: rhs
 real, dimension(0:101,0:101,0:81), intent(InOut) :: p4
 real, dimension(0:101,0:101,0:81), intent(In) :: p0
 real, dimension(0:101,0:101,0:81) :: p3
 real, dimension(0:101,0:101,0:81) :: p2
 real, dimension(0:101,0:101,0:81) :: p1
!RF4A Begin Inline
 do i = 0, 101
 do j = 0, 101
 do k = 0, 81
if ( i == 100 + 1 ) then
p1(i,j,k) = p0(i - 100,j,k)
else if ( i == 0 ) then
p1(i,j,k) = p0(i + 100,j,k)
else if ( j == 100 + 1 ) then
p1(i,j,k) = p0(i - 1,j,k)
else if ( j == 0 ) then
 p1(i,j,k) = p0(i,j,k)
 else if ( k == 0 ) then
 p1(i,j,k) = p0(i,j,k)
 else if ( k == 80 + 1 ) then
 p1(i,j,k) = p0(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p0(i + 1,j,k) + 0.5 * p0(i - 1,j,k) + 0.5 * p0(i,j + 1, &
      k) + 0.5 * p0(i,j - 1,k) + 0.5 * p0(i,j,k + 1) + 0.5 * p0(i,j,k - 1) - rhs(i,j,k)) - p0(i,j, &
      k))
 p1(i,j,k) = p0(i,j,k) + reltmp
 end if 
 end do 
 end do 
 end do 
 do i = 0, 101
 do j = 0, 101
 do k = 0, 81
if ( i == 100 + 1 ) then
p2(i,j,k) = p1(i - 100,j,k)
else if ( i == 0 ) then
p2(i,j,k) = p1(i + 100,j,k)
else if ( j == 100 + 1 ) then
p2(i,j,k) = p1(i - 1,j,k)
else if ( j == 0 ) then
 p2(i,j,k) = p1(i,j,k)
 else if ( k == 0 ) then
 p2(i,j,k) = p1(i,j,k)
 else if ( k == 80 + 1 ) then
 p2(i,j,k) = p1(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p1(i + 1,j,k) + 0.5 * p1(i - 1,j,k) + 0.5 * p1(i,j + 1, &
      k) + 0.5 * p1(i,j - 1,k) + 0.5 * p1(i,j,k + 1) + 0.5 * p1(i,j,k - 1) - rhs(i,j,k)) - p1(i,j, &
      k))
 p2(i,j,k) = p1(i,j,k) + reltmp
 end if 
 end do 
 end do 
 end do 
 do i = 0, 101
 do j = 0, 101
 do k = 0, 81
if ( i == 100 + 1 ) then
p3(i,j,k) = p2(i - 100,j,k)
else if ( i == 0 ) then
p3(i,j,k) = p2(i + 100,j,k)
else if ( j == 100 + 1 ) then
p3(i,j,k) = p2(i - 1,j,k)
else if ( j == 0 ) then
 p3(i,j,k) = p2(i,j,k)
 else if ( k == 0 ) then
 p3(i,j,k) = p2(i,j,k)
 else if ( k == 80 + 1 ) then
 p3(i,j,k) = p2(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p2(i + 1,j,k) + 0.5 * p2(i - 1,j,k) + 0.5 * p2(i,j + 1, &
      k) + 0.5 * p2(i,j - 1,k) + 0.5 * p2(i,j,k + 1) + 0.5 * p2(i,j,k - 1) - rhs(i,j,k)) - p2(i,j, &
      k))
 p3(i,j,k) = p2(i,j,k) + reltmp
 end if 
 end do 
 end do 
 end do 
 do i = 0, 101
 do j = 0, 101
 do k = 0, 81
if ( i == 100 + 1 ) then
p4(i,j,k) = p3(i - 100,j,k)
else if ( i == 0 ) then
p4(i,j,k) = p3(i + 100,j,k)
else if ( j == 100 + 1 ) then
p4(i,j,k) = p3(i - 1,j,k)
else if ( j == 0 ) then
 p4(i,j,k) = p3(i,j,k)
 else if ( k == 0 ) then
 p4(i,j,k) = p3(i,j,k)
 else if ( k == 80 + 1 ) then
 p4(i,j,k) = p3(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p3(i + 1,j,k) + 0.5 * p3(i - 1,j,k) + 0.5 * p3(i,j + 1, &
      k) + 0.5 * p3(i,j - 1,k) + 0.5 * p3(i,j,k + 1) + 0.5 * p3(i,j,k - 1) - rhs(i,j,k)) - p3(i,j, &
      k))
 p4(i,j,k) = p3(i,j,k) + reltmp
 end if 
 end do 
 end do 
 end do 
!RF4A End Inline
 end subroutine sor_superkernel

end module singleton_module_src_4_postcpp_sor_superkernel

