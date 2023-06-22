program test_sor_unroll
      use singleton_module_src_2_postcpp_sor_superkernel ! _create_module_src
use sor_params  ! emit_AnnLines(test_sor_unroll)
use sor_routines  ! emit_AnnLines(test_sor_unroll)
      implicit none
real, dimension(0:101, 0:101, 0:81) :: p0
real, dimension(0:101, 0:101, 0:81) :: p1
real, dimension(0:101, 0:101, 0:81) :: rhs
real, dimension(0:101, 0:101, 0:81) :: p2
integer :: iter
integer :: niters
integer :: i
integer :: j
integer :: k
do i = 0, 101
do j = 0, 101
do k = 0, 81
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
    end do 
    end do 
    end do 
    niters = 1200 / 2
    do iter = 1, niters
      call sor_superkernel(p0,p2,rhs)
    p0 = p2
    end do 
    print * , p0(100 / 2,100 / 2,80 / 2)
    end program test_sor_unroll
