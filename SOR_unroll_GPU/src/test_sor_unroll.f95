program test_sor_unroll
#ifdef WITH_OPENMP
use omp_lib
#endif

use sor_params
use sor_routines
real, dimension(0:im+1,0:jm+1,0:km+1) :: p0, p1, rhs
real, dimension(:,:,:), allocatable, device :: d_p0, d_p1, d_rhs
integer :: iter, niters
integer :: i,j,k
#ifdef TIMING
integer :: clock_rate
integer, dimension(0:1) :: timestamp
#endif

allocate(d_p0(0:im+1,0:jm+1,0:km+1))
allocate(d_p1(0:im+1,0:jm+1,0:km+1))
allocate(d_rhs(0:im+1,0:jm+1,0:km+1))

do i = 0,im+1
do j = 0,jm+1
do k = 0,km+1
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
end do
end do
end do

d_rhs = rhs
d_p0 = p0

niters = 10
#ifdef WITH_OPENMP
call omp_set_num_threads(4)
#endif
#ifdef TIMING
call system_clock(count_rate=clock_rate)
call system_clock(count_max=timestamp(0))
call system_clock(count=timestamp(1))
#endif
do iter = 1,niters
    call sor(d_p0,d_p1,d_rhs)
    d_p0 = d_p1
end do
#ifdef TIMING
call system_clock(count=timestamp(0))
print *, 'Total time: ', real(timestamp(0)-timestamp(1))/real(clock_rate), ' sec'
#endif

p1 = d_p1
print *, p1(im/2,jm/2,km/2)

deallocate(d_p0)
deallocate(d_p1)
deallocate(d_rhs)
end program test_sor_unroll
