 program test_sor_unroll
#ifdef WITH_OPENMP
use omp_lib
#endif

use sor_params
use sor_routines
use cudafor

real, dimension(0:im+1,0:jm+1,0:km+1) :: p0
real, dimension(0:im+1,0:jm+1,0:km+1) :: p1
real, dimension(0:im+1,0:jm+1,0:km+1) :: rhs
real, dimension(0:im+1,0:jm+1,0:km+1), device :: d_p0
real, dimension(0:im+1,0:jm+1,0:km+1), device :: d_p1
real, dimension(0:im+1,0:jm+1,0:km+1), device :: d_rhs

integer :: iter, niters

integer :: i,j,k
#ifdef TIMING
    integer :: clock_rate
    integer, dimension(0:1) :: timestamp
#endif

do i = 0,im+1
do j = 0,jm+1
do k = 0,km+1
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
enddo
enddo
enddo

d_p0 = p0
d_p1 = p0
d_rhs = rhs

niters = 12
#ifdef WITH_OPENMP
omp_set_num_threads(4)
#endif
do iter = 1,niters
#ifdef TIMING
call system_clock(count_rate = clock_rate)
call system_clock(count = timestamp(0))
#endif
#ifdef WITH_OPENMP
!$omp parallel do
#endif
    call sor<<<blocks, threads>>>(d_p0,d_p1,d_rhs)
#ifdef TIMING
call system_clock(count = timestamp(1))
print *,'Iteration', iter, 'took', real(timestamp(1) - timestamp(0)) / real(clock_rate), 'seconds.'
#endif
if (mod(iter,2) == 0) then
    d_p0 = d_p1
else
    d_p1 = d_p0
endif
enddo
p1 = d_p1

print *, 'Result: ', p1(im/2+1,jm/2+1,km/2+1)

end program test_sor_unroll
