! test_sor_unroll.f95 
 program test_sor_unroll
#ifdef WITH_OPENMP
use omp_lib
#endif

use sor_params
use sor_routines
#ifndef DYN_ALLOC
real, dimension(0:im+1,0:jm+1,0:km+1), device :: p0  !!! MODIFIED
real, dimension(0:im+1,0:jm+1,0:km+1), device :: p1  !!! MODIFIED
real, dimension(0:im+1,0:jm+1,0:km+1), device :: rhs  !!! MODIFIED
#else
    real, device, allocatable  :: p0(:,:,:)  !!! MODIFIED
    real, device, allocatable  :: p1(:,:,:)  !!! MODIFIED
    real, device, allocatable  :: rhs(:,:,:)  !!! MODIFIED
#endif
integer :: iter, niters

integer :: i,j,k
#ifdef TIMING
    integer :: clock_rate
    integer, dimension(0:1) :: timestamp
#endif

#ifdef DYN_ALLOC
    allocate(p0(0:im+1,0:jm+1,0:km+1))
    allocate(p1(0:im+1,0:jm+1,0:km+1))
    allocate(rhs(0:im+1,0:jm+1,0:km+1))
#endif

do i = 0,im+1
do j = 0,jm+1
do k = 0,km+1
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
end do
end do
end do

niters = 12/UNROLL
#ifdef TIMING
    call system_clock(timestamp(0), clock_rate)
#endif
do iter = 1,niters
    print *,iter
call sor (p0,p1,rhs)
#if UNROLL==1
p0=p1
#endif
end do
#ifdef TIMING
    call system_clock(timestamp(1), clock_rate)
    print '(f8.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)
#endif
print *, p0(im/2,jm/2,km/2)
#ifdef DYN_ALLOC
    deallocate(p0)
    deallocate(p1)
    deallocate(rhs)
#endif
end program
