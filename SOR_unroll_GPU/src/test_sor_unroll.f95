! Inline and then run through the apf compiler, then reduce, then translate to C

! Write a simple host code: just a time loop that calls sor

! $ time ./test_sor_unroll_1
! -390.423431

! real	0m7.482s
! user	0m7.482s
! sys	0m0.000s
! $ time ./test_sor_unroll_2
! -391.846161

! real	0m7.910s
! user	0m7.902s
! sys	0m0.008s
! $ time ./test_sor_unroll_3
! -392.749146

! real	0m7.947s
! user	0m7.935s
! sys	0m0.012s
! $ time ./test_sor_unroll_4
! -393.359375

! real	0m7.982s
! user	0m7.965s
! sys	0m0.016s


program test_sor_unroll
#ifdef WITH_OPENMP
use omp_lib
#endif

use sor_params
use sor_routines

!<<CHANGED>> Declare device arrays
real, dimension(:,:,:), device, allocatable :: p0_d, p1_d, rhs_d

!<<CHANGED>> Declare host arrays
real, dimension(:,:,:), allocatable :: p0_h, p1_h, rhs_h

integer :: iter, niters
integer :: i,j,k
#ifdef TIMING
    integer :: clock_rate
    integer, dimension(0:1) :: timestamp
#endif

!<<CHANGED>> Allocate device arrays
allocate(p0_d(0:im+1,0:jm+1,0:km+1))
allocate(p1_d(0:im+1,0:jm+1,0:km+1))
allocate(rhs_d(0:im+1,0:jm+1,0:km+1))

!<<CHANGED>> Allocate host arrays
allocate(p0_h(0:im+1,0:jm+1,0:km+1))
allocate(p1_h(0:im+1,0:jm+1,0:km+1))
allocate(rhs_h(0:im+1,0:jm+1,0:km+1))

do i = 0,im+1
do j = 0,jm+1
do k = 0,km+1
    rhs_h(i,j,k) = 1.0
    p0_h(i,j,k) = 1.0
end do
end do
end do

!<<CHANGED>> Copy data from host to device
rhs_d = rhs_h
p0_d = p0_h

niters = 12/UNROLL
#ifdef TIMING
    call system_clock(timestamp(0), clock_rate)
#endif

do iter = 1,niters
    print *,iter

!<<CHANGED>> Call CUDA kernel
    call sor(p0_d,p1_d,rhs_d)

!<<CHANGED>> Swap pointers for next iteration
    if (mod(iter, 2) == 0) then
        call swap(p0_d, p1_d)
    end if
end do

!<<CHANGED>> Copy results from device to host
p1_h = p1_d

#ifdef TIMING
    call system_clock(timestamp(1), clock_rate)
    print '(f8.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)
#endif

print *, p1_h(im/2,jm/2,km/2)

!<<CHANGED>> Deallocate device arrays
deallocate(p0_d)
deallocate(p1_d)
deallocate(rhs_d)

!<<CHANGED>> Deallocate host arrays
deallocate(p0_h)
deallocate(p1_h)
deallocate(rhs_h)

end program test_sor_unroll


