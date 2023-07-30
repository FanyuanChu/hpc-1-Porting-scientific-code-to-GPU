program test_sor_unroll
#ifdef WITH_OPENMP
use omp_lib
#endif

use sor_params
use sor_routines
real, dimension(:,:,:), device, allocatable :: p0, p1, rhs
real, dimension(:,:,:), allocatable :: p0_host
integer :: iter, niters

integer :: i,j,k

allocate(p0(0:im+1,0:jm+1,0:km+1))
allocate(p1(0:im+1,0:jm+1,0:km+1))
allocate(rhs(0:im+1,0:jm+1,0:km+1))

do i = 0,im+1
do j = 0,jm+1
do k = 0,km+1
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
end do
end do
end do

niters = 12
do iter = 1,niters
    print *,iter
    call sor_kernel<<<dim3(im+1,jm+1,km+1),1>>>(p0, p1, rhs, i, j, k)
    p0=p1
end do

allocate(p0_host(0:im+1,0:jm+1,0:km+1))
p0_host = p0
print *, p0_host(im/2,jm/2,km/2)

deallocate(p0)
deallocate(p1)
deallocate(rhs)
deallocate(p0_host)

end program
