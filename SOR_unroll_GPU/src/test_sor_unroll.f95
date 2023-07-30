program test_sor_unroll

use sor_params
use sor_routines

real, dimension(0:im+1,0:jm+1,0:km+1) :: p0
real, dimension(0:im+1,0:jm+1,0:km+1) :: p1
real, dimension(0:im+1,0:jm+1,0:km+1) :: rhs
real, dimension(:,:,:), allocatable, device :: d_p0, d_p1, d_rhs
integer :: blocks(3), threads(3), iter, niters

blocks = [im/32+1, jm/32+1, km/32+1]
threads = [32, 32, 32]

do i = 0,im+1
do j = 0,jm+1
do k = 0,km+1
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
enddo
enddo
enddo

allocate(d_p0(0:im+1,0:jm+1,0:km+1))
allocate(d_p1(0:im+1,0:jm+1,0:km+1))
allocate(d_rhs(0:im+1,0:jm+1,0:km+1))

d_p0 = p0
d_p1 = p0
d_rhs = rhs

niters = 12
do iter = 1,niters
    call sor<<<blocks, threads>>>(d_p0,d_p1,d_rhs)
    d_p0 = d_p1
enddo
p1 = d_p1

print *, 'Result: ', p1(im/2+1,jm/2+1,km/2+1)

deallocate(d_p0)
deallocate(d_p1)
deallocate(d_rhs)

end program test_sor_unroll
