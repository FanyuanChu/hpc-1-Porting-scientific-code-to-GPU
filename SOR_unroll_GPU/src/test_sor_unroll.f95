! test_sor_unroll.f95 second
program test_sor_unroll
use cudafor
use sor_params
use sor_routines

integer, parameter :: UNROLL = 4
real, device, allocatable  :: p0(:,:,:), p1(:,:,:), p2(:,:,:), p3(:,:,:), p4(:,:,:), rhs(:,:,:)
integer :: iter, niters
integer :: i,j,k
real :: start_time, end_time
real :: sample

allocate(p0(0:im+1,0:jm+1,0:km+1))
allocate(p1(0:im+1,0:jm+1,0:km+1))
allocate(rhs(0:im+1,0:jm+1,0:km+1))
allocate(p2(0:im+1,0:jm+1,0:km+1))
allocate(p3(0:im+1,0:jm+1,0:km+1))
allocate(p4(0:im+1,0:jm+1,0:km+1))

! Initialization on the host
real, allocatable :: rhs_host(:,:,:), p0_host(:,:,:)
allocate(rhs_host(0:im+1,0:jm+1,0:km+1))
allocate(p0_host(0:im+1,0:jm+1,0:km+1))
rhs_host = 1.0
p0_host = 1.0
rhs = rhs_host
p0 = p0_host

niters = 12 / UNROLL
start_time = rtc()
do iter = 1, niters
    print *, iter
    call sor(p0, p1, rhs)
    call sor(p1, p2, rhs)
    call sor(p2, p3, rhs)
    call sor(p3, p4, rhs)
    p0 = p4
end do
end_time = rtc()

! Printing a sample value

sample = p0(im/2,jm/2,km/2)
print *, sample
print *, "Time elapsed: ", end_time - start_time, " seconds."

deallocate(p0)
deallocate(p1)
deallocate(rhs)
deallocate(rhs_host)
deallocate(p0_host)
deallocate(p2)
deallocate(p3)
deallocate(p4)

end program
