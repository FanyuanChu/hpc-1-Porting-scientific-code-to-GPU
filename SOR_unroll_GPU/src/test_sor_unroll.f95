program test_sor_unroll
    use omp_lib
    use sor_params
    use sor_routines

    real, dimension(:,:,:), allocatable :: p0_h, p1_h, rhs_h
    real, dimension(:,:,:), device, allocatable :: p0_d, p1_d, rhs_d
    integer :: iter, niters
    integer :: i, j, k, blocks, grid
    integer :: clock_rate
    integer, dimension(0:1) :: timestamp
    integer, dimension(3) :: block_dim, grid_dim

    ! Initialize block and grid dimensions
    block_dim = [256, 1, 1]
    grid_dim = [(im+1+block_dim(1)-1)/block_dim(1), (jm+1+block_dim(2)-1)/block_dim(2), (km+1+block_dim(3)-1)/block_dim(3)]

    ! Allocate host arrays
    allocate(p0_h(0:im+1,0:jm+1,0:km+1))
    allocate(p1_h(0:im+1,0:jm+1,0:km+1))
    allocate(rhs_h(0:im+1,0:jm+1,0:km+1))

    ! Initialize host arrays
    do i = 0,im+1
        do j = 0,jm+1
            do k = 0,km+1
                rhs_h(i,j,k) = 1.0
                p0_h(i,j,k) = 1.0
            end do
        end do
    end do

    ! Allocate device memory
    allocate(p0_d(0:im+1,0:jm+1,0:km+1))
    allocate(p1_d(0:im+1,0:jm+1,0:km+1))
    allocate(rhs_d(0:im+1,0:jm+1,0:km+1))

    ! Copy data from host to device
    p0_d = p0_h
    rhs_d = rhs_h

    niters = 12/UNROLL
    call system_clock(timestamp(0), clock_rate)

    do iter = 1,niters
        print *,iter

        call sor<<<grid_dim, block_dim>>>(p0_d, p1_d, rhs_d)
        call cudaDeviceSynchronize()

        if (mod(iter, 2) == 0) then
            call swap(p0_d, p1_d)
        end if
    end do

    ! Copy results from device to host
    p1_h = p1_d

    call system_clock(timestamp(1), clock_rate)
    print '(f8.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)

    print *, p1_h(im/2,jm/2,km/2)

    ! Free device memory
    deallocate(p0_d)
    deallocate(p1_d)
    deallocate(rhs_d)

    ! Deallocate host arrays
    deallocate(p0_h)
    deallocate(p1_h)
    deallocate(rhs_h)

end program test_sor_unroll
