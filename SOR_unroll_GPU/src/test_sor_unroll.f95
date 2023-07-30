program test_sor_unroll
    use sor_params
    use sor_routines
    use cudafor

    real, dimension(0:im+1,0:jm+1,0:km+1), device :: p0
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: p1
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: rhs
    integer :: iter, niters
    integer :: i,j,k

    allocate(p0, p1, rhs)

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
        call sor (p0, p1, rhs)
        p0 = p1
    end do

    print *, p0(im/2,jm/2,km/2)

    deallocate(p0, p1, rhs)
end program test_sor_unroll
