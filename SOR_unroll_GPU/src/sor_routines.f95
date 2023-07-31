module sor_routines
use sor_params
contains

subroutine sor (p0,p1,rhs)
    use sor_params    
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: p0  ! Add device attribute here
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out), device :: p1  ! Add device attribute here
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: rhs  ! Add device attribute here
    integer :: i,j,k
    do i = 1,im
        do j = 1,jm
            do k = 1,km
                call sor_kernel(p0,p1,rhs,i,j,k)
            end do
        end do
    end do
end subroutine sor

subroutine sor_kernel(p0,p1,rhs,i,j,k) 
    use sor_params
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: p0
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out), device :: p1
    real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: rhs 
    integer, intent(In) :: i,j,k
    real(kind=4), parameter :: cn1 = 1.0/3.0
    real(kind=4), parameter :: cn2l = 0.5
    real(kind=4), parameter :: cn2s = 0.5
    real(kind=4), parameter :: cn3l = 0.5
    real(kind=4), parameter :: cn3s = 0.5
    real(kind=4), parameter :: cn4l = 0.5
    real(kind=4), parameter :: cn4s = 0.5
    real, parameter  :: omega = 1.0
    real :: reltmp

    ! The actual SOR expression
    reltmp = omega*(cn1 *(cn2l*p0(i+1,j,k) + &
        cn2s*p0(i-1,j,k) +cn3l*p0(i,j+1,k) + &
        cn3s*p0(i,j-1,k) +cn4l*p0(i,j,k+1) + &
        cn4s*p0(i,j,k-1) -rhs(i,j,k))-p0(i,j,k))
    p1(i,j,k) = p0(i,j,k) +reltmp    
end subroutine sor_kernel
end module sor_routines
