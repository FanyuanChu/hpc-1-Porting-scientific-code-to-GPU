module sor_routines
    use sor_params
    use cudafor

contains

attributes(global) subroutine sor_kernel(p0, p1, rhs)
    use sor_params
    real, device, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: p0
    real, device, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: p1
    real, device, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: rhs
    integer :: i, j, k
    real(kind=4), parameter :: cn1 = 1.0/3.0
    real(kind=4), parameter :: cn2l = 0.5
    real(kind=4), parameter :: cn2s = 0.5
    real(kind=4), parameter :: cn3l = 0.5
    real(kind=4), parameter :: cn3s = 0.5
    real(kind=4), parameter :: cn4l = 0.5
    real(kind=4), parameter :: cn4s = 0.5
    real, parameter  :: omega = 1.0
    real :: reltmp

    i = blockIdx%x * blockDim%x + threadIdx%x
    j = blockIdx%y * blockDim%y + threadIdx%y
    k = blockIdx%z * blockDim%z + threadIdx%z

    if (i <= im+1 .and. j <= jm+1 .and. k <= km+1) then
        if (i == im+1) then
            p1(i,j,k) = p0(i-im,j,k)
        else if (i == 0) then
            p1(i,j,k) = p0(i+im,j,k)
        else if (j == jm+1) then
            p1(i,j,k) = p0(i-1,j,k)
        else if (j == 0) then
            p1(i,j,k) = p0(i,j,k)
        else if (k == 0 .or. k == km+1) then
            p1(i,j,k) = p0(i,j,k)
        else
            reltmp = omega * (cn1 * (cn2l * p0(i+1,j,k) + cn2s * p0(i-1,j,k) + cn3l * p0(i,j+1,k) + cn3s * p0(i,j-1,k) + cn4l * p0(i,j,k+1) + cn4s * p0(i,j,k-1) - rhs(i,j,k)) - p0(i,j,k))
            p1(i,j,k) = p0(i,j,k) + reltmp
        end if
    end if
end subroutine sor_kernel

subroutine sor(p0, p1, rhs)
    use sor_params
    real, device, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: p0
    real, device, dimension(0:im+1,0:jm+1,0:km+1), intent(Out) :: p1
    real, device, dimension(0:im+1,0:jm+1,0:km+1), intent(In) :: rhs
    integer, dimension(3) :: threads, blocks

    threads = [16, 16, 4]
    blocks = [(im+2)/16, (jm+2)/16, (km+2)/4]

    call sor_kernel<<<blocks, threads>>>(p0, p1, rhs)
end subroutine sor

end module sor_routines
