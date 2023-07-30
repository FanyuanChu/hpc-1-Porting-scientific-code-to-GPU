! sor_routines.f95
module sor_routines
    use sor_params
    implicit none
    contains

    subroutine sor_kernel(p0,p1,rhs,i,j,k) 
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: p0  !!! MODIFIED
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(Out), device :: p1  !!! MODIFIED
        real, dimension(0:im+1,0:jm+1,0:km+1), intent(In), device :: rhs  !!! MODIFIED
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
        real :: temp1, temp2, temp3, temp4, temp5, temp6, p0val

        ! assume i=x =  west to east , y=j=south to north, k=z = vertical
        if (i==im+1) then
            ! circular
            ! i=im+1
            temp1 = p0(i-im,j,k)
            p1(i,j,k) = temp1
        else if (i==0) then
            ! i=0
            ! circular
            temp2 = p0(i+im,j,k)
            p1(i,j,k) = temp2
        else if (j==jm+1) then
            ! open
            ! j = jm+1
            temp3 = p0(i-1,j,k)
            p1(i,j,k)=temp3
        else if (j==0) then
            ! fixed
            ! j = 0
            ! We keep the original values
            temp4 = p0(i,j,k)
            p1(i,j,k)=temp4
        else if (k==0) then
            temp5 = p0(i,j,k)
            p1(i,j,k)=temp5
        else if (k==km+1) then
            temp6 = p0(i,j,k)
            p1(i,j,k)=temp6
        else
            ! the core
            ! The actual SOR expression
            temp1 = cn2l*p0(i+1,j,k)
            temp2 = cn2s*p0(i-1,j,k)
            temp3 = cn3l*p0(i,j+1,k)
            temp4 = cn3s*p0(i,j-1,k)
            temp5 = cn4l*p0(i,j,k+1)
            temp6 = cn4s*p0(i,j,k-1)
            p0val = p0(i,j,k)
            reltmp = omega*(cn1 *(temp1 + temp2 + temp3 + temp4 + temp5 + temp6 -rhs(i,j,k))-p0val)
            p1(i,j,k) = p0val + reltmp
        end if

    end subroutine sor_kernel

end module sor_routines
