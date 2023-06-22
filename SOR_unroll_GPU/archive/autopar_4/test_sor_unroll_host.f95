program test_sor_unroll
use sor_routines 

use oclWrapper
use module_sor_superkernel_superkernel_init
      implicit none

! Original declarations
      real, dimension(0:101,0:101,0:81) :: p0 !!
      real, dimension(0:101,0:101,0:81) :: p1 !!
      real, dimension(0:101,0:101,0:81) :: rhs !!
      real, dimension(0:101,0:101,0:81) :: p2 !!
      real, dimension(0:101,0:101,0:81) :: p3 !!
      real, dimension(0:101,0:101,0:81) :: p4 !!
      integer :: iter !!
      integer :: niters !!
      integer :: i !!
      integer :: j !!
      integer :: k !!



!

! otherStatements

! remainingDecls

! Extra declarations
!NOTHING!
! Buffer declarations
      integer(8) :: state_ptr_buf

integer :: state_ptr

! Size declarations
      integer, dimension(1) :: state_ptr_sz

call sor_superkernel_superkernel_init()

! Size assignments
      state_ptr_sz = shape(state_ptr)

! Buffer loads
      call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)

! Original code with buffer writes and reads
do i = 0, 101
do j = 0, 101
do k = 0, 81
    rhs(i,j,k) = 1.0
    p0(i,j,k) = 1.0
    end do
    end do
    end do
    niters = 1200 / 4
    do iter = 1, niters
      call sor_superkernel(p0,p4,rhs)
    p0 = p4
    end do
    print * , p0(100 / 2,100 / 2,80 / 2)
end program test_sor_unroll
