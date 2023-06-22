module singleton_module_src_3_postcpp_sor_superkernel
 contains
 subroutine sor_superkernel(p0,p3,rhs)

 use module_sor_superkernel_superkernel_init
 use oclWrapper
!      implicit none

      implicit none

! otherStatements

! remainingDecls
      real(4), parameter :: cn1 = 0.333333333333333  !!
      real(4), parameter :: cn2l = 0.5  !!
      real(4), parameter :: cn2s = 0.5  !!
      real(4), parameter :: cn3l = 0.5  !!
      real(4), parameter :: cn3s = 0.5  !!
      real(4), parameter :: cn4l = 0.5  !!
      real(4), parameter :: cn4s = 0.5  !!
      real, parameter :: omega = 1  !!
      real :: reltmp !!
      integer :: i !!
      integer :: j !!
      integer :: k !!
      real, dimension(0:101,0:101,0:81), intent(in) :: rhs !!
      real, dimension(0:101,0:101,0:81), intent(inout) :: p3 !!
      real, dimension(0:101,0:101,0:81), intent(in) :: p0 !!
      real, dimension(0:101,0:101,0:81) :: p2 !!
      real, dimension(0:101,0:101,0:81) :: p1 !!

 ! Extra declarations
 real (kind=4) :: exectime

 ! Buffer declarations
 integer(8) :: state_ptr_buf

 integer :: state_ptr

 ! Size declarations
 integer, dimension(1) :: state_ptr_sz

 
! Size assignments
 state_ptr_sz = shape(state_ptr)

 ! Buffer loads
 call oclLoadBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)

 ! Original code with buffer writes and reads
! ---- BEGIN sor_superkernel_map_22 -------------------------------------------------------------------------------------------
 oclGlobalRange = (((101 - 0) + 1) * (((101 - 0) + 1) * ((81 - 0) + 1)))
 oclLocalRange = 0
 state_ptr(1) = ST_SOR_SUPERKERNEL_MAP_22
 
 state_ptr_ptr(1) = state_ptr
 call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_ptr_sz,state_ptr_ptr)! Automatic conversion to array
 call runOcl(oclGlobalRange,oclLocalRange,exectime)
 ! call sor_superkernel_map_22

! ---- END --------------------------------------------------------------------------------------------------------------------
! ---- BEGIN sor_superkernel_map_46 -------------------------------------------------------------------------------------------
 oclGlobalRange = (((101 - 0) + 1) * (((101 - 0) + 1) * ((81 - 0) + 1)))
 oclLocalRange = 0
 state_ptr(1) = ST_SOR_SUPERKERNEL_MAP_46
 
 state_ptr_ptr(1) = state_ptr
 call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_ptr_sz,state_ptr_ptr)! Automatic conversion to array
 call runOcl(oclGlobalRange,oclLocalRange,exectime)
 ! call sor_superkernel_map_46

! ---- END --------------------------------------------------------------------------------------------------------------------
! ---- BEGIN sor_superkernel_map_70 -------------------------------------------------------------------------------------------
 oclGlobalRange = (((101 - 0) + 1) * (((101 - 0) + 1) * ((81 - 0) + 1)))
 oclLocalRange = 0
 state_ptr(1) = ST_SOR_SUPERKERNEL_MAP_70
 
 state_ptr_ptr(1) = state_ptr
 call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_ptr_sz,state_ptr_ptr)! Automatic conversion to array
 call runOcl(oclGlobalRange,oclLocalRange,exectime)
 ! call sor_superkernel_map_70

! ---- END --------------------------------------------------------------------------------------------------------------------
 end subroutine sor_superkernel
! Footer (produceCode_progUnit c)
end module singleton_module_src_3_postcpp_sor_superkernel
