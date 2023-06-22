module module_sor_superkernel_superkernel_init

integer, parameter :: ST_SOR_SUPERKERNEL_MAP_23 = 0 !  sor_superkernel_map_23
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_47 = 1 !  sor_superkernel_map_47
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_71 = 2 !  sor_superkernel_map_71
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_95 = 3 !  sor_superkernel_map_95
        integer, parameter ::P0_BUF_IDX = 1
        integer, parameter ::P1_BUF_IDX = 3
        integer, parameter ::P2_BUF_IDX = 4
        integer, parameter ::P3_BUF_IDX = 5
        integer, parameter ::P4_BUF_IDX = 6
        integer, parameter ::RHS_BUF_IDX = 2
        integer, parameter ::STATE_PTR_BUF_IDX = 7

contains


! WV 2021-06-22

    subroutine sor_superkernel_superkernel_init()

        use oclWrapper
        character(len=*), parameter :: srcstr = "module_sor_superkernel_superkernel.cl"
        character(len=*), parameter :: kstr   = "sor_superkernel_superkernel"
! parameters
              real(4), parameter :: cn1 = 0.333333333333333 
              real(4), parameter :: cn2l = 0.5 
              real(4), parameter :: cn2s = 0.5 
              real(4), parameter :: cn3l = 0.5 
              real(4), parameter :: cn3s = 0.5 
              real(4), parameter :: cn4l = 0.5 
              real(4), parameter :: cn4s = 0.5 
              real, parameter :: omega = 1 
! declarations
        real, dimension(0:101,0:101,0:81) :: p0
        real, dimension(0:101,0:101,0:81) :: rhs
        real, dimension(0:101,0:101,0:81) :: p1
        real, dimension(0:101,0:101,0:81) :: p2
        real, dimension(0:101,0:101,0:81) :: p3
        real, dimension(0:101,0:101,0:81) :: p4
        integer :: state_ptr
! buffer declarations
        integer(8) :: p0_buf
        integer(8) :: rhs_buf
        integer(8) :: p1_buf
        integer(8) :: p2_buf
        integer(8) :: p3_buf
        integer(8) :: p4_buf
        integer(8) :: state_ptr_buf
        integer, dimension(3) :: p0_sz
        integer, dimension(3) :: rhs_sz
        integer, dimension(3) :: p1_sz
        integer, dimension(3) :: p2_sz
        integer, dimension(3) :: p3_sz
        integer, dimension(3) :: p4_sz
        integer, dimension(1) :: state_ptr_ptr_sz
        integer, dimension(1) :: state_ptr_ptr

        call oclInit(srcstr,kstr)

        p0_sz = shape(p0)
        rhs_sz = shape(rhs)
        p1_sz = shape(p1)
        p2_sz = shape(p2)
        p3_sz = shape(p3)
        p4_sz = shape(p4)
        state_ptr_ptr_sz = shape(state_ptr_ptr)

        call oclMake3DFloatArrayReadWriteBuffer(p0_buf,p0_sz,p0)
        call oclMake3DFloatArrayReadWriteBuffer(rhs_buf,rhs_sz,rhs)
        call oclMake3DFloatArrayReadWriteBuffer(p1_buf,p1_sz,p1)
        call oclMake3DFloatArrayReadWriteBuffer(p2_buf,p2_sz,p2)
        call oclMake3DFloatArrayReadWriteBuffer(p3_buf,p3_sz,p3)
        call oclMake3DFloatArrayReadWriteBuffer(p4_buf,p4_sz,p4)
        call oclMake1DIntArrayReadWriteBuffer(state_ptr_buf,state_ptr_ptr_sz,state_ptr_ptr)! Automatic conversion to array

        call oclSetFloatArrayArg(0, p0_buf)
        call oclSetFloatArrayArg(1, rhs_buf)
        call oclSetFloatArrayArg(2, p1_buf)
        call oclSetFloatArrayArg(3, p2_buf)
        call oclSetFloatArrayArg(4, p3_buf)
        call oclSetFloatArrayArg(5, p4_buf)
        call oclSetIntArrayArg(6, state_ptr_buf)

        call oclStoreBuffer(P0_BUF_IDX, p0_buf)
        call oclStoreBuffer(P1_BUF_IDX, p1_buf)
        call oclStoreBuffer(P2_BUF_IDX, p2_buf)
        call oclStoreBuffer(P3_BUF_IDX, p3_buf)
        call oclStoreBuffer(P4_BUF_IDX, p4_buf)
        call oclStoreBuffer(RHS_BUF_IDX, rhs_buf)
        call oclStoreBuffer(STATE_PTR_BUF_IDX, state_ptr_buf)


    end subroutine sor_superkernel_superkernel_init
end module module_sor_superkernel_superkernel_init