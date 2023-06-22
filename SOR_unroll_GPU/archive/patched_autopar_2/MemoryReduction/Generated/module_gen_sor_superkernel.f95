module module_sor_superkernel
    contains
subroutine sor_superkernel( global_id_0, p0_0, rhs_0, p2_1, state_ptr)
    real, dimension(1:853128), intent(In) :: p0_0
    real, dimension(1:853128), intent(In) :: rhs_0
    real, dimension(1:853128), intent(Out) :: p2_1
    integer, intent(In) :: global_id_0
    integer, parameter :: ST_STAGE_KERNEL_1 = 1 ! stage_kernel_1
    integer :: state
    integer :: state_ptr
    state = state_ptr ! state
! SUPERKERNEL BODY
    select case(state)
      case (ST_STAGE_KERNEL_1)
      call stage_kernel_1(global_id_0, p0_0, rhs_0, p2_1)
    end select
end subroutine sor_superkernel

subroutine stage_kernel_1(global_id_0, p0_0, rhs_0, p2_1)
! NON-MAP-ARG-DECLS: [integer, intent(In) :: global_id_0]
! arg_decls
    real, dimension(1:853128), intent(In) :: p0_0
    real, dimension(1:853128), intent(In) :: rhs_0
    real, dimension(1:853128), intent(Out) :: p2_1
! uniqueGeneratedDecls'
    integer, parameter, dimension(1:9) :: s1 = [103,10405,10407,10506,10507,10508,10607,10609,20911]
    real, dimension(1:9,1:9) :: svec_p2_1_3
    integer :: s_idx_1
    integer :: s_idx_2
    real, dimension(1:9) :: svec_p2_1_4
    integer, intent(In) :: global_id_0

    integer :: idx
!    call get_global_id(idx,0)
    idx = global_id_0
!$RF4A Begin Inline
! Stencil s1, s1
    do s_idx_1 = 1,9
    do s_idx_2 = 1,9
        if (idx+s1(s_idx_1)+s1(s_idx_2)>=1 .and. idx+s1(s_idx_1)+s1(s_idx_2)<=853128) then
            svec_p2_1_3(s_idx_1, s_idx_2) = p0_0(idx+s1(s_idx_1)+s1(s_idx_2))
        else
            svec_p2_1_3(s_idx_1, s_idx_2) = p0_0(idx)
        end if
    end do
    end do

! Stencil s1
    do s_idx_1 = 1,9
        if (idx+s1(s_idx_1)>=1 .and. idx+s1(s_idx_1)<=853128) then
            svec_p2_1_4(s_idx_1) = rhs_0(idx+s1(s_idx_1))
        else
            svec_p2_1_4(s_idx_1) = rhs_0(idx)
        end if
    end do

! Map
    call f_comp_p2_1_2(global_id_0, svec_p2_1_3, svec_p2_1_4, rhs_0(idx), p2_1(idx))


!$RF4A End Inline
end subroutine stage_kernel_1



subroutine sor_map_21(global_id_0, p0_s_0, rhs_0, p1_1)
    use singleton_module_sor_map_21, only : sor_map_21_scal
    integer, intent(In) :: global_id_0
    real, dimension(1:9), intent(In) :: p0_s_0
    real, intent(In) :: rhs_0
    real, intent(Out) :: p1_1

    ! Temp vars

    ! Call to the original scalarised subroutine

    call sor_map_21_scal(global_id_0, p0_s_0(1), p0_s_0(2), p0_s_0(3), p0_s_0(4), p0_s_0(5), p0_s_0(6), p0_s_0(7), p0_s_0(8), p0_s_0(9), rhs_0, p1_1)

end subroutine sor_map_21


subroutine sor_map_45(global_id_0, p1_s_0, rhs_0, p2_1)
    use singleton_module_sor_map_45, only : sor_map_45_scal
    integer, intent(In) :: global_id_0
    real, dimension(1:9), intent(In) :: p1_s_0
    real, intent(In) :: rhs_0
    real, intent(Out) :: p2_1

    ! Temp vars

    ! Call to the original scalarised subroutine

    call sor_map_45_scal(global_id_0, p1_s_0(1), p1_s_0(2), p1_s_0(3), p1_s_0(4), p1_s_0(5), p1_s_0(6), p1_s_0(7), p1_s_0(8), p1_s_0(9), rhs_0, p2_1)

end subroutine sor_map_45



subroutine f_rapplyt_p2_1_1(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, rhs_0_in, sv_p1_1_out, rhs_0_out)

    integer :: global_id_0
    real, dimension(9, 9) :: sv_p0_s_0_in
    real, dimension(9) :: sv_rhs_0_in
    real :: rhs_0_in
    real, dimension(9) :: sv_p1_1_out
    real :: rhs_0_out

    call f_maps_p2_1_0(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, sv_p1_1_out)
    rhs_0_out = rhs_0_in

end subroutine f_rapplyt_p2_1_1




subroutine f_maps_p2_1_0(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, sv_p1_1_out)

    integer :: global_id_0
    real, dimension(9, 9) :: sv_p0_s_0_in
    real, dimension(9) :: sv_rhs_0_in
    real, dimension(9) :: sv_p1_1_out

    integer :: i
    do i=1,9
        call sor_map_21(global_id_0, sv_p0_s_0_in(i, :), sv_rhs_0_in(i), sv_p1_1_out(i))
    end do
end subroutine f_maps_p2_1_0



subroutine f_comp_p2_1_2(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, rhs_0_in, p2_1)

    integer :: global_id_0
    real, dimension(9, 9) :: sv_p0_s_0_in
    real, dimension(9) :: sv_rhs_0_in
    real :: rhs_0_in
    real :: p2_1
    real, dimension(9) :: p1_s_0
    real :: rhs_0

    call f_rapplyt_p2_1_1(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, rhs_0_in, p1_s_0, rhs_0)
    call sor_map_45(global_id_0, p1_s_0, rhs_0, p2_1)
end subroutine f_comp_p2_1_2


end module module_sor_superkernel
