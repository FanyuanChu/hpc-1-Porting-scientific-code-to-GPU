module module_sor_superkernel
    contains
subroutine sor_superkernel( global_id_0, p0_0, rhs_0, p3_1, state_ptr)
    real, dimension(1:853128), intent(In) :: p0_0
    real, dimension(1:853128), intent(In) :: rhs_0
    real, dimension(1:853128), intent(Out) :: p3_1
    integer, intent(In) :: global_id_0
    integer, parameter :: ST_STAGE_KERNEL_1 = 1 ! stage_kernel_1
    integer :: state
    integer :: state_ptr
    state = state_ptr ! state
! SUPERKERNEL BODY
    select case(state)
      case (ST_STAGE_KERNEL_1)
      call stage_kernel_1(global_id_0, p0_0, rhs_0, p3_1)
    end select
end subroutine sor_superkernel

subroutine stage_kernel_1(global_id_0, p0_0, rhs_0, p3_1)
! NON-MAP-ARG-DECLS: [integer, intent(In) :: global_id_0]
! arg_decls
    real, dimension(1:853128), intent(In) :: p0_0
    real, dimension(1:853128), intent(In) :: rhs_0
    real, dimension(1:853128), intent(Out) :: p3_1
! uniqueGeneratedDecls'
    integer, parameter, dimension(1:9) :: s1 = [103,10405,10407,10506,10507,10508,10607,10609,20911]
    real, dimension(1:9,1:9,1:9) :: svec_p3_1_7
    integer :: s_idx_1
    integer :: s_idx_2
    integer :: s_idx_3
    real, dimension(1:9,1:9) :: svec_p3_1_8
    real, dimension(1:9) :: svec_p3_1_9
    integer, intent(In) :: global_id_0

    integer :: idx
!    call get_global_id(idx,0)
    idx = global_id_0
!$RF4A Begin Inline
! Stencil s1, s1, s1
    do s_idx_1 = 1,9
    do s_idx_2 = 1,9
    do s_idx_3 = 1,9
        if (idx+s1(s_idx_1)+s1(s_idx_2)+s1(s_idx_3)>=1 .and. idx+s1(s_idx_1)+s1(s_idx_2)+s1(s_idx_3)<=853128) then
            svec_p3_1_7(s_idx_1, s_idx_2, s_idx_3) = p0_0(idx+s1(s_idx_1)+s1(s_idx_2)+s1(s_idx_3))
        else
            svec_p3_1_7(s_idx_1, s_idx_2, s_idx_3) = p0_0(idx)
        end if
    end do
    end do
    end do

! Stencil s1, s1
    do s_idx_1 = 1,9
    do s_idx_2 = 1,9
        if (idx+s1(s_idx_1)+s1(s_idx_2)>=1 .and. idx+s1(s_idx_1)+s1(s_idx_2)<=853128) then
            svec_p3_1_8(s_idx_1, s_idx_2) = rhs_0(idx+s1(s_idx_1)+s1(s_idx_2))
        else
            svec_p3_1_8(s_idx_1, s_idx_2) = rhs_0(idx)
        end if
    end do
    end do

! Stencil s1
    do s_idx_1 = 1,9
        if (idx+s1(s_idx_1)>=1 .and. idx+s1(s_idx_1)<=853128) then
            svec_p3_1_9(s_idx_1) = rhs_0(idx+s1(s_idx_1))
        else
            svec_p3_1_9(s_idx_1) = rhs_0(idx)
        end if
    end do

! Map
    call f_comp_p3_1_6(global_id_0, svec_p3_1_7, svec_p3_1_8, svec_p3_1_9, rhs_0(idx), p3_1(idx))


!$RF4A End Inline
end subroutine stage_kernel_1



subroutine sor_map_22(global_id_0, p0_s_0, rhs_0, p1_1)
    use singleton_module_sor_map_22, only : sor_map_22_scal
    integer, intent(In) :: global_id_0
    real, dimension(1:9), intent(In) :: p0_s_0
    real, intent(In) :: rhs_0
    real, intent(Out) :: p1_1

    ! Temp vars

    ! Call to the original scalarised subroutine

    call sor_map_22_scal(global_id_0, p0_s_0(1), p0_s_0(2), p0_s_0(3), p0_s_0(4), p0_s_0(5), p0_s_0(6), p0_s_0(7), p0_s_0(8), p0_s_0(9), rhs_0, p1_1)

end subroutine sor_map_22


subroutine sor_map_46(global_id_0, p1_s_0, rhs_0, p2_1)
    use singleton_module_sor_map_46, only : sor_map_46_scal
    integer, intent(In) :: global_id_0
    real, dimension(1:9), intent(In) :: p1_s_0
    real, intent(In) :: rhs_0
    real, intent(Out) :: p2_1

    ! Temp vars

    ! Call to the original scalarised subroutine

    call sor_map_46_scal(global_id_0, p1_s_0(1), p1_s_0(2), p1_s_0(3), p1_s_0(4), p1_s_0(5), p1_s_0(6), p1_s_0(7), p1_s_0(8), p1_s_0(9), rhs_0, p2_1)

end subroutine sor_map_46


subroutine sor_map_70(global_id_0, p2_s_0, rhs_0, p3_1)
    use singleton_module_sor_map_70, only : sor_map_70_scal
    integer, intent(In) :: global_id_0
    real, dimension(1:9), intent(In) :: p2_s_0
    real, intent(In) :: rhs_0
    real, intent(Out) :: p3_1

    ! Temp vars

    ! Call to the original scalarised subroutine

    call sor_map_70_scal(global_id_0, p2_s_0(1), p2_s_0(2), p2_s_0(3), p2_s_0(4), p2_s_0(5), p2_s_0(6), p2_s_0(7), p2_s_0(8), p2_s_0(9), rhs_0, p3_1)

end subroutine sor_map_70



subroutine f_rapplyt_p3_1_2(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, rhs_0_in, sv_p1_1_out, rhs_0_out)

    integer :: global_id_0
    real, dimension(9, 9) :: sv_p0_s_0_in
    real, dimension(9) :: sv_rhs_0_in
    real :: rhs_0_in
    real, dimension(9) :: sv_p1_1_out
    real :: rhs_0_out

    call f_maps_p3_1_1(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, sv_p1_1_out)
    rhs_0_out = rhs_0_in

end subroutine f_rapplyt_p3_1_2


subroutine f_rapplyt_p3_1_5(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, rhs_0_in, sv_p2_1_out, rhs_0_out)

    integer :: global_id_0
    real, dimension(9, 9, 9) :: sv_sv_p0_s_0_in_in
    real, dimension(9, 9) :: sv_sv_rhs_0_in_in
    real, dimension(9) :: sv_rhs_0_in_in
    real :: rhs_0_in
    real, dimension(9) :: sv_p2_1_out
    real :: rhs_0_out

    call f_comp_p3_1_4(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, sv_p2_1_out)
    rhs_0_out = rhs_0_in

end subroutine f_rapplyt_p3_1_5




subroutine f_maps_p3_1_0(global_id_0, sv_p1_s_0_in, sv_rhs_0_in, sv_p2_1_out)

    integer :: global_id_0
    real, dimension(9, 9) :: sv_p1_s_0_in
    real, dimension(9) :: sv_rhs_0_in
    real, dimension(9) :: sv_p2_1_out

    integer :: i
    do i=1,9
        call sor_map_46(global_id_0, sv_p1_s_0_in(i, :), sv_rhs_0_in(i), sv_p2_1_out(i))
    end do
end subroutine f_maps_p3_1_0


subroutine f_maps_p3_1_1(global_id_0, sv_p0_s_0_in, sv_rhs_0_in, sv_p1_1_out)

    integer :: global_id_0
    real, dimension(9, 9) :: sv_p0_s_0_in
    real, dimension(9) :: sv_rhs_0_in
    real, dimension(9) :: sv_p1_1_out

    integer :: i
    do i=1,9
        call sor_map_22(global_id_0, sv_p0_s_0_in(i, :), sv_rhs_0_in(i), sv_p1_1_out(i))
    end do
end subroutine f_maps_p3_1_1



subroutine f_maps_p3_1_3(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, sv_sv_p1_1_out_out, sv_rhs_0_out_out)

    integer :: global_id_0
    real, dimension(9, 9, 9) :: sv_sv_p0_s_0_in_in
    real, dimension(9, 9) :: sv_sv_rhs_0_in_in
    real, dimension(9) :: sv_rhs_0_in_in
    real, dimension(9, 9) :: sv_sv_p1_1_out_out
    real, dimension(9) :: sv_rhs_0_out_out

    integer :: i
    do i=1,9
        call f_rapplyt_p3_1_2(global_id_0, sv_sv_p0_s_0_in_in(i, :, :), sv_sv_rhs_0_in_in(i, :), sv_rhs_0_in_in(i), sv_sv_p1_1_out_out(i, :), sv_rhs_0_out_out(i))
    end do
end subroutine f_maps_p3_1_3


subroutine f_comp_p3_1_4(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, sv_p2_1_out)

    integer :: global_id_0
    real, dimension(9, 9, 9) :: sv_sv_p0_s_0_in_in
    real, dimension(9, 9) :: sv_sv_rhs_0_in_in
    real, dimension(9) :: sv_rhs_0_in_in
    real, dimension(9) :: sv_p2_1_out
    real, dimension(9, 9) :: sv_p1_s_0_in
    real, dimension(9) :: sv_rhs_0_in

    call f_maps_p3_1_3(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, sv_p1_s_0_in, sv_rhs_0_in)
    call f_maps_p3_1_0(global_id_0, sv_p1_s_0_in, sv_rhs_0_in, sv_p2_1_out)
end subroutine f_comp_p3_1_4



subroutine f_comp_p3_1_6(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, rhs_0_in, p3_1)

    integer :: global_id_0
    real, dimension(9, 9, 9) :: sv_sv_p0_s_0_in_in
    real, dimension(9, 9) :: sv_sv_rhs_0_in_in
    real, dimension(9) :: sv_rhs_0_in_in
    real :: rhs_0_in
    real :: p3_1
    real, dimension(9) :: p2_s_0
    real :: rhs_0

    call f_rapplyt_p3_1_5(global_id_0, sv_sv_p0_s_0_in_in, sv_sv_rhs_0_in_in, sv_rhs_0_in_in, rhs_0_in, p2_s_0, rhs_0)
    call sor_map_70(global_id_0, p2_s_0, rhs_0, p3_1)
end subroutine f_comp_p3_1_6


end module module_sor_superkernel
