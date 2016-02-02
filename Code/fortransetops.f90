subroutine is_subset(n_coalitions, n_obsv, n_agents, coalitions, N_bool, subset_bool)
    implicit none
    integer, intent(in) :: n_coalitions, n_obsv, n_agents
    integer, intent(in), dimension(0:n_coalitions-1, 0:n_agents-1) :: coalitions
    integer, intent(in), dimension(0:n_obsv-1, 0:n_agents-1) :: N_bool
    integer, intent(out), dimension(0:n_coalitions-1, 0:n_obsv-1) :: subset_bool
    integer :: i, j, k
    do i = 0, n_obsv-1
        do j = 0, n_coalitions-1
            subset_bool(j, i) = 1
            do k = 0, n_agents-1
                if (coalitions(j, k) - N_bool(i, k) < 0) then
                    subset_bool(j, i) = 0
                    exit
                end if
            end do
        end do
    end do
end subroutine is_subset

subroutine is_set(n_coalitions, n_obsv, n_agents, coalitions, N_bool, subset_bool)
    implicit none
    integer, intent(in) :: n_coalitions, n_obsv, n_agents
    integer, intent(in), dimension(0:n_coalitions-1, 0:n_agents-1) :: coalitions
    integer, intent(in), dimension(0:n_obsv-1, 0:n_agents-1) :: N_bool
    integer, intent(out), dimension(0:n_coalitions-1, 0:n_obsv-1) :: subset_bool
    integer :: i, j, k
    do i = 0, n_obsv-1
        do j = 0, n_coalitions-1
            subset_bool(j, i) = 1
            do k = 0, n_agents-1
                if (coalitions(j, k) - N_bool(i, k) /= 0) then
                    subset_bool(j, i) = 0
                    exit
                end if
            end do
        end do
    end do
end subroutine is_set
