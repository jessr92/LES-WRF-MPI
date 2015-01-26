subroutine program_haloexchange3drealexample(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    print *, 'Hello from ', model_id
end subroutine program_haloexchange3drealexample

