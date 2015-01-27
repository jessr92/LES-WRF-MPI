subroutine program_haloexchange3drealexample(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    integer :: tile_id
    call gmcfInitCoupler(sys, tile, model_id)
    call gmcfGetTileId(tile, tile_id)
    print *, 'Hello from ', model_id, ' and tile_id ', tile_id
    call gmcfFinished(model_id)
end subroutine program_haloexchange3drealexample

