Generating connectivity matrix for 4 processes per row and 3 processes per column
module gmcfConfiguration
    implicit none
    integer, dimension(NMODELS, NMODELS) :: gmcfConnectivityMatrix = reshape( / &
        0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, &
        1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, &
        0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, &
        0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, &
        1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, &
        0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, &
        0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, &
        0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, &
        0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, &
        0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, &
        0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, &
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0 &
     /), shape(gmcfConnectivityMatrix) )
end module gmcfConfiguration
