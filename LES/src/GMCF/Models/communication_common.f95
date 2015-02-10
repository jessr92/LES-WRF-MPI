module communication_common
use fortran_helper
integer, parameter :: topTag = 1, bottomTag = 2, leftTag = 3, rightTag = 4
integer, parameter :: zbmTag = 5
integer, parameter :: leftSideTag = 6, rightSideTag = 7
integer, parameter :: dxTag = 8, dyTag = 9, collect3DReal4Tag = 10
integer, parameter :: topLeftTag = 11, topRightTag = 12, bottomLeftTag = 13, bottomRightTag = 14
integer, parameter :: globalSumTag = 15, globalMaxTag = 16, globalMinTag = 17
integer, parameter :: leftNeighbour = 1, rightNeighbour = 2, topNeighbour = 3, bottomNeighbour = 4
end module

