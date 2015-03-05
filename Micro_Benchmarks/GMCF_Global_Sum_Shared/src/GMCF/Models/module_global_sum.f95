module module_global_sum

use gmcfAPI

integer :: stillToWrite = 0
integer :: stillToRead = 0
integer :: opResult = 0

contains

subroutine getGlobalSumOfGMCF(model_id, value)
    implicit none
    integer, intent(in) :: model_id
    real(kind=4), intent(inout) :: value
    call getGlobalOp(model_id, value, 1)
end subroutine getGlobalSumOfGMCF

subroutine getGlobalOp(model_id, value, tag)
    implicit none
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    if (model_id .eq. 1) then
        call getGlobalOpMaster(value, tag)
    else
        call getGlobalOpNotMaster(value, tag)
    end if
    call gmcfLockGlobalOpSpinLock()
    stillToRead = stillToRead - 1
    call gmcfUnlockGlobalOpSpinLock()
end subroutine getGlobalOp

subroutine getGlobalOpMaster(value, tag)
    integer, intent(in) :: tag
    real(kind=4), intent(inout) :: value
    do while (stillToRead .ne. 0)
    end do
    call gmcfLockGlobalOpSpinLock()
    stillToWrite = INSTANCES
    stillToRead = INSTANCES
    opResult = 0
    call gmcfUnlockGlobalOpSpinLock()
    call reduce(value, tag)
end subroutine getGlobalOpMaster

subroutine getGlobalOpNotMaster(value, tag)
    integer, intent(in) :: tag
    real(kind=4), intent(inout) :: value
    do while (stillToWrite .eq. 0)
    end do
    call reduce(value, tag)
end subroutine getGlobalOpNotMaster

subroutine reduce(value, tag)
    integer, intent(in) :: tag
    real(kind=4), intent(inout) :: value
    call gmcfLockGlobalOpSpinLock()
    if (tag .eq. 1) then
        opResult = opResult + tag
    else
        print*, 'Unknown tag ', tag
    end if
    stillToWrite = stillToWrite - 1
    call gmcfUnlockGlobalOpSpinLock()
    do while (stillToWrite .ne. 0)
    end do
    value = opResult
end subroutine reduce

end module module_global_sum

