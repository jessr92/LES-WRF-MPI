module oasisHelpers
use mod_oasis
implicit none

contains

subroutine checkIError(ierror, componentId, componentName, message)
    implicit none
    integer, intent(in) :: ierror, componentId
    character (len=6), intent(in) :: componentName
    character (len=*), intent(in), optional :: message
    if (ierror /= 0) then
        if (present(message)) then
            call oasis_abort(componentId, componentName, message)
        else
            call oasis_abort(componentId, componentName, 'Unspecified error occured')
        end if
    end if
end subroutine checkIError

end module oasisHelpers
