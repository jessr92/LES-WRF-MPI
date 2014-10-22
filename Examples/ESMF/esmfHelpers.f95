module esmfHelpers
use ESMF
implicit none

contains

subroutine checkRC(rc, message)
    implicit none
    integer, intent(in) :: rc
    character (len=*), intent(in), optional :: message
    if (rc /= ESMF_SUCCESS) then
        if (present(message)) then
            call ESMF_LogWrite(message, ESMF_LOGMSG_ERROR)
        else
            call ESMF_LogWrite("Unspecified error occurred.", ESMF_LOGMSG_ERROR)
        end if
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if    
end subroutine checkRC

end module esmfHelpers

