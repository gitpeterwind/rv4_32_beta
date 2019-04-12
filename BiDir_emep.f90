module BiDir_emep
  ! DUMMY
  ! Will act as interface between emep-ctm and BiDir_module

  use Config_module, only : USES, MasterProc
  implicit none
  private

  character(len=*), parameter, public :: BiDir_emep_Status='TOBEDONE'

  public :: Init_BiDir  ! FUTURE

contains
  subroutine Init_BiDir()
     logical, save :: first_call = .true.
     character(len=*), parameter :: dtxt='IniBD:'
     if ( USES%BIDIR .and. MasterProc .and.  first_call) then
        write(*,*) dtxt//' FUTURE INIT'
     end if
  end subroutine Init_BiDir
end module BiDir_emep
