module DA_mod
implicit none
logical, parameter ::     &
  DEBUG_DA_1STEP=.false.    ! run only 1 DA step (no adv/chem)
endmodule DA_mod
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
module DA_3DVar_mod
use CheckStop_mod,     only: CheckStop
use Config_module,only: ANALYSIS
implicit none
character(len=*), parameter  ::  &
  mname = 'DA_3DVar_mod', &
  errmsg= 'No 3DVar available. Need to recompile, e.g. make MACC-3DVar'
integer, parameter :: NTIMING_3DVAR=0, T_3DVAR=0
contains
!-----------------------------------------------------------------------
! Empty calls, for "standrd" model compilation
!-----------------------------------------------------------------------
subroutine DA_3DVar_Init(status)
! --- in/out ----------------------------
integer, intent(out)         ::  status
! --- const -----------------------------
character(len=*), parameter  ::  rname = mname//'/DA_3DVar_Init'
! --- begin -----------------------------
  write(*,"(A,': ',A)")rname,errmsg
  status = 1
endsubroutine DA_3DVar_Init
!-----------------------------------------------------------------------
subroutine DA_3DVar_Done(status)
! --- in/out ----------------------------
integer, intent(out)         ::  status
! --- const -----------------------------
character(len=*), parameter  ::  rname = mname//'/DA_3DVar_Done'
! --- begin -----------------------------
  write(*,"(A,': ',A)")rname,errmsg
  status = 1
endsubroutine DA_3DVar_Done
!-----------------------------------------------------------------------
subroutine main_3dvar(status)
! --- in/out ----------------------------
integer, intent(out)         ::  status
! --- const -----------------------------
character(len=*), parameter  ::  rname = mname//'/main_3dvar'
! --- begin -----------------------------
  write(*,"(A,': ',A)")rname,errmsg
  status = 1
endsubroutine main_3dvar
endmodule DA_3DVar_mod
