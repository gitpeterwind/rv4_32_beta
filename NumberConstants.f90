module NumberConstants
  implicit none
  private

! from KPP system
! Definition of different levels of accuracy
! for REAL variables using KIND parameterization
!
! KPP SP - Single precision kind
  integer, public, parameter :: sp = selected_real_kind(6,30)
! KPP DP - Double precision kind
  integer, public, parameter :: dp = selected_real_kind(14,300)
! CYGWIN can't handle quad precision, so we re-define
! KPP QP - Quadruple precision kind
  integer, public, parameter :: qp = dp ! selected_real_kind(18,400)

!DEWS working precision can be changed here
! Typically should be dp, but qp for testing

  integer, public, parameter :: wp = qp

!integer, public, parameter :: dp = kind(0.0d0)  ! Double precision real(qp) kind

! Sentinel values
!real(qp), public, parameter :: UNDEF_D = -huge(0.0_dp)
    real,     public, parameter :: UNDEF_R = -huge(0.0)
    real(wp), public, parameter :: UNDEF_WP = -huge(0.0_wp)
    integer,  public, parameter :: UNDEF_I = -huge(0)

end module NumberConstants
