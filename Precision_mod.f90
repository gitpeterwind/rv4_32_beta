module Precision_mod

! from KPP system
!
! Definition of different levels of accuracy
! for REAL variables using KIND parameterization
!
! KPP SP - Single precision kind
  integer, parameter :: sp = selected_real_kind(6,30)
! KPP DP - Double precision kind
  integer, parameter :: dp = selected_real_kind(14,300)
! KPP QP - Quadruple precision kind
  integer, parameter :: qp = selected_real_kind(18,400)

end module Precision_mod

