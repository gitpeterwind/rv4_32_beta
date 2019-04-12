program tester
  use AeroFunctions,          only: testAeroFunctions     =>self_test
! use AeroFunctions,          only: testAeroFunctions     =>self_test_fracs
  use GridAllocate_mod,        only: testGridAllocate      =>self_test
  use KeyValueTypes,          only: testKeyValueTypes     =>self_test
  use TimeDate_ExtraUtil_mod,  only: testTimeDate_ExtraUtil=>self_test
  use Io_Progs_mod,            only: testIo_Progs          =>self_test
! use DefPhotolysis,          only: testDefPhotolysis     =>self_test_fracs
  use SmallUtils_mod,          only: testSmallUtils        =>self_test
  implicit none
  integer :: i
  character(len=64) :: ml
          
  do i = 1, iargc()
    call getarg(i, ml)
    write(*,*) "Testing: ",trim(ml)
    select case(ml)
    case("AeroFunctions","AeroFunctions.f90")
      call testAeroFunctions()
    case("GridAllocate_mod","GridAllocate_mod.f90")
      call testGridAllocate()
    case("KeyValueTypes","KeyValueTypes.f90")
      call testKeyValueTypes()
    case("TimeDate_ExtraUtil_mod","TimeDate_ExtraUtil_mod.f90")
      call testTimeDate_ExtraUtil()
    case("Io_Progs_mod","Io_Progs_mod.f90")
      call testIo_Progs()
!   case("DefPhotolysis","DefPhotolysis.f90")
!     call testDefPhotolysis()
    case("SmallUtils_mod","SmallUtils_mod.f90")
      call testSmallUtils()
    end select
  end do
endprogram tester
