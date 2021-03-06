module DO3SE_mod

  use CheckStop_mod,  only : CheckStop
  use LandDefs_mod, only : LandDefs, LandType ! SPOD
  use LocalVariables_mod,  only : L ! &
!         t2C  => L%t2c  &! surface temp. at height 2m (deg. C)
!        ,vpd  => L%vpd  &! vapour pressure deficit (kPa)
!        ,SWP  => L%SWP  &! soil water potential (MPa)
!        ,PARsun => L%PARsun      &! photosynthetic active radn. for sun-leaves
!        ,PARshade => L%PARshade       &!  " " for shade leaves
!        ,LAIsunfrac => L%LAIsunfrac      ! fraction of LAI in sun

  use Config_module,   only : NLANDUSEMAX, MasterProc, USES
  use Debug_module,    only:  DEBUG   ! -> DEBUG%DO3SE
  use SmallUtils_mod,  only : find_index
  use TimeDate_mod,    only : current_date, daynumber, print_date

  implicit none
  private

!=============================================================================
!  Contains

 public :: Init_DO3SE     ! Reads DOSEinputs.csv -> gmax + all f params
 public :: g_stomatal     ! produces g_sto and g_sun
 public :: fPhenology     !  -> f_phen

!-----------------------------------------------------------------------------
! Notes: Basis is Emberson et al, EMEP Report 6/2000
! Numbers updated to Mapping Manual, 2004 and changes recommended
!  in Tuovinen et al, 2004
!

 ! 2 ) Phenology part

 !******   Data to be read from Phenology_inputs.dat:

  type, public :: do3se_type
     character(len=30) :: code
     character(len=30) :: name
     real:: g_max           ! max. value conductance g_s
     real:: f_min           ! min. value Conductance, factor
     real:: f_phen_a        ! f_phen a  (very start of season
     real:: f_phen_b        ! f_phen b
     real:: f_phen_c        ! f_phen c
     real:: f_phen_d        ! f_phen d
     real:: f_phen_Slen     ! Length of Startup  (days) = e
     real:: f_phen_Elen     ! Length of End period (days) = f
     real:: Astart_rel      ! 
     real:: Aend_rel        ! 
     real:: f_light         ! light coefficient
     real:: T_min           ! temperature when f_temp starts
     real:: T_opt           ! temperature when f_temp max.
     real:: T_max           ! temperature when f_temp stops
     real:: RgsS            ! ground surface resistance, Sulphur
     real:: RgsO            ! ground surface resistance, Ozone
     real:: VPD_max         ! threshold VPD when relative f = f_min
     real:: VPD_min         ! threshold VPD when relative f = 1
     real:: VPDcrit         ! threshold SumVPD for TC/RC/IAM_CR
     real:: SWP_max         ! threshold SWP when relative f = 1
     real:: PWP             ! threshold SWP when relative f = f_min
                        ! and assumed equal to permanent wilting point
     real:: rootdepth        ! root depth (mm)
     real:: Lw               ! cros-wind leaf dimension (ony used for IAM)
  end type do3se_type

  type(do3se_type), public, dimension(NLANDUSEMAX) :: do3se

  ! For some veg we have a SumVPD limitation. Usually just for a few,
  ! so we assume max 3 for now
  integer, private, parameter :: MAXnSumVPD=10
  integer, public, save       :: nSumVPD
  integer, public, dimension(MAXnSumVPD), save :: SumVPD_LC

  real, private, dimension(7) ::  needed      ! For debugging


contains
!=======================================================================
  !=======================================================================
  subroutine Init_DO3SE(io_num,fname,ncodes,wanted_codes,io_msg)
  !=======================================================================
      integer, intent(in) :: io_num
      character(len=*), intent(in) :: fname 
      integer, intent(in) ::  ncodes ! number of land-codes in mapped data
      character(len=*), dimension(:), intent(in) :: wanted_codes 
      character(len=*), intent(inout) :: io_msg 
      character(len=300)  :: inputline
      integer :: iLC, ios, nLC
      type(do3se_type) :: input_do3se
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Read data (still old-style reading, on all processors)

      io_msg="ok"
      open(unit=io_num,file=fname,status="old",&
                      action="read",position="rewind",iostat=ios)
      call CheckStop(ios,"ERROR : Opening " // fname)


      !------ Read in file. Lines beginning with "!" are taken as
      !       comments and skipped

       !APR2013 iLC = 1    
       nLC = 0
       nSumVPD = 0
       do
            read(unit=io_num,fmt="(a200)",iostat=ios) inputline

            if ( ios /= 0 ) then !! End of File, hopefully
                exit
            end if

            if( inputline(1:1) == "#" ) then ! Is a  comment
                !print *, "COMMENT: ", trim(inputline)
                cycle
            end if

            read(unit=inputline,fmt=*) input_do3se
            !APR2013 read(unit=inputline,fmt=*) do3se(iLC)
            iLC = find_index( input_do3se%code, wanted_codes)

            if ( iLC < 1 ) then
                if(MasterProc) write(*,*) " DO3SE skips iLC", iLC, input_do3se%code 
                cycle
            end if

            do3se(iLC) = input_do3se  

            if ( DEBUG%DO3SE>0 .and. MasterProc ) then
                write(*,*) " DO3SE iLC", iLC,  do3se(iLC)%code, wanted_codes(iLC)
            end if
            if ( do3se(iLC)%VPDcrit > 0.0 ) then
              nSumVPD = nSumVPD + 1
              call CheckStop( nSumVPD > MAXnSumVPD, "DO3SE nSumVPD")
              SumVPD_LC(nSumVPD) = iLC
              if(DEBUG%DO3SE>0 .and. MasterProc) &
                write(*,*)'VPDlimit ',do3se(iLC)%VPDcrit,' for iLC ',iLC, nSumVPD
            end if

            !APR2013 call CheckStop( wanted_codes(iLC), do3se(iLC)%code, "DO3SE MATCHING")
            !APR2013 iLC = iLC + 1
            nLC = nLC + 1
       end do
       close(unit=io_num)

       call CheckStop( nLC /= ncodes, "Init_DO3SE didn't find all codes")

  end subroutine Init_DO3SE

!=======================================================================

    subroutine g_stomatal(iLC, debug_flag)
!=======================================================================

!    Calculates stomatal conductance g_sto based upon methodology from 
!    EMEP MSC-W Note 6/00 and Mapping Manual (2004), and revisions (Simpson
!    and Emberson, Chapter 5, EMEP Rep 1/2006, Mapping Manual revisions, 2007,
!    and l. Emberson Notes from Forest group, Dec. 2007):
!
!    g_sto = [g_max * f_pot * f_light * f_temp * f_vpd * f_swp ]/41000.0
!

     integer, intent(in) :: iLC
     logical, intent(in) :: debug_flag
     character(len=20) :: txtdate

! Outputs:
!    L%g_sto, L%g_sun       ! stomatal conductance for canopy and sun-leaves

 ! environmental f factors

 !FE2013 real :: f_sun         ! light-factor for upper-canopy sun-leaves
 !FE2013 real :: f_shade       ! shade-leaf contribution to f_light

  real :: dg, dTs, bt   ! for temperate calculations
  real :: mmol2sm       !  Units conversion, mmole/m2/s to s/m
  real :: tmp


        
!..1 ) Calculate f_phen. Max value is 1.0.
!---------------------------------------
!   Not done here!  - these calculations only needed once per day
!--------------------------------------------------------------------


!..2 ) Calculate f_light 
!---------------------------------------
!    Calculate f_light, using methodology as described in Emberson et 
!    al. (1998), eqns. 31-35, based upon sun/shade method of  
!    Norman (1979,1982)

    L%f_sun   = (1.0 - exp (-do3se(iLC)%f_light*L%PARsun  ) ) 
    L%f_shade = (1.0 - exp (-do3se(iLC)%f_light*L%PARshade) ) 

    L%f_light = L%LAIsunfrac * L%f_sun + (1.0 - L%LAIsunfrac) * L%f_shade

!--------------------------------------------------------------------
  

!..3) Calculate  f_temp
!---------------------------------------
! Asymmetric  function from Mapping Manual
! NB _ much more efficient to tabulate this - do later!
  
  dg  =    ( do3se(iLC)%T_opt - do3se(iLC)%T_min )
  bt  =    ( do3se(iLC)%T_max - do3se(iLC)%T_opt ) / dg
  dTs = max( do3se(iLC)%T_max - L%t2C, 0.0 )      !CHECK why max?
  tmp = dTs / ( do3se(iLC)%T_max - do3se(iLC)%T_opt )
  L%f_temp = ( L%t2C - do3se(iLC)%T_min ) / dg *  tmp**bt

  L%f_temp = max( L%f_temp, 0.01 )  ! Revised usage of min value during 2007


!..4) Calculate f_vpd
!---------------------------------------

 L%f_vpd = do3se(iLC)%f_min + &
          (1.0-do3se(iLC)%f_min) * (do3se(iLC)%VPD_min - L%vpd )/ &
              (do3se(iLC)%VPD_min - do3se(iLC)%VPD_max )
 L%f_vpd = min(L%f_vpd, 1.0)
 L%f_vpd = max(L%f_vpd, do3se(iLC)%f_min)


!..5) Calculate f_swp
!---------------------------------------

  !/  Soil water effects. We now used the soil-moisture
  !   index from ECMWF if possible, otherwise some equivalent.
  !   Once per day, but for simplicity we do it every time-step.

  ! ************************************
   !FEB2013 Set fSW in CellMet
  ! ************************************


!.. And finally,
!---------------------------------------
!  ( with revised usage of min value for f_temp during 2007)

   L%f_env = L%f_vpd * L%fSW
   L%f_min  = do3se(iLC)%f_min
   L%f_env = max( L%f_env, L%f_min )
   L%f_env = max( L%f_temp, 0.01) * L%f_env

   L%f_env = L%f_phen * L%f_env * L%f_light  ! Canopy average

! From mmol O3/m2/s to s/m given in Jones, App. 3, gives 41000 for 20 deg.C )
!   (should we just use P=100 hPa?)

   mmol2sm = 8.3144e-8 * L%t2       ! 0.001 * RT/P

   L%g_sto = do3se(iLC)%g_max * L%f_env * mmol2sm 

   L%g_sun = do3se(iLC)%g_max * mmol2sm * L%f_phen * L%f_sun * &
         max( do3se(iLC)%f_min,  L%f_temp * L%f_vpd * L%fSW )


   if ( DEBUG%DO3SE>0 .and. debug_flag ) then ! EXTRA
      txtdate =  print_date(current_date)
      !txtdate =  print_date()
      !print *, "TXT PD CD2", trim(txtdate)
      if(iLC>=20)  write(*,"(2a,i5,L2,99g10.3)") "IN RSUR gstomatal ", &
              print_date(), iLC, USES%SOILWATER, L%PARsun, L%PARshade,&
              do3se(iLC)%g_max, L%g_sto, L%f_env,  L%f_phen, L%f_vpd,&
              L%fSW, L%g_sto * L%f_sun/L%f_light, L%g_sun 
   end if

    if ( DEBUG%DO3SE>0 ) then
        needed = (/ L%t2C,L%t2,L%vpd ,L%SWP ,&
                    L%PARsun ,L%PARshade ,L%LAIsunfrac /)
        if ( any( needed(:) < -998.0 )) then
          print *, needed
          call CheckStop("ERROR in g_stomatal, Missing data")
        end if

        !SPOD if ( debug_flag.and.current_date%seconds==0 .and. iLC<5  &
         !SPOD     .and. current_date%hour==12.and. iLC<5 )  then
        if ( debug_flag.and.current_date%seconds==0 .and. LandType(iLC)%is_forest  &
             .and. current_date%hour==12 )  then
           write(*,"(a,2i3,99f8.3)") "F-DO3SE ", daynumber, &!current_date%hour, &
               iLC, L%f_phen, L%f_light,L%f_sun, L%f_temp, L%f_vpd, L%fSW, &
                 L%fSW*L%f_vpd, L%f_min, L%f_env

          ! Met params, except soil water  where fSW =~ REW

           write(*,"(a,2i3,2f7.2,2f8.3,9f9.2)") "M-DO3SE ", daynumber, &!current_date%hour, &
               iLC, L%LAI, L%t2C, L%vpd, L%fSW, L%PARsun ,L%PARshade ,L%LAIsunfrac
        end if
    end if
         

  end subroutine g_stomatal

!===========================================================================

 !elemental function fPhenology(iLC,jday,SGS,EGS,debug_flag) result (fphen)
 function fPhenology(iLC,jday,SGS,EGS,debug_flag) result (fphen)
  real :: fphen

! Input
  integer, intent(in) :: iLC
  integer, intent(in) :: jday
  integer, intent(in):: SGS, EGS
  logical, intent(in) :: debug_flag
  real  :: a,b,c,d,Slen,Elen,Astart, Aend
  real  :: gsf  ! factor to scale for short growing-seasons

 !CEH 2014 GAEZ
  gsf =  1.0
  if( EGS - SGS  < 90 ) then
     gsf = (EGS - SGS)/90.0
     if( debug_flag ) write(*,"(a,2i5,f8.4)") "DO3SE gsf ", SGS, EGS, gsf
  end if
    

        a =  do3se(iLC)%f_phen_a
        b =  do3se(iLC)%f_phen_b
        c =  do3se(iLC)%f_phen_c
        d =  do3se(iLC)%f_phen_d
        Slen =  gsf * do3se(iLC)%f_phen_Slen  ! e
        Elen =  gsf * do3se(iLC)%f_phen_Elen  ! f

        Astart   = SGS  + gsf * do3se(iLC)%Astart_rel
        Aend   = EGS  - gsf * do3se(iLC)%Aend_rel


        if ( jday <  SGS ) then
                fphen = 0.0
        else if ( jday <= Astart ) then
                fphen = a
        else if ( jday <= Astart+Slen ) then
                fphen = b + (c-b) * ( jday-Astart)/real(Slen)
        else if ( jday <= Aend-Elen ) then
                fphen = c
        else if ( jday < Aend ) then
                fphen = d + (c-d) * ( Aend-jday)/real(Elen)
        else if ( jday <= EGS ) then
                fphen = d
        else
                fphen = 0.0
        end if

end function fPhenology


end module DO3SE_mod
