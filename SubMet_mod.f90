! <SubMet_mod.f90 - A component of the EMEP MSC-W Chemical transport Model>
!*****************************************************************************! 
module SubMet_mod
!=============================================================================
!+
! Description
!  Module for setting some local grid-cell data (mainly from NWP)
!  and for calculating sub-grid  meteorology for each land-use. 
!  The sub-grid part of this module is also undergoing constant change!!
!=============================================================================

use CheckStop_mod, only: StopAll
use Config_module, only:  NLANDUSEMAX, FluxPROFILE, LANDIFY_MET, USES &
                      , USE_ZREF & ! TEST
                      , Zmix_ref !height at which concentration above different landuse are considered equal 
use Debug_module,  only: DEBUG_SUBMET  ! Needs DEBUG_RUNCHEM to get debug_flag
use Functions_mod, only: T_2_Tpot  !needed if FluxPROFILE == Ln95
use MetFields_mod, only: ps        !needed if FluxPROFILE == Ln95

use BLPhysics_mod, only: MIN_USTAR_LAND
use CheckStop_mod, only: CheckStop
use LandDefs_mod,   only: LandType, LandDefs
use Landuse_mod,    only: LandCover
use LocalVariables_mod, only: Grid, SubDat
use MicroMet_mod, only:  PsiM, AerRes    !functions
use MicroMet_mod, only:  Launiainen1995
use PhysicalConstants_mod, only: PI, RGAS_KG, CP, GRAV, KARMAN, CHARNOCK, T0

implicit none
private

  public :: Get_Submet    ! calculates met. data for sub-grid areas
  type(SubDat), public, dimension(0:NLANDUSEMAX), save :: Sub

contains
!=======================================================================

  subroutine Get_Submet(iL, debug_flag )

!---------------------------------------------------------------
!  Sub-grid calculations of  stability, Ra and ustar for this landuse
!---------------------------------------------------------------
!
!..The profile manipulation is introduced to calculate different Ra and Rb
!..values for different landuse types (e.g. from SEI). Thus, different z0,
!..are assumed within each EMEP square, from which one averaged value is 
!..available as the basic input from the NWP-model. Therefore, we first have a
!..grid square average for u*, T*, L and z0 (i.e. from the NWP-model).  Then,
!..we calculate the wind speed u from the wind profile (according to the
!..M-O similarity) and the NWP-model data, and from this u we calculate
!..new u* values for each z0, i.e. landuse, within each grid square. Here,
!..we have to use the old L, even although L=f(u*), since the new u* is
!..unknown before it is calculated. (An iteration process is possible.) 
!..Anyway, by using the new u*'s, we obtain
!..new T*'s, L's for each landuse type and finally Ra's and ustars for each
!..subgrid area.
!
!.. For further details, see EMEP report 1/2003 (and before that 3/95 ...)
!
! The subroutine Get_Submet also generates output for 
! two terms utilized in Rsurface, namely rh (the relative humidity
! term required for the evaluation of the stomatal compensation point 
! point for NH_3) and vpd (the vapour pressure deficit term required
! for the evaluation of stomatal conductance).

!----------------------------------------------------------------- 
!..In

   integer, intent(in) :: iL      ! lu index
   logical, intent(in) :: debug_flag   ! set true for wanted grid square

   ! IMPORTANT - ASSUMES INITIAL VALUES SET FOR USTAR, INVL, ....

!.. Local
    real :: rho_surf               ! Density at surface (2 m), kg/m3
    real :: z_1m                   !  1m above vegetation
    real :: z_3m                   !  3m above ground, or top of trees
    real :: z_3md                  !  minus displacemt ht.
    real :: Zmix_refd              !  Zmix_ref minus displacemt
    
    logical, save ::  my_first_call = .true.
    integer, parameter ::  NITER = 1           ! no. iterations to be performed
    !testing integer ::  NITER     ! no. iterations to be performed

    integer :: iter                ! iteration variable

   ! For vapour pressure calculations

    real, parameter :: ESAT0=611.0   ! saturation vapour pressure at 
                                     ! T=0 deg. C (Pa)
      
    real :: qw                       ! specific humidity (kg/kg) corrected  
                                     ! down to z_0+d metres above the ground 
    real :: esat    ! saturation vapour pressure  (Pa)
    real :: e       ! vapour pressure at surface
    real :: Ra_2m   ! to get 2m qw

    real :: theta2


    ! initial guesses for u*, t*, 1/L
        Sub(iL)%ustar  = Grid%ustar      ! First guess = NWP value
        Sub(iL)%invL   = 0.0       ! Start at neutral...
        Sub(iL)%Hd     = Grid%Hd         ! First guess = NWP value
        Sub(iL)%LE     = Grid%LE         ! First guess = NWP value
        Sub(iL)%t2     = Grid%t2         ! First guess = NWP value
        Sub(iL)%t2C    = Grid%t2C        ! First guess = NWP value
        Sub(iL)%is_veg = LandType(iL)%is_veg
        Sub(iL)%is_ice = LandType(iL)%is_ice

        Sub(iL)%is_water  = LandType(iL)%is_water
        Sub(iL)%is_forest = LandType(iL)%is_forest
        Sub(iL)%is_crop   = LandType(iL)%is_crop   

        !if( USES%SOILWATER )
          Sub(iL)%fSW    = Grid%fSW ! MAR2013 - not needed, but for safety
!GMO3
  if( index( LandDefs(iL)%name , 'Irrigated' ) > 0 ) then
    Sub(iL)%fSW = 1.0
  end if
!GMO3

     ! If NWP thinks this is a sea-square, but we anyway have land,
     ! the surface temps will be wrong and so will stability gradients.
     ! Use of LANDIFY_MET should have corrected this to some extent. If
     ! not in use, as a simple substitute, we assume neutral conditions for these
     ! situations.

      if ( .not. LANDIFY_MET .and. &
              Grid%is_mainlysea  .and. (.not. Sub(iL)%is_water) ) then
           Sub(iL)%invL = 0.0
           Sub(iL)%Hd   = 0.0
      end if


!    The zero-plane displacement (d) is the height that
!     needs to be added in order to make the profile theories
!     work over a tall vegetation (see e.g. Stull (1988), p.381). 
!     This corresponds to moving our co-ordinate system upwards 
!     by a distance d.
! 
!     by definition: u(z0+d) = 0
!
!     it has been observed that d is approximately 0.7 times
!     the mean height of the vegetation (h) and z0=h/10
!     (see e.g. Stull, 1998, Garratt, 1992.).
!     The reference height for u* transformation is then
!     taken arbitrarily at about 45m, the height of the
!     centre of the EMEP grid cell.
          

!.. For water, we introduce a new zero plane 
!   displacement and use the Charnock relation to calculate the z0-values
! nb - addded max 1cm limit for z0 over sea, because of problems
! caused by z0>1m. Garratt (section 4.1, Fig 4.2) suggested that Charnock's
! relation is only valid for  u* < 1 m/s, which gives z0 < 1 cm/s.



        if ( Sub(iL)%is_water ) then ! water
             Sub(iL)%d  = 0.0
             Sub(iL)%z0 = CHARNOCK * Sub(iL)%ustar * Sub(iL)%ustar/GRAV
           ! We use the same restriction on z0 as in Berge, 1990 
           ! (Tellus,42B,389-407)
             Sub(iL)%z0 = max( Sub(iL)%z0 ,1.5e-5)
             z_1m   = 1.0       ! 1m above sea surface
             z_3m   = 3.0       ! 3m above sea surface

        else if ( Sub(iL)%is_forest ) then ! forest
           ! We restrict z0 to 1.0m, since comparison with CarboEurope
           ! results shows that this provides better u* values for
           ! forests.
             Sub(iL)%d  =  0.78 * Sub(iL)%hveg   ! Jarvis, 1976
             Sub(iL)%z0 =  min( 0.07 * Sub(iL)%hveg, 1.0 )
             z_1m   = (Sub(iL)%hveg + 1.0) - Sub(iL)%d
             z_3m   = max(3.0,Sub(iL)%hveg)
        else
             Sub(iL)%d  =  0.7 * Sub(iL)%hveg
             Sub(iL)%z0 = max( 0.1 * Sub(iL)%hveg, 0.001) !  Fix for deserts, 
               ! ice, snow (where, for bare ground, h=0 and hence z0=0)

           !Heights relative to displacement height, d:

             z_1m   = (Sub(iL)%hveg + 1.0) - Sub(iL)%d
             z_3m   = max(3.0,Sub(iL)%hveg)

        end if
          
        if ( USE_ZREF ) then  !EXPERIMENTAL. Not recommended so far
           Sub(iL)%z_refd = Grid%z_ref
        else
           Sub(iL)%z_refd = Grid%z_ref - Sub(iL)%d  !  minus displacement height
        end if
        z_3md  = z_3m  - Sub(iL)%d               !  minus displacement height


        rho_surf = Grid%psurf/(RGAS_KG * Sub(iL)%t2 )


        if( Grid%is_allsea ) then
          Sub(iL)%ustar = Grid%ustar
          Sub(iL)%invL  = Grid%invL  
        else  ! Calculate ustar, invL for each landcover

    !NITER = 1
    !TEST if ( Grid%Hd > -1 ) NITER = 2  ! Almost neutral to unstable
    !TEST if ( Grid%Hd > 1  ) NITER = 4  ! more unstable

     if ( FluxPROFILE == "Ln95") then !TESTING

        call StopAll("Ln95 disabled for ESX testing. Needs more work anyway")

        theta2 = Grid%t2 * T_2_Tpot( Grid%psurf )
        call Launiainen1995( Grid%u_ref, Sub(iL)%z_refd, Sub(iL)%z0, Sub(iL)%z0, &
         theta2, Grid%theta_ref, Sub(iL)%invL )

        Sub(iL)%ustar = Grid%u_ref * KARMAN/ &
         (log( Sub(iL)%z_refd/Sub(iL)%z0 ) - PsiM( Sub(iL)%z_refd*Sub(iL)%invL)&
           + PsiM( Sub(iL)%z0*Sub(iL)%invL ) )

        if (  DEBUG_SUBMET .and. debug_flag ) then
            write(6,"(a12,i2,i3,5f8.3,10f12.3)") "VDHH  SUBI", iter,iL, &
                Sub(iL)%hveg, Sub(iL)%z0, Sub(iL)%d, &
                  Sub(iL)%z_refd, z_3md, &
                 Sub(iL)%invL, Sub(iL)%ustar, Grid%invL, Grid%ustar
            write(6,"(a12,i2,i3,5f8.3,10f12.3)") "VDHH  ZZZZ", iter,iL, &
                Grid%z_mid, Grid%z_ref, Sub(iL)%z_refd
        end if

       if( DEBUG_SUBMET .and. &
            Sub(iL)%invL > 10.0 .or. Sub(iL)%invL < -10.0 ) then
           call CheckStop("FluxPROFILE STOP")
        end if

   else if ( FluxPROFILE == "Iter" ) then

    do iter = 1, NITER 

        ! ****
        !   PsiM calculates the stability functions for momentum
        !   at heights z_ref (about 45m) & z0
        ! ****               
        !..calculate friction velocity based first on NWP-model PsiM-values 
        !..and u_ref. The NWP-model PsiM-values are used despite the fact that
        !..L=F(u*), since we do not know the EMEP subgrid averaged 
        !..z0-values ...

       if ( DEBUG_SUBMET .and. debug_flag ) then
            write(6,"(a12,i2,i3,5f8.3,2f12.3)") "SUBMET ITER", iter,iL, &
                Sub(iL)%hveg, Sub(iL)%z0, Sub(iL)%d, &
                  Sub(iL)%z_refd, z_3md, Sub(iL)%invL, Sub(iL)%ustar
       end if

    !  We must use L (the Monin-Obukhov length) to calculate deposition,
    ! Thus, we calculate T* and then L, based on sub-grid data. 

    ! New 1/L value ....

        Sub(iL)%invL =  -KARMAN * GRAV * Sub(iL)%Hd / &
           ( CP * rho_surf * Sub(iL)%ustar**3 * Sub(iL)%t2)

      !.. we limit the range of 1/L to prevent numerical and printout problems
      !   This range is very wide anyway.

        ! Sub(iL)%invL  = max( -1.0, Sub(iL)%invL ) !! limit very unstable
        ! Sub(iL)%invL  = min(  1.0, Sub(iL)%invL ) !! limit very stable

      ! To a good approx we could omit the PsiM(z0/L) term, but needed at ca. invL->-1

        Sub(iL)%ustar = Grid%u_ref * KARMAN/ &
         (log( Sub(iL)%z_refd/Sub(iL)%z0 ) &
            - PsiM( Sub(iL)%z_refd*Sub(iL)%invL)  &
            + PsiM( Sub(iL)%z0*Sub(iL)%invL    )) 

          if (  DEBUG_SUBMET .and. debug_flag ) then
              write(6,"(a12,i2,i3,5f8.3,2f12.3)") "SUBMET ITERi ", iter,iL, &
                Sub(iL)%hveg, Sub(iL)%z0, Sub(iL)%d, &
                  Sub(iL)%z_refd, z_3md, Sub(iL)%invL, Sub(iL)%ustar
              write(6,"(a12,i3,3f7.1,20g11.3)") "SUBMET ITERA ",iL, &
                Sub(iL)%z0, Sub(iL)%d, &
                Sub(iL)%z_refd, 0.001*Grid%psurf, Sub(iL)%t2, rho_surf, &
               Sub(iL)%Hd, Sub(iL)%ustar, Sub(iL)%invL , &
               log( Sub(iL)%z_refd/Sub(iL)%z0 ), &
                PsiM( Sub(iL)%z_refd*Sub(iL)%invL )
           end if

       Sub(iL)%ustar = max( Sub(iL)%ustar, MIN_USTAR_LAND )
    end do ! iter
  else
     call StopAll("Incorrect FluxPROFILE")

  end if ! FluxPROFILE

 end if ! allsea

    if (  DEBUG_SUBMET .and. debug_flag ) then
        write(6,"(a12,10f9.3)") "SUBMET" // trim(FluxProfile), Sub(iL)%z0, &
         Sub(iL)%d, Sub(iL)%z_refd, 0.001*Grid%psurf, Sub(iL)%t2, rho_surf, &
         Sub(iL)%Hd, Sub(iL)%ustar, Sub(iL)%t2, Sub(iL)%invL
        write(*,*) "UKDEP LOGICS ", iL, &
         Sub(iL)%is_water, Sub(iL)%is_forest, Grid%is_allsea


        if ( my_first_call ) then ! title line

                write(unit=*, fmt="(a6,3a3, a6, 3a8,2a7, 2a6)") &
                 "STAB ", "mm", "dd", "hh", "t2_C", "Hd", &
                 "L_nwp", "L  ", "z/L_nwp", "z/L ", "u*_nwp", "u*"
                my_first_call = .false.
        end if

        write(unit=*, &
              fmt="(a6,4i3, f6.1, 3f8.2, 2f7.2, 2f6.2)") "SUBB", iL, &
              999, & !SUBcurrent_date%month, &
              999, & !SUBcurrent_date%day, &
              999, & !SUBcurrent_date%hour, &
              Sub(iL)%t2C, Sub(iL)%Hd, Grid%invL, Sub(iL)%invL, &
              Sub(iL)%z_refd*Grid%invL, Sub(iL)%z_refd*Sub(iL)%invL, &
                 Grid%ustar, Sub(iL)%ustar
    end if



!     *** Aerodynamic resistances for each landuse type k ***
!      Ra_ref is used to estimate the aerodynamic resistance to latent
!      heat transfer from height z_ref to z0+d and from height  h+3 to 
!      z0+d, respectively.
!      Only Ra_ref and Ra_3m are used in main code.
      
        Sub(iL)%Ra_ref = AerRes(Sub(iL)%z0,Sub(iL)%z_refd,Sub(iL)%ustar,&
            Sub(iL)%invL,KARMAN)
        Zmix_refd = max(Zmix_ref-Sub(iL)%d,Sub(iL)%z_refd)
        Sub(iL)%Ra_X = AerRes(Sub(iL)%z0,Zmix_refd,Sub(iL)%ustar,&
            Sub(iL)%invL,KARMAN)
        Sub(iL)%Ra_3m  = AerRes(Sub(iL)%z0,z_3md,Sub(iL)%ustar,Sub(iL)%invL,KARMAN)
        Ra_2m  = AerRes(Sub(iL)%z0,1.0+z_1m,Sub(iL)%ustar,Sub(iL)%invL,KARMAN)

    if (  DEBUG_SUBMET .and. debug_flag ) then
       if ( Sub(iL)%Ra_ref < 0 .or. Sub(iL)%Ra_3m < 0 &
           .or. Ra_2m < 0  ) call CheckStop("RAREF NEG ")
      if ( Sub(iL)%Ra_3m > Sub(iL)%Ra_ref ) &
           call CheckStop("ERROR!!! Ra_ref<Ra_3")
    end if


!  *****  Calculate rh and vpd  *********

!  
!....The model has the specific humidity qw_ref for the lowest model layer as 
!    an input obtained from the NWP model.  We now correct this down to 
!    z_0+d metres above the ground using q(z_0+d) = q(z2) + Q*Ra_ref,
!    where Ra_ref is the same as calculated above. Q is latent heat flux 
!    (W/m^2) divided by latent heat of vap. (2.5e6  J/kg). From input data, 
!    Hl is directed downwards (!), so  we used Q = -Hl/2.5e6
!    but above ds reversed it, so we go back to the normal micromet. 
!    formula Q = Hl/2.5e6

                       
! 2m qw:
        qw = Grid%qw_ref  + Sub(iL)%LE/2.5e6 * ( Sub(iL)%Ra_ref - Ra_2m)  

      !..   qw is in kg/kg  so  e = qw*psurf/epsilon
      !..   to get e in Pascal.

       e = qw * Grid%psurf/0.622

!The equation below relies on the following assumptions: epsilon=0.622
!and L=2.5x10^6 Joules/Kg, where "epsilon" and "L" denote the ratio of 
!the gas constants for dry air against water vapour and the latent
!heat of vaporization, respectively.


     esat = ESAT0 * exp(0.622*2.5e6*((1.0/T0) - (1.0/Sub(iL)%t2))/RGAS_KG )

    if (  DEBUG_SUBMET .and. debug_flag ) then
        print "(a15,2f12.6,2f12.3)", "UKDEP SUB water", Grid%qw_ref, qw, &
              Sub(iL)%LE, 100.0*e/esat
    end if

   ! Straighforward calculation sometimes gives rh<0 or rh>1.0 -
   ! probably due to mismatches between the assumptions used for the stability
   ! profile here and in HIRLAM. Here we set crude limits on e to prevent
   ! impossible rh values at least:


     e = max(0.001*esat,e)    ! keeps rh >= 0.1%
     !e = min(esat,e)          ! keeps rh <= 1
     Sub(iL)%rh = e/esat
     Sub(iL)%rh = min(1.0,Sub(iL)%rh)! keeps rh <= 1

!  ****  leaf sat. vapour pressure

      Sub(iL)%vpd    =  0.001*(esat-e)     ! gives vpd in kPa !
      Sub(iL)%vpd    =  max(Sub(iL)%vpd,0.0) 


    if (  DEBUG_SUBMET .and. debug_flag ) then
        write(6,"(a22,2f12.4)") "UKDEP SUB7 e/esat, rh", e/esat, Sub(iL)%rh
    end if

  end subroutine Get_Submet
! =====================================================================

end module SubMet_mod
