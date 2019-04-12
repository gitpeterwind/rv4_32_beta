module NH3variables_mod

 implicit none
 integer, public, parameter :: &
    NNH3=17                    &  ! number of NH3 activity classes
   ,TSPERDAY=8                 &  ! timesteps per day in meteorology
   ,MAXTIMESTEPS=366*TSPERDAY     ! for dimensions. The exact number of
                                  ! timesteps is calc. in subroutines depending
                                  ! on 365 or 366 days
!integer, parameter, public  :: &
!     IO_NH3EMIS      = 120   !(c) Sector NH3 emis file from Gyldenkaerne 
                              ! To be set in Io_mod

  integer, parameter :: &
    I_ISO_STABLE=1 &
   ,I_OPEN_STABLE=2 &
   ,I_STORAGE=3 &
   ,I_WINTER_CROP=4 &
   ,I_SPRING_CROP=5 &
   ,I_SPRING_SBEET=6 &
   ,I_SPRING_GRASS=7 &
   ,I_MANURE1=8 &
   ,I_MANURE2=9 &
   ,I_MANURE3=10 &
   ,I_MANURE4=11 &
   ,I_MANURE4a=12 &  !??
   ,I_MINERAL_SPRING=13 &
   ,I_MINERAL_AUTUMN=14 &
   ,I_GRAZ_CATTLE=15 &
   ,I_NH3_GRASS=16 &
   ,I_TRAFFIC=17

end module NH3variables_mod
