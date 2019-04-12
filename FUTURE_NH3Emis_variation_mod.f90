module NH3Emis_variation_mod
use NH3variables_mod,        only :I_ISO_STABLE,I_OPEN_STABLE,I_STORAGE &
                            ,I_WINTER_CROP,I_SPRING_CROP,I_SPRING_SBEET &
                            ,I_SPRING_GRASS,I_MANURE1,I_MANURE2,I_MANURE3 &
                            ,I_MANURE4,I_MINERAL_SPRING,I_MINERAL_AUTUMN &
                            ,I_GRAZ_CATTLE,I_NH3_GRASS,I_TRAFFIC &
                            ,I_MANURE4a,NNH3,TSPERDAY
use calc_emis_potential_mod, only : lddagtemp,lNH3emis_pot, lEmis50_nh3, emnh3
use TimeDate_mod,            only : nydays, current_date
use MetFields_mod,           only : t2_nwp,u_ref, &
                                   foundv10_met,ps, roa,foundws10_met,ws_10m
use Par_mod,                 only : LIMAX,LJMAX,me,limax,ljmax
use GridValues_mod ,         only : i_fdom, j_fdom, sigma_bnd, xm2  
use Config_module,      only : DEBUG_i, DEBUG_j, PT, DEBUG_NH3, &
                                   KMAX_MID, MasterProc
use Io_mod,                  only : IO_NH3_DEB 
use TimeDate_mod,            only : day_of_year, nmdays
use PhysicalConstants_mod,   only : T0
use EmisDef_mod,             only : NH3EMIS_VAR
use PhysicalConstants_mod,   only : GRAV,  AVOG

implicit none
private

real, public, save, dimension(NNH3,LIMAX,LJMAX) :: tnh3_fac !timefactors.
                   !Need to be reset every 3 hour (metstep)
                   !(or really:the same timestep as used in NH3emis_potential)
public :: NH3emis_variation
public :: SetNH3

contains
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine NH3emis_variation() !only one grid cell (and later one timestep at a time, now loop)
      integer :: NHOUR,TIME1 !timesteps nr since start of year 
      integer :: NDAYS,I,NTIMESTEPS,KKK ,NDAY
      real    :: T2,V10
      real    :: VH10HELP
      real    :: VENTFACT,TSTALD
      real    :: GEMEMIS
      real    :: C1, D1, PI,SQRTTWOPI
      integer :: year,month,day, hour,j,lu,nlu,ilu
      integer, parameter :: TSPERDAY=8
      logical :: debug_flag
      real, parameter :: Z10 = 10.0  ! 10m height
      tnh3_fac(:,:,:)=0.0 !initialize
      
      GEMEMIS=1.
      NDAYS    = nydays !day_of_year(ISTARTYEAR,12,31)!find number of days in year
      NDAY =day_of_year(current_date%year,current_date%month,current_date%day) !day in year
      NHOUR= (NDAY-1)*24 + current_date%hour 
      TIME1=NHOUR/(24/TSPERDAY) +1
      if (MasterProc)then
         write(6,*)'Date and nhour and ntstep in NH3Emis_variation',current_date%year,current_date%month,current_date%day,current_date%hour,NHOUR,TIME1
      end if

      NTIMESTEPS=NDAYS*TSPERDAY 

      PI=4.*ATAN(1.)
      SQRTTWOPI=(PI*2.)**0.5


    do j = 1,ljmax
        do i = 1,limax


     ! - Set up debugging coordinates first. ---------------------------!
     ! If location matches debug i,j value, set debug_flag. 

      debug_flag = .false. 
      if ( i_fdom(i)==DEBUG_i .and. j_fdom(j)==DEBUG_j)then
         debug_flag = .true.
         write(6,*)'Found coords for Tange, prog, i,j, ',me,i,j
      end if
           T2=t2_nwp(i,j,1)-T0 ! T2 in C not in K degrees
     
           if(foundws10_met)then
              V10=ws_10m(i,j,1) 
!           else
!              V10=u_ref(i,j) !~45m wind??
           end if
           if ( DEBUG_NH3 .and. debug_flag )then !write out for Tange
            write(6,*) 'DEBUG 2m nwp temp for Tange NDAY, NHOUR',T2,me,i,j, NDAY,NHOUR
            write(6,*) 'DEBUG 10m wind V10 foundu10_met foundv10_met ',foundws10_met,V10,me,i,j
           end if



!C
!C *******************************************
!C * EMISSION VARIATION FOR ISOLATED STABLES *
!C *******************************************
!C
      IF (T2.LT.0) THEN
      VENTFACT=0.2
      TSTALD=MAX(0.,(18.0+0.5*(T2-0.)))
      ELSE IF (T2.GE.0.AND.T2.LT.12.5) THEN
      TSTALD=18.
      VENTFACT=(0.2+(0.38-0.2)*(MAX(0.,T2)-0.)/(12.5-0.))
      ELSE
      TSTALD=18.+(T2-12.5)*0.77
      VENTFACT=0.38 !SAME AS ABOVE
      ENDIF
      tnh3_fac(I_ISO_STABLE,i,j)=&
          (VENTFACT**0.26)*(TSTALD**0.89)*&
          (1.0*GEMEMIS/(lNH3emis_pot(I_ISO_STABLE,i,j)))


!C ***************************************
!C * EMISSION VARIATION FOR OPEN STABLES *
!C ***************************************
!C
      IF (T2.LT.1.) THEN
      TSTALD=4.
      ELSE
      TSTALD=(T2+3.)
      ENDIF
      VENTFACT=0.214+&
      0.014*SIN((243.*TIME1+(REAL(TIME1)))*2.*PI/(TSPERDAY*REAL(NDAYS))) ! 24 -> TSPERDAY !I->TIME1
       tnh3_fac(I_OPEN_STABLE,i,j)=&
           (VENTFACT**0.26)*(TSTALD**0.89)*&
          (GEMEMIS/(lNH3emis_pot(I_OPEN_STABLE,i,j)))

!C
!C **********************************************
!C * EMISSION POTENTIAL FOR STORAGE FASCIITIES *
!C **********************************************
!C
      IF (T2.LT.1.) THEN
      TSTALD=1.
      ELSE
      TSTALD=T2
      ENDIF
      tnh3_fac(I_STORAGE,i,j)=(V10**0.26)*(TSTALD**0.89)*&
          (GEMEMIS/(lNH3emis_pot(I_STORAGE,i,j)))


      VH10HELP=EXP(0.0419*V10)

!C
!C ******************************************************
!C * EMISSION VARIATION FOR MANURE TYPE 1: EARLY SPRING *
!C ******************************************************
!C
      C1=(lddagtemp(1,i,j))*TSPERDAY ! 24 -> TSPERDAY
      D1=9.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MANURE1,i,j)=EXP(0.0223*T2)*&
           VH10HELP*&
          (1./(D1*SQRTTWOPI))*(GEMEMIS/(lNH3emis_pot(I_MANURE1,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C *****************************************************
!C * EMISSION VARIATION FOR MANURE TYPE 2: LATE SPRING *
!C *****************************************************
!C
      C1=(lddagtemp(3,i,j))*TSPERDAY ! 24 -> TSPERDAY
      D1=9.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MANURE2,i,j)=EXP(0.0223*T2)*&
           VH10HELP*&
          (1./(D1*SQRTTWOPI))*(GEMEMIS/(lNH3emis_pot(I_MANURE2,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C ************************************************
!C * EMISSION VARIATION FOR MANURE TYPE 3: SUMMER *
!C ************************************************
!C
      C1=(lddagtemp(7,i,j))*TSPERDAY ! 24 -> TSPERDAY
      D1=16.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MANURE3,i,j)=EXP(0.0223*T2)*&
           VH10HELP*&
          (1./(D1*SQRTTWOPI))*(GEMEMIS/(lNH3emis_pot(I_MANURE3,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C ************************************************
!C * EMISSION VARIATION FOR MANURE TYPE 4: AUTUMN *
!C ************************************************
!C
      C1=270.*TSPERDAY ! 24 -> TSPERDAY
      D1=16.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MANURE4,i,j)=EXP(0.0223*T2)*&
           VH10HELP*&
          (1./(D1*SQRTTWOPI))*(GEMEMIS/(lNH3emis_pot(I_MANURE4,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C *****************************************************
!C * EMISSION VARIATION FOR MANURE TYPE 4a: EMPTY TANKS *
!C *****************************************************
!C
      C1=270.*TSPERDAY ! 24 -> TSPERDAY
      D1=09.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MANURE4a,i,j)=EXP(0.0223*T2)*VH10HELP*&
          (1./(D1*SQRTTWOPI))*(GEMEMIS/(lNH3emis_pot(I_MANURE4a,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
 
!C
!C ****************************************************************************** 
!C * EMISSION VARIATION FOR GRAZING CATTLE: ASSUMED TO FOLLOW THE GRASS PATTERN *
!C ******************************************************************************
!C
      C1=lddagtemp(7,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=60.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_GRAZ_CATTLE,i,j)=(1./(D1*SQRTTWOPI))*&
           (GEMEMIS/(lNH3emis_pot(I_GRAZ_CATTLE,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C ***************************************
!C * EMISSION VARIATION FOR WINTER CROPS *
!C ***************************************
!C
      C1=lddagtemp(5,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=39.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_WINTER_CROP,i,j)=(1./(D1*SQRTTWOPI))*&
           (GEMEMIS/(lNH3emis_pot(I_WINTER_CROP,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)


!C
!C ***************************************
!C * EMISSION VARIATION FOR SPRING CROPS *
!C ***************************************
!C
      C1=lddagtemp(6,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=25.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_SPRING_CROP,i,j)=(1./(D1*SQRTTWOPI))*&
           (GEMEMIS/(lNH3emis_pot(I_SPRING_CROP,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C *********************************************
!C * EMISSION VARIATION FOR SPRING SUGAR BEETS *
!C *********************************************
!C
      C1=lddagtemp(8,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=45.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_SPRING_SBEET,i,j)=(1./(D1*SQRTTWOPI))*&
           (GEMEMIS/(lNH3emis_pot(I_SPRING_SBEET,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C ********************************************
!C * EMISSION VARIATION FOR GRASS IN ROTATION *
!C ********************************************
!C
      C1=lddagtemp(7,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=60.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_SPRING_GRASS,i,j)=(1./(D1*SQRTTWOPI))*&
           (GEMEMIS/(lNH3emis_pot(I_SPRING_GRASS,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C *****************************************************
!C * EMISSION VARIATION FOR MINERAL FERTIIZER: SPRING *

!C
      C1=lddagtemp(2,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=9.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MINERAL_SPRING,i,j)=EXP(0.0223*T2)*VH10HELP*&
          (1./(D1*SQRTTWOPI))*(0.9*GEMEMIS/(lNH3emis_pot(I_MINERAL_SPRING,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C *****************************************************
!C * EMISSION VARIATION FOR MINERAL FERTIIZER: SUMMER *
!C *****************************************************
!C
      C1=lddagtemp(7,i,j)*TSPERDAY ! 24 -> TSPERDAY
      D1=16.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_MINERAL_AUTUMN,i,j)=EXP(0.0223*T2)*VH10HELP*&
          (1./(D1*SQRTTWOPI))*(0.1*GEMEMIS/(lNH3emis_pot(I_MINERAL_AUTUMN,i,j)))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C ************************************************
!C * AMMONIA LOSSES FROM NH3 THREATMEANT OF GRASS *
!C ************************************************
!C
      C1=(lddagtemp(4,i,j)+15)*TSPERDAY ! 24 -> TSPERDAY
      D1=9.*TSPERDAY ! 24 -> TSPERDAY
      tnh3_fac(I_NH3_GRASS,i,j)=GEMEMIS*(1./(D1*SQRTTWOPI))*&
           EXP(-0.5*((TIME1-C1)/D1)**2.)
!C
!C ************************
!C * AMMONIA FROM TRAFFIC *
!C ************************
!C
      tnh3_fac(I_TRAFFIC,i,j)=GEMEMIS/(TSPERDAY *REAL(NDAYS)) ! 24 -> TSPERDAY
!C
!C
      !write out for Tange
      if ( DEBUG_NH3 .and.  i_fdom(i)==DEBUG_i .and. j_fdom(j)==DEBUG_j )then 
! output to file
!         write(IO_NH3_DEB,'(3i3,i5,18F20.10)')current_date%month,&
!              current_date%day,current_date%hour,TIME1,&
!              tnh3_fac(I_ISO_STABLE,i,j),tnh3_fac(I_OPEN_STABLE,i,j),&
!              tnh3_fac(I_STORAGE,i,j),tnh3_fac(I_WINTER_CROP,i,j),&
!              tnh3_fac(I_SPRING_CROP,i,j), tnh3_fac(I_SPRING_SBEET,i,j),&
!              tnh3_fac(I_SPRING_GRASS,i,j), tnh3_fac(I_MANURE1,i,j), &
!              tnh3_fac(I_MANURE2,i,j),tnh3_fac(I_MANURE3,i,j),&
!              tnh3_fac(I_MANURE4,i,j),tnh3_fac(I_MANURE4a,i,j),&
!              tnh3_fac(I_MINERAL_SPRING,i,j),tnh3_fac(I_MINERAL_AUTUMN,i,j),&
!              tnh3_fac(I_GRAZ_CATTLE,i,j),tnh3_fac(I_NH3_GRASS,i,j), &
!              tnh3_fac(I_TRAFFIC,i,j),sum(tnh3_fac(:,i,j))

         print *, '-------------DEBUG VARIATIONS-------------------------'
         write(6,'(3i3,3i5,F6.0,17F20.10)')current_date%month,current_date%day,current_date%hour,TIME1,&
              I,J,T2,tnh3_fac(I_ISO_STABLE,i,j),&
              tnh3_fac(I_OPEN_STABLE,i,j),tnh3_fac(I_STORAGE,i,j),tnh3_fac(I_WINTER_CROP,i,j),&
              tnh3_fac(I_SPRING_CROP,i,j), tnh3_fac(I_SPRING_SBEET,i,j), tnh3_fac(I_SPRING_GRASS,i,j), &
              tnh3_fac(I_MANURE1,i,j), tnh3_fac(I_MANURE2,i,j),&
              tnh3_fac(I_MANURE3,i,j),tnh3_fac(I_MANURE4,i,j),tnh3_fac(I_MANURE4a,i,j),tnh3_fac(I_MINERAL_SPRING,i,j),&
              tnh3_fac(I_MINERAL_AUTUMN,i,j),tnh3_fac(I_GRAZ_CATTLE,i,j),&
              tnh3_fac(I_NH3_GRASS,i,j), tnh3_fac(I_TRAFFIC,i,j)
      end if 

  end do !i
end do !j

end subroutine NH3emis_variation
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!hf NH3emis
!------------------------------------------------------------------------------
subroutine SetNH3()
!------------------------------------------------------------------------------


  integer :: i,j,k
  real    :: ehlpcom,ehlpcom0
  real    :: NTPERYEAR

  if (NH3EMIS_VAR) then

     call NH3emis_variation() !set timefactors for this timestep
     
     ehlpcom0 = GRAV* 0.001*AVOG/ (sigma_bnd(KMAX_MID+1) - sigma_bnd(KMAX_MID))
     emnh3(:,:,:)=0.0
  
     do j = 1,ljmax
        do i = 1,limax
      
           ehlpcom = ehlpcom0 * roa(i,j,KMAX_MID,1)/(ps(i,j,1)-PT)
           !tnh3_fac(k,i,j) is the fraction of emissions per 3 hour(1/3h)
           !Already in /s, thus we multiply tnh3_fac up to a year
           NTPERYEAR=nydays*TSPERDAY

           do k=1,NNH3
              emnh3(k,i,j) = lEmis50_nh3(k,i,j)*ehlpcom *tnh3_fac(k,i,j) &
              *NTPERYEAR *xm2(i,j)/14.0 !from kg(N)/m2/s to  molecules/cm3/s
              
              if ( DEBUG_NH3 .and.  i_fdom(i)==DEBUG_i .and. j_fdom(j)==DEBUG_j)then !write out for Tange
                 write(6,*)'DEBUG for Tange' 
                 write(6,*)'k and emnh3', k, emnh3(k,i,j)
                 write(6,*)'proc,lEmis50_nh3',me, lEmis50_nh3(k,i,j)
                 write(6,*)'tnh3_fac(k,i,j)', tnh3_fac(k,i,j)*NTPERYEAR 
                 write(6,*)'NH3Emisvariation ',emnh3(k,i,j)
                 write(6,*)'NTPERYEAR, nydays, TSPERDAY',NTPERYEAR,nydays,TSPERDAY
              end if 
           end do

        end do ! i
     end do ! j
     
  end if ! NH3EMIS_VAR
end subroutine SetNH3

end module NH3Emis_variation_mod

