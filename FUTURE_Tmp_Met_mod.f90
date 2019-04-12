!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
! EXPERIMENTAL NH3 EMISSIONS MODULE
! Created for testing the impacts of online NH3 emis
! Only for a part of the domain.
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
module calc_emis_potential_mod
  use Met_mod,           only: GetCDF_short
  use MetFields_mod,     only: foundu10_met,foundv10_met
  use Config_module,only: MasterProc, DEBUG_NH3,DEBUG_i, DEBUG_j,&
                              METSTEP, NMET, KMAX_MID
  use Par_mod,           only: GIMAX,GJMAX,me,IRUNBEG,JRUNBEG, LIMAX,&
                              LJMAX,MSG_READ2,MSG_READ3,MSG_READ5, &
                              limax,ljmax !hb new meteoread
  use TimeDate_mod,      only: nydays, &
                              current_date, date,Init_nmdays,nmdays, &
                              add_secs,timestamp,&
                              make_timestamp, make_current_date
  use NH3variables_mod,  only: TSPERDAY,MAXTIMESTEPS,NNH3,I_ISO_STABLE,&
                              I_OPEN_STABLE,I_STORAGE,I_WINTER_CROP, &
                              I_SPRING_CROP,I_SPRING_SBEET,I_SPRING_GRASS, &
                              I_MANURE1,I_MANURE2,I_MANURE3,I_MANURE4, &
                              I_MINERAL_SPRING,I_MINERAL_AUTUMN,I_GRAZ_CATTLE,&
                              I_NH3_GRASS,I_TRAFFIC,I_MANURE4a
  use CheckStop_mod,     only: CheckStop
  use Io_mod,            only: open_file, ios, IO_NH3 
  use GridValues_mod,    only: GRIDWIDTH_M    & !  size of grid (m)
                              ,xm2           & ! map factor squared
                              ,xm_j, xm_i    & ! hb new meteo read
                              ,i_fdom, j_fdom   ! for testing
  use PhysicalConstants_mod, only: T0
  implicit none
  private
  
  real, public,  dimension(NNH3,GIMAX,GJMAX)             :: NH3emis_pot
  real, public, save, dimension(NNH3,LIMAX,LJMAX)  :: lNH3emis_pot
  
  Real,  public, save, dimension(NNH3,GIMAX,GJMAX)       :: gEmis50_nh3 ! on actual model domain
  Real,  public, save, dimension(NNH3,LIMAX,LJMAX) :: lEmis50_nh3 ! kg/m2/s on local(processor) domain
  Real,  public, save, dimension(NNH3,LIMAX,LJMAX) :: emnh3 ! after scaling with density etc
  

  real, save, dimension(8,GIMAX,GJMAX)               :: ddagtemp
  real, public, save, dimension(8,LIMAX,LJMAX) :: lddagtemp  
  public :: readNH3emis
  public :: NH3emis_potential !Sum emissions. Only needed once
  public :: daydagt ! Determines start (day) of growth seasons

  character (len = 100)       :: field_not_found='field_not_found'
contains

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine NH3emis_potential(year) !(li,lj,T2,V10)
!Temporary read in met for preprocessing of nh3 emissions
!and make emission potential
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
real, dimension(8) :: DAYDAGTEMP    
integer, intent(in):: year !  Year ( 4-digit)
integer     :: i,j,li,lj,ndim,k,nyear,nmonth,nday,nhour , &
               ntime,nhour_first,nrec!nrec=record in meteofile
integer     :: JJ,  NDAYS,NTIMESTEPS
integer     :: KKK,TIME1
real        :: VH10HELP
character*4 :: EMISYEAR*4
real        :: GEMEMIS(NNH3), &
               C1, D1, PI,SQRTTWOPI
real        :: TSTALD, &
               VENTFACT !  SUMSTALDPOULTRY NOT USED

logical     :: debug_flag



character (len = 100) :: namefield & ! name of the requested field
                         ,validity !field is either instaneous or averaged
character (len = 100) :: meteoname !name of the meteofile
!real, dimension(132,111) :: &
!     t2_cels & !temp at 2m
!     ,u_ref     !
!real, dimension(LIMAX,LJMAX,NMET) :: &
real, dimension(GIMAX,GJMAX,NMET) :: &
! do not use NMET
!real, dimension(LIMAX,LJMAX) :: &
      t2_cels   &  ! Temp 2 m   deg. K
      ,u_10      &
      ,v_10         ! 10m wind in u and v direction  
real, dimension(0:GIMAX,GJMAX,KMAX_MID,NMET) :: u  ! m/s
real, dimension(GIMAX,0:GJMAX,KMAX_MID,NMET) :: v  ! m/s
real, dimension(GIMAX,GJMAX,KMAX_MID) :: &
      umid   & ! wind u-compnent, m/s (real, not projected)                   
      ,vmid     ! wind v-compnent, m/s     
real, dimension(MAXTIMESTEPS) :: &
      T2 &!temp at 2m
      ,V10! windspeed m/s, now midpoint, should be changed
!real, dimension(LIMAX,LJMAX,MAXTIMESTEPS) :: &
real, dimension(GIMAX,GJMAX,MAXTIMESTEPS) :: &
      T2_C & !temp at 2m
      ,T2_tmp&
      ,UREF  !wind at 10m

! hb new read meteo
integer ::   nr         ! Fields are interpolate in time (NMET = 2): between
                        ! nr=1 and nr=2
type(date)      :: next_inptime      ! hfTD,addhours_to_input
type(timestamp) :: ts_now            ! time in timestamp format
real            :: nsec              ! step in seconds

write(6,*) "EmisPot called with me, year", me, year
write(6,*)'DEBUG is Tange, i, j',DEBUG_i,DEBUG_j

! ****************************************************************************
!  Read Meteorology
! ****************************************************************************

   NDAYS = nydays
   NTIMESTEPS=NDAYS*TSPERDAY
   nr=2 !  set to one only when first meteo is read
   foundu10_met = .false. 
   foundv10_met = .false. 
if (MasterProc) then !read in metfield and make emispotensial
write(6,*) "MasterProc me==", me
   do ntime=1,NTIMESTEPS
     if(ntime == 1)then !first time meteo is read
       nr = 1
       next_inptime = current_date     
     elseif(ntime == 2)then
       nsec=METSTEP*3600.0 !from hr to sec
       ts_now = make_timestamp(current_date)
       call add_secs(ts_now,nsec)
       next_inptime=make_current_date(ts_now)
     else
       nsec=METSTEP*3600.0 !from hr to sec
       ts_now = make_timestamp(next_inptime)
       call add_secs(ts_now,nsec)
       next_inptime=make_current_date(ts_now)
     end if

     nyear=next_inptime%year
     nmonth=next_inptime%month
     nday=next_inptime%day
     nhour=next_inptime%hour

     if(DEBUG_NH3)write(6,*)'*** ntime, next_inptime',ntime, next_inptime    
     if(  current_date%month == 1 .and.         &
        current_date%day   == 1 .and.         &
        current_date%hour  == 0 )         &
        call Init_nmdays( current_date )

     if((ntime-1)*METSTEP<=nhour_first)nrec=0
     nrec=nrec+1

     if(nrec>TSPERDAY.or.nrec==1) then   ! define a new meteo input file
56     FORMAT(a5,i4.4,i2.2,i2.2,a3)
       write(meteoname,56)'meteo',nyear,nmonth,nday,'.nc'
       if(DEBUG_NH3)write(*,*)'reading ',trim(meteoname)
       nrec = 1
     end if

     if(DEBUG_NH3) write(*,*)'nrec,nhour=',nrec,nhour

     ! T2m 2D fields (surface) (i,j)
     ndim=2
     namefield='temperature_2m'
     call Get_tmpmeteofield(meteoname,namefield,nrec,ndim,&
          validity, t2_cels(:,:,nr))
        call CheckStop(validity==field_not_found, "meteo field not found:" // trim(namefield))
!      T2_C temperature in celcius
       T2_C(:,:,ntime)=t2_cels(:,:,1)-T0


     ! u10 10m wind in x direction
     ndim=2
     namefield='u10'
     call Get_tmpmeteofield(meteoname,namefield,nrec,ndim,&
          validity, u_10(:,:,nr))
     if(validity==field_not_found)then
        if(DEBUG_NH3)write(*,*)'WARNING: u10 not found '
        foundu10_met = .false.
     else
        foundu10_met = .true.
     end if 

     ndim=2
     namefield='v10'
     call Get_tmpmeteofield(meteoname,namefield,nrec,ndim,&
          validity, v_10(:,:,nr))
     if(validity==field_not_found)then
        if(DEBUG_NH3)write(*,*)'WARNING: v10 not found '
        foundv10_met = .false.
     else
        foundv10_met = .true.
     end if
 
     do j = 1,GJMAX
        do i = 1,GIMAX
          if(foundv10_met .and. foundu10_met)then  
              UREF(i,j,ntime)=&
                sqrt(( u_10(i,j,1)**2)+(v_10(i,j,1)**2))        
          end if
        end do ! i
     end do! j

   end do ! timesteps

! *************************************************************************
! Finished reading meteo
! *************************************************************************

   PI=4.*ATAN(1.)
   SQRTTWOPI=(PI*2.)**0.5

   do li=1,GIMAX
      do lj=1,GJMAX
         T2(:)=T2_C(li,lj,:)
         V10(:)=UREF(li,lj,:)
         

         !write out for Tange  
            if ( DEBUG_NH3 .and.(li==DEBUG_i) .and.(lj==DEBUG_j) )then !write out for Tange
               write(6,*) 'DEBUG TEMP AND V10 FOR TANGE IN EMISPOTENTIAL'
                       do i=1, NTIMESTEPS
                          write(6,*) i,T2(i),T2_C(li,lj,i)
                          write(6,*) i,V10(i),UREF(li,lj,i)
                       end do
         end if
         
         WRITE(EMISYEAR,'(I4)') year

         do k=1,NNH3
            NH3emis_pot(k,li,lj)=0.
         end do
  
         GEMEMIS(:)=1.
! OBS skal en her sette GEMEMIS=de faktiske utslippa?
         
         call daydagt(T2,DAYDAGTEMP) !find daydagtemp for growth season
         ddagtemp(:,li,lj)=DAYDAGTEMP(:)  
            if ( DEBUG_NH3 .and.(li==DEBUG_i) .and.(lj==DEBUG_j) )then !write out for Tange
            write(6,*) 'DEBUG DAYDAGT FOR TANGE IN EMISPOTENTIAL'
            write(6,*) (DAYDAGTEMP(KKK),KKK=1,8)
         end if
     
         TIME1=0
         DO I=1, NTIMESTEPS !nhours should be ntimestep
            TIME1=TIME1+1.
        ! test I and TIME1, isn't this the same?

            !C
            !C *******************************************
            !C * EMISSION POTENTIAL FOR ISOLATED STABLES * Gyldenkærne et al 2005
            !C *******************************************
            !C
21          CONTINUE
            IF (T2(I).LT.0) THEN
               VENTFACT=0.2
               TSTALD=MAX(0.,(18.0+0.5*(T2(I)-0.)))
            ELSE IF (T2(I).GE.0.AND.T2(I).LT.12.5) THEN
               TSTALD=18.
               VENTFACT=(0.2+(0.38-0.2)*(MAX(0.,T2(I))-0.)/(12.5-0.))
            ELSE
               VENTFACT=0.38
               TSTALD=18.+(T2(I)-12.5)*0.77
            ENDIF
            NH3emis_pot(I_ISO_STABLE,li,lj)=NH3emis_pot(I_ISO_STABLE,li,lj)+ &
                 (VENTFACT**0.26)*TSTALD**0.89 !units:relative values
            !C
            !C ***************************************
            !C * EMISSION POTENTIAL FOR OPEN STABLES *
            !C ***************************************
            !C
            IF (T2(I).LT.1.) THEN
               TSTALD=4.
            ELSE
               TSTALD=T2(I)+3.
            ENDIF
            VENTFACT=0.214+ &
                 0.014*SIN( (243.*TIME1+(REAL(TIME1)))*2.*PI/(TSPERDAY*REAL(NDAYS)) ) !24->TSPERDAY Correct?
            NH3emis_pot(I_OPEN_STABLE,li,lj)=NH3emis_pot(I_OPEN_STABLE,li,lj)+&
                 (VENTFACT**0.26)*(TSTALD**0.89)
            if ( DEBUG_NH3 .and.(li==DEBUG_i) .and.(lj==DEBUG_j) )then !write out for Tange
               write(6,*) 'DEBUG OPEN_STABLE IN EMISPOTENSIALE'
               write(6,*) 'I,TSTALD,V10,v10**0.26,TSTALD,TSTALD**0.89,NH3emis_pot(I_OPEN_STABLE,i,j)'
               write(6,*) I,TSTALD,V10(I),V10(I)**0.26,TSTALD**0.89,NH3emis_pot(I_OPEN_STABLE,li,lj)
            end if
            !C
            !C **********************************************
            !C * EMISSION POTENTIAL FOR STORAGE FASCILITIES *
            !C **********************************************
            !C
            IF (T2(I).LT.1.) THEN
               TSTALD=1.
            ELSE
               TSTALD=T2(I)
            ENDIF
            NH3emis_pot(I_STORAGE,li,lj)= NH3emis_pot(I_STORAGE,li,lj)+&
                (V10(I)**0.26)*(TSTALD**0.89)
            !C

            if ( DEBUG_NH3 .and.(li==DEBUG_i) .and.(lj==DEBUG_j) )then !write out for Tange
               write(6,*) 'DEBUG STORAGE IN EMISPOTENSIALE'
               write(6,*) 'I,TSTALD,V10,v10**0.26,TSTALD,TSTALD**0.89,NH3emis_pot(I_STORAGE,i,j)'
               write(6,*) I,TSTALD,V10(I),V10(I)**0.26,TSTALD**0.89,NH3emis_pot(I_STORAGE,li,lj)
            end if


            VH10HELP=EXP(0.0419*V10(I)) !V10=wind speed per hour?
            !C
            !C ******************************************************
            !C * EMISSION POTENTIAL FOR MANURE TYPE 1: EARLY SPRING *
            !C ******************************************************
            !C
            C1=(DAYDAGTEMP(1))*TSPERDAY!24->TSPERDAY
            D1=9.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MANURE1,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MANURE1,li,lj) !time1=timestep number
            !C
            !C *****************************************************
            !C * EMISSION POTENTIAL FOR MANURE TYPE 2: LATE SPRING *
            !C *****************************************************
            !C
            C1=(DAYDAGTEMP(3))*TSPERDAY!24->TSPERDAY
            D1=9.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MANURE2,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MANURE2,li,lj)
            !C
            !C ************************************************
            !C * EMISSION POTENTIAL FOR MANURE TYPE 3: SUMMER *
            !C ************************************************
            !C
            C1=(DAYDAGTEMP(7)) *TSPERDAY!24->TSPERDAY
            D1=16.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MANURE3,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MANURE3,li,lj)
            !C
            !C ************************************************
            !C * EMISSION POTENTIAL FOR MANURE TYPE 4: AUTUMN *
            !C ************************************************
            !C
            C1=(270.)*TSPERDAY!24->TSPERDAY
            D1=16.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MANURE4,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MANURE4,li,lj)
            !C
            !C ******************************************************
            !C * EMISSION POTENTIAL FOR MANURE TYPE 4a: EMPTY TANKS *
            !C ******************************************************
            !C
            C1=(270.)*TSPERDAY!24->TSPERDAY
            D1=9.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MANURE4a,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MANURE4a,li,lj)
            !C
            !C *****************************************************
            !C * EMISSION POTENTIAL FOR MINERAL FERTILIZER: SPRING *
            !C *****************************************************
            !C
            C1=(DAYDAGTEMP(2))*TSPERDAY!24->TSPERDAY
            D1=9.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MINERAL_SPRING,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MINERAL_SPRING,li,lj)
            !C
            !C *****************************************************
            !C * EMISSION POTENTIAL FOR MINERAL FERTILIZER: SUMMER *
            !C *****************************************************
            !C
            C1=(DAYDAGTEMP(7))*TSPERDAY!24->TSPERDAY
            D1=16.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_MINERAL_AUTUMN,li,lj)=EXP(0.0223*T2(I))*(1./(D1*SQRTTWOPI))*&
                 VH10HELP*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_MINERAL_AUTUMN,li,lj)
            !C
            !C ******************************************************************************
            !C * EMISSION POTENTIAL FOR GRAZING CATTLE: ASSUMED TO FOLLOW THE GRASS PATTERN *
            !C ******************************************************************************
            !C
            C1=DAYDAGTEMP(7)*TSPERDAY!24->TSPERDAY
            D1=60.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_GRAZ_CATTLE,li,lj)=(1./(D1*SQRTTWOPI))*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_GRAZ_CATTLE,li,lj)
            !C
            !C ***************************************
            !C * EMISSION POTENTIAL FOR WINTER CROPS *
            !C ***************************************
            !C
            C1=DAYDAGTEMP(5)*TSPERDAY!24->TSPERDAY
            D1=39.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_WINTER_CROP,li,lj)=(1./(D1*SQRTTWOPI))*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_WINTER_CROP,li,lj)
            !C
            !C ***************************************
            !C * EMISSION POTENTIAL FOR SPRING CROPS *
            !C ***************************************
            !C
            C1=DAYDAGTEMP(6)*TSPERDAY!24->TSPERDAY
            D1=25.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_SPRING_CROP,li,lj)=(1./(D1*SQRTTWOPI))*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_SPRING_CROP,li,lj)
            !C
            !C *********************************************
            !C * EMISSION POTENTIAL FOR SPRING SUGAR BEETS *
            !C *********************************************
            !C
            C1=DAYDAGTEMP(8)*TSPERDAY!24->TSPERDAY
            D1=45.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_SPRING_SBEET,li,lj)=(1./(D1*SQRTTWOPI))*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_SPRING_SBEET,li,lj)
            !C
            !C ********************************************
            !C * EMISSION POTENTIAL FOR GRASS IN ROTATION *
            !C ********************************************
            !C
            C1=DAYDAGTEMP(7)*TSPERDAY!24->TSPERDAY
            D1=60.*TSPERDAY!24->TSPERDAY
            NH3emis_pot(I_SPRING_GRASS,li,lj)=(1./(D1*SQRTTWOPI))*&
                 EXP(-0.5*((TIME1-C1)/D1)**2.)+NH3emis_pot(I_SPRING_GRASS,li,lj)
            !C
            !C                        
         ENDDO
         
         !write out debug for Tange
         if ( DEBUG_NH3 .and.(li==DEBUG_i) .and.(lj==DEBUG_j) )then 
            print *, 'DEBUG POTENTIALS FOR TANGE IN EMISPOTENTIAL'
            write(6,*) li,lj,NH3emis_pot(I_ISO_STABLE,li,lj),&
                 li,lj,NH3emis_pot(I_OPEN_STABLE,li,lj),&
                 li,lj,NH3emis_pot(I_STORAGE,li,lj),&
                 li,lj,NH3emis_pot(I_WINTER_CROP,li,lj),&
                 li,lj,NH3emis_pot(I_SPRING_CROP,li,lj),&
                 li,lj,NH3emis_pot(I_SPRING_SBEET,li,lj),&
                 li,lj,NH3emis_pot(I_SPRING_GRASS,li,lj),&
                 li,lj,NH3emis_pot(I_MANURE1,li,lj),&
                 li,lj,NH3emis_pot(I_MANURE2,li,lj),&
                 li,lj,NH3emis_pot(I_MANURE3,li,lj),&
                 li,lj,NH3emis_pot(I_MANURE4,li,lj),&
                 li,lj,NH3emis_pot(I_MANURE4a,li,lj),&
                 li,lj,NH3emis_pot(I_MINERAL_SPRING,li,lj),&
                 li,lj,NH3emis_pot(I_MINERAL_AUTUMN,li,lj),&
                 li,lj,NH3emis_pot(I_GRAZ_CATTLE,li,lj)!,&           
         end if
         
         !C
         !C
         !C ******************************
         !C * END OF EMISSION POTENTIALS *
         !C ******************************
         !C
         !C
         !
      end do !lj
   end do !li
end if !MasterProc

call global2local(NH3emis_pot,lNH3emis_pot,MSG_READ2,NNH3,GIMAX,GJMAX,1,1,1)
call global2local(ddagtemp,lddagtemp,MSG_READ5,8,GIMAX,GJMAX,1,1,1)

end  subroutine NH3emis_potential
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!******************************************************************
subroutine daydagt(T2,DAYDAGTEMP)
  !
  ! Determines start (day) of growth seasons
  !******************************************************************
  use NH3variables_mod, only   : TSPERDAY
  use TimeDate_mod, only       : current_date,day_of_year
  real, INTENT(IN) :: T2(:)
  real, dimension(8), INTENT(OUT) :: DAYDAGTEMP
  
  integer :: ISTARTYEAR
  integer :: DAYFEB,NDAYS,JJ,I,KKK	
  integer :: IDAYTAELLER
  real    :: DAGTEMP,T2_DAYMEAN
  real    :: T2MEAN(366)
  
  ISTARTYEAR=current_date%year          
  !Find T2_DAYMEAN, needed to determine growth season
  !CHECK start of day(termin at start) 

  NDAYS    = nydays !day_of_year(ISTARTYEAR,12,31)!find number of days in year
  JJ=0 ! JJ=timestep
  DO I=1,NDAYS
     T2_DAYMEAN=0.
     DO KKK=1,TSPERDAY
        JJ=JJ+1
        T2_DAYMEAN=T2_DAYMEAN+T2(JJ)/TSPERDAY
     ENDDO
     T2MEAN(I)=T2_DAYMEAN
  ENDDO
  
  DAYFEB=day_of_year(ISTARTYEAR,3,1)-1!find daynumber end of february
  IDAYTAELLER=DAYFEB !num days in end of february
  DAGTEMP=0.
  DAYDAGTEMP(:)=0.
  DO I=DAYFEB,NDAYS !from march to end of year
     DAGTEMP=DAGTEMP+T2MEAN(I) !add daytemeperature
     IDAYTAELLER=IDAYTAELLER+1
     IF (DAGTEMP.GT.090.AND.DAYDAGTEMP(1).EQ.0) THEN
        DAYDAGTEMP(1)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.200.AND.DAYDAGTEMP(2).EQ.0) THEN
        DAYDAGTEMP(2)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.260.AND.DAYDAGTEMP(3).EQ.0) THEN
        DAYDAGTEMP(3)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.1700.AND.DAYDAGTEMP(4).EQ.0) THEN
        DAYDAGTEMP(4)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.710.AND.DAYDAGTEMP(5).EQ.0) THEN
        DAYDAGTEMP(5)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.950.AND.DAYDAGTEMP(6).EQ.0) THEN
        DAYDAGTEMP(6)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.1400.AND.DAYDAGTEMP(7).EQ.0) THEN
        DAYDAGTEMP(7)=IDAYTAELLER
     ENDIF
     IF (DAGTEMP.GT.2040.AND.DAYDAGTEMP(8).EQ.0) THEN
        DAYDAGTEMP(8)=IDAYTAELLER
     ENDIF
  ENDDO
 
end subroutine daydagt
!******************************************************************************


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine readNH3emis !read northwestern 16.67km emissions and convert to 50km
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
  use EmisDef_mod, only : dknh3_agr

  integer :: i,j,i50,j50,k, IGRIDNR, ii,ll
  
  real    :: tonne_to_kgm2s    ! Converts tonnes/grid to kg/m2/s
  real, dimension(17):: RHELP  ! reading emis
  logical, parameter :: READNH3=.true.
  integer, parameter :: NX16=396, NY16=333, NX50=132,NY50=111! 16.67km EMEP domain
  
  
  Real, dimension(NNH3,NX50,NY50)   ::  Emis50_nh3 !in tonnes N/year, 50km EMEP domain
  Real, dimension(NNH3,NX16,NY16)   ::  tmp_nh3 !in tonnes N/year, 16.67km EMEP domain
  Real, dimension(NNH3,96*96)   ::  test_nh3 !in tonnes N/year, 16.67km EMEP domain
  Real :: fracemis !fraction between reported and Skjoeth agricultural NH3 emissions
  
  if(READNH3)then
     
     if (MasterProc)then
        !Read activity data for NH3 (tonnes N) for NEUROPE, from i=44, j=35 to i=76,j=67
        test_nh3(:,:)=0.0 
        tmp_nh3(:,:,:)=0.0
        Emis50_nh3(:,:,:)=0.0
        gEmis50_nh3(:,:,:)=0.0
        call open_file(IO_NH3,"r","Sector_NH3Emis.txt",needed=.true.,skip=0)
        call CheckStop(ios,"NH3: ios error Sector_NH3Emis.txt")
           ! read heading
           read(IO_NH3,*)
           do ll=1,9216
           read(IO_NH3,*,err=1002,end=1002) IGRIDNR,(RHELP(I),I=1,17)
              test_nh3(1,IGRIDNR)=RHELP(1) !isostald  - Isolated stables (pigs)                       
              test_nh3(2,IGRIDNR)=RHELP(2) !uisostalf - Unisolated stables (cattle)                 
              test_nh3(3,IGRIDNR)=RHELP(3) !lager     - Storage     
              test_nh3(4,IGRIDNR)=RHELP(4) !crop1  - Winter crops   
              test_nh3(5,IGRIDNR)=RHELP(5) !crop2  - Spring crops   
              test_nh3(6,IGRIDNR)=RHELP(6) !crop3  - Sugar beets    
              test_nh3(7,IGRIDNR)=RHELP(7) !crop4  - Grass in rotation         
              test_nh3(8,IGRIDNR)=RHELP(8) !gylle1 - Manure 1st application at field  early spring        
              test_nh3(9,IGRIDNR)=RHELP(9) !gylle2 - Manure 2nd appl.                 late spring              
              test_nh3(10,IGRIDNR)=RHELP(10)!gylle3 - Manure 3rd appl.                summer     
              test_nh3(11,IGRIDNR)=RHELP(11)!gylle4 - Manure 4rd appl.                autumn       
              test_nh3(12,IGRIDNR)=RHELP(12)!Manure type 4a, empty tanks 
              test_nh3(13,IGRIDNR)=RHELP(13)+RHELP(14)!fertilizer   - Fertlizer, both spring and summer appl.  
              test_nh3(14,IGRIDNR)=RHELP(15)!afraes - Grassing cattle etc.                    
              test_nh3(15,IGRIDNR)=RHELP(16)!nh3_halm - Evaporation of NH3 from straw 
              test_nh3(16,IGRIDNR)=RHELP(17)!traffic 
           end do

1002    close(IO_NH3)

        ii=0

! hard coded:
           do j=103,198
            do i=130,225
              ii=ii+1
              tmp_nh3(1,i,j)=test_nh3(1,ii) !isostald  - Isolated stables (pigs)                       
              tmp_nh3(2,i,j)=test_nh3(2,ii) !uisostalf - Unisolated stables (cattle)                 
              tmp_nh3(3,i,j)=test_nh3(3,ii) !lager     - Storage     
              tmp_nh3(4,i,j)=test_nh3(4,ii) !crop1  - Winter crops   
              tmp_nh3(5,i,j)=test_nh3(5,ii) !crop2  - Spring crops   
              tmp_nh3(6,i,j)=test_nh3(6,ii) !crop3  - Sugar beets    
              tmp_nh3(7,i,j)=test_nh3(7,ii) !crop4  - Grass in rotation         
              tmp_nh3(8,i,j)=test_nh3(8,ii) !gylle1 - Manure 1st application at field  early spring        
              tmp_nh3(9,i,j)=test_nh3(9,ii) !gylle2 - Manure 2nd appl.                 late spring              
              tmp_nh3(10,i,j)=test_nh3(10,ii)!gylle3 - Manure 3rd appl.                summer     
              tmp_nh3(11,i,j)=test_nh3(11,ii)!gylle4 - Manure 4rd appl.                autumn       
              tmp_nh3(12,i,j)=test_nh3(12,ii)!Manure type 4a, empty tanks 
              tmp_nh3(13,i,j)=test_nh3(13,ii)!fertilizer   - Fertlizer, both spring and summer appl.  
              tmp_nh3(14,i,j)=test_nh3(14,ii)!afraes - Grassing cattle etc.                    
              tmp_nh3(15,i,j)=test_nh3(15,ii)!nh3_halm - Evaporation of NH3 from straw 
              tmp_nh3(16,i,j)=test_nh3(16,ii)!traffic 
           end do !j
        end do !i

        do i=1,NX50
           do j=1,NY50
              do k=1,NNH3
                 gEmis50_nh3(k,i,j)=tmp_nh3(k,3*i,3*j)+&
                      tmp_nh3(k,3*i-1,3*j-1)+&
                      tmp_nh3(k,3*i,3*j-1)+&
                      tmp_nh3(k,3*i-1,3*j)+&
                      tmp_nh3(k,3*i-2,3*j)+&
                      tmp_nh3(k,3*i,3*j-2)+&
                      tmp_nh3(k,3*i-1,3*j-2)+&
                      tmp_nh3(k,3*i-2,3*j-1)+&
                      tmp_nh3(k,3*i-2,3*j-2)              
              end do !k
           end do !j
        end do !i
!        write(6,*) 'Sum nh3 emissions after interpol= ',sum(gEmis50_nh3(:,:,:)),sum(tmp_nh3(:,:,:))
        do j=1,GJMAX
           do i=1,GIMAX
              gEmis50_nh3(I_MINERAL_SPRING,i,j)=0.9*gEmis50_nh3(I_MINERAL_SPRING,i,j)
              gEmis50_nh3(I_MINERAL_AUTUMN,i,j)=0.1*gEmis50_nh3(I_MINERAL_SPRING,i,j)
           end do
        end do


        fracemis=dknh3_agr*14.0/((sum(gEmis50_nh3(:,:,:)))*17.0) !to ensure emission the same as reported
        write(6,*)'NMR NH3 emis reported and from Skjoeth ',dknh3_agr,sum(gEmis50_nh3(:,:,:)),fracemis
        gEmis50_nh3(:,:,:)=gEmis50_nh3(:,:,:)*fracemis

        write(6,*)'NMR NH3 after fracemis',sum(gEmis50_nh3(:,:,:))
     end if ! MasterProc

     if ( DEBUG_NH3 .and. MasterProc) then
        write(6,*)'NMR NH3 after if READNH3',sum(gEmis50_nh3(:,:,:))
     end if
     
     !    Conversions --
     !
     !     The emission-data file are so far in units of 
     !     tonnes per grid-square. The conversion factor from tonnes per 50*50km2
     !     annual emission values to surface flux (kg/m2/s) is found by division
     !     with (nydays*24*60*60)s and (h*h)m2 and multiply by 1.e+3.
     !     the conversion factor then equals 1.27e-14
     !
!     if ( DEBUG_NH3) print *,  "NH3 CONV:me, nydays, gridwidth = ",me,nydays,GRIDWIDTH_M
     
     tonne_to_kgm2s  = 1.0e3 / (nydays * 24.0 * 3600.0 * GRIDWIDTH_M * GRIDWIDTH_M)
     
     if ( DEBUG_NH3 .and. MasterProc ) then
        write(unit=6,fmt=*) "No. days in Emissions: ", nydays
        write(unit=6,fmt=*) "tonne_to_kgm2s in Emissions: ", tonne_to_kgm2s
        write(unit=6,fmt=*) "Emissions sums:",sum(gEmis50_nh3(:,:,:))
     end if
     
     
!     write(6,*) 'NMR NH3 emis on this domain ktonne ',0.001*sum(gEmis50_nh3(:,:,:))
!     write(6,*) 'NMR NH3 emis on emep domain ktonne',0.001*sum(Emis50_nh3(:,:,:))
     
     do k=1,NNH3
        !       gEmis50_nh3(k,:,:)=gEmis50_nh3(k,:,:)* tonne_to_kgm2s * xm2(:,:) !do xm2 in NH3Emisvariation_mod
        gEmis50_nh3(k,:,:)=gEmis50_nh3(k,:,:)* tonne_to_kgm2s    !already N(not NH3) in input
     end do !k      
   
     !gemis on global
  end if !READNH3 
  lEmis50_nh3(:,:,:)=0.0 !initialize
  call global2local(gEmis50_nh3,lEmis50_nh3,MSG_READ3,NNH3,GIMAX,GJMAX,1,1,1) 
  
end subroutine readNH3emis
!----------------------------------------------------------------------------------------------
subroutine Get_tmpmeteofield(meteoname,namefield,nrec,&
       ndim,validity,field)
    !
    ! Read the meteofields without distibute to nodes

    implicit none

    real, dimension(*),intent(out)  :: field ! dimensions: (LIMAX,LJMAX)
    character (len = *),intent(in)  ::meteoname,namefield
    character (len = *),intent(out) ::validity
    integer,intent(in)              :: nrec,ndim
    integer*2, allocatable ::var_global(:,:,:)   ! faster if defined with
    ! fixed dimensions for all
    real :: scalefactors(2)
    integer :: KMAX,ijk,i,k,j,nfetch

    validity=''

    if(ndim==3)KMAX=KMAX_MID
    if(ndim==2)KMAX=1
       allocate(var_global(GIMAX,GJMAX,KMAX))
       nfetch=1
       call GetCDF_short(namefield,meteoname,var_global,GIMAX,IRUNBEG,GJMAX, &
            JRUNBEG,KMAX,nrec,nfetch,scalefactors,validity)


    ijk=0
    do k=1,KMAX ! KMAX is =1 for 2D arrays
       do j=1,GJMAX
          do i=1,GIMAX
             ijk=ijk+1
             field(ijk)=var_global(i,j,k)*scalefactors(1)+scalefactors(2)
          end do
       end do
    end do

    deallocate(var_global)

end subroutine Get_tmpmeteofield


end module calc_emis_potential_mod
