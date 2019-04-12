!==============================================================================
module My_Derived_mod
!---------------------------------------------------------------------------
! DESCRIPTION
! This module specifies the "derived" fields, such as accumulated
! precipitation
! or sulphate, daily, monthly or yearly averages, depositions. These fields
! are all typically output as netCDF fields.
!
! This module provides the user-defined setups which are used in Derived_mod.
! Derived fields are identified by a "class", such as "ADV" of "VOC", and
! the Derived_mod should perform any integrations for this.
!
! Several often-used routines (e.g. for AOTs, acc. sulphate, are defined
! in the Derived_mod.f90, but users can define their own here, since
! we do not use "use only" in Derived_mod.
!
!   Only text strings used here to define wanted data
!   All data field characteristics should be defined in Derived_mod, e.g.
!   in f_2d arrays.
!   Derived fields such as d_2d only exist in Derived_mod, so are
!   accessed here through subroutine calls - using just the (i,j) part
!   of the bigger d_2d arrays
!
! NOTE - this routine will likely be deleted in future, as we are
!  moving most definitions to the config namelist system.
!---------------------------------------------------------------------------

use AOTx_mod,          only: VEGO3_OUTPUTS, nOutputVegO3,OutputVegO3
use CheckStop_mod,     only: CheckStop
use Chemfields_mod,    only: xn_adv, xn_shl, cfac
use ChemDims_mod          ! Use IXADV_ indices...
use ChemGroups_mod        ! Allow all groups to ease compilation
                          !  eg. OXN_GROUP, DDEP_OXNGROUP, BVOC_GROUP
use ChemSpecs_mod         ! Use IXADV_ indices...
use Config_module,     only: MasterProc, SOURCE_RECEPTOR, & !
                            USES, USE_SOILNOX, USE_OCEAN_DMS, USE_OCEAN_NH3, &
                            IOU_KEY,      & !'Y'=>IOU_YEAR,..,'I'=>IOU_HOUR_INST
                            KMAX_MID,     & ! =>  z dimension
                            RUNDOMAIN,    &
                            fullrun_DOMAIN,month_DOMAIN,day_DOMAIN,hour_DOMAIN,&
                            startdate, out_startdate, spinup_enddate,&
                            num_lev3d,lev3d &! 3D levels on 3D output
                            , SecEmisOutWanted,EmisSplit_OUT
use Debug_module,      only: DEBUG ! => DEBUG_MY_DERIVED
use EmisDef_mod,       only: NSECTORS, EMIS_FILE, Nneighbors
use EmisGet_mod,       only: nrcemis, iqrc2itot
use GridValues_mod,    only: RestrictDomain
use Io_Nums_mod,       only: IO_NML
use Io_Progs_mod,      only: PrintLog
use MosaicOutputs_mod, only: nMosaic, MAX_MOSAIC_OUTPUTS, MosaicOutput, & !
                            Init_MosaicMMC,  Add_MosaicMetConcs, &
                            Add_NewMosaics, Add_MosaicVEGO3, Add_MosaicDDEP

use OwnDataTypes_mod,only: Deriv, TXTLEN_DERIV, TXTLEN_SHORT,&
                          typ_s3, typ_s4, typ_s5ind, typ_s1ind
use Par_mod,         only: limax,ljmax        ! => used x, y area
use SmallUtils_mod,  only: AddArray,LenArray,NOT_SET_STRING,WriteArray,find_index
implicit none
private

public  :: Init_My_Deriv
public  :: My_DerivFunc ! Miscelleaneous functions of xn_adv for output
                       ! (not currently used)

!    Depositions are stored in separate arrays for now - to keep size of
!    derived arrays smaller and to allow possible move to a Deposition
!    module at a later stage.
!    Factor 1.0e6 converts from kg/m2/a to mg/m2/a

!        We normally distinguish source-receptor (SR) stuff from model
!        evaluation.  The SR runs should use as few as possible outputs
!        to keep CPU and disc-requirements down. We define first then the
!        minimum list of outputs for use in SR, then define an extra list
!        of parameters needed in model evaluation, or even for the base-case
!        of SR runs.

!============ parameters for concentration + dep outputs ==================!
integer, public, parameter ::       &
  MAX_NUM_DERIV2D = 343,            &
  MAX_NUM_DERIV3D = 179,            &
  MAX_NUM_DDEP_ECOS = 9,            & ! Grid, Conif, etc.
  MAX_NUM_DDEP_WANTED = NSPEC_ADV,  & !plenty big
  MAX_NUM_WDEP_WANTED = NSPEC_ADV,  & !plenty big
  MAX_NUM_NEWMOS  = 30,             & !New system.
  ! Older system
  MAX_NUM_MOSCONCS  = 10,           & !careful here, we multiply by next:
  MAX_NUM_MOSLCS    = 10              !careful here, we multiply bY prev:
character(len=TXTLEN_DERIV), public, save :: &
  wanted_deriv2d(MAX_NUM_DERIV2D) = NOT_SET_STRING, &
  wanted_deriv3d(MAX_NUM_DERIV3D) = NOT_SET_STRING
integer, private, save :: mynum_deriv2d,mynum_deriv3d

!Mass-outputs of advected species, will be added to Derived
! time-res: 'M'-> monthly, 'D'-> daily....

! some shorthands for this table
logical, parameter, private :: T=.true., F=.false.
character(len=TXTLEN_SHORT), private, parameter ::&
  D2="2d", D3="3d", SPEC="SPEC", GROUP="GROUP", SHL ="SHL"

!REMEMBER - KEEP UPPER CASE FOR ALL GASES
type(typ_s5ind), public, save, dimension(MAX_NUM_DERIV2D) :: OutputFields
integer, public, save :: nOutputFields = 0
integer, public, save :: nOutputWdep   = 0

! Direct setting of derived fields:
type(Deriv), public, save, dimension(MAX_NUM_DERIV2D) :: OutputMisc= Deriv()
integer, save, public :: nOutputMisc = 0

type(typ_s5ind), public, save, dimension(MAX_NUM_DERIV2D) :: &
  OutputConcs = typ_s5ind("-","-","-","-","-","-")

! Depositions
type(typ_s1ind), public, save, dimension(MAX_NUM_DDEP_ECOS) :: &
  DDEP_ECOS = typ_s1ind("-",'-') ! e.g. "Grid","YMD", 

type(typ_s3), public, save, dimension(MAX_NUM_DDEP_WANTED) :: &
  DDEP_WANTED = typ_s3('-','-','-'), & ! e.g. typ_s3("SO2",SPEC,"mgS"),
  SDEP_WANTED = typ_s3('-','-','-')    ! Stomatal deposition (for HTAP)
type(typ_s4), public, save, dimension(MAX_NUM_WDEP_WANTED) :: &
  WDEP_WANTED = typ_s4('-','-','-','-')

character(len=TXTLEN_DERIV), public, parameter, dimension(4) :: &
  D2_SR = [character(len=TXTLEN_DERIV):: &
    ! all array members will have len=TXTLEN_DERIV
    ! Surface pressure used for crosssection
    "SURF_MAXO3","SURF_PM25water","SOMO35","PSURF"] 

!============ Extra parameters for model evaluation: ===================!
!character(len=TXTLEN_DERIV), public, parameter, dimension(13) :: &
character(len=TXTLEN_DERIV), public, parameter, dimension(5) :: &
  D2_EXTRA = [character(len=TXTLEN_DERIV):: &
    ! all array members will have len=TXTLEN_DERIV
    "Area_Grid_km2","Area_Conif_Frac","Area_Decid_Frac",&
    "Area_Seminat_Frac","Area_Crops_Frac"]
!   "SoilWater_deep","SoilWater_uppr,&! See SMI_deep above
!   "AreaPOLL"]                       ! Future usage. Should change name too


! Ecosystem dep output uses receiver land-cover classes (LCs)
! which might include several landuse types, e.g. Conif in D2_SO2_m2Conif.
integer, private, save :: nOutDDep, nOutVEGO3
integer, private, save :: nOutMET

!- specify some species and land-covers we want to output
! dep. velocities for in netcdf files. Set in My_DryDep_mod.
! NewMosaic seems to mean new-style, to avoid  needing all combinations
! of MET & LC
type(typ_s5ind), public, save, dimension(MAX_NUM_NEWMOS) :: &
  NewMosaic = typ_s5ind('-','-','-','-','-','-')
  !A17 =[typ_s5ind('Mosaic','VG','O3','Grid','cms','YM')]
integer, private, save :: nOutputMosMet, nOutputMosLC, nOutputNewMos
character(len=10), private, save ::  Mosaic_timefmt='YM'  ! eg 'YMD'

! For met-data and canopy concs/fluxes ...
character(len=TXTLEN_DERIV), public, save, dimension(MAX_NUM_MOSCONCS) :: &
  MOSAIC_METCONCS = '-' ! = [character(len=TXTLEN_DERIV):: & 
     !,"VPD","FstO3","EVAP","Gsto" ,"USTAR","INVL"/)
! "USTAR","LAI","CanopyO3","FstO3"] ! SKIP CanopyO3
! "g_sto" needs more work - only set as L%g_sto

character(len=TXTLEN_DERIV), public, save, dimension(MAX_NUM_MOSLCS) :: &
  MET_LCS = '-' 
! [character(len=TXTLEN_DERIV)::  "DF","GR","BF","TC","IAM_DF","IAM_CR"]

!----------------------
! For some reason having this as a parameter caused problems for PC-gfortran runs.
! other (non-ppb) 3D output, set as zero-size (eg 4:1) for normal runs
character(len=TXTLEN_DERIV), public, save, dimension(4:1) :: &
  D3_OTHER  != (/ "D3_PM25water" /) !**** Under construction *******
            != (/ "D3_m_TH", "D3_m2s_Kz" /)
character(len=len(NOT_SET_STRING)), public, save :: PS_needed
integer, private :: i,j,k,n, ivoc, isec    ! Local loop variables
logical ::found_3D_hourly, found_3D_hourly_inst
logical ::found_hourly_PS, found_hourly_inst_PS

contains

!=========================================================================
subroutine Init_My_Deriv()

  integer :: i, ix, itot, nDD, nMET, nVEGO3=0, n1, istat, nMc, neigh
  integer :: nOutputConcs
  character(len=100) :: errmsg,line
  character(len=TXTLEN_DERIV), dimension(size(OutputConcs(:)%txt1)) :: &
    tag_name    ! Needed to concatanate some text in AddArray calls
                ! - older (gcc 4.1?) gfortran's had bug
  character(len=TXTLEN_SHORT) :: outname, outunit, outdim, outtyp, outclass
  logical :: Is3D,debug0   !  if(DEBUG%MY_DERIVED.and.MasterProc )
  character(len=12), save :: sub='InitMyDeriv:'
  logical ::  &
    lev3d_from_surface=.false. ! levels are to be read from surface up
  character(len=2)::  isec_char
  character(len=3)::  neigh_char
  NAMELIST /OutputConcs_config/OutputMisc,OutputConcs,OutputVegO3
  NAMELIST /OutputDep_config/DDEP_ECOS, DDEP_WANTED, WDEP_WANTED,SDEP_WANTED,&
             NewMosaic, MOSAIC_METCONCS, MET_LCS, Mosaic_timefmt
  NAMELIST /OutputSize_config/fullrun_DOMAIN,month_DOMAIN,day_DOMAIN,&
              hour_DOMAIN, out_startdate, spinup_enddate,&
              num_lev3d,lev3d,lev3d_from_surface

! default output sizes
  fullrun_DOMAIN = RUNDOMAIN
  month_DOMAIN =   RUNDOMAIN
  day_DOMAIN =     RUNDOMAIN
  hour_DOMAIN =    RUNDOMAIN

! default outputting dates
  spinup_enddate = startdate! end of spinup. Does not average concentration etc before that date
  out_startdate = (/-1,-1,-1,-1/) ! notset values

! default levels on 3d output: all model levels (top to bottom)
  num_lev3d=KMAX_MID
  lev3d(1:KMAX_MID)=[(i,i=1,KMAX_MID)]
  lev3d_from_surface=.false.

  debug0 = DEBUG%MY_DERIVED.and.MasterProc

  rewind(IO_NML)
  read(IO_NML,NML=OutputConcs_config,iostat=istat, iomsg=errmsg)
  if (istat/=0) then
      backspace(IO_NML)
      read(IO_NML,fmt='(A)') line
      call CheckStop(errmsg , errmsg // ": " // trim(line))
  end if
  read(IO_NML,NML=OutputDep_config,iostat=istat, iomsg=errmsg)
  if (istat/=0) then
      backspace(IO_NML)
      read(IO_NML,fmt='(A)') line
      call CheckStop(errmsg , errmsg // ": " // trim(line))
  end if
  read(IO_NML,NML=OutputSize_config,iostat=istat, iomsg=errmsg)
  if (istat/=0) then
      backspace(IO_NML)
      read(IO_NML,fmt='(A)') line
      call CheckStop(errmsg , errmsg // ": " // trim(line))
  end if
  if(out_startdate(1)<0)then
     ! notset values are not set in config
     out_startdate = spinup_enddate !start to output when spinup period ends.
  endif

  !shift output domain according to rundomain
  call RestrictDomain(fullrun_DOMAIN)
  call RestrictDomain(month_DOMAIN)
  call RestrictDomain(day_DOMAIN)
  call RestrictDomain(hour_DOMAIN)

  ! restrict number of levels to available model levels
  num_lev3d=max(num_lev3d,1)
  num_lev3d=min(num_lev3d,KMAX_MID)
  do i=1,num_lev3d
    lev3d(i)=max(lev3d(i),1)
    lev3d(i)=min(lev3d(i),KMAX_MID)
  ! levels from surface to model levels (from top)
    if(lev3d_from_surface)&
      lev3d(i)=KMAX_MID-lev3d(i)+1
  ! ensure no level is entered twice
    if(MasterProc)&
      call CheckStop(count(lev3d(1:i)==lev3d(i)),1,&
        "Init_My_Deriv, repeated levels in lev3d")
  end do

  !! Find number of wanted OutoutConcs
  nOutputMisc  = find_index("-", OutputMisc(:)%name, first_only=.true. ) -1
  nOutputConcs = find_index("-", OutputConcs(:)%txt1, first_only=.true. ) -1
  nOutputVegO3 = find_index("-", OutputVegO3(:)%name, first_only=.true. ) -1
  nOutputWdep  = find_index("-", WDEP_WANTED(:)%txt1, first_only=.true. ) -1
  nOutputMosMet = find_index("-", MOSAIC_METCONCS(:), first_only=.true. ) -1
  nOutputMosLC  = find_index("-", MET_LCS(:), first_only=.true. ) -1
  nOutputNewMos = find_index("-", NewMosaic(:)%txt1, first_only=.true. ) -1
       
  if(MasterProc) write(*,"(a,i3)") "NMLOUT nOUTMISC ", nOutputMisc
  found_3D_hourly = .false.
  found_3D_hourly_inst = .false.
  found_hourly_PS = .false.
  found_hourly_inst_PS = .false.

  do i = 1,nOutputMisc  
    Is3D=(OutputMisc(i)%class=="MET3D").or.(OutputMisc(i)%name(1:2)=='D3')&
         .or.(OutputMisc(i)%subclass(1:2)=='D3')
    
    if(MasterProc) write(*,"(4(A,1X),i3,1X,L1,A)")"NMLOUT OUTMISC",&
      trim(OutputMisc(i)%name),trim(OutputMisc(i)%class),&
      trim(OutputMisc(i)%subclass),OutputMisc(i)%index,Is3D
    tag_name(1) = trim(OutputMisc(i)%name)
    if(Is3D)then
      call AddArray(tag_name(1:1),wanted_deriv3d,NOT_SET_STRING,errmsg)
    else
      call AddArray(tag_name(1:1),wanted_deriv2d,NOT_SET_STRING,errmsg)
    end if
    if(tag_name(1)=="PS" .and. scan(OutputMisc(i)%iotype,'H')>0)then
       found_hourly_PS = .true.
    endif
    if(tag_name(1)=="PS" .and. scan(OutputMisc(i)%iotype,'I')>0)then
       found_hourly_inst_PS = .true.
    endif
  end do
   ! OutputVegO3 will be added to derived fields from within the Mosaics_mod
   ! after adding 
  if(MasterProc) then
    write(*,"(3a,f7.1)")("NMLOUT OUTVegO3 ",OutputVegO3(i)%name,&
          OutputVegO3(i)%class, OutputVegO3(i)%Threshold, i=1,nOutputVegO3) 
    write(*,"(a,i3)") "NMLOUT nOUTCONC ", nOutputConcs
    write(*,"(4a)")("NMLOUT CONC ", OutputConcs(i)%txt1, &
             OutputConcs(i)%txt4, OutputConcs(i)%ind, i=1,nOutputConcs)
    do i = 1,size(DDEP_ECOS)  
      if(all(SCAN(DDEP_ECOS(i)%ind,IOU_KEY)==0)) exit
      write(*,"(3a)") "NMLOUT DEP ", DDEP_ECOS(i)%name, DDEP_ECOS(i)%ind
    end do
    do i = 1,size(DDEP_WANTED)  
      if(DDEP_WANTED(i)%txt1=='-') exit
      write(*,"(2a)") "NMLOUT DDEP ", DDEP_WANTED(i)%txt1
    end do
    write(*,"(3a)")("NMLOUT WDEP ",&
      WDEP_WANTED(i)%txt1,WDEP_WANTED(i)%txt3, i=1,nOutputWdep)
    write(*,*) " END NMLOUT INSIDE Init_My_Deriv"
  end if

  call Init_MosaicMMC(MOSAIC_METCONCS)  ! sets MMC_USTAR etc.


 ! Build up the array wanted_deriv2d with the required field names

 call AddArray( "WDEP_" // WDEP_WANTED(1:nOutputWdep)%txt1, &
       wanted_deriv2d, NOT_SET_STRING,errmsg)
 call CheckStop( errmsg, errmsg // "WDEP_WANTED too long" )

! Emission sums - we always add these (good policy!)
  do i = 1, size(EMIS_FILE)
    tag_name(1) = "Emis_mgm2_" // trim(EMIS_FILE(i))
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
  end do
  do  i = 1, NEMIS_FILE
     tag_name(1)="Sec_Emis_mgm2_"//trim(EMIS_FILE(i))
     call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
     do isec=1,NSECTORS
        if(SecEmisOutWanted(isec))then
           write(tag_name(1),"(A,I0,A)")"Sec",isec,"_Emis_mgm2_"//trim(EMIS_FILE(i))
           call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
        endif
     end do
  end do ! 

  if(USE_SOILNOX) then
    tag_name(1) = "Emis_mgm2_BioNatNO"
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
  end if
! HARD-CODED, also since TERP isn't a species name
    tag_name(1) = "Emis_mgm2_BioNatC5H8"
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
    tag_name(1) = "Emis_mgm2_BioNatTERP"
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
!
  if(USES%BIDIR) then
    tag_name(1) = "Emis_mgm2_BioNatNH3"
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
  end if
  if(USE_OCEAN_DMS)then
    tag_name(1) = "Emis_mgm2_DMS"
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
  end if
  if(USE_OCEAN_NH3)then
    tag_name(1) = "Emis_mgm2_Ocean_NH3"
    call AddArray( tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
  end if

 if(EmisSplit_OUT)then
    do i=1,max(18,nrcemis)
      tag_name(1) = "EmisSplit_mgm2_"//trim(species(iqrc2itot(i))%name)
      call AddArray(tag_name(1:1), wanted_deriv2d, NOT_SET_STRING, errmsg)
    end do
 end if

! Do SR last, so we get PM25 after groups have been done
  call AddArray( D2_SR,  wanted_deriv2d, NOT_SET_STRING, errmsg)
  call CheckStop( errmsg, errmsg // "D2_SR too long" )
  if(.not.SOURCE_RECEPTOR) then !may want extra?
    call AddArray( D2_EXTRA, wanted_deriv2d, NOT_SET_STRING, errmsg)
    call CheckStop( errmsg, errmsg // "D2_EXTRA too long" )
  end if

!------------- Depositions to ecosystems --------------------------------
  call Add_MosaicDDEP(DDEP_ECOS,DDEP_WANTED,nDD)
  nOutDDep = nDD

!------------- VEGO3 stuff ----------------------------------------------
! For fluxes or AOTs we start with a formatted name, eg. POD_3.0_CF and
! untangle it to get threshold Y (=3.0) and landcover type
  allocate(VEGO3_OUTPUTS( nOutputVegO3 ), stat=istat)
  if(DEBUG%MY_DERIVED.and.istat/=0) &
     write(*,*) "My_Derived ISTAT ERR VEGO3"

  do n = 1, nOutputVegO3
    VEGO3_OUTPUTS(n) = OutputVegO3(n)
    if(debug0)  write(*,*) "VEGO3 NUMS ", n, trim(OutputVegO3(n)%name) 
  end do
  if(MasterProc) call WriteArray(VEGO3_OUTPUTS(:)%name,nOutputVegO3,&
                                   " VEGO3 OUTPUTS:")
  ! nVEGO3 is output, excluding missing LC types:
  call Add_MosaicVEGO3(nOutVEGO3) 

!----- some "luxury outputs" -------------------------------------------
  if( .not.SOURCE_RECEPTOR)then
    !------------- Deposition velocities ---------------------
    call Add_NewMosaics(NewMosaic, nMc)
    if(debug0)  write(*,*) 'NewMos Nums ', nOutputNewMos, nMC

    !------------- Met data for d_2d -------------------------
    ! We find the various combinations of met and ecosystem,
    ! adding them to the derived-type array LCC_Met (e.g. => Met_CF)
    !FEB2011  Daiyl output asked for just now. Change larer

    call Add_MosaicMetConcs(MOSAIC_METCONCS(1:nOutputMosMet),&
           MET_LCS(1:nOutputMosLC),Mosaic_timefmt, nMET)
    nOutMET = nMET !not needed?

    if(debug0) then
      write(*,*) "NEWMOSAIC   NUM ", nMc
      write(*,*) "VEGO3 FINAL NUM ", nVEGO3
      write(*,*) "nOutputMosMet FINAL NUM ", nOutputMosMet
      write(*,*) "nOutputMosLC  FINAL NUM ", nOutputMosLC
      write(*,*) "nOutputNewMos  FINAL NUM ", nOutputNewMos
      write(*,*) "nOutMet  FINAL NUM ", nOutMet
      write(*,*) "nMosaic  FINAL NUM ", nMosaic 
    end if
  end if ! SOURCE_RECEPTOR

!------------- end LCC data for d_2d -------------------------
  call CheckStop( NMosaic >= MAX_MOSAIC_OUTPUTS, sub//"too many nMosaics" )
  call AddArray( MosaicOutput(1:nMosaic)%name, &
                    wanted_deriv2d, NOT_SET_STRING, errmsg)
  call CheckStop( errmsg, sub//errmsg // "MosaicOutput too long" )

  mynum_deriv2d  = LenArray( wanted_deriv2d, NOT_SET_STRING )

  ! Add the pollutants wanted from OutputConcs:
  ! to both OutputFields and wanted_deriv arrays (TOO MESSY)
  ! Requested species which are not present will trigger warnings
  !type(typ_s5ind), public, parameter, dimension(27) :: &
  !   OutputConcs = (/  typ_s5ind("SO2", "ugS", D2,"AIR_CONCS", SPEC, M),&
  !                     typ_s5ind("SO4", "ugS", D2,"AIR_CONCS", SPEC, M),&

  
  do n = 1, nOutputConcs
    outname= trim(OutputConcs(n)%txt1)
    outunit= trim(OutputConcs(n)%txt2)
    outdim = trim(OutputConcs(n)%txt3)
    outtyp = trim(OutputConcs(n)%txt4)
    outclass=trim(OutputConcs(n)%txt5) ! MISC or SPEC or GROUP
    Is3D    =.false.
    if(outclass=="MISC") then
      select case(outtyp)
      case('FLYmax6h','FLYmax6h:SPEC','FLYmax6h:GROUP')
        tag_name(1)= "MAX6h_" //trim(outname)//"_"//trim(outdim)
      case('COLUMN','COLUMN:SPEC','COLUMN:GROUP')
        tag_name(1)= "COLUMN_"//trim(outname)//"_"//trim(outdim)
      case('AOD','AOD:TOTAL','AOD:SPEC','AOD:SHL','AOD:GROUP',&
           'EXT','EXT:TOTAL','EXT:SPEC','EXT:SHL','EXT:GROUP')
        if(.not.USES%AOD)cycle
        if(outname(1:3)/=outtyp(1:3))&
          outname  = outtyp(1:3)//"_"//trim(outname)
        tag_name(1)=            trim(outname)//"_"//trim(outdim)
        Is3D       =(outtyp(1:3)=="EXT")
      case default
         if(outdim=='3d')Is3D=.true.
         tag_name(1)= trim(outname) ! Just use raw name here
      end select
       
      ! OutputConcs can redefine output param of an output that is wanted by
      ! default
      if(Is3D)then
        if(find_index(tag_name(1),wanted_deriv3d,any_case=.true.)<1)& 
        call AddArray(tag_name(1:1),wanted_deriv3d,NOT_SET_STRING,errmsg)
      else
        if(find_index(tag_name(1),wanted_deriv2d,any_case=.true.)<1)&
        call AddArray(tag_name(1:1),wanted_deriv2d,NOT_SET_STRING,errmsg)
      end if
      call CheckStop(errmsg,errmsg//trim(outname)//" too long")
      nOutputFields = nOutputFields + 1
      OutputFields(nOutputFields) = OutputConcs(n)

    elseif(outtyp=="AIR_CONCS") then
      select case(outclass)
        case(SPEC ) ;n1=find_index(outname,species(:)%name,any_case=.true.)
        case(SHL  ) ;n1=find_index(outname,species(:)%name,any_case=.true.)
        case(GROUP) ;n1=find_index(outname,chemgroups(:)%name,any_case=.true.)
        case default;n1=-1
      end select

      if(n1<1) then
        if( debug0 ) write(*,*) "Xd-2d-SKIP ", n, trim(outname)
        call PrintLog("WARNING: Requested My_Derived OutputField not found: "&
            //trim(outclass)//":"//trim(outname), MasterProc)
        cycle
      end if

      select case(outdim)
      case("2d","2D","SURF")   
        tag_name(1) = "SURF_" // trim(outunit) // "_" //  trim(outname)
        call AddArray(  tag_name(1:1) , wanted_deriv2d, &
                  NOT_SET_STRING, errmsg)
        call CheckStop( errmsg, errmsg // trim(outname) // " too long" )
        nOutputFields = nOutputFields + 1
        OutputFields(nOutputFields) = OutputConcs(n)

      case("Local_Correct")   
        tag_name(1) = "SURF_LF_" // trim(outunit) // "_" //  trim(outname)
        call AddArray(  tag_name(1:1) , wanted_deriv2d, &
                  NOT_SET_STRING, errmsg)
        call CheckStop( errmsg, errmsg // trim(outname) // " too long" )
        nOutputFields = nOutputFields + 1
        OutputFields(nOutputFields) = OutputConcs(n)

      case("3d","3D","MLEV")
        tag_name(1) = "D3_" // trim(outunit) // "_" //  trim(outname)
        call AddArray(  tag_name(1:1) , wanted_deriv3d, &
             NOT_SET_STRING, errmsg)
        call CheckStop( errmsg, errmsg // trim(outname) // " too long" )
        nOutputFields = nOutputFields + 1
          OutputFields(nOutputFields) = OutputConcs(n)
          Is3D=.true.
      case default
        if( debug0 ) write(*,*) "Xd-2d-SKIP ", n, trim(outname)
        call PrintLog("WARNING: Unsupported My_Derived OutputField%outdim: "&
            //trim(outclass)//":"//trim(outname)//":"//trim(outdim), MasterProc)
        cycle
      end select
    else
      call CheckStop("My_Deriv: Unsupported OutputConcs" // &
          trim(outname)//":"//trim(outtyp)//":"//trim(outdim))
    end if
    if(Is3D .and. scan(OutputConcs(n)%ind,'H')>0)found_3D_hourly = .true.
    if(Is3D .and. scan(OutputConcs(n)%ind,'I')>0)found_3D_hourly_inst = .true.
    if(tag_name(1)=="PS" .and. scan(OutputConcs(n)%ind,'H')>0)found_hourly_PS = .true.
    if(tag_name(1)=="PS" .and. scan(OutputConcs(n)%ind,'I')>0)found_hourly_inst_PS = .true.

    if(debug0)write(*,*)"OutputFields-tags ",n,trim(outname),"->",tag_name(1)
 end do

  ! ditto wanted_deriv3d....
  if (.not.SOURCE_RECEPTOR.and.size(D3_OTHER)>0) then
    call AddArray( D3_OTHER,  wanted_deriv3d, NOT_SET_STRING, errmsg)
    call CheckStop( errmsg, errmsg // "Wanted D3 too long" )
  end if


!if 3D hourly outputs are produced, we MUST output PS for defining the vertical coordinates
  PS_needed = NOT_SET_STRING
  tag_name(1) = "PS"  

  PS_needed = ''     
  if(found_3D_hourly .or. found_hourly_PS) PS_needed = trim(PS_needed)//'H'
  if(found_3D_hourly_inst .or. found_hourly_inst_PS) PS_needed = trim(PS_needed)//'I'

  if((found_3D_hourly .and. .not. found_hourly_PS ).or. &
       found_3D_hourly_inst .and. .not. found_hourly_inst_PS )then
     if(MasterProc)write(*,*)'adding PS in hourly output ',trim(PS_needed)
     if(find_index(tag_name(1),wanted_deriv2d)<1)then
        call AddArray( tag_name(1:1), wanted_deriv2d,NOT_SET_STRING, errmsg)
     else
        ix=find_index(tag_name(1),OutputMisc(:)%name)
        if(ix>0)then
           !add hourly outputs
           if(found_3D_hourly)OutputMisc(ix)%iotype=trim(OutputMisc(ix)%iotype)//'H'
           if(found_3D_hourly_inst)OutputMisc(ix)%iotype=trim(OutputMisc(ix)%iotype)//'I'
      else
          ix=find_index(tag_name(1),OutputConcs(:)%txt1)
          if(ix>0)then
             !add hourly outputs
            if(found_3D_hourly)OutputConcs(ix)%ind=trim(OutputConcs(ix)%ind)//'H'
            if(found_3D_hourly_inst)OutputConcs(ix)%ind=trim(OutputConcs(ix)%ind)//'I'
          else
             if(MasterProc)write(*,*)'WARNING : problem writing out PS in hourly'
          endif
        end if        
     end if
  end if

!  do i = 1, num_deriv3d
!     if(me==0)write(*,*)'derived ',f_3d(i)%name,IOU_HOUR,f_3d(i)%iotype
!     if ( wanted_iou(IOU_HOUR,f_3d(i)%iotype) ) then
!        found=.false.
!        do n = 1, num_deriv2d
!           if(me==0)write(*,*)'derived2d ',f_2d(n)%name,f_2d(n)%iotype
!           if(f_2d(n)%name=='PS'.and. wanted_iou(IOU_HOUR,f_2d(n)%iotype) )then
!              found=.true.
!              exit
!           endif
!        enddo
!             NOT_SET_STRING, errmsg)
!        exit
!    end if   
!  enddo

! TEST HERE
  mynum_deriv2d = LenArray( wanted_deriv2d, NOT_SET_STRING )
  mynum_deriv3d = LenArray( wanted_deriv3d, NOT_SET_STRING )

  if(MasterProc) then
    if(DEBUG%MY_DERIVED )then
      write(*,*) "Init_My_Deriv, mynum_deriv2d = ", mynum_deriv2d
      write(*,*) "Init_My_Deriv, mynum_deriv3d = ", mynum_deriv3d
      write(*,*)("DEBUG MyDERIV2D ",i,mynum_deriv2d,wanted_deriv2d(i),&
           i=1,mynum_deriv2d)
    end if
    call WriteArray(wanted_deriv2d,mynum_deriv2d," Required 2D output ")
    call WriteArray(wanted_deriv3d,mynum_deriv3d," Required 3D output ")
  end if
end subroutine Init_My_Deriv
!=========================================================================
subroutine My_DerivFunc( e_2d, class )!  , density )

! We define here here any functions which cannot easily be defined
! in the more general Derived_mod.
  real, dimension(:,:), intent(inout) :: e_2d  !  (i,j) 2-d extract of d_2d
  character(len=*), intent(in)    :: class       ! Class of data
  integer, save :: num_warnings = 0  ! crude counter for now

! real, intent(in), dimension(LIMAX,LJMAX)  :: density
! density = 1 ( or = roa when unit ug)

  select case(class)
!      (not currently used)
    case ( "FRNIT" )
      forall ( i=1:limax, j=1:ljmax )
        e_2d( i,j ) = &
          ( xn_adv(IXADV_NO3_f,i,j,KMAX_MID) * cfac(IXADV_NO3_f,i,j)  &
          + xn_adv(IXADV_NO3_c,i,j,KMAX_MID) * cfac(IXADV_NO3_c,i,j)) &
       /max(1E-80, (xn_adv(IXADV_HNO3,i,j,KMAX_MID) *  cfac(IXADV_HNO3,i,j))&
          + xn_adv(IXADV_NO3_f,i,j,KMAX_MID) * cfac(IXADV_NO3_f,i,j)    &
          + xn_adv(IXADV_NO3_c,i,j,KMAX_MID) * cfac(IXADV_NO3_c,i,j))
      endforall

    case default
      if ( MasterProc .and. num_warnings < 100 ) then
        write(*,*) "My_Deriv:WARNING - REQUEST FOR UNDEFINED OUTPUT:", n, class
          num_warnings = num_warnings + 1
      end if
    end select
end subroutine My_DerivFunc
!=========================================================================
endmodule My_Derived_mod
