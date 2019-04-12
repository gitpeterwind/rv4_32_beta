module Nest_mod
! This module performs the reading or writing of data for nested runs
!
! The Nesting modes and related setings read from Nest_config nml:
!  MODE_READ
!    'NONE':      do not read (default)
!    'NHOUR':     read every NHOURREAD
!    'START':     read at the start of run
!    'RESTART:    read at the start of run, if the files are found
!  MODE_SAVE
!    'NONE':      do not write (default)
!    'NHOUR':     write every NHOURSAVE
!    'END':       write at end of run
!    'OUTDATE':   write every OUTDATE(1:OUTDATE_NDUMP)
!
! To make a nested run:
! 1) run with MODE_SAVE='NHOUR' to write out 3d BC (name in filename_write defined below)
! 2) copy or link filename_write to filename_read_BC (for example "ln -s EMEP_OUT.nc EMEP_IN.nc")
! 3) run (in a smaller domain) with MODE_READ='NHOUR'
!
! Set MODE_SAVE/MODE_READ (in Nest_config nml) and out_DOMAIN (same namelist)
! Choose NHOURSAVE and NHOURREAD
! Also filename_read_BC and filename_read_3D should point to appropriate files
! Be careful to remove old BC files before making new ones.
!
! Grids may have any projection.
! Horizontal interpolation uses a weighted average of the four closest points
! This will work also if points in the present grid are not covered by the external grid.
! Vertical interpolation is done from hybrid coordinates.
!
!To do:
!  It should be possible to save only xn_adv_bnd if the inner grid is known for the outer grid.
!  The routines should be thought together with GlobalBC_mod (can it replace it?)

!----------------------------------------------------------------------------!
! External Boundary (BC) and Initial Conditions (IC)
!   ExternalBICs_mod should handle different for different external sources.
!   Experiment specific information must be set on ExternalBICs namelists.
!   So far coded for CIFS (CAMS50/71) and EnsClimRCA(?) work.
use ExternalBICs_mod,     only: set_extbic, icbc, ICBC_FMT,&
      EXTERNAL_BIC_SET, EXTERNAL_BC, EXTERNAL_BIC_NAME, TOP_BC, &
      iw, ie, js, jn, kt, & ! i West/East bnd; j North/South bnd; k Top
      filename_eta,BC_DAYS
!----------------------------------------------------------------------------!
use CheckStop_mod,           only: CheckStop,check=>CheckNC
use Chemfields_mod,          only: xn_adv    ! emep model concs.
use ChemDims_mod,            only: NSPEC_ADV, NSPEC_SHL
use ChemSpecs_mod,           only: species_adv
use GridValues_mod,          only: A_mid,B_mid, glon,glat, i_fdom,j_fdom, RestrictDomain
use Io_mod,                  only: open_file,IO_TMP,IO_NML,PrintLog
use InterpolationRoutines_mod,  only : grid2grid_coeff,point2grid_coeff
use MetFields_mod,           only: roa
use Config_module,           only: Pref,PT,KMAX_MID,MasterProc,NPROC,DataDir,GRID,&
                                  IOU_INST,RUNDOMAIN,USES
use Debug_module,           only: DEBUG_NEST,DEBUG_ICBC=>DEBUG_NEST_ICBC
use MPI_Groups_mod  
use netcdf,                 only: nf90_open,nf90_write,nf90_close,nf90_inq_dimid,&
                                  nf90_inquire_dimension,nf90_inq_varid,&
                                  nf90_inquire_variable,nf90_get_var,nf90_get_att,&
                                  nf90_put_att,nf90_noerr,nf90_nowrite,nf90_global
use netcdf_mod,              only: Out_netCDF,&
                                  CDFtype=>Real4,ReadTimeCDF,max_filename_length
use OwnDataTypes_mod,        only: Deriv,TXTLEN_SHORT
use Par_mod,                 only: me,li0,li1,lj0,lj1,limax,ljmax,GIMAX,GJMAX,gi0,gj0,gi1,gj1
use Pollen_const_mod,        only: pollen_check
use TimeDate_mod,            only: date,current_date,nmdays
use TimeDate_ExtraUtil_mod,  only: date2nctime,nctime2date,nctime2string,&
                                  date2string,date2file,compare_date
use Units_mod,               only: Units_Scale
use SmallUtils_mod,          only: find_index,key2str,to_upper
use ChemGroups_mod,          only: chemgroups
implicit none

! Nested input/output on OUTDATE mode
integer,private,parameter :: OUTDATE_NDUMP_MAX = 4  ! Number of nested output
integer, public, save     :: OUTDATE_NDUMP     = 1  ! Read by emepctm.f90
! on forecast run (1: start next forecast; 2-4: NMC statistics)
type(date), public :: outdate(OUTDATE_NDUMP_MAX)=date(-1,-1,-1,-1,-1)

!coordinates of subdomain to write, relative to FULL domain (only used in write mode)
integer, public, save :: out_DOMAIN(4) ! =[istart,iend,jstart,jend]

!/-- subroutines

public  :: readxn
public  :: wrtxn

private

logical, private, save :: mydebug =  .false.

integer,private,parameter ::  len_mode=20
character(len=len_mode),private, parameter :: &
  READ_MODES(5)=[character(len=len_mode)::'NONE','RESTART','NHOUR','START','MONTH'],&
  SAVE_MODES(5)=[character(len=len_mode)::'NONE','OUTDATE','NHOUR','END','MONTH']
character(len=len_mode),public, save ::  &
  MODE_READ='',&  ! read  mode
  MODE_SAVE=''    ! write mode
integer, private, save :: NHOURSAVE,NHOURREAD ! write/read frequency
!if(NHOURREAD<NHOURSAVE) the data is interpolated in time

character(len=max_filename_length),public, save ::  &
  template_read_3D = 'EMEP_IN.nc',&       ! Different paths can be set here
  template_read_BC = 'EMEP_IN.nc',&       ! for each of the IO IC/BC files,
  template_write   = 'EMEP_OUT.nc'        ! on Nest_config namelist, if needed.
character(len=max_filename_length),private, save ::  &
  filename_read_3D = 'template_read_3D',& ! Overwritten in readxn and wrtxn.
  filename_read_BC = 'template_read_BC',& ! Filenames are updated according to date
  filename_write   = 'template_write'     ! following respective templates
logical,private, save ::  &               ! if IC/BC are in the same model/run
  native_grid_3D = .false.,&              ! grid, the expensive call to
  native_grid_BC = .false.,&              ! grid2grid_coeff in init_nest can be avoided
  omit_zero_write= .false.                ! skip const=0.0 variables

character(len=max_filename_length),public, save :: MET_inner ='NOTSET' !path to metdata for inner grid
integer, save, public :: RUNDOMAIN_inner(4)=-1 ! RUNDOMAIN used for run in inner grid

! Limit output, e.g. for NMC statistics (3DVar)
character(len=TXTLEN_SHORT), private, save, dimension(NSPEC_ADV) :: &
  WRITE_SPC = ""  ! If these varables remain ""
character(len=TXTLEN_SHORT), private, save, dimension(size(chemgroups)) :: &
  WRITE_GRP = ""  ! all advected species will be written out.

real(kind=8), parameter :: &
  halfsecond=0.5/(24.0*3600.0)! used to avoid rounding errors
!BC values at boundaries in present grid
real, save, allocatable, dimension(:,:,:,:) :: &
  xn_adv_bndw, xn_adv_bnde, & ! west and east
  xn_adv_bnds, xn_adv_bndn, & ! north and south
  xn_adv_bndt                 ! top

real, save, allocatable, dimension(:) :: &
  ndays_ext ! time stamps in days since 1900. NB: only defined on MasterProc

!dimension of external grid for BC
integer,save :: N_ext_BC      ! Note: only defined on MasterProc
integer,save :: KMAX_ext_BC

integer,save :: itime
real(kind=8),save :: rtime_saved(2)

integer,save :: &
  date_nextfile(4), & ! date corresponding to the next BC file to read
  NHOURS_Stride_BC    ! number of hours between start of two consecutive records in BC files
integer, public, parameter :: &
  NHOURS_Stride_BC_default=6 !time between records if only one record per file (RCA for example)

type(icbc), private, target, dimension(NSPEC_ADV) :: &
  adv_ic=icbc('none','none',1.0,.false.,.false.,-1)  ! Initial 3D IC/BC: spcname,varname,wanted,found,ixadv
type(icbc), private, pointer, dimension(:) :: &
  adv_bc=>null()                                     ! Time dependent BC: spcname,varname,wanted,found,ixadv

logical, allocatable, save :: mask_restrict(:,:)
logical, save :: MASK_SET=.false.

contains
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine Config_Nest()
  integer :: ios,i
  logical, save :: first_call=.true.
  NAMELIST /Nest_config/ MODE_READ,MODE_SAVE,NHOURREAD,NHOURSAVE, &
    template_read_3D,template_read_BC,template_write,BC_DAYS,&
    native_grid_3D,native_grid_BC,omit_zero_write,out_DOMAIN,&
    MET_inner,RUNDOMAIN_inner,&
    WRITE_SPC,WRITE_GRP,OUTDATE_NDUMP,outdate

  if(.not.first_call)return
  mydebug = DEBUG_NEST.and.MasterProc
! default write/read supported modes: do nothing
  MODE_READ='NONE'
  MODE_SAVE='NONE'
! write/read frequency: Hours between consecutive saves(wrtxn)/reads(readxn)
  NHOURSAVE=3   ! Between wrtxn calls.  Should be fraction of 24
  NHOURREAD=1   ! Between readxn calls. Should be fraction of 24
! Default domain for write modes
  out_DOMAIN=RUNDOMAIN
  rewind(IO_NML)
  read(IO_NML,NML=Nest_config,iostat=ios)
  call CheckStop(ios,"NML=Nest_config")
  if(mydebug)then
    write(*,*) "NAMELIST IS "
    write(*,NML=Nest_config)
  end if
  if(MODE_READ=='')then
    MODE_READ='NONE'
  else
    MODE_READ=to_upper(MODE_READ)
  end if
  if(MODE_SAVE=='')then
    MODE_SAVE='NONE'
  else
    MODE_SAVE=to_upper(MODE_SAVE)
  end if
! write/read supported modes
  if(MasterProc)then
    call CheckStop(.not.any(MODE_READ==READ_MODES),&
      "Config_Nest: Unsupported MODE_READ='"//trim(MODE_READ))
    call CheckStop(.not.any(MODE_SAVE==SAVE_MODES),&
      "Config_Nest: Unsupported MODE_SAVE='"//trim(MODE_SAVE))
  end if
! write/read frequency should be fraction of 24
  if(MasterProc)then
    call CheckStop(mod(24,NHOURSAVE),"Config_Nest: NHOURSAVE should be fraction of 24")
    call CheckStop(mod(24,NHOURREAD),"Config_Nest: NHOURREAD should be fraction of 24")
  end if
! expand DataDir/GRID keyswords
  template_read_3D=key2str(template_read_3D,'DataDir',DataDir)
  template_read_3D=key2str(template_read_3D,'GRID',GRID)
  template_read_BC=key2str(template_read_BC,'DataDir',DataDir)
  template_read_BC=key2str(template_read_BC,'GRID',GRID)
  template_write  =key2str(template_write  ,'DataDir',DataDir)
  template_write  =key2str(template_write  ,'GRID',GRID)
! Update filenames according to date following templates defined on Nest_config
  call init_icbc(cdate=current_date)
! Ensure out-domain is not larger than run-domain
  if(MODE_SAVE/='NONE')call RestrictDomain(out_DOMAIN)
! Ensure that only OUTDATE_NDUMP are taking into account
  if(MODE_SAVE=='OUTDATE')then
    if(OUTDATE_NDUMP<OUTDATE_NDUMP_MAX)&
      outdate(OUTDATE_NDUMP+1:OUTDATE_NDUMP_MAX)%day=0
    if(MasterProc)&
      write (*,"(1X,A,10(1X,A,:,','))")'OUTDATE nest/dump at:',&
       (date2string("YYYY-MM-DD hh:mm:ss",outdate(i)),i=1,OUTDATE_NDUMP)
  end if
  first_call=.false.
end subroutine Config_Nest
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine readxn(indate)
  type(date), intent(in) :: indate           ! Gives year..seconds

  integer :: n,i,j,k,KMAX_BC,bc,ndate(4)
  real(kind=8):: ndays_indate

  !    real , dimension(48,48,20) ::data
  real :: W1,W2
  logical, save :: first_call=.true.
  logical :: fexist_3D=.false.,fexist_BC=.false.
  integer, save :: oldmonth=0

  call Config_Nest()
  if(mydebug) write(*,*)'Nest:Read BC, MODE=',MODE_READ
  if(MODE_READ=='NONE')return

  KMAX_BC=KMAX_MID
  ndate(1:4)=[indate%year,indate%month,indate%day,indate%hour]
  call date2nctime(ndate,ndays_indate)
  if(first_call)date_nextfile=ndate

  select case(MODE_READ)
  case('MONTH') ! monthly input file
    if(indate%month==oldmonth)return
    if(MasterProc.and.oldmonth==0) write(*,*)'Nest: Initialzing IC'
    oldmonth=indate%month
    if(MasterProc) write(*,*)'Nest: New month, reset BC'
  case('START')
    if(.not.first_call)return
    first_call=.false.
    filename_read_3D=date2string(template_read_3D,ndate,mode='YMDH',debug=mydebug)
    if(MasterProc) write(*,*)'Nest RESET ALL XN 3D ',trim(filename_read_3D)
    call reset_3D(ndays_indate)
    return
  case default
   !if(MasterProc) print *,'call to READXN',indate%hour,indate%seconds
    if(mod(indate%hour,NHOURREAD)/=0.or.indate%seconds/=0)return
  end select
  ! never comes to this point if MODE=100, 11 or 12

  if(DEBUG_NEST.and.MasterProc) write(*,*) 'Nest: kt', kt, first_call

! Update filenames according to date following templates defined on Nest_config nml
  filename_read_3D=date2string(template_read_3D,ndate,&
                               mode='YMDH',debug=mydebug)
  filename_read_BC=date2file  (template_read_BC,ndate,BC_DAYS,"days",&
                               mode='YMDH',debug=mydebug)
  inquire(file=filename_read_3D,exist=fexist_3D)
  inquire(file=filename_read_BC,exist=fexist_BC)

  if(first_call)then
    first_call=.false.
    if(fexist_3D)then
      if(MasterProc)write(*,*)'Nest RESET ALL XN 3D ',trim(filename_read_3D)
      call reset_3D(ndays_indate)
    else
      if(MasterProc)write(*,*)'No Nest IC file found: ',trim(filename_read_3D)
    end if

! the first hour only these values are used, no real interpolation between two records
    if(fexist_BC)then
      if(mydebug) write(*,*)'Nest: READING FIRST BC DATA from ',&
            trim(filename_read_BC), ndays_indate
      call read_newdata_LATERAL(ndays_indate)
      if(mydebug) write(*,"(a,5i4)")'Nest: iw, ie, js, jn, kt ',iw,ie,js,jn,kt
    end if
  end if
  if(.not.fexist_BC)then
    if(MasterProc)write(*,*)'No Nest BC file found: ',trim(filename_read_BC)
    return
  end if

  if(ndays_indate-rtime_saved(2)>halfsecond.or.MODE_READ=='MONTH')then
    ! look for a new data set
    if(MasterProc) write(*,*)'Nest: READING NEW BC DATA from ',&
          trim(filename_read_BC)
    call read_newdata_LATERAL(ndays_indate)
  end if

!   make weights for time interpolation
  if(MODE_READ=='MONTH')then  ! don't interpolate for now
    W1=0.0;  W2=1.0           ! use last read value
  else
    W1=1.0;  W2=0.0           ! default: use first read value
    if(rtime_saved(2)-rtime_saved(1)<halfsecond)then
      W1=0.0;  W2=1.0         ! use last read value
    elseif(ndays_indate-rtime_saved(1)>halfsecond)then
      W2=(ndays_indate-rtime_saved(1))/(rtime_saved(2)-rtime_saved(1))
      W1=1.0-W2               ! interpolate
    end if
  end if
  if(DEBUG_NEST.and.MasterProc) then
    write(*,*) 'Nesting BC 2D: time weights : ',W1,W2
    write(*,*) 'Nesting BC 2D: time stamps : ',rtime_saved(1),rtime_saved(2)
  end if

  do bc=1,size(adv_bc)
    if(.not.(adv_bc(bc)%wanted.and.adv_bc(bc)%found))cycle
    n=adv_bc(bc)%ixadv
    if(DEBUG_ICBC.and.MasterProc) write(*,"(2(A,1X),I0,'-->',I0)") &
      'NestICBC: Nesting component',trim(adv_bc(bc)%varname),bc,n
    forall (i=iw:iw, k=1:KMAX_BC, j=1:ljmax, i>=1) &
      xn_adv(n,i,j,k)=W1*xn_adv_bndw(n,j,k,1)+W2*xn_adv_bndw(n,j,k,2)
    forall (i=ie:ie, k=1:KMAX_BC, j=1:ljmax, i<=limax) &
      xn_adv(n,i,j,k)=W1*xn_adv_bnde(n,j,k,1)+W2*xn_adv_bnde(n,j,k,2)
    forall (j=js:js, k=1:KMAX_BC, i=1:limax, j>=1) &
      xn_adv(n,i,j,k)=W1*xn_adv_bnds(n,i,k,1)+W2*xn_adv_bnds(n,i,k,2)
    forall (j=jn:jn, k=1:KMAX_BC, i=1:limax, j<=ljmax) &
      xn_adv(n,i,j,k)=W1*xn_adv_bndn(n,i,k,1)+W2*xn_adv_bndn(n,i,k,2)
    forall (k=kt:kt, i=1:limax, j=1:ljmax, k>=1) &
      xn_adv(n,i,j,k)=W1*xn_adv_bndt(n,i,j,1)+W2*xn_adv_bndt(n,i,j,2)
  end do

  call CheckStop(EXTERNAL_BIC_NAME=="RCA",&
    "WORK NEEDED: RCA BICs commented out in Nest_mod - not consistent with all chem schemes")
end subroutine readxn

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine wrtxn(indate,WriteNow)
  type(date), intent(in) :: indate
  logical, intent(in) :: WriteNow !Do not check indate value
  real :: data(LIMAX,LJMAX,KMAX_MID) ! Data array
  logical, parameter :: APPEND=.false.

  type(Deriv) :: def1 ! definition of fields
  integer :: n,i,j,k,iotyp,ndim,kmax,ncfileID
  real :: scale
  logical :: fexist, wanted, wanted_out, overwrite
  logical, save :: first_call=.true.

  call Config_Nest()
  if(MODE_SAVE=='NONE')return

! Check if the file exist already at start of run. Do not wait until first write to stop!
! If you know what you are doing you can set paramter APPEND=.true.,
! and the new data will be appended to the file
  overwrite=first_call.and..not.APPEND
  if(overwrite.and.MasterProc)then
    filename_write=date2string(template_write,indate,mode='YMDH',debug=mydebug)
    inquire(file=fileName_write,exist=overwrite)
    call CheckStop(overwrite.and.MODE_SAVE/='OUTDATE',&
      "Nest: Refuse to overwrite. Remove this file: "//trim(fileName_write))
  end if

  select case(MODE_SAVE)
  case('END')
    if(.not.WriteNow)return
  case('OUTDATE')
    outdate(:)%seconds=0   ! output only at full hours
    if(.not.compare_date(OUTDATE_NDUMP,indate,outdate(:OUTDATE_NDUMP),&
                         wildcard=-1))return
    if(MasterProc) write(*,*)&
      date2string(" Forecast nest/dump at YYYY-MM-DD hh:mm:ss",indate)
  case('MONTH')
    if(indate%month==1.or.indate%day/=1.or.indate%hour/=0.or.indate%seconds/=0)return
  case default
    if(mod(indate%hour,NHOURSAVE)/=0.or.indate%seconds/=0)return
  end select

  iotyp=IOU_INST
  ndim=3 !3-dimensional
  kmax=KMAX_MID
  scale=1.0
  def1%class='Advected' ! written
  def1%avg=.false.      ! not used
  def1%index=0          ! not used
  def1%scale=scale      ! not used
  def1%iotype=''        ! not used
  def1%name=''          ! written
  def1%unit='mix_ratio' ! written

! Update filenames according to date following templates defined on Nest_config nml
! e.g. set template_write="EMEP_BC_MMYYYY.nc" on namelist for different names each month
  filename_write=date2string(template_write,indate,mode='YMDH',debug=mydebug)
  if(MasterProc)then
    inquire(file=fileName_write,exist=fexist)
    write(*,*)'Nest:write data ',trim(fileName_write)
  end if
  CALL MPI_BCAST(fexist,1,MPI_LOGICAL,0,MPI_COMM_CALC,IERROR)
  overwrite=fexist.and.first_call.and..not.APPEND

! Limit output, e.g. for NMC statistics (3DVar and restriction to inner grid BC)
  if(first_call)then
    call init_icbc(cdate=indate)
    if(any([WRITE_GRP,WRITE_SPC]/=""))then
      adv_ic(:)%wanted=.false.
      do n=1,size(WRITE_GRP)
        if(WRITE_GRP(n)=="")cycle
        i=find_index(WRITE_GRP(n),chemgroups(:)%name)
        if(i>0)then
          where(chemgroups(i)%specs>NSPEC_SHL) &
            adv_ic(chemgroups(i)%specs-NSPEC_SHL)%wanted=.true.
        elseif(MasterProc)then
          write(*,"(A,':',/2(2X,A,1X,'''',A,'''',1X,A,'.'))")&
           "Warning (wrtxn)", &
           "Wanted group",trim(WRITE_GRP(n)),"was not found", &
           "Can not be written to file:",trim(filename_write),""
        end if
      end do
      do n=1,size(WRITE_SPC)
        if(WRITE_SPC(n)=="")cycle
        i=find_index(WRITE_SPC(n),species_adv(:)%name)
        if(i>0)then
          adv_ic(i)%wanted=.true.
        elseif((DEBUG_NEST.or.DEBUG_ICBC).and.MasterProc)then
          write(*,"(A,':',/2(2X,A,1X,'''',A,'''',1X,A,'.'))")&
           "Warning (wrtxn)", &
           "Wanted specie",trim(WRITE_SPC(n)),"was not found", &
           "Can not be written to file:",trim(filename_write),""
        end if
      end do
    elseif(USES%POLLEN)then
      ! POLLEN group members are written to pollen restart/dump file
      call pollen_check(igrp=i)
      if(i>0)then
        where(chemgroups(i)%specs>NSPEC_SHL) &
          adv_ic(chemgroups(i)%specs-NSPEC_SHL)%wanted=.false.
        if((DEBUG_NEST.or.DEBUG_ICBC).and.MasterProc)&
          write(*,"(A,':',/2(2X,A,1X,'''',A,'''',1X,A,'.'))")&
           "Warning (wrtxn)", &
           "Group","POLLEN","is written to pollen restart/dump file", &
           "Will not be written to file:",trim(filename_write),""
      end if
    end if
    do n=1,NSPEC_ADV
      if(.not.adv_ic(n)%wanted)then
        if((DEBUG_NEST.or.DEBUG_ICBC).and.MasterProc)&
          write(*,"(A,':',/2(2X,A,1X,'''',A,'''',1X,A,'.'))")&
            "Nest(wrtxn) DEBUG_ICBC",&
            "Variable",trim(species_adv(n)%name),"is not wanted as IC",&
            "Will not be written to file:",trim(filename_write),""
      elseif(omit_zero_write)then !  further reduce output
        wanted=any(xn_adv(n,:,:,:)/=0.0)
        CALL MPI_ALLREDUCE(wanted,wanted_out,1,MPI_LOGICAL,MPI_LOR,&
                           MPI_COMM_CALC,IERROR)
        adv_ic(n)%wanted=wanted_out
        if(.not.adv_ic(n)%wanted.and.&
          (DEBUG_NEST.or.DEBUG_ICBC).and.MasterProc)&
          write(*,"(A,':',/2(2X,A,1X,'''',A,'''',1X,A,'.'))")&
            "Nest(wrtxn) DEBUG_ICBC",&
            "Variable",trim(species_adv(n)%name),"was found constant=0.0",&
            "Will not be written to file:",trim(filename_write),""
      end if
    end do

    if(MET_inner /= "NOTSET")then
       ! find region that is really needed, i.e. boundaries of inner grid
       !find lon and lat of inner grid restricted to BC 
       call init_mask_restrict(MET_inner,RUNDOMAIN_inner)
    endif

  end if

  !do first one loop to define the fields, without writing them (for performance purposes)
  ncfileID=-1 ! must be <0 as initial value
  if(.not.fexist.or.overwrite)then
    do n=1,NSPEC_ADV
      if(.not.adv_ic(n)%wanted)cycle
      def1%name=species_adv(n)%name   ! written
!!    data=xn_adv(n,:,:,:)
      call Out_netCDF(iotyp,def1,ndim,kmax,data,scale,CDFtype=CDFtype,&
            out_DOMAIN=out_DOMAIN,create_var_only=.true.,overwrite=overwrite,&
            fileName_given=trim(fileName_write),ncFileID_given=ncFileID)
      overwrite=.false.
    end do
  end if

  do n=1,NSPEC_ADV
    if(.not.adv_ic(n)%wanted)cycle
    def1%name=species_adv(n)%name     ! written
    if(MASK_SET)then
       do k=1,KMAX_MID
          do j=1,LJMAX
             do i=1,LIMAX
                if(mask_restrict(i,j))then
                   data(i,j,k)=xn_adv(n,i,j,k)
                else
                   data(i,j,k)=0.0
                endif
             end do
          end do
       end do
    else
       data=xn_adv(n,:,:,:)
    endif
    call Out_netCDF(iotyp,def1,ndim,kmax,data,scale,CDFtype=CDFtype,&
         out_DOMAIN=out_DOMAIN,create_var_only=.false.,&
         fileName_given=trim(fileName_write),ncFileID_given=ncFileID)
  end do

  if(first_call .and. MET_inner /= "NOTSET" .and. me==0)then
     !mark the file as defined in a restricted area only
     call check(nf90_put_att(ncFileID,nf90_global,"restricted","BC_restricted"))
  endif
  first_call=.false.

  if(MasterProc)call check(nf90_close(ncFileID))

end subroutine wrtxn

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine init_icbc(idate,cdate,ndays,nsecs)
!----------------------------------------------------------------------------!
! Setup IC/BC detailed description.
! ICs are assumed to come from emepctm.
!
! adv_ic            IC detailed description for all adv species
! adv_bc            BC detailed description relevant adv species
! EXTERNAL_BC       External (non emepctm) BC detailed description/setup
! EXTERNAL_BIC_SET  EXTERNAL_BC has been set (adv_bc=>EXTERNAL_BC)
!        otherwise  Assume emepctm BCs        (adv_bc=>adv_ic)
!----------------------------------------------------------------------------!
  integer,   intent(in), optional :: idate(4)
  type(date),intent(in), optional :: cdate
  real(kind=8),intent(in),optional:: ndays
  integer,     intent(in),optional:: nsecs
  logical, save :: first_call=.true.
  integer :: n,dat(4)

  if(.not.first_call)return
  first_call=.false.

! One of the date formats needs to be provided
  call CheckStop(count([present(idate),present(cdate),present(ndays),&
                        present(nsecs)]),1,"init_icbc: wrong date option")

! Update filenames according to date following templates defined on Nest_config nml
  if(present(idate)) dat=idate
  if(present(cdate)) dat=[cdate%year,cdate%month,cdate%day,cdate%hour]
  if(present(ndays)) call nctime2date(dat,ndays)
  if(present(nsecs)) call nctime2date(dat,nsecs)
  call set_extbic(dat)  ! set mapping, EXTERNAL_BC, TOP_BC

  if(.not.EXTERNAL_BIC_SET.and.MODE_READ=='NONE'.and.MODE_SAVE=='NONE')return !No nesting

  filename_read_3D=date2string(template_read_3D,dat,&
                               mode='YMDH',debug=mydebug)
  filename_read_BC=date2file  (template_read_BC,dat,BC_DAYS,"days",&
                               mode='YMDH',debug=mydebug)
  filename_write  =date2string(template_write  ,dat,&
                               mode='YMDH',debug=mydebug)

  adv_ic(:)%ixadv=(/(n,n=1,NSPEC_ADV)/)
  adv_ic(:)%spcname=species_adv(:)%name
  adv_ic(:)%varname=species_adv(:)%name
  adv_ic(:)%frac=1.0
  adv_ic(:)%wanted=.true.
  adv_ic(:)%found=find_icbc(filename_read_3D,adv_ic%varname(:))
  if(EXTERNAL_BIC_SET) then
    adv_bc=>EXTERNAL_BC
    adv_bc(:)%found=find_icbc(filename_read_bc,adv_bc%varname(:))
  else
    adv_bc=>adv_ic
  end if

  if(MasterProc)then
    do n = 1,size(adv_ic%varname)
      if(.not.adv_ic(n)%found)then
        call PrintLog("WARNING: IC variable '"//trim(adv_ic(n)%varname)//"' not found")
      elseif(DEBUG_NEST.or.DEBUG_ICBC)then
        write(*,*) "init_icbc filled adv_ic "//trim(adv_ic(n)%varname)
      end if
    end do
    do n = 1,size(adv_bc%varname)
      if(.not.adv_bc(n)%found)then
        call PrintLog("WARNING: BC variable '"//trim(adv_bc(n)%varname)//"' not found")
      elseif(DEBUG_NEST.or.DEBUG_ICBC)then
        write(*,*) "init_icbc filled adv_bc "//trim(adv_bc(n)%varname)
      end if
    end do
  end if

  if((DEBUG_NEST.or.DEBUG_ICBC).and.MasterProc)then
    write(*,"(a)") "Nest: DEBUG_ICBC Variables:",&
      trim(filename_read_3D),trim(filename_read_BC)
    write(*,"((1X,A,I3,'->',"//ICBC_FMT//"))") &
      ('Nest: ADV_IC',n,adv_ic(n),n=1,size(adv_ic)),&
      ('Nest: ADV_BC',n,adv_bc(n),n=1,size(adv_bc))
  end if
contains
function find_icbc(filename_read,varname) result(found)
!----------------------------------------------------------------------------!
! Check if variables (varname) are present on file (filename_read)
!----------------------------------------------------------------------------!
  implicit none
  character(len=*), intent(in)               :: filename_read
  character(len=*), dimension(:), intent(in) :: varname
  logical, dimension(size(varname))          :: found
  integer :: ncFileID,varID,status,n

  found(:)=.false.
  if(MasterProc)then
    status=nf90_open(trim(filename_read),nf90_nowrite,ncFileID)
    if(status/=nf90_noerr) then
      print *,'icbc: not found ',trim(filename_read)
    else
      print *,'icbc: reading ',trim(filename_read)
      do n=1,size(varname)
        if(varname(n)=="") cycle
        status=nf90_inq_varid(ncFileID,trim(varname(n)),varID)
        found(n)=(status==nf90_noerr)
      end do
      call check(nf90_close(ncFileID))
    end if
  end if
  CALL MPI_BCAST(found,size(found),MPI_LOGICAL,0,MPI_COMM_CALC,IERROR)
end function find_icbc
end subroutine init_icbc

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine init_nest(ndays_indate,filename_read,native_grid,IIij,JJij,Weight,&
     k1_ext,k2_ext,weight_k1,weight_k2,N_ext,KMAX_ext,GIMAX_ext,GJMAX_ext)
  logical, parameter :: USE_LAST_HYBRID_LEVELS=.true.
  character(len=*),intent(in) :: filename_read
  logical,intent(in) :: native_grid
  real ,intent(out):: Weight(4,LIMAX,LJMAX)
  integer ,intent(out)::IIij(4,LIMAX,LJMAX),JJij(4,LIMAX,LJMAX)
  integer, intent(out), dimension(*) :: k1_ext,k2_ext
  real, intent(out), dimension(*) :: weight_k1,weight_k2
  integer ,intent(out)::N_ext,KMAX_ext,GIMAX_ext,GJMAX_ext
  real(kind=8) :: ndays_indate
  integer :: ncFileID,timeDimID,varid,status,dimIDs(3)
  real :: P_emep
  integer :: i,j,k,n,k_ext
  real, allocatable, dimension(:,:) ::lon_ext,lat_ext
  real, allocatable, dimension(:) ::hyam,hybm,P_ext,temp_ll
  character(len=80) ::projection,word,iDName,jDName
  logical :: reversed_k_BC,time_exists,fexist

  rtime_saved = -99999.9 !initialization

  !Read dimensions (global)
  if(MasterProc)then
    status = nf90_open(trim(filename_read),nf90_nowrite,ncFileID)
    if(status/=nf90_noerr) then
      print *,'init_Nest: not found',trim(filename_read)
      return
    else
      print *,'init_Nest: reading ',trim(filename_read)
    end if

    projection='Unknown'
    status = nf90_get_att(ncFileID,nf90_global,"projection",projection)
    if(status==nf90_noerr) then
      if(projection=='lon_lat')projection='lon lat'
      write(*,*)'Nest: projection: '//trim(projection)
    else
      projection='lon lat'
      write(*,*)'Nest: projection not found for ',&
           trim(filename_read)//', assuming '//trim(projection)
    end if
    !get dimensions id/name/len: include more dimension names, if necessary
    GIMAX_ext=get_dimLen([character(len=12)::"i","lon","longitude"],name=iDName)
    GJMAX_ext=get_dimLen([character(len=12)::"j","lat","latitude" ],name=jDName)
    KMAX_ext =get_dimLen([character(len=12)::"k","mlev","lev","level"])

    select case(projection)
    case('Stereographic')
      call CheckStop("i",iDName,"Nest: unsuported "//&
        trim(iDName)//" as i-dimension on "//trim(projection)//" projection")
      call CheckStop("j",jDName,"Nest: unsuported "//&
        trim(jDName)//" as j-dimension on "//trim(projection)//" projection")
    case('lon lat')
      call CheckStop("lon",iDName(1:3),"Nest: unsuported "//&
        trim(iDName)//" as i-dimension on "//trim(projection)//" projection")
      call CheckStop("lat",jDName(1:3),"Nest: unsuported "//&
        trim(jDName)//" as j-dimension on "//trim(projection)//" projection")
    case default
     !call CheckStop("Nest: unsuported projection "//trim(projection))
     !write(*,*)'GENERAL PROJECTION ',trim(projection)
      call CheckStop("i",iDName,"Nest: unsuported "//&
        trim(iDName)//" as i-dimension on "//trim(projection)//" projection")
      call CheckStop("j",jDName,"Nest: unsuported "//&
        trim(jDName)//" as j-dimension on "//trim(projection)//" projection")
    end select

    N_ext=0
    status = nf90_inq_dimid(ncFileID,"time",timeDimID)
    time_exists=(status==nf90_noerr)
    if(time_exists) then
      call check(nf90_inquire_dimension(ncFileID,timedimID,len=N_ext))
    else
      status = nf90_inq_dimid(ncFileID,"Months",dimID=timeDimID)
      if(status==nf90_noerr)then
        call check(nf90_inquire_dimension(ncFileID,timedimID,len=N_ext))
        call CheckStop(N_ext,12,'Nest BC: did not find 12 months')
      else
        write(*,*)'Nest: time dimension not found. Assuming only one record '
        N_ext=1
      end if
    end if

    write(*,*)'Nest: dimensions external grid',GIMAX_ext,GJMAX_ext,KMAX_ext,N_ext
    if(.not.allocated(ndays_ext))then
      allocate(ndays_ext(N_ext))
    elseif(size(ndays_ext)<N_ext)then
      if(Masterproc)write(*,*)'Nest: Sizes times ',N_ext
      deallocate(ndays_ext)
      allocate(ndays_ext(N_ext))
    end if

  end if
  CALL MPI_BCAST(GIMAX_ext,4*1,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
  CALL MPI_BCAST(GJMAX_ext,4*1,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
  CALL MPI_BCAST(KMAX_ext ,4*1,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
 !CALL MPI_BCAST(N_ext,4*1,MPI_BYTE,0,MPI_COMM_CALC,IERROR) !not needed by others than MasterProc

  allocate(lon_ext(GIMAX_ext,GJMAX_ext),lat_ext(GIMAX_ext,GJMAX_ext))
  allocate(hyam(KMAX_ext+1),hybm(KMAX_ext+1),P_ext(KMAX_ext))

  if(MasterProc)then
    !Read lon lat of the external grid (global)
    if(trim(projection)==trim('lon lat')) then
      call check(nf90_inq_varid(ncFileID,iDName,varID),&
        "Read lon-variable: "//trim(iDName))
      allocate(temp_ll(GIMAX_ext))
      call check(nf90_get_var(ncFileID,varID,temp_ll))
      lon_ext=SPREAD(temp_ll,2,GJMAX_ext)
      deallocate(temp_ll)
      call check(nf90_inq_varid(ncFileID,jDName,varID),&
        "Read lat-variable: "//trim(jDName))
      allocate(temp_ll(GJMAX_ext))
      call check(nf90_get_var(ncFileID,varID,temp_ll))
      lat_ext=SPREAD(temp_ll,1,GIMAX_ext)
      deallocate(temp_ll)
    else
      call check(nf90_inq_varid(ncFileID,"lon",varID),"dim:lon")
      call check(nf90_get_var(ncFileID,varID,lon_ext),"get:lon")

      call check(nf90_inq_varid(ncFileID,"lat",varID),"dim:lat")
      call check(nf90_get_var(ncFileID,varID,lat_ext),"get:lat")
    end if

    if(time_exists)then
      call ReadTimeCDF(filename_read,ndays_ext,N_ext)
    else
      !cannot read time on file. assumes it is correct
      ndays_ext(1)=ndays_indate
    end if
    if(MODE_READ=='MONTH')then
      !asuming 12 monthes for BC, and 12 or 1 values for IC
      ndays_ext(1)=0
      do n=2,N_ext
        ndays_ext(n)=ndays_ext(n-1)+nmdays(n-1)
      end do
    elseif(ndays_ext(1)-ndays_indate>halfsecond)then
      write(*,*)'WARNING: Nest did not find BIC for date ',&
        nctime2string('YYYY-MM-DD hh:mm:ss',ndays_indate)
      write(*,*)'Nest first date found ',&
        nctime2string('YYYY-MM-DD hh:mm:ss',ndays_ext(1))
    end if

    if(N_ext>1)then
      NHOURS_Stride_BC = nint((ndays_ext(2)-ndays_ext(1))*24)
    else
      !use manually set stride:
      NHOURS_Stride_BC = NHOURS_Stride_BC_default
    end if
    write(*,*)'Nest: new BC record every ',NHOURS_Stride_BC,' hours'

    ! Read pressure for vertical levels
    write(*,*)'Nest: reading vertical levels'

    status = nf90_inq_varid(ncFileID,"hyam",varID)
    if(status==nf90_noerr) then
      write(*,*)'Found hyam type levels (values at level midpoints)'
      call check(nf90_inquire_variable(ncFileID,varID,dimIDs=dimIDs))
      call check(nf90_inquire_dimension(ncFileID,dimIDs(1),len=k))
      call CheckStop(k<KMAX_ext,"Nest BC, wrong hyam/hybm dimension")
      if(USE_LAST_HYBRID_LEVELS)then
        k_ext=1+k-KMAX_ext ! for 1+k-KMAX_ext .. k levels
      else
        k_ext=1            ! for 1 .. KMAX_ext levels
      end if
      if(k/=KMAX_ext)&
        write(*,"(A,4(1X,A,I0))")'Nest BC warning:',&
             'kdim #lev=',KMAX_ext,'and hyam/hybm #lev=',k,&
             '. Using only levels ',k_ext,'..',k
      call check(nf90_get_var(ncFileID,varID,hyam,start=(/k_ext/),count=(/KMAX_ext/)))
      status = nf90_get_att(ncFileID,VarID,"units",word)
      if(status==nf90_noerr)then
        if(word(1:3)=='hPa')then
          write(*,*)'Changing hyam from hPa to Pa'
          hyam=100*hyam
        end if
      end if
      call check(nf90_inq_varid(ncFileID,"hybm",varID))
      call check(nf90_get_var(ncFileID,varID,hybm,start=(/k_ext/),count=(/KMAX_ext/)))
    else

      status = nf90_inq_varid(ncFileID,"hyai",varID)
      if(status==nf90_noerr) then
        write(*,*)'Found hyai type levels (values at level interfaces)'

        call check(nf90_inquire_variable(ncFileID,varID,dimIDs=dimIDs))
        call check(nf90_inquire_dimension(ncFileID,dimIDs(1),len=k))
        call CheckStop(k<KMAX_ext+1,"Nest BC, wrong hyai/hybi dimension")
        if(USE_LAST_HYBRID_LEVELS)then
          k_ext=1+(k-1)-KMAX_ext ! for 1+k-KMAX_ext .. k levels
        else
          k_ext=1            ! for 1 .. KMAX_ext levels
        end if
        if(k/=KMAX_ext+1.and.MasterProc)&
          write(*,"(A,4(1X,A,I0))")'Nest BC warning:',&
            'kdim #lev=',KMAX_ext,'and hyam/hybm #lev=',k,&
            '. Using only levels ',k_ext,'..',k
        call check(nf90_get_var(ncFileID,varID,hyam,start=(/k_ext/),count=(/KMAX_ext+1/)))
        status = nf90_get_att(ncFileID,VarID,"units",word)
        if(status==nf90_noerr)then
          if(word(1:3)=='hPa')then
            write(*,*)'Changing hyai from hPa to Pa'
            hyam=100*hyam
          end if
        end if
        call check(nf90_inq_varid(ncFileID,"hybi",varID))
        call check(nf90_get_var(ncFileID,varID,hybm,start=(/k_ext/),count=(/KMAX_ext+1/)))
        do k=1,KMAX_ext
          hyam(k)=0.5*(hyam(k)+hyam(k+1))
          hybm(k)=0.5*(hybm(k)+hybm(k+1))
        end do

      else
        inquire(file=filename_eta,exist=fexist)
        status = nf90_inq_varid(ncFileID,"k",varID)
        if(status==nf90_noerr) then
          write(*,*)'Nest: assuming sigma level and PT=',PT,KMAX_ext
          call check(nf90_get_var(ncFileID, varID, hybm,count=(/ KMAX_ext /) ))!NB: here assume = sigma
          do k=1,KMAX_ext
            hyam(k)=PT*(1.0-hybm(k))
          end do
        elseif(fexist) then
          !read eta levels from ad-hoc text file
          write(*,*)'Nest: Reading vertical level from ',trim(filename_eta)
          call open_file(IO_TMP,"r",trim(filename_eta),needed=.true.)
          do n=1,10000
             read(IO_TMP,*)word
             if(trim(word)=='vct')exit
          end do
          read(IO_TMP,*)(hyam(k),k=1,KMAX_ext+1)!NB: here = A_bnd, not mid
          read(IO_TMP,*)(hybm(k),k=1,KMAX_ext+1)!NB: here = B_bnd, not mid
          close(IO_TMP)
          !convert to mid levels coefficients
          do k=1,KMAX_ext
            hyam(k)=0.5*(hyam(k)+hyam(k+1))
            hybm(k)=0.5*(hybm(k)+hybm(k+1))
          end do
        else
          status = nf90_inq_varid(ncFileID,"lev",varID)
          if(status == nf90_noerr) then
            call CheckStop('Pressure levels not yet implemented')
            write(*,*)'Nest: assuming pressure levels and hPa'
            call check(nf90_get_var(ncFileID, varID, hyam,count=(/ KMAX_ext /) ))
            hyam=100.0*hyam ! hPa ->Pa
            hybm=0.0
          else
            call CheckStop('Vertical coordinate Unknown/Not yet implemented')
          end if
        end if
      end if
    end if

    call check(nf90_close(ncFileID))
  end if !end MasterProc

  CALL MPI_BCAST(lon_ext,8*GIMAX_ext*GJMAX_ext,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
  CALL MPI_BCAST(lat_ext,8*GIMAX_ext*GJMAX_ext,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
  CALL MPI_BCAST(hyam   ,8*KMAX_ext           ,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
  CALL MPI_BCAST(hybm   ,8*KMAX_ext           ,MPI_BYTE,0,MPI_COMM_CALC,IERROR)

  ! find horizontal interpolation constants
  ! note that i,j are local and but IIij,JJij refer to the full nest-file
  if(native_grid)then   ! nest-file is on the model/run grid
    forall(i=1:limax,j=1:ljmax)
      IIij(:,i,j)=i_fdom(i)-RUNDOMAIN(1)+1
      JJij(:,i,j)=j_fdom(j)-RUNDOMAIN(3)+1
      Weight(:,i,j)=[1.0,0.0,0.0,0.0]
    endforall
    i=IIij(1,limax,ljmax);j=JJij(1,limax,ljmax)
    call CheckStop((i>GIMAX_ext).or.(j>GJMAX_ext),&
                  'Nest: domain mismatch for native_grid')
  else                  ! find the four closest points
    call grid2grid_coeff(glon,glat,IIij,JJij,Weight,lon_ext,lat_ext,&
      GIMAX_ext,GJMAX_ext,LIMAX,LJMAX,limax,ljmax,mydebug,1,1)
  end if
  deallocate(lon_ext,lat_ext)

  !find vertical interpolation coefficients
  !use pressure as reference
  !we want, if possible, P_ext(k1) and P_ext(k2) to be on each side of P_emep
  !We assume constant surface pressure, both for emep and external grid; should not be so
  !   important as long as they are both terrain following.
  do k_ext=1,KMAX_EXT
    P_ext(k_ext)=hyam(k_ext)+hybm(k_ext)*Pref
    if(mydebug) write(*,fmt="(A,I3,F10.2)")'Nest: P_ext',k_ext,P_ext(k_ext)
  end do
  reversed_k_BC=(P_ext(1)>P_ext(2))
  ! .true.  --> assumes k_ext=KMAX_EXT is top and k_ext=1 is surface
  ! .false. --> assumes k_ext=1 is top and k_ext=KMAX_EXT is surface

  if(reversed_k_BC)then
    do k=1,KMAX_MID
      P_emep=A_mid(k)+B_mid(k)*Pref !Pa
      if(mydebug) write(*,fmt="(A,I3,F10.2)")'Nest: P_emep',k,P_emep
      !largest available P smaller than P_emep (if possible)
      k1_ext(k)=1 !start at surface, and go up until P_emep
      do k_ext=1,KMAX_EXT
        if(P_ext(k_ext)<P_emep)exit
        k1_ext(k)=k_ext
      end do
      !smallest available P larger than P_emep (if possible)
      k2_ext(k)=KMAX_EXT !start at top, and go down until P_emep
      if(k2_ext(k)==k1_ext(k))k2_ext(k)=KMAX_EXT-1 !avoid k2=k1
      do k_ext=KMAX_EXT,1,-1
         if(P_ext(k_ext)>P_emep)exit
         if(k_ext/=k1_ext(k))k2_ext(k)=k_ext
      end do
      weight_k1(k)=(P_emep-P_ext(k2_ext(k)))/(P_ext(k1_ext(k))-P_ext(k2_ext(k)))
      weight_k2(k)=1.0-weight_k1(k)
      if(mydebug)&
        write(*,fmt="(A,I3,2(A,I2,A,F5.2))")'Nest: level',k,&
          ' is the sum of level ',k1_ext(k),' weight ',weight_k1(k),&
          ' and level ',k2_ext(k),' weight ',weight_k2(k)
    end do

  else
    do k=1,KMAX_MID
      P_emep=A_mid(k)+B_mid(k)*Pref !Pa
      if(mydebug) write(*,fmt="(A,I3,F10.2)")'Nest: P_emep',k,P_emep
      !largest available P smaller than P_emep (if possible)
      k1_ext(k)=KMAX_EXT !start at surface, and go up until P_emep
      do k_ext=KMAX_EXT,1,-1
        if(P_ext(k_ext)<P_emep)exit
        k1_ext(k)=k_ext
      end do
      !smallest available P larger than P_emep (if possible)
      k2_ext(k)=1 !start at top, and go down until P_emep
      if(k2_ext(k)==k1_ext(k))k2_ext(k)=2 !avoid k2=k1
      do k_ext=1,KMAX_EXT
        if(P_ext(k_ext)>P_emep)exit
        if(k_ext/=k1_ext(k))k2_ext(k)=k_ext
      end do
      weight_k1(k)=(P_emep-P_ext(k2_ext(k)))/(P_ext(k1_ext(k))-P_ext(k2_ext(k)))
      weight_k2(k)=1.0-weight_k1(k)
      if(mydebug) &
        write(*,fmt="(A,I3,2(A,I2,A,F5.2))")'Nest: level',k,&
          ' is the sum of level ', k1_ext(k),' weight ',weight_k1(k),&
           ' and level ', k2_ext(k),' weight ',weight_k2(k)
      end do
    end if
  deallocate(P_ext,hyam,hybm)

  if(mydebug) &
    write(*,*)'Nest: finished determination of interpolation parameters'
contains
function get_dimLen(dimName,id,name) result(len)
  character(len=*), dimension(:), intent(in) :: dimName
  integer,          optional,     intent(out):: id
  character(len=*), optional,     intent(out):: name
  integer :: d, dID, len

  do d=1,size(dimName)
    status = nf90_inq_dimid(ncFileID,dimName(d),dID)
    if(status==nf90_noerr)then
      if(present(id))  id=dID
      if(present(name))name=trim(dimName(d))
      call check(nf90_inquire_dimension(ncFileID,dID,len=len),&
        "get_dimLen: "//trim(dimName(d)))
      exit
    end if
  end do
  call CheckStop(status,nf90_noerr,'Nest: '//&
    trim(dimName(1))//'-dimension not found: '//&
    trim(filename_read)//'. Include new name in init_nest')
end function get_dimLen
end subroutine init_nest

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine init_mask_restrict(filename_read,rundomain_ext)

  !find lon and lat of boundaries of grid and build mask_restrict
  integer,intent(inout) ::rundomain_ext(4)
  character(len=*),intent(in) :: filename_read
  integer ::GIMAX_ext,GJMAX_ext
  integer :: ncFileID,varid,status
  integer :: i,j,n
  real, allocatable, dimension(:,:) ::lon_ext,lat_ext
  real, allocatable, dimension(:) :: temp_ll
  character(len=80) ::projection,iDName,jDName
  real, allocatable, dimension(:,:) ::Weight_rstrct,glon_rundom,glat_rundom
  integer, allocatable, dimension(:,:) ::IIij_rstrct,JJij_rstrct
  real, allocatable, dimension(:) ::lon_rstrct,lat_rstrct
  integer :: N_rstrct_BC,n4,N_rstrct_BC_per_proc

  allocate(mask_restrict(limax,ljmax))

  !Read dimensions (global)
  if(me==0)then
     status = nf90_open(trim(filename_read),nf90_nowrite,ncFileID)
     if(status/=nf90_noerr) then
        print *,'init_mask_restrict: not found',trim(filename_read)      
     else
        MASK_SET = .true.
        print *,'init_mask_restrict: reading ',trim(filename_read)

        projection='Unknown'
        status = nf90_get_att(ncFileID,nf90_global,"projection",projection)
        if(status==nf90_noerr) then
           if(projection=='lon_lat')projection='lon lat'
           write(*,*)'Nest: projection: '//trim(projection)
        else
           projection='lon lat'
           write(*,*)'Nest: projection not found for ',&
                trim(filename_read)//', assuming '//trim(projection)
        end if
        !get dimensions id/name/len: include more dimension names, if necessary
        GIMAX_ext=get_dimLen([character(len=12)::"i","lon","longitude","west_east"],name=iDName)
        GJMAX_ext=get_dimLen([character(len=12)::"j","lat","latitude","south_north" ],name=jDName)

        select case(projection)
        case('Stereographic')
           call CheckStop("i",iDName,"Nest: unsuported "//&
                trim(iDName)//" as i-dimension on "//trim(projection)//" projection")
           call CheckStop("j",jDName,"Nest: unsuported "//&
                trim(jDName)//" as j-dimension on "//trim(projection)//" projection")
        case('lon lat')
           if(trim(iDName)=='west_east')then!wrf metdata
              iDName='XLONG'
              write(*,*)'assuming ',trim(iDName)//' as longitude variable'
           endif
           if(trim(jDName)=='south_north')then!wrf metdata
              jDName='XLAT'
              write(*,*)'assuming ',trim(jDName)//' as latitude variable'
           endif
        case default
           !call CheckStop("Nest: unsuported projection "//trim(projection))
           !write(*,*)'GENERAL PROJECTION ',trim(projection)
           call CheckStop("i",iDName,"Nest: unsuported "//&
                trim(iDName)//" as i-dimension on "//trim(projection)//" projection")
           call CheckStop("j",jDName,"Nest: unsuported "//&
                trim(jDName)//" as j-dimension on "//trim(projection)//" projection")
        end select

        write(*,*)'Nest: dimensions inner grid',GIMAX_ext,GJMAX_ext

     end if
  end if

  CALL MPI_BCAST(MASK_SET,1,MPI_LOGICAL,0,MPI_COMM_CALC,IERROR)
  if (.not.MASK_SET)then
     deallocate(mask_restrict)
     return
  endif

  if(me==0)then
     allocate(lon_ext(GIMAX_ext,GJMAX_ext),lat_ext(GIMAX_ext,GJMAX_ext))
     !Read lon lat of the external grid (global)
     if(trim(projection)==trim('lon lat')) then
        if(trim(iDName)=='XLONG')then
           !wrf metdata
           call check(nf90_inq_varid(ncFileID,trim(iDName),varID),"dim:"//trim(iDName))
           call check(nf90_get_var(ncFileID,varID,lon_ext),"get:lon")
           
           call check(nf90_inq_varid(ncFileID,trim(jDName),varID),"dim:"//trim(jDName))
           call check(nf90_get_var(ncFileID,varID,lat_ext),"get:lat")
        else
           call check(nf90_inq_varid(ncFileID,iDName,varID),&
                "Read lon-variable: "//trim(iDName))
           allocate(temp_ll(GIMAX_ext))
           call check(nf90_get_var(ncFileID,varID,temp_ll))
           lon_ext=SPREAD(temp_ll,2,GJMAX_ext)
           deallocate(temp_ll)
           call check(nf90_inq_varid(ncFileID,jDName,varID),&
                "Read lat-variable: "//trim(jDName))
           allocate(temp_ll(GJMAX_ext))
           call check(nf90_get_var(ncFileID,varID,temp_ll))
           lat_ext=SPREAD(temp_ll,1,GIMAX_ext)
           deallocate(temp_ll)
        endif
     else
        call check(nf90_inq_varid(ncFileID,"lon",varID),"dim:lon")
        call check(nf90_get_var(ncFileID,varID,lon_ext),"get:lon")

        call check(nf90_inq_varid(ncFileID,"lat",varID),"dim:lat")
        call check(nf90_get_var(ncFileID,varID,lat_ext),"get:lat")
     end if

     call check(nf90_close(ncFileID))

     !N_rstrct_BC = number of points on boundaries in the inner grid 
     if(rundomain_ext(1)<1)rundomain_ext(1)=1
     if(rundomain_ext(2)<1 .or. rundomain_ext(2)>GIMAX_ext) rundomain_ext(2)=GIMAX_ext
     if(rundomain_ext(3)<1)rundomain_ext(3)=1
     if(rundomain_ext(4)<1 .or. rundomain_ext(4)>GJMAX_ext) rundomain_ext(4)=GJMAX_ext
     N_rstrct_BC=2*(rundomain_ext(2)-rundomain_ext(1)+1)+2*(rundomain_ext(4)-rundomain_ext(3)-1)
     N_rstrct_BC=2*(rundomain_ext(2)-rundomain_ext(1)+1)+2*(rundomain_ext(4)-rundomain_ext(3)-1)
     allocate(lon_rstrct(N_rstrct_BC))
     allocate(lat_rstrct(N_rstrct_BC))

     !take out only boundary cells
     N_rstrct_BC=0
     j=rundomain_ext(3)
     do i=rundomain_ext(1),rundomain_ext(2)
        N_rstrct_BC = N_rstrct_BC + 1
        lon_rstrct(N_rstrct_BC)=lon_ext(i,j)
        lat_rstrct(N_rstrct_BC)=lat_ext(i,j)
     enddo
     j=rundomain_ext(4)
     do i=rundomain_ext(1),rundomain_ext(2)
        N_rstrct_BC = N_rstrct_BC + 1
        lon_rstrct(N_rstrct_BC)=lon_ext(i,j)
        lat_rstrct(N_rstrct_BC)=lat_ext(i,j)
     enddo
     i=rundomain_ext(1)
     do j=rundomain_ext(3)+1,rundomain_ext(4)-1
        N_rstrct_BC = N_rstrct_BC + 1
        lon_rstrct(N_rstrct_BC)=lon_ext(i,j)
        lat_rstrct(N_rstrct_BC)=lat_ext(i,j)
     enddo
     i=rundomain_ext(2)
     do j=rundomain_ext(3)+1,rundomain_ext(4)-1
        N_rstrct_BC = N_rstrct_BC + 1
        lon_rstrct(N_rstrct_BC)=lon_ext(i,j)
        lat_rstrct(N_rstrct_BC)=lat_ext(i,j)
     enddo
     if(N_rstrct_BC/=2*(rundomain_ext(2)-rundomain_ext(1)+1)+2*(rundomain_ext(4)-rundomain_ext(3)-1))then
        write(*,*)'accounting error'
        stop
     endif
     deallocate(lon_ext,lat_ext)
     CALL MPI_BCAST(N_rstrct_BC,1,MPI_INTEGER,0,MPI_COMM_CALC,IERROR)
  else
     CALL MPI_BCAST(N_rstrct_BC,1,MPI_INTEGER,0,MPI_COMM_CALC,IERROR)
     allocate(lon_rstrct(N_rstrct_BC))
     allocate(lat_rstrct(N_rstrct_BC))
  endif

  CALL MPI_BCAST(lon_rstrct,N_rstrct_BC*8,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
  CALL MPI_BCAST(lat_rstrct,N_rstrct_BC*8,MPI_BYTE,0,MPI_COMM_CALC,IERROR)


  allocate(IIij_rstrct(4,N_rstrct_BC))
  allocate(JJij_rstrct(4,N_rstrct_BC))
  allocate(Weight_rstrct(4,N_rstrct_BC))

  !find nearest neighbors of model grid for each lon_rstrct_BC lat_rstrct_BC
  allocate(glon_rundom(GIMAX,GJMAX))
  allocate(glat_rundom(GIMAX,GJMAX))
  glon_rundom=0.0
  glat_rundom=0.0
  do j=1,ljmax
     do i=1,limax
        glon_rundom(gi0+i-1,gj0+j-1)=glon(i,j)
        glat_rundom(gi0+i-1,gj0+j-1)=glat(i,j)
     enddo
  enddo
  CALL MPI_ALLREDUCE(MPI_IN_PLACE, glon_rundom, GIMAX*GJMAX, &
       MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_CALC, IERROR)
  CALL MPI_ALLREDUCE(MPI_IN_PLACE, glat_rundom, GIMAX*GJMAX, &
       MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_CALC, IERROR)

  
  !divide the work among processors
  N_rstrct_BC_per_proc=(N_rstrct_BC+NPROC-1)/NPROC
  
  ! find the four closest points
!  call grid2grid_coeff( &
!       lon_rstrct,lat_rstrct,         &
!       IIij_rstrct,JJij_rstrct,Weight_rstrct,   & ! Returns coordinates of 4 nearest pts and weights
!       glon_rundom,glat_rundom,GIMAX,GJMAX,N_rstrct_BC,1,N_rstrct_BC,1,&
!       .false., 1, 1) !1,1 is just a crude coord, while checking
  IIij_rstrct=0
  JJij_rstrct=0
  Weight_rstrct=0.0
  do n=me*N_rstrct_BC_per_proc+1,min((me+1)*N_rstrct_BC_per_proc,N_rstrct_BC)
     call point2grid_coeff(lon_rstrct(n),lat_rstrct(n),&
          IIij_rstrct(1,n),JJij_rstrct(1,n),Weight_rstrct(1,n),&
          glon_rundom,glat_rundom,GIMAX,GJMAX,.false.)
  enddo
  deallocate(glon_rundom,glat_rundom,lon_rstrct,lat_rstrct)
  CALL MPI_ALLREDUCE(MPI_IN_PLACE, IIij_rstrct, 4*N_rstrct_BC, &
       MPI_INTEGER, MPI_SUM, MPI_COMM_CALC, IERROR)
  CALL MPI_ALLREDUCE(MPI_IN_PLACE, JJij_rstrct, 4*N_rstrct_BC, &
       MPI_INTEGER, MPI_SUM, MPI_COMM_CALC, IERROR)
  CALL MPI_ALLREDUCE(MPI_IN_PLACE, Weight_rstrct, 4*N_rstrct_BC, &
       MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_CALC, IERROR)

  mask_restrict = .false. !default: do not include
  do n=1, N_rstrct_BC
     do n4=1, 4
        i=IIij_rstrct(n4,n)
        j=JJij_rstrct(n4,n)
        if(i>=gi0 .and. i<=gi1 .and. j>=gj0 .and. j<=gj1)then
           if(abs(Weight_rstrct(n4,n))> 1.0E-6)then !contribute little, probably noise
              mask_restrict(i-gi0+1,j-gj0+1)= .true.
           endif
        endif
     enddo
  enddo
  deallocate(IIij_rstrct,JJij_rstrct,Weight_rstrct)


  if(mydebug) &
       write(*,*)'Nest: finished determination of interpolation parameters'
contains
  function get_dimLen(dimName,id,name) result(len)
    character(len=*), dimension(:), intent(in) :: dimName
    integer,          optional,     intent(out):: id
    character(len=*), optional,     intent(out):: name
    integer :: d, dID, len

    do d=1,size(dimName)
       status = nf90_inq_dimid(ncFileID,dimName(d),dID)
       if(status==nf90_noerr)then
          if(present(id))  id=dID
          if(present(name))name=trim(dimName(d))
          call check(nf90_inquire_dimension(ncFileID,dID,len=len),&
               "get_dimLen: "//trim(dimName(d)))
          exit
       end if
    end do
    call CheckStop(status,nf90_noerr,'Nest: '//&
         trim(dimName(1))//'-dimension not found: '//&
         trim(filename_read)//'. Include new name in init_nest_restrict')
  end function get_dimLen
end subroutine init_mask_restrict

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine read_newdata_LATERAL(ndays_indate)
  real(kind=8), intent(in)::ndays_indate
  real, allocatable, dimension(:,:,:) ::data
  integer :: ncFileID,varid,status
  integer :: ndate(4),n,i,j,k,bc
  real    :: unitscale
  logical, save :: first_call=.true.

  ! 4 nearest points from external grid  (horizontal)
  integer, save,allocatable :: IIij(:,:,:),JJij(:,:,:)
  ! weights of the 4 nearest points (horizontal)
  real, save,allocatable :: Weight(:,:,:)

  ! 2 adjacent levels from external grid  (vertical)
  integer, allocatable,save, dimension(:) :: k1_ext,k2_ext
  ! weights of the 2 adjacent levels (vertical)
  real, allocatable,save, dimension(:) :: weight_k1,weight_k2

  integer:: KMAX_BC ! which lvels are interpolated, = KMAX_MID for now
  integer:: timedimID

  ! dimensions of external grid for BC
  integer, save ::GIMAX_ext,GJMAX_ext
  character (len=80) ::units
  real :: scale_factor,add_offset
  logical :: time_exists,divbyroa

  KMAX_BC=KMAX_MID
  if(mydebug)write(*,*)'Nest: read_newdata_LATERAL, first?', first_call
  if(first_call)then
    if(mydebug)write(*,*)'Nest: initializations 2D'
    allocate(IIij(4,LIMAX,LJMAX),JJij(4,LIMAX,LJMAX))
    allocate(Weight(4,LIMAX,LJMAX))
    allocate(k1_ext(KMAX_MID),k2_ext(KMAX_MID))
    allocate(weight_k1(KMAX_MID),weight_k2(KMAX_MID))

    call init_icbc(ndays=ndays_indate)
    if(mydebug)write(*,*)'calling init_nest for '//trim(filename_read_BC)
    call init_nest(ndays_indate,filename_read_BC,native_grid_BC,&
                   IIij,JJij,Weight,k1_ext,k2_ext,weight_k1,weight_k2,&
                   N_ext_BC,KMAX_ext_BC,GIMAX_ext,GJMAX_ext)
    if(MODE_READ=='MONTH'.and.N_ext_BC/=12.and.MasterProc)then
      write(*,*)'Nest: WARNING: Expected 12 months in BC file, found ',N_ext_BC
      call CheckStop('Nest BC: wrong number of months')
    end if

    ! Define & allocate West/East/South/North Boundaries
    iw=li0-1;ie=li1+1   ! i West/East   boundaries
    js=lj0-1;jn=lj1+1   ! j South/North boundaries
    kt=0;if(TOP_BC)kt=1 ! k Top         boundary
    if(mydebug)then
      if(kt==1)then
        write(*,*)'Nest-kt test: Also including the top layer in BC'
      else
        write(*,*)'Nest-kt test: Not resetting the top layer'
      end if
    end if

    if(iw>=1    .and..not.allocated(xn_adv_bndw)) &
      allocate(xn_adv_bndw(NSPEC_ADV,LJMAX,KMAX_MID,2)) ! West
    if(ie<=limax.and..not.allocated(xn_adv_bnde)) &
      allocate(xn_adv_bnde(NSPEC_ADV,LJMAX,KMAX_MID,2)) ! East
    if(js>=1    .and..not.allocated(xn_adv_bnds)) &
      allocate(xn_adv_bnds(NSPEC_ADV,LIMAX,KMAX_MID,2)) ! South
    if(jn<=ljmax.and..not.allocated(xn_adv_bndn)) &
      allocate(xn_adv_bndn(NSPEC_ADV,LIMAX,KMAX_MID,2)) ! North
    if(kt>=1    .and..not.allocated(xn_adv_bndt)) &
      allocate(xn_adv_bndt(NSPEC_ADV,LIMAX,LJMAX,2)) ! Top
    if(DEBUG_ICBC)then
      CALL MPI_BARRIER(MPI_COMM_CALC, IERROR)
      if(MasterProc) write(*, "(A)") "Nest: DEBUG_ICBC Boundaries:"
      write(*,"(1X,'me=',i3,5(1X,A,I0,'=',L1))")&
        me,'W:i',iw,allocated(xn_adv_bndw),'E:i',ie,allocated(xn_adv_bnde),&
           'S:j',js,allocated(xn_adv_bnds),'N:j',jn,allocated(xn_adv_bndn),&
           'T:k',kt,allocated(xn_adv_bndt)
      if(MasterProc)flush(6)
      CALL MPI_BARRIER(MPI_COMM_CALC, IERROR)
    end if
    rtime_saved(2)=-99.0!just to put a value
    if(mydebug)write(*,*)'Nest: end initializations 2D'

  end if

  rtime_saved(1)=rtime_saved(2) ! put old values in 1
  allocate(data(GIMAX_ext,GJMAX_ext,KMAX_ext_BC), stat=status)
  if(MasterProc)then
    call check(nf90_open(trim(fileName_read_BC),nf90_nowrite,ncFileID))
    status = nf90_inq_dimid(ncFileID,"time",timeDimID)
    time_exists=(status==nf90_noerr)
    if(time_exists) then
      call check(nf90_inquire_dimension(ncFileID,timedimID,len=N_ext_BC))
    else
      status = nf90_inq_dimid(ncFileID,"Months",timeDimID)
      if(status==nf90_noerr)then
        call check(nf90_inquire_dimension(ncFileID,timedimID,len=N_ext_BC))
        call CheckStop(N_ext_BC,12,'Nest BC: did not find 12 monthes')
      else
        N_ext_BC=1
     end if
    end if

    if(size(ndays_ext)<N_ext_BC)then
      write(*,*)'Nest: New size times in BC file ',N_ext_BC
      deallocate(ndays_ext)
      allocate(ndays_ext(N_ext_BC))
    end if

    if(MODE_READ=='MONTH')then
      ! only care of the month
      call nctime2date(ndate,ndays_indate,'Nest: BC reading record MM')
      n=ndate(2)
    else
      if(time_exists)then
        call ReadTimeCDF(filename_read_BC,ndays_ext,N_ext_BC)
        do n=1,N_ext_BC
          if(ndays_indate-ndays_ext(n)<halfsecond) goto 876
        end do
        n=N_ext_BC
        write(*,*)'Nest: WARNING: did not find correct date ',n
876     continue
      else
        ! cannot read time on file. assume it is corresponds to date_nextfile
        n=1
        call date2nctime(date_nextfile,ndays_ext(n))
      end if
    end if

    call nctime2date(ndate,ndays_ext(n),'Nest: Reading date YYYY-MM-DD hh:mm:ss')
    if(mydebug) write(*,*)'Nest: Record ',n,' of ',N_ext_BC
    itime=n
    rtime_saved(2)=ndays_ext(n)
    if(n==N_ext_BC)then
      ! next data to be read should be from another file
      if(mydebug)then
        write(*,*)'Nest: Last record reached ',n,N_ext_BC
        call nctime2date(date_nextfile,ndays_ext(n)+NHOURS_Stride_BC/24.0,&
              'next BC date to read:  YYYY-MM-DD hh:mm:ss')
        write(*,*)'Nest: date_nextfile ',date_nextfile
      else
        call nctime2date(date_nextfile,ndays_ext(n)+NHOURS_Stride_BC/24.0)
      end if
    end if

  end if

  CALL MPI_BCAST(rtime_saved,8*2,MPI_BYTE,0,MPI_COMM_CALC,IERROR)

  if(.not.first_call) call store_old_bc() ! store the old values in 1
  if(allocated(xn_adv_bndw)) xn_adv_bndw(:,:,:,2)=0.0
  if(allocated(xn_adv_bnde)) xn_adv_bnde(:,:,:,2)=0.0
  if(allocated(xn_adv_bnds)) xn_adv_bnds(:,:,:,2)=0.0
  if(allocated(xn_adv_bndn)) xn_adv_bndn(:,:,:,2)=0.0
  if(allocated(xn_adv_bndt)) xn_adv_bndt(:,:,:,2)=0.0

  DO_BC: do bc=1,size(adv_bc)
    if(.not.(adv_bc(bc)%wanted.and.adv_bc(bc)%found))cycle DO_BC
    n=adv_bc(bc)%ixadv
    if(MasterProc)then
      if(DEBUG_NEST.or.DEBUG_ICBC) write(*,"(2(A,1X),I0,'-->',I0)")&
        'Nest: DO_BC',trim(adv_bc(bc)%varname),bc,n
      ! Could fetch one level at a time if sizes becomes too big
      call check(nf90_inq_varid(ncFileID,trim(adv_bc(bc)%varname),varID))

      call check(nf90_get_var(ncFileID, varID, data &
            ,start=(/ 1,1,1,itime /),count=(/ GIMAX_ext,GJMAX_ext,KMAX_ext_BC,1 /) ))
      status = nf90_get_att(ncFileID,VarID,"scale_factor",scale_factor)
      if(status==nf90_noerr) data=data*scale_factor
      status = nf90_get_att(ncFileID,VarID,"add_offset",add_offset)
      if(status==nf90_noerr) data=data+add_offset
      status = nf90_get_att(ncFileID,VarID,"units",units)
      if(units=="1")then
        if(index(adv_bc(bc)%varname,"vmr")>0)units="vmr"
        if(index(adv_bc(bc)%varname,"mmr")>0)units="mmr"
      end if
      if(status==nf90_noerr) then
        if(DEBUG_NEST.or.DEBUG_ICBC) write(*,*)&
          'Nest: variable '//trim(adv_bc(bc)%varname)//' has unit '//trim(units)
        call Units_Scale(units,n,unitscale,needroa=divbyroa,&
                         debug_msg="read_newdata_LATERAL")
        unitscale=adv_bc(bc)%frac/unitscale
      else
        if(DEBUG_NEST.or.DEBUG_ICBC) write(*,*)&
          'Nest: variable '//trim(adv_bc(bc)%varname//' has no unit attribute')
        unitscale=adv_bc(bc)%frac
      end if
      if(unitscale/=1.0) data=data*unitscale
    end if
    CALL MPI_BCAST(data,8*GIMAX_ext*GJMAX_ext*KMAX_ext_BC,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
    CALL MPI_BCAST(divbyroa,1,MPI_LOGICAL,0,MPI_COMM_CALC,IERROR)

    ! overwrite Global Boundaries (lateral faces)
    if(divbyroa)then
      if(allocated(xn_adv_bndw)) forall(k=1:KMAX_BC, j=1:ljmax) &
        xn_adv_bndw(n,j,k,2)=xn_adv_bndw(n,j,k,2) &
                            +(WeightData(iw,j,k1_ext(k))*weight_k1(k) &
                             +WeightData(iw,j,k2_ext(k))*weight_k2(k))&
                            /roa(iw,j,k,1)
      if(allocated(xn_adv_bnde)) forall(k=1:KMAX_BC, j=1:ljmax) &
        xn_adv_bnde(n,j,k,2)=xn_adv_bnde(n,j,k,2) &
                            +(WeightData(ie,j,k1_ext(k))*weight_k1(k) &
                             +WeightData(ie,j,k2_ext(k))*weight_k2(k))&
                            /roa(ie,j,k,1)
      if(allocated(xn_adv_bnds)) forall(k=1:KMAX_BC, i=1:limax) &
        xn_adv_bnds(n,i,k,2)=xn_adv_bnds(n,i,k,2) &
                            +(WeightData(i,js,k1_ext(k))*weight_k1(k) &
                             +WeightData(i,js,k2_ext(k))*weight_k2(k))&
                            /roa(i,js,k,1)
      if(allocated(xn_adv_bndn)) forall(k=1:KMAX_BC, i=1:limax) &
        xn_adv_bndn(n,i,k,2)=xn_adv_bndn(n,i,k,2) &
                            +(WeightData(i,jn,k1_ext(k))*weight_k1(k) &
                             +WeightData(i,jn,k2_ext(k))*weight_k2(k))&
                            /roa(i,jn,k,1)
      if(allocated(xn_adv_bndt)) forall(i=1:limax, j=1:ljmax) &
        xn_adv_bndt(n,i,j,2)=xn_adv_bndt(n,i,j,2) &
                            +(WeightData(i,j,k1_ext(kt))*weight_k1(kt) &
                             +WeightData(i,j,k2_ext(kt))*weight_k2(kt))&
                            /roa(i,j,kt,1)
    else
      if(allocated(xn_adv_bndw)) forall(k=1:KMAX_BC, j=1:ljmax) &
        xn_adv_bndw(n,j,k,2)=xn_adv_bndw(n,j,k,2) &
                            +WeightData(iw,j,k1_ext(k))*weight_k1(k)&
                            +WeightData(iw,j,k2_ext(k))*weight_k2(k)
      if(allocated(xn_adv_bnde)) forall(k=1:KMAX_BC, j=1:ljmax) &
        xn_adv_bnde(n,j,k,2)=xn_adv_bnde(n,j,k,2) &
                            +WeightData(ie,j,k1_ext(k))*weight_k1(k)&
                            +WeightData(ie,j,k2_ext(k))*weight_k2(k)
      if(allocated(xn_adv_bnds)) forall(k=1:KMAX_BC, i=1:limax) &
        xn_adv_bnds(n,i,k,2)=xn_adv_bnds(n,i,k,2) &
                            +WeightData(i,js,k1_ext(k))*weight_k1(k)&
                            +WeightData(i,js,k2_ext(k))*weight_k2(k)
      if(allocated(xn_adv_bndn)) forall(k=1:KMAX_BC, i=1:limax) &
        xn_adv_bndn(n,i,k,2)=xn_adv_bndn(n,i,k,2) &
                            +WeightData(i,jn,k1_ext(k))*weight_k1(k)&
                            +WeightData(i,jn,k2_ext(k))*weight_k2(k)
      if(allocated(xn_adv_bndt)) forall(i=1:limax, j=1:ljmax) &
        xn_adv_bndt(n,i,j,2)=xn_adv_bndt(n,i,j,2) &
                            +WeightData(i,j,k1_ext(kt))*weight_k1(kt)&
                            +WeightData(i,j,k2_ext(kt))*weight_k2(kt)
    end if
  end do DO_BC

  if(first_call)then
    ! copy 2 into 1 so that both are well defined
    rtime_saved(1)=rtime_saved(2)!put  time in 1
    call store_old_bc() ! store the old values in 1
  end if

  deallocate(data)
  if(MasterProc) call check(nf90_close(ncFileID))
  first_call=.false.
  return
  contains
  PURE function WeightData(i,j,k) result(wsum)
    integer, intent(in)::i,j,k
    real:: wsum
    wsum=dot_product(Weight(:,i,j),[&
      data(IIij(1,i,j),JJij(1,i,j),k),data(IIij(2,i,j),JJij(2,i,j),k),&
      data(IIij(3,i,j),JJij(3,i,j),k),data(IIij(4,i,j),JJij(4,i,j),k)])
  end function WeightData
  subroutine store_old_bc !store the old values in 1
    if(allocated(xn_adv_bndw)) xn_adv_bndw(:,:,:,1)=xn_adv_bndw(:,:,:,2)
    if(allocated(xn_adv_bnde)) xn_adv_bnde(:,:,:,1)=xn_adv_bnde(:,:,:,2)
    if(allocated(xn_adv_bnds)) xn_adv_bnds(:,:,:,1)=xn_adv_bnds(:,:,:,2)
    if(allocated(xn_adv_bndn)) xn_adv_bndn(:,:,:,1)=xn_adv_bndn(:,:,:,2)
    if(allocated(xn_adv_bndt)) xn_adv_bndt(:,:,:,1)=xn_adv_bndt(:,:,:,2)
  end subroutine store_old_bc
end subroutine read_newdata_LATERAL

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine reset_3D(ndays_indate)
  implicit none
  real(kind=8), intent(in)::ndays_indate
  real, allocatable, dimension(:,:,:) ::data
  integer :: ndate(4),n,i,j,k,itime=0,status
  integer :: ncFileID,varid
  real    :: unitscale
  logical, save :: first_call=.true.

  ! 4 nearest points from external grid
  integer, save,allocatable :: IIij(:,:,:),JJij(:,:,:)

  ! weights of the 4 nearest points
  real, save,allocatable :: Weight(:,:,:)

  ! dimensions of external grid for 3D
  integer, save ::N_ext,KMAX_ext,GIMAX_ext,GJMAX_ext

  ! 2 adjacent levels from external grid  (vertical)
  integer, allocatable,save, dimension(:) :: k1_ext,k2_ext
  ! weights of the 2 adjacent levels (vertical)
  real, allocatable,save, dimension(:) :: weight_k1,weight_k2

  character (len=80) :: units, restricted
  real :: scale_factor,add_offset
  logical :: divbyroa

  if(mydebug) write(*,*) 'Nest: initializations 3D', first_call

  if(first_call)then
    if(mydebug) write(*,*)'Nest: initializations 3D'
    allocate(IIij(4,LIMAX,LJMAX),JJij(4,LIMAX,LJMAX))
    allocate(Weight(4,LIMAX,LJMAX))
    allocate(k1_ext(KMAX_MID),k2_ext(KMAX_MID))
    allocate(weight_k1(KMAX_MID),weight_k2(KMAX_MID))
    first_call=.false.
    if(mydebug) write(*,*) 'Nest: init-icbc'
    call init_icbc(ndays=ndays_indate)
    if(mydebug) write(*,*)'calling init_nest for 3D '//trim(filename_read_3D)
    call init_nest(ndays_indate,filename_read_3D,native_grid_3D,&
                  IIij,JJij,Weight,k1_ext,k2_ext,weight_k1,weight_k2,&
                  N_ext,KMAX_ext,GIMAX_ext,GJMAX_ext)
    if(MODE_READ=='MONTH'.and.(N_ext/=12.and.N_ext/=1.and.MasterProc))then
      write(*,*)'Nest: WARNING: Expected 12 or 1 monthes in IC file, found ',N_ext
      call CheckStop('Nest: IC: wrong number of months')
    end if
    if(mydebug) write(*,*)'Nest: end initializations 3D'
  end if

  !check that the file is defined in 3D, i.e. not restricted to BC data
  call check(nf90_open(trim(fileName_read_3D),nf90_nowrite,ncFileID))
  status = nf90_get_att(ncFileID,nf90_global,"restricted",restricted)
  call check(nf90_close(ncFileID))
  
  if(status/=nf90_noerr .or. trim(restricted)/="BC_restricted") then     
     
     allocate(data(GIMAX_ext,GJMAX_ext,KMAX_ext), stat=status)
     if(MasterProc)then
        call check(nf90_open(trim(fileName_read_3D),nf90_nowrite,ncFileID))
        if(MODE_READ=='MONTH')then
           if(N_ext==1)then
              n=1
           else
              call nctime2date(ndate,ndays_indate,'Using record MM')
              n=ndate(2)
           end if
        else
           do n=1,N_ext
              if(ndays_ext(n)>=ndays_indate) goto 876
           end do
           n=N_ext
           write(*,*)'Nest: WARNING: did not find correct date'
876        continue
           call nctime2date(ndate,ndays_ext(n),'Using date YYYY-MM-DD hh:mm:ss')
        end if
        itime=n
     end if
     
     if(mydebug)write(*,*)'Nest: overwrite 3D'
     
     DO_SPEC: do n= 1, NSPEC_ADV
        if(.not.(adv_ic(n)%wanted.and.adv_ic(n)%found)) cycle DO_SPEC
        if(MasterProc)then
           if(DEBUG_NEST) print *,'Nest: 3D component ',trim(adv_ic(n)%varname)
           ! Could fetch one level at a time if sizes becomes too big
           call check(nf90_inq_varid(ncFileID,trim(adv_ic(n)%varname),varID))
           call check(nf90_get_var(ncFileID, varID, data &
                ,start=(/ 1,1,1,itime /),count=(/ GIMAX_ext,GJMAX_ext,KMAX_ext,1 /) ))
           status = nf90_get_att(ncFileID,VarID,"scale_factor",scale_factor)
           if(status==nf90_noerr) data=data*scale_factor
           status = nf90_get_att(ncFileID,VarID,"add_offset",add_offset)
           if(status==nf90_noerr) data=data+add_offset
           status = nf90_get_att(ncFileID,VarID,"units",units)
           if(units=="1")then
              if(index(adv_ic(n)%varname,"vmr")>0)units="vmr"
              if(index(adv_ic(n)%varname,"mmr")>0)units="mmr"
           end if
           if(status==nf90_noerr) then
              if(DEBUG_NEST) write(*,*)&
                   'Nest: variable '//trim(adv_ic(n)%varname)//' has unit '//trim(units)
              call Units_Scale(units,n,unitscale,needroa=divbyroa,&
                   debug_msg="reset_3D")
              unitscale=adv_ic(n)%frac/unitscale
           else
              if(DEBUG_NEST) write(*,*)&
                   'Nest: variable '//trim(adv_ic(n)%varname//' has no unit attribute')
              unitscale=adv_ic(n)%frac
           end if
           if(unitscale/=1.0) data=data*unitscale
        end if
        CALL MPI_BCAST(data,8*GIMAX_ext*GJMAX_ext*KMAX_ext,MPI_BYTE,0,MPI_COMM_CALC,IERROR)
        CALL MPI_BCAST(divbyroa,1,MPI_LOGICAL,0,MPI_COMM_CALC,IERROR)
        
        ! overwrite everything 3D (init)
        if(divbyroa)then
           forall (k=1:KMAX_MID, j=1:ljmax, i=1:limax) &
                xn_adv(n,i,j,k)=(WeightData(i,j,k1_ext(k))*weight_k1(k) &
                +WeightData(i,j,k2_ext(k))*weight_k2(k))&
                /roa(i,j,k,1)
        else
           forall (k=1:KMAX_MID, j=1:ljmax, i=1:limax) &
                xn_adv(n,i,j,k)=WeightData(i,j,k1_ext(k))*weight_k1(k)&
                +WeightData(i,j,k2_ext(k))*weight_k2(k)
        end if
        
     end do DO_SPEC

     deallocate(data)
     if(MasterProc) call check(nf90_close(ncFileID))
  else
     if(me==0)write(*,*)'WARNING: did not reset 3D, because only BC data in '//trim(filename_read_3D)
  endif

  contains
  PURE function WeightData(i,j,k) result(wsum)
    integer, intent(in)::i,j,k
    real:: wsum
    wsum=dot_product(Weight(:,i,j),[&
      data(IIij(1,i,j),JJij(1,i,j),k),data(IIij(2,i,j),JJij(2,i,j),k),&
      data(IIij(3,i,j),JJij(3,i,j),k),data(IIij(4,i,j),JJij(4,i,j),k)])
  end function WeightData
end subroutine reset_3D

endmodule Nest_mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
