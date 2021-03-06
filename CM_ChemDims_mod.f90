! Generated by GenChem.py - DO NOT EDIT
! scheme(s)  EmChem16x ShipNOx BVOC_EmChem16x Aqueous_EmChem16x Aero2017nx VBS_EmChem16x FFireInert SeaSalt DustExtended Ash_EmChem16x
module ChemDims_mod

  implicit none
  character(len=*),parameter, public :: CM_schemes_ChemDims = " EmChem16x ShipNOx BVOC_EmChem16x Aqueous_EmChem16x Aero2017nx VBS_EmChem16x FFireInert SeaSalt DustExtended Ash_EmChem16x"
  

    ! NSPEC for TOT : All reacting species
    integer, public, parameter :: NSPEC_TOT=128
    
    ! NSPEC for ADV : Advected species
    integer, public, parameter :: NSPEC_ADV=111
    
    ! NSPEC for SHL : Short-lived (non-advected) species
    integer, public, parameter :: NSPEC_SHL=17
    
    ! NSPEC for SEMIVOL : Semi-volatile organic aerosols
    integer, public, parameter :: NSPEC_SEMIVOL=22
        
    ! No. DRY deposition species
    integer, public, parameter :: NDRYDEP_ADV = 68
    
    ! No. WET deposition species
    integer, public, parameter :: NWETDEP_ADV = 59
    
    ! No. rate coefficients
    integer, parameter, public :: NCHEMRATES = 117
    
    ! No. photolysis rates used
    integer, parameter, public :: NPHOTOLRATES = 14
    
    ! No. emission Files
    integer, parameter, public :: NEMIS_File = 7
    
    ! No. emission Specss
    integer, parameter, public :: NEMIS_Specs = 44

end module ChemDims_mod
