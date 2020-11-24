module PrescribeZ0Mod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! CLM Satelitte Phenology model (SP) ecosystem dynamics (phenology, vegetation). 
  ! Allow some subroutines to be used by the CLM Carbon Nitrogen model (CLMCN) 
  ! so that DryDeposition code can get estimates of LAI differences between months.
  !
  ! !USES:
  use shr_strdata_mod , only : shr_strdata_type, shr_strdata_create
  use shr_strdata_mod , only : shr_strdata_print, shr_strdata_advance
  use shr_kind_mod    , only : r8 => shr_kind_r8
  use shr_kind_mod    , only : CL => shr_kind_CL
  use shr_log_mod     , only : errMsg => shr_log_errMsg
  use decompMod       , only : bounds_type
  use abortutils      , only : endrun
  use clm_varctl      , only : scmlat,scmlon,single_column
  use clm_varctl      , only : iulog, use_lai_streams
  use clm_varcon      , only : grlnd
  use decompMod       , only : gsmap_lnd_gdc2glo
  use domainMod       , only : ldomain
  use fileutils       , only : getavu, relavu
  use PatchType       , only : patch                
  use CanopyStateType , only : canopystate_type
  use WaterDiagnosticBulkType  , only : waterdiagnosticbulk_type
  use perf_mod        , only : t_startf, t_stopf
  use spmdMod         , only : masterproc
  use spmdMod         , only : mpicom, comp_id
  use mct_mod
  use ncdio_pio   
  !
  ! !PUBLIC TYPES:
  implicit none
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: readDailyRoughness     ! Read in daily roughness length from fsurdat file
  !

  ! !PRIVATE MEMBER DATA:
  type(shr_strdata_type) :: sdat_lai           ! LAI input data stream
  !
  ! !PRIVATE TYPES:

  character(len=*), parameter, private :: sourcefile = &
       __FILE__
  !-----------------------------------------------------------------------

contains
   
  !-----------------------------------------------------------------------
  subroutine readDailyRoughness (bounds, &
       fveg, doy, canopystate_inst)
    !
    ! !DESCRIPTION:
    ! Read monthly vegetation data for two consec. months.
    !
    ! !USES:
    use clm_varpar       , only : numpft
    use pftconMod        , only : noveg
    use fileutils        , only : getfil
    use spmdMod          , only : masterproc, mpicom, MPI_REAL8, MPI_INTEGER
    use shr_scam_mod     , only : shr_scam_getCloseLatLon
    use clm_time_manager , only : get_nstep
    use netcdf
    !
    ! !ARGUMENTS:
    type(bounds_type) , intent(in) :: bounds  
    character(len=*)  , intent(in) :: fveg      ! file with monthly vegetation data
    integer           , intent(in) :: doy       ! day of year
    type(canopystate_type), intent(inout) :: canopystate_inst
    !
    ! !LOCAL VARIABLES:
    character(len=256) :: locfn           ! local file name
    type(file_desc_t)  :: ncid            ! netcdf id
    integer :: g,k,l,p                    ! indices
    integer :: dimid,varid                ! input netCDF id's
    integer :: ntim                       ! number of input data time samples
    integer :: nlon_i                     ! number of input data longitudes
    integer :: nlat_i                     ! number of input data latitudes
    integer :: npft_i                     ! number of input data patch types
    integer :: ier                        ! error code
    integer :: closelatidx,closelonidx
    real(r8):: closelat,closelon
    logical :: readvar
    real(r8), pointer :: z0m(:,:)         ! momentum roughness read from input files
    character(len=32) :: subname = 'readDailyRoughness'
    !-----------------------------------------------------------------------
    associate(                                                           &
                                                     
         prescribed_z0m       => canopystate_inst%prescribed_z0m_patch &  ! Output: [real(r8) (:) ] Prescribed surface roughness length for momentum (only used if prescribe_z0 = true) 
         )
    !-----------------------------------------------------------------------

    ! Determine necessary indices

    allocate(z0m(bounds%begg:bounds%endg,0:numpft),  stat=ier)
    if (ier /= 0) then
       write(iulog,*)subname, 'allocation big error '
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if

    call getfil(fveg, locfn, 0)
    call ncd_pio_openfile (ncid, trim(locfn), 0)

    if (single_column) then
       call shr_scam_getCloseLatLon (ncid, scmlat, scmlon, closelat, closelon,&
            closelatidx, closelonidx)
    endif


       call ncd_io(ncid=ncid, varname='Z0MV_pft', flag='read', data=z0m, dim1name=grlnd, &
            nt=doy, readvar=readvar)
       if (.not. readvar) call endrun(msg=' ERROR: ZOMV_pft NOT on fveg file'//errMsg(sourcefile, __LINE__))


       ! Only vegetated patches have nonzero values
       ! Assign lai/sai/hgtt/hgtb to the top [maxpatch_pft] patches
       ! as determined in subroutine surfrd

       do p = bounds%begp,bounds%endp
          g =patch%gridcell(p)
          if (patch%itype(p) /= noveg) then     ! vegetated pft
             do l = 0, numpft
                if (l == patch%itype(p)) then
                   prescribed_z0m(p) = z0m(g,l)
                end if
             end do
          else                        ! non-vegetated pft
             prescribed_z0m(p) = 0._r8
          end if
       end do   ! end of loop over patches


    call ncd_pio_closefile(ncid)

    if (masterproc) then
       write(iulog,*) 'Successfully read daily roughness data for'
       write(iulog,*) 'day ', doy
       write(iulog,*)
    end if

    deallocate(z0m)


    end associate
    
  end subroutine readDailyRoughness 

end module PrescribeZ0Mod
