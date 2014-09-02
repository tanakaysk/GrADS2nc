program ncmake_CF16

  use netcdf
  use cnst,  only: i4, r4
  use nclib, only: CFnclib__dimensionAttribute, CFnclib__dimensionPut, &
  &     CFnclib__varAttr, CFnclib__varAttr_range, CFnclib__varPut, nclib__errMssg

  implicit none

  integer(i4), parameter:: unml = 16 ! unit number for namelist.ncmake
  integer(i4), parameter:: udim = 20 ! unit number for dimension value file 
  integer(i4), parameter:: uvar = 21 ! unit number for variable value file
  character(len = 15), parameter:: file_nml = 'namelist.ncmake'

  type dimCF16
    character(len = 20):: name
    integer(i4)        :: dimID
    integer(i4)        :: varID
    integer(i4)        :: dlen
  end type dimCF16

  integer(i4)               :: ncid         ! netCDF ID
  integer(i4)               :: varID        ! variable ID
  integer(i4),   allocatable:: vshape(:)    ! shape of variables
  integer(i4)               :: shp1(1), shp2(2), shp3(3), shp4(4)
  integer(i4)               :: n            ! loop variable
  integer(i4)               :: ierr, nerr
  real(r4),      allocatable:: vdim(:)      ! dimension value
  real(r4),      allocatable:: variable0(:) ! variable
  logical                   :: ldef         ! defined mode flag
  type(dimCF16), allocatable:: dimIfs(:)    ! coordinate information

  ! &nml_files
  character(len = 200):: file_nc   ! name of the output netCDF file
  logical             :: flg_64bit ! 64-bit offset format flag
  ! &nml_general
  integer(i4)         :: ncor   ! number of coordinates
  integer(i4)         :: nvar   ! number of variables
  character(len = 100):: title  ! 'title' attribute
  character(len = 200):: source ! 'source' attribute
  ! &nml_coordinate and &nml_variable
  character(len = 200):: standard_name ! 'standard_name' attribute
  character(len = 200):: long_name     ! 'long_name' attribute
  character(len = 100):: units         ! 'units' attribute
  ! &nml_coordinate
  integer(i4)         :: dimLength ! length of the dimension
  character(len = 20) :: dimName   ! name of the dimension
  character(len = 10) :: dimKind   ! kind of the dimension variable
  character(len = 1)  :: axis      ! 'axis' attribute of the dimension
  character(len = 4)  :: positive  ! 'positive' attribute of the dimension
  character(len = 20) :: calendar  ! 'calendar' attibute of time dimension
  character(len = 20) :: dateRef   ! reference date of the time axis
  character(len = 200):: fileCord  ! name of dimension file
  ! &nml_variable
  integer(i4)         :: ndim         ! number of dimensions
  character(len = 20) :: varName      ! name of the variable
  character(len = 10) :: varKind      ! kind of the variable
  character(len = 20) :: dimList(20)  ! list of the dimension name
  real(r4)            :: undef(2)     ! input/output undefined values
  real(r4)            :: scale_factor ! 'scale_factor' attribute of the variable
  real(r4)            :: add_offset   ! 'add_offset' attribute of the variable
  integer(i4)         :: nrec(3)      ! record number and interval in the source file
  character(len = 200):: fileVar      ! name of the source file of the variable


  namelist /nml_files/   file_nc, flg_64bit
  namelist /nml_general/ ncor, nvar, title, source

  ! read namelist
  flg_64bit = .false.
  open(unml, file = file_nml, form = 'formatted', status = 'old', action = 'read', iostat = ierr)
  if ( ierr /= 0 ) then
    print '(a)', 'Error: cannot open ' // file_nml
    stop
  endif
  read(unml, nml = nml_files)
  read(unml, nml = nml_general)

  ! open netCDF file
  if ( flg_64bit ) then
    nerr = nf90_create( trim( file_nc ), cmode = or( NF90_NOCLOBBER, NF90_64BIT_OFFSET ), ncid = ncid )
  else
    nerr = nf90_create( trim( file_nc ), NF90_NOCLOBBER, ncid )
  endif
  call nclib__errMssg( nerr, 'NF90_CREATE', ierr )
  if ( ierr == 0 ) then
    ldef = .true.
    print '(a)', 'Output to ' // trim( file_nc )
  else
    stop
  endif

  ! put global attributes
  nerr = nf90_put_att( ncid, NF90_GLOBAL, 'Conventions', 'CF-1.6' )
  call nclib__errMssg( nerr, 'NF90_PUT_ATT (Conventions)', ierr )
  nerr = nf90_put_att( ncid, NF90_GLOBAL, 'title', trim( title ) )
  call nclib__errMssg( nerr, 'NF90_PUT_ATT (title)', ierr )
  nerr = nf90_put_att( ncid, NF90_GLOBAL, 'source', trim( source ) )
  call nclib__errMssg( nerr, 'NF90_PUT_ATT (source)', ierr )

  ! define and put dimensions and coordinate variables
  allocate(dimIfs(ncor))
  do n = 1, ncor
   ! change to define mode
    if ( .not. ldef ) then
      nerr = nf90_redef( ncid )
      call nclib__errMssg( nerr, 'NF90_REDEF', ierr )
      if ( ierr == 0 ) ldef = .true.
    endif

    ! read namelist
    call readnml_coordinate()
    dimIfs(n)%name = trim( dimName )
    dimIfs(n)%dlen = dimLength
 
    if ( trim( standard_name ) == '@@@@' ) then
      if ( trim( long_name ) == '@@@@' ) then
        print '(a)', 'Error: neither long_name nor standard_name is specified. '
        stop
      else
        if ( trim( units ) == '@@@@' ) then
          call CFnclib__dimensionAttribute( ncid, trim( dimName ), dimLength, &
          &  dimIfs(n)%dimID, dimIfs(n)%varID, trim( dimKind ), &
          &  long_name = trim( long_name ), &
          &  axis = axis, positive = trim( positive ), &
          &  calendar = trim( calendar ), dateRef = dateRef )
        else
          call CFnclib__dimensionAttribute( ncid, trim( dimName ), dimLength, &
          &  dimIfs(n)%dimID, dimIfs(n)%varID, trim( dimKind ), &
          &  long_name = trim( long_name ), &
          &  units = trim( units ), axis = axis, positive = trim( positive ), &
          &  calendar = trim( calendar ), dateRef = dateRef )
        endif
      endif
    else
      if ( trim( long_name ) == '@@@@' ) then
        if ( trim( units ) == '@@@@' ) then
          call CFnclib__dimensionAttribute( ncid, trim( dimName ), dimLength, &
          &  dimIfs(n)%dimID, dimIfs(n)%varID, trim( dimKind ), &
          &  standard_name = trim( standard_name ), &
          &  axis = axis, positive = trim( positive ), &
          &  calendar = trim( calendar ), dateRef = dateRef )
        else
          call CFnclib__dimensionAttribute( ncid, trim( dimName ), dimLength, &
          &  dimIfs(n)%dimID, dimIfs(n)%varID, trim( dimKind ), &
          &  standard_name = trim( standard_name ), &
          &  units = trim( units ), axis = axis, positive = trim( positive ), &
          &  calendar = trim( calendar ), dateRef = dateRef )
        endif
      else
        if ( trim( units ) == '@@@@' ) then
          call CFnclib__dimensionAttribute( ncid, trim( dimName ), dimLength, &
          &  dimIfs(n)%dimID, dimIfs(n)%varID, trim( dimKind ), &
          &  standard_name = trim( standard_name ), long_name = trim( long_name ), &
          &  axis = axis, positive = trim( positive ), &
          &  calendar = trim( calendar ), dateRef = dateRef )
        else
          call CFnclib__dimensionAttribute( ncid, trim( dimName ), dimLength, &
          &  dimIfs(n)%dimID, dimIfs(n)%varID, trim( dimKind ), &
          &  standard_name = trim( standard_name ), long_name = trim( long_name ), &
          &  units = trim( units ), axis = axis, positive = trim( positive ), &
          &  calendar = trim( calendar ), dateRef = dateRef )
        endif
      endif
    endif

    nerr = nf90_enddef( ncid )
    call nclib__errMssg( nerr, 'NF90_ENDDEF', ierr )
    if ( ierr /= 0 ) stop
    if ( ierr == 0 ) ldef = .false.

    ! read dimension values
    allocate(vdim(dimLength))
    call readvalue_coordinate( trim( fileCord ), vdim )

    ! write dimension variables
    call CFnclib__dimensionPut( ncid, dimIfs(n)%varID, vdim )
    deallocate(vdim)
  enddo

  ! define and put variables
  do n = 1, nvar
    ! change to define mode
    if ( .not. ldef ) then
      nerr = nf90_redef( ncid )
      call nclib__errMssg( nerr, 'NF90_REDEF', ierr )
      if ( ierr == 0 ) ldef = .true.
    endif

    ! read namelist
    call readnml_variable( )
    allocate(vshape(ndim))

    ! set attribute
    call CFnclib__varAttr( ncid, dimIfs(:)%name, dimIfs(:)%dimID, dimIfs(:)%dlen, &
    &  trim( varName ), ndim, trim( varKind ), dimList, &
    &  trim( standard_name ), trim( long_name ), trim( units ), varID, vshape )
    call CFnclib__varAttr_range( ncid, varID, undef(2), &
    &  scale_factor = scale_factor, add_offset = add_offset )

    nerr = nf90_enddef( ncid )
    call nclib__errMssg( nerr, 'NF90_ENDDEF', ierr )
    if ( ierr /= 0 ) stop
    if ( ierr == 0 ) ldef = .false.

    ! read variable values
    allocate(variable0(product( vshape(1:ndim) )))
    call readvalue_variable( )

    ! put value in netCDF file
    select case ( ndim )
    case ( 1 )
      shp1 = vshape
      call CFnclib__varPut( ncid, varID, trim( varName ), reshape( variable0, shp1 ) )
    case ( 2 )
      shp2 = vshape
      call CFnclib__varPut( ncid, varID, trim( varName ), reshape( variable0, shp2 ) )
    case ( 3 )
      shp3 = vshape
      call CFnclib__varPut( ncid, varID, trim( varName ), reshape( variable0, shp3 ) )
    case ( 4 )
      shp4 = vshape
      call CFnclib__varPut( ncid, varID, trim( varName ), reshape( variable0, shp4 ) )
    case default
      print '(a)', 'Error: variable dimension exceeds 4.'
      stop
    end select
    deallocate(variable0)
  enddo

  ! close netCDF file
  nerr = nf90_close( ncid )
  call nclib__errMssg( nerr, 'NF90_CLOSE', ierr )
  if ( ierr /= 0 ) stop
  close(unml)


  contains

    subroutine readnml_coordinate( )
      namelist /nml_coordinate/ dimName, dimLength, dimKind, standard_name, long_name, &
      &                         axis, positive, units, calendar, dateRef, fileCord

      ! initial value
      standard_name = '@@@@'
      long_name     = '@@@@'
      axis          = '@'
      positive      = '@@@@'
      units         = '@@@@'
      calendar      = 'gregorian'
      dateRef       = '00000000000000000000'

      read(unml, nml = nml_coordinate)
    end subroutine readnml_coordinate


    subroutine readnml_variable( )
      namelist /nml_variable/ varName, ndim, varKind, dimList, standard_name, long_name, &
      &                       units, undef, scale_factor, add_offset, fileVar, nrec

      ! initial value for namelist variables
      long_name     = '@@@@'
      standard_name = '@@@@'
      units         = '@@@@'
      scale_factor  = 0.e0
      add_offset    = 0.e0
      undef         = 10.e0
      nrec          = (/ -1, -1, -1 /)

      ! read namelist and check
      read(unml, nml = nml_variable)
      if ( undef(2) == 10.e0 ) then
        if ( undef(1) == 10.e0 ) then
          print '(a)', 'Error: undefined value is not specified. '
          stop
        else
          undef(2) = undef(1)
        endif
      endif
      
    end subroutine readnml_variable


    subroutine readvalue_coordinate( fileIn, array )
      character(len = *), intent(in) :: fileIn   ! name of input file
      real(r4),           intent(out):: array(:) ! output array

      integer(i4):: dimlen ! length of the array
      integer(i4):: n      ! loop variable
      integer(i4):: ierr

      dimlen = size( array )

      open(udim, file = fileIn, form = 'formatted', status = 'old', action = 'read', iostat = ierr)
      if ( ierr /= 0 ) then
        print '(a)', 'Error: cannot open dimension value file. ' // fileIn
        stop
      endif
      if ( dimlen >= 10 ) then
        do n = 1, dimlen / 10
          read(udim, *) array(10*(n-1)+1:10*n)
        enddo
      endif
      if ( mod( dimlen, 10 ) > 0 ) then
        read(udim, *) array(dimlen/10*10+1:dimlen)
      endif
      close(udim)

    end subroutine readvalue_coordinate


    subroutine readvalue_variable( )
      integer(i4):: lrecvar ! record length of the variable
      integer(i4):: nt, nr  ! loop variables
      integer(i4):: idxi, idxe, numrec
      integer(i4):: ierr

      if ( nrec(1) == nrec(2) .or. nrec(2) == -1 ) then
        ! depth-independent variable
        lrecvar = product( vshape(1:ndim-1) )
        open(uvar, file = trim( fileVar ), form = 'unformatted', status = 'old', action = 'read', &
        &    access = 'direct', recl = 4*lrecvar, iostat = ierr)
        if ( ierr /= 0 ) then
          print '(a)', 'Error: cannot open variable value file. ' // trim( fileVar )
          stop
        endif

        lp_time_1: do nt = 1, vshape(ndim)
          idxi = lrecvar * (nt - 1) + 1
          idxe = idxi + lrecvar - 1
          numrec = nrec(3) * (nt - 1) + nrec(1)
          read(uvar, rec = numrec) variable0(idxi:idxe)
#ifdef DEBUG2
          print '(a, i6, a)', '  from ' // trim( fileVar ) // ' (rec=', numrec, ')'
#endif /* DEBUG2 */
        enddo lp_time_1
      else
        ! depth-dependent variable
        lrecvar = 4 * product( vshape(1:ndim-2) )
        open(uvar, file = trim( fileVar ), form = 'unformatted', status = 'old', action = 'read', &
        &    access = 'direct', recl = 4*lrecvar, iostat = ierr)
        if ( ierr /= 0 ) then
          print '(a)', 'Error: cannot open variable value file. ' // trim( fileVar )
          stop
        endif

        lp_time_2: do nt = 1, vshape(ndim)
          do nr = nrec(1), nrec(2)
            idxi = lrecvar * (nr - nrec(1)) + lrecvar * (nt - 1) + 1
            idxe = idxi + lrecvar - 1
            numrec = nrec(3) * (nt - 1) + nr
            read(uvar, rec = numrec) variable0(idxi:idxe)
          enddo
#ifdef DEBUG2
          print '(a, 2(i6, a))', '  from ' // trim( fileVar ) // ' (rec=', &
          & numrec - (nrec(2) - nrec(1)), '-', numrec, ')'
#endif /* DEBUG2 */
        enddo lp_time_2
      endif
      close(uvar)

      ! change undefined value
      if ( undef(1) /= undef(2) ) then
        where ( variable0 == undef(1) ) variable0 = undef(2)
      endif

    end subroutine readvalue_variable

end program ncmake_CF16
