program ncmake_CF16

  use netcdf
  use cnst,  only: i4, r4
  use nclib, only: nclib__handle_error

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

  integer(i4)               :: ncid      ! netCDF ID
  integer(i4)               :: n         ! loop variable
  integer(i4)               :: ierr, nerr
  logical                   :: ldef      ! defined mode flag
  type(dimCF16), allocatable:: dimIfs(:) ! coordinate information

  ! &nml_files
  character(len = 200):: file_nc   ! name of the output netCDF file
  logical             :: flg_64bit ! 64-bit offset format flag
  ! &nml_general
  integer(i4)         :: ncor   ! number of coordinates
  integer(i4)         :: nvar   ! number of variables
  character(len = 100):: title  ! 'title' attribute
  character(len = 200):: source ! 'source' attribute

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
  call nclib__handle_error( nerr, 'NF90_CREATE', ierr )
  if ( ierr == 0 ) then
    ldef = .true.
    print '(a)', 'Output to ' // trim( file_nc )
  else
    stop
  endif

  ! put global attributes
  nerr = nf90_put_att( ncid, NF90_GLOBAL, 'Conventions', 'CF-1.6' )
  call nclib__handle_error( nerr, 'NF90_PUT_ATT (Conventions)', ierr )
  nerr = nf90_put_att( ncid, NF90_GLOBAL, 'title', trim( title ) )
  call nclib__handle_error( nerr, 'NF90_PUT_ATT (title)', ierr )
  nerr = nf90_put_att( ncid, NF90_GLOBAL, 'source', trim( source ) )
  call nclib__handle_error( nerr, 'NF90_PUT_ATT (source)', ierr )

  ! define and put dimensions and coordinate variables
  allocate(dimIfs(ncor))
  do n = 1, ncor
    call ncmake_CF16__add_dimension( dimIfs(n) )
  enddo

  ! define and put variables
  do n = 1, nvar
    call ncmake_CF16__add_variable( dimIfs )
  enddo

  ! close netCDF file
  nerr = nf90_close( ncid )
  call nclib__handle_error( nerr, 'NF90_CLOSE', ierr )
  if ( ierr /= 0 ) stop

  close(unml)


  contains

    subroutine ncmake_CF16__add_dimension( dimIf )
      type(dimCF16), intent(out):: dimIf

      integer(i4)          :: n          ! loop variable
      real(r4), allocatable:: vdim(:)    ! dimension value
      character(len = 100) :: units_time ! 'units' attribute for time dimension

      ! &nml_coordinate
      integer(i4)         :: length        ! length of the dimension
      character(len = 20) :: name_dim      ! name of the dimension
      character(len = 200):: long_name     ! 'long_name' attribute of the dimension
      character(len = 200):: standard_name ! 'standard_name' attribute of the dimension
      character(len = 100):: units         ! 'units' attribute of the dimension
      character(len = 1)  :: axis          ! 'axis' attribute of the dimension
      character(len = 4)  :: positive      ! 'positive' attribute of the dimension
      character(len = 20) :: date_ref      ! reference date of the time axis
      character(len = 20) :: calendar      ! 'calendar' attibute of time dimension
      character(len = 200):: file_cord     ! name of dimension file

      namelist /nml_coordinate/ name_dim, long_name, standard_name, length, units, &
      &                         axis, positive, date_ref, calendar, file_cord

      ! initial value for namelist variables
      long_name     = 'none'
      standard_name = 'none'
      units         = 'dimensionless'
      axis          = 'N'
      positive      = 'none'
      calendar      = 'gregorian'
      date_ref      = '00000000000000000000'

      ! read namelist and check
      read(unml, nml = nml_coordinate)
      if ( trim( long_name ) == 'none' .and. trim( standard_name ) == 'none' ) then
        print '(a)', 'Error: neither long_name nor standard_name are specified. '
        stop
      endif
      if ( axis == 'N' ) then
        print '(a)', 'Error: axis is not specified. '
        stop
      endif

      ! change to define mode
      if ( .not. ldef ) then
        nerr = nf90_redef( ncid )
        call nclib__handle_error( nerr, 'NF90_REDEF', ierr )
        if ( ierr == 0 ) ldef = .true.
      endif

      ! define dimension
      dimIf%name = name_dim
      dimIf%dlen = length
      nerr = nf90_def_dim( ncid, trim( name_dim ), length, dimIf%dimID )
      call nclib__handle_error( nerr, 'NF90_DEF_DIM (' // trim( name_dim ) // ')', ierr )

      ! define dimension variable
      nerr =  nf90_def_var( ncid, trim( name_dim ), NF90_FLOAT, dimIf%dimID, dimIf%varID )
      call nclib__handle_error( nerr, 'NF90_DEF_VAR (' // trim( name_dim ) // ')', ierr )
      !   long_name
      if ( trim( long_name ) /= 'none' ) then
        nerr = nf90_put_att( ncid, dimIf%varID, 'long_name', trim( long_name ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_dim ) // ':long_name)', ierr )
      endif
      !   standard_name
      if ( trim( standard_name ) /= 'none' ) then
        nerr = nf90_put_att( ncid, dimIf%varID, 'standard_name', trim( standard_name ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_dim ) // ':standard_name)', ierr )
      endif
      !   units
      if ( trim( units ) /= 'dimensionless' ) then
        if ( axis == 'T' ) then
          if ( date_ref == '00000000000000000000' ) then
            print '(a)', 'Error: date_ref is not specified.'
            stop
          endif
          units_time = trim( units ) // ' since ' // date_ref(1:4) // '-' // &
          &            date_ref(5:6) // '-' // date_ref(7:8)
          if ( date_ref(9:20) /= '000000000000' ) then
            units_time = trim( units_time ) // ' ' // date_ref(9:10) // ':' // &
            &            date_ref(11:12) // ':' // date_ref(13:14)
            if ( date_ref(15:20) /= '000000' ) then
              units_time = trim( units_time ) // '.' // date_ref(15:20)
            endif
          endif
          nerr = nf90_put_att( ncid, dimIf%varID, 'units', trim( units_time ) )
        else
          nerr = nf90_put_att( ncid, dimIf%varID, 'units', trim( units ) )
        endif
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_dim ) // ':units)', ierr )
      endif
      !   axis (and positive)
      nerr = nf90_put_att( ncid, dimIf%varID, 'axis', axis )
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_dim ) // ':axis)', ierr )
      if ( axis == 'Z' ) then
        if ( trim( positive ) == 'none' ) then
          print '(a)', 'Error: positive is not specified. '
          stop
        else
          nerr = nf90_put_att( ncid, dimIf%varID, 'positive', trim( positive ) )
          call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_dim ) // ':positive)', ierr )
        endif
      endif
      !   calendar
      if ( axis == 'T' ) then
        nerr = nf90_put_att( ncid, dimIf%varID, 'calendar', trim( calendar ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_dim ) // ':calendar)', ierr )
      endif

      nerr = nf90_enddef( ncid )
      call nclib__handle_error( nerr, 'NF90_ENDDEF', ierr )
      if ( ierr /= 0 ) stop
      if ( ierr == 0 ) ldef = .false.

      ! read dimension values
      open(udim, file = trim( file_cord ), form = 'formatted', status = 'old', action = 'read', iostat = ierr)
      if ( ierr /= 0 ) then
        print '(a)', 'Error: cannot open dimension value file. ' // trim( file_cord )
        stop
      endif
      allocate(vdim(length))
      if ( length >= 10 ) then
        do n = 1, length / 10
          read(udim, *) vdim(10*(n-1)+1:10*n)
        enddo
      endif
      if ( mod( length, 10 ) > 0 ) then
        read(udim, *) vdim(length/10*10+1:length)
      endif
      close(udim)

      ! write dimension variables
      nerr = nf90_put_var( ncid, dimIf%varID, vdim )
      call nclib__handle_error( nerr, 'NF90_PUT_VAR (' // trim( name_dim ) // ')', ierr )

    end subroutine ncmake_CF16__add_dimension


    subroutine ncmake_CF16__add_variable( dimIfs )
      type(dimCF16), intent(in):: dimIfs(:) ! dimension information

      integer(i4)             :: ncor          ! number of coordinates
      integer(i4), allocatable:: dimIDs(:)     ! list of dimenstion IDs
      integer(i4), allocatable:: dimidx(:)     ! list of dimension index
      integer(i4)             :: varID         ! variable ID
      integer(i4)             :: lrecvar       ! record length of the variable
      integer(i4)             :: vshape(10)    ! shape of the variable
      integer(i4)             :: shp1(1), shp2(2), shp3(3), shp4(4)
      integer(i4)             :: n, nc, nt, nr ! loop variables
      integer(i4)             :: idxi, idxe, numrec
      integer(i4)             :: flgdim
      real(r4),    allocatable:: variable(:)   ! variable value
#ifdef DEBUG
      logical,     allocatable:: maskvar(:)  ! mask for variable array
#endif

      ! &nml_variable
      integer(i4)         :: ndim          ! number of dimensions
      integer(i4)         :: nrec(3)       ! record number and interval in the source file
      real(r4)            :: undef(2)      ! input/output undefined values
      real(r4)            :: scale_factor  ! 'scale_factor' attribute of the variable
      real(r4)            :: add_offset    ! 'add_offset' attribute of the variable
      character(len = 200):: long_name     ! 'long_name' attribute of the variable
      character(len = 200):: standard_name ! 'standard_name' attribute of the variable
      character(len = 20) :: units         ! 'units' attribute of the variable
      character(len = 20) :: name_var      ! name of the variable
      character(len = 20) :: list_cord(20) ! list of the dimension name
      character(len = 200):: file_var      ! name of the source file of the variable

      namelist /nml_variable/ name_var, long_name, standard_name, ndim, list_cord, &
      &                       units, scale_factor, add_offset, undef, file_var, nrec

      ! initial value for namelist variables
      long_name     = 'none'
      standard_name = 'none'
      units         = 'dimensionless'
      scale_factor  = 0.e0
      add_offset    = 0.e0
      undef         = 10.e0
      nrec          = (/ -1, -1, -1 /)

      ! read namelist and check
      read(unml, nml = nml_variable)
      if ( trim( long_name ) == 'none' .and. trim( standard_name ) == 'none' ) then
        print '(a)', 'Error: neither long_name nor standard_name are specified. '
        stop
      endif
      if ( undef(1) == 10.e0 .and. undef(2) == 10.e0 ) then
        print '(a)', 'Error: undefined value is not specified. '
        stop
      endif

      ! change to define mode
      if ( .not. ldef ) then
        nerr = nf90_redef( ncid )
        call nclib__handle_error( nerr, 'NF90_REDEF', ierr )
        if ( ierr == 0 ) ldef = .true.
      endif

      ! define variable
      ncor = size( dimIfs )
      allocate(dimIDs(ndim))
      allocate(dimidx(ndim))
      do n = 1, ndim
        flgdim = 0
        do nc = 1, ncor
          if ( trim( list_cord(n) ) == trim( dimIfs(nc)%name ) ) then
            dimidx(n) = nc
            dimIDs(n) = dimIfs(nc)%dimID
            flgdim = 1
            exit
          endif
        enddo

        if ( flgdim == 0 ) then
          print '(a)', 'Error: dimension does not exist. ' // trim( list_cord(n) )
          stop
        endif
      enddo
      nerr = nf90_def_var( ncid, trim( name_var ), NF90_FLOAT, dimIDs, varID )
      call nclib__handle_error( nerr, 'NF90_DEF_VAR (' // trim( name_var ) // ')', ierr )
      if ( ierr /= 0 ) stop
      !   long_name
      if ( trim( long_name ) /= 'none' ) then
        nerr = nf90_put_att( ncid, varID, 'long_name', trim( long_name ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_var ) // ':' // 'long_name)', ierr )
      endif
      !   standard_name
      if ( trim( standard_name ) /= 'none' ) then
        nerr = nf90_put_att( ncid, varID, 'standard_name', trim( standard_name ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_var ) // ':' // 'standard_name)', ierr )
      endif
      !   units
      if ( trim( units ) /= 'dimensionless' ) then
        nerr = nf90_put_att( ncid, varID, 'units', trim( units ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_var ) // ':' // 'units)', ierr )
      endif
      !   _FillValue
      if ( undef(1) /= undef(2) .and. undef(2) /= 10.e0 ) then
        nerr = nf90_put_att( ncid, varID, '_FillValue', undef(2) )
      else
        nerr = nf90_put_att( ncid, varID, '_FillValue', undef(1) )
      endif
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_var ) // ':' // '_FillValue)', ierr )
      ! scale_factor
      if ( scale_factor /= 0.e0 ) then
        nerr = nf90_put_att( ncid, varID, 'scale_factor', scale_factor )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_var ) // ':' // 'scale_factor)', ierr )
      endif
      ! add_offset
      if ( add_offset /= 0.e0 ) then
        nerr = nf90_put_att( ncid, varID, 'add_offset', add_offset )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name_var ) // ':' // 'add_offset)', ierr )
      endif

      nerr = nf90_enddef( ncid )
      call nclib__handle_error( nerr, 'NF90_ENDDEF', ierr )
      if ( ierr /= 0 ) stop
      if ( ierr == 0 ) ldef = .false.

      ! read variable values
      lrecvar = 4
      if ( nrec(1) == nrec(2) .or. nrec(2) == -1 ) then
        do n = 1, ndim - 1
          lrecvar = lrecvar * dimIfs(dimidx(n))%dlen
        enddo
      else
        do n = 1, ndim - 2
          lrecvar = lrecvar * dimIfs(dimidx(n))%dlen
        enddo
      endif

      vshape = 0
      do n = 1, ndim
        vshape(n) = dimIfs(dimidx(n))%dlen
      enddo
      print '(a, i0)', 'Size of variable: ', product( vshape(1:ndim) )

      allocate(variable(product( vshape(1:ndim) )))
      open(uvar, file = trim( file_var ), form = 'unformatted', status = 'old', action = 'read', &
      &    access = 'direct', recl = lrecvar, iostat = ierr)
      if ( ierr /= 0 ) then
        print '(a)', 'Error: cannot open variable value file. ' // trim( file_var )
        stop
      endif
      print '(a)', 'Add variable ' // trim( name_var ) // ' (' // trim( long_name ) // ')'

      do nt = 1, vshape(ndim)
        if ( nrec(1) == nrec(2) .or. nrec(2) == -1 ) then
          idxi = product( vshape(1:ndim-1) ) * (nt - 1) + 1
          idxe = idxi + product( vshape(1:ndim-1) ) - 1
          numrec = nrec(3) * (nt - 1) + nrec(1)
          read(uvar, rec = numrec) variable(idxi:idxe)
!          print '(a, i6, a)', '  from ' // trim( file_var ) // ' (rec=', numrec, ')'
        else
          do nr = nrec(1), nrec(2)
            idxi = product( vshape(1:ndim-2) ) * (nr - nrec(1)) &
            &    + product( vshape(1:ndim-1) ) * (nt - 1) + 1
            idxe = idxi + product( vshape(1:ndim-2) ) - 1
            numrec = nrec(3) * (nt - 1) + nr
            read(uvar, rec = numrec) variable(idxi:idxe)
          enddo
!          print '(a, 2(i6, a))', '  from ' // trim( file_var ) // ' (rec=', &
!          & numrec - (nrec(2) - nrec(1)), '-', numrec, ')'
        endif
      enddo
      close(uvar)

      ! change undefined value
      if ( undef(1) /= undef(2) .and. undef(2) /= 10.e0 ) then
        where ( variable == undef(1) ) variable = undef(2)
      endif

#ifdef DEBUG
      allocate(maskvar(product( vshape(1:ndim) )))
      maskvar = .true.
      do n = 1, product( vshape(1:ndim) )
        if ( variable(n) == undef(2) ) maskvar(n) = .false.
      enddo
      print '(2(a, f15.5))', '    Max.: ', maxval( variable, mask = maskvar ), &
      &                      ';  Min.: ', minval( variable, mask = maskvar )
      deallocate(maskvar)
#endif /* DEBUG */

      ! put variable
      select case ( ndim )
        case ( 1 )
          shp1 = vshape(1:1)
          nerr = nf90_put_var( ncid, varID, reshape( variable, shp1 ) )
        case ( 2 )
          shp2 = vshape(1:2)
          nerr = nf90_put_var( ncid, varID, reshape( variable, shp2 ) )
        case ( 3 )
          shp3 = vshape(1:3)
          nerr = nf90_put_var( ncid, varID, reshape( variable, shp3 ) )
        case ( 4 )
          shp4 = vshape(1:4)
          nerr = nf90_put_var( ncid, varID, reshape( variable, shp4 ) )
        case default
          print '(a)', 'Error: dimension number exceed current maximum. '
          stop
      end select
      call nclib__handle_error( nerr, 'NF90_PUT_VAR (' // trim( name_var ) // ')', ierr )

      deallocate(variable)
    end subroutine ncmake_CF16__add_variable

end program ncmake_CF16
