!   Fortran
!u# https://rosettacode.org/wiki/Abelian_sandpile_model
!c# 2024-02-07 

! 'flang' fails, 'polymorphic types' not implemented...

module rgbimage_m

  implicit none

  private
  public :: rgbimage

  type rgbimage
    !! usage
    !!    1) init
    !!    2a) fill_image
    !!      or
    !!    2b) set_pixel
    !!    3) normalize
    !!    4) write

    private

    integer, dimension(:,:,:), allocatable :: rgb
      !! pixel arrays of rgb values
      !! indices (i,j,k)
      !!    i: position x_i
      !!    j: position y_j
      !!    k=1: red, k=2: green, k=3: blue

    integer :: n(2) = 0
      !! image dimensions: [height, width]

  contains
    procedure :: init       => rgbimage_init              ! inits image
    procedure :: fill_image => rgbimage_fill_image        ! fill image with constant rgb value
    procedure :: get_pixel  => rgbimage_get_pixel         ! gets one pixel
    procedure :: normalize  => rgbimage_normalize         ! normalizes all pixels onto range [0, 255]
    procedure :: set_pixel  => rgbimage_set_pixel         ! sets one pixel
    procedure :: write      => rgbimage_write             ! outputs image to file

    procedure, private :: inside => rgbimage_inside
    procedure, private :: valid  => rgbimage_valid
  end type

contains

  subroutine rgbimage_init(this, height, width)
    !! initialize image.
    !! sets dimensions, allocates pixels and sets colors to 0.

    class(rgbimage), intent(out) :: this
    integer,         intent(in)  :: height, width

    this%n = [height, width]
    allocate (this%rgb(height,width,3), source=0)
  end subroutine

  logical function rgbimage_valid(this, check_rgb_vals)
    !! checks if the image has valid dimensions and optionally valid rgb values.

    class(rgbimage), intent(in)           :: this
    logical,         intent(in), optional :: check_rgb_vals
      !! check if rgb values are in allowed range [0, 255]?
      !! default: dont check

    ! always check that dimensions match
    rgbimage_valid = ( all(this%n > 0)                     .and. &
      &               (size(this%rgb, dim=1) == this%n(1)) .and. &
      &               (size(this%rgb, dim=2) == this%n(2)) .and. &
      &               (size(this%rgb, dim=3) == 3)               )

    ! optionally: check if rgb values are in allowed range
    if (present(check_rgb_vals)) then
      if (check_rgb_vals) rgbimage_valid = ( rgbimage_valid       .and. &
        &                                   (all(this%rgb >= 0))  .and. &
        &                                   (all(this%rgb <= 255))      )
    end if

  end function

  logical function rgbimage_inside(this, x, y)
    !! checks if given coordinates are inside the image

    class(rgbimage), intent(in) :: this
    integer,         intent(in) :: x, y

    rgbimage_inside = ((x > 0) .and. (x <= this%n(1)) .and. (y > 0) .and. (y <= this%n(2)))
  end function

  subroutine rgbimage_set_pixel(this, x, y, rgb)
    class(rgbimage), intent(inout) :: this
    integer,         intent(in)    :: x, y
      !! coordinates
    integer,         intent(in)    :: rgb(3)
      !! red, green, blue values

    if (this%inside(x, y)) then
      ! use given data at first
      this%rgb(x,y,:) = rgb

      ! check if given data was out of bounds
      where     (this%rgb(x,y,:) > 255)
        this%rgb(x,y,:) = 255
      elsewhere (this%rgb(x,y,:) < 0)
        this%rgb(x,y,:) = 0
      end where
    end if
  end subroutine

  function rgbimage_get_pixel(this, x, y) result(rgb)
    class(rgbimage), intent(in) :: this
    integer,         intent(in) :: x, y
      !! coordinates
    integer                     :: rgb(3)
      !! red, green, blue values

    if (this%inside(x, y)) then
      rgb = this%rgb(x,y,:)
    else
      rgb = 0
    end if
  end function

  subroutine rgbimage_normalize(this)
    !! normalize colors to be in range [0, 255]

    class(rgbimage), intent(inout) :: this

    where     (this%rgb(:,:,:) > 255)
      this%rgb(:,:,:) = 255
    elsewhere (this%rgb(:,:,:) < 0)
      this%rgb(:,:,:) = 0
    end where
  end subroutine

  subroutine rgbimage_fill_image(this, rgb)
    !! fill whole image with given rgb values.

    class(rgbimage), intent(inout) :: this
    integer,         intent(in)    :: rgb(3)
      !! red, green, blue values

    integer :: i

    if (this%valid()) then
      do i = 1, 3
        this%rgb(:,:,i) = rgb(i)
      end do
    end if
  end subroutine

  subroutine rgbimage_write(this, fname)
    class(rgbimage), intent(in) :: this
    character(*),    intent(in) :: fname
      !! file path, e.g. "tmp/out.ppm"

    integer :: iounit, ios, i,j,k

    open (newunit=iounit, file=fname, iostat=ios, action='WRITE')
!   if (ios /= 0) error stop "Error opening file: " // fname
! Error: Parameter ‘fname’ at (1) has not been declared or is a variable, which does not reduce to a constant expression
! (but runs without this test...)

    ! write header
    write (iounit, '(A)')         'P6'
    write (iounit, '(I0, A, I0)') this%n(1), " ", this%n(2)
    write (iounit, '(A)')         '255'

    do i = 1, this%n(1)
      do j = 1, this%n(2)
        write (iounit, '(3A1)', advance='no') [(achar(this%rgb(i,j,k)), k=1,3)]
      end do
    end do

    close (unit=iounit, iostat=ios)
    if (ios /= 0) error stop "Error closing file"
  end subroutine

end module
module abelian_sandpile_m

  implicit none

  private
  public :: pile

  type :: pile
    !! usage:
    !!    1) init
    !!    2) run

    integer, allocatable :: grid(:,:)
    integer              :: n(2)

  contains
    procedure :: init
    procedure :: run

    procedure, private :: process_node
    procedure, private :: inside
  end type

contains

  logical function inside(this, i)
    class(pile), intent(in) :: this
    integer,     intent(in) :: i(2)

    inside = ((i(1) > 0) .and. (i(1) <= this%n(1)) .and. (i(2) > 0) .and. (i(2) <= this%n(2)) )
  end function

  recursive subroutine process_node(this, i)
    !! start process

    class(pile), intent(inout) :: this
    integer,     intent(in)    :: i(2)
      !! node coordinates to process

    integer :: i0(2,2), j(2), d, k

    ! if node has more than 4 grains -> redistribute
    if (this%grid(i(1),i(2)) >= 4) then
      ! unit vectors: help shift only one dimension (see below)
      i0 = reshape([1,0,0,1], [2,2])

      ! subtract 4 grains
      this%grid(i(1),i(2)) = this%grid(i(1),i(2))-4

      ! add one grain to neighbor if not out of bound
      do d = 1, 2               ! loop dimensions
        do k = -1, 1, 2         ! loop +-1 step in direction d
          j = i+k*i0(:,d)       ! j = i, but one element is shifted by +-1
          if (this%inside(j)) this%grid(j(1),j(2)) = this%grid(j(1),j(2)) + 1
        end do
      end do

      ! check neighbor nodes
      do d = 1, 2               ! loop dimensions
        do k = -1, 1, 2         ! loop +-1 step in direction d
          j = i+k*i0(:,d)       ! j = i, but one element is shifted by +-1
          if (this%inside(j)) call this%process_node(j)
        end do
      end do

      ! check itself
      call this%process_node(i)
    end if
  end subroutine

  subroutine run(this)
    !! start process

    class(pile), intent(inout) :: this

    ! only node that could be unstable is inital node
    call this%process_node(this%n/2)
  end subroutine

  subroutine init(this, nx, ny, h)
    class(pile), intent(out) :: this
    integer,     intent(in)  :: nx, ny
      !! grid dimensions
    integer,     intent(in)  :: h
      !! height of and grains in middle of grid

    this%n = [nx, ny]
    allocate (this%grid(nx,ny), source=0)
    this%grid(nx/2, ny/2) = h
  end subroutine

end module

program main

  use rgbimage_m
  use abelian_sandpile_m

  implicit none

  integer :: nx, ny, i, j

  integer :: colors(0:3,3)

  type(rgbimage) :: im
  type(pile) :: p

  colors(0,:) = [255,255,255]
  colors(1,:) = [0,0,90]
  colors(2,:) = [0,0,170]
  colors(3,:) = [0,0,255]

  nx = 200
  ny = 100

  call p%init(nx, ny, 2000)
  call p%run

  call im%init(nx, ny)

  do i = 1, nx
    do j = 1, ny
      call im%set_pixel(i, j, colors(p%grid(i,j),:))
    end do
  end do

  call im%write('fig.ppm')

end program
