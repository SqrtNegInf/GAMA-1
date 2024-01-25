module peaceful_queens_elements

  use, intrinsic :: iso_fortran_env, only: int64

  implicit none
  private

  integer, parameter, public :: m = 4
  integer, parameter, public :: n = 5
  integer, parameter, public :: max_solutions = 1000

  ! 64-bit integers, for boards up to 8-by-8.
  integer, parameter, private :: kind8x8 = int64
  integer, parameter, public :: board_kind = kind8x8


  public :: rooks1_attack_check
  public :: rooks2_attack_check
  public :: rooks_attack_check
  public :: bishops1_attack_check
  public :: bishops2_attack_check
  public :: bishops_attack_check
  public :: queens_attack_check

  public :: board_rotate90
  public :: board_rotate180
  public :: board_rotate270
  public :: board_reflect1
  public :: board_reflect2
  public :: board_reflect3
  public :: board_reflect4

  integer(kind = kind8x8), parameter :: rook1_mask_5x5_0 = int (z'0000000000000000000000000000001F', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook1_mask_5x5_1 = int (z'000000000000000000000000000003E0', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook1_mask_5x5_2 = int (z'00000000000000000000000000007C00', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook1_mask_5x5_3 = int (z'000000000000000000000000000F8000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook1_mask_5x5_4 = int (z'00000000000000000000000001F00000', kind = kind8x8)

  integer(kind = kind8x8), parameter :: rook2_mask_5x5_0 = int (z'00000000000000000000000000108421', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook2_mask_5x5_1 = int (z'00000000000000000000000000210842', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook2_mask_5x5_2 = int (z'00000000000000000000000000421084', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook2_mask_5x5_3 = int (z'00000000000000000000000000842108', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook2_mask_5x5_4 = int (z'00000000000000000000000001084210', kind = kind8x8)

  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_0 = int (z'00000000000000000000000001041041', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_1 = int (z'00000000000000000000000000082082', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_2 = int (z'00000000000000000000000000820820', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_3 = int (z'00000000000000000000000000004104', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_4 = int (z'00000000000000000000000000410400', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_5 = int (z'00000000000000000000000000000208', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop1_mask_5x5_6 = int (z'00000000000000000000000000208000', kind = kind8x8)

  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_0 = int (z'00000000000000000000000000111110', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_1 = int (z'00000000000000000000000000008888', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_2 = int (z'00000000000000000000000000222200', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_3 = int (z'00000000000000000000000000000444', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_4 = int (z'00000000000000000000000000444000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_5 = int (z'00000000000000000000000000000022', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop2_mask_5x5_6 = int (z'00000000000000000000000000880000', kind = kind8x8)

  integer(kind = kind8x8), parameter :: rook_mask_5x5_0 = int (z'0000000000000000000000000000001F', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_1 = int (z'000000000000000000000000000003E0', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_2 = int (z'00000000000000000000000000007C00', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_3 = int (z'000000000000000000000000000F8000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_4 = int (z'00000000000000000000000001F00000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_5 = int (z'00000000000000000000000000108421', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_6 = int (z'00000000000000000000000000210842', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_7 = int (z'00000000000000000000000000421084', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_8 = int (z'00000000000000000000000000842108', kind = kind8x8)
  integer(kind = kind8x8), parameter :: rook_mask_5x5_9 = int (z'00000000000000000000000001084210', kind = kind8x8)

  integer(kind = kind8x8), parameter :: bishop_mask_5x5_0 = int (z'00000000000000000000000001041041', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_1 = int (z'00000000000000000000000000111110', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_2 = int (z'00000000000000000000000000082082', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_3 = int (z'00000000000000000000000000008888', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_4 = int (z'00000000000000000000000000820820', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_5 = int (z'00000000000000000000000000222200', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_6 = int (z'00000000000000000000000000004104', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_7 = int (z'00000000000000000000000000000444', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_8 = int (z'00000000000000000000000000410400', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_9 = int (z'00000000000000000000000000444000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_10 = int (z'00000000000000000000000000000208', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_11 = int (z'00000000000000000000000000000022', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_12 = int (z'00000000000000000000000000208000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: bishop_mask_5x5_13 = int (z'00000000000000000000000000880000', kind = kind8x8)

  integer(kind = kind8x8), parameter :: queen_mask_5x5_0 = int (z'0000000000000000000000000000001F', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_1 = int (z'000000000000000000000000000003E0', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_2 = int (z'00000000000000000000000000007C00', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_3 = int (z'000000000000000000000000000F8000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_4 = int (z'00000000000000000000000001F00000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_5 = int (z'00000000000000000000000000108421', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_6 = int (z'00000000000000000000000000210842', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_7 = int (z'00000000000000000000000000421084', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_8 = int (z'00000000000000000000000000842108', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_9 = int (z'00000000000000000000000001084210', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_10 = int (z'00000000000000000000000001041041', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_11 = int (z'00000000000000000000000000111110', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_12 = int (z'00000000000000000000000000082082', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_13 = int (z'00000000000000000000000000008888', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_14 = int (z'00000000000000000000000000820820', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_15 = int (z'00000000000000000000000000222200', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_16 = int (z'00000000000000000000000000004104', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_17 = int (z'00000000000000000000000000000444', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_18 = int (z'00000000000000000000000000410400', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_19 = int (z'00000000000000000000000000444000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_20 = int (z'00000000000000000000000000000208', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_21 = int (z'00000000000000000000000000000022', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_22 = int (z'00000000000000000000000000208000', kind = kind8x8)
  integer(kind = kind8x8), parameter :: queen_mask_5x5_23 = int (z'00000000000000000000000000880000', kind = kind8x8)

contains

  elemental function rooks1_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, rook1_mask_5x5_0) /= 0) .and. (iand (army2, rook1_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, rook1_mask_5x5_1) /= 0) .and. (iand (army2, rook1_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, rook1_mask_5x5_2) /= 0) .and. (iand (army2, rook1_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, rook1_mask_5x5_3) /= 0) .and. (iand (army2, rook1_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, rook1_mask_5x5_4) /= 0) .and. (iand (army2, rook1_mask_5x5_4) /= 0))
  end function rooks1_attack_check

  elemental function rooks2_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, rook2_mask_5x5_0) /= 0) .and. (iand (army2, rook2_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, rook2_mask_5x5_1) /= 0) .and. (iand (army2, rook2_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, rook2_mask_5x5_2) /= 0) .and. (iand (army2, rook2_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, rook2_mask_5x5_3) /= 0) .and. (iand (army2, rook2_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, rook2_mask_5x5_4) /= 0) .and. (iand (army2, rook2_mask_5x5_4) /= 0))
  end function rooks2_attack_check

  elemental function bishops1_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, bishop1_mask_5x5_0) /= 0) .and. (iand (army2, bishop1_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, bishop1_mask_5x5_1) /= 0) .and. (iand (army2, bishop1_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, bishop1_mask_5x5_2) /= 0) .and. (iand (army2, bishop1_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, bishop1_mask_5x5_3) /= 0) .and. (iand (army2, bishop1_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, bishop1_mask_5x5_4) /= 0) .and. (iand (army2, bishop1_mask_5x5_4) /= 0)) .or. &
              & ((iand (army1, bishop1_mask_5x5_5) /= 0) .and. (iand (army2, bishop1_mask_5x5_5) /= 0)) .or. &
              & ((iand (army1, bishop1_mask_5x5_6) /= 0) .and. (iand (army2, bishop1_mask_5x5_6) /= 0))
  end function bishops1_attack_check

  elemental function bishops2_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, bishop2_mask_5x5_0) /= 0) .and. (iand (army2, bishop2_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, bishop2_mask_5x5_1) /= 0) .and. (iand (army2, bishop2_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, bishop2_mask_5x5_2) /= 0) .and. (iand (army2, bishop2_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, bishop2_mask_5x5_3) /= 0) .and. (iand (army2, bishop2_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, bishop2_mask_5x5_4) /= 0) .and. (iand (army2, bishop2_mask_5x5_4) /= 0)) .or. &
              & ((iand (army1, bishop2_mask_5x5_5) /= 0) .and. (iand (army2, bishop2_mask_5x5_5) /= 0)) .or. &
              & ((iand (army1, bishop2_mask_5x5_6) /= 0) .and. (iand (army2, bishop2_mask_5x5_6) /= 0))
  end function bishops2_attack_check

  elemental function rooks_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, rook_mask_5x5_0) /= 0) .and. (iand (army2, rook_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_1) /= 0) .and. (iand (army2, rook_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_2) /= 0) .and. (iand (army2, rook_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_3) /= 0) .and. (iand (army2, rook_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_4) /= 0) .and. (iand (army2, rook_mask_5x5_4) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_5) /= 0) .and. (iand (army2, rook_mask_5x5_5) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_6) /= 0) .and. (iand (army2, rook_mask_5x5_6) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_7) /= 0) .and. (iand (army2, rook_mask_5x5_7) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_8) /= 0) .and. (iand (army2, rook_mask_5x5_8) /= 0)) .or. &
              & ((iand (army1, rook_mask_5x5_9) /= 0) .and. (iand (army2, rook_mask_5x5_9) /= 0))
  end function rooks_attack_check

  elemental function bishops_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, bishop_mask_5x5_0) /= 0) .and. (iand (army2, bishop_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_1) /= 0) .and. (iand (army2, bishop_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_2) /= 0) .and. (iand (army2, bishop_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_3) /= 0) .and. (iand (army2, bishop_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_4) /= 0) .and. (iand (army2, bishop_mask_5x5_4) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_5) /= 0) .and. (iand (army2, bishop_mask_5x5_5) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_6) /= 0) .and. (iand (army2, bishop_mask_5x5_6) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_7) /= 0) .and. (iand (army2, bishop_mask_5x5_7) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_8) /= 0) .and. (iand (army2, bishop_mask_5x5_8) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_9) /= 0) .and. (iand (army2, bishop_mask_5x5_9) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_10) /= 0) .and. (iand (army2, bishop_mask_5x5_10) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_11) /= 0) .and. (iand (army2, bishop_mask_5x5_11) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_12) /= 0) .and. (iand (army2, bishop_mask_5x5_12) /= 0)) .or. &
              & ((iand (army1, bishop_mask_5x5_13) /= 0) .and. (iand (army2, bishop_mask_5x5_13) /= 0))
  end function bishops_attack_check

  elemental function queens_attack_check (army1, army2) result (attacking)
    integer(kind = kind8x8), value :: army1, army2
    logical :: attacking

    attacking = ((iand (army1, queen_mask_5x5_0) /= 0) .and. (iand (army2, queen_mask_5x5_0) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_1) /= 0) .and. (iand (army2, queen_mask_5x5_1) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_2) /= 0) .and. (iand (army2, queen_mask_5x5_2) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_3) /= 0) .and. (iand (army2, queen_mask_5x5_3) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_4) /= 0) .and. (iand (army2, queen_mask_5x5_4) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_5) /= 0) .and. (iand (army2, queen_mask_5x5_5) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_6) /= 0) .and. (iand (army2, queen_mask_5x5_6) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_7) /= 0) .and. (iand (army2, queen_mask_5x5_7) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_8) /= 0) .and. (iand (army2, queen_mask_5x5_8) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_9) /= 0) .and. (iand (army2, queen_mask_5x5_9) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_10) /= 0) .and. (iand (army2, queen_mask_5x5_10) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_11) /= 0) .and. (iand (army2, queen_mask_5x5_11) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_12) /= 0) .and. (iand (army2, queen_mask_5x5_12) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_13) /= 0) .and. (iand (army2, queen_mask_5x5_13) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_14) /= 0) .and. (iand (army2, queen_mask_5x5_14) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_15) /= 0) .and. (iand (army2, queen_mask_5x5_15) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_16) /= 0) .and. (iand (army2, queen_mask_5x5_16) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_17) /= 0) .and. (iand (army2, queen_mask_5x5_17) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_18) /= 0) .and. (iand (army2, queen_mask_5x5_18) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_19) /= 0) .and. (iand (army2, queen_mask_5x5_19) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_20) /= 0) .and. (iand (army2, queen_mask_5x5_20) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_21) /= 0) .and. (iand (army2, queen_mask_5x5_21) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_22) /= 0) .and. (iand (army2, queen_mask_5x5_22) /= 0)) .or. &
              & ((iand (army1, queen_mask_5x5_23) /= 0) .and. (iand (army2, queen_mask_5x5_23) /= 0))
  end function queens_attack_check

  elemental function board_rotate90 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Rotation 90 degrees in one of the orientations.

    b = ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_0, a), 0)), 0), &
      &   ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_1, a), -5)), 1), &
      &     ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_2, a), -10)), 2), &
      &       ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_3, a), -15)), 3), &
      &            ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_4, a), -20)), 4)))))
  end function board_rotate90

  elemental function board_rotate180 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Rotation 180 degrees.

    b = board_reflect1 (board_reflect2 (a))
  end function board_rotate180

  elemental function board_rotate270 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Rotation 270 degrees in one of the orientations.

    b = ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_0, a), 0)), 4), &
      &   ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_1, a), -5)), 3), &
      &     ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_2, a), -10)), 2), &
      &       ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_3, a), -15)), 1), &
      &            ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_4, a), -20)), 0)))))
  end function board_rotate270

  elemental function board_reflect1 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Reflection of rows or columns.

    b = ior (ishft (iand (rook2_mask_5x5_0, a), 4), &
      &      ior (ishft (iand (rook2_mask_5x5_1, a), 2), &
      &           ior (ishft (iand (rook2_mask_5x5_2, a), 0), &
      &                ior (ishft (iand (rook2_mask_5x5_3, a), -2), &
      &                     ishft (iand (rook2_mask_5x5_4, a), -4)))))
  end function board_reflect1

  elemental function board_reflect2 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Reflection of rows or columns.

    b = ior (ishft (iand (rook1_mask_5x5_0, a), 20), &
      &      ior (ishft (iand (rook1_mask_5x5_1, a), 10), &
      &           ior (ishft (iand (rook1_mask_5x5_2, a), 0), &
      &                ior (ishft (iand (rook1_mask_5x5_3, a), -10), &
      &                     ishft (iand (rook1_mask_5x5_4, a), -20)))))
  end function board_reflect2

  elemental function board_reflect3 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Reflection around one of the two main diagonals.

    b = ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_0, a), 0)), 0), &
      &   ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_1, a), -5)), 1), &
      &     ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_2, a), -10)), 2), &
      &       ior (ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_3, a), -15)), 3), &
      &            ishft (insert_zeros_5 (ishft (iand (rook1_mask_5x5_4, a), -20)), 4)))))
  end function board_reflect3

  elemental function board_reflect4 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    ! Reflection around one of the two main diagonals.

    b = ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_0, a), 0)), 4), &
      &   ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_1, a), -5)), 3), &
      &     ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_2, a), -10)), 2), &
      &       ior (ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_3, a), -15)), 1), &
      &            ishft (reverse_insert_zeros_5 (ishft (iand (rook1_mask_5x5_4, a), -20)), 0)))))
  end function board_reflect4

  elemental function insert_zeros_5 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    b = ior (ishft (ibits (a, 0, 1), 0), &
      &      ior (ishft (ibits (a, 1, 1), 5), &
      &           ior (ishft (ibits (a, 2, 1), 10), &
      &                ior (ishft (ibits (a, 3, 1), 15), &
      &                     ishft (ibits (a, 4, 1), 20)))))
  end function insert_zeros_5

  elemental function reverse_insert_zeros_5 (a) result (b)
    integer(kind = kind8x8), value :: a
    integer(kind = kind8x8) :: b

    b = ior (ishft (ibits (a, 4, 1), 0), &
      &      ior (ishft (ibits (a, 3, 1), 5), &
      &           ior (ishft (ibits (a, 2, 1), 10), &
      &                ior (ishft (ibits (a, 1, 1), 15), &
      &                     ishft (ibits (a, 0, 1), 20)))))
  end function reverse_insert_zeros_5

end module peaceful_queens_elements
