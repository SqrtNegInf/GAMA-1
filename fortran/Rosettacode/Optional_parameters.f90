!   Fortran
!u# https://rosettacode.org/wiki/Optional_parameters
!c# 2024-01-04 
!!  No 'table' data so, not runnable code anyhow

!Works with: Fortran version 95 and later
!In Fortran, each argument has its "name". The optional attribute can be used to specify that an argument is optional, and its presence (or absence) can be tested using the present intrinsic (so that we can give a default value, or execute accordingly a totally different code).

module ExampleOptionalParameter
  ! use any module needed for the sort function(s)
  ! and all the interfaces needed to make the code work
  implicit none
contains

  subroutine sort_table(table, ordering, column, reverse)
    type(table_type), intent(inout) :: table                !! placeholder error here
    integer, optional :: column
    logical, optional :: reverse
    optional :: ordering
    interface
       integer function ordering(a, b)
         type(table_element), intent(in) :: a, b            !! same
       end function ordering
    end interface

    integer :: the_column, i
    logical :: reversing
    type(table_row) :: rowA, rowB

    if ( present(column) ) then
       if ( column > get_num_of_columns(table) ) then
          ! raise an error?
       else
          the_column = column
       end if
    else
       the_column = 1   ! a default value, de facto
    end if

    reversing = .false.  ! default value
    if ( present(reverse) ) reversing = reverse

    do
       ! loops over the rows to sort... at some point, we need
       ! comparing an element (cell) of the row, with the element
       ! in another row; ... let us suppose rowA and rowB are
       ! the two rows we are considering
       ea = get_element(rowA, the_column)
       eb = get_element(rowB, the_column)
       if ( present(ordering) ) then
          if ( .not. reversing ) then
             if ( ordering(ea, eb) > 0 ) then
                ! swap the rowA with the rowB
             end if
          else   ! < instead of >
             if ( ordering(ea, eb) < 0 ) then
                ! swap the rowA with the rowB
             end if
          end if
       else
          if ( .not. reversing ) then
             if ( lexinternal(ea, eb) > 0 ) then
                ! swap the rowA with the rowB
             end if
          else   ! < instead of >
             if ( lexinternal(ea, eb) < 0 ) then
                ! swap the rowA with the rowB
             end if
          end if
       end if
       ! ... more of the sorting algo ...
       ! ... and rows traversing ... (and an exit condition of course!)
    end do

  end subroutine sort_table

end module ExampleOptionalParameter

program UsingTest
  use ExampleOptionalParameter
  implicit none

  type(table_type) :: table

  ! create the table...

  ! sorting taking from column 1, not reversed, using internal
  ! default comparator
  call sort_table(table)

  ! the same as above, but in reversed order; we MUST specify
  ! the name of the argument since it is not given in the same
  ! order of the subroutine spec
  call sort_table(table, reverse=.true.)

  ! sort the table using a custom comparator
  call sort_table(table, my_cmp)
  ! or
  call sort_table(table, ordering=my_cmp)

  ! as above, but taking from column 2
  call sort_table(table, my_cmp, 2)
  ! or (swapping the order of args for fun)
  call sort_table(table, column=2, ordering=my_cmp)

  ! with custom comparator, column 2 and reversing...
  call sort_table(table, my_cmp, 2, .true.)
  ! of course we can swap the order of optional args
  ! by prefixing them with the name of the arg

  ! sort from column 2, with internal comparator
  call sort_table(table, column=2)

end program UsingTest
