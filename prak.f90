! author: mc-wesban // Alexander Linnik
! prefix big makes letter High bN = N

function f(x) 
    real x
    real f
    f = tanh(x-1) 
end function f


function get_x_from_interval(c, d, bigN, stepNumber)
    real c
    real d
    integer bigN
    integer stepNumber
    real get_x_from_interval
    get_x_from_interval = c + (d-c)*stepNumber/bigN 
end function get_x_from_interval


subroutine get_arr_x(arr_x, c, d, bigN)
    integer bigN
    real c
    real d
    real arr_x(0:bigN)

    integer i

    do i = 0, bigN
        arr_x(i) = get_x_from_interval(c, d, bigN, i)
    end do
end subroutine get_arr_x


function p_n(n, arr_a, x)
    integer n
    real ::arr_a(0:n)
    real x
    real p_n
    
    real curr_x
    integer i

    curr_x = 1
    p_n = 0
    do i = 0, n
        p_n = p_n + arr_a(n-i) * curr_x
        curr_x = curr_x * x
    end do
end function p_n


! function bigF(n, arr_a, arr_x, bigN)    
!     integer n
!     integer ::arr_a(0:n)
    
!     real, dimension(vect_size)
!     real ::bigF = 0

!     integer i
!     do i = 0, bigN
!         bigF = bigF + sqr(f(i) )
!     end do
! end function bigF


program main
    use tests
    integer, parameter ::n0 = 2
    integer, parameter ::bigN = 25
    integer, parameter ::m = 2
    integer ::i

    real, parameter ::c = 0.1
    real, parameter ::d = 1.1
    
    real ::arr_x(0:bigN)
    logical tess
    call get_arr_x(arr_x, c, d, bigN)
    
    tess = run_unit_tests()
    ! do i = 0, bigN
    !     print '(1x, F10.3)', arr_x(i)
    ! end do
    print *, tess
end program main



! ! tests.f90
! module tests
!     use assert_func

!     private :: get_test_report
!     private :: test_f_zero
!     private :: test_f
!     private :: test_get_x_from_interval
!     private :: test_get_arr_x
!     ! private :: test_p_n

!     public :: run_unit_tests

! contains
!     subroutine get_test_report(result, name_of_test)
!         logical result
!         character(30) name_of_test

!         if (result) then
!             print *, name_of_test, ' passed'
!         else
!             print *, name_of_test, ' failed'
!         end if 
!     end subroutine get_test_report

!     function test_f_zero()
!         logical test_f_zero
!         character(30) ::name_of_test = 'test_f_zero'

!         test_f_zero = u_assert(f(1.0), 0.0)
!         call get_test_report(test_f_zero, name_of_test)
!     end function test_f_zero

!     function test_f()
!         logical test_f
!         character(30) ::name_of_test = 'test_f'

!         test_f = u_assert(f(2.0), (exp(2.0)-1)/(exp(2.0)+1) )
!         call get_test_report(test_f, name_of_test)
!     end function test_f

!     function test_get_x_from_interval()
!         logical test_get_x_from_interval
!         character(30) ::name_of_test = 'test_get_x_from_interval'
        
!         test_get_x_from_interval = u_assert(get_x_from_interval(0.1, 1.1, 4, 2), 0.6)
!         call get_test_report(test_get_x_from_interval, name_of_test)
!     end function test_get_x_from_interval

!     function test_get_arr_x()
!         logical test_get_arr_x
!         character(30) ::name_of_test = 'test_get_arr_x'
!         real arr_x(1:5) /0.0, 0.0, 0.0, 0.0, 0.0/
!         real answer(1:5) /0.1, 0.35, 0.6, 0.85, 1.1/

!         test_get_arr_x = .TRUE.
!         call get_arr_x(arr_x, 0.1, 1.1, 4)
!         do i = 1, 4
!             test_get_arr_x = u_assert(arr_x(i), answer(i)) .AND. test_get_arr_x
!         end do
!         call get_test_report(test_get_arr_x, name_of_test)
!     end function test_get_arr_x

!    ! function test_p_n()
!    !     logical test_p_n
!    !     character(30) ::name_of_test = 'test_p_n'

!    ! end function test_p_n

!     function run_unit_tests()
!         logical run_unit_tests
!         if (test_f_zero() &
!                 .AND. test_f() &
!                 .AND. test_get_x_from_interval() &
!                 .AND. test_get_arr_x()) then
!             run_unit_tests = .TRUE.
!         else
!             run_unit_tests = .FALSE.
!             print *, 'Unit tests failed'
!         end if
!     end function run_unit_tests
! end module tests

! ! assertions.f90
! module assert_func
!     private
!     interface u_assert
!        module procedure u_assert_integer4, u_assert_integer8, u_assert_real4, u_assert_real8,  u_assert_logical
!     end interface u_assert

!     public :: u_assert

! contains
!     function u_assert_integer4(statement1, statement2)
!         integer(KIND=4) statement1, statement2
!         logical u_assert_integer4
!         if (statement1 == statement2) then
!             u_assert_integer4 = .TRUE.
!         else 
!             print *,'test fallen'
!             print *, statement1, '!=', statement2 
!             u_assert_integer4 = .FALSE.
!         end if

!     end function u_assert_integer4

!     function u_assert_integer8(statement1, statement2)
!         integer(KIND=8) statement1, statement2
!         logical u_assert_integer8
!         if (statement1 == statement2) then
!             u_assert_integer8 = .TRUE.
!         else 
!             print *,'test fallen'
!             print *, statement1, '!=', statement2 
!             u_assert_integer8 = .FALSE.
!         end if
!     end function u_assert_integer8
    
!     function u_assert_real4(statement1, statement2)
!         real(KIND=4) statement1, statement2
!         logical u_assert_real4
!         if (statement1 == statement2) then
!             u_assert_real4 = .TRUE.
!         else 
!             print *,'test fallen'
!             print *, statement1, '!=', statement2 
!             u_assert_real4 = .FALSE.
!         end if
!     end function u_assert_real4

!     function u_assert_real8(statement1, statement2)
!         real(KIND=8) statement1, statement2
!         logical u_assert_real8
!         if (statement1 == statement2) then
!             u_assert_real8 = .TRUE.
!         else 
!             print *,'test fallen'
!             print *, statement1, '!=', statement2 
!             u_assert_real8 = .FALSE.
!         end if
!     end function u_assert_real8
    
!     function u_assert_logical(statement1, statement2)
!         logical statement1, statement2
!         logical u_assert_logical
!         if (statement1 .eqv. statement2) then
!             u_assert_logical = .TRUE.
!         else 
!             print *,'test fallen'
!             print *, statement1, '!=', statement2 
!             u_assert_logical = .FALSE.
!         end if
!     end function u_assert_logical
! end module assert_func