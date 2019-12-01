module tests
    use assert_func

    private :: get_test_report
    private :: test_f_zero
    private :: test_f
    private :: test_get_x_from_interval
    private :: test_get_arr_x
    ! private :: test_p_n

    public :: run_unit_tests

contains
    subroutine get_test_report(result, name_of_test)
        logical result
        character(30) name_of_test

        if (result) then
            print *, name_of_test, ' passed'
        else
            print *, name_of_test, ' failed'
        end if 
    end subroutine get_test_report

    function test_f_zero()
        logical test_f_zero
        character(30) ::name_of_test = 'test_f_zero'

        test_f_zero = u_assert(f(1.0), 0.0)
        call get_test_report(test_f_zero, name_of_test)
    end function test_f_zero

    function test_f()
        logical test_f
        character(30) ::name_of_test = 'test_f'

        test_f = u_assert(f(2.0), (exp(2.0)-1)/(exp(2.0)+1) )
        call get_test_report(test_f, name_of_test)
    end function test_f

    function test_get_x_from_interval()
        logical test_get_x_from_interval
        character(30) ::name_of_test = 'test_get_x_from_interval'
        
        test_get_x_from_interval = u_assert(get_x_from_interval(0.1, 1.1, 4, 2), 0.6)
        call get_test_report(test_get_x_from_interval, name_of_test)
    end function test_get_x_from_interval

    function test_get_arr_x()
        logical test_get_arr_x
        character(30) ::name_of_test = 'test_get_arr_x'
        real arr_x(1:5) /0.0, 0.0, 0.0, 0.0, 0.0/
        real answer(1:5) /0.1, 0.35, 0.6, 0.85, 1.1/

        test_get_arr_x = .TRUE.
        call get_arr_x(arr_x, 0.1, 1.1, 4)
        do i = 1, 4
            test_get_arr_x = u_assert(arr_x(i), answer(i)) .AND. test_get_arr_x
        end do
        call get_test_report(test_get_arr_x, name_of_test)
    end function test_get_arr_x

   ! function test_p_n()
   !     logical test_p_n
   !     character(30) ::name_of_test = 'test_p_n'

   ! end function test_p_n

    function run_unit_tests()
        logical run_unit_tests
        if (test_f_zero() &
                .AND. test_f() &
                .AND. test_get_x_from_interval() &
                .AND. test_get_arr_x()) then
            run_unit_tests = .TRUE.
        else
            run_unit_tests = .FALSE.
            print *, 'Unit tests failed'
        end if
    end function run_unit_tests
end module tests