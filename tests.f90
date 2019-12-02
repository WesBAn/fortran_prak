module tests
    use assert_func
    use utils
    
    private :: get_test_report
    private :: test_f_zero
    private :: test_f
    private :: test_get_x_from_interval
    private :: test_get_arr_x
    private :: test_gorner
    private :: test_scal
    private :: test_get_p_n
    private :: test_get_bigF
    private :: test_get_lambda
    private :: test_get_grad_bigF
    private :: test_get_avsqrt_dispersion
    private :: test_get_delta_k
    private :: test_get_beta_k
    private :: test_lsp

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
        character(30), parameter ::name_of_test = 'test_f_zero'

        test_f_zero = u_assert(f(1.0), 0.0)
        call get_test_report(test_f_zero, name_of_test)
    end function test_f_zero

    function test_f()
        logical test_f
        character(30), parameter ::name_of_test = 'test_f'

        test_f = u_assert(f(2.0), (exp(2.0)-1)/(exp(2.0)+1) )
        call get_test_report(test_f, name_of_test)
    end function test_f

    function test_get_x_from_interval()
        logical test_get_x_from_interval
        character(30), parameter ::name_of_test = 'test_get_x_from_interval'
        
        test_get_x_from_interval = u_assert(get_x_from_interval(0.1, 1.1, 4, 2), 0.6)
        call get_test_report(test_get_x_from_interval, name_of_test)
    end function test_get_x_from_interval

    function test_get_arr_x()
        logical test_get_arr_x
        character(30), parameter ::name_of_test = 'test_get_arr_x'
        real ::arr_x(1:5) = (/0.0, 0.0, 0.0, 0.0, 0.0/)
        real, parameter ::answer(1:5) = (/0.1, 0.35, 0.6, 0.85, 1.1/)

        test_get_arr_x = .TRUE.
        call get_arr_x(arr_x, 0.1, 1.1, 4)
        do i = 1, 4
            test_get_arr_x = u_assert(arr_x(i), answer(i)) .AND. test_get_arr_x
        end do
        call get_test_report(test_get_arr_x, name_of_test)
    end function test_get_arr_x

    function test_gorner()
        logical test_gorner
        character(30), parameter ::name_of_test = 'test_gorner'
        real, parameter ::arr(2:4) = (/1.0, -4.0, 3.0/)
        real y
        
        call gorner(arr, 2, 4, 5.0, y)
        test_gorner = u_assert(y, 8.0)
        
        call get_test_report(test_gorner, name_of_test)
    end function test_gorner

    function test_scal()
        logical test_scal
        character(30), parameter ::name_of_test = 'test_scal'
        real, parameter ::arr1(2:4) = (/1.0, -2.0, 3.0/)
        real, parameter ::arr2(2:4) = (/3.0, -4.0, 0.0/)
        real y

        call scal(arr1, arr2, 2, 4, y)
        test_scal = u_assert(y, 11.0)

        call get_test_report(test_scal, name_of_test)
    end function test_scal
    
    function test_get_p_n()
        logical test_get_p_n
        character(30), parameter ::name_of_test = 'test_get_p_n'
        real, parameter ::arr_a(0:2) = (/2.0, -2.0, 3.0/)
        real ::x = 2.0

        test_get_p_n = u_assert(get_p_n(2, arr_a, x), 7.0)
        call get_test_report(test_get_p_n, name_of_test)
    end function test_get_p_n

    function test_get_bigF()
        logical test_get_bigF
        character(30), parameter ::name_of_test = 'test_get_bigF'
        real, parameter ::arr_a(0:2) = (/1.0, -2.0, 3.0/)
        real, parameter ::arr_x(0:2) = (/1.0, 2.0, 3.0/)
        real ::answer = 0.0
        ! y1 = 0  y2 =0.76159 y3 = 0.96402  pn(1) = 2 pn(2) = 3 pn(3) = 6 
        answer = get_bigF(2, arr_a, arr_x, 2)
        test_get_bigF = u_assert(answer, 34.371479) ! Округлено для простоты
        call get_test_report(test_get_bigF, name_of_test)
    end function test_get_bigF

    function test_get_lambda()
        logical test_get_lambda
        character(30), parameter ::name_of_test = 'test_get_lambda'
        real, parameter ::arr_a(0:2) = (/-1.0, 0.0, 1.0/)
        real, parameter ::arr_x(0:1) = (/1.0, 2.0/)

        !test_get_lambda = u_assert(get_lambda(2, arr))
        call get_test_report(test_get_lambda, name_of_test)
    end function test_get_lambda

    function test_get_grad_bigF()
        logical test_get_grad_bigF
        character(30), parameter ::name_of_test = 'test_get_grad_bigF'
        
        call get_test_report(test_get_grad_bigF, name_of_test)
    end function test_get_grad_bigF

    function test_get_avsqrt_dispersion()
        logical test_get_avsqrt_dispersion
        character(30), parameter ::name_of_test = 'test_get_avsqrt_dispersion'
        
        call get_test_report(test_get_avsqrt_dispersion, name_of_test)
    end function test_get_avsqrt_dispersion
    
    function test_get_delta_k()
        logical test_get_delta_k
        character(30), parameter ::name_of_test = 'test_get_delta_k'
        
        call test_get_test_report(test_get_delta_k, name_of_test)
    end function test_get_delta_k

    function test_get_beta_k()
        logical test_get_beta_k
        character(30), parameter ::name_of_test = 'test_get_beta_k'
        
        call get_test_report(test_get_beta_k, name_of_test)
    end function test_get_beta_k

    function test_lsp()
        logical test_lsp
        character(30), parameter ::name_of_test = 'test_lsp'
        
        call get_test_report(test_lsp, name_of_test)
    end function test_lsp

    function run_unit_tests()
        logical run_unit_tests
        if (test_f_zero() &
                .AND. test_f() &
                .AND. test_get_x_from_interval() &
                .AND. test_get_arr_x() &
                .AND. test_gorner() &
                .AND. test_get_p_n() &
                .AND. test_get_bigF() &
                .AND. test_get_lambda() &
                .AND. test_get_grad_bigF() &
                .AND. test_get_avsqrt_dispersion() &
                .AND. test_get_delta_k() &
                .AND. test_get_beta_k() &
                .AND. test_lsp()) then
            run_unit_tests = .TRUE.
            print *, 'Unit tests passed'
        else
            run_unit_tests = .FALSE.
            print *, 'Unit tests failed'
        end if
    end function run_unit_tests
end module tests