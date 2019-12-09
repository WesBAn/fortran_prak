module pfuncs
    character(30), private, parameter ::a_iteration_step_format = '(1x A I2 A F7.5 A F8.4 A F8.4)'
    character(11), private, parameter ::main_divider_format = '(1x A I1 A)'
    character(11), private, parameter ::sigma_format = '(1x A F6.4)'
    character(17), private, parameter ::a_i_format = '(1x A1 I1 A F8.4)'
    character(37), private, parameter ::main_table_format = '(1x A I3 A F8.4 A F8.4 A F8.4 A F8.4)'

    public :: print_n_size
    public :: print_fx_pn
    public :: print_sigma
    public :: print_a_i
    public :: print_a_iteration_step
    public :: print_welcome_message

contains
    subroutine print_n_size(i)
        integer i
        print *, ''
        print *, '////////////////////////////////////////////////'
        print main_divider_format, 'Многочлен p_', i, ' степени'
        print *, ''
    end subroutine print_n_size

    subroutine print_fx_pn(j, x_j, fx, pn_j, delta_fx_pn)
        integer j
        real x_j, fx, pn_j, delta_fx_pn

        print main_table_format, 'j =', j, &
                                 ' | x =', x_j, &
                                 ' | f(x) =', fx, &
                                 ' | p_n(x) =', pn_j, &
                                 ' | |f(x)-p_n(x)| =', delta_fx_pn
    end subroutine print_fx_pn

    subroutine print_sigma(sigma)
        real sigma
        print sigma_format, 'sigma = ', sigma
    end subroutine print_sigma

    subroutine print_a_i(i, a_i)
        integer i
        real a_i
        print a_i_format, 'a', i, ' =', a_i
    end subroutine print_a_i

    subroutine print_a_iteration_step(step, curr_delta, g, curr_bigF)
        integer step
        real curr_delta, g, curr_bigF
        print a_iteration_step_format, '(step)k=', step, ' delta=', curr_delta, ' g=', g, ' bigF=', curr_bigF
    end subroutine print_a_iteration_step

    subroutine print_welcome_message()
        print *, " *  *  *  *  *  *  *  *  *  *  * "
        print *, " *                             * "
        print *, " *    Линник Александр 305     * "
        print *, " *                             * "
        print *, " *    Задание 1                * "
        print *, " *    Вариант 4-1-4            * "
        print *, " *    f(x)=tanh(x-1)           * "
        print *, " *    с = 0.1; d = 1.1         * "
        print *, " *    N = 25; n0 = 2; m = 2    * "
        print *, " *                             * "
        print *, " *    Отладочная функция       * "
        print *, " *    f(x)=(x-0.5)(x-0.3)      * "
        print *, " *    Необходимо получить      * "
        print *, " *    a = (1.0, -0.8, 0.15)    * "
        print *, " *                             * "
        print *, " *  *  *  *  *  *  *  *  *  *  * "
    end subroutine print_welcome_message
end module pfuncs