! author: mc-wesban // Alexander Linnik
! prefix big makes letter High bN = N

function f(x) 
    real x
    real f
    ! f = (x-0.5)*(x-0.3)
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


function get_p_n(n, arr_a, x)
    use utils

    integer n
    real ::arr_a(0:n)
    real x
    real get_p_n
    call gorner(arr_a, 0, n, x, get_p_n)
end function get_p_n


function get_bigF(n, arr_a, arr_x, bigN)
    use utils 
    integer n
    integer bigN
    real arr_a(0:n)
    real arr_x(0:bigN)
    real get_bigF
    integer i

    get_bigF = 0.0
    do i = 0, bigN
        get_bigF = get_bigF + (f(arr_x(i)) - get_p_n(n, arr_a, arr_x(i)))**2
    end do
end function get_bigF


subroutine get_grad_bigF(n, arr_a, arr_x, bigN, arr_grad)
    integer n
    integer bigN
    real arr_a(0:n)
    real arr_x(0:bigN)
    real arr_grad(0:n)
    
    do i = 0, n
        arr_grad(i) = 0
        do j = 0, bigN
            arr_grad(i) = arr_grad(i) + 2.0 * (arr_x(j) ** (n-i)) * ( get_p_n(n, arr_a, arr_x(j)) - f(arr_x(j)) )
        end do
    end do
end subroutine get_grad_bigF


function get_sigma(n, arr_a, arr_x, bigN)
    integer n
    integer bigN
    real arr_a(0:n)
    real arr_x(0:bigN)
    real get_sigma
    get_sigma = sqrt(get_bigF(n, arr_a, arr_x, bigN) / (bigN+1))
end function get_sigma


function get_delta_k(n, arr_a_k, arr_a_k_minus1) 
    integer n
    real, dimension(0:n) ::arr_a_k, arr_a_k_minus1
    real get_delta_k

    sigma_k = 0
    do i = 0, n
        get_delta_k = max(sigma_k, abs((arr_a_k(i) - arr_a_k_minus1(i)) / arr_a_k(i)))
    end do
end function get_delta_k


function get_beta_k(n, arr_a_k, arr_a_k_plus1, arr_x, bigN)
    integer n, bigN
    real arr_x(0:bigN)
    real, dimension(0:n) ::arr_a_k, arr_a_k_plus1
    real, dimension(0:n) ::grad_a_k, grad_a_k_plus1
    real get_beta_k

    
    call get_grad_bigF(n, arr_a_k, arr_x, bigN, grad_a_k)
    call get_grad_bigF(n, arr_a_k_plus1, arr_x, bigN, grad_a_k_plus1)

    get_beta_k = (norm2(grad_a_k_plus1)**2) / (norm2(grad_a_k)**2) 
end function get_beta_k


function get_lambda(n, arr_a_k, arr_v, arr_x, bigN)
    integer n
    integer bigN
    real, dimension(0:n) ::arr_a_k, arr_v
    real, dimension(0:bigN) ::arr_x
    real get_lambda
    real, dimension(1:3) ::mu
    real, parameter ::lambdas(1:3) = (/-1.0, 0.0, 1.0/)

    do i = 1, 3
        mu(i) = get_bigF(n, arr_a_k + lambdas(i) * arr_v, arr_x, bigN)
    end do
    get_lambda = ( mu(1) - mu(3) ) / ( 2*(mu(1) - 2*mu(2) + mu(3)) )

end function get_lambda


subroutine minimize_bigF_and_get_arr_a(n, arr_a, arr_x, bigN, eps)
    use pfuncs
    integer n
    integer bigN
    integer step
    real eps
    real, dimension(0:n) ::arr_a, curr_arr_a, prev_arr_a, new_current_arr_a
    real, dimension(0:n) ::curr_arr_v, prev_arr_v
    real, dimension(0:bigN) ::arr_x

    real curr_lambda
    real prev_beta
    real g
    real curr_bigF
    real curr_grad(0:n)

    curr_arr_a = arr_a
    step = -1
    prev_arr_a = 0.0
    prev_arr_v = 0.0
    prev_beta = 0.0
    print *, '(2) Процесс минимизации Ф(a)=Ф(a0,a1,a2,...,an)'
    do
        step = step + 1
        curr_bigF = get_bigF(n, curr_arr_a, arr_x, bigN)
        call get_grad_bigF(n, curr_arr_a, arr_x, bigN, curr_grad)
        g = norm2(curr_grad)

        if (step > 0) then
           prev_beta = get_beta_k(n, prev_arr_a, curr_arr_a, arr_x, bigN)
        end if

        curr_arr_v = -1.0*curr_grad + prev_beta*prev_arr_v
        curr_lambda = get_lambda(n, curr_arr_a, curr_arr_v, arr_x, bigN)

        new_current_arr_a = curr_arr_a + curr_lambda*curr_arr_v

        curr_delta = get_delta_k(n, new_current_arr_a, curr_arr_a)
        call print_a_iteration_step(step, curr_delta, g, curr_bigF)
        
        prev_arr_a = curr_arr_a
        curr_arr_a = new_current_arr_a
        prev_arr_v = curr_arr_v

        if (curr_delta < eps) exit
    end do

    print *, ''
    arr_a = new_current_arr_a
end subroutine minimize_bigF_and_get_arr_a


program main
    use tests
    use pfuncs

    integer, parameter ::n0 = 2
    integer, parameter ::bigN = 25
    integer, parameter ::m = 2
    integer ::n

    real, parameter ::c = 0.1
    real, parameter ::d = 1.1
    real, parameter ::eps = 10e-5 
    real ::arr_x(0:bigN)
    real ::arr_a(0:n0+m-1) = 0.0 
    real curr_p_n, sigma
    real delta_fx_pn
    integer i, j
    logical tests_result

    tests_result = run_unit_tests()
    call print_welcome_message
    call get_arr_x(arr_x, c, d, bigN)
    do i = 0, m-1
        call print_n_size(i+n0)
        n = n0 + i
        call minimize_bigF_and_get_arr_a(n, arr_a(m-1-i:n0+m-1), arr_x, bigN, eps)
        print *, '(1 + 3.1) Сравнение y_i и p_n(x_i)'
        do j = 0, bigN
            curr_p_n = get_p_n(n, arr_a(m-1-i:n0+m), arr_x(j))
            delta_fx_pn = abs(f(arr_x(j))-curr_p_n)
            call print_fx_pn(j, arr_x(j), f(arr_x(j)), curr_p_n, delta_fx_pn)
        end do

        sigma = get_sigma(n, arr_a(m-1-i:n0+m-1), arr_x, bigN)
        call print_sigma(sigma)
        print *, '(4) Коэффициенты p_n'
        do j = m-1-i, n0+m-1
            call print_a_i(j-m+i+1, arr_a(j))
        end do
    end do

    call print_graphic_urls()
end program main
