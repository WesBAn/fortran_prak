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
        do j = 0, N
            arr_grad(i) = arr_grad(i) + 2 * (arr_x(j) ** (n-i)) * ( get_p_n(n, arr_a, arr_x(j)) - f(arr_x(j)) )
        end do
    end do
end subroutine get_grad_bigF


function get_avsqrt_dispersion(n, arr_a, arr_x, bigN)
    integer n
    integer bigN
    real arr_a(0:n)
    real arr_x(0:bigN)
    real get_avsqrt_dispersion
    get_avsqrt_dispersion = sqrt(get_bigF(n, arr_a, arr_x, bigN) / (bigN+1))
end function get_avsqrt_dispersion


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


subroutine lsp(n, arr_a, arr_x, bigN, eps)
    integer n
    integer bigN
    integer step
    real eps
    real, dimension(0:n) ::arr_a, curr_arr_a, prev_arr_a, pprev_arr_a
    real, dimension(0:n) ::curr_arr_v
    real, dimension(0:n) ::prev_arr_v(0:n)
    real, dimension(0:bigN) ::arr_x

    real curr_lambda
    real ::curr_beta = 0
    real g

    step = -1
    prev_arr_v = 0.0
    prev_arr_a = arr_a
    pprev_arr_a = 0.0
    do 
        step = step + 1
        call get_grad_bigF(n, prev_arr_a, arr_x, bigN, curr_arr_v)
        g = norm2(curr_arr_v)        
        curr_arr_v = -1*curr_arr_v + curr_beta * prev_arr_v
        ! arr_v = arr_v + curr_beta * 
        
        curr_lambda = get_lambda(n, prev_arr_a, curr_arr_v, arr_x, bigN)
        curr_arr_a = prev_arr_a + curr_lambda * curr_arr_v
        curr_delta = get_delta_k(n, curr_arr_a, prev_arr_a)
        print '(1x A I2 A F8.4 A F8.4)', '(step)k=', step, ' delta=', curr_delta, ' g=', g

        pprev_arr_a = prev_arr_a
        prev_arr_a = curr_arr_a
        prev_arr_v = curr_arr_v
        curr_beta = get_beta_k(n, pprev_arr_a, prev_arr_a, arr_x, bigN)

        if (curr_delta < eps) exit
    end do
    arr_a = curr_arr_a
end subroutine lsp


program main
    use tests
    integer, parameter ::n0 = 2
    integer, parameter ::bigN = 25
    integer, parameter ::m = 2
    integer ::n

    real, parameter ::c = 0.1
    real, parameter ::d = 1.1
    real, parameter ::eps = 10e-5 
    real ::arr_x(0:bigN)
    real ::arr_a(0:n0+m) = 0.0 
    real ::curr_p_n
    integer i, j

    logical tests_result
    call get_arr_x(arr_x, c, d, bigN)
    
    tests_result = run_unit_tests()
    if (tests_result) then
        do i = 0, m
            print *, ''
            print '(1x A I1 A)', '/////', i, '/////'
            print *, ''
            n = n0 + m - i
            call lsp(n, arr_a(m-i:n0+m), arr_x, bigN, eps)
            do j = 0, bigN
                curr_p_n = get_p_n(n, arr_a(m-i:n0+m), arr_x(j))
                print '(1x A I2 A F7.4 A F7.4 A F7.4 A F7.4)', 'j=', j, &
                                                               ' x=', arr_x(j), &
                                                               ' f(x)=', f(arr_x(j)), &
                                                               ' p_n(x)=', curr_p_n, &
                                                               ' |f(x)-p_n(x)|=', abs(f(arr_x(j))-curr_p_n)
            end do
            print '(1x A F7.4)', 'sigma=', get_avsqrt_dispersion(n, arr_a(m-i:n0+m), arr_x, bigN)
            do j = m-i, n0+m
                print '(1x A1 I2 A F9.4)', 'a', j-m+i, '=', arr_a(j)
            end do
        end do
    end if
end program main