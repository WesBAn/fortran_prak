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
    integer, parameter ::n0 = 2
    integer, parameter ::bigN = 25
    integer, parameter ::m = 2
    integer ::i

    real, parameter ::c = 0.1
    real, parameter ::d = 1.1
    
    real ::arr_x(0:bigN)

    call get_arr_x(arr_x, c, d, bigN)

    do i = 0, bigN
        print '(1x, F10.3)', arr_x(i)
    end do
end program main