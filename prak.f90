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


subroutine get_arr_p(arr_p, n, x, arr_of_a)
    integer n, i
    real ::arr_of_a(0:n)
    real p, x
    p = 0.0
    do i = 0, n
        p = p + arr_of_a(n-i) * (x**i) 
    end do
end subroutine get_arr_p


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


! function bigF(vect, vect_size, N)
!     integer N
!     integer vect_size
!     integer i
!     real, dimension(vect_size) :: vect
!     real ::bigF = 0
!     do i = 1, N
!         bigF = bigF + sqr(f(i) )
!     end do
! end function bigF

