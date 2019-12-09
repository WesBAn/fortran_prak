module utils

contains
    subroutine gorner(A, K, L, X, Y)
        real, dimension(K:L) ::A
        integer K, L
        real X, Y

        Y = 0
        do i = K, L
           Y = Y + A(i) * (X**(L-i)) 
        end do
    end subroutine gorner

    subroutine scal(A, B, K, L, Y)
        real, dimension(K:L) ::A, B
        integer K, L
        real Y
        
        Y = 0
        do i = K, L
            Y = Y + A(i)*B(i)
        end do 
    end subroutine scal
end module