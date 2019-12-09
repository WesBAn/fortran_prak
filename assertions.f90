module assert_func
    private
    interface u_assert
       module procedure u_assert_integer4, u_assert_integer8, u_assert_real4, u_assert_double8, u_assert_logical
    end interface u_assert

    public :: u_assert

contains
    function u_assert_integer4(statement1, statement2)
        integer(KIND=4) statement1, statement2
        logical u_assert_integer4
        if (statement1 == statement2) then
            u_assert_integer4 = .TRUE.
        else 
            print *,'test fallen'
            print *, statement1, '!=', statement2 
            u_assert_integer4 = .FALSE.
        end if

    end function u_assert_integer4

    function u_assert_integer8(statement1, statement2)
        integer(KIND=8) statement1, statement2
        logical u_assert_integer8
        if (statement1 == statement2) then
            u_assert_integer8 = .TRUE.
        else 
            print *,'test fallen'
            print *, statement1, '!=', statement2 
            u_assert_integer8 = .FALSE.
        end if
    end function u_assert_integer8
    
    function u_assert_real4(statement1, statement2)
        real(KIND=4) statement1, statement2
        logical u_assert_real4
        if (statement1 == statement2 .OR. abs(statement1-statement2)<0.0001) then
            u_assert_real4 = .TRUE.
        else 
            print *,'test fallen'
            print *, statement1, '!=', statement2 
            u_assert_real4 = .FALSE.
        end if
    end function u_assert_real4

    function u_assert_double8(statement1, statement2)
        double precision statement1, statement2
        logical u_assert_double8
        if (statement1 == statement2 .OR. abs(statement1-statement2)<0.0001) then
            u_assert_double8 = .TRUE.
        else 
            print *,'test fallen'
            print *, statement1, '!=', statement2 
            u_assert_double8 = .FALSE.
        end if
    end function u_assert_double8
    
    function u_assert_logical(statement1, statement2)
        logical statement1, statement2
        logical u_assert_logical
        if (statement1 .eqv. statement2) then
            u_assert_logical = .TRUE.
        else 
            print *,'test fallen'
            print *, statement1, '!=', statement2 
            u_assert_logical = .FALSE.
        end if
    end function u_assert_logical
end module assert_func