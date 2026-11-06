PROGRAM circle
        IMPLICIT NONE
        REAL :: d,A
        REAL,PARAMETER :: pi=3.14

       !LOGIC
        PRINT*, "insert diameter(d):"
        READ*, d
        A=((d/2)**2)*pi

        PRINT*,"Area of cicle:",A

ENDPROGRAM
