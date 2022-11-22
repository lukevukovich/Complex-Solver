* Complex Solver -  Program 6 - Group 3: Gage, Luke, Austin
      program complexSolver
* Creting needed variables
        implicit none
        integer :: i, numEquations
        complex, dimension(11,10) :: equations
        character :: YorElse
        logical :: exit, correct, noSolutions

* Do while user chooses to keep solving
        exit = .false.
        do while (exit .eqv. .false.)

* Ask for # of equations, loop until valid value is passed
          print *, "NUMBER OF EQUATIONS/UNKNOWNS (1-10):"
          read(*,*) numEquations

          do while (numEquations .lt. 1 .or. numEquations .gt. 10)
            print *, "NUMBER OF EQUATIONS/UNKNOWNS (1-10):"
            read(*,*) numEquations
          end do

* Loop until user says data entered is correct
          correct = .false.
          do while(correct .eqv. .false.)

* Read and print data subroutines
            call getData(numEquations, equations)
            call printData(numEquations, equations)

* Ask if data is correct, if Y - continue; else - loop again
            print *, "IS DATA CORRECT? (Y/ELSE):"
            read(*,"(A)") YorElse
            if (YorElse .eq. 'y' .or. YorElse .eq. 'Y') then
              correct = .true.
            end if
          end do

* Solve data subroutine. Print data if there is solutions
* If no solutions, print message
          noSolutions = .false.
          call solve(numEquations, equations, noSolutions)
          if (noSolutions .eqv. .true.) then
            print *, ""
            print *, "NO SOLUTIONS"
          else
            call printSolutions(numEquations, equations)
          end if

* Ask to solve more equations, If Y - keep looping; else - end
          print *, ""
          print *, "SOLVE MORE EQUATIONS? (Y/ELSE):"
          read(*,"(A)") YorElse
          if (YorElse .ne. 'y' .and. YorElse .ne. 'Y') then
            exit = .true.
          end if

        end do

      end program

* Read data from user
      subroutine getData(var, equations)
        implicit none
        integer :: i, j
        real :: realPart, imagPart
        integer, intent(in) :: var
        complex, dimension(11,10), intent(out) :: equations

1       format(" ", "A(", I2, ",", I2, ") AS REAL IMAGINARY:")
2       format(" ", "CONSTANT(", I2, ") AS REAL IMAGINARY:")
3       format(" ", "ENTER " I2, " EQUATIONS WITH ", I2, " COEFFICIENTS
     CEACH FOR THE COEFFICIENT MATRIX:")
4       format(" ", "ENTER THE ", I2, " CONSTANT VECTOR VALUES:")

* Read coefficient matrix from user
* Reads real and imaginary part
        print *, ""
        print 3, var, var
        do i = 1, var
          do j = 1, var
            print 1, i, j
            read(*,*) realPart, imagPart
            equations(i,j) = cmplx(realPart, imagPart)
          end do
        end do

* Read solution vector from user
* Reads reall and imaginary part
        print *, ""
        print 4, var
        do i = 1, var
          print 2, i
          read(*,*) realPart, imagPart
          equations(i, var + 1) = cmplx(realPart, imagPart)
        end do

        return
      end subroutine

* Print formatted data
      subroutine printData(var, equations)
        implicit none
        integer :: i, j
        integer, intent(in) :: var
        character, dimension(10) :: plusSign
        complex, dimension(11,10), intent(in) :: equations

* Format statements
1       format(" ", 10(A1, F6.2, " + i", F6.2, A1, "X", I2, X, A1, X))
2       format(" ", "CONSTANT(", I2, ") = (", F6.2, " + i", F6.2, ")")

* Used to print a plus sign based on number of equations/unknowns
* EX: If there are 3 equations, 2 plus signs will be printed
        plusSign = [("+", i = 1, 10)]
        plusSign(var) = ""

        print *, ""
        print *, "DATA:"
* Print formatted coefficient matrix equations (with real and imag)
        do j = 1, var
3         print 1, ("(",real(equations(j,i)),aimag(equations(j,i)),")",
     C    i, plusSign(i), i = 1, var)
        end do

* Print formatted solution vector data (with real and imag)
        do j = 1, var
          print 2, j,real(equations(j,var+1)),
     C    aimag(equations(j,var+1))
        end do
        print *, ""

        return
      end subroutine

* Solve for unknowns using Guassian Elimination
      subroutine solve(var, matrix, nSol)
        implicit none
        integer :: i, j, k
        integer, intent(in) :: var
        real :: temp
        complex, dimension(11,10), intent(out) :: matrix
        logical, intent(out) :: nSol

*** Start Gaussian Elimination algorithm
        do i = 1, var
* Div by 0 check
          if (matrix(i,i) .eq. 0) then
            nSol = .true.
          else
            do j = 1, var
              if (i .ne. j) then
                temp = matrix(j,i)/matrix(i,i)
                do k = 1, var + 1
                  matrix(j,k) = matrix(j,k) - temp*matrix(i,k)
                end do
              end if
            end do
          end if
* No solution check
          if (matrix(i,i) .ne. 0) then
            nSol = .false.
            temp = matrix(i,i)
            do j = 1, var + 1
              matrix(i,j) = matrix(i,j)/temp
            end do
          else
            nSol = .true.
          end if
        end do
*** End Gaussian Elimination algorithm

        return
      end subroutine

* Print known solutions
      subroutine printSolutions(var, matrix)
        implicit none
        integer :: i
        integer, intent(in) :: var
        complex, dimension(11,10), intent(in) :: matrix

1       format(" ", "X", I2, " = (", F6.2, " + i", F6.2, ")")

        print *, ""
        print *, "SOLUTIONS:"
* Print known solutions - loop for var times
        do i = 1, var
          print 1, i, real(matrix(i,var+1)),
     C    aimag(matrix(i,var+1))
        end do

        return
      end subroutine