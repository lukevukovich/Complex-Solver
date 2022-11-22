* Used to develop and test guass elimination method
      program guassProgram
        implicit none
        integer :: i, j, k, var
        real, dimension(10,9) :: matrix
        real :: temp

1       format(" ", "Enter data for (", I1, ",", I1, "):")
2       format(" ", "Enter solution for equation ", I1, ":")
3       format(" ", 10(F6.2, 2X))
4       format(" ", "X", I1, " = ", F6.2)

        print *, "Enter # of variables (1-9):"
        read(*,*) var

        do i = 1, var
          do j = 1, var
            print 1, i, j
            read(*,*) matrix(i,j)
          end do
        end do

        do i = 1, var
          print 2, i
          read(*,*) matrix(i, var+1)
        end do

        print *, "Augmented Matrix:"
        do j = 1, var
          print 3, (matrix(j, i), i = 1, var + 1)
        end do

        do i = 1, var
          if (matrix(i,i) .eq. 0) then
            print *, "Div by 0 error"
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
          if (matrix(i,i) .ne. 0) then
            temp = matrix(i,i)
            do j = 1, var + 1
              matrix(i,j) = matrix(i,j)/temp
            end do
          else
            print *, "No Solution"
          end if
        end do

        print *, "Solved Matrix:"
        do j = 1, var
          print 3, (matrix(j, i), i = 1, var + 1)
        end do

        print *, "Solutions:"
        do j = 1, var
          print 4, j, matrix(j, var + 1)
        end do

      end program