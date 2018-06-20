program fnn
!
! Use Statements
!
use mod_numeric, only: real_kind, & ! kind for Real vars
                       int_kind     ! kind for Integer vars
use mod_matrix_arithmetic, only: output_to_class
use mod_utilities, only: matrix_print
!
! Implicit Statements
!
implicit none
!
! Declaration Statements
!
external imp, train
integer(kind=int_kind), parameter :: ninput = 4,        &
                                     nclasses = 3,      &
                                     ntrainings = 75,   &
                                     nvalidations = 37, &
                                     ntests = 38,       &
                                     max_iterations = 500
real(kind=real_kind) :: training_set(ntrainings, ninput + nclasses + 1), &
                        tmp1(ntrainings)
real(kind=real_kind) :: validation_set(nvalidations, ninput + nclasses + 1), &
                        tmp2(nvalidations)
real(kind=real_kind) :: test_set(ntests, ninput + nclasses + 1), &
                        tmp3(ntests)
real(kind=real_kind) :: weights(ninput + 1, nclasses)
real(kind=real_kind) :: regerror(3), claserror(3)
!
! Implementation
!
  write(*,*) "FNN - FORTRAN NEURAL NETWORK"
  write(*,*) " Importing Data Files"
  call imp("../iris_data_files/iris_training.dat",  &
          training_set(:, 1:ninput+nclasses), &
          (/ntrainings, ninput + nclasses/))
  call imp("../iris_data_files/iris_validation.dat", &
          validation_set(:, 1:ninput+nclasses), &
          (/nvalidations, ninput + nclasses/))
  call imp("../iris_data_files/iris_test.dat", &
          test_set(:, 1:ninput+nclasses), &
          (/ntests, ninput + nclasses/))
  call output_to_class(training_set(:, ninput+1:ninput+nclasses), tmp1)
  training_set(:, ninput+nclasses+1:ninput+nclasses+1) = reshape(tmp1, (/ntrainings, 1/))
  call output_to_class(validation_set(:, ninput+1:ninput+nclasses), tmp2)
  validation_set(:, ninput+nclasses+1:ninput+nclasses+1) = reshape(tmp2, (/nvalidations, 1/))
  call output_to_class(test_set(:, ninput+1:ninput+nclasses), tmp3)
  test_set(:, ninput+nclasses+1:ninput+nclasses+1) = reshape(tmp3, (/ntests, 1/))
! Train
  call train(training_set, validation_set, test_set, weights, ntrainings, &
          nvalidations, ntests, ninput, nclasses, regerror, claserror)
end program fnn
