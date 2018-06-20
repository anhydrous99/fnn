subroutine train(training_set, validation_set, test_set, weights, ntrainsamples, nvalidationsamples, &
                ntestsamples, ninputs, nclasses, regerror, claserror)
!
! Use Statement
!
use mod_numeric, only: int_kind, & ! kind for Interger vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: fill_matrix_rand
!
! Implicit Statement
!
implicit none
!
! Var Decleration
!
intrinsic mod
integer(kind=int_kind), intent(in) :: ntrainsamples, nvalidationsamples, ntestsamples, ninputs, nclasses
real(kind=real_kind), intent(out)  :: weights(ninputs + 1, nclasses), regerror(3), claserror(3)
real(kind=real_kind), intent(in)   :: training_set(ntrainsamples, ninputs + nclasses + 1), &
                                      validation_set(nvalidationsamples, ninputs + nclasses + 1), &
                                      test_set(ntestsamples, ninputs + nclasses + 1)
! Parameters
real(kind=real_kind), parameter   :: max_weight = 0.5,    &
                                     learning_rate = 0.1, &
                                     validation_threshold = 0.1
integer(kind=int_kind), parameter :: max_iterations = 1000
! Tmp vars
integer(kind=int_kind) :: i
real(kind=real_kind)   :: training_bias(ntrainsamples,1),         &
                          validation_bias(nvalidationsamples, 1), &
                          test_bias(ntestsamples,1)
real(kind=real_kind), dimension(max_iterations) :: training_regression_error, training_classification_error,     &
                                                   validation_regression_error, validation_classification_error, &
                                                   test_regression_error, test_classification_error
!
! Implementation
!
  ! Fill bias with ones
  training_bias = 1
  validation_bias = 1
  test_bias = 1
  ! Fill weights matrix with random numbers
  call fill_matrix_rand(weights, max_weight)
  do i = 1, max_iterations
    ! Update Weights
    call update_backpropagation(training_set(1:ntrainsamples, 1:ninputs), weights, &
            learning_rate, training_bias, (/ntrainsamples, ninputs/),                &
            (/ninputs + 1, nclasses/), (/ntrainsamples, 1/))
    ! Evaluate Network against training samples
    call eval_network(training_set(1:ntrainsamples, 1:ninputs), weights, training_bias,              &
                      training_set(1:ntrainsamples, ninputs+1:ninputs+nclasses),                     &
                      training_set(1:ntrainsamples, ninputs+nclasses+1:ninputs+nclasses+1),          &
                      training_regression_error(i), training_classification_error(i),                &
                      (/ntrainsamples, ninputs/), (/ninputs+1, nclasses/), (/ntrainsamples,1/))
    ! Evaluate Network against validation samples
    call eval_network(validation_set(1:nvalidationsamples, 1:ninputs), weights, validation_bias,     &
                      validation_set(1:nvalidationsamples, ninputs+1:ninputs+nclasses),              &
                      validation_set(1:nvalidationsamples, ninputs+nclasses+1:ninputs+nclasses+1),   &
                      validation_regression_error(i), validation_classification_error(i),            &
                      (/nvalidationsamples, ninputs/), (/ninputs+1, nclasses/), (/ntrainsamples,1/))
    ! Evaluate Network against test samples
    call eval_network(test_set(1:ntestsamples, 1:ninputs), weights, validation_bias,                 &
                      test_set(1:ntestsamples, ninputs+1:ninputs+nclasses),                          &
                      test_set(1:ntestsamples, ninputs+nclasses+1:ninputs+nclasses+1),               &
                      test_regression_error(i), test_classification_error(i),                        &
                      (/ntestsamples, ninputs/), (/ninputs+1, nclasses/), (/ntestsamples,1/))
    if (mod(i, 10) == 0) then
      write(*,*)
      write(*,*) " Iteration: ", i
      write(*,*) " Regression Training: ", training_regression_error(i),            &
                 " Classification Training: ", training_classification_error(i)
      write(*,*) " Regression Validation: ", validation_regression_error(i),        &
                 " Classification Validation: ", validation_classification_error(i)
      write(*,*) " Regreesion Test: ", test_regression_error(i),                    &
                 " Classification Test: ", test_classification_error(i)
    end if
    ! If threshold is reached stop loop
    if (validation_regression_error(i) < validation_threshold) exit
  end do
end subroutine train
