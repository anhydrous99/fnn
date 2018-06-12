subroutine train(training_set, validation_set, test_set, weights, ntrainsamples, nvalidationsamples, ntestsamples, &
                 ninputs, nclasses, regerror, claserror)
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
!
! Implementation
!
  ! Fill bias with ones
  training_bias = 1
  balidation_bias = 1
  test_bias = 1
  ! Fill weights matrix with random numbers
  call fill_matrix_rand(weights, max_weight)
  do i = 1, max_iterations
    call update_backpropagation(training_set(1:ntrainsamples, 1:ninputs), weights, learning_rate, training_bias, &
            (/ntrainamples, ninputs), (/ninputs + 1, nclasses/), (/ntrainsamples, 1/))
    ! TODO

  end do
end subroutine train
