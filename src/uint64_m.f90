module uint64_m
   use :: uint64_t
   use :: assignment_m, only: assignment(=)
   use :: addition_m, only: operator(+)
   use :: subtraction_m, only: operator(-)
   ! No other arithmetic operators will be provided. 

   use :: equivalence_m, only: operator(==), operator(/=)


end module uint64_m