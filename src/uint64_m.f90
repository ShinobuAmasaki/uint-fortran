module uint64_m
   use :: uint64_t
   use :: assignment_m, only: assignment(=)
   use :: addition_m, only: operator(+)
   use :: subtraction_m, only: operator(-)
   ! No other arithmetic operators will be provided. 

   use :: equivalence_m, only: operator(==), operator(/=)
   use :: greater_than_m, only: operator(>), operator(>=)
   use :: less_than_m, only: operator(<), operator(<=) 
   
   use :: mod_m, only: mod
   use :: dble_m, only: dble



end module uint64_m