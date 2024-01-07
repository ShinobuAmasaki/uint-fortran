module uint8_m
   use :: uint8_t
   use :: assignment_m, only: assignment(=)

   use :: addition_m,       only: operator(+)
   use :: subtraction_m,    only: operator(-)
   use :: multiplication_m, only: operator(*)
   use :: division_m,       only: operator( / )

   use :: greater_than_m, only: operator(>),  operator(>=)
   use :: less_than_m,    only: operator(<),  operator(<=)
   use :: equivalence_m,  only: operator(==), operator(/=)

   use :: mod_m, only: mod


   use :: uint_io_m

end module uint8_m