module uint32_m
   use :: uint32_t
   use :: assignment_m, only: assignment(=)

   use :: addition_m,       only: operator(+)
   use :: subtraction_m,    only: operator(-)
   use :: multiplication_m, only: operator(*)
   use :: division_m,       only: operator( / )
   use :: power_m,          only: operator(**)

   use :: greater_than_m, only: operator(>),  operator(>=)
   use :: less_than_m,    only: operator(<),  operator(<=)
   use :: equivalence_m,  only: operator(==), operator(/=)

   use :: mod_m, only: mod
   use :: int_m, only: int2, int4
   use :: dble_m, only: dble
   use :: real_m, only: real
end module uint32_m