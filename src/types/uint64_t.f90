module uint64_t
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none
   
   private

   integer(int64), parameter, public :: INT64_UPPER_LIMIT = 9223372036854775807_int64
   integer(int64), parameter, public :: INT64_LOWER_LIMIT = INT64_UPPER_LIMIT+1

   type, public, bind(c) :: uint64
      integer(c_int64_t) :: u64
   end type uint64

end module uint64_t 