module power_m
   use :: iso_fortran_env
   use :: uint16_t 
   use :: uint32_t
   implicit none
   private

   public :: operator(**)
   interface operator(**)
      module procedure :: uint32_pow_uint32
      module procedure :: uint32_pow_int32, int32_pow_uint32
      module procedure :: uint32_pow_int64, int64_pow_uint32
   end interface

contains

   function uint32_pow_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      integer(int64) :: res
      res = cast_to_int64(ua)**cast_to_int64(ub)

   end function uint32_pow_uint32


   function uint32_pow_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int64(ua) ** b
   end function uint32_pow_int32

   
   function int32_pow_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int64(ub)
   end function int32_pow_uint32
   

   function uint32_pow_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = cast_to_int64(ua) ** b
   end function uint32_pow_int64

   
   function int64_pow_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a ** cast_to_int64(ub)
   end function int64_pow_uint32

end module power_m