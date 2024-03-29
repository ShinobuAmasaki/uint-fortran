module power_m
   use :: iso_fortran_env
   use :: uint8_t
   use :: uint16_t 
   use :: uint32_t
   implicit none
   private

   public :: operator(**)
   interface operator(**)
      module procedure :: uint8_pow_uint8
      module procedure :: uint8_pow_int16, int16_pow_uint8
      module procedure :: uint8_pow_int32, int32_pow_uint8
      module procedure :: uint8_pow_int64, int64_pow_uint8 

      module procedure :: uint16_pow_uint16
      module procedure :: uint16_pow_int8, int8_pow_uint16
      module procedure :: uint16_pow_int16, int16_pow_uint16
      module procedure :: uint16_pow_int32, int32_pow_uint16
      module procedure :: uint16_pow_int64, int64_pow_uint16 

      module procedure :: uint32_pow_uint32
      module procedure :: uint32_pow_int32, int32_pow_uint32
      module procedure :: uint32_pow_int64, int64_pow_uint32
   end interface

contains

!-- uint8 ------------------------------------------------------------!

   pure elemental function uint8_pow_uint8(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua, ub
      integer(int64) :: res

      res = cast_to_int16(ua)**cast_to_int16(ub)
   end function  uint8_pow_uint8

   pure elemental function uint8_pow_int8 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int8), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int16(ua) ** int(b, kind=int16)
   end function uint8_pow_int8

   pure elemental function int8_pow_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int16(ub)
   end function int8_pow_uint8

   pure elemental function uint8_pow_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int16(ua) ** b
   end function uint8_pow_int16

   pure elemental function int16_pow_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int16(ub)
   end function int16_pow_uint8

   pure elemental function uint8_pow_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int16(ua) ** b
   end function uint8_pow_int32

   pure elemental function int32_pow_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int16(ub)
   end function int32_pow_uint8

   pure elemental function uint8_pow_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int16(ua) ** b
   end function uint8_pow_int64

   pure elemental function int64_pow_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int16(ub)
   end function int64_pow_uint8

!-- uint16 -----------------------------------------------------------!
   pure elemental function uint16_pow_uint16(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      integer(int64) :: res

      res = cast_to_int32(ua)**cast_to_int32(ub)
   end function  uint16_pow_uint16

   pure elemental function uint16_pow_int8 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int8), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int32(ua) ** int(b, kind=int16)
   end function uint16_pow_int8

   pure elemental function int8_pow_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int8), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int32(ub)
   end function int8_pow_uint16

   pure elemental function uint16_pow_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int32(ua) ** b
   end function uint16_pow_int16

   pure elemental function int16_pow_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int32(ub)
   end function int16_pow_uint16

   pure elemental function uint16_pow_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int32(ua) ** b
   end function uint16_pow_int32

   pure elemental function int32_pow_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int32(ub)
   end function int32_pow_uint16

   pure elemental function uint16_pow_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int32(ua) ** b
   end function uint16_pow_int64

   pure elemental function int64_pow_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int32(ub)
   end function int64_pow_uint16
!-- uint32 -----------------------------------------------------------!

   pure elemental function uint32_pow_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      integer(int64) :: res
      res = cast_to_int64(ua)**cast_to_int64(ub)

   end function uint32_pow_uint32


   pure elemental function uint32_pow_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int64(ua) ** b
   end function uint32_pow_int32

   
   pure elemental function int32_pow_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a ** cast_to_int64(ub)
   end function int32_pow_uint32
   

   pure elemental function uint32_pow_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = cast_to_int64(ua) ** b
   end function uint32_pow_int64

   
   pure elemental function int64_pow_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a ** cast_to_int64(ub)
   end function int64_pow_uint32

end module power_m