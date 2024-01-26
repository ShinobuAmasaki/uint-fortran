module mod_m
   use :: uint8_t 
   use :: uint16_t 
   use :: uint32_t
   use :: uint64_t
   use :: assignment_m
   implicit none

   public :: mod
   interface mod
      module procedure :: uint8_mod_uint8
      module procedure :: uint8_mod_int8, int8_mod_uint8
      module procedure :: uint8_mod_int16, int16_mod_uint8
      module procedure :: uint8_mod_int32, int32_mod_uint8
      module procedure :: uint8_mod_int64, int64_mod_uint8

      module procedure :: uint16_mod_uint16
      module procedure :: uint16_mod_int16, int16_mod_uint16
      module procedure :: uint16_mod_int32, int32_mod_uint16
      module procedure :: uint16_mod_int64, int64_mod_uint16

      module procedure :: uint32_mod_uint32
      module procedure :: uint32_mod_int32, int32_mod_uint32
      module procedure :: uint32_mod_int64, int64_mod_uint32

      module procedure :: uint64_mod_int32
      module procedure :: uint64_mod_int64
   end interface

contains
   
   pure elemental function uint8_mod_uint8 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua, ub
      type(uint8) :: res

      res = mod(cast_to_int16(ua), cast_to_int16(ub))
   end function uint8_mod_uint8

   pure elemental function uint8_mod_int8 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8),intent(in) :: ua
      integer(int8), intent(in) :: b 
      type(uint8) :: res

      res = mod(cast_to_int16(ua), b)
   end function uint8_mod_int8

   pure elemental function int8_mod_uint8 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int8) :: res

      res = mod(a, cast_to_int16(ub))
   end function int8_mod_uint8

   pure elemental function uint8_mod_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      type(uint8) :: res 

      res = mod(cast_to_int16(ua), b)
   end function uint8_mod_int16

   pure elemental function int16_mod_uint8 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int16) :: res

      res = mod(a, cast_to_int16(ub))
   end function int16_mod_uint8

   pure elemental function uint8_mod_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      type(uint8) :: res 

      res = mod(cast_to_int16(ua), b)
   end function uint8_mod_int32

   pure elemental function int32_mod_uint8 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int32) :: res

      res = mod(a, cast_to_int16(ub))
   end function int32_mod_uint8

   pure elemental function uint8_mod_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      type(uint8) :: res 

      res = mod(cast_to_int16(ua), b)
   end function uint8_mod_int64

   pure elemental function int64_mod_uint8 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = mod(a, cast_to_int16(ub))
   end function int64_mod_uint8

!-- uint16 -----------------------------------------------------------!

    pure elemental function uint16_mod_uint16 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16) :: res

      res = mod(cast_to_int32(ua), cast_to_int32(ub))
   end function uint16_mod_uint16

   pure elemental function uint16_mod_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      type(uint16) :: res 

      res = mod(cast_to_int32(ua), b)
   end function uint16_mod_int16

   pure elemental function int16_mod_uint16 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int16) :: res

      res = mod(a, cast_to_int32(ub))
   end function int16_mod_uint16

   pure elemental function uint16_mod_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      type(uint16) :: res 

      res = mod(cast_to_int32(ua), b)
   end function uint16_mod_int32

   pure elemental function int32_mod_uint16 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int32) :: res

      res = mod(a, cast_to_int32(ub))
   end function int32_mod_uint16

   pure elemental function uint16_mod_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      type(uint16) :: res 

      res = mod(cast_to_int32(ua), b)
   end function uint16_mod_int64

   pure elemental function int64_mod_uint16 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      integer(int64) :: res

      res = mod(a, cast_to_int32(ub))
   end function int64_mod_uint16

!-- uint32 -----------------------------------------------------------!

   pure elemental function uint32_mod_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res

      res = mod(cast_to_int64(ua), cast_to_int64(ub))
   end function uint32_mod_uint32


   pure elemental function uint32_mod_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = mod(cast_to_int64(ua),b)
   end function 

   
   pure elemental function int32_mod_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = mod(a,cast_to_int64(ub))
   end function int32_mod_uint32
   

   pure elemental function uint32_mod_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = mod( cast_to_int64(ua), b)
   end function uint32_mod_int64

   
   pure elemental function int64_mod_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = mod(a, cast_to_int64(ub))
   end function int64_mod_uint32

!-- uint64 -----------------------------------------------------------!
 
   pure elemental function uint64_mod_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      type(uint64) :: res

      if (ua%u64 >= 0 ) then
         res = mod(ua%u64, b)
      else  
         block
            integer(int64) :: upper, lower, diff, diff_u, diff_l
            integer(int64), parameter :: separator = 10_8**2
            integer(int32) :: n

            n = b

            diff = (ua%u64 - INT64_LOWER_LIMIT)
            diff_u = diff/separator 
            diff_l = mod(diff, separator)
   
            upper = INT64_UPPER_LIMIT/separator + diff_u
            lower = mod(INT64_UPPER_LIMIT, separator) + diff_l + 1

            res = mod( mod(upper, n)*mod(separator, n) + mod(lower, n), n)

         end block
      end if

   end function uint64_mod_int32


   pure elemental function uint64_mod_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b
      type(uint64) :: res

      if (ua%u64 >= 0 ) then
         res = mod(ua%u64, b)
      else  
         block
            integer(int64) :: upper, lower, diff, diff_u, diff_l
            integer(int64), parameter :: separator = 10_8**2
            integer(int64) :: n

            n = b

            diff = (ua%u64 - INT64_LOWER_LIMIT)
            diff_u = diff/separator 
            diff_l = mod(diff, separator)
   
            upper = INT64_UPPER_LIMIT/separator + diff_u
            lower = mod(INT64_UPPER_LIMIT, separator) + diff_l + 1

            res = mod( mod(upper, n)*mod(separator, n) + mod(lower, n), n)

         end block
      end if

   end function uint64_mod_int64

end module mod_m