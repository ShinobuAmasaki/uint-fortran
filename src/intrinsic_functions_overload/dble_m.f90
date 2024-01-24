module dble_m
   use, intrinsic :: iso_fortran_env
   use :: uint8_t
   use :: uint16_t
   use :: uint32_t
   use :: uint64_t
   use :: int_m
   implicit none
   private

   public :: dble
   interface dble
      module procedure :: uint8_cast_to_real64
      module procedure :: uint16_cast_to_real64
      module procedure :: uint32_cast_to_real64
      module procedure :: uint64_cast_to_real64
   end interface dble

contains
   
   pure elemental function uint8_cast_to_real64(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      real(real64) :: res

      res = dble(int2(ua))
      
   end function uint8_cast_to_real64

   pure elemental function uint16_cast_to_real64(ua) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      real(real64) :: res

      res = dble(int4(ua))
   end function uint16_cast_to_real64

   pure elemental function uint32_cast_to_real64(ua) result(res)
      implicit none
      type(uint32), intent(in) :: ua
      real(real64) :: res

      res = dble(int(ua))
   end function uint32_cast_to_real64

   pure elemental function uint64_cast_to_real64 (ua) result(res)
      implicit none
      type(uint64), intent(in) :: ua
      real(real64) :: res

      if (ua%u64 > 0) then
         res = dble(ua%u64)
         return
      else
         block      
            integer(int64) :: upper, lower, diff, diff_u, diff_l
            integer(int64), parameter :: separator = 10_8**2
            
            diff = (ua%u64 - INT64_LOWER_LIMIT)
            diff_u = diff/separator 
            diff_l = mod(diff, separator)
   
            upper = INT64_UPPER_LIMIT/separator + diff_u
            lower = mod(INT64_UPPER_LIMIT, separator) + diff_l + 1

            res = dble(upper)*separator + dble(lower)

         end block
      end if
   end function uint64_cast_to_real64



end module dble_m