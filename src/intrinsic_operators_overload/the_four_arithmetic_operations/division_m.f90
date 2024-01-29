module division_m
   use, intrinsic :: iso_fortran_env
   use :: uint8_t
   use :: uint16_t 
   use :: uint32_t
   use :: uint64_t
   use :: assignment_m
   implicit none
   
   public :: operator( / )
   interface operator(/)
      module procedure :: uint8_div_uint8
      module procedure :: uint8_div_int8, int8_div_uint8
      module procedure :: uint8_div_int16, int16_div_uint8
      module procedure :: uint8_div_int32, int32_div_uint8
      module procedure :: uint8_div_int64, int64_div_uint8

      module procedure :: uint16_div_int16, int16_div_uint16
      module procedure :: uint16_div_int32, int32_div_uint16
      module procedure :: uint16_div_int64, int64_div_uint16
      module procedure :: uint16_div_uint16

      module procedure :: uint32_div_uint32
      module procedure :: uint32_div_int32, int32_div_uint32
      module procedure :: uint32_div_int64, int64_div_uint32
      
      module procedure :: uint64_div_uint64
      module procedure :: uint64_div_int32
      module procedure :: uint64_div_int64
   end interface 


contains
!-- uint8 ------------------------------------------------------------!

   pure elemental function uint8_div_uint8 (ua, ub) result(res)
      implicit none
      type(uint8), intent(in) :: ua, ub
      integer(int32) :: res

      res = int(cast_to_int16(ua) / cast_to_int16(ub), kind=int32)
   end function uint8_div_uint8

   pure elemental function uint8_div_int8 (ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int8), intent(in) :: b
      integer(int32) :: res

      res = int(cast_to_int16(ua) / b, kind=int32)
   end function uint8_div_int8

   pure elemental function int8_div_uint8(a, ub) result(res)
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int32) :: res

      res = int(a / cast_to_int16(ub), kind=int32)
   end function int8_div_uint8

   pure elemental function uint8_div_int16 (ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      integer(int32) :: res

      res = int(cast_to_int16(ua) / b, kind=int32)
   end function uint8_div_int16

   pure elemental function int16_div_uint8(a, ub) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int32) :: res

      res = int(a / cast_to_int16(ub), kind=int32)
   end function int16_div_uint8


   pure elemental function uint8_div_int32 (ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int32) :: res

      res = int(cast_to_int16(ua) / b, kind=int32)
   end function uint8_div_int32

   pure elemental function int32_div_uint8(a, ub) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int32) :: res

      res = int(a / cast_to_int16(ub), kind=int32)
   end function int32_div_uint8

   pure elemental function uint8_div_int64 (ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res

      res = int(cast_to_int16(ua) / b, kind=int64)
   end function uint8_div_int64

   pure elemental function int64_div_uint8(a, ub) result(res)
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = int(a / cast_to_int16(ub), kind=int64)
   end function int64_div_uint8

!-- uint16 -----------------------------------------------------------!

   pure elemental function uint16_div_uint16 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      integer(int32) :: res

      res = cast_to_int32(ua) / cast_to_int32(ub)
   end function

   pure elemental function int16_div_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a / cast_to_int32(ua) 
   end function


   pure elemental function uint16_div_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) / a  
   end function


   pure elemental function uint16_div_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) / a
   end function

   pure elemental function int32_div_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a / cast_to_int32(ua)  
   end function   
 
   
   pure elemental function int64_div_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = a / cast_to_int32(ua)  
   end function


   pure elemental function uint16_div_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = cast_to_int32(ua) / a  
   end function

!-- uint32 -----------------------------------------------------------!

   pure elemental function uint32_div_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      integer(int64) :: res

      res = cast_to_int64(ua) / cast_to_int64(ub)
   end function uint32_div_uint32


   pure elemental function uint32_div_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int64(ua) / b
   end function 

   
   pure elemental function int32_div_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a / cast_to_int64(ub)
   end function int32_div_uint32
   

   pure elemental function uint32_div_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = cast_to_int64(ua) / b
   end function uint32_div_int64

   
   pure elemental function int64_div_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a / cast_to_int64(ub)
   end function int64_div_uint32

!-- uint64 -----------------------------------------------------------!

   pure elemental function uint64_div_uint64 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua, ub
      integer(int64) :: res

      if (ua%u64 >= 0 .and. ub%u64 >= 0) then
         res = ua%u64 / ub%u64
      
      else if (ua%u64 < 0 .and. ub%u64 >=0) then
         res = uint64_div_int64(ua, ub%u64)
      
      else if (ua%u64 >= 0 .and. ub%u64 < 0) then
         res = 0

      else if (ua%u64 < 0 .and. ub%u64 < 0) then
         if (ua%u64 >= ub%u64) then
            res = 1
         else
            res = 0
         end if
      end if
   end function uint64_div_uint64


   pure elemental function uint64_div_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b

      integer(int64) :: res, b_8

      b_8 = b

      res = uint64_div_int64(ua, b_8)

   end function uint64_div_int32
      

   pure elemental function uint64_div_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      use :: greater_than_m
      use :: subtraction_m
      use :: assignment_m

      implicit none
      type(uint64), intent(in) :: ua 
      integer(int64), intent(in) :: b

      type(uint64) :: dividend
      type(uint64) :: cache, ans
      integer(int64) :: divisor 
      integer(int64) :: res
      logical :: is_minus
       

      dividend = ua
      divisor = b

      if (ua%u64 >= 0) then
         res = ua%u64 / divisor
         return
      end if


      is_minus = .false. 
      if (divisor < 0) then
         is_minus = .true.
         divisor = - divisor
      end if


      block
         integer(int64) :: shifted, k

         shifted = divisor
         k = 0
         do while (shifted > 0)
            k = k + 1
            shifted = ishft(shifted, -1)
         end do
         k = 64 - k


         ans = 0
         shifted = divisor
         
         do while (k >= 0)

            shifted = int(ishft(divisor, k), kind=int64) 

            if (dividend >= shifted) then
               dividend = dividend - shifted
               
               cache = ishft(1, k)
               
               ans = ibset(ans%u64, k)

            end if

            k = k -1
         end do

      end block

      res = ans%u64

      if (is_minus) res = - res
         
   end function uint64_div_int64

end module division_m