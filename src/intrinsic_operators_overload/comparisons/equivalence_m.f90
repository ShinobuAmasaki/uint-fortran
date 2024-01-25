module equivalence_m
   use :: uint8_t
   use :: uint16_t 
   use :: uint32_t 
   use :: uint64_t
   implicit none

   public :: operator(==)
   interface operator(==)
      module procedure :: uint8_eq_uint8
      module procedure :: uint8_eq_int8, int8_eq_uint8
      module procedure :: uint8_eq_int16, int16_eq_uint8
      module procedure :: uint8_eq_int32, int32_eq_uint8
      module procedure :: uint8_eq_int64, int64_eq_uint8
      

      module procedure :: uint16_eq_uint16
      module procedure :: uint16_eq_int16, int16_eq_uint16
      module procedure :: uint16_eq_int32, int32_eq_uint16
      module procedure :: uint16_eq_int64, int64_eq_uint16

      module procedure :: uint32_eq_uint32
      module procedure :: uint32_eq_int32, int32_eq_uint32
      module procedure :: uint32_eq_int64, int64_eq_uint32

      module procedure :: uint64_eq_uint64
      module procedure :: uint64_eq_int32, int32_eq_uint64
      module procedure :: uint64_eq_int64, int64_eq_uint64
   end interface 
   
   !---------------------------------------------------------!
   public :: operator(/=)
   interface operator(/=)
      module procedure :: uint8_ne_uint8
      module procedure :: uint8_ne_int8, int8_ne_uint8
      module procedure :: uint8_ne_int16, int16_ne_uint8
      module procedure :: uint8_ne_int32, int32_ne_uint8
      module procedure :: uint8_ne_int64, int64_ne_uint8

      module procedure :: uint16_ne_uint16
      module procedure :: uint16_ne_int16, int16_ne_uint16
      module procedure :: uint16_ne_int32, int32_ne_uint16
      module procedure :: uint16_ne_int64, int64_ne_uint16
   
      module procedure :: uint32_ne_uint32
      module procedure :: uint32_ne_int32, int32_ne_uint32
      module procedure :: uint32_ne_int64, int64_ne_uint32
      
      module procedure :: uint64_ne_uint64
      module procedure :: uint64_ne_int32, int32_ne_uint64
      module procedure :: uint64_ne_int64, int64_ne_uint64
   end interface 

contains
   
!== equal to =========================================================!

!-- uint8 ------------------------------------------------------------!
   pure elemental function uint8_eq_uint8 (ua, ub) result(res)
      implicit none
      type(uint8), intent(in) :: ua, ub
      logical :: res

      res = ua%u8 == ub%u8
   end function uint8_eq_uint8
   
   pure elemental function uint8_eq_int8 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int8), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) == b
   end function uint8_eq_int8

   pure elemental function int8_eq_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a == cast_to_int16(ub)
   end function int8_eq_uint8

   pure elemental function uint8_eq_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) == b
   end function uint8_eq_int16

   pure elemental function int16_eq_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a == cast_to_int16(ub)
   end function int16_eq_uint8

   pure elemental function uint8_eq_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) == b
   end function uint8_eq_int32

   pure elemental function int32_eq_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a == cast_to_int16(ub)
   end function int32_eq_uint8

   pure elemental function uint8_eq_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) == b
   end function uint8_eq_int64

   pure elemental function int64_eq_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a == cast_to_int16(ub)
   end function int64_eq_uint8

!-- uint16 -----------------------------------------------------------!

   pure elemental function uint16_eq_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) == cast_to_int32(ub)
   end function uint16_eq_uint16

   pure elemental function uint16_eq_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) == b
   end function uint16_eq_int16

   pure elemental function int16_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a == cast_to_int32(ub)
   end function int16_eq_uint16

   pure elemental function uint16_eq_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) == b
   end function uint16_eq_int32

   pure elemental function int32_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a == cast_to_int32(ub)
   end function int32_eq_uint16

   pure elemental function uint16_eq_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) == b
   end function uint16_eq_int64

   pure elemental function int64_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a == cast_to_int32(ub)
   end function int64_eq_uint16

!-- uint32 -----------------------------------------------------------!

   pure elemental function uint32_eq_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = ua%u32 == ub%u32
   end function 
   
   
   pure elemental function uint32_eq_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) == b 
   end function uint32_eq_int32
   
   
   pure elemental function uint32_eq_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) == b
   end function uint32_eq_int64
   
   
   pure elemental function int32_eq_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a == cast_to_int64(ub)
   end function int32_eq_uint32
   
   
   pure elemental function int64_eq_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a == cast_to_int64(ub)
   end function int64_eq_uint32

!-- uint64 -----------------------------------------------------------!

   pure elemental function uint64_eq_uint64 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua, ub
      logical :: res  
      res = ua%u64 == ub%u64
   end function
   
   pure elemental function uint64_eq_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      if (b < 0) then
      	res = .false.
      else if (ua%u64 < 0) then
         res = .false.
      else
      	res = ua%u64 == int(b, kind=int64)
      end if 
   end function uint64_eq_int32
   
   
   pure elemental function uint64_eq_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      if (b < 0) then
      	res = .false.
      else if (ua%u64 < 0) then
         res = .false.
      else
	      res = ua%u64 == b
	   end if 
   end function uint64_eq_int64
   
   
   pure elemental function int32_eq_uint64(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res
      
      if (a < 0) then
      	res = .false.
      else if (ub%u64 < 0) then
         res = .false.
      else
      	res = int(a, kind=int64) == ub%u64
      end if
   end function int32_eq_uint64
   
   
   pure elemental function int64_eq_uint64(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      
      if (a < 0) then
	      res = .false.
	   else if (ub%u64 < 0) then
         res = .false.
      else
	   	res = a == ub%u64
	   end if 
   end function int64_eq_uint64

!== Not equal ========================================================!

!-- uint8 ------------------------------------------------------------!
   
   pure elemental function uint8_ne_uint8 (ua, ub) result(res)
      implicit none
      type(uint8), intent(in) :: ua, ub
      logical :: res

      res = ua%u8 /= ub%u8
   end function uint8_ne_uint8
   
   pure elemental function uint8_ne_int8 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int8), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) /= b
   end function uint8_ne_int8

   pure elemental function int8_ne_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int16(ub)
   end function int8_ne_uint8

   pure elemental function uint8_ne_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) /= b
   end function uint8_ne_int16

   pure elemental function int16_ne_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int16(ub)
   end function int16_ne_uint8

   pure elemental function uint8_ne_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) /= b
   end function uint8_ne_int32

   pure elemental function int32_ne_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int16(ub)
   end function int32_ne_uint8

   pure elemental function uint8_ne_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res

      res = cast_to_int16(ua) /= b
   end function uint8_ne_int64

   pure elemental function int64_ne_uint8(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int16(ub)
   end function int64_ne_uint8

   pure elemental function uint16_ne_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) /= cast_to_int32(ub)
   end function uint16_ne_uint16

   pure elemental function uint16_ne_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) /= b
   end function uint16_ne_int16

   pure elemental function int16_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a /= cast_to_int32(ub)
   end function int16_ne_uint16

   pure elemental function uint16_ne_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) /= b
   end function uint16_ne_int32

   pure elemental function int32_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int32(ub)
   end function int32_ne_uint16

   pure elemental function uint16_ne_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) /= b
   end function uint16_ne_int64

   pure elemental function int64_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int32(ub)
   end function int64_ne_uint16

   pure elemental function uint32_ne_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = ua%u32 /= ub%u32
   end function 
   
   
   pure elemental function uint32_ne_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) /= b 
   end function uint32_ne_int32
   
   
   pure elemental function uint32_ne_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) /= b
   end function uint32_ne_int64
   
   
   pure elemental function int32_ne_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a /= cast_to_int64(ub)
   end function int32_ne_uint32
   
   
   pure elemental function int64_ne_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a /= cast_to_int64(ub)
   end function int64_ne_uint32

!-- uint64 -----------------------------------------------------------!

   pure elemental function uint64_ne_uint64 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua, ub
      logical :: res  
      res = ua%u64 /= ub%u64
   end function
   
   pure elemental function uint64_ne_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      if (b < 0) then
      	res = .true.
      else if (ua%u64 < 0) then
         res = .true.
      else
      	res = ua%u64 /= int(b, kind=int64)
      end if 
   end function uint64_ne_int32
   
   
   pure elemental function uint64_ne_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      if (b < 0) then
      	res = .true.
      else if (ua%u64 < 0) then
         res = .true.
      else
	      res = ua%u64 /= b
	   end if 
   end function uint64_ne_int64
   
   
   pure elemental function int32_ne_uint64(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res
      
      if (a < 0) then
      	res = .true.
      else if (ub%u64 < 0) then
         res = .true.
      else
      	res = int(a, kind=int64) /= ub%u64
      end if
   end function int32_ne_uint64
   
   
   pure elemental function int64_ne_uint64(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      
      if (a < 0) then
	      res = .true.
	   else if (ub%u64 < 0) then
         res = .true.
      else
	   	res = a /= ub%u64
	   end if 
   end function int64_ne_uint64

end module equivalence_m