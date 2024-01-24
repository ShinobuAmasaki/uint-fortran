module addition_m
   use, intrinsic :: iso_fortran_env
   use :: uint8_t
   use :: uint16_t 
   use :: uint32_t
   use :: uint64_t
   use :: assignment_m
   implicit none
   private

   public :: operator(+)
   interface operator(+)
      module procedure :: uint8_add_uint16, uint16_add_uint8
      module procedure :: uint8_add_uint32, uint32_add_uint8
      module procedure :: uint16_add_uint32, uint32_add_uint16

      module procedure :: uint8_add_int8, int8_add_uint8
      module procedure :: uint8_add_int16, int16_add_uint8
      module procedure :: uint8_add_int32, int32_add_uint8
      module procedure :: uint8_add_int64, int64_add_uint8
      module procedure :: uint8_add_uint8

      module procedure :: uint16_add_int16, int16_add_uint16
      module procedure :: uint16_add_int32, int32_add_uint16
      module procedure :: uint16_add_int64, int64_add_uint16
      module procedure :: uint16_add_uint16

      module procedure :: uint32_add_uint32
      module procedure :: uint32_add_int32, int32_add_uint32
      module procedure :: uint32_add_int64, int64_add_uint32

      module procedure :: uint64_add_uint64
      module procedure :: uint64_add_int32, int32_add_uint64
      module procedure :: uint64_add_int64, int64_add_uint64
   end interface 
   
contains

   pure elemental function uint8_add_uint16 (ua, ub) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      type(uint16), intent(in) :: ub
      type(uint16) :: res

      res = int16_add_uint16(cast_to_int16(ua), ub)
   end function uint8_add_uint16


   pure elemental function uint16_add_uint8 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      type(uint8), intent(in) :: ub
      type(uint16) :: res 

      res = uint16_add_int16(ua, cast_to_int16(ub))
   end function uint16_add_uint8

   pure elemental function uint8_add_uint32 (ua, ub) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      type(uint32), intent(in) :: ub
      type(uint32) :: res

      res = int32_add_uint32(int(cast_to_int16(ua), kind=int32), ub)

   end function uint8_add_uint32

   pure elemental function uint32_add_uint8 (ua, ub) result(res)
      implicit none
      type(uint32), intent(in) :: ua
      type(uint8), intent(in) :: ub 
      type(uint32) :: res 

      res = uint32_add_int32(ua, int(cast_to_int16(ub), kind=int32))

   end function uint32_add_uint8

   pure elemental function uint16_add_uint32 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      type(uint32), intent(in) :: ub
      type(uint32) :: res

      res = int32_add_uint32(cast_to_int32(ua), ub)

   end function uint16_add_uint32

   pure elemental function uint32_add_uint16 (ua, ub) result(res)
      implicit none
      type(uint32), intent(in) :: ua
      type(uint16), intent(in) :: ub
      type(uint32) :: res

      res = uint32_add_int32(ua, cast_to_int32(ub))

   end function uint32_add_uint16


!-- uint8 ------------------------------------------------------------!

   pure elemental function uint8_add_uint8 (ua, ub) result(res)
      use :: iso_c_binding
      implicit none
      type(uint8), intent(in) :: ua, ub
      type(uint8) :: res

      res%u8 = int(cast_to_int16(ua) + cast_to_int16(ub), c_int8_t)
   end function uint8_add_uint8

   pure elemental function int8_add_uint8(a, ub) result(res)
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int16) :: res

      res = a + cast_to_int16(ub)
   end function int8_add_uint8

   pure elemental function uint8_add_int8(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int8), intent(in) :: b
      integer(int16) :: res

      res = cast_to_int16(ua) + b
   end function uint8_add_int8

   pure elemental function int16_add_uint8(a, ub) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int16) :: res

      res = a + cast_to_int16(ub)
   end function int16_add_uint8

   pure elemental function uint8_add_int16(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      integer(int16) :: res

      res = cast_to_int16(ua) + b
   end function uint8_add_int16

   pure elemental function int32_add_uint8(a, ub) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int32) :: res

      res = a + cast_to_int16(ub)
   end function int32_add_uint8

   pure elemental function uint8_add_int32(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int32) :: res

      res = cast_to_int16(ua) + b
   end function uint8_add_int32

   pure elemental function int64_add_uint8(a, ub) result(res)
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = a + cast_to_int16(ub)
   end function int64_add_uint8

   pure elemental function uint8_add_int64(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int16(ua) + b
   end function uint8_add_int64

!-- uint16 ------------------------------------------------------------!

   pure elemental function uint16_add_uint16 (ua, ub) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16) :: res

      res%u16 = int(cast_to_int32(ua) + cast_to_int32(ub), c_int16_t)

   end function uint16_add_uint16
      

   pure elemental function int16_add_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a + cast_to_int32(ua) 

   end function


   pure elemental function uint16_add_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) + a  

   end function


   pure elemental function uint16_add_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) + a  

   end function

   pure elemental function int32_add_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) + a  

   end function   
 
   
   pure elemental function int64_add_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = cast_to_int32(ua) + a  

   end function 


   pure elemental function uint16_add_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = cast_to_int32(ua) + a  

   end function

!-- uint32 ------------------------------------------------------------!

   pure elemental function uint32_add_uint32 (ua, ub) result(res)
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res

      res%u32 = ua%u32 + ub%u32
   end function uint32_add_uint32


   pure elemental function uint32_add_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int64(ua) + b
   end function 

   
   pure elemental function int32_add_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a + cast_to_int64(ub)
   end function int32_add_uint32
   

   pure elemental function uint32_add_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = cast_to_int64(ua) + b
   end function uint32_add_int64

   
   pure elemental function int64_add_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a + cast_to_int64(ub)
   end function int64_add_uint32

!-- uint64 -----------------------------------------------------------!

   pure elemental function uint64_add_uint64(ua,ub) result(res)
      implicit none
      type(uint64), intent(in) :: ua, ub
      type(uint64) :: res

      res = ua%u64 + ub%u64
   end function uint64_add_uint64

   pure elemental function uint64_add_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      type(uint64) :: res

      res = ua%u64 + b
   end function 

   pure elemental function int32_add_uint64 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint64), intent(in) :: ub
      type(uint64) :: res

      res = a + ub%u64
   end function

   pure elemental function uint64_add_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b
      type(uint64) :: res

      res = ua%u64 + b
   end function 

   pure elemental function int64_add_uint64 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint64), intent(in) :: ub
      type(uint64) :: res

      res = a + ub%u64
   end function 

end module addition_m