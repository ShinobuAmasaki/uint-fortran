module equivalence_m
   use :: uint16_t 
   use :: uint32_t 
   implicit none

   public :: operator(==)
   interface operator(==)
      module procedure :: uint16_ne_uint16
      module procedure :: uint16_ne_int16, int16_ne_uint16
      module procedure :: uint16_ne_int32, int32_ne_uint16
      module procedure :: uint16_ne_int64, int64_ne_uint16

      module procedure :: uint32_eq_uint32
      module procedure :: uint32_eq_int32, int32_eq_uint32
      module procedure :: uint32_eq_int64, int64_eq_uint32
   end interface 
   
   !---------------------------------------------------------!
   public :: operator(/=)
   interface operator(/=)
      module procedure :: uint16_eq_uint16
      module procedure :: uint16_eq_int16, int16_eq_uint16
      module procedure :: uint16_eq_int32, int32_eq_uint16
      module procedure :: uint16_eq_int64, int64_eq_uint16
   
      module procedure :: uint32_ne_uint32
      module procedure :: uint32_ne_int32, int32_ne_uint32
      module procedure :: uint32_ne_int64, int64_ne_uint32
   end interface 

contains
   
   ! equal to
   function uint16_eq_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) == cast_to_int32(ub)
   end function uint16_eq_uint16

   function uint16_eq_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) == b
   end function uint16_eq_int16

   function int16_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a == cast_to_int32(ub)
   end function int16_eq_uint16

   function uint16_eq_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) == b
   end function uint16_eq_int32

   function int32_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a == cast_to_int32(ub)
   end function int32_eq_uint16

   function uint16_eq_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) == b
   end function uint16_eq_int64

   function int64_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a == cast_to_int32(ub)
   end function int64_eq_uint16

   function uint32_eq_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = ua%u32 == ub%u32
   end function 
   
   
   function uint32_eq_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) == b 
   end function uint32_eq_int32
   
   
   function uint32_eq_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) == b
   end function uint32_eq_int64
   
   
   function int32_eq_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a == cast_to_int64(ub)
   end function int32_eq_uint32
   
   
   function int64_eq_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a == cast_to_int64(ub)
   end function int64_eq_uint32


   ! Not equal
   function uint16_ne_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) /= cast_to_int32(ub)
   end function uint16_ne_uint16

   function uint16_ne_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) /= b
   end function uint16_ne_int16

   function int16_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a /= cast_to_int32(ub)
   end function int16_ne_uint16

   function uint16_ne_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) /= b
   end function uint16_ne_int32

   function int32_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int32(ub)
   end function int32_ne_uint16

   function uint16_ne_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) /= b
   end function uint16_ne_int64

   function int64_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a /= cast_to_int32(ub)
   end function int64_ne_uint16

   function uint32_ne_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = ua%u32 /= ub%u32
   end function 
   
   
   function uint32_ne_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) /= b 
   end function uint32_ne_int32
   
   
   function uint32_ne_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) /= b
   end function uint32_ne_int64
   
   
   function int32_ne_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a /= cast_to_int64(ub)
   end function int32_ne_uint32
   
   
   function int64_ne_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a /= cast_to_int64(ub)
   end function int64_ne_uint32


end module equivalence_m