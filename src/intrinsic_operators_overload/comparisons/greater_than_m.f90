module greater_than_m
   use :: iso_fortran_env
   use :: uint16_t 
   use :: uint32_t 
   implicit none
   
   private

   public :: operator(>)
   interface operator(>)
      module procedure :: uint16_gt_uint16
      module procedure :: uint16_gt_int16, int16_gt_uint16 
      module procedure :: uint16_gt_int32, int32_gt_uint16
      module procedure :: uint16_gt_int64, int64_gt_uint16  

      module procedure :: uint32_gt_uint32
      module procedure :: uint32_gt_int32, int32_gt_uint32
      module procedure :: uint32_gt_int64, int64_gt_uint32      
   end interface

   public :: operator(>=)
   interface operator(>=)
      module procedure :: uint16_ge_uint16
      module procedure :: uint16_ge_int16, int16_ge_uint16
      module procedure :: uint16_ge_int32, int32_ge_uint16
      module procedure :: uint16_ge_int64, int64_ge_uint16
   
      module procedure :: uint32_ge_uint32
      module procedure :: uint32_ge_int32, int32_ge_uint32
      module procedure :: uint32_ge_int64, int64_ge_uint32
   end interface



contains

 ! greater than
   function uint16_gt_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) > cast_to_int32(ub)
   end function uint16_gt_uint16

   function uint16_gt_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) > b
   end function uint16_gt_int16

   function  int16_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a > cast_to_int32(ub)
   end function int16_gt_uint16

   function uint16_gt_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) > b
   end function uint16_gt_int32

   function int32_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a > cast_to_int32(ub)
   end function int32_gt_uint16

   function uint16_gt_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) > b
   end function uint16_gt_int64

   function int64_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a > cast_to_int32(ub)
   end function int64_gt_uint16

   
   ! greater or equal than
   function uint16_ge_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) >= cast_to_int32(ub)
   end function uint16_ge_uint16

   function uint16_ge_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) >= b
   end function uint16_ge_int16

   function int16_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a >= cast_to_int32(ub)
   end function int16_ge_uint16

   function uint16_ge_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) >= b
   end function uint16_ge_int32

   function int32_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a >= cast_to_int32(ub)
   end function int32_ge_uint16

   function uint16_ge_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) >= b
   end function uint16_ge_int64

   function int64_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a >= cast_to_int32(ub)
   end function int64_ge_uint16



     ! greater than
   function uint32_gt_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = cast_to_int64(ua) > cast_to_int64(ub)
   end function 


   function uint32_gt_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) > b 
   end function uint32_gt_int32


   function uint32_gt_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) > b
   end function uint32_gt_int64


   function int32_gt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a > cast_to_int64(ub)
   end function int32_gt_uint32


   function int64_gt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a > cast_to_int64(ub)
   end function int64_gt_uint32


   !----------------------------------------------------!  
   ! Greater or equal than
   function uint32_ge_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = cast_to_int64(ua) >= cast_to_int64(ub)
   end function 
   
   
   function uint32_ge_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) >= b 
   end function uint32_ge_int32
   
   
   function uint32_ge_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) >= b
   end function uint32_ge_int64
   
   
   function int32_ge_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a >= cast_to_int64(ub)
   end function int32_ge_uint32
   
   
   function int64_ge_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a >= cast_to_int64(ub)
   end function int64_ge_uint32

end module greater_than_m