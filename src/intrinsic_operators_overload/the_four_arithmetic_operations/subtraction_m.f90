module subtraction_m
   use, intrinsic :: iso_fortran_env
   use :: uint16_t 
   use :: uint32_t
   use :: assignment_m
   implicit none
   private

   public :: operator(-)
   interface operator(-)
      module procedure :: uint16_sub_int16, int16_sub_uint16
      module procedure :: uint16_sub_int32, int32_sub_uint16
      module procedure :: uint16_sub_int64, int64_sub_uint16
      module procedure :: uint16_sub_uint16

      module procedure :: uint32_sub_uint32
      module procedure :: uint32_sub_int32, int32_sub_uint32
      module procedure :: uint32_sub_int64, int64_sub_uint32
      
   end interface

contains

    function uint16_sub_uint16(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16)  res
      
      res = cast_to_int32(ua) - cast_to_int32(ub)
   end function uint16_sub_uint16

   function int16_sub_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a - cast_to_int32(ua) 
   end function


   function uint16_sub_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) - a  
   end function


   function uint16_sub_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = cast_to_int32(ua) - a
   end function

   function int32_sub_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a - cast_to_int32(ua)  
   end function   
 
   
   function int64_sub_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = a - cast_to_int32(ua)  
   end function


   function uint16_sub_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = cast_to_int32(ua) - a  
   end function  


   function uint32_sub_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res

      res = cast_to_int64(ua) - cast_to_int64(ub)
   end function uint32_sub_uint32


   function uint32_sub_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = cast_to_int64(ua) - b
   end function 

   
   function int32_sub_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a - cast_to_int64(ub)
   end function int32_sub_uint32
   

   function uint32_sub_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = cast_to_int64(ua) - b
   end function uint32_sub_int64

   
   function int64_sub_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a - cast_to_int64(ub)
   end function int64_sub_uint32

end module subtraction_m