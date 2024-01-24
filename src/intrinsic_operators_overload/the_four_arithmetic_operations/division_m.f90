module division_m
   use, intrinsic :: iso_fortran_env
   use :: uint16_t 
   use :: uint32_t
   use :: assignment_m
   implicit none
   
   public :: operator( / )
   interface operator(/)
      module procedure :: uint16_div_int16, int16_div_uint16
      module procedure :: uint16_div_int32, int32_div_uint16
      module procedure :: uint16_div_int64, int64_div_uint16
      module procedure :: uint16_div_uint16

      module procedure :: uint32_div_uint32
      module procedure :: uint32_div_int32, int32_div_uint32
      module procedure :: uint32_div_int64, int64_div_uint32
   end interface 


contains
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


end module division_m