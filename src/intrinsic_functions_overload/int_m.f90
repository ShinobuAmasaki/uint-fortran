module int_m
   use, intrinsic :: iso_fortran_env
   use :: uint8_t
   use :: uint16_t
   use :: uint32_t
   implicit none

   public :: int2
   interface int2
      module procedure :: uint8_cast_to_int16
      module procedure :: uint16_cast_to_int16
      module procedure :: uint32_cast_to_int16
   end interface

   public :: int4
   interface int4
      module procedure :: uint8_cast_to_int32
      module procedure :: uint16_cast_to_int32
      module procedure :: uint32_cast_to_int32
   end interface

contains

   function uint8_cast_to_int16(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16) :: res

      res = int(cast_to_int16(ua), kind=int16)
   end function uint8_cast_to_int16

   function uint16_cast_to_int16(ua) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      integer(int16) :: res

      res = int(cast_to_int32(ua), kind=int16)
   end function uint16_cast_to_int16

   function uint32_cast_to_int16(ua) result(res)
      implicit none
      type(uint32), intent(in) :: ua
      integer(int16) :: res

      res = int(cast_to_int64(ua), kind=int16)
   end function uint32_cast_to_int16


   function uint8_cast_to_int32(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32) :: res

      res = int(cast_to_int16(ua), kind=int32)
   end function uint8_cast_to_int32

   function uint16_cast_to_int32(ua) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32) :: res

      res = int(cast_to_int32(ua), kind=int32)
   end function uint16_cast_to_int32

   function uint32_cast_to_int32(ua) result(res)
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32) :: res

      res = int(cast_to_int64(ua), kind=int32)
   end function uint32_cast_to_int32

end module int_m
   