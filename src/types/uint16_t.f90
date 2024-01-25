module uint16_t
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none
   private
   
   integer(int32),public, parameter :: UINT16_LIMIT = 65535

   type, public, bind(c) :: uint16
      integer(c_int16_t) :: u16   !0から32767までは同一、-32768から-1の値の場合は+65536する  
   end type uint16

   public :: cast_to_int32
   interface cast_to_int32
      module procedure :: cast_to_int32
   end interface

contains

   pure elemental function  validate(ua)
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32) :: validate

      validate = ua%u16
      if (validate < 0) validate = validate + (UINT16_LIMIT +1)

   end function validate


   pure elemental function  cast_to_int32(ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32) :: res

      res = validate(ua)
   end function cast_to_int32

end module uint16_t
 