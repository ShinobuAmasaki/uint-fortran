module uint8_t
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none
   private

   integer(int16), parameter, public :: UINT8_LIMIT = 255_int16
   
   type, public, bind(c) :: uint8
      integer(c_int8_t) :: u8
   end type uint8


   public :: cast_to_int16
   interface cast_to_int16
      module procedure :: cast_to_int16
   end interface


contains

      
   pure elemental function validate(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16) :: res
      
      res = ua%u8
      if (res < 0) res = res + (UINT8_LIMIT + 1_2)
   end function validate

   ! Casting
   pure elemental function cast_to_int16(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16) :: res

      res = validate(ua)
   end function cast_to_int16


end module uint8_t