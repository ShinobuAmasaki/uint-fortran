module uint32_t
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none
   
   private

   integer(int64), parameter, public :: UINT32_LIMIT = 4294967295_int64

   type, public, bind(c) :: uint32
      integer(c_int32_t) :: u32
   end type uint32

   public :: cast_to_int64
   interface cast_to_int64
      module procedure :: cast_to_int64
   end interface

contains

   pure elemental function  validate(ua)
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64) :: validate

      validate = ua%u32
      if (validate < 0) validate = validate + (UINT32_LIMIT +1)
   
   end function validate

   ! Casting
   pure integer(int64) function cast_to_int64(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      
      cast_to_int64 = validate(ua)

   end function cast_to_int64

end module uint32_t