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


   !==================================================================!
   ! Casting

   pure integer(int64) function cast_to_int64(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      
      cast_to_int64 = validate(ua)

   end function cast_to_int64


   pure real(real64) function cast_to_dble(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua

      cast_to_dble = dble(validate(ua))
   end function cast_to_dble


   pure real(real32) function cast_to_real(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      cast_to_real = real(validate(ua))
   end function cast_to_real


   pure elemental function  cast_to_complex_64_re(ua, im) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      real(real64), intent(in) :: im
      complex(real64) :: res
      res = cmplx(validate(ua), im, kind(0d0))
   end function cast_to_complex_64_re

   pure elemental function  cast_to_complex_64_im(re, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      real(real64), intent(in) :: re
      complex(real64) :: res
      res = cmplx(re, validate(ua), kind(0d0))
   end function cast_to_complex_64_im


end module uint32_t