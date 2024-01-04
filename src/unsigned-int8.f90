module unsigned_int8
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none
   private

   integer(int16), parameter :: UINT8_LIMIT = 255_int16
   
   type, public, bind(c) :: uint8
      integer(c_int8_t) :: u8
   end type uint8

   public :: pick

   public :: assignment(=)
   interface assignment(=)
      module procedure :: assign_int8, assign_int16, assign_int32, assign_int64
   end interface 

   public :: operator(+)
   interface operator(+)
      module procedure :: uint8_add_int8, int8_add_uint8
      module procedure :: uint8_add_int16, int16_add_uint8
      module procedure :: uint8_add_int32, int32_add_uint8
      module procedure :: uint8_add_int64, int64_add_uint8
      module procedure :: uint8_add_uint8
   end interface 

   public :: int
   interface int
      module procedure :: cast_to_int32
   end interface

   public :: int2
   interface int2
      module procedure :: cast_to_int16
   end interface 

contains

   function unsign_int8(a) result(res)
      implicit none
      integer(int8), intent(in) :: a
      type(uint8) :: res

      res%u8 = a
   end function unsign_int8

   function unsign_int16(a) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint8) :: res

      if ((UINT8_LIMIT+1)/2 < a) then
         res%u8 = int(a - UINT8_LIMIT+1, c_int8_t)
      else
         res%u8 = int(a, c_int8_t)
      end if

   end function unsign_int16

   function unsign_int32(a) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint8) :: res

      if((UINT8_LIMIT+1)/2 < a) then
         res%u8 = int(a - (UINT8_LIMIT+1), c_int8_t)
      else
         res%u8 = int(a, c_int8_t)
      end if

   end function unsign_int32

   function unsign_int64(a) result(res)
      implicit none
      integer(int64), intent(in) :: a
      type(uint8) :: res

      if ((UINT8_LIMIT+1)/2 < a) then
         res%u8 = int(a - (UINT8_LIMIT+1), c_int8_t)
      else
         res%u8 = int(a, c_int8_t)
      end if
   end function unsign_int64
      
	function pick(ua) result(res)
	   implicit none
	   type(uint8), intent(in) :: ua
	   integer(int16) :: res
	   
	   res = ua%u8
	   if (res < 0) res = res + (UINT8_LIMIT + 1)
	end function pick

   ! Casting
   function cast_to_int16(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16) :: res

      res = pick(ua)
   end function cast_to_int16

   function cast_to_int32(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32) :: res

      res = pick(ua)
   end function cast_to_int32

   function cast_to_int64(ua) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64) :: res

      res = pick(ua)
   end function cast_to_int64



   !==================================================================!
   ! Assignment

   subroutine assign_int8(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int8), intent(in) :: a

      ua = unsign_int8(a)
   end subroutine assign_int8

   subroutine assign_int16(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int16), intent(in) :: a
      
      ua = unsign_int16(a)
   end subroutine assign_int16

   subroutine assign_int32(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int32), intent(in) :: a

      ua = unsign_int32(a)
   end subroutine assign_int32

   subroutine assign_int64(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int64), intent(in) :: a

      ua = unsign_int64(a)
   end subroutine assign_int64

   !==================================================================!
   ! Addition

   function uint8_add_uint8 (ua, ub) result(res)
      implicit none
      type(uint8), intent(in) :: ua, ub
      type(uint8) :: res

      res%u8 = int(pick(ua) + pick(ub), c_int8_t)
   end function uint8_add_uint8

   function int8_add_uint8(a, ub) result(res)
      implicit none
      integer(int8), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int16) :: res

      res = a + pick(ub)
   end function int8_add_uint8

   function uint8_add_int8(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int8), intent(in) :: b
      integer(int16) :: res

      res = pick(ua) + b
   end function uint8_add_int8

   function int16_add_uint8(a, ub) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int16) :: res

      res = a + pick(ub)
   end function int16_add_uint8

   function uint8_add_int16(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int16), intent(in) :: b
      integer(int16) :: res

      res = pick(ua) + b
   end function uint8_add_int16

   function int32_add_uint8(a, ub) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int32) :: res

      res = a + pick(ub)
   end function int32_add_uint8

   function uint8_add_int32(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int32) :: res

      res = pick(ua) + b
   end function uint8_add_int32

   function int64_add_uint8(a, ub) result(res)
      implicit none
      integer(int64), intent(in) :: a
      type(uint8), intent(in) :: ub
      integer(int64) :: res

      res = a + pick(ub)
   end function int64_add_uint8

   function uint8_add_int64(ua, b) result(res)
      implicit none
      type(uint8), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res

      res = pick(ua) + b
   end function uint8_add_int64


end module unsigned_int8