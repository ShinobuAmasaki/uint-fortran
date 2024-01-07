module assignment_m
   use :: iso_fortran_env
   use :: uint8_t
   use :: uint16_t
   use :: uint32_t
   implicit none
   private
   
   public :: assignment(=)
   interface assignment(=)
      module procedure :: to_int32_assign_uint8
      module procedure :: to_int32_assign_uint16
      module procedure :: to_int64_assign_uint32 


      module procedure :: to_uint8_assign_int8
      module procedure :: to_uint8_assign_int16
      module procedure :: to_uint8_assign_int32
      module procedure :: to_uint8_assign_int64

      module procedure :: to_uint16_assign_int16
      module procedure :: to_uint16_assign_int32
      module procedure :: to_uint16_assign_int64

      module procedure :: to_uint32_assign_int16
      module procedure :: to_uint32_assign_int32
      module procedure :: to_uint32_assign_int64
      
   end interface

contains

   subroutine to_int32_assign_uint8(a, ub)
      implicit none
      type(uint8), intent(in) :: ub
      integer(int32), intent(out) :: a

      a = int(cast_to_int16(ub), kind=int32)
   end subroutine to_int32_assign_uint8

   subroutine to_int32_assign_uint16(a, ub)
      implicit none
      type(uint16), intent(in) :: ub
      integer(int32), intent(out) :: a

      a = cast_to_int32(ub)
   end subroutine to_int32_assign_uint16
      

   subroutine to_int64_assign_uint32(a, ub)
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(out) :: a

      a = cast_to_int64(ub)
   end subroutine to_int64_assign_uint32
      


   function to_uint8_unsign_int8(a) result(res)
      implicit none
      integer(int8), intent(in) :: a
      type(uint8) :: res

      res%u8 = a
   end function to_uint8_unsign_int8

   function to_uint8_unsign_int16(a) result(res)
      use :: iso_c_binding
      implicit none
      integer(int16), intent(in) :: a
      type(uint8) :: res

      if ((UINT8_LIMIT+1)/2 < a) then
         res%u8 = int(a - UINT8_LIMIT+1, c_int8_t)
      else
         res%u8 = int(a, c_int8_t)
      end if

   end function to_uint8_unsign_int16

   function to_uint8_unsign_int32(a) result(res)
      use :: iso_c_binding
      implicit none
      integer(int32), intent(in) :: a
      type(uint8) :: res

      if((UINT8_LIMIT+1)/2 < a) then
         res%u8 = int(a - (UINT8_LIMIT+1), c_int8_t)
      else
         res%u8 = int(a, c_int8_t)
      end if

   end function to_uint8_unsign_int32

   function to_uint8_unsign_int64(a) result(res)
      use :: iso_c_binding
      implicit none
      integer(int64), intent(in) :: a
      type(uint8) :: res

      if ((UINT8_LIMIT+1)/2 < a) then
         res%u8 = int(a - (UINT8_LIMIT+1), c_int8_t)
      else
         res%u8 = int(a, c_int8_t)
      end if
   end function to_uint8_unsign_int64

   subroutine to_uint8_assign_int8(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int8), intent(in) :: a

      ua = to_uint8_unsign_int8(a)
   end subroutine to_uint8_assign_int8

   subroutine to_uint8_assign_int16(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int16), intent(in) :: a
      
      ua = to_uint8_unsign_int16(a)
   end subroutine to_uint8_assign_int16

   subroutine to_uint8_assign_int32(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int32), intent(in) :: a

      ua = to_uint8_unsign_int32(a)
   end subroutine to_uint8_assign_int32

   subroutine to_uint8_assign_int64(ua, a)
      implicit none
      type(uint8), intent(out) :: ua
      integer(int64), intent(in) :: a

      ua = to_uint8_unsign_int64(a)
   end subroutine to_uint8_assign_int64

!=====================================================================!

   function to_uint16_unsign_int16(a) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint16) :: res

         res%u16 = a

   end function

   function to_uint16_unsign_int32(a) result(res)
      use :: iso_c_binding
      implicit none
      integer(int32), intent(in) :: a
      type(uint16) :: res

      if ((UINT16_LIMIT+1)/2 < a) then
         res%u16 = int(a - (UINT16_LIMIT+1), c_int16_t)
      else
         res%u16 = int(a, c_int16_t)
      end if
   end function


   function to_uint16_unsign_int64 (a) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(int64), intent(in) :: a
      type(uint16) :: res

      if ((UINT16_LIMIT+1)/2 < a) then
         res%u16 = int(a - (UINT16_LIMIT+1), c_int16_t)
      else
         res%u16 = int(a, c_int16_t)
      end if
   end function

   function to_uint32_unsign_int16(a) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint32) :: res

      res%u32 = a

   end function to_uint32_unsign_int16

   
   function to_uint32_unsign_int32(a) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint32) :: res

      res%u32 = a 
   end function to_uint32_unsign_int32
   
   
   function to_uint32_unsign_int64(a) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(int64), intent(in) :: a
      type(uint32) :: res

      if ((UINT32_LIMIT+1)/2 < a) then
         res%u32 = int(a - (UINT32_LIMIT+1), c_int)
      else
         res%u32 = int(a, c_int)
      end if
   end function to_uint32_unsign_int64










   subroutine to_uint16_assign_int16 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(out) :: ua
      integer(int16), intent(in) :: a

      ua = to_uint16_unsign_int16(a)

   end subroutine to_uint16_assign_int16


   subroutine to_uint16_assign_int32 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(out) :: ua
      integer(int32), intent(in) :: a

      ua = to_uint16_unsign_int32(a)

   end subroutine to_uint16_assign_int32


   subroutine to_uint16_assign_int64 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(out) :: ua
      integer(int64), intent(in) :: a

      ua = to_uint16_unsign_int64(a)

   end subroutine to_uint16_assign_int64


   subroutine to_uint32_assign_int16 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(out) :: ua
      integer(int16), intent(in) :: a

      ua = to_uint32_unsign_int16(a)

   end subroutine to_uint32_assign_int16


   subroutine to_uint32_assign_int32 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(out) :: ua
      integer(int32), intent(in) :: a

      ua = to_uint32_unsign_int32(a)

      ! if (a < 0) then
      !    error stop
      ! end if

   end subroutine to_uint32_assign_int32


   subroutine to_uint32_assign_int64 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(out) :: ua
      integer(int64), intent(in) :: a

      ua = to_uint32_unsign_int64(a)

   end subroutine to_uint32_assign_int64

end module assignment_m