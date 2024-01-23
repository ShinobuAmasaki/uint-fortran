module less_than_m
   use :: uint16_t 
   use :: uint32_t 
   use :: uint64_t
   implicit none
   private

   public :: operator(<)
   interface operator(<)
      module procedure :: uint16_lt_uint16
      module procedure :: uint16_lt_int16, int16_lt_uint16
      module procedure :: uint16_lt_int32, int32_lt_uint16
      module procedure :: uint16_lt_int64, int64_lt_uint16

      module procedure :: uint32_lt_uint32
      module procedure :: uint32_lt_int32, int32_lt_uint32
      module procedure :: uint32_lt_int64, int64_lt_uint32
   end interface

   !---------------------------------------------------------!      
   public :: operator(<=)
   interface operator(<=)
      module procedure :: uint16_le_uint16
      module procedure :: uint16_le_int16, int16_le_uint16
      module procedure :: uint16_le_int32, int32_le_uint16
      module procedure :: uint16_le_int64, int64_le_uint16

      module procedure :: uint32_le_uint32
      module procedure :: uint32_le_int32, int32_le_uint32
      module procedure :: uint32_le_int64, int64_le_uint32

      module procedure :: uint64_le_uint64
      module procedure :: uint64_le_int32, int32_le_uint64
      module procedure :: uint64_le_int64, int64_le_uint64
   end interface
   
contains
   
!== less than ========================================================!
!-- uint16 -----------------------------------------------------------!
   function uint16_lt_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) < cast_to_int32(ub)
   end function uint16_lt_uint16

   function uint16_lt_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) < b
   end function uint16_lt_int16

   function int16_lt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a < cast_to_int32(ub)
   end function int16_lt_uint16

   function uint16_lt_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) < b
   end function uint16_lt_int32

   function int32_lt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a < cast_to_int32(ub)
   end function int32_lt_uint16

   function uint16_lt_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) < b
   end function uint16_lt_int64

   function int64_lt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a < cast_to_int32(ub)
   end function int64_lt_uint16

!-- uint32 -----------------------------------------------------------!

   function uint32_lt_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = cast_to_int64(ua) < cast_to_int64(ub)
   end function 


   function uint32_lt_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) < b 
   end function uint32_lt_int32


   function uint32_lt_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) < b
   end function uint32_lt_int64


   function int32_lt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a < cast_to_int64(ub)
   end function int32_lt_uint32


   function int64_lt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a < cast_to_int64(ub)
   end function int64_lt_uint32

!-- uint64 -----------------------------------------------------------!
   function uint64_lt_uint64 (ua, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ua, ub
      logical :: res

      if (ua%u64 < 0 .and. ub%u64 < 0) then
         res = ua%u64 < ub%u64
      else
         if (ua%u64 < 0) then
            res = .false.
         else if (ub%u64 < 0) then
            res = .true.
         else
            res = ua%u64 < ub%u64 
         end if
      end if
   end function uint64_lt_uint64

   function uint64_lt_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      if (b < 0) then
         res = .false.
      else
         if (ua%u64 < 0) then
            res = .false.
         else
            res = ua%u64 < b
         end if
      end if
   end function uint64_lt_int32

   function uint64_lt_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .false.
      else
         if (ua%u64 < 0) then
            res = .false.
         else
            res = ua%u64 < b
         end if
      end if
   end function uint64_lt_int64

   function int32_lt_uint64 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint64), intent(in) :: ub
      logical :: res

      if (a < 0) then
         res = .true.
      else
         if (ub%u64 < 0) then
            res = .true.
         else
            res = a < ub%u64
         end if
      end if
   end function int32_lt_uint64

   function int64_lt_uint64 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint64), intent(in) :: ub
      logical :: res

      if (a < 0) then
         res = .true.
      else
         if (ub%u64 < 0) then
            res = .true.
         else
            res = a < ub%u64
         end if
      end if
   end function int64_lt_uint64


!== less then or equal ===============================================!
!-- uint16 -----------------------------------------------------------!

   function uint16_le_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) <= cast_to_int32(ub)
   end function uint16_le_uint16

   function uint16_le_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) <= b
   end function uint16_le_int16

   function int16_le_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a <= cast_to_int32(ub)
   end function int16_le_uint16

   function uint16_le_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) <= b
   end function uint16_le_int32

   function int32_le_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a <= cast_to_int32(ub)
   end function int32_le_uint16

   function uint16_le_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) <= b
   end function uint16_le_int64

   function int64_le_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a <= cast_to_int32(ub)
   end function int64_le_uint16

!-- uint32 -----------------------------------------------------------!

   function uint32_le_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = cast_to_int64(ua) <= cast_to_int64(ub)
   end function 


   function uint32_le_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) <= b 
   end function uint32_le_int32


   function uint32_le_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) <= b
   end function uint32_le_int64


   function int32_le_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a <= cast_to_int64(ub)
   end function int32_le_uint32


   function int64_le_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a <= cast_to_int64(ub)
   end function int64_le_uint32

!-- uint64 -----------------------------------------------------------!
   function uint64_le_uint64 (ua, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ua, ub
      logical :: res

      if (ua%u64 < 0 .and. ub%u64 < 0) then
         res = ua%u64 <= ub%u64
      else
         if (ua%u64 < 0) then
            res = .false.
         else if (ub%u64 < 0) then
            res = .true.
         else
            res = ua%u64 <= ub%u64 
         end if
      end if
   end function uint64_le_uint64

   function uint64_le_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .false.
      else
         if (ua%u64 < 0) then
            res = .false.
         else
            res = ua%u64 <= b
         end if
      end if
   end function uint64_le_int32

   function uint64_le_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .false.
      else
         if (ua%u64 < 0) then
            res = .false.
         else
            res = ua%u64 <= b
         end if
      end if
   end function uint64_le_int64

   function int32_le_uint64 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint64), intent(in) :: ub
      logical :: res

      if (a < 0) then
         res = .true.
      else
         if (ub%u64 < 0) then
            res = .true.
         else
            res = a <= ub%u64
         end if
      end if
   end function int32_le_uint64

   function int64_le_uint64 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint64), intent(in) :: ub
      logical :: res

      if (a < 0) then
         res = .true.
      else
         if (ub%u64 < 0) then
            res = .true.
         else
            res = a <= ub%u64
         end if
      end if
   end function int64_le_uint64

end module less_than_m