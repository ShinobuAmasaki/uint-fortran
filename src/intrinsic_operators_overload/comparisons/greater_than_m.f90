module greater_than_m
   use :: iso_fortran_env
   use :: uint16_t 
   use :: uint32_t 
   use :: uint64_t 
   implicit none
   
   private

   public :: operator(>)
   interface operator(>)
      module procedure :: uint16_gt_uint16
      module procedure :: uint16_gt_int16, int16_gt_uint16 
      module procedure :: uint16_gt_int32, int32_gt_uint16
      module procedure :: uint16_gt_int64, int64_gt_uint16  

      module procedure :: uint32_gt_uint32
      module procedure :: uint32_gt_int32, int32_gt_uint32
      module procedure :: uint32_gt_int64, int64_gt_uint32
      
      module procedure :: uint64_gt_uint64
      module procedure :: uint64_gt_int32, int32_gt_uint64
      module procedure :: uint64_gt_int64, int64_gt_uint64
   end interface

   public :: operator(>=)
   interface operator(>=)
      module procedure :: uint16_ge_uint16
      module procedure :: uint16_ge_int16, int16_ge_uint16
      module procedure :: uint16_ge_int32, int32_ge_uint16
      module procedure :: uint16_ge_int64, int64_ge_uint16
   
      module procedure :: uint32_ge_uint32
      module procedure :: uint32_ge_int32, int32_ge_uint32
      module procedure :: uint32_ge_int64, int64_ge_uint32

      module procedure :: uint64_ge_uint64
      module procedure :: uint64_ge_int32, int32_ge_uint64
      module procedure :: uint64_ge_int64, int64_ge_uint64
   end interface



contains

!== Greater then ============================================!
!-- uint16 --------------------------------------------------!
   pure elemental function uint16_gt_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) > cast_to_int32(ub)
   end function uint16_gt_uint16

   pure elemental function uint16_gt_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) > b
   end function uint16_gt_int16

   pure elemental function  int16_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a > cast_to_int32(ub)
   end function int16_gt_uint16

   pure elemental function uint16_gt_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) > b
   end function uint16_gt_int32

   pure elemental function int32_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a > cast_to_int32(ub)
   end function int32_gt_uint16

   pure elemental function uint16_gt_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) > b
   end function uint16_gt_int64

   pure elemental function int64_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a > cast_to_int32(ub)
   end function int64_gt_uint16

!-- uint32 -----------------------------------------------------------!

   pure elemental function uint32_gt_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = cast_to_int64(ua) > cast_to_int64(ub)
   end function 


   pure elemental function uint32_gt_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) > b 
   end function uint32_gt_int32


   pure elemental function uint32_gt_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) > b
   end function uint32_gt_int64


   pure elemental function int32_gt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a > cast_to_int64(ub)
   end function int32_gt_uint32


   pure elemental function int64_gt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a > cast_to_int64(ub)
   end function int64_gt_uint32

!-- uint64 -----------------------------------------------------------!
   pure elemental function uint64_gt_uint64 (ua, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ua, ub
      logical :: res 

      if (ua%u64 < 0 .and. ub%u64 < 0) then
         res = ua%u64 > ub%u64
      else if (ua%u64 < 0) then
         res = .true. 
      else if (ub%u64 < 0) then
         res = .false.
      else
         res = ua%u64 > ub%u64
      end if
   end function uint64_gt_uint64

   pure elemental function uint64_gt_int32 (ua, b) result(res)
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .true.
      else
         if (ua%u64 < 0) then
            res = .true.
         else
            res = ua%u64 > b
         end if
      end if
   end function uint64_gt_int32

   pure elemental function uint64_gt_int64 (ua, b) result(res)
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .true.
      else
         if (ua%u64 < 0) then
            res = .true.
         else
            res = ua%u64 > b
         end if
      end if
   end function uint64_gt_int64


   pure elemental function int32_gt_uint64 (a, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ub
      integer(int32), intent(in) :: a
      logical :: res

      if (a < 0) then
         res = .false.
      else
         if (ub%u64 < 0) then
            res = .false.
         else
            res = a > ub%u64
         end if
      end if
   end function int32_gt_uint64


   pure elemental function int64_gt_uint64 (a, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res

      if (a < 0) then
         res = .false.
      else
         if (ub%u64 < 0) then
            res = .false.
         else
            res = a > ub%u64
         end if
      end if
   end function int64_gt_uint64


!== Greater then or equal ============================================!

   pure elemental function uint16_ge_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = cast_to_int32(ua) >= cast_to_int32(ub)
   end function uint16_ge_uint16

   pure elemental function uint16_ge_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) >= b
   end function uint16_ge_int16

   pure elemental function int16_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a >= cast_to_int32(ub)
   end function int16_ge_uint16

   pure elemental function uint16_ge_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = cast_to_int32(ua) >= b
   end function uint16_ge_int32

   pure elemental function int32_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a >= cast_to_int32(ub)
   end function int32_ge_uint16

   pure elemental function uint16_ge_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = cast_to_int32(ua) >= b
   end function uint16_ge_int64

   pure elemental function int64_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a >= cast_to_int32(ub)
   end function int64_ge_uint16

   pure elemental function uint32_ge_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = cast_to_int64(ua) >= cast_to_int64(ub)
   end function 
   
   
   pure elemental function uint32_ge_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = cast_to_int64(ua) >= b 
   end function uint32_ge_int32
   
   
   pure elemental function uint32_ge_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = cast_to_int64(ua) >= b
   end function uint32_ge_int64
   
   
   pure elemental function int32_ge_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a >= cast_to_int64(ub)
   end function int32_ge_uint32
   
   
   pure elemental function int64_ge_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a >= cast_to_int64(ub)
   end function int64_ge_uint32

!-- uint64 -----------------------------------------------------------!

   pure elemental function uint64_ge_uint64 (ua, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ua, ub
      logical :: res 

      if (ua%u64 < 0 .and. ub%u64 < 0) then
         res = ua%u64 >= ub%u64
      else if (ua%u64 < 0) then
         res = .true. 
      else if (ub%u64 < 0) then
         res = .false.
      else
         res = ua%u64 >= ub%u64
      end if
   end function uint64_ge_uint64


   pure elemental function uint64_ge_int32 (ua, b) result(res)
      implicit none
      type(uint64), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .true.
      else
         if (ua%u64 < 0) then
            res = .true.
         else
            res = ua%u64 >= b
         end if
      end if
   end function uint64_ge_int32


   pure elemental function uint64_ge_int64 (ua, b) result(res)
      implicit none
      type(uint64), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res

      if (b < 0) then
         res = .true.
      else
         if (ua%u64 < 0) then
            res = .true.
         else
            res = ua%u64 >= b
         end if
      end if
   end function uint64_ge_int64


   pure elemental function int32_ge_uint64 (a, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ub
      integer(int32), intent(in) :: a
      logical :: res

      if (a < 0) then
         res = .false.
      else
         if (ub%u64 < 0) then
            res = .false.
         else
            res = a >= ub%u64
         end if
      end if
   end function int32_ge_uint64


   pure elemental function int64_ge_uint64 (a, ub) result(res)
      implicit none
      type(uint64), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res

      if (a < 0) then
         res = .false.
      else
         if (ub%u64 < 0) then
            res = .false.
         else
            res = a >= ub%u64
         end if
      end if
   end function int64_ge_uint64


end module greater_than_m