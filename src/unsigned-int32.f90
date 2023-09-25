module unsigned_int32
   use, intrinsic :: iso_fortran_env
   implicit none
   
   private

   integer(int64), parameter :: UINT32_LIMIT = 4294967295_int64

   type, public, bind(c) :: uint32
      integer(int32) :: u32
   end type uint32

   public :: operator(+)
   interface operator(+)
      module procedure :: uint32_add_uint32
      module procedure :: uint32_add_int32, int32_add_uint32
      module procedure :: uint32_add_int64, int64_add_uint32
   end interface

   public :: operator(-)
   interface operator(-)
      module procedure :: uint32_sub_uint32
      module procedure :: uint32_sub_int32, int32_sub_uint32
      module procedure :: uint32_sub_int64, int64_sub_uint32
   end interface 

   public :: operator(*)
   interface operator(*)
      module procedure :: uint32_mul_uint32
      module procedure :: uint32_mul_int32, int32_mul_uint32
      module procedure :: uint32_mul_int64, int64_mul_uint32
   end interface 

   public :: operator( / )
   interface operator(/)
      module procedure :: uint32_div_uint32
      module procedure :: uint32_div_int32, int32_div_uint32
      module procedure :: uint32_div_int64, int64_div_uint32
   end interface 

   public :: operator(**)
   interface operator(**)
      module procedure :: uint32_pow_uint32
      module procedure :: uint32_pow_int32, int32_pow_uint32
      module procedure :: uint32_pow_int64, int64_pow_uint32
   end interface 

   public :: assignment(=)
   interface assignment(=)
      module procedure :: assign_int16, assign_int32, assign_int64
   end interface

   !---------------------------------------------------------!
   public :: operator(>)
   interface operator(>)
      module procedure :: uint32_gt_uint32
      module procedure :: uint32_gt_int32, int32_gt_uint32
      module procedure :: uint32_gt_int64, int64_gt_uint32
   end interface

   !---------------------------------------------------------!
   public :: operator(>=)
   interface operator(>=)
      module procedure :: uint32_ge_uint32
      module procedure :: uint32_ge_int32, int32_ge_uint32
      module procedure :: uint32_ge_int64, int64_ge_uint32
   end interface

  
   !---------------------------------------------------------!
   public :: operator(<)
   interface operator(<)
      module procedure :: uint32_lt_uint32
      module procedure :: uint32_lt_int32, int32_lt_uint32
      module procedure :: uint32_lt_int64, int64_lt_uint32
   end interface

   !---------------------------------------------------------!      
   public :: operator(<=)
   interface operator(<=)
      module procedure :: uint32_le_uint32
      module procedure :: uint32_le_int32, int32_le_uint32
      module procedure :: uint32_le_int64, int64_le_uint32
   end interface

   !---------------------------------------------------------!
   public :: operator(==)
   interface operator(==)
      module procedure :: uint32_eq_uint32
      module procedure :: uint32_eq_int32, int32_eq_uint32
      module procedure :: uint32_eq_int64, int64_eq_uint32
   end interface 
   
   !---------------------------------------------------------!
   public :: operator(/=)
   interface operator(/=)
      module procedure :: uint32_ne_uint32
      module procedure :: uint32_ne_int32, int32_ne_uint32
      module procedure :: uint32_ne_int64, int64_ne_uint32
   end interface 

   !=========================================================!
   ! Modulo
   public :: mod
   interface mod
      module procedure :: uint32_mod_uint32
      module procedure :: uint32_mod_int32, int32_mod_uint32
      module procedure :: uint32_mod_int64, int64_mod_uint32
   end interface 

   !========================================================!

   public :: int
   interface int
      module procedure :: cast_to_int64
   end interface

   public :: real
   interface real
      module procedure :: cast_to_real
   end interface

   public :: dble
   interface dble
      module procedure :: cast_to_dble
   end interface

   public :: cmplx
   interface cmplx
      module procedure :: cast_to_complex_64_im, cast_to_complex_64_re
   end interface cmplx

   public :: pick
   interface pick
      module procedure :: validate
   end interface

contains


   function unsign_int16(a) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint32) :: res

      res%u32 = a

   end function unsign_int16

   
   function unsign_int32(a) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint32) :: res

      res%u32 = a 
   end function unsign_int32
   
   
   function unsign_int64(a) result(res)
      implicit none
      integer(int64), intent(in) :: a
      type(uint32) :: res

      if ((UINT32_LIMIT+1)/2 < a) then
         res%u32 = a - (UINT32_LIMIT+1)
      else
         res%u32 = a
      end if
   end function unsign_int64


   function validate(ua)
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64) :: validate

      validate = ua%u32
      if (validate < 0) validate = validate + (UINT32_LIMIT +1)
   
   end function validate


   !==================================================================!
   ! Casting

   integer(int64) function cast_to_int64(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      
      cast_to_int64 = validate(ua)

   end function cast_to_int64


   real(real64) function cast_to_dble(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua

      cast_to_dble = dble(validate(ua))
   end function cast_to_dble


   real(real32) function cast_to_real(ua)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      cast_to_real = real(validate(ua))
   end function cast_to_real


   function cast_to_complex_64_re(ua, im) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      real(real64), intent(in) :: im
      complex(real64) :: res
      res = cmplx(validate(ua), im, kind(0d0))
   end function cast_to_complex_64_re

   function cast_to_complex_64_im(re, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      real(real64), intent(in) :: re
      complex(real64) :: res
      res = cmplx(re, validate(ua), kind(0d0))
   end function cast_to_complex_64_im


   !==================================================================!
   ! Assignment

   subroutine assign_int16 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(out) :: ua
      integer(int16), intent(in) :: a

      ua = unsign_int16(a)

   end subroutine assign_int16


   subroutine assign_int32 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(out) :: ua
      integer(int32), intent(in) :: a

      ua = unsign_int32(a)

   end subroutine assign_int32


   subroutine assign_int64 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(out) :: ua
      integer(int64), intent(in) :: a

      ua = unsign_int64(a)

   end subroutine assign_int64
   
   !==================================================================!
   ! Addition
   function uint32_add_uint32 (ua, ub) result(res)
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res

      res%u32 = ua%u32 + ub%u32
   end function uint32_add_uint32


   function uint32_add_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = validate(ua) + b
   end function 

   
   function int32_add_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a + validate(ub)
   end function int32_add_uint32
   

   function uint32_add_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = validate(ua) + b
   end function uint32_add_int64

   
   function int64_add_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a + validate(ub)
   end function int64_add_uint32


   !==================================================================!
   ! Subtraction

   function uint32_sub_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res

      res = validate(ua) - validate(ub)
   end function uint32_sub_uint32


   function uint32_sub_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = validate(ua) - b
   end function 

   
   function int32_sub_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a - validate(ub)
   end function int32_sub_uint32
   

   function uint32_sub_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = validate(ua) - b
   end function uint32_sub_int64

   
   function int64_sub_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a - validate(ub)
   end function int64_sub_uint32


   !==================================================================!
   ! Multiplication

   function uint32_mul_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res
      res%u32 = validate(ua)*validate(ub)

   end function uint32_mul_uint32


   function uint32_mul_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = validate(ua) * b
   end function uint32_mul_int32

   
   function int32_mul_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a * validate(ub)
   end function int32_mul_uint32
   

   function uint32_mul_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = validate(ua) * b
   end function uint32_mul_int64

   
   function int64_mul_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a * validate(ub)
   end function int64_mul_uint32

   !==================================================================!
   ! Power

   function uint32_pow_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      integer(int64) :: res
      res = validate(ua)**validate(ub)

   end function uint32_pow_uint32


   function uint32_pow_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = validate(ua) ** b
   end function uint32_pow_int32

   
   function int32_pow_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a ** validate(ub)
   end function int32_pow_uint32
   

   function uint32_pow_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = validate(ua) ** b
   end function uint32_pow_int64

   
   function int64_pow_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a ** validate(ub)
   end function int64_pow_uint32

   !==================================================================!
   ! Division
   function uint32_div_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      integer(int64) :: res

      res = validate(ua) / validate(ub)
   end function uint32_div_uint32


   function uint32_div_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = validate(ua) / b
   end function 

   
   function int32_div_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = a / validate(ub)
   end function int32_div_uint32
   

   function uint32_div_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = validate(ua) / b
   end function uint32_div_int64

   
   function int64_div_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = a / validate(ub)
   end function int64_div_uint32

   !==================================================================!
   ! Comparison

   ! greater than
   function uint32_gt_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = validate(ua) > validate(ub)
   end function 


   function uint32_gt_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = validate(ua) > b 
   end function uint32_gt_int32


   function uint32_gt_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = validate(ua) > b
   end function uint32_gt_int64


   function int32_gt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a > validate(ub)
   end function int32_gt_uint32


   function int64_gt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a > validate(ub)
   end function int64_gt_uint32


   !----------------------------------------------------!  
   ! Greater or equal than
   function uint32_ge_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = validate(ua) >= validate(ub)
   end function 
   
   
   function uint32_ge_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = validate(ua) >= b 
   end function uint32_ge_int32
   
   
   function uint32_ge_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = validate(ua) >= b
   end function uint32_ge_int64
   
   
   function int32_ge_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a >= validate(ub)
   end function int32_ge_uint32
   
   
   function int64_ge_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a >= validate(ub)
   end function int64_ge_uint32

 
   !----------------------------------------------------!
   ! less than
   function uint32_lt_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = validate(ua) < validate(ub)
   end function 

 
   function uint32_lt_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = validate(ua) < b 
   end function uint32_lt_int32


   function uint32_lt_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = validate(ua) < b
   end function uint32_lt_int64


   function int32_lt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a < validate(ub)
   end function int32_lt_uint32


   function int64_lt_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a < validate(ub)
   end function int64_lt_uint32


   !----------------------------------------------------!
   ! less or equal than
   function uint32_le_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = validate(ua) <= validate(ub)
   end function 


   function uint32_le_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = validate(ua) <= b 
   end function uint32_le_int32


   function uint32_le_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = validate(ua) <= b
   end function uint32_le_int64


   function int32_le_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a <= validate(ub)
   end function int32_le_uint32


   function int64_le_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a <= validate(ub)
   end function int64_le_uint32


   !--------------------------------------------!
   ! Equal
   function uint32_eq_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = ua%u32 == ub%u32
   end function 
   
   
   function uint32_eq_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = validate(ua) == b 
   end function uint32_eq_int32
   
   
   function uint32_eq_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = validate(ua) == b
   end function uint32_eq_int64
   
   
   function int32_eq_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a == validate(ub)
   end function int32_eq_uint32
   
   
   function int64_eq_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a == validate(ub)
   end function int64_eq_uint32


      !--------------------------------------------!
   ! Not equal
   function uint32_ne_uint32 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      logical :: res  
      res = ua%u32 /= ub%u32
   end function 
   
   
   function uint32_ne_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res
      res = validate(ua) /= b 
   end function uint32_ne_int32
   
   
   function uint32_ne_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b 
      logical :: res
      res = validate(ua) /= b
   end function uint32_ne_int64
   
   
   function int32_ne_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub 
      integer(int32), intent(in) :: a
      logical :: res 
      res = a /= validate(ub)
   end function int32_ne_uint32
   
   
   function int64_ne_uint32(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ub
      integer(int64), intent(in) :: a
      logical :: res 
      res = a /= validate(ub)
   end function int64_ne_uint32
   
   !==================================================================!
   ! Modulo operation
   function uint32_mod_uint32(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua, ub
      type(uint32) :: res

      res = mod(validate(ua), validate(ub))
   end function uint32_mod_uint32


   function uint32_mod_int32 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int32), intent(in) :: b
      integer(int64) :: res

      res = mod(validate(ua),b)
   end function 

   
   function int32_mod_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res

      res = mod(a,validate(ub))
   end function int32_mod_uint32
   

   function uint32_mod_int64 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(in) :: ua
      integer(int64), intent(in) :: b
      integer(int64) :: res
      res = mod( validate(ua), b)
   end function uint32_mod_int64

   
   function int64_mod_uint32 (a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint32), intent(in) :: ub
      integer(int64) :: res
      res = mod(a, validate(ub))
   end function int64_mod_uint32

end module unsigned_int32