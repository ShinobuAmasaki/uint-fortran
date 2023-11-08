module unsigned_int16 
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none
   private
   
   integer(int32), parameter :: UINT16_LIMIT = 65535

   type, public, bind(c) :: uint16
      integer(c_int16_t) :: u16   !0から32767までは同一、-32768から-1の値の場合は+65536する  
   end type uint16

   public :: operator(+)
   interface operator(+)
      module procedure :: uint16_add_int16, int16_add_uint16
      module procedure :: uint16_add_int32, int32_add_uint16
      module procedure :: uint16_add_int64, int64_add_uint16
      module procedure :: uint16_add_uint16
   end interface

   public :: operator(-)
   interface operator(-)
      module procedure :: uint16_sub_int16, int16_sub_uint16
      module procedure :: uint16_sub_int32, int32_sub_uint16
      module procedure :: uint16_sub_int64, int64_sub_uint16
      module procedure :: uint16_sub_uint16
   end interface

   public :: operator(*)
   interface  operator(*)
      module procedure :: uint16_mul_int16, int16_mul_uint16
      module procedure :: uint16_mul_int32, int32_mul_uint16
      module procedure :: uint16_mul_int64, int64_mul_uint16
      module procedure :: uint16_mul_uint16
   end interface 

   public :: operator( / )
   interface  operator(/)
      module procedure :: uint16_div_int16, int16_div_uint16
      module procedure :: uint16_div_int32, int32_div_uint16
      module procedure :: uint16_div_int64, int64_div_uint16
      module procedure :: uint16_div_uint16
   end interface 

   public :: operator(<)
   interface operator(<)
      module procedure :: uint16_lt_uint16
      module procedure :: uint16_lt_int16, int16_lt_uint16
      module procedure :: uint16_lt_int32, int32_lt_uint16
      module procedure :: uint16_lt_int64, int64_lt_uint16
   end interface
   
   public :: operator(>)
   interface operator(>)
      module procedure :: uint16_gt_uint16
      module procedure :: uint16_gt_int16, int16_gt_uint16 
      module procedure :: uint16_gt_int32, int32_gt_uint16
      module procedure :: uint16_gt_int64, int64_gt_uint16  
   end interface
   
   public :: operator(<=)
   interface operator(<=)
      module procedure :: uint16_le_uint16
      module procedure :: uint16_le_int16, int16_le_uint16
      module procedure :: uint16_le_int32, int32_le_uint16
      module procedure :: uint16_le_int64, int64_le_uint16
   end interface
   
   public :: operator(>=)
   interface operator(>=)
      module procedure :: uint16_ge_uint16
      module procedure :: uint16_ge_int16, int16_ge_uint16
      module procedure :: uint16_ge_int32, int32_ge_uint16
      module procedure :: uint16_ge_int64, int64_ge_uint16
   end interface

   public :: operator(/=)
   interface operator(/=)
      module procedure :: uint16_ne_uint16
      module procedure :: uint16_ne_int16, int16_ne_uint16
      module procedure :: uint16_ne_int32, int32_ne_uint16
      module procedure :: uint16_ne_int64, int64_ne_uint16
   end interface
   
   public :: operator(==)
   interface operator(==)
      module procedure :: uint16_eq_uint16
      module procedure :: uint16_eq_int16, int16_eq_uint16
      module procedure :: uint16_eq_int32, int32_eq_uint16
      module procedure :: uint16_eq_int64, int64_eq_uint16
   end interface

   public:: assignment(=)
   interface assignment(=)
      module procedure :: assign_int16, assign_int32, assign_int64
   end interface

   public :: int
   interface int
      module procedure :: cast_to_int32
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
      module procedure :: cast_to_complex_64
   end interface cmplx

   public :: pick
   interface pick
      module procedure :: validate
   end interface

contains

   function unsign_int16(a) result(res)
      implicit none
      integer(int16), intent(in) :: a
      type(uint16) :: res

         res%u16 = a

   end function

   function unsign_int32(a) result(res)
      implicit none
      integer(int32), intent(in) :: a
      type(uint16) :: res

      if ((UINT16_LIMIT+1)/2 < a) then
         res%u16 = int(a - (UINT16_LIMIT+1), c_int16_t)
      else
         res%u16 = int(a, c_int16_t)
      end if
   end function


   function unsign_int64 (a) result(res)
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
   

   function validate(ua)
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32) :: validate

      validate = ua%u16
      ! 
      if (validate < 0) validate = validate + (UINT16_LIMIT +1)

   end function validate


   ! Casting

   function cast_to_int32(ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      integer(int32) :: res

      res = validate(ua)
   end function cast_to_int32

   function cast_to_dble(ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      real(real64) :: res

      res = dble(validate(ua))

   end function cast_to_dble

   
   function cast_to_real(ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      real(real32) :: res

      res = real(validate(ua))

   end function cast_to_real


   function cast_to_complex_64 (ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua
      complex(kind(0d0)) :: res

      res = cmplx(validate(ua), 0, kind(0d0))
   
   end function cast_to_complex_64

      
   !==================================================================!
   ! Assignment

   subroutine assign_int16 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(out) :: ua
      integer(int16), intent(in) :: a

      ua = unsign_int16(a)

   end subroutine assign_int16


   subroutine assign_int32 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(out) :: ua
      integer(int32), intent(in) :: a

      ua = unsign_int32(a)

   end subroutine assign_int32


   subroutine assign_int64 (ua, a)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(out) :: ua
      integer(int64), intent(in) :: a

      ua = unsign_int64(a)

   end subroutine assign_int64

   !==================================================================!
   ! Addition

   function uint16_add_uint16 (ua, ub) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16) :: res

      res%u16 = int(validate(ua) + validate(ub), c_int16_t)

   end function uint16_add_uint16
      

   function int16_add_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a + validate(ua) 

   end function


   function uint16_add_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) + a  

   end function


   function uint16_add_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) + a  

   end function

   function int32_add_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) + a  

   end function   
 
   
   function int64_add_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = validate(ua) + a  

   end function 


   function uint16_add_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = validate(ua) + a  

   end function  


   !==================================================================!
   ! Subtraction

   function uint16_sub_uint16(ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16)  res
      
      res = validate(ua) - validate(ub)
   end function uint16_sub_uint16

   function int16_sub_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a - validate(ua) 
   end function


   function uint16_sub_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) - a  
   end function


   function uint16_sub_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) - a
   end function

   function int32_sub_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a - validate(ua)  
   end function   
 
   
   function int64_sub_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = a - validate(ua)  
   end function


   function uint16_sub_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = validate(ua) - a  
   end function  

   !==================================================================!
   ! Multiplicaiton

   function uint16_mul_uint16 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16) :: res
      
      res%u16 = int(validate(ua) * validate(ub), c_int16_t)

   end function uint16_mul_uint16

   function int16_mul_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) * a 
   end function


   function uint16_mul_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) * a  
   end function


   function uint16_mul_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) * a  
   end function

   function int32_mul_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) * a  
   end function   
 
   
   function int64_mul_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = validate(ua) * a  
   end function 


   function uint16_mul_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = validate(ua) * a  
   end function  

   !==================================================================!
   ! division

   function uint16_div_uint16 (ua, ub) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint16), intent(in) :: ua, ub
      integer(int32) :: res

      res = validate(ua) / validate(ub)
   end function

   function int16_div_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a / validate(ua) 
   end function


   function uint16_div_int16 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int16), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) / a  
   end function


   function uint16_div_int32 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = validate(ua) / a
   end function

   function int32_div_uint16 (a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int32) :: res

      res = a / validate(ua)  
   end function   
 
   
   function int64_div_uint16(a, ua) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = a / validate(ua)  
   end function


   function uint16_div_int64 (ua, a) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int64), intent(in) :: a
      type(uint16), intent(in)   :: ua
      integer(int64) :: res

      res = validate(ua) / a  
   end function  

   !==================================================================!
   ! Comparison

   ! greater than
   function uint16_gt_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = validate(ua) > validate(ub)
   end function uint16_gt_uint16

   function uint16_gt_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = validate(ua) > b
   end function uint16_gt_int16

   function int16_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a > validate(ub)
   end function int16_gt_uint16

   function uint16_gt_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = validate(ua) > b
   end function uint16_gt_int32

   function int32_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a > validate(ub)
   end function int32_gt_uint16

   function uint16_gt_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = validate(ua) > b
   end function uint16_gt_int64

   function int64_gt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a > validate(ub)
   end function int64_gt_uint16

   
   ! greater or equal than
   function uint16_ge_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = validate(ua) >= validate(ub)
   end function uint16_ge_uint16

   function uint16_ge_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = validate(ua) >= b
   end function uint16_ge_int16

   function int16_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a >= validate(ub)
   end function int16_ge_uint16

   function uint16_ge_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = validate(ua) >= b
   end function uint16_ge_int32

   function int32_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a >= validate(ub)
   end function int32_ge_uint16

   function uint16_ge_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = validate(ua) >= b
   end function uint16_ge_int64

   function int64_ge_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a >= validate(ub)
   end function int64_ge_uint16


    ! less than
   function uint16_lt_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = validate(ua) < validate(ub)
   end function uint16_lt_uint16

   function uint16_lt_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = validate(ua) < b
   end function uint16_lt_int16

   function int16_lt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a < validate(ub)
   end function int16_lt_uint16

   function uint16_lt_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = validate(ua) < b
   end function uint16_lt_int32

   function int32_lt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a < validate(ub)
   end function int32_lt_uint16

   function uint16_lt_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = validate(ua) < b
   end function uint16_lt_int64

   function int64_lt_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a < validate(ub)
   end function int64_lt_uint16


   ! less or equal than
   function uint16_le_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = validate(ua) <= validate(ub)
   end function uint16_le_uint16

   function uint16_le_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = validate(ua) <= b
   end function uint16_le_int16

   function int16_le_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a <= validate(ub)
   end function int16_le_uint16

   function uint16_le_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = validate(ua) <= b
   end function uint16_le_int32

   function int32_le_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a <= validate(ub)
   end function int32_le_uint16

   function uint16_le_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = validate(ua) <= b
   end function uint16_le_int64

   function int64_le_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a <= validate(ub)
   end function int64_le_uint16


    ! equal to
   function uint16_eq_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = validate(ua) == validate(ub)
   end function uint16_eq_uint16

   function uint16_eq_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = validate(ua) == b
   end function uint16_eq_int16

   function int16_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a == validate(ub)
   end function int16_eq_uint16

   function uint16_eq_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = validate(ua) == b
   end function uint16_eq_int32

   function int32_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a == validate(ub)
   end function int32_eq_uint16

   function uint16_eq_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = validate(ua) == b
   end function uint16_eq_int64

   function int64_eq_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a == validate(ub)
   end function int64_eq_uint16


    ! not equal to
   function uint16_ne_uint16 (ua, ub) result(res)
      implicit none
      type(uint16), intent(in) :: ua, ub
      logical :: res
      res = validate(ua) /= validate(ub)
   end function uint16_ne_uint16

   function uint16_ne_int16 (ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int16), intent(in) :: b
      logical :: res

      res = validate(ua) /= b
   end function uint16_ne_int16

   function int16_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int16), intent(in) :: a
      type(uint16), intent(in) :: ub 
      logical :: res
      
      res = a /= validate(ub)
   end function int16_ne_uint16

   function uint16_ne_int32(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int32), intent(in) :: b
      logical :: res

      res = validate(ua) /= b
   end function uint16_ne_int32

   function int32_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int32), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a /= validate(ub)
   end function int32_ne_uint16

   function uint16_ne_int64(ua, b) result(res)
      use, intrinsic :: iso_fortran_env
      type(uint16), intent(in) :: ua
      integer(int64), intent(in) :: b
      logical :: res
      
      res = validate(ua) /= b
   end function uint16_ne_int64

   function int64_ne_uint16(a, ub) result(res)
      use, intrinsic :: iso_fortran_env
      integer(int64), intent(in) :: a
      type(uint16), intent(in) :: ub
      logical :: res

      res = a /= validate(ub)
   end function int64_ne_uint16





























end module unsigned_int16

 