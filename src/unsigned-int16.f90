module unsigned_int16 
   use, intrinsic :: iso_fortran_env
   implicit none
   private
   
   integer(int32), parameter :: UINT16_LIMIT = 65535

   type, public :: uint16
      integer(int16) :: u16   !0から32767までは同一、-32768から-1の値の場合は+65536する     
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

   ! public :: operator(<)
   ! public :: operator(>)
   ! public :: operator(<=)
   ! public :: operator(>=)
   ! public :: operator(/=)
   ! public :: operator(==)

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

   public :: write(formatted)
   interface write(formatted)
      procedure :: print_uint16
   end interface

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
         res%u16 = a - (UINT16_LIMIT+1)
      else
         res%u16 = a
      end if
   end function


   function unsign_int64 (a) result(res)
      implicit none
      integer(int64), intent(in) :: a
      type(uint16) :: res

      if ((UINT16_LIMIT+1)/2 < a) then
         res%u16 = a - (UINT16_LIMIT+1)
      else
         res%u16 = a
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


   ! Writing
   subroutine print_uint16 (self, unit, iotype, arglist, iostatus, iomessage)
      implicit none
      class(uint16), intent(in   ) :: self
      integer,       intent(in   ) :: unit
      character(*),  intent(in   ) :: iotype
      integer,       intent(in   ) :: arglist(:)
      integer,       intent(  out) :: iostatus
      character(*),  intent(inout) :: iomessage

      if (iotype == "LISTDIRECTED" .or. size(arglist) < 1) then
         write(unit=unit, fmt= *, iostat=iostatus, iomsg=iomessage) validate(self)
         return
      else
         if (iotype(3:) /= "uint16") then
            print *, "Error: type mismatch"
            return
         end if

         block
            character(2) :: width_total, width_decimal
            character(6) :: uint_spec
            character(:), allocatable :: fmt
            
            write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) validate(self)
         end block
      end if

   end subroutine print_uint16
   
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
      implicit none
      type(uint16), intent(in) :: ua, ub
      type(uint16) :: res

      res%u16 = validate(ua) + validate(ub)

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
      
      res%u16 = validate(ua) * validate(ub)

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

end module unsigned_int16

 