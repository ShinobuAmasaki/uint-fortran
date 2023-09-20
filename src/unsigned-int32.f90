module unsigned_int32
   use, intrinsic :: iso_fortran_env
   implicit none
   
   private

   integer(int64), parameter :: UINT32_LIMIT = 4294967295_int64

   type, public :: uint32
      integer(int32) :: u32
   end type uint32

   public :: assignment(=)
   interface assignment(=)
      module procedure :: assign_int16, assign_int32, assign_int64
   end interface

   public :: write(formatted)
   interface write(formatted)
      procedure :: print_uint32
   end interface

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


   subroutine print_uint32 (self, unit, iotype, arglist, iostatus, iomessage)
      implicit none
      class(uint32), intent(in) :: self
      integer,       intent(in) :: unit
      character(*),  intent(in) :: iotype
      integer,       intent(in) :: arglist(:)
      integer,       intent(out) :: iostatus
      character(*), intent(inout) :: iomessage

      if (iotype == "LISTDIRECTED" .or. size(arglist) < 1) then
         write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) validate(self)
         return
      else
         if (iotype(3:) /= "u") then
            print *, "Error: type mismatch"
         end if

         block
            character(3) :: width_total, width_decimal
            character(6) :: unit_spec
            character(:), allocatable :: fmt

            write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) validate(self)
         end block
      end if
   end subroutine print_uint32


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
   

end module unsigned_int32