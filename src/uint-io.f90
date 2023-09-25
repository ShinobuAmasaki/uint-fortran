module uint_io
   use unsigned_int16
   use unsigned_int32

   public :: write(formatted)
   interface write(formatted)
      module procedure :: print_uint16
      module procedure :: write_uint32_formatted
   end interface


   public :: write(unformatted)
   interface write(unformatted)
      module procedure :: write_uint32_unformatted
   end interface

   public :: read(formatted)
   interface read(formatted)
      module procedure :: read_uint32_formatted
   end interface
   
   public :: read(unformatted)
   interface read(unformatted)
      module procedure :: read_uint32_unformatted
   end interface

contains

   !=====================================================================!
   ! Derived type I/O

   subroutine read_uint32_unformatted(self, unit, iostatus, iomessage)
      implicit none
      type(uint32), intent(inout) :: self
      integer,       intent(in) :: unit
      integer,       intent(out) :: iostatus
      character(*), intent(inout) :: iomessage

      read(unit=unit, iostat=iostatus, iomsg=iomessage) self%u32
   end subroutine read_uint32_unformatted


   subroutine write_uint32_unformatted(self, unit, iostatus, iomessage)
      implicit none
      type(uint32), intent(in) :: self
      integer,       intent(in) :: unit
      integer,       intent(out) :: iostatus
      character(*), intent(inout) :: iomessage

      write(unit=unit, iostat=iostatus, iomsg=iomessage) self%u32
   end subroutine write_uint32_unformatted


   subroutine read_uint32_formatted (self, unit, iotype, arglist, iostatus, iomessage)
      use, intrinsic :: iso_fortran_env
      implicit none
      type(uint32), intent(inout) :: self
      integer,       intent(in) :: unit
      character(*),  intent(in) :: iotype
      integer,       intent(in) :: arglist(:)
      integer,       intent(out) :: iostatus
      character(*), intent(inout) :: iomessage

      integer(int64) :: buf 

      if (iotype == "LISTDIRECTED" .or. size(arglist) < 1) then
         read(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) buf
         self = buf
         return
      else
         if (iotype(3:) /= "u32" .or. iotype(3:) /= "U32") then
            print *, "Error: type mismatch"
         end if

         block
            character(2) :: width_total, width_minimal
            character(6) :: Spec
            character(:), allocatable :: fmt

            write(width_total, '(I2)') arglist(1)
            write(width_minimal, '(I2)') arglist(2)

            if (size(arglist,dim=1) == 1) then
               spec = 'I'//width_total
            else
               spec = 'I'//width_total//'.'//width_minimal
            end if

            fmt = "'("//spec//")'"

            read(unit=unit, fmt=fmt, iostat=iostatus, iomsg=iomessage) buf
            self = buf
         end block
      end if
   end subroutine read_uint32_formatted


   subroutine write_uint32_formatted (self, unit, iotype, arglist, iostatus, iomessage)
      implicit none
      type(uint32), intent(in) :: self
      integer,       intent(in) :: unit
      character(*),  intent(in) :: iotype
      integer,       intent(in) :: arglist(:)
      integer,       intent(out) :: iostatus
      character(*), intent(inout) :: iomessage

      if (iotype == "LISTDIRECTED" .or. size(arglist) < 1) then
         write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) pick(self)
         return
      else
         if (iotype(3:) /= "u32" .or. iotype(3:) /= "U32") then
            print *, "Error: type mismatch"
         end if

         block
            character(2) :: width_total, width_minimal
            character(6) :: Spec
            character(:), allocatable :: fmt

            write(width_total, '(I2)') arglist(1)
            write(width_minimal, '(I2)') arglist(2)

            if (size(arglist,dim=1) == 1) then
               spec = 'I'//width_total
            else
               spec = 'I'//width_total//'.'//width_minimal
            end if

            fmt = "'("//spec//")'"

            write(unit=unit, fmt=fmt, iostat=iostatus, iomsg=iomessage) pick(self)
         end block
      end if
   end subroutine write_uint32_formatted



   ! Writing
   subroutine print_uint16 (self, unit, iotype, arglist, iostatus, iomessage)
      use :: unsigned_int16
      implicit none
      type(uint16), intent(in   ) :: self
      integer,       intent(in   ) :: unit
      character(*),  intent(in   ) :: iotype
      integer,       intent(in   ) :: arglist(:)
      integer,       intent(  out) :: iostatus
      character(*),  intent(inout) :: iomessage

      if (iotype == "LISTDIRECTED" .or. size(arglist) < 1) then
         write(unit=unit, fmt= *, iostat=iostatus, iomsg=iomessage) pick(self)
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
            
            write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) pick(self)
         end block
      end if

   end subroutine print_uint16
   

end module uint_io
