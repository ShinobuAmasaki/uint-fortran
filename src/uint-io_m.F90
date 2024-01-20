module uint_io_m
   use :: uint8_t
   use :: uint16_t 
   use :: uint32_t
   use :: assignment_m

   private

   public :: write(formatted)
   public :: write(unformatted)
   public :: read(formatted)
   public :: read(unformatted)

   interface write(formatted)
      module procedure :: print_uint8
      module procedure :: write_uint16_formatted
      module procedure :: write_uint32_formatted
   end interface

   interface write(unformatted)
      module procedure :: write_uint32_unformatted
   end interface

   interface read(formatted)
      module procedure :: read_uint32_formatted
   end interface
   
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
         write(unit=unit, fmt='(i10)', iostat=iostatus, iomsg=iomessage) cast_to_int64(self)
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

            write(unit=unit, fmt=fmt, iostat=iostatus, iomsg=iomessage) cast_to_int64(self)
         end block
      end if
   end subroutine write_uint32_formatted


   subroutine write_uint16_formatted (self, unit, iotype, arglist, iostatus, iomessage)
      implicit none
      type(uint16), intent(in   ) :: self
      integer,       intent(in   ) :: unit
      character(*),  intent(in   ) :: iotype
      integer,       intent(in   ) :: arglist(:)
      integer,       intent(  out) :: iostatus
      character(*),  intent(inout) :: iomessage

      if (iotype == "LISTDIRECTED".or. size(arglist) < 1) then
         write(unit=unit, fmt= '(i6)', iostat=iostatus, iomsg=iomessage) cast_to_int32(self)
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
            
            write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) cast_to_int32(self)
         end block
      end if

   end subroutine write_uint16_formatted

   
   subroutine print_uint8 (self, unit, iotype, arglist, iostatus, iomessage)
      implicit none
      type(uint8), intent(in   ) :: self
      integer,       intent(in   ) :: unit
      character(*),  intent(in   ) :: iotype
      integer,       intent(in   ) :: arglist(:)
      integer,       intent(  out) :: iostatus
      character(*),  intent(inout) :: iomessage 


      if (iotype == "LISTDIRECTED".or. size(arglist) < 1) then
         write(unit=unit, fmt= '(i3)', iostat=iostatus, iomsg=iomessage) cast_to_int16(self)
         return
      else
         if (iotype(3:) /= "uint8") then
            print *, "Error: type mismatch"
            return
         end if

         block
            character(2) :: width_total, width_decimal
            character(6) :: uint_spec
            character(:), allocatable :: fmt
            
            write(unit=unit, fmt=*, iostat=iostatus, iomsg=iomessage) cast_to_int16(self)
         end block
      end if

   end subroutine print_uint8

 

end module uint_io_m