module uint_swap
   use :: uint8_t
   use :: uint16_t 
   use :: uint32_t
   private

   public :: swap16
   interface swap16
      module procedure :: swap_uint16
   end interface swap16

   public :: swap32
   interface swap32
      module procedure :: swap_uint32
   end interface swap32

   ! public :: swap64
   ! interface swap64
   !    module procedure :: swap_uint64
   ! end interface swap64

   public :: bits_invert

contains

   ! Byte swapping for uint16
   pure elemental function swap_uint16(ua) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      type(uint16) :: res

      res%u16 = ishft(ua%u16, 8)
      res%u16 = ior(res%u16, ishft(ua%u16, -8))

   end function swap_uint16

   ! Byte swapping for uint32
   pure elemental function swap_uint32(ua) result(res)
      implicit none 
      type(uint32), intent(in) :: ua
      type(uint32) :: res
      
      res%u32 = 0
      res%u32 = ishft(ua%u32, 24)
      res%u32 = ior(res%u32, ishft(iand(ua%u32, 65280), 8))
      res%u32 = ior(res%u32, ishft(iand(ua%u32, 16711680),-8))
      res%u32 = ior(res%u32, ishft(ua%u32, -24))

   end function swap_uint32

   pure elemental function bits_invert(ua) result(res)
      use :: iso_fortran_env
      implicit none
      type(uint8), intent(in) :: ua
      type(uint8) :: res
      integer(int32) :: x

      x = ua%u8

      x = ior(ishft(iand(x, 15),4), iand(ishft(x,-4), 15))
      x = ior(ishft(iand(x, 51), 2), iand(ishft(x,-2), 51))
      x = ior(ishft(iand(x, 85), 1), iand(ishft(x,-1), 85)) 
      
      res = uint8(x)

   end function bits_invert

end module uint_swap