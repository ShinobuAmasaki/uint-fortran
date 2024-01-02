module uint_swap
   use :: unsigned_int32
   use :: unsigned_int16
   private

   public :: swap16
   interface swap16
      module procedure :: swap_uint16
   end interface swap16

   public :: swap32
   interface swap32
      module procedure :: swap_uint32
   end interface swap32

contains

   ! Byte swapping for uint16
   function swap_uint16(ua) result(res)
      implicit none
      type(uint16), intent(in) :: ua
      type(uint16) :: res

      res%u16 = ishft(ua%u16, 8)
      res%u16 = ior(res%u16, ishft(ua%u16, -8))

   end function swap_uint16

   ! Byte swapping for uint32
   function swap_uint32(ua) result(res)
      implicit none 
      type(uint32), intent(in) :: ua
      type(uint32) :: res
      
      res%u32 = 0
      res%u32 = ishft(ua%u32, 24)
      res%u32 = ior(res%u32, ishft(iand(ua%u32, 65280), 8))
      res%u32 = ior(res%u32, ishft(iand(ua%u32, 16711680),-8))
      res%u32 = ior(res%u32, ishft(ua%u32, -24))

   end function swap_uint32

end module uint_swap