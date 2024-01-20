! This program tests the addition operator for uint16 + uint32. 
program check
   use :: iso_fortran_env
   use :: unsigned_m
   implicit none

   character(32) :: ustr, str

   integer :: count
   type(uint16) :: ua
   type(uint32) :: ub, uans
   integer(int32) :: a, b, ix,i, ans
   real :: x
   integer :: n = 10000000

   logical :: isPassed = .true.

   ustr = ''
   str = '' 

   count = 0
   do i = 1, n
      call random_number(x)
      ix = int(x*UINT16_LIMIT)
      a = ix
      ua = ix 

      call random_number(x)
      ix = int(x*UINT32_LIMIT)
      b = ix
      ub = ix

      ans = a + b
      uans = ua+ub
      
      
      if (ans >UINT32_LIMIT .or. ans < 0) cycle

      write(ustr, *) uans
      write(str,*) ans
      
      if (trim(adjustl(str)) /= trim(adjustl(ustr))) then 
         write (*, *) a, b, ans, uans
         isPassed = .false.
      end if
      count = count + 1
   end do

   if (isPassed) print *, "[Test] uint16 + uint32: Passed, ", count, " cases."

   
end program check
