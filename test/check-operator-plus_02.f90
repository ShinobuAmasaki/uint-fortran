! This program tests the addition operator for uint16 + int16. 
program check
   use :: iso_fortran_env
   use :: unsigned_m
   implicit none

   character(32) :: ustr, str

   integer :: count
   type(uint16) :: ua, uans
   integer(int32) :: a, b, ix,i, ans
   real :: x
   integer :: n = 10000000

   logical :: isPassed = .true.

   ustr = ''
   str = '' 

   count = 0
   do i = 1, n
      call random_number(x)
      ix = int(x*65335)
      a = ix
      ua = ix 

      call random_number(x)
      ix = int(x*65335)
      b = ix

      ans = a + b
      uans = ua+b
      
      
      if (ans >65535 .or. ans < 0) cycle

      write(ustr, *) uans
      write(str,*) ans
      
      if (trim(adjustl(str)) /= trim(adjustl(ustr))) then 
         write (*, *) a, b, ans, uans
         isPassed = .false.
      end if
      count = count + 1
   end do

   if (isPassed) print *, "[Test] uint16 +  int16: Passed, ", count, " cases."

   
end program check
