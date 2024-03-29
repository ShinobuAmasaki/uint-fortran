! This program tests the addition operator for int16 + uint16. 
program check
   use :: iso_fortran_env
   use :: unsigned_m
   implicit none

   character(32) :: ustr, str

   integer :: count
   type(uint16) :: ub
   integer(int32) :: uans
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


      call random_number(x)
      ix = int(x*65335)
      b = ix
      ub = ix

      ans = a + b
      uans = a + ub
      
      
      if (ans >65535 .or. ans < 0) cycle

      write(ustr, *) uans
      write(str,*) ans
      
      if (trim(adjustl(str)) /= trim(adjustl(ustr))) then 
         write (*, *) a, b, ans, uans
         isPassed = .false.
      end if
      count = count + 1
   end do

   if (isPassed) print *, "[Test]  int16 + uint16: Passed, ", count, " cases."

   
end program check
