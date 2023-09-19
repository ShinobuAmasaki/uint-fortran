program check
   use :: iso_fortran_env
   use :: unsigned_int16
   implicit none

   character(32) :: ustr, str

   type(uint16) :: ua, ub, uans
   integer(int32) :: a, b, ix,i, ans
   real :: x
   integer :: n = 10000000

   logical :: isPassed = .true.

   ustr = ''
   str = '' 

   do i = 1, n
      call random_number(x)
      ix = int(x*65335)
      a = ix
      ua = ix 

      call random_number(x)
      ix = int(x*65335)
      b = ix
      ub = ix

      ans = a + b
      uans = ua+ub
      
      
      if (ans >65535 .or. ans < 0) cycle

      write(ustr, *) uans
      write(str,*) ans
      
      if (trim(adjustl(str)) /= trim(adjustl(ustr))) then 
         write (*, *) a, b, ans, uans
         isPassed = .false.
      end if

   end do

   if (isPassed) print *, "[Test] uint16 + uint16: Passed"

   
end program check
