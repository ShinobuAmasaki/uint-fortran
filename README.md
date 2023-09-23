# uint-fortran

`uint-fortran`—Unsigned Integer in Fortran Interoperable with C

In Fortran, handling unsigned integers that are interoperable with C can be a bit troublesome,
as Fortran natively does not contain unsigned integer types.

However, with the `uint-fortran` package, you can work with unsigned integer seemlessly.

Let's break down how to use this package and demonstrate its interoperability with C functions.

## Build

Add below in `fpm.toml` of your package.
```toml
[dependencies]
uint-fortran = {git ="https://github.com/ShinobuAmasaki/uint-fortran"}
```

## Usage

### `use` statement 
To use the `uint-fortran` package, start by including the `unsigned` module in your Fortran program.

```fortran
use unsigned
```

Make sure not to use the `only:` clause with this module to ensure proper operator overrides.

### Declaration

Now, you can declare unsigned integer variables of 32- and 16-bit lengths, such as `uint32` and `uint16`.

```fortran
type(uint32) :: a
```

### Assignment and Operations

You can assign values to `uint32` variables just like regular integer types:

```fortran
a = 3456
```
However, if you attempt to assign a negative number, it will underflow:

```fortran
a = -10
print *, a   ! This will print '4294967286'.
```

If you assign  a number greater than 4294967295, it will overflow:

```fortran
a = 4294967296_8
print *, a   ! This will print '0'.
```

If you want to use a `uint32` value as an `integer(8)` type, you can do so by using the `int` function that is overloaded.
```
int(a)
```


## Interoperability with C 

### Call C Function from Fortran

#### 1. Pass `uint32` as an Argument

Suppose you have a C code that takes an unsigned integer as an argument and prints it using `printf`.
```c
#include <stdio.h>

void unsigned_val_print(unsigned int val )
{
   printf("%u\n", val);
   return;
}
```

To call this C function from Fortran, you need to create an interface block as follows.
```fortran
use, intrinsic :: iso_c_binding
interface
   subroutine unsigned_val_print(val) bind(c)
      import c_int
      implicit none
      integer(c_int), intent(in), value :: val
   end subroutine unsigned_val_print
end interface
```

In standard Fortran, unsigned integers don't exist, so you must use the equivalent signed `c_int` type when passing arguments to C functions.

The `uint32` type from this package corresponding to the C `unsigned int` type. You can pass its components to C functions as demonstrated below.


```fortran
type(uint32) :: a

a = 4294967295_8  ! An example value that is maximum value of unsigned int

call unsigned_val_print(a%u32)
```
Running this code will display the result, which in this case would be `4294967295`

Of course, you can also use these unsigned integers Fortran itself.

```
print *, a 　　! This will display the same value, 4294967295
```


Here's the complete Fortran program for the example:
```fortran
program main
   use unsigned_int32
   use, intrinsic :: iso_c_binding
   implicit none
   
   interface
      subroutine unsigned_val_print(val) bind(c)
         import c_int, c_int64_t
         implicit none
         integer(c_int), intent(in), value :: val
      end subroutine unsigned_val_print
   end interface

   block
      type(uint32) :: a 
      
      a = 4294967295_8

      call unsigned_val_print(a%u32)

      print *, a    ! This will display the same value 4294967295 to standard output.
   end block

end program main 
```

#### 2. Getting a `uint32` as a Returned Value from C

Next, let's consider a C function that always returns the value `2333444555`, which exceeds
the range of 32-bit signed integer:

```c
unsigned int return_unsigned(void)
{
   unsigned int val = 2333444555;
   return val;
}
```

To call this C function from Fortran, you can create a Fortran interface like this:

```fortran
use unsigned

interface
   function return_unsigned () bind(c, name='return_unsigned') result(res)
      import uint32
      implicit none
      type(uint32) :: res
   end function return_unsigned
end interface 
```
 
By declaring this interface, you can use the function in Fortran.

When a function returns a `uint32`, you simply assign it to a regular derived type variable,
just like this:

```
block
   type(uint32) :: result
   result = return_unsigned()

   print *, result
end block
```

Running this code will correctly display the value previously defined in C on the standard output:

```
   2333444555
```

Here's the complete Fortran code used this example:

```fortran
program main
   use unsigned
   implicit none 

   interface
      function return_unsigned () bind(c, name='return_unsigned') result(res)
         import uint32
         implicit none
         type(uint32) :: res
      end function return_unsigned
   end interface 

   block
      type(uint32) :: result
      result = return_unsigned()

      print *, result
   end block

end program main
```

## Internal Structure

The `uint32` derived type is declared within the `unsigned_int32` module as follows:

```fortran
   type, public, bind(c) :: uint32
      integer(int32) :: u32
   end type uint32
```

Internally, it simply contains a signed integer with the same 32-bit length.

To enable proper handling of `uint32` as an unsigned integer in Fortran, operator
overload for assginment, arithmetic operations, comparison operations,
and exponentiation, as well as derived type I/O feature, are utilized.
