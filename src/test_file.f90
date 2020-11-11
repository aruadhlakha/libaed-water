function r4_normal_ab ( a, b, seed )

!*****************************************************************************80
!
!! R4_NORMAL_AB returns a scaled pseudonormal R4.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A, the mean of the PDF.
!
!    Input, real ( kind = 4 ) B, the standard deviation of the PDF.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 4 ) R4_NORMAL_AB, a sample of the normal PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r4_normal_ab
  real ( kind = 8 ), parameter :: r4_pi = 3.141592653589793E+00
  real ( kind = 8 ) random_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  integer (kind=4) val
  real ( kind = 8 ) random_val

  val=3624378;
  random_val=0.99
  call random_number(random_val)
  seed=IDINT(random_val*seed)
  r1 = random_num ( seed )
  r2 = random_num ( seed )
  x = sqrt ( - 2.00000E+00 * log ( r1 ) ) * cos ( 2.00000E+00 * r4_pi * r2 )

  r4_normal_ab = a + b * x

  return
end

function random_num ( seed )

  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) random_num

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'random_num - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  random_num = real ( seed, kind = 8 ) * 4.656612875E-10



  return
end
