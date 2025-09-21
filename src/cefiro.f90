module cefiro

  use, intrinsic :: iso_c_binding

  implicit none
  private
  public :: calculate_shear
contains

  real(c_double) function calculate_alpha(w_low, w_high, h_low, h_high) result(res)
    real(c_double), intent(in) :: w_low, w_high
    real(c_double), intent(in) :: h_low, h_high

    res = log(w_high/w_low) / log(h_high/h_low)

  end function calculate_alpha

  subroutine calculate_shear(n, ws1, ws2, dir, hl, hh, count, wsl, wsh, shear) &
  &bind(C, name = "calculate_shear_c")
    integer(c_int), intent(in)            :: n
    real(c_double), intent(in)            :: ws1(n), ws2(n), dir(n)
    real(c_double), intent(in)            :: hl, hh
    real(c_double), intent(in out)        :: count(16)
    real(c_double), intent(in out)        :: wsl(16)
    real(c_double), intent(in out)        :: wsh(16)
    real(c_double), intent(in out)        :: shear(16)

    real(c_double)                        :: alpha
    integer                               :: i

    real(c_double), dimension(16, 4)      :: alpha_dir

    ! clean shear table
    do i = 1, 16
      count(i) = 0
      wsl(i) = 0
      wsh(i) = 0
      shear(i) = 0
    end do

    ! clean alpha_do table
    do i=1, 16
      !alpha_dir(i, 1) = (i-1) * 22.5  ! sectors
      alpha_dir(i, 1) = 0   ! shear
      alpha_dir(i, 2) = 0   ! count
      alpha_dir(i, 3) = 0   ! wsl (wind speeds at lower level)
      alpha_dir(i, 4) = 0   ! ws2 (wind speeds at higher level)
    end do

    do i = 1, n
      alpha = calculate_alpha(ws1(i), ws2(i), hl, hh)
      if ((dir(i) >= 0 .and. dir(i) < 11.25) .or. dir(i) > 348.75) then
        alpha_dir(1, 1) = alpha_dir(1, 1) + alpha
        alpha_dir(1, 2) = alpha_dir(1, 2) + 1
        alpha_dir(1, 3) = alpha_dir(1, 3) + ws1(i)
        alpha_dir(1, 4) = alpha_dir(1, 4) + ws2(i)
      end if
      ! NNW
      if (dir(i) >= 11.25 .and. dir(i) < 33.75) then
        alpha_dir(2, 1) = alpha_dir(2, 1) + alpha
        alpha_dir(2, 2) = alpha_dir(2, 2) + 1
        alpha_dir(2, 3) = alpha_dir(2, 3) + ws1(i)
        alpha_dir(2, 4) = alpha_dir(2, 4) + ws2(i)
      end if
      ! NE
      if (dir(i) >= 33.75 .and. dir(i) < 56.25) then
        alpha_dir(3, 1) = alpha_dir(3, 1) + alpha
        alpha_dir(3, 2) = alpha_dir(3, 2) + 1
        alpha_dir(3, 3) = alpha_dir(3, 3) + ws1(i)
        alpha_dir(3, 4) = alpha_dir(3, 4) + ws2(i)
      end if
      ! ENE
      if (dir(i) >= 56.25 .and. dir(i) < 78.75) then
        alpha_dir(4, 1) = alpha_dir(4, 1) + alpha
        alpha_dir(4, 2) = alpha_dir(4, 2) + 1
        alpha_dir(4, 3) = alpha_dir(4, 3) + ws1(i)
        alpha_dir(4, 4) = alpha_dir(4, 4) + ws2(i)
      end if
      ! E
      if (dir(i) >= 78.75 .and. dir(i) < 101.25) then
        alpha_dir(5, 1) = alpha_dir(5, 1) + alpha
        alpha_dir(5, 2) = alpha_dir(5, 2) + 1
        alpha_dir(5, 3) = alpha_dir(5, 3) + ws1(i)
        alpha_dir(5, 4) = alpha_dir(5, 4) + ws2(i)
      end if
      ! ESE
      if (dir(i) >= 101.25 .and. dir(i) < 123.75) then
        alpha_dir(6, 1) = alpha_dir(6, 1) + alpha
        alpha_dir(6, 2) = alpha_dir(6, 2) + 1
        alpha_dir(6, 3) = alpha_dir(6, 3) + ws1(i)
        alpha_dir(6, 4) = alpha_dir(6, 4) + ws2(i)
      end if
      ! SE
      if (dir(i) >= 123.75 .and. dir(i) < 146.25) then
        alpha_dir(7, 1) = alpha_dir(7, 1) + alpha
        alpha_dir(7, 2) = alpha_dir(7, 2) + 1
        alpha_dir(7, 3) = alpha_dir(7, 3) + ws1(i)
        alpha_dir(7, 4) = alpha_dir(7, 4) + ws2(i)
      end if
      ! SSE
      if (dir(i) >= 146.25 .and. dir(i) < 168.75) then
        alpha_dir(8, 1) = alpha_dir(8, 1) + alpha
        alpha_dir(8, 2) = alpha_dir(8, 2) + 1
        alpha_dir(8, 3) = alpha_dir(8, 3) + ws1(i)
        alpha_dir(8, 4) = alpha_dir(8, 4) + ws2(i)
      end if
      ! S
      if (dir(i) >= 168.75 .and. dir(i) < 191.25) then
        alpha_dir(9, 1) = alpha_dir(9, 1) + alpha
        alpha_dir(9, 2) = alpha_dir(9, 2) + 1
        alpha_dir(9, 3) = alpha_dir(9, 3) + ws1(i)
        alpha_dir(9, 4) = alpha_dir(9, 4) + ws2(i)
      end if
      ! SSW
      if (dir(i) >= 191.25 .and. dir(i) < 213.75) then
        alpha_dir(10, 1) = alpha_dir(10, 1) + alpha
        alpha_dir(10, 2) = alpha_dir(10, 2) + 1
        alpha_dir(10, 3) = alpha_dir(10, 3) + ws1(i)
        alpha_dir(10, 4) = alpha_dir(10, 4) + ws2(i)
      end if
      ! SW
      if (dir(i) >= 213.75 .and. dir(i) < 236.25) then
        alpha_dir(11, 1) = alpha_dir(11, 1) + alpha
        alpha_dir(11, 2) = alpha_dir(11, 2) + 1
        alpha_dir(11, 3) = alpha_dir(11, 3) + ws1(i)
        alpha_dir(11, 4) = alpha_dir(11, 4) + ws2(i)
      end if
      ! WSW
      if (dir(i) >= 236.25 .and. dir(i) < 258.75) then
        alpha_dir(12, 1) = alpha_dir(12, 1) + alpha
        alpha_dir(12, 2) = alpha_dir(12, 2) + 1
        alpha_dir(12, 3) = alpha_dir(12, 3) + ws1(i)
        alpha_dir(12, 4) = alpha_dir(12, 4) + ws2(i)
      end if
      ! W
      if (dir(i) >= 258.75.and. dir(i) < 281.25) then
        alpha_dir(13, 1) = alpha_dir(13, 1) + alpha
        alpha_dir(13, 2) = alpha_dir(13, 2) + 1
        alpha_dir(13, 3) = alpha_dir(13, 3) + ws1(i)
        alpha_dir(13, 4) = alpha_dir(13, 4) + ws2(i)
      end if
      ! WNW
      if (dir(i) >= 281.25 .and. dir(i) < 303.75) then
        alpha_dir(14, 1) = alpha_dir(14, 1) + alpha
        alpha_dir(14, 2) = alpha_dir(14, 2) + 1
        alpha_dir(14, 3) = alpha_dir(14, 3) + ws1(i)
        alpha_dir(14, 4) = alpha_dir(14, 4) + ws2(i)
      end if
      ! NW
      if (dir(i) >= 303.75 .and. dir(i) < 326.25) then
        alpha_dir(15, 1) = alpha_dir(15, 1) + alpha
        alpha_dir(15, 2) = alpha_dir(15, 2) + 1
        alpha_dir(15, 3) = alpha_dir(15, 3) + ws1(i)
        alpha_dir(15, 4) = alpha_dir(15, 4) + ws2(i)
      end if
      ! NNW
      if (dir(i) >= 326.25 .and. dir(i) < 348.75) then
        alpha_dir(16, 1) = alpha_dir(16, 1) + alpha
        alpha_dir(16, 2) = alpha_dir(16, 2) + 1
        alpha_dir(16, 3) = alpha_dir(16, 3) + ws1(i)
        alpha_dir(16, 4) = alpha_dir(16, 4) + ws2(i)
      end if
    end do

  ! averaging, but not for count
  do i=1, 16
    shear(i) = alpha_dir(i, 1) / alpha_dir(i, 2)
    count(i) = alpha_dir(i, 2)
    wsl(i) = alpha_dir(i, 3) / alpha_dir(i, 2)
    wsh(i) = alpha_dir(i, 4) / alpha_dir(i, 2)
  end do

end subroutine calculate_shear

end module cefiro
