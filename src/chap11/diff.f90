PROGRAM diff
IMPLICIT NONE
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6,r=37)
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL) :: ans, d_ans, d_error, d_fx, d_fxdx, d_dx, d_x = 0.15_DBL
INTEGER :: i
REAL(KIND=SGL) :: s_ans, s_error, s_fx, s_fxdx, s_dx, s_x = 0.15_SGL
WRITE(*, 1)
1 FORMAT ('        DX    TRUE ANS      SP ANS                DP ANS', &
          '   SP ERR   DP ERR')
ans = - (1.0_DBL / d_x**2)
step_size: DO i = 1, 10
  s_dx = 1.0 / 10.0**i
  d_dx = 1.0_DBL / 10.0_DBL**i
  s_fxdx = 1. / (s_x + s_dx)
  s_fx = 1. / s_x
  s_ans = (s_fxdx - s_fx) / s_dx
  s_error = (s_ans - REAL(ans)) / REAL(ans) * 100.
  d_fxdx = 1.0_DBL / (d_x + d_dx)
  d_fx = 1.0_DBL / d_x
  d_ans = (d_fxdx - d_fx) / d_dx
  d_error = (d_ans - ans) / ans * 100.
  WRITE(*, 100) d_dx, ans, s_ans, d_ans, s_error, d_error
  100 FORMAT (ES10.3, F12.7, F12.7, ES22.14, F9.3, F9.3)
END DO step_size

END PROGRAM diff
