      PROGRAM geodesic
        IMPLICIT NONE
        
        !DOUBLE PRECISION,dimension(3),save::pos_cart
        !DOUBLE PRECISION,dimension(3),save::pos_spher
!1/pi*180
        double precision, parameter :: phi1=0.0 
        double precision, parameter :: phi2=55.0
        double precision, parameter :: theta1=0.0 
        double precision, parameter :: theta2=45.0
        double precision, parameter :: radius=1.0
        double precision, parameter :: toangle = 57.29577951308232
        double precision :: angle_if
        double precision :: sina, sinb, sinab
        double precision :: ip1, ip2, ip3
        double precision :: fp1, fp2, fp3
        double precision :: p1, p2, p3
        double precision :: phi_i, theta_i, radius_i
        integer :: numrots=21
        integer :: rotstepper
        double precision :: t
 
        !call spher2cart(radius ,phi1, theta1, ip1, ip2, ip3 )
        call spher2cart(radius , phi1, theta1, ip1 , ip2, ip3)
        call spher2cart(radius , phi2, theta2, fp1 , fp2, fp3)
        write(*,*) "initial position ", phi1, theta1 , ip1, ip2, ip3
        write(*,*) "final position ", phi2, theta2 , fp1, fp2, fp3
        call cart2spher(fp1, fp2, fp3, radius_i, phi_i, theta_i)
        call get_angle( ip1, ip2, ip3, fp1, fp2, fp3, angle_if)
        write(*,*) angle_if

        do rotstepper=1,numrots
                t=1.0/(numrots-1) * (rotstepper-1)
                write(*,*) rotstepper, t
                sina=dsin(angle_if*(1-t)/toangle)
                sinb=dsin(angle_if*t/toangle)
                sinab=dsin(angle_if/toangle)
                p1 = ip1 * sina/sinab + fp1*sinb/sinab
                p2 = ip2 * sina/sinab + fp2*sinb/sinab
                p3 = ip3 * sina/sinab + fp3*sinb/sinab
                !write(*,*) p1, p2, p3
                call cart2spher(p1,p2,p3, radius_i, phi_i, theta_i)
                write(*,*) radius_i, phi_i, theta_i
        end do

      END PROGRAM geodesic

!--input is spherical polars, output is cartesian
        SUBROUTINE spher2cart(r, phi, theta, x,y,z)
                DOUBLE PRECISION, INTENT(IN) :: r ,phi,theta
                DOUBLE PRECISION, INTENT(OUT) :: x,y,z
                double precision, parameter :: pi = 3.1415926535897932D0
                double precision, parameter :: toangle = 57.29577951308232
        x = r * dsin(phi/toangle) * dcos(theta/toangle)
        y = r * dsin(phi/toangle) * dsin(theta/toangle)
        z = r * dcos(phi/toangle)
        END SUBROUTINE spher2cart
        
!--input is cartesian, output is spherical
        SUBROUTINE cart2spher(x,y,z ,r, phi,theta)
                DOUBLE PRECISION, INTENT(IN)::x,y,z
                DOUBLE PRECISION, INTENT(OUT) ::r
                DOUBLE PRECISION, INTENT(OUT)::phi,theta
                double precision, parameter :: pi = 3.1415926535897932D0
                double precision, parameter :: toangle = 57.29577951308232
        r = dsqrt(x*x + y*y + z*z )
        theta = datan2(y, x) /pi * 180.0
        phi = dacos(z/r) /pi * 180.0
        !write(*,*) 'cart2spher' , theta, phi
        END SUBROUTINE cart2spher

        !DOUBLE PRECISION FUNCTION angle(x1,y1,z1, x2,y2,z2)
        !        DOUBLE PRECISION :: x1, y1, z1, x2, y2, z2
        !        DOUBLE PRECISION :: r1, r2, inner
        !        double precision, parameter :: pi = 3.1415926535897932D0
        !        double precision, parameter :: toangle = 57.29577951308232
        !        r1 = dsqrt(x1*x1+y1*y1+z1*z1)
        !        r2 = dsqrt(x2*x2+y2*y2+z2*z2)
        !        inner = x1*x2+y1*y2+z1*z2
        !        angle = dacos( inner/r1/r2 )/pi * 180.0
        !        !write(*,*) 'angle', r1, r2, inner, angle
        !END

        SUBROUTINE get_angle(x1,y1,z1, x2,y2,z2, angle)
                DOUBLE PRECISION, INTENT(IN) :: x1, y1, z1, x2, y2, z2
                DOUBLE PRECISION, INTENT(OUT) :: angle
                DOUBLE PRECISION :: r1, r2, inner
                double precision, parameter :: pi = 3.1415926535897932D0
                double precision, parameter :: toangle = 57.29577951308232
                r1 = dsqrt(x1*x1+y1*y1+z1*z1)
                r2 = dsqrt(x2*x2+y2*y2+z2*z2)
                inner = x1*x2+y1*y2+z1*z2
                angle = dacos( inner/r1/r2 )/pi * 180.0
                !write(*,*) 'angle', r1, r2, inner, angle
        END
