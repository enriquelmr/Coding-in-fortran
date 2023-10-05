!  PotNewtoniano.f90 
!
!  FUNCTIONS:
!  PotNewtoniano - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: PotNewtoniano
!
!  PURPOSE:  This program seeks the solution of the Newton Field Equation in
!            spherical symmetry...
!
!            $\partial_r^2\phi+\frac{2}{r}\partial_r\phi=4\pi\rho$
!
!            We are considering a mass distribution $\rho$ and
!            ratio equal to $R$.
!
!****************************************************************************

program PotNewtoniano

    implicit none

    real(8):: rlim                  ! Upper limit for r
    real(8):: radius                ! Sphere radius
    real(8):: h                     ! Step

    print*
    print*, 'PROGRAM THAT SOLVES POISSON EQUATION FOR A NEWTONIAN FIELD'
    print*
    print*
    print*, 'Give the upper limit for r'
    read*, rlim
    print*, 'Give the radius of the sphere'
    read*, radius
    print*, 'Enter the stepsize h'
    read*, h


    open(unit=1, file="RK2Newton.dat")
        call RK2(rlim,h,radius)
    close(1)

end program PotNewtoniano

subroutine RK2(rlim,h,radius)
    implicit none

    real(8):: rlim,h,radius
    real(8):: phi,u,r               ! phi, u=dphi/dr, r
    real(8):: k1,k2,l1,l2           ! Usual quantities for RK2 method
    real(8):: error
    real(8):: pi                    ! pi=3.141519...
    real::    f,g                   ! Functions for RK2 method
    real::    rho                   ! Mass distribution

    pi=acos(-1d0)

    ! Values of phi and u=dphi/dr at r=0. This is important, I considered the values of phi(r=0)
    ! and dphi(r=0)/dr as those given in the theory (problem VI exercise 1, Homework 1)
    r=0
    phi=-2d0*pi*rho(r,radius)*(radius**2)
    u=0

    write(1,*) r,phi

    r=r+h

    ! Iterate
    do while(r <= rlim)

        k1=f(r,phi,u)
        l1=g(r,phi,u,radius)
        k2=f(r+h,phi+k1*h,u+l1*h)
        l2=g(r+h,phi+k1*h,u+l1*h,radius)

        phi=phi+h*(k1+k2)/2d0
        u=u+h*(l1+l2)/2d0

        write(1,*) r,phi

        r=r+h

    end do

end subroutine

! Definition of f=dphi/dr, for RK2 method
real function f(r,phi,u)
    implicit none

    real(8):: r,phi,u

    f=u

end function

! Definition of g=d^2phi/dr^2, for RK2 method
real function g(r,phi,u,radius)
    implicit none

    real(8):: r,phi,u
    real(8):: pi,radius
    real::    rho

    pi=acos(-1d0)

    g=-(2d0/r)*u + 4*pi*rho(r,radius)

    end function

! Definition of rho as a function of r

real function rho(r,radius)
    implicit none

    real(8)::r,radius

    ! Exercise 2
    if (r<=radius) then
        rho=1
    else
        rho=0
    end if

    ! Exercise 4
    !if (r<=radius) then
    !    rho=exp(-r**2)
    !else
    !    rho=0
    !end if

end function
