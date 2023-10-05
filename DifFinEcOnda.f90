!  DifFinEcOnda.f90
!
!  FUNCTIONS:
!  DifFinEcOnda - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: DifFinEcOnda
!
!  PURPOSE:  This program solves the Wave Equation by finite difference method
!            using central differences in time and space. Homework 2, Excercise 4
!
!****************************************************************************

    program DifFinEcOnda

    implicit none

    real(8):: rho
    real(8):: ht,hx
    real(8):: xmin,xmax,tmin,tmax

    real(8),allocatable,dimension(:,:) :: phi
    real(8),allocatable,dimension(:)   :: x
    real(8),allocatable,dimension(:)   :: t

    integer:: Nx,Nt
    integer:: i,j
    integer:: m,n

    real::    f,g

    print*, 'This program solves the Wave Equation by finite difference method'
    print*, 'using central differences in time and space. Homework 2, Excercise 4'
    print*
    print*, 'Give me the number of points in the grid for x'
    read*, Nx
    print*, 'Give me the number of points in the grid for t'
    read*, Nt
    print*, 'Give me the Courant Factor, in this case defined as rho=c delta t/ delta x'
    read*, rho
    print*, 'x min'
    read*, xmin
    print*, 'x max'
    read*, xmax
    print*, 't min'
    read*, tmin
    print*, 't max'
    read*, tmax

    open(unit=1, file="waveeq.dat",status='old')

    allocate(phi(0:Nx,0:Nt))
    allocate(t(0:Nt))
    allocate(x(0:Nx))

    hx=(xmax-xmin)/Nx
    ht=(tmax-tmin)/Nt

    do i=0,Nt
        t(i)=tmin+dble(i)*ht
    end do

    do j=0,Nx
        x(j)=xmin+dble(j)*hx
    end do

    ! Initial Conditions
    do m=0,Nx
        phi(0,m)=f(x(m))
    end do

    ! Boundary Conditions
    do n=0,Nt
        phi(n,0)=0
        phi(n,Nx)=0
    end do

    ! phi(1,m), in order to start evolution
    do m=1,Nx-1
        phi(1,m)=phi(0,m)+(1d0/2d0)*(rho**2)*(phi(0,m+1)-2*phi(0,m)+phi(0,m-1))+ht*g(x(m))
    end do

    ! Begin evolution
    do n=1,Nt-1
        do m=1,Nx-1
            phi(n+1,m)=2*phi(n,m)-phi(n-1,m)+(rho**2)*(phi(n,m+1)-2*phi(n,m)+phi(n,m-1))
        end do
    end do

    ! Save data
    do n=0,Nt
        do m=0,Nx
            write(1,*) x(n), t(m), phi(n,m)
        end do
    end do

    close(1)

    pause
end program DifFinEcOnda

!***********************************
!********** INITIAL DATA ***********
!***********************************

! f(x)=phi(0,x)

    real function f(x)
    implicit none

    real(8):: x
    real(8):: pi

    pi=acos(-1d0)

    f=sin(pi*x)

    end function

! g(x)=dphi(t=0,x)

    real function g(x)
    implicit none

    real(8):: x

    g=0

    end function
