program DiferenciasDivididas
!Cuando compilemos este programa nos apareceran los datos en una lista, tomamos en cuenta los de la primera tabla hasta donde aparezca un doble 0, es decir ira de; las diferencias divididas de la primera tabla sera la primera serie de datos que tenga secuencia de cero a 5, cero a 4, cero a 3, cero a 2, cero a 1 y 0. La segunda tabla correspondera a la segunda secuencia de datos empezando de 0 a 4, 0 a 3, 0 a 2, 0 a 1 y 0.
!Comenzamos definiendo el intervalo donde se leera la variable dd, en este caso consideramos que esta variable toma dos valores reales y nos arroja un valor real.
real :: dd(0:5,0:5),x(0:5)
!Primera tabla del problema 1
!Como vimos en clase, podemos considerar los resultados de las diferencias dividas como una matriz triangular superior, en este caso comenzamos con los datos que nos proporcionan correspondientes a la primera columna es decir todos los valores dd(0,i) con i variando de 0 a 5.
x(0) = 1.0
x(1) = 1.8
x(2) = 3.0
x(3) = 4.0
x(4) = 5.0
x(5) = 0.6545
dd(0,0) = 3.6
dd(0,1) = 2.0
dd(0,2) = 1.2
dd(0,3) = 0.9
dd(0,4) = 0.72
dd(0,5) = 0.6545
!Consideramos este do para formar los elementos de la primera columna, es decir dejando la primera entrada en cero y variando j como se indica
    do j = 0,5
!Ya que de acuerdo a las formulas para las diferencias dividas de orden cero corresponden a los valores que toma la funcion en cada uno de los argumentos, simplemento imprimimos estos valores en la pantalla.
    write(*,*) j, dd(0,j)
    enddo
!Del mismo modo, solo que ingresamos la formula para orden uno, y las diferencias divididas seran los elementos de la matriz correspondiente a la segunda columna empezamos en dd(1,j) y tendra un elemento menos que la anterior de modo que corremos el indice de 0 a 4.
        do j = 0,4
            dd(1,j)=(dd(0,j+1)-dd(0,j))/(x(j+1)-x(j))
        write(*,*) j, dd(1,j)
        enddo
!Esto lo hacemos hasta que corramos el indice de cero a cero que corresponde al ultimo elemento de la esquina superior derecha de la matriz triangular superior, es decir usamos la formula para calcular las diferencias dividas de orden cinco.
            do j = 0,3
                dd(2,j)=(dd(1,j+1)-dd(1,j))/(x(j+2)-x(j))
            write(*,*) j, dd(2,j)
            enddo

                do j = 0,2
                    dd(3,j)=(dd(2,j+1)-dd(2,j))/(x(j+3)-x(j))
                write(*,*) j, dd(3,j)
                enddo

                    do j = 0,1
                        dd(4,j)=(dd(3,j+1)-dd(3,j))/(x(j+4)-x(j))
                    write(*,*) j, dd(4,j)
                    enddo

                        do j = 0,0
                            dd(5,j)=(dd(4,j+1)-dd(4,j))/(x(j+5)-x(j))
                        write(*,*) j, dd(5,j)
                        enddo

!Segunda tabla del problema 1


                        x(0) = 0.0
                        x(1) = 1.0
                        x(2) = 2.0
                        x(3) = 3.0
                        x(4) = 4.0
                        dd(0,0) = 1.0
                        dd(0,1) = 0.36788
                        dd(0,2) = 0.13534
                        dd(0,3) = 0.04979
                        dd(0,4) = 0.01832

                    do j = 0,4
                    write(*,*) j, dd(0,j)
                    enddo

                do j = 0,3
            dd(1,j)=(dd(0,j+1)-dd(0,j))/(x(j+1)-x(j))
                write(*,*) j, dd(1,j)
                enddo

            do j = 0,2
        dd(2,j)=(dd(1,j+1)-dd(1,j))/(x(j+2)-x(j))
            write(*,*) j, dd(2,j)
            enddo

        do j = 0,1
    dd(3,j)=(dd(2,j+1)-dd(2,j))/(x(j+3)-x(j))
        write(*,*) j, dd(3,j)
        enddo

do j = 0,0
dd(4,j)=(dd(3,j+1)-dd(3,j))/(x(j+4)-x(j))
write(*,*) j, dd(4,j)
enddo

end


