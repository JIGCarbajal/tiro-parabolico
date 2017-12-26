!======================================================================
!	Archivo: tiroParabolicoRebote.f90
!	Tiro parabólico con rebote en y=0 y perdida de energía
!	x(t) = x0 + v0x*t, y(t) = y0 + v0y*t - (1/2)*g*t^2
!----------------------------------------------------------------------

program tiroParabolicoRebote

	implicit none

!----------------------------------------------------------------------
!	Declaracion de variables:

	real x0, y0, x, y, v0x, v0y, vx, vy, t, t0, tf, dt
	real Lx, Ly, Bx, By
	real, parameter :: g = 0 ! sin gravedad

!----------------------------------------------------------------------
!	Parametros del modelo:
	
	!posicion inicial	
	read *, x0
	read *, y0
	!velocidad incial
	read *, v0x
	read *, v0y
	!tiempo
	t0 = 0.0
	read *, tf
	read *, dt
	!Parametros adicionales
	read *, Lx ! limite máximo en x
	read *, Ly ! limite máximo en y
	read *, Bx ! coeficiente de perdida de velocidad en x
	read *, By ! coeficiente de perdida de velocidad en y
	

!----------------------------------------------------------------------
!	Inicializacion:

	x = x0
	y = y0
	vx = v0x
	vy = v0y
	t = t0

	if( x <= 0.0 .or. x >= Lx) stop "Valor de x0 inválido"
	if( y <= 0.0 .or. y >= Ly) stop "Valor de y0 inválido"	
	
	open(unit=11, file="tiroParabolicoRebote.dat") 

!----------------------------------------------------------------------
!	Calcular:

	do while( t <= tf )
		x = x + vx*dt
		y = y + vy*dt - 0.5*g*dt**2
		vy = vy - g*dt
		write(11,*)t, x, y
		t = t + dt
		if( x <= 0.0) then
		x = 0.0 
		vx = -vx ! Sin perdida en pared izquierda
		!vx = -(1-Bx)*vx ! perdida en pared izquierda
		endif
		if( x >= Lx) then
		x = Lx 
		vx = -vx ! Sin perdida en pared derecha
		!vx = -(1-Bx)*vx ! perdida en pared derecha		
		endif	
		if( y <= 0.0) then
		y = 0.0
		vy = -vy ! Sin perdida en pared inferior
		!vy = -(1-By)*vy ! perdida en pared inferior
		endif
		if( y >= Ly) then
		y = Ly
		vy = -vy ! Sin perdida en pared superior
		!vy = -(1-By)*vy ! perdida en pared superior
		endif
	enddo
	close(11)
end program tiroParabolicoRebote
