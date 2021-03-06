# Funci�n que calcula las medidas de duraci�n
# dado un vector de plazos (en a�os), un vector 
# de cupones y el tir
#
# Las medidas de duraci�n que se calculan son:
#     - Duraci�n
#     - Duraci�n modificada
#     - Sensibilidad
# 
# Adem�s se vuelve el precio (o cotizaci�n) actual como 
# primer valor. A continuaci�n, las medidas indicadas 
# anteriormente (en el mismo orden)
  
duracion = function(plazos, cupon, tir){
	precio = sum(cupon/(1+tir)^plazos)
	duracion = 1/precio* sum(plazos*cupon/(1+tir)^plazos)
	duracion_modificada = duracion/(1+tir)
	sensibilidad = duracion_modificada*precio/100
	return(c(precio, duracion, duracion_modificada, sensibilidad))
}


# Funci�n que calcula las medidas de convexidad
# dado un vector de plazos (en a�os), un vector 
# de cupones y el tir
#
# Las medidas de convexidad que se calculan son:
#     - Convexidad
#     - Convexidad modificada
#     - CCC
# 
# Adem�s se vuelve el precio (o cotizaci�n) actual como 
# primer valor. A continuaci�n, las medidas indicadas 
# anteriormente (en el mismo orden)

convexidad = function(plazos, cupon, tir){
	convexidad_absoluta = (1/100)^2*sum((cupon*plazos*(plazos+1))/(1+tir)^(plazos+1))
	precio = sum(cupon/(1+tir)^plazos)	
	convexidad_modificada = 100^2*convexidad_absoluta/precio
	ccc = 1/2*convexidad_absoluta
	return(c(precio, convexidad_absoluta, convexidad_modificada, ccc))
}

# Valores del activo 1
tir1 = 0.0395
cupones1 = c(46, 46, 1046)
plazos = c(0.945, 1.945, 2.945)

# Valores del activo 2
tir2 = 0.08
cupones2 = c(55.8, 55.8, 1055.8) 


# Calculamos los valores de duraci�n y convexidad para el activo 1
duracion(plazos, cupones1, tir1)
convexidad(plazos, cupones1, tir1)

# Calculamos los valores de duraci�n y convexidad para el activo 1
duracion(plazos, cupones2, tir2)
convexidad(plazos, cupones2, tir2)