# Función que calcula las medidas de duración
# dado un vector de plazos (en años), un vector 
# de cupones y el tir
#
# Las medidas de duración que se calculan son:
#     - Duración
#     - Duración modificada
#     - Sensibilidad
# 
# Además se vuelve el precio (o cotización) actual como 
# primer valor. A continuación, las medidas indicadas 
# anteriormente (en el mismo orden)
  
duracion = function(plazos, cupon, tir){
	precio = sum(cupon/(1+tir)^plazos)
	duracion = 1/precio* sum(plazos*cupon/(1+tir)^plazos)
	duracion_modificada = duracion/(1+tir)
	sensibilidad = duracion_modificada*precio/100
	return(c(precio, duracion, duracion_modificada, sensibilidad))
}


# Función que calcula las medidas de convexidad
# dado un vector de plazos (en años), un vector 
# de cupones y el tir
#
# Las medidas de convexidad que se calculan son:
#     - Convexidad
#     - Convexidad modificada
#     - CCC
# 
# Además se vuelve el precio (o cotización) actual como 
# primer valor. A continuación, las medidas indicadas 
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


# Calculamos los valores de duración y convexidad para el activo 1
duracion(plazos, cupones1, tir1)
convexidad(plazos, cupones1, tir1)

# Calculamos los valores de duración y convexidad para el activo 1
duracion(plazos, cupones2, tir2)
convexidad(plazos, cupones2, tir2)