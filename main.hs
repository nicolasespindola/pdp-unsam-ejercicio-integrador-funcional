vendedores = ["Martin", "Diego", "Claudio", "José"]

ventas = [((1,2,2006), "Martin", ["Monitor GPRS 3000", "Motherboard ASUS 1500"]),
 ((1,2,2006), "Diego", ["Monitor ASC 543", "Motherboard Pindorcho"]),
 ((10,2,2006), "Martin", ["Monitor ASC 543", "Motherboard ASUS 1200"]),
 ((12,2,2006), "Diego", ["Monitor GPRS 3000", "Motherboard ASUS 1200"]),
 ((4,3,2006), "Diego", ["Monitor GPRS 3000", "Motherboard ASUS 1500"])] 

precios = [("Monitor GPRS 3000", 200), 
 ("Motherboard ASUS 1500", 120),
 ("Monitor ASC 543", 250), 
 ("Motherboard ASUS 1200", 100),
 ("Motherboard Pindorcho", 30)]

precio (_,p) = p
descripcion (d,_) = d
find criterio = head . filter criterio

componentes (_,_,c) = c
vendedor (_,v,_) = v

type Venta = ((Int, Int, Int), [Char], [String])

--1.1. precioMaquina/1: recibe una lista de componentes, devuelve el precio de la máquina que se puede armar con esos componentes, 
--                      que es la suma de los precios de cada componente incluido. 
precioMaquina :: [String] -> Float
precioMaquina [] = 0
precioMaquina (parte : restoPartes) = precio (find ((==parte) . descripcion) precios) + precioMaquina restoPartes


--1.2. cantVentasComponente/1: recibe un componente y devuelve la cantidad de veces que fue vendido, o sea que formó parte de una máquina que se vendió. 
cantVentasComponente :: String -> Int
cantVentasComponente nombreComponente = length (filter ((elem nombreComponente).componentes) ventas)

--1.3. vendedorDelMes/1: se le pasa un par que representa (mes,año) y devuelve el nombre del vendedor que más vendió en plata en el mes. 
--                       O sea no cantidad de ventas, sino importe total de las ventas. El importe de una venta es el que indica la función precioMaquina.
--vendedorDelMes :: (Int, Int) -> [Venta]
--vendedorDelMes (mes, año) = maximum map vendedor (filtrarPorFecha (mes, año))

filtrarVentasPorFecha :: (Int, Int) -> [Venta]
filtrarVentasPorFecha (mes, año) = filter (\((_, mesVenta, añoVenta),_,_) -> mesVenta == mes && añoVenta == año) ventas