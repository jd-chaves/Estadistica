	#variables globales del programa

	#en el vector x se guardan las 500 muestras de tamanho n generadas por el metodo de F inversa
	x <- c();

	#la variable y tiene los mismos valores que x pero es una lista de listas
	y <- c();
	
	#este vector almacena los 500 valores calculados del estimador de momentos, uno por cada muestra
	estimadoresMomentos <- c();
	
	#este vector almacena los 500 valores calculados del estimador de maxima verosimilitud, uno por cada muestra
	estimadoresVerosimilitud <- c();
	
	#variable global que almacena las eficiencias relaticvas para cada n
	eficienciasRelativas <- c()
	

	#funcion para generar la muestra de tamanho n de una Beta(1,1), se retorna un vector de tamanho n
	muestraBeta <- function(alpha, n)
	{ 
		#genera una uniforme y le aplica F inversa, note que se generan los n datos de una vez
		return(runif(n, 0,1)^(1/alpha));
	}

	#la parte 1 del proyecto
	parte1 <- function()
	{
		#se genera una muestra de 50000 datos de una B(0.3,1)
		arr <- muestraBeta(0.3, 50000);


		#se plotea el histograma
		jpeg("Histograma Beta(0.3,1).jpg",units="in", width=5, height=5, res=200)
		hist(arr,main="Histograma Beta(0.3,1)",xlab="Valores muestra", ylab="Frecuencia");
		dev.off();


		#se plotea el qq-plot comparando la muestra del metodo F inversa con la muestra generada por R
		jpeg("QQ Plot Beta(0.3,1).jpg",units="in", width=5, height=5, res=200)
		qqplot(arr, rbeta(50000, 0.3, 1),main="QQ Plot Beta(0.3,1)",xlab="muestra F inversa", ylab="muestra R");
		dev.off()


		#se genera una muestra de 50000 datos de una B(1,1)
		arr <- muestraBeta(1, 50000);

		#se plotea el histograma
		jpeg("Histograma Beta(1,1).jpg",units="in", width=5, height=5, res=200)
		hist(arr,main="Histograma Beta(1,1)",xlab="Valores muestra",ylab="Frecuencia");	
		dev.off();

		#se plotea el qq-plot comparando la muestra del metodo F inversa con la muestra generada por R
		jpeg("QQ Plot Beta(1,1).jpg",units="in", width=5, height=5, res=200);
		qqplot(arr, rbeta(50000, 1, 1),main="QQ Plot Beta(1,1)",xlab="muestra F inversa", ylab="muestra R");
		dev.off();

		#se genera una muestra de 50000 datos de una B(3,1)
		arr <- muestraBeta(3, 50000);

		#se plotea el histograma
		jpeg("Histograma Beta(3,1).jpg",units="in", width=5, height=5, res=200);
		hist(arr,main="Histograma Beta(3,1)",xlab="Valores muestra",ylab="Frecuencia");
		dev.off();

		#se plotea el qq-plot comparando la muestra del metodo F inversa con la muestra generada por R
		jpeg("QQ Plot Beta(3,1).jpg",units="in", width=5, height=5, res=200);
		qqplot(arr, rbeta(50000, 3, 1),main="QQ Plot Beta(3,1)",xlab="muestra F inversa", ylab="muestra R");
		dev.off();
	}

	#funcion auxiliar para calcular el valor del estimador de momentos
	auxMomentos <- function(param)
	{
		temp <- mean(param);
		return((temp)/(1-temp));
	}

	#funcion auxiliar para calcular el valor del estimador de maxima verosimilitud
	auxVerosimilitud <- function(param)
	{
    	return(-length(param)/sum(unlist(lapply(param,log))));
	}

	#funcion que calcula para cada muestra el valor del estimador de momentos y retorna el error cuadratico medio de dicho estimador
	simulacionMomentos <- function(alpha)
	{
		#se calculan los estimadores de momentos para cada una de las 500 muestras
		estimadoresMomentos <<- unlist(lapply(y, auxMomentos));
		#se calculan los sesgos para el estimador de momentos restando alpha del promedio de los valores
		#calculados para el estimador
		sesgoMomentos <- mean(estimadoresMomentos)-alpha; 
		#se calcula la varianza de los 500 valores calculados para el estimador de momentos
		varMomentos <- var(estimadoresMomentos);
		#se calcula el error cuadratico medio del estimador de momentos
		ecmMomentos <- varMomentos + sesgoMomentos^2;

		#se retorna el error cuadratico medio del estimador de momentos
		return(ecmMomentos);
	}
	
	#funcion que calcula para cada muestra el valor del estimador de maxima verosimilitud y retorna el error cuadratico medio de dicho estimador
	simulacionVerosimilitud <- function(alpha)
	{
		#se calculan los estimadores de macima verosimilitud para cada una de las 500 muestras 
		estimadoresVerosimilitud <<- unlist(lapply(y, auxVerosimilitud));
		#se calculan los sesgos para el estimador de maxima verosimilitud restando alpha del promedio de 
		#los valores calculados para el estimador
		sesgoVerosimilitud <- mean(estimadoresVerosimilitud)-alpha;
		#se calcula la varianza de los 500 valores calculados para el estimador de maxima verosimilitud
		varVerosimilitud <- var(estimadoresVerosimilitud);
		#se calcula el error cuadratico medio del estimador de maxima verosimilitud
		ecmVerosimilitud <- varVerosimilitud + sesgoVerosimilitud^2;

		#se retorna el error cuadratico medio del estimador de maxima verosimilitud
		return(ecmVerosimilitud);	
	}


	simulacion <- function(alpha, n)
	{
		#se crean 500 muestras de tamanho n de una Beta(alpha, 1)
		x <<- replicate(500, muestraBeta(alpha,n));
		#se convierte x en una lista de listas
		y <<- lapply(seq_len(ncol(x)), function(i) x[,i]);

		#se calcula el error cuadratico medio del estimador de momentos
		ecmMomentos <- simulacionMomentos(alpha);
		#se calcula el error cuadratico medio del estimador de maxima verosimilitud
		ecmVerosimilitud <- simulacionVerosimilitud(alpha);

		#se plotea el boxplot de ambos estimadores, note que cuando se llaman las funciones simulacionMomentos y simulacionVerosimilitud quedan almacenados los valores de los estimadores en las variables globales estimadoresMomentos y estimadoresVerosimilitud respectivamente
		jpeg(paste("boxplot alpha=",alpha," n=",n,".jpg", sep=""),units="in", width=5, height=5, res=200);
		boxplot(estimadoresMomentos, estimadoresVerosimilitud, main=paste("boxplot alpha=",alpha," n=",n, sep=""),
			names=(c("momentos", "verosimilitud")));
		dev.off();

		#se retorna un vector de tamanho 2 que contiene los errores cuadraticos medios de los estimadores
		return(c(ecmMomentos,ecmVerosimilitud));
	}

	#funcion que corre la simulacion para un alpha dado
	graficas <- function(param)
	{
		#se declara un arreglo con los n que dice el enunciado
		n <- c(50,100,200,500,1000);

		par <- seq(2,10, by=2);
		impar <- seq(1, 9, by=2);

		#se calculan para cada n los valores de los estimadores (a partir de las 500 muestras)
		resp <<- unlist(lapply(n, simulacion, alpha=param));

		#preguntarle a Adolfo que hago con las eficiencias relativas

		#se calculan las eficiencias relativas, los valores del estimador de momentos estan en las posiciones impares del arreglo y los valores del estimador de maxima verosimilitud estan en las posiciones impares
		eficienciasRelativas <<- resp[impar]/resp[par];

		#se plotea la grafica de ambos estimadores en escala logaritmica
		jpeg(paste("logaritmica alpha=",param,".jpg", sep=""),units="in", width=5, height=5, res=200);
		plot(log(n), log(resp[par]), type="o",col="red", main = bquote(paste("ln(n) vs ln(ECM(", theta,")), alpha=",.(param), sep="")),
			xlab="Tamanho muestra",ylab="Error cuadratico medio", ylim=c(min(log(resp[par])),max(log(resp[impar]))));
		lines(log(n),type="o", col="blue", log(resp[impar]));
		legend("bottomleft", legend=c("Momentos", "Verosimilitud"),
       		col=c("blue", "red"),lty = c(1,1),
      		pch = "o",  inset = .05);
		dev.off();
	}

	#funcion que corre todo el proyecto, es como el main()
	principal <- function()
	{
		#se crea el directorio que almacenara las graficas
		make.dir("Proyecto1 Chaves-Perez");
		setwd("Proyecto1 Chaves-Perez");

		#se corre la parte 1 del proyecto
		parte1();

		#se corre para cada alpha del enunciado la simulacion
		graficas(0.3);
		graficas(1);
		graficas(3);
		graficas(10);

		setwd("..");
	}


	make.dir <- function(fp) {
		if(!file.exists(fp)) {  
		dir.create(fp)
		} else {     
		unlink(fp, recursive = TRUE)
		dir.create(fp)
  	}
} 