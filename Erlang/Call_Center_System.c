#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<time.h>

#define	lambda 0.02222f //calls/sec
#define	dm 120.0f //min: 1min  max: 5mins  Avg: 2mins (120s)
#define mean 60
#define std_deviation 20
#define	ARRIVAL 0
#define	DEPARTURE 1
#define	RANDOM 2

#define	SPECIFIC 0
#define	GENERAL 1

//#define MAX_AMOSTRAS 25

// Definição da estrutura da lista
typedef struct{
	int tipo;
	int area; //Ou atendimento geral ou especifico
	double tempo;
	double delayed; //quanto delay sofreu
	struct lista * proximo;
} lista;

typedef struct {
  double *array;
  size_t used;
  size_t size;
} Array;

//Função para gerar valores
double newEvent(int event){
  double u = ((double) rand() + 1)/ (RAND_MAX);

  if(event == ARRIVAL)
    return -(1/lambda)*log(u);
  else if(event == DEPARTURE)
    return -dm*log(u);
	else if(event == RANDOM)
		return (double)u;
}
double generalDuration(int area){

	double p;
	if(area == GENERAL) {	 //EXPONENTIAL   Min:60 Max:300 Avg: 120

		double u = newEvent(RANDOM);
		p = (double) 60 -dm*log(u);

		if(p > (double) 300)
    	p = (double) 300;
	}
	else { 	//GAUSSIAN  Min: 30 Max: 120 Avg: 60

		double k=newEvent(RANDOM);
		double u=newEvent(RANDOM);  //(Box-Muller)
		double teta = 2*M_PI*u;
		p = (sqrt(-2*log(k))*cos(teta));
		p = 30 + ((p*std_deviation) + mean);

		if(p > (double) 120)
			p = (double) 120;
	}
	return p;
}
double specificDuration(){

	double u = newEvent(RANDOM);
	double b = 150.0;
	return (double)(60 -b*log(u));
}
int getArea(){

	double p = (double)newEvent(RANDOM);
	int aux=0;

	if (p <= 0.3)
		aux = GENERAL;
	else
		aux = SPECIFIC;

	return aux;
}
double movingAverage(int n, double sample, double old_average){
		return (old_average*(n-1) + sample)/n;
}
double getAvg(double* array, int n){
    double k = 0;
    for (int i = 0; i < n; i++ )
        k += array[i];
    return k/n;
}
double STDeviation(double* array, int n, double average){
  int i;
  double k = 0;
  for (i = 0; i < n; i++)
  	k += (array[i] - average) * (array[i] - average);

  return sqrt(k / (n - 1));
}

//Funções para imprimir e tratar o histograma
int getMax(int *histogram, int size){
	int max =0;
	for(int z = 0; z < size; z++){
		if(histogram[z] > max)
			max = histogram[z];
	}
	return max;
}
int *getHistogram(double data, int size, int * histogram, double delta){
	for(int z = 0; z < size; z++){
		if(data >= z*delta && data < (z+1)*data) {
			histogram[z]++;
		}
    if(z == 24 && data >= (z+1)*data)
			histogram[z]++;
	}
	return histogram;
}
void printGraph(int histogram[25], int max){

  for(int height = max; height > 0; height--){

    printf("%02d ", height);
    for(int z=0; z < 25; z++){
			if(z==0)
			printf("  ");

      if(histogram[z] >= height)
        printf("=  ");
      else
        printf("  ");
    }
  	printf("\n");
  }
  printf("x: ");

    for(int z = 0; z < 25; z++){
      printf("%02d ", z);
    }
  printf(" (s)\n");

}

void histogramToFile(int *histogram){//quick solution
  //open file
  FILE *fptr = fopen("Histograma.txt", "wb");

  if (fptr == NULL) {
      printf("Could not open file");
  }
  for (int i=0; i<sizeof(histogram); i++) {
    fprintf(fptr,"%d %d\n", i, histogram[i]);
  }
  fclose(fptr);
}
void histogramToFile2(int *histogram){//quick solution
  //open file
  FILE *fptr = fopen("Prediction_Histogram.txt", "wb");

  if (fptr == NULL) {
      printf("Could not open file");
  }
  for (int i=0; i<sizeof(histogram); i++) {
    fprintf(fptr,"%d %d\n", i, histogram[i]);
  }
  fclose(fptr);
}



// Função que remove o primeiro elemento da lista
lista * remover (lista * apontador) {
	lista * lap = (lista *)apontador -> proximo;
	free(apontador);
	return lap;
}
// Função que adiciona novo elemento � lista, ordenando a mesma por tempo
lista * adicionar (lista * apontador, int n_tipo, int area, double delayed, double n_tempo) {
	lista * lap = apontador;
	lista * ap_aux, * ap_next;
	if(apontador == NULL)
	{
		apontador = (lista *) malloc(sizeof (lista));
		apontador -> proximo = NULL;
		apontador -> tipo = n_tipo;
		apontador -> area = area;
		apontador -> delayed = delayed;
		apontador -> tempo = n_tempo;
		return apontador;
	}
	else
	{
		if (apontador->tempo > n_tempo) {
	        ap_aux = (lista *) malloc(sizeof (lista));
	        ap_aux -> tipo = n_tipo;
	        ap_aux -> area = area;
					ap_aux -> delayed = delayed;
          ap_aux -> tempo = n_tempo;
          ap_aux -> proximo = (struct lista *) apontador;
            return ap_aux;
	    }

		ap_next = (lista *)apontador -> proximo;
		while(apontador != NULL)
		{
			if((ap_next == NULL) || ((ap_next -> tempo) > n_tempo))
				break;
			apontador = (lista *)apontador -> proximo;
			ap_next = (lista *)apontador -> proximo;
		}
		ap_aux = (lista *)apontador -> proximo;
		apontador -> proximo = (struct lista *) malloc(sizeof (lista));
		apontador = (lista *)apontador -> proximo;
		if(ap_aux != NULL)
			apontador -> proximo = (struct lista *)ap_aux;
		else
			apontador -> proximo = NULL;
			apontador -> tipo = n_tipo;
			apontador -> area = area;
			apontador -> delayed = delayed;
			apontador -> tempo = n_tempo;
		return lap;
	}
}
// Função que imprime no ecra todos os elementos da lista
void imprimir (lista * apontador) {
	if(apontador == NULL)
		printf("Lista vazia!\n");
	else
	{
		while(apontador != NULL)
		{
			printf("Tipo=%d\tArea=%d\tTempo=%lf\n", apontador -> tipo, apontador -> area, apontador -> tempo);
			apontador = (lista *)apontador -> proximo;
		}
	}
}




void initArray(Array *a, size_t initialSize) {
  a->array = (double *)malloc(initialSize * sizeof(double));
	if (a->array == NULL) {
        printf("ERROR: Memory allocation failure!\n");
        exit(1);
    }
  a->used = 0;
  a->size = initialSize;
}

void insertArray(Array *a, int element) {
  // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
  // Therefore a->used can go up to a->size
  if (a->used == a->size) {
    a->size *= 2;
    a->array = (double *)realloc(a->array, a->size * sizeof(double));
  }
  a->array[a->used++] = element;
}

void freeArray(Array *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}

/*
In the Call Center of a telecom operator calls are received at an arrival rate of 80
calls/hour (Poisson process), during the peak hour.
These calls have the following distribution:

 -30% of the calls refer to requests that can be processed by a general-purpose
call center operator. These calls have a minimum duration of 1 min plus a
duration following an exponential distribution with average equal to 2 min.
The maximum duration is 5 min.

 -70% of the calls require the contact with an area-specific call center
operator (e.g., technical issues, billing). These calls are transferred
accordingly. Until they are transferred to an area-specific call center operator,
the duration of the calls follows a Gaussian distribution with an average of 1
min and standard deviation of 20 seconds. The minimum and maximum
duration are respectively 30 and 120 seconds.
*/
int main(int argc, char const *argv[]) {

  //inputs em Portugês, o resto em Inglês
	int generalChannels=0;
	int specificChannels=0;
	int amostras=0;
  int L=0;
  printf("  _|_|_|            _|  _|        _|_|_|                        _|                         \n");
  printf("_|          _|_|_|  _|  _|      _|          _|_|    _|_|_|    _|_|_|_|    _|_|    _|  _|_| \n");
  printf("_|        _|    _|  _|  _|      _|        _|_|_|_|  _|    _|    _|      _|_|_|_|  _|_|     \n");
  printf("_|        _|    _|  _|  _|      _|        _|        _|    _|    _|      _|        _|       \n");
  printf("  _|_|_|    _|_|_|  _|  _|        _|_|_|    _|_|_|  _|    _|      _|_|    _|_|_|  _|       \n");

	printf("\n\nSamples: ");
	scanf("%d", &amostras);
	printf("Number of general channels: ");
	scanf("%d", &generalChannels);
	printf("Number of specific channels:");
	scanf("%d", &specificChannels);
  printf("Queue length (General purpose): ");
  scanf("%d", &L);

  printf("Loading... (More samples = More waiting time)\n");

  lista  *general = NULL;
  lista  *general_queue = NULL;
	lista  *specific = NULL;
	lista  *specific_queue = NULL;

	double Vmin = (0.2)*(1/lambda); //DELTA para o gráfico
	double Vmax = 5*(1/lambda); // Valor maximo reresentavel

  int originalLen=L;//apenas para motivos de impressão
	int max=0;
  int size = (Vmax/Vmin);
  int sampleCounter=0;
	int blocked=0;

	int total_delayed=0;
	int area = 0;
	int general_busy=0, specific_busy=0;
	int total_specific = 0, specific_delayed=0, error_count=0;

	double d=0, total_delay=0, total_specific_delay=0;
	double delay=0, specific_delay=0, avg_specific_total_delay=0;
	double avg=0, abs_error=0, rel_error=0, total_rel_error=0, total_abs_error=0;

	Array delays;
	initArray(&delays, 10); //github Array utils

  int *general_histogram = (int *)malloc(size*sizeof(int)); //tentar implemntar size dinâmico
	int *prediction_histogram = (int *)malloc(size*sizeof(int));
	srand((unsigned)time(NULL));


  while(sampleCounter < amostras){

    if(sampleCounter == 0) //Primeiro caso
      general = adicionar(general, ARRIVAL, SPECIFIC, 0, 0);


		if(general->tipo == ARRIVAL){		//Gerar o evento

			area = getArea();
			sampleCounter++;
			d = newEvent(ARRIVAL);
			general = adicionar(general, ARRIVAL, area, 0, (general->tempo + d));

			//Verificar de da para atender a chamada que acabou de chegar
			if(general_busy < generalChannels) { //Está livre
				general_busy++;
				d = generalDuration(general->area);
				general = adicionar(general, DEPARTURE, general->area, 0, general->tempo + d);
			}
			else if(L > 0){
					total_delayed++;
					L--;
					general_queue=adicionar(general_queue, ARRIVAL, general->area, 0, general->tempo);
					printf("Sample ID: %d ---> Estimated time left: %lf\n", sampleCounter, avg);
				}
			else
				blocked++;
		}
		else { 	//Eveto de DEPARTURE (fim do antendimento da chamada em si)
			general_busy--;

			if(general_queue != NULL) {
				general_busy++;
				delay = general->tempo - general_queue->tempo;
				general_queue->delayed+=delay;
				total_delay += delay;

				abs_error = fabs(delay-avg);
				total_abs_error += abs_error;
				total_rel_error += rel_error;
				error_count++;

				prediction_histogram = getHistogram(abs_error, size, prediction_histogram, Vmin);
				//max = getMax(prediction_histogram, size);
				general_histogram = getHistogram(delay, size, general_histogram, Vmin);

				avg = movingAverage(error_count, delay, avg); //Média em tempo real

				d = generalDuration(general_queue->area);
				general = adicionar(general, DEPARTURE, general_queue->area, general_queue->delayed, general->tempo + d);
				general_queue = remover(general_queue);
				L++;
			}

			if(general->area == SPECIFIC) {

				total_specific++;
				specific = adicionar(specific, ARRIVAL, SPECIFIC, general->delayed, general->tempo);
				while(specific->tempo < general->tempo && specificChannels > 0 && specific != NULL) {

					if(specific->tipo == DEPARTURE){
						specific_busy--;
						//Os que estavam na queue
						if(specific_queue != NULL){
							specific_busy++;
							specific_delay = (specific->tempo - specific_queue->tempo);
							specific_queue->delayed += specific_delay;
							total_specific_delay += specific_delay;
							d = specificDuration();

							avg_specific_total_delay+= specific_queue->delayed;
							insertArray(&delays, specific_queue->delayed);
							specific = adicionar(specific, DEPARTURE, SPECIFIC, specific_queue->delayed, (specific->tempo + d));
							specific_queue = remover(specific_queue);
						}
					}
					else {
						if(specific_busy < specificChannels) { //Está libre (agora para as especificas)

							specific_busy++;
							d = specificDuration();

							avg_specific_total_delay+=specific->delayed;
							insertArray(&delays, specific->delayed);
							specific = adicionar(specific, DEPARTURE, SPECIFIC, specific->delayed, specific->tempo + d);

						}
						else {
							specific_queue = adicionar(specific_queue, ARRIVAL, SPECIFIC, specific->delayed, specific->tempo);
							specific_delayed++;
						}
					}
					specific = remover(specific);
				}
			}
		}

		general = remover(general);
		}

  if(getMax(general_histogram, size) <= 125 || getMax(prediction_histogram, size) <= 125){ //Uma alternativa para o excel no caso de o histograma ficar monstruoso xD achei esta proporção ao fim de alguns testes
		printf("\nGeneral Histogram: \n");
	  printGraph(general_histogram, getMax(general_histogram, size));
		printf("\nPrediction Histogram: \n");
		printGraph(prediction_histogram, getMax(prediction_histogram, size));
  }

    histogramToFile(general_histogram);// ---> Histograma.txt
    printf("Histogram.txt was created! \n");
		histogramToFile2(prediction_histogram);// ---> predHistograma.txt
		printf("Prediction_Histogram was created! \n");

  printf("\nLambda :\t\t %.4lf Calls per second", lambda);
  printf("\nDelta :\t\t\t %.3lf", Vmin);
  printf("\nNumber of samples :\t %d", amostras);
  printf("\nGeneral operators:\t %d", generalChannels);
	printf("\nSpecific operators:\t %d", specificChannels);
  printf("\nL :\t\t\t %d", originalLen);

  printf("\n--------------------------------------------------------------\n");
	double avgDelay = (double)total_delay/total_delayed;
	double avgAbs =  (double)total_abs_error/error_count;
	double avgQueueTime_spec = getAvg(delays.array, delays.used);

	double desvioPadrao = STDeviation(delays.array , delays.used, avgQueueTime_spec);
	double erroPadrao_avg = desvioPadrao/(sqrt(delays.used));


	double Z90 = 1.645; // Valor de Z para um int de confiança de 90%
	double confInterval = Z90*erroPadrao_avg;
	rel_error = fabs(abs_error/avgDelay);


	printf("\nAvg Packets Delayed:\t\t\t\t %.4lf%%\n", (double)total_delayed/amostras*100);
	printf("Avg Delayed Time:\t\t\t\t %.4lfs\n", avgDelay);
	printf("Avg Specific purpose packets Delayed:\t\t %.4lf%%\n", (double)specific_delayed/total_specific*100);
	printf("Avg Specific purpose delayed Time:\t\t %.4lfs\n", (double)total_specific_delay/specific_delayed);
	printf("Avg Time of specific purpose packets in queues:  %.4lfs\n", avgQueueTime_spec);
	printf("Avg Blocked packets:\t\t\t\t %.4lf%%\n", (double)blocked/amostras*100);
  printf("\n--------------------------------------------------------------\n");
	printf("\nAvg Absolute prediction Error:\t %.3lf\n", avgAbs);
	printf("Avg Relative prediction Error:\t %.4lf%%\n", rel_error *100);
	printf("\nConfidence interval of 90%% --> %.4lf\n", confInterval);

	return 0;
}
