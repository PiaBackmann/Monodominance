/*mainwindow.cpp     November 2014 - April 2015 by  Pia Backmann     for Martin */

/** Hier sind alle Fensterspezifischen Funktionen **/

#include "mainwindow.h"
#include "iostream"     //if console output is needed
#include<time.h>        /* time_t, struct tm, difftime, time, mktime */
#include<string.h>
#include<math.h>
#include<stdio.h>
#include<stdlib.h>

using namespace std;

int main(int argc, char *argv[])
{
    // setze default params:
    //ijmax = 512;  // Weltgröße
    //number_pfts = 8;  // hier später dynamisch ändern lassen...
    //mortality_rate = 0.015; // Sterblichkeit der Bäume
    //neighbourhood = 0; // 0 = "in-radius" 1 = "Manhattan"
    int num_cluster, ijmax = 512, number_pfts = 8, seed_mass = 9450, i = 1,  seed = 1, radius = 20, density_radius = 1, random_init = 0, 
    init_option = 0;
    double prop_factor = 1;
    int* one_dim_grid;
    int* local_grid;
    int* next; int* cluster; 
    int pos = 0;
    World_t *welt;
    double prop_radius[31];
    char *cluster_file = (char*)malloc(1000*sizeof(char));
    char *histo_file= (char*)malloc(1000*sizeof(char));
    FILE *out; FILE *histogramm;
    char *pfad_allgemein, *pfad_ende;
    pfad_allgemein = (char*)malloc(1000*sizeof(char));
    pfad_ende = (char*)malloc(1000*sizeof(char));
    prop_radius[1] =  4.0/1256.0;
    prop_radius[2] = (12.0/1256.0);
    prop_radius[3] = (28.0/1256.0);
    prop_radius[4] = (48.0/1256.0);
    prop_radius[5] = (80.0/1256.0);
    prop_radius[6] = (112.0/1256.0);
    prop_radius[7] = (148.0/1256.0);
    prop_radius[8] = (198.0/1256.0);
    prop_radius[9] = (252.0/1256.0);
    prop_radius[10] = (316.0/1256.0);
    prop_radius[11] = (376.0/1256.0);
    prop_radius[12] = (440.0/1256.0);
    prop_radius[13] = (528.0/1256.0);
    prop_radius[14] = (612.0/1256.0);
    prop_radius[15] = (708.0/1256.0);
    prop_radius[16] = (796.0/1256.0);
    prop_radius[17] = (900.0/1256.0);
    prop_radius[18] = (1008.0/1256.0);
    prop_radius[19] = 1128.0/1256.0;
    prop_radius[20] = 1;
    prop_radius[30] = 2821.0/1256.0;

    double mortality_rate = 0.015; // mortality of trees
     while( i<argc)
     {
              if(strcmp(argv[i],"--size") == 0)
              {
                  ijmax = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--num_species") == 0)
              {
                  number_pfts = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--mort_rate") == 0)
              {
                  mortality_rate = atof(argv[++i]);
              }
              else if(strcmp(argv[i],"--neighbourhood") == 0)
              {
                 // neighbourhood = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--seed_mass") == 0)
              {
                 seed_mass = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--seed") == 0)
              {
                 seed = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--radius") == 0)
              {
                radius = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--densrad") == 0)
              {
                density_radius = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--random_init") == 0)
              {
                random_init = atoi(argv[++i]);
              }
              else if(strcmp(argv[i],"--init_option") == 0)
              {
                init_option = atoi(argv[++i]);
              }
              else
              {
                  i++;
              }
      } 
    prop_factor = prop_radius[radius];
    /************************************* FILE Routines:  ***********************************************************************/

    char* myfileroot="";
    sprintf(pfad_allgemein, "%sData/" , myfileroot); // kein slash am anfang -> ab working directory
  //  sprintf(pfad_ende, "_size_%d_seedmass_%d_radius_%d_mort_%f_init_option_%d_seed_%d", ijmax, seed_mass, radius, mortality_rate, init_option, seed); 
// without seed
    sprintf(pfad_ende, "_size_%d_seedmass_%d_radius_%d_mort_%f_init_option_%d", ijmax, seed_mass, radius, mortality_rate, init_option); 
    sprintf(cluster_file, "%sStats/Cluster_stats%s.out",pfad_allgemein, pfad_ende);
    out = fopen(cluster_file, "w"); 
    sprintf(histo_file, "%sClusterhistogramme/ClusterHistogramm%s.out", pfad_allgemein, pfad_ende);
    histogramm = fopen(histo_file, "a"); 
 
     /****************************************************************************************************************************/
     /*  Eingabeparameter  */
     next = (int*) malloc(4*ijmax*ijmax*sizeof(int));	
     srand48(seed); //random seed setzen

    // double test = drand48();
     //printf( "seed = %d \t zufallszahl 1 = %f \n", seed, test);
     /***************      INTITIALISATION      *****************/
     welt = new_World(ijmax, number_pfts, mortality_rate, seed_mass, seed, pfad_allgemein, radius, prop_factor, density_radius, init_option, pfad_ende);

     distribute_seeds(welt); // verteile bäume nach lotterie 

     one_dim_grid = (int*) malloc(welt->size*welt->size*sizeof(int));
     local_grid = (int*) malloc(welt->size*welt->size*sizeof(int));
     cluster = (int*) malloc(welt->size*welt->size*sizeof(int));
    /******************************************************************************************************************************/
    /**********  Routines  **********/
    fill_neighbour_grid(next, welt->size);
    while(welt->tick <= 30000)
    {


        // if( welt->tick == 1 || welt->tick == 5000 || welt->tick == 10000)
        // {
	   //  two_to_one_dim(welt, one_dim_grid);
	  //   num_cluster = percol_cluster(4,welt->size*welt->size, next, one_dim_grid, cluster, welt, out, histogramm);
            //local_grid = make_local_grid(welt);
	 //    two_to_one_dim(welt, one_dim_grid);
          //   print_one_dim_grid_file(1, welt, cluster);
       //  } 
   
        if (welt->tick == welt->clusteroutput[pos])
        {
	         // clusteralgorithmus
	         two_to_one_dim(welt, one_dim_grid);
	         num_cluster = percol_cluster(4,welt->size*welt->size, next, one_dim_grid, cluster, welt, out, histogramm);
          	 pos++;
        }         
        if(welt->tick % 500 == 0 || welt->tick == 1)
        {
	  // cout << "Tick = " << welt->tick << endl;
            print_world(welt);
          /*  local_grid = make_local_grid(welt);
	    two_to_one_dim(welt, one_dim_grid);
            print_one_dim_grid_file(2, welt, cluster);
	    print_recruitment_wahrscheinlichkeit(welt);
            print_recruitment_wahrscheinlichkeit(welt);
	    two_to_one_dim(welt, one_dim_grid);   
            print_one_dim_grid_file(1, welt, cluster);   */
        }  
	 welt = go(welt);
    } 
    /**************** FREE MEMORY **********************/
    fclose(out);
    fclose(histogramm);
    kill_World(welt);
    free(cluster_file);
    free(histo_file);
    free(pfad_allgemein);
    free(pfad_ende);
    free(one_dim_grid);
    free(local_grid);
    free(next);
    free(cluster); 
    return(0);
}



