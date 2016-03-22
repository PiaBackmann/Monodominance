/*mono_cluster.cpp     Dezember 2014 - April 2015 by Pia Backmann     for Martin */


#include "mainwindow.h"
#include "iostream" 
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include <iomanip>

using namespace std;


/***********************************************************************************************************/
/*
Cluster Algorithm:

1) Function to scan the whole Grid (world) and detect clusters of the monodominant species
   so that every cluster gets a different number
   it returns: Number of monodominant Clusters (int)

It writes the Cluster statistics into an output file:

----- tick | number of clusters | absolute size of biggest cluster | relative size of biggest cluster | relative abundance of monodom. spec. | relative sizes of biggest, not-percolating cluster | world mortality rate -------


*************************************************************************************************************/

void two_to_one_dim(World_t *welt, int* one_dim_grid)
/********************************************************/
/** Converts two-dim world grid into one-dim array     **/
/** with entries:  -1 = no monodom species on patch    **/
/**                 1 = monodom species on patch       **/
/**                                                    **/
/********************************************************/
{
    int i, j, cluster;                              // cluster: -1 if no monodom., 1 if occupied

    for(i = 0; i < welt->size; i++)
    {
        for(j = 0; j < welt->size; j++)
        {
            cluster = -1;

           if(welt->grid[i][j]->color == 1)        // if monodominant species on current grid cell
           {
               cluster = 1;
           }
           one_dim_grid[j+i*welt->size] = cluster; 
        }
    }
}


void print_one_dim_grid(int worldsize, int* one_dim_grid)
{
    for(int i = 0; i < worldsize*worldsize; i++)
    {
        if(i % worldsize == 0 && i != 0)
        {
            cout << endl;
        }
        cout << setw(6) << one_dim_grid[i];
    }
    cout << endl;
}


void print_one_dim_grid_file(int which, World_t *welt, int* one_dim_grid)
{
    char name_outfile[1000];
    FILE *out;
    if(which == 1)
    {
        sprintf(name_outfile, "%sCluster/ClusterView%s_seed_%d_tick_%d.out",welt->pfad, welt->ende, welt->seed, welt->tick);
    }
    else if(which == 2)
    {
        sprintf(name_outfile, "%sLocalmap/Localmap%s_density%d_seed_%d_tick_%d.out",welt->pfad, welt->ende, welt->density_radius, welt->seed, welt->tick);
    }
    out = fopen(name_outfile, "w");

    for(int i = 0; i < welt->size*welt->size; i++)
    {
        if(i % welt->size == 0 && i != 0)
        {
            fprintf(out,"\n");
        }
        fprintf(out,"%7d ", one_dim_grid[i]);
    }
    fprintf(out,"\n");
    fclose(out);
}

void fill_neighbour_grid(int* next, int worldsize)
{
    int j = 0;
    double quot;
    for(int i = 0; i < 4*worldsize*worldsize; i=i+4)    // 4 next neighbours
    {
        quot = i/4;
        j = (int) floor(quot);
         next[i] =   j + 1;                            // (+x)
         next[i+1] = j - 1;                            // (-x)
         if(j % (worldsize-1) == 0 && j != 0)          // on the right side of the grid 
         {
            next[i] = j - worldsize + 1;               // (+x)
         }
         if((j % worldsize) == 0 || j == 0)            // on the left side of the grid
         {
             next[i+1] = j + worldsize - 1;            // (-x)
         }

         next[i+2] = j + worldsize;                    // (+y)
         next[i+3] = j - worldsize;                    // (-y)
        if(j >= worldsize*(worldsize-1))               // on the bottom of the grid
        {
             next[i+2] =  j - worldsize*(worldsize-1); // (+y)
        }
        if(j < worldsize)                              // on top of the grid
        {
            next[i+3] = j + worldsize*(worldsize-1);   // (-y)
        }
    }
}


/******************* percol_cluster() *******************/
/** Calculates the connected clusters of the lattice   **/
/** ’site’. Occupied sites have ’site[i]=1’ (i=1..N).  **/
/** Neigbours of occupied sites form clusters          **/
/** For each site, in ’cluster[i]’ the id of the       **/
/** cluster (starting at 0) is stored                  **/
/** PARAMETERS: (*)= return-paramter                   **/
/**     num_n: number of neighbours                    **/
/**     N: number of sites                             **/
/**     next: gives neighbours (0..N)x(0..num_n-1)     **/
/**     0 not used here. Use NEXT() to access          **/
/**     site: tells whether site is occupied           **/
/** (*) cluster: id of clusters sites are contained in **/
/** RETURNS: number of clusters                        **/
/********************************************************/
int percol_cluster(int num_n, int N, int *next, int *site, int *cluster, World_t *welt, FILE *out, FILE *histogramm)
{
    int num_clusters = 0;
    int t, perko, r,  current_size, biggest_size, biggest_nonperko, size_all;
    /* loop counters over sites, directions */
    int current, neighbour, bool_perko;
    int* perko_x_direction;
    int* perko_y_direction;
    int *histo; 
    FILE *ini;
    char *ini_file = (char*)malloc(1000*sizeof(char));
    double gyration_nonperko;
    histo = new_histogram(N, histo);
    perko_x_direction = (int*)malloc(N*sizeof(int));
    perko_y_direction = (int*)malloc(N*sizeof(int));
    /* sites */
    lstack_t *members;                              // stack of members for cluster

    for(t = 0; t < N; t++)                          // initialise all clusters empty
    {
        cluster[t] = -1;
    }
    for(int o = 0; o< welt->size; o++)
    {
        perko_x_direction[o] = -1;
    }
    for(int o = 0; o< welt->size; o++)
    {
        perko_y_direction[o] = -1;
    }
    members = lstack_new(N);
    biggest_size = 0;
    biggest_nonperko = 0;
    size_all = 0;
    for(t=0; t<N; t++)                             // loop over all sites
    {
        perko = t;
        current_size = 0;
        if((site[t] == 1)&&(cluster[t] == -1))     // new cluster ?
        {
            lstack_push(members, t);               // start cluster
            cluster[t] = num_clusters;           

                                                   // transform 1 D grid to 2 D for percolation grid
            perko = t % welt->size;                // x-direction
            perko_x_direction[perko] = 1;

            perko = floor(t/welt->size);           // y-direction
            perko_y_direction[perko] = 1;
	    
            while(lstack_is_empty(members) == 0)   // extend cluster if stack still not empty
            {
                current = lstack_pop(members);
		
                for(r=0; r<num_n; r++)                          // loop over neighbours
                {
                    neighbour = NEXT(current, r);
                    if((site[neighbour]==1)&&(cluster[neighbour]==-1))
                    {                                          // neighbour belongs to same cluster
                        lstack_push(members, neighbour);
                         cluster[neighbour] = num_clusters;
                                                               // Test if Cluster is percolating:
                        perko = neighbour % welt->size;        // x-direction
                        perko_x_direction[perko] = 1;
                        perko = floor(neighbour/welt->size);   // y-direction
                        perko_y_direction[perko] = 1;
                    }
                }
                current_size++;
                size_all++;
            }
             if(welt->tick % 10000 == 0 )
             {
                // Save the Cluster's sizes in a histogram 
                histo[current_size]++;
             }
                // check, whether cluster is percolating:  in x-direction
             for(int o = 0; o< welt->size; o++)
             {
                bool_perko = 1;                       // 0 = false, 1 = true
                if(perko_x_direction[o] == -1)
                {
                    bool_perko = 0;
                    break;                            // if a x-coordinate is not occupied by a monodominant cell: no percolation
                }
             }
             if(bool_perko == 0)                     // only necessary, if no percolation in x-direction (percolation criterium requires only one direction)
             {
                                                     // check, whether cluster is percolating:  in y-direction
                for(int o = 0; o< welt->size; o++)
                {
                    bool_perko = 1;	            // 0 = false, 1 = true
                    if(perko_y_direction[o] == -1)
                    {
                        bool_perko = 0;
                        break;                      // if a y-coordinate is not occupied by a monodominant cell: no percolation
                    }
                }
             }
            // Clear all entries made in percolation matrix for next cluster to be tested 
            for(int o = 0; o< welt->size; o++)
            {
                 perko_x_direction[o] = -1;
            }
            for(int o = 0; o< welt->size; o++)
            {
                 perko_y_direction[o] = -1;
            }
            if(biggest_size < current_size)
            {
                 biggest_size = current_size;
            }
            if(biggest_nonperko < current_size)
            {
                if(bool_perko == 0)                // clguster not percolating? 
                {
                    biggest_nonperko = current_size;
                }
            }
            num_clusters++;
       }
    }
    lstack_delete(members);
  //  fprintf(out, " %d\t%d\t%d\t%f\t%f\t%f\t%f\n",welt->tick, num_clusters, biggest_size,(double) biggest_size/N, (double) size_all/N,(double) biggest_nonperko/N, welt->mortality_rate);  // clusterstats
    if(welt->tick == 10000)
    {
         print_histogram_in_File(histo,N, histogramm);    // print Histogramm of Clustersizes
         gyration_nonperko = radius_of_gyration(num_n, N, next, site, cluster, welt);

         printf("%d\t%d\t%d\t%f\t%d \t %d \t%d\t%f\t%f\t%f\t%f\n", welt->size, welt->seed_mass, welt->seed, welt->mortality_rate, welt->init_option, num_clusters, biggest_size,(double) biggest_size/N, (double) size_all/(double) N, (double) biggest_nonperko/(double)N, gyration_nonperko);
    }
    if(welt->tick == 1)
    {
	 char* myfileroot="";
	 sprintf(ini_file, "%sData/Init/Init_Stats%s" , myfileroot, welt->ende);
	 ini = fopen(ini_file,"a");
         fprintf(ini, "%d\t%d\t%d\t%f\t%d\t %d \t%d\t%f\t%f\n", welt->size, welt->seed_mass, welt->seed, welt->mortality_rate, welt->init_option, num_clusters, biggest_size, (double) biggest_size/ (double) N, (double) size_all/(double) N);
    	 fclose(ini);
    }
    free(histo);
    free(perko_x_direction);
    free(perko_y_direction);
    free(ini_file);
    return(num_clusters);
}







/*
	1) berechnet für jeden Cluster Radius of Gyration R und gibt ihn zurück (double)
	in dem Clusteralgorithmus (nicht in dieser Funktion:)
	2) und Größe des Clusters M -> printe in gyrationsfile in extra Funktion) pro Cluster: M, R
	-> printe nun: in ein extra file: R, M für alle Cluster (bei letztem Zeitschritt)
	werte mit R aus: 
	for schleife über alle seeds, erzeuge ln(M) über ln(R) plot, suche slope durch fit.
	bilde Mittelwert über alle slopes aller seeds (alles für Pc)
	3) und so die Fraktale dim df: M ~ R^df  (außerhalb des Programms mit R)
	-> df = Steigung des log/log Plots ln(M) über ln(R)
	4) bilde aus den Werten für alle Cluster den Mittelwert
	5) außerdem wichtig: radius of gyration von größtem,g nicht perkol. cluster (für eponent nu, korrelationslänge)
	-> deshalb: eine eigene formel, um gyrationsrad. zu berechnen
	6) noch eine Formel um mittl. Clustergröße zu berechnen, für exp gamma
	*/



/** ********************* radius_of_gyration() **********************/
/**								   **/
/**   returns radius of gyration of biggest (non-percolating)      **/
/**   cluster.   (double)                                          **/
/** *****************************************************************/
double radius_of_gyration(int num_n, int N, int *next, int *site, int* cluster, World_t *welt)
{
    FILE* out;
    char* myfileroot="";
    char* pfad_allgemein = (char*)malloc(1000*sizeof(char));
    char *gyro_file = (char*)malloc(1000*sizeof(char));
    int t,  r,  current_size, biggest_size, biggest_nonperko, i, j;
    int  neighbour, bool_perko;
    int* perko_x_direction;
    int* perko_y_direction;
    int* gitter;
    int i_flip_center, j_flip_center, i_flip, j_flip;
    double center_x, center_y, radius_gyration, radius_biggest_nonperko = 0.0;
    gyro_t current;
    double gyration_nonperko = 0.0;
    sprintf(pfad_allgemein, "%sData/" , myfileroot); // kein slash am anfang -> ab working directory
    sprintf(gyro_file, "%sGyration/Gyration%s.out", pfad_allgemein, welt->ende);
    out = fopen(gyro_file, "a");
    int num_clusters, o, p, current_position;
    perko_x_direction = (int*)malloc(N*sizeof(int));
    perko_y_direction = (int*)malloc(N*sizeof(int));
    /* sites */

    gyro_stack_t *members;                  // stack of members for cluster
    gitter = cluster;
    for(t = 0; t < N; t++)                  // initialise all clusters empty
    {
        gitter[t] = -2;
    }
    for(o = 0; o< welt->size; o++)
    {
        perko_x_direction[o] = -1;
    }
    for(o = 0; o< welt->size; o++)
    {
        perko_y_direction[o] = -1;
    }
    members = gyro_stack_new(N);
    biggest_size = 0;
    biggest_nonperko = 0;
    num_clusters = 0;

    for(t=0; t<N; t++)                      // loop over all sites
    {
        current_size = 0;
        center_x = 0.0;
        center_y = 0.0;
        radius_gyration = 0.0;


        // 1. Calculate the center of gravity of the cluster:

        if((site[t] == 1)&&(gitter[t] == -2))           // new cluster ?
        {
            i = floor(t/welt->size);
            j = t % welt->size;
            perko_x_direction[i] = 1;
            perko_y_direction[j] = 1;

            gyro_stack_push(members, i, j , 0, 0);     // start cluster
            gitter[t] = -1;
            current_size++;

            center_x = center_x + i;   		       // x-coordinate of center of gravity
            center_y = center_y + j;  		       // y-coordinate of center of gravity

            while(gyro_stack_is_empty(members) == 0)   // extend cluster if stack still not empty 
            {
                                                       // take first element of Stack
                current = gyro_stack_pop(members);
                i_flip_center = current.i_flip;
                j_flip_center = current.j_flip;
                i = current.i_coord;
                j = current.j_coord;

                //  Iterate through all neighbours of the current grid cell
                for(int k = i-1; k <= i+1; k++)
                {
                        for(int l = j-1; l <= j+1; l++)
                        {
                            o = k;
                            p = l;
                            i_flip = 0;
                            j_flip = 0;
                                // check periodic boundaries
                            if(o < 0)
                            {
                                o = welt->size + o;
                                i_flip = -1; // weil man quasi um "-welt->size" zurückflippen muss, um wieder an virtuell "normalen" Cluster heranzukommen
                            }
                            if(o >= welt->size)
                            {
                                o = o - welt->size;
                                i_flip = 1;
                            }
                            // diese o und i_flip werden so im Stack gespeichert.
                            // genau dasselbe für y-Koordinate:
                            if(p < 0)
                            {
                                p = welt->size + p;
                                j_flip = -1;
                            }
                            if(p >= welt->size)
                            {
                                p = p - welt->size;
                                j_flip = 1; 
                            }
                            if(calculate_distance(i, j, k, l) <= 1) // only direct neighbours are of importance
                            {
                                // create 1D-coordinate for the 1D-Grid
                                current_position = p+o*welt->size;
                                if(site[current_position] == 1 && gitter[current_position] < -1)  // is this place of the grid monodominant?
                                {
                                    gitter[current_position] = -1;
                                    gyro_stack_push(members, o, p, i_flip + i_flip_center, j_flip + j_flip_center);
                                    // put the coordinate-flips and re-flips together to one x- and one y-coordinate
                                    o = o + (i_flip + i_flip_center)* welt->size;
                                    p = p + (j_flip + j_flip_center)* welt->size;
                                    center_x = center_x + o;
                                    center_y = center_y + p;
                                    current_size++;
                                }
                            }
                        }
                    }
                }

		// Voilà: the x- and y-coordinates of the center of gravity: 
                center_x = center_x/(double)current_size;
                center_y = center_y/(double)current_size;
/*******************************************************************************************/
                current_size = 0;
                i = floor(t/welt->size);
                j = t % welt->size;
                gyro_stack_push(members, i, j , 0, 0);  // start cluster
                num_clusters++;

                gitter[t] = num_clusters;
                // Calculate radius of gyration by adding all coordinates (flipped...) 
                radius_gyration = radius_gyration + (i - center_x)*(i - center_x) + (j - center_y)*(j - center_y);
                while(gyro_stack_is_empty(members) == 0)    // extend cluster if stack not already empty
                {
                    // take first element from stack
                    current_size ++;
                    current = gyro_stack_pop(members);
                    i_flip_center = current.i_flip;
                    j_flip_center = current.j_flip;
                    i = current.i_coord;
                    j = current.j_coord;

                    // Iterate through all neighbours of the current grid-cell
                    for(int k = i-1; k <= i+1; k++)
                    {
                            for(int l = j-1; l <= j+1; l++)
                            {
                                o = k;
                                p = l;
                                i_flip = 0;
                                j_flip = 0;
                                                          // check periodic boundaries
                                if(o < 0)
                                {
                                    o = welt->size + o;
                                    i_flip = -1;          // weil man quasi um "-welt->size" zurückflippen muss, um wieder an virtuell "normalen" Cluster heranzukommen
                                }
                                if(o >= welt->size)
                                {
                                    o = o - welt->size;
                                    i_flip = 1;
                                }
                                // diese o und i_flip werden so im Stack gespeichert.
                                // genau dasselbe für y-Koordinate:
                                if(p < 0)
                                {
                                    p = welt->size + p;
                                    j_flip = -1;
                                }
                                if(p >= welt->size)
                                {
                                    p = p - welt->size;
                                    j_flip = 1;
                                }
                                if(calculate_distance(i, j, k, l) <= 1) // only direct neighbours are of interest
                                {
                                    // erzeuge 1D-Koordinate für 1D-Grids
                                    current_position = p+o*welt->size;
                                    if(site[current_position] == 1 && gitter[current_position] <= -1)  // ist Stelle im Grid Monodominant
                                    {
                                        gitter[current_position] = num_clusters;
                                        gyro_stack_push(members, o,p,i_flip_center + i_flip, j_flip_center + j_flip);
                                        // Does the Cluster percolate?
                                        perko_x_direction[o] = 1;
                                        perko_y_direction[p] = 1;
                                        o = o + (i_flip + i_flip_center)* welt->size;
                                        p = p + (j_flip + j_flip_center)* welt->size;

                                        radius_gyration = radius_gyration + (o-center_x)*(o-center_x) + (p-center_y)*(p-center_y);
                                    }
                                }
                            }
                        }
                    }
                    radius_gyration = sqrt(radius_gyration/(double)current_size);
                    if( current_size > 1)
                    {
                        fprintf(out,"%d\t%d\t%f\t%d\t%d\t%f\n",welt->size, welt->seed_mass, welt->mortality_rate, welt->seed, current_size ,radius_gyration );
                    }
                    // Does cluster percolate in x-direction?
                    for(int m = 0; m< welt->size; m++)
                    {
                        bool_perko = 1;                  // 0 = false, 1 = true
                        if(perko_x_direction[m] == -1)
                        {
                             bool_perko = 0;
                             break;                      // no percolation if one of x-coordinates not covered by monodominant species.
                        }
                    }
                    if(bool_perko == 0)                  // only necessary if no percolation in x-direction
                    {
                         // Does cluster percolate in y-direction?
                         for(int m = 0; m < welt->size; m++)
                         {
                               bool_perko = 1;	         // 0 = false, 1 = true
                               if(perko_y_direction[m] == -1)
                               {
                                     bool_perko = 0;
                                     break;              // no percolation if one of y-coordinates not covered by monodominant species.
                               }
                          }
                     }
                     // Delete all entries of percolation matrix for next cluster
                     for(int m = 0; m < welt->size; m++)
                     {
                           perko_x_direction[m] = -1;
                     }
                     for(int m = 0; m < welt->size; m++)
                     {
                           perko_y_direction[m] = -1;
                     }
                     if(biggest_nonperko < current_size)
                     {
                           if(bool_perko == 0)    // Does the Cluster percolate?
                           {
                                biggest_nonperko = current_size;
                                radius_biggest_nonperko = radius_gyration;
                           }
                     }
           }
    }
/*************  FREES  ************/
    free(pfad_allgemein);
    free(gyro_file);
    gyro_stack_delete(members);
    free(perko_x_direction);
    free(perko_y_direction);
    return(radius_biggest_nonperko);
}





/**************************    STACK ROUTINES    ********************g******************/


gyro_stack_t* gyro_stack_new(int N)
/************************************************************/
/** Erzeuge neuen Stapel -> Last in First out              **/
/**                                                        **/
/************************************************************/
{
      gyro_stack_t* stack;
      stack = (gyro_stack_t*) malloc(sizeof(gyro_stack_t));
      stack->info = (gyro_t*) malloc(N*sizeof(gyro_t));    // stack has a maximal size of N elements
      stack->current = -1;
      stack->max = N;
      return(stack);
}


void gyro_stack_push(gyro_stack_t *members, int i, int j, int flip_i, int flip_j)
/**************************************************/
/** Ein Element wird oben auf den Stack gelegt.  **/
/**                                         	 **/
/**************************************************/
{
     members->current++;
     if(members->current < members->max)
     {
             members->info[members->current].i_coord = i;
             members->info[members->current].j_coord = j;
             members->info[members->current].i_flip = flip_i;
             members->info[members->current].j_flip = flip_j;
     }
     else
     {
             cout << "Stack is full!" << endl;
     }
}


lstack_t* lstack_new(int N)
/************************************************************/
/**  create new stack  -> Last in First out                **/
/**                                                        **/
/************************************************************/
{
    lstack_t* stack;
    stack = (lstack_t*) malloc(sizeof(lstack_t));
    stack->info = (int*) malloc(N*sizeof(int));       // stack has a maximal size of N elements
    stack->max = N;
    stack->current = -1;
    return(stack);
}

void lstack_push(lstack_t *members, int t)
/**************************************************/
/**    Put an element on top of the stack        **/
/**						 **/
/**************************************************/
{
    members->current++;
    if(members->current < members->max)
    {
        members->info[members->current] = t;
    }
    else
    {
        cout << "Stack is full!" << endl;
    }
}

int lstack_is_empty(lstack_t *members)
{
    if (members->current > -1)
    {
        return(0);
    }
    return(1);
}

int lstack_pop(lstack_t *members)
/******************************************************/
/** Take first element from stack	             **/
/**						     **/
/******************************************************/
{
    members->current = members->current - 1;
    return(members->info[(members->current + 1)]);
}

void lstack_delete(lstack_t *members)
/***************************************************/
/**     Delete the entire stack                   **/
/**                                               **/
/***************************************************/
{
        free(members->info);
        free(members);  
}

int gyro_stack_is_empty(gyro_stack_t *members)
{
      if (members->current > -1)
      {
             return(0);
      }
      return(1);
}

gyro_t gyro_stack_pop(gyro_stack_t *members)
/******************************************************/
/** Nehme ein Element oben vom Stack	             **/
/**                                                  **/
/******************************************************/
{
      members->current = members->current - 1;
      return(members->info[(members->current + 1)]);
}

void gyro_stack_delete(gyro_stack_t *members)
/***************************************************/
/** Lösche kompletten Stack aus dem Speicher.     **/
/**                                               **/
/***************************************************/
{
       free(members->info);
       free(members);
}


