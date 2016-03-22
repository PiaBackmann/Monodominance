/*mono_basics.cpp     April 2015    Pia Backmann     for Martin */

/** Most basic calculation routines and print routines of Monodominance Modell **/


#include "mainwindow.h"
#include "iostream" 
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>


using namespace std;

double rand_double()
/*******************************************************/
/** Produces a random number between 0 and 1 (double) **/
/**                                                   **/
/*******************************************************/
{
    double randMax = static_cast<double>(RAND_MAX);
    double r2 = ( static_cast<double>(rand()) / randMax );
    return r2;
}

int rand_int(int from, int to)
/********************************************************/
/**  Produces integer number between "from" and "to"   **/
/**                                                    **/
/********************************************************/
{
    int help = to-from;
    int i2 = rand() % help;
    i2 += from;
    return i2;
}


double calculate_distance(int i_center, int j_center, int i_pos, int j_pos)
/**  ****************************************************************** **/
/**  returns euclidian distance of two points (normally tree as center) **/
/**  PARAMETERS: positions of center and second point                   **/
/** RETURNS: (double)  distance between the two  coordinates            **/
/**  unit of resulting distance is patches                              **/
/** ******************************************************************* **/
{
    double distance;
    distance = sqrt((i_center - i_pos)*(i_center - i_pos)+(j_center - j_pos)*(j_center - j_pos));
    return(distance);
}


/************  LISTENROUTINEN  **************/

liste_t* create_liste(int info)
{
    liste_t *liste;
    liste = (liste_t*) malloc (sizeof(liste_t));
    liste->info = info;
    liste->next = NULL;

    return(liste);
}

liste_t* insert_element_liste(liste_t *list, liste_t *elem)
/*****************************************************************/
/**  inserts element always at the beginning of list	        **/
/**  PARAMS: list = pointer to beginning of list, elem: element **/
/**          to be inserted 				        **/
/**  RETURNS: pointer to beginning of list			**/
/*****************************************************************/
{
    elem->next = list;
    list = elem;

    return(list);
}


void delete_histo(liste_t *list)
{
    liste_t *current = list;
    while(current !=NULL)
    {
        list = current;
        current = current->next;
        free(list);
    }
}

void print_list_in_FILE(liste_t *list, FILE* out)
{
    while(list !=NULL)
    {
        fprintf(out, "%d ", list->info);
        list = list->next;
    }
    fprintf(out, "\n");
}

/********************************************/

elem_t* create_element(int value1, int value2)
{
    elem_t *elem;
    elem = (elem_t*) malloc (sizeof(elem_t));
    elem->coord_i = value1;
    elem->coord_j = value2;
    elem->next = NULL;

    return(elem);
}

elem_t* insert_element(elem_t *list, elem_t *elem)
/*****************************************************************/
/**  inserts element always at the beginning of list	        **/
/**  PARAMS: list = pointer to beginning of list, elem: element **/
/**          to be inserted 				        **/
/**  RETURNS: pointer to beginning of list			**/
/*****************************************************************/
{
    elem->next = list;
    list = elem;

    return(list);
}


void delete_liste(elem_t *list)
{
    elem_t *current =list;
    while(current !=NULL)
    {
        list = current;
        current = current->next;
        free(list);
    }
}

void print_list(elem_t *list)
{
    while(list !=NULL)
    {
        cout << list->coord_i << " " << list->coord_j << " ";
        list = list->next;
    }
    cout << endl;
}


int* new_histogram(int groesse_max, int* histogram)
{
	int i;
	histogram = (int*)malloc((groesse_max+1)*sizeof(int));
	for(i = 0; i <= groesse_max; i++)
	{
	      histogram[i] = 0;
	}
	return(histogram);
}



double patch_recruitement_probability(World_t *welt, int i, int j)
/****************************************************************/
/**  Calculates for each patch witch coords i, j in the grid   **/
/**  the probability that the monodominants will               **/
/**  recruit this patch in the next round (if it was a gap)    **/
/****************************************************************/
{
    int o,p;
    Tree_t *tree;
    int seedarray[8];
    double gesamtenergie = 0.0;
    double wahrscheinlichkeit = 0.0;
    double mono = 0.0;
    for(o = 0; o < 8; o++)
    {
        seedarray[o] = 0;
    }
    // array over all center-patches (distance: from center to max_dispersalrange)
    for(int k = i-welt->maxdispersal; k <= i+welt->maxdispersal; k++)
    {
        for(int l = j-welt->maxdispersal; l <= j+welt->maxdispersal; l++)
        {
           o = k;
           p = l;
            // 1. : Flip the coordinates directly at the edge (periodic boundaries)

            // check periodic boundaries
            if(o < 0){o = welt->size + o;}
            if(o >= welt->size){o = o - welt->size;}
            if(p < 0){p = welt->size + p;}
            if(p >= welt->size){p = p - welt->size;}

           if(welt->grid[o][p]->current_tree != NULL && (i != o || j != p)) // look at each tree in the max-dispersal radius (except of focal tree)
           {
              tree = welt->grid[o][p]->current_tree;
              if (calculate_distance(i, j, k, l) <= tree->dispersalrange)
              {
                    seedarray[tree->species] = seedarray[tree->species] + tree->seednumberperpatch;    	
	/*	    if(j == 3 && i == 0 && welt->tick == 1)
	            {
			printf("Test: species = %d, seedarray(species) = %d \n", tree->species, seedarray[tree->species]);
		    }*/
              }
           }
        }
    }

    for(int k = 0; k <= 7; k++) // iterate through the newly calculated seedbank
    {
        gesamtenergie = gesamtenergie + seedarray[k];
    }
    mono = seedarray[0];
    wahrscheinlichkeit = mono/gesamtenergie;
    return(wahrscheinlichkeit);
}


/**********************************************   IO-Routinen   *******************************************************/



void print_world(World_t *welt)
/***************************************************/
/**  Produce a picture of current world view      **/
/**  and store it in "Folder"                     **/
/**                                               **/
/***************************************************/
{
    char name_outfile[200];
    FILE *out;

    sprintf(name_outfile, "%sWeltview/Monodominanz%s_seed_%d_tick_%d.out", welt->pfad, welt->ende, welt->seed, welt->tick);
    out = fopen(name_outfile, "w");
    for(int i = 0; i < welt->size; i++)
    {
        for(int j = 0; j < welt->size; j++)
        {
            fprintf(out,"%d ", welt->grid[i][j]->color) ;
        }
         fprintf(out,"\n") ;
    }
    fclose(out);
}



void print_recruitment_wahrscheinlichkeit(World_t *welt)
/**********************************************************/
/**  Produce a picture of current probability of all     **/
/**  grid cells to be colonized by the monodominant      **/
/**  species (recruitement probability in seed bank)     **/
/**********************************************************/
{
    char name_outfile[200];
    FILE *out;
    int i, j;
    // ALLOCATE MEMORY
    double** recruitment_grid;//[welt->size][welt->size];
    recruitment_grid =(double**) malloc(welt->size*sizeof(double*));
    for (i = 0; i < welt->size; i++)
    {
        recruitment_grid[i] = (double*) malloc(welt->size*sizeof(double));
    }
    for(i = 0; i < welt->size; i++)
    {
        for(j = 0; j < welt->size; j++)
        {
            recruitment_grid[i][j] = patch_recruitement_probability(welt, i, j);
        }
    }
    // Print the probability distribution in File:
    sprintf(name_outfile, "%sLocalmap/Recruitement_prob%s_tick_%d.out", welt->pfad, welt->ende, welt->tick);
    out = fopen(name_outfile, "w");
    for(int i = 0; i < welt->size; i++)
    {
        for(int j = 0; j < welt->size; j++)
        {
            fprintf(out,"%1.4f ", recruitment_grid[i][j]);
        }
        fprintf(out,"\n") ;
    }
    // FREE MEMORY
    for (i = 0; i < welt->size; i++)
    {
        free(recruitment_grid[i]);
    }
    free(recruitment_grid);
    fclose(out);
}


void print_histogram_in_File(int* histogram, int groesse_max, FILE *out)
/** *************** print_histogram() **********************/
/** Prints histograms of all clusters into console        **/
/**                                                       **/
/**  PARAMETERS: histogram: *int: shows number of 	  **/
/**                            size 1 - groesse_max       **/

/**   Array                                               **/
/**   (int) number Cluster 1   2   3   4   5   6          **/
/**   (int) Größe          10  3   4   5   21  14         **/
/**
                                                          **/
/**  RETURNS:  nothing                                    **/
/** ***************************************************** **/
{
	int count = 0;
	for(int i = 0; i < groesse_max; i++)
	{
		if(histogram[i] > 0)
		{
			fprintf(out, "%d ", i);
			fprintf(out, "%d\t", histogram[i]);
			count++;
		}
	}
	if(count == 0)
	{
		fprintf(out, "%d", 0);
	}
	fprintf(out,"\n");
}

void print_patch_grid(Patch_t ***grid, int maxsize)
{
    for (int i = 0; i < maxsize; i++)
    {
        for(int j = 0; j < maxsize; j++)
        {
            cout << grid[i][j]->color << " ";
        }
        cout << endl;
    }
}

