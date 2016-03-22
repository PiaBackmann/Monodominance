/*mono_routinen.cpp     November 2014 - April 2015   by Pia Backmann     for Martin */

/** more complex functions of Monodominance-Model **/

#include "mainwindow.h"
#include "iostream" //if console output is needed
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>


using namespace std;




void set_new_tree(World_t* welt, int i,int j, int species)
/** ***************************************************** **/
/**  Creats new tree and sets it into specific grid cell  **/
/**  PARAMETERS: world_t*                                 **/
/**  RETURNS: (nothing) changes everything                **/
/** 			via pointers                      **/
/**                                                       **/
/** ***************************************************** **/
{
    if(welt->grid[i][j]->current_tree != NULL)
    {
        welt->grid[i][j]->former_species = welt->grid[i][j]->current_tree->species;   // species which has been present in former time-step
        welt->grid[i][j]->former_shadetolerance = welt->grid[i][j]->current_tree->shadetolerance;
    }
    else
    {
        welt->grid[i][j]->former_species = 0;   // species which has been present in former time-step
        welt->grid[i][j]->former_shadetolerance = 0;
    }
    //welt->grid[i][j]->seed_array[species] = welt->grid[i][j]->seed_array[species] - 1;
    welt->grid[i][j]->color = species+1;        //  0 = no tree (gap)
    welt->grid[i][j]->current_tree = new_Tree(species, welt->real_propagule, welt->radius);
}



void distribute_seeds(World_t *welt)
/** **************************************** **/
/**  Distribute seeds randomly over world    **/
/**  PARAMETERS: world_t*                    **/
/**  RETURNS: (nothing) changes everything   **/
/** 			via pointers         **/
/**                                          **/
/** **************************************** **/
{
    int i,j,k, species; // species: number of "verloster" species (1 = monodom, >1: other species)
    double summe, current_number = 0, zufall = 0, previous_number = 0;
    species = 0;
    for(i = 0; i < welt->size; i++)
    {
        for(j = 0; j < welt->size; j++)
        {
            if(welt->grid[i][j]->current_tree == NULL)  //no tree here?
            {
            // hier muss "verlost werden" -> Intervall Taktik, Zufallszahl zwischen 0 und 1,
            // 1. species als zwischen summe seeds/summe_Einträge etc. auslosen
                zufall = drand48();  // draw random number between 0 and 1
                summe = 0.0;
                for (k = 0; k < welt->num_species; k++)
                {
                    summe = summe + welt->grid[i][j]->seed_array[k];
                }
                current_number = 0;
                previous_number = 0;
                for (int k = 0; k < welt->num_species; k++)
                {
                    previous_number = current_number;
                    current_number = current_number + (double) welt->grid[i][j]->seed_array[k] / summe;

                    if(zufall >= previous_number && zufall <= current_number )
                    {
                         species = k;
                         break;
                    }
                }
                set_new_tree(welt, i,j,species);
            }
        }
    }
}

elem_t* mortality(World_t *welt)
/*****************************************************************/
/** Iterate over all trees: draw random number "zufall"         **/		
/** if "zufall" < mortality: tree dies          	        **/
/** Returns pointer to first elem of List of all empty patches  **/
/*****************************************************************/	

{
    int i, j;
    double zufall = 0;
    elem_t *start, *elem;
    start = NULL; // zeigt auf Anfang der Liste aller leeren patches

    for(i = 0; i < welt->size; i++)
    {
        for(j = 0; j < welt->size; j++)
        {
            if(welt->grid[i][j]->current_tree != NULL) // is there a tree on grid-cell ? 
            {
                zufall = drand48();
                if(zufall*100 < welt->mortality_rate*100)
                {
                    kill_Tree(i, j, welt);
                    elem = create_element(i,j);
                    start = insert_element(start, elem);
                }
            }
            else
            {
                elem = create_element(i,j);
                start = insert_element(start, elem);
            }
        }
    }
    return(start);
}

void recruitment(World_t *welt)
/*******************************************************************/
/** 1)  setze das lokale seed array pro patch auf 0	  	  **/
/**     (für jeden patch des grids)				  **/
/** 2)  Fülle alle leeren Felder des Grids mit Samen		  **/
/**     (in local seed array)					  **/
/** 3)  würfele pro leerem patch eine neue Art aus, entsprechend  **/
/**     Samenanzahl in local seed array				  **/
/**								  **/
/*******************************************************************/
{
    int i,j,k, summe = 0, species, zufall = 0, id, rest_seedpool;
    elem_t* elem = welt->empty_patches;

    while(elem != NULL)
    {
        i = elem->coord_i;
        j = elem->coord_j;
        for(k = 0; k < welt->num_species; k++) 
        {
             welt->grid[i][j]->seed_array[k] = 0; // all grid-cells "0"
        }

       // Fill all empty grid-cells of world with seeds
	patch_get_seeds(welt, i, j);
	elem = elem->next;
    }
    // "chose" next species (per empty grid-cell) via lottery
    elem = welt->empty_patches;
    while(elem != NULL)
    {
        summe = 0;
        i = elem->coord_i;
        j = elem->coord_j;
        for(k = 0; k < welt->num_species; k++)  
        {
             summe = summe + welt->grid[i][j]->seed_array[k];
        }
        if(summe > 0)                       // are there any seeds on grid-cell?
        {
            zufall =  rand_int(0, summe);   // generiert eine Zahl zwischen 0 und summe (int)
            id = 0;
            rest_seedpool = 0;
            species = 0;
            for(id = 0; id < welt->num_species; id++)
            {
                 if(zufall < welt->grid[i][j]->seed_array[id] + rest_seedpool)
                 {
                    species = id;
                    break;
                 }
                 rest_seedpool = rest_seedpool + welt->grid[i][j]->seed_array[id];
             }
                   // generierate new species
             set_new_tree(welt, i,j, species);
         }

        elem = elem->next;
    }
    free(elem);
}

void disturbance(World_t *welt)
{

}


World_t* go(World_t *welt)
/****************************************************************/
/**  Main simulation routine:                                  **/
/**  1) calculate natural mortality of trees                   **/
/**  2) include disturbances                                   **/
/**  3) recruitment of trees                                   **/
/**             (distribute seeds on empty fields and chose    **/
/**             among the local seed array a new species       **/
/**  4)  update status set changes into grid                   **/
/**  5)  if tick reached: print out .png of world and          **/
/**                       run cluster algorithm                **/
/****************************************************************/
{
    disturbance(welt); 
    recruitment(welt);
    delete_liste(welt->empty_patches);	
    welt->empty_patches = mortality(welt);
    update_status(welt); // hier werden soweit erstmal alle juveniles auf "alive" gesetzt
    // und alle mit status "dead" werden getötet
    welt->tick ++;

    return(welt);
}

void tree_spread_seeds(World_t *welt, int i, int j, Tree_t* tree) // distance = maxdispersal of species
/****************************************************************************/
/**   fills all grid cells within dispersal range of focal tree            **/
/**   with seeds (seednumberperpatch)                                      **/
/**   Parameters: i, j (int): Position of Tree,                            **/
/**                     tree: focal tree                                   **/
/**									   **/
/**   Rückgabe: keine nötig (verändert die Welt über pointer)              **/
/****************************************************************************/
{
    int o,p, count = 0;
    // iterate all grid-cells around focal tree
    for(int k = i-tree->dispersalrange; k <= i+tree->dispersalrange; k++)
    {
        for(int l = j-tree->dispersalrange; l <= j+tree->dispersalrange; l++)
        {
            if (calculate_distance(i, j, k, l) <= tree->dispersalrange)
            {
                o = k;
                p = l;
                count ++;
                // check periodic boundaries
                if(o < 0){o = welt->size + o;}
                if(o >= welt->size){o = o - welt->size;}
                if(p < 0){p = welt->size + p;}
                if(p >= welt->size){p = p - welt->size;}


                if(welt->grid[o][p]->current_tree == NULL) // fill only empty grid-cells
                {
                    welt->grid[o][p]->seed_array[tree->species] = welt->grid[o][p]->seed_array[tree->species] + tree->seednumberperpatch;
                       // these processes will be implemented later (for second publication):
                       /* Hier sollten eigentlich noch Mechanismen für sameadjustment etc hinein:
                        *      let localseeds seednumberperpatch
                               ;;Prozess 1: ähnliche Schadentoleranz bevorzugen
                               set localseeds localseeds * ( 1 + similaradjustment * ( 1 - abs(shadetolerance - former_shadetolerance) / 100 ) )
                               ;;Prozess 2: gleiche Art bevorzugen
                               if ( species = former_species) [ set localseeds localseeds * (1 + sameadjustment) ]
                       */
                }
            }
        }
    }
 }



void patch_get_seeds(World_t *welt, int i, int j)
// distance = maxdispersal von species
/****************************************************************************/
/**   here the grid cell is in the focus. All trees having the current	   **/
/**   grid-cell within their range fill seeds into its seedbank            **/
/**                                                                        **/
/**   Parameters: i, j (int): Position of Patche                           **/
/**                     					           **/
/**									   **/
/**   Rückgabe: keine nötig (verändert die Welt über pointer)              **/
/****************************************************************************/
{
    int o,p;
    Tree_t *tree;
    // iterate all grid-cells around focal grid-cell
    for(int k = i-welt->maxdispersal; k <= i+welt->maxdispersal; k++)
    {
        for(int l = j-welt->maxdispersal; l <= j+welt->maxdispersal; l++)
        {
	    o = k;
	    p = l;
            // 1. : Flippe Koordinaten an Rändern (periodische Randbedingungen)

            // check periodic boundaries
            if(o < 0){o = welt->size + o;}
            if(o >= welt->size){o = o - welt->size;}
            if(p < 0){p = welt->size + p;}
            if(p >= welt->size){p = p - welt->size;}

           if(welt->grid[o][p]->current_tree != NULL) // iterate over all trees within range max-dispersalrange
           {
              tree = welt->grid[o][p]->current_tree;
              if (calculate_distance(i, j, k, l) <= tree->dispersalrange)
              {
                    welt->grid[i][j]->seed_array[tree->species] = welt->grid[i][j]->seed_array[tree->species] + tree->seednumberperpatch;
                       // all other processes later:
                       /*  Later: Mechanismen for sameadjustment etc will be implemented here:
                        *      let localseeds seednumberperpatch
                               ;;Prozess 1: ähnliche Schadentoleranz bevorzugen
                               set localseeds localseeds * ( 1 + similaradjustment * ( 1 - abs(shadetolerance - former_shadetolerance) / 100 ) )
                               ;;Prozess 2: gleiche Art bevorzugen
                               if ( species = former_species) [ set localseeds localseeds * (1 + sameadjustment) ]
                       */
               }
           }
       }
   }
}

void update_status(World_t *welt)
/************************************************************/
/**  Is  called at the end of every time-step              **/
/**  Turns all seedlings/young trees into adults           **/
/************************************************************/
{
    for(int i = 0; i< welt->size; i++)
    {
        for(int j = 0; j< welt->size; j++)
        {
            if(welt->grid[i][j]->current_tree != NULL) // if tree present on grid-cell
            {
                welt->grid[i][j]->current_tree->status = 2; // 1 = juvenile, 2 = adult, 0 = dead
            }
        }
    }
}
