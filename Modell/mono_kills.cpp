/*mono_kills.cpp     November 2014 - April 2015   by  Pia Backmann     for Martin */

/** Free the memory after structs are not needed any more and at the end of the simulation  **/


#include "mainwindow.h"
#include "iostream" //if console output is needed
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>


using namespace std;


int kill_Tree(int i, int j, World_t* welt)
{
    free(welt->grid[i][j]->current_tree);
    welt->grid[i][j]->current_tree = NULL;
    welt->grid[i][j]->color = 0;
    return(0);
}

int kill_Patch(int i, int j, World_t* welt)
{
    // 1, befreie tree, dann patch

   // patch = (Patch_t*) malloc(sizeof(Patch_t));
   // patch->seed_array = (int*) malloc(welt->num_species*sizeof(int));
    //patch->current_tree = (Tree_t*) malloc(sizeof(Tree_t));
    kill_Tree(i, j, welt);
    free(welt->grid[i][j]->seed_array);
    free(welt->grid[i][j]);
    return(0);
}

int kill_World(World_t *welt)
{
    // befreie grid aus patches -> befreie trees, dann patches, dann grid, dann welt
     int i, j;
     for(i = 0; i < welt->size; i++)
     {
        for(j = 0; j < welt->size; j++)
        {
            kill_Patch(i,j,welt);
        }
        free(welt->grid[i]);
     }
     delete_liste(welt->empty_patches);
     free(welt->grid);
     //reihenfolge der frees ist entscheidend: alloc: 1,2,3 -> free : 3,2,1 !!!
     free(welt);
     return(0);
}
