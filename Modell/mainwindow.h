#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <string>

/******************************************************************************/
/**  defines an one-dimensional array for the grid 			     **/
/**  site[t]=1 if grid cell t occupied by monodominant species.		     **/
/**  Realisation of grid-structure: Variable ’num n’ (=Number of neighbours) **/
/**  and with Array 'next'.					             **/
/**  Every grid cell ’t’ has  num_n neighbours, which are saved in           **/
/**  next[t*num_n]... next[(t+1)*num_n-1].				     **/
/**  Order (hypercubic): +x,-x,+y,-y,... direction.			     **/
/**  Access with Makros: 						     **/
/**                                                                          **/
#define INDEX(t, r, nn) ((t)*(nn)+r)
#define NEXT(t, r) next[INDEX(t, r, num_n)]                                  
/**									     **/
/******************************************************************************/
struct elem_struct
{
    int coord_i;                  // coordinates of empty patch (i and j)
    int coord_j;
    struct elem_struct *next;     // pointer to next element
};
typedef struct elem_struct elem_t;



typedef struct
{
    int species;                  // 1 = Monodom, 2-N: other pfts
    int status;                   // 1 = juvenile 2 = adult?
    int dispersalrange;           // Range of dispersal (in patches)
    int dispersalarea;            
    int seednumberperpatch;       // how many seeds are dispersed
    double shadetolerance;        // Describes how shade-tolerant different species are supposed to be (between 0 and 100)
    double similaradjustment;     // How strongly does the species formerly inhabiting the plot influence the chances of a similar species to inhabit that plot?
    double sameadjustment;        // Like similaradjustment, but for same species.

}Tree_t;

typedef struct
{
    int former_species;            // species which has been present in former time-step
    int former_shadetolerance;
    int color;                     // each species is linked to a color, represented as int (0 = no species, 1= monodom 2-8 = other pfts)
    double *seed_array;               // stores data about abundance of seed types during current time-step
    Tree_t* current_tree;          // which tree is currently on patch? If no tree then NULL
    int pos_i, pos_j;              // x und y position auf dem grid.

}Patch_t;



typedef struct
{
    int size;
    double real_propagule;
    elem_t *empty_patches;
    int seed;
    int num_species;                         // how many different species in simulation, default 8; 0 = monodominant species
    Patch_t*** grid;
    int tick;                                // current time in years
    double variation_range;                  // 0 = all "identical" species have the same seed mass
					     // 0.1 = all "identical species" have random a variation of [0..10%] of basic seed mass
    double mortality_rate;                   // annual mortality of all trees (default 0.015)
    double disturbance_chance;               // default 0.0
    double disturbance_severity;
    double disturbance_range;
    int clusteroutput[300];                  // array of clusteroutputs
    int seed_mass;		             // seed mass 
    int maxdispersal;
    char* pfad;
    char* ende;
    int radius; 
    int density_radius;
    int init_option;			     
					     //  initialization modus of the grid
} World_t;


typedef struct
{
    double x;
    double y;
}position_t;


typedef struct
{
    int max;
    int current;
    int* info;
} lstack_t;

typedef struct
{
    int i_coord;    // coordinate (as it is originally found in the grid, not flipped to calculate radius of gyration)
    int j_coord;    // coordinate (as it is originally found in the grid, not flipped to calculate radius of gyration)
    int i_flip;     // 0: not flipped, -1 = i-coordinate - worldsize, 1 = i-coordinate + worldsize
    int j_flip;     // 0: not flipped, -1 = j-coordinate - worldsize, 1 = j-coordinate + worldsize
} gyro_t;

typedef struct
{
    int current;
    gyro_t* info;
    int max;
} gyro_stack_t;

typedef struct
{
    int day;
    int hour;
    int  min;
    int  sec;
    int misec;
}zeit_t;


struct liste_struct
{
    int info;
    struct liste_struct *next;  // pointer to next element
};

typedef struct liste_struct liste_t;


        /***********************************************************************/
        /*  Function headers: */

        /* Struct inits: *****************************/
        Tree_t *new_Tree(int species, int seed_mass, int radius);
        Patch_t *new_Patch(int i, int j, double* startseeds, World_t* welt);
        World_t *new_World(int worldsize, int num_species, double mortality, int seed_mass, int seed, char* pfad, int radius, double prop_factor, int density_radius, int init_option, char *ende);  
        void set_new_tree(World_t* welt, int i,int j, int species);     // puts tree_t species in world (grid[i][j])
        elem_t *create_element(int value1, int value2);
        elem_t* insert_element(elem_t *list, elem_t *elem);
        liste_t *create_liste(int info);
        liste_t *insert_element_liste(liste_t *list, liste_t *elem);
        void delete_histo(liste_t *list);
        void print_list_in_FILE(liste_t *list, FILE *out);
        void delete_liste(elem_t *list);
        void print_list(elem_t *list);
        /*********************************************/

        /* Struct kills: *****************************/
        int kill_Tree(int i, int j, World_t* welt);
        int kill_Patch(int i, int j, World_t* welt);
        int kill_World(World_t *welt);
        /*********************************************/

        /* mono_basic.cpp*/
        int rand_int(int from, int to); 				// Random number generator for integers between two values
        double rand_double();           				// generator for a double random number between 0 and 0.9999999
        double calculate_distance(int i_center, int j_center, int i_pos, int j_pos);
        void print_world(World_t *welt);
        double patch_recruitement_probability(World_t *welt, int i, int j);
        void print_recruitment_wahrscheinlichkeit(World_t *welt);

        void print_histogram(int groesse_max, int* histogram);
        int* new_histogram(int groesse_max, int* histogram);

        /* mono_inits.cpp */
	double*** initialize_seeds(World_t* welt, double*** startgrid);                      // initialises seeds in grid. 
									// init_option: 0 = normal; -1 = streets; 
									// 1 = "checkerboard" with one cell distance; 2 = same as "1", 								                //  dist. 2; 3 = eqiv., dist. 3


        int *make_local_grid(World_t *welt); 				// makes a one dim grid of local densities of monodom. species

        /* mono_routinen.cpp */
        World_t* go(World_t *welt); 					// 1 mortality 2 disturbance 3 recruitment 4 update status 5 clusteroutputlist (it tick)
        void distribute_seeds(World_t *welt);
        elem_t* mortality(World_t *welt);
        void recruitment(World_t *welt);
        void disturbance(World_t *welt);
        void tree_spread_seeds(World_t *welt, int i, int j, Tree_t* tree);
        void patch_get_seeds(World_t *welt, int i, int j);

       /* update status ******************************/
        void update_status(World_t *welt);
        /*********************************************/

        /****** Cluster Algorithm  *******************/
        int percol_cluster(int num_n, int N, int *next, int *site, int *cluster, World_t *welt, FILE *out, FILE *histogramm);
        int size_cluster_noperco(int num_n, int N, int *next, int *site, int *cluster, World_t *welt);
        double radius_of_gyration(int num_n, int N, int *next, int *site, int *cluster, World_t *welt);

        // Stack Routines
        lstack_t* lstack_new(int N);
        void lstack_push(lstack_t *members, int t);                    // actually int *t
        int lstack_is_empty(lstack_t *members);
        int lstack_pop(lstack_t *members);                         

    // actually void und int* current
        void lstack_delete(lstack_t *members);

        //1-D grid routinen
        void two_to_one_dim(World_t *welt, int* one_dim_grid);
        void print_one_dim_grid(int worldsize, int* one_dim_grid);
        void fill_neighbour_grid(int* next, int worldsize);

        // Routines to Show, Print etc
        void print_patch_grid(Patch_t ***grid, int maxsize);
        void print_one_dim_grid_file(int which, World_t *welt, int* one_dim_grid); //which 1 = cluster 2 = local grid
        void print_histogram_in_File(int* histogram, int groesse_max, FILE *out);

        // Gyration-Stack Routines:
        void gyro_stack_delete(gyro_stack_t *members);
        gyro_t gyro_stack_pop(gyro_stack_t *members);
        int gyro_stack_is_empty(gyro_stack_t *members);
        void gyro_stack_push(gyro_stack_t *members, int i, int j, int flip_i, int flip_j);
        gyro_stack_t* gyro_stack_new(int N);


#endif // MAINWINDOW_H



