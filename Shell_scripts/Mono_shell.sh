#!/bin/bash

radius=20
mortality=0.015
for mass in 9200 9300 9400 9500 9600 9700 
do
	for worldsize in 512
	do
		seed=1
		while [ $seed -le 10 ]
		do

			./Monodominance --size $worldsize -- num_species 8 --mort_rate $mortality --seed_mass $mass --seed $seed --radius $radius  >>"Data/Results/Monodominance_Results_size_"$worldsize"_seedmass_"$mass"_mortality_"$mortality"_radius_"$radius".txt"
			seed=$(( seed+1 ))	 # increments $seed
		done
	done
done
