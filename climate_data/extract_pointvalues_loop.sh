# This one drops only the missing values for each point, so point associations are lost
mkdir pnos_directsampling_no_missing_data_no_point_associations
for i in ./../Saxifragales_all_layers_30s/BIOCLIM*.asc; do
./extract_pointvalues_evenhigherthroughput.py ${i} ./occurrences_cleaning_done/
done



mkdir pnos_directsampling_no_missing_data_no_point_associations
for i in ./../Saxifragales_all_layers_30s/[C-Z]*.asc; do
./extract_pointvalues_evenhigherthroughput.py ${i} ./occurrences_cleaning_done/
done
