# This one drops only the missing values for each point, so point associations are lost
for i in /mnt/Botbot/nitfix/Saxifragales_all_layers_30s/*.asc; do
python3 extract_pointvalues_spatialgrid.py ${i} ./../Fagales_CSVs_ToShare/
done

