# conv_to_sample_factor.py by Young C. Song (04/09/25)
# This script reads the csv files and convert it to three column
# csv file, where first col is the sample names, second is the factor
# and third is the value. Ideal for generating jitter plots, etc.

from optparse import OptionParser
import string
import os
import re
import sys
import pandas
import configparser
import os.path


# Generate dictionary that represents a converted format of the CSV file
def convert_tbl(csv_path, scale, scale_factor, output_csv):
	converted_dict = {}

	
	csv_read = pandas.read_csv(csv_path, encoding='utf-8', header=0, low_memory=False)

	header_list = csv_read.columns.tolist()
	factor_name = header_list[0]

	factor_list = csv_read[factor_name].tolist()

	max_list = []
	min_list = []

	with open(output_csv, 'w') as output_handle:
		output_handle.write("Sample,Factor,Value\n")
		for i in range(1,len(header_list)):
			sample_name = header_list[i]
			sample_val_list = csv_read[sample_name].tolist()

			max_of_val_list = max(sample_val_list)
			max_list.append(max_of_val_list)

			min_of_val_list = min(sample_val_list)
			min_list.append(min_of_val_list)

			for j in range(0,len(factor_list)):
				factor = factor_list[j]
				factor_val = sample_val_list[j]
				sample_factor_key = sample_name + "," + factor

				output_handle.write("%s,%s\n" % (sample_factor_key,str(factor_val)))
				#print("%s,%s" % (sample_factor_key,str(factor_val)))

			if scale == "yes":
				scale_max = max(max_list) + float(scale_factor)
				scale_min = min(min_list) - float(scale_factor)

				sample_factor_max_key = sample_name + ",scale_max"
				output_handle.write("%s,%s\n" % (sample_factor_max_key,str(scale_max)))
				#print("%s,%s" % (sample_factor_max_key,str(scale_max)))

				sample_factor_min_key = sample_name + ",scale_min"
				output_handle.write("%s,%s\n" % (sample_factor_min_key,str(scale_min)))
				#print("%s,%s" % (sample_factor_min_key,str(scale_min)))
			else:
				print("Scaling option omitted.")



if __name__ == '__main__':
	parser = OptionParser(usage='%prog ...')
	parser.add_option("-c","--config_file", help="Configuration File")
	(options, args) = parser.parse_args()

	config_file = options.config_file

	config = configparser.ConfigParser()
	config.read(config_file)

	working_path = config.get('INPUT', 'Working_Path')
	csv_path     = working_path + "/" + config.get('INPUT', 'CSV_FILE') # full path of your input csv file.
	scale        = config.get('INPUT', 'SCALE')
	scale_factor = config.get('INPUT', 'SCALE_FACTOR')

	output_csv   = working_path + "/"  + config.get('OUTPUT', 'CONV_CSV')
	#mkout_cmd = "mkdir %s" % (output_path)


	if os.path.exists(csv_path):
		print("## The input file, %s exists.\n" % (csv_path))
		print("## Converting input to sampe to factor format.\n")
		#col_names      = get_col_names(csv_path)
		converted_tbl  = convert_tbl(csv_path, scale, scale_factor, output_csv)

			
		#with open(output_file, 'w') as output_handle:
		#	output_handle.write("%s\n" % col_names)
		#	for tbl_key in compressed_tbl.keys():
		#		columns = "\t".join(compressed_tbl[tbl_key])
		#		output_handle.write("%s\t%s\n" % (tbl_key, columns))

	else:
		print("## The input file, %s does not exist.\n" % (csv_path))
