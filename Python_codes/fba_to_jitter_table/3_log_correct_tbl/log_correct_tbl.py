# log_correct_tbl by Young C. Song (04/28/25)
# This script reads multi-column files (CSVs) in a designated path and corrects the values
# using natural log or log base 10. See Configuration file for different options.
# Input table should have name and value columns. The value column should also have a header


from optparse import OptionParser
import string
import os
import re
import sys
import pandas
import math
import statistics
import configparser
import os.path

def get_header(csv_file):
	header = ""
	with open(csv_file, 'r') as input_handle:
		header = input_handle.readline()
		header = header.rstrip()

	return header

# Calculate the standard dev. for corrected entry across each row
def calc_stand_dev(entry_list):
	test_list = []
	for entry in entry_list:
		test_list.append(float(entry))

	return round(statistics.stdev(test_list))


# Calculate the average for corrected entry across each row
def calc_avg(entry_list):
	test_list = []
	for entry in entry_list:
		test_list.append(float(entry))

	return round((sum(test_list)/len(test_list)),2)
	


# Log-correct the values in the filtered table 
def log_correct(intermediate_path, log_method, print_avg_stv, adjust, av_threshold, output_path):
	for csv_file in os.listdir(intermediate_path):
		filtered_csv = intermediate_path + "/" + csv_file

		if print_avg_stv == "yes":
			file_header  = get_header(filtered_csv) + ",avg,stdev"
		else:
			file_header  = get_header(filtered_csv)

		file_prefix, file_extension = os.path.splitext(csv_file)
		corrected_csv = output_path + "/" + file_prefix + "_corr.csv"

		print("### Correcting values in the table, %s." % (filtered_csv))
		with open(corrected_csv, "w") as corr_handle:
			corr_handle.write("%s\n" % file_header)
			csv_read = pandas.read_csv(filtered_csv, encoding='utf-8', header=0, low_memory=False)
			csv_col_list = csv_read.columns.tolist()

			first_col = csv_read[csv_col_list[0]].tolist()

			for i in range(0, len(first_col)):
				corr_entry_list = []

				for j in range(1, len(csv_col_list)):
					other_col = csv_read[csv_col_list[j]].tolist()
					entry = other_col[i]
					adjust_entry = float(entry) + float(adjust)
					corr_entry = ""
					if log_method == "BT":
						corr_entry = math.log10(adjust_entry)
					elif log_method == "LN":
						corr_entry = math.log(adjust_entry)

					if corr_entry < 0: # These are originally zero pre-correction, so let's fix 'em back to zero
						corr_entry = 0
					else:
						corr_entry = corr_entry

					corr_entry_list.append(str(round(corr_entry,2)))
			
				average_corr = calc_avg(corr_entry_list)
				stdev_corr   = calc_stand_dev(corr_entry_list)

				if average_corr >= float(av_threshold):
					if print_avg_stv == "yes":
						corr_handle.write("%s,%s,%s,%s\n" % (first_col[i],",".join(corr_entry_list),str(average_corr),str(stdev_corr)))
					else:
						corr_handle.write("%s,%s\n" % (first_col[i],",".join(corr_entry_list)))
					#print("\t%s,%s,%s" % (first_col[i],",".join(corr_entry_list),str(average_corr)))
				else:
					print("\t### Removing the row, %s, as its average is below the proposed threshold." % (first_col[i]))
			#print("\t%s,%s" % (first_col[i],",".join(corr_entry_list)))


# Filter row(s) with negative entries
def filter_table(table_file_path, intermediate_path):
	for csv_file in os.listdir(table_file_path):
		#Get header for each file
		input_file = table_file_path + "/" + csv_file
		file_header = get_header(input_file)
		
		file_prefix, file_extension = os.path.splitext(csv_file)
		#print(file_prefix)
		intermediate_file = intermediate_path + "/" + file_prefix + "_neg_rm.csv"
		#print(intermediate_file)

		# Convert csv file into pandas object
		print("### Scanning %s for negative values." % (input_file))
		with open(intermediate_file,'w') as inter_handle:
			inter_handle.write("%s\n" % file_header)
			csv_read = pandas.read_csv(input_file, encoding='utf-8', header=0, low_memory=False)
			csv_col_list = csv_read.columns.tolist()

			first_col = csv_read[csv_col_list[0]].tolist()

			for i in range(0, len(first_col)): # Going down the table
				neg_marker = 0 # if this marker for a given row becomes > 0, then we filter out that row
				entry_list = []
				for j in range(1, len(csv_col_list)): # Going across the table
					other_col = csv_read[csv_col_list[j]].tolist()
					entry = other_col[i]
					entry_list.append(str(entry))
					if entry < 0:
						print("\t### The entry for the row, '%s' at the column '%s' is negative: %s" % (first_col[i], csv_col_list[j], str(entry)))
						neg_marker += 1
					else:
						print("\t### The entry for the row, '%s' at the column '%s' is positive: %s" % (first_col[i], csv_col_list[j], str(entry)))

				if neg_marker == 0:
					inter_handle.write("%s,%s\n" % (first_col[i], ",".join(entry_list)))
					#print("%s,%s" % (first_col[i], ",".join(entry_list)))
				else:
					print("\t\t### Removing the row, %s from subsequent correction steps." % (first_col[i]))


if __name__ == '__main__':
	parser = OptionParser(usage='%prog ...')
	parser.add_option("-c","--config_file", help="Configuration File")
	(options, args) = parser.parse_args()

	config_file = options.config_file

	config = configparser.ConfigParser()
	config.read(config_file)

	table_file_path = config.get('INPUT', 'INPUT_FOLDER')
	log_method      = config.get('INPUT', 'LOG_METHOD')

	print_avg_stv   = config.get('INPUT', 'AVG_STV')

	av_threshold    = config.get('INPUT', 'AV_THRESHOLD')
	adjust          = config.get('INPUT', 'ADJUST')

	output_path = config.get('OUTPUT', 'OUTPUT_FOLDER')

	if not os.path.exists(table_file_path):
		print("### The specified path, %s doesn't exist.\n" % (table_file_path))
	else:
		#break_to_pieces(column_file_path)
		intermediate_path = "./intermediate_files"
		print("### Pre-filtering tables by removing the rows with negative values.\n")
		if not os.path.exists(intermediate_path):
			os.mkdir(intermediate_path)
			filter_table(table_file_path, intermediate_path)
		else:
			print("### The path %s already exists." % (intermediate_path))

		print("### Log-correcting values in the filtered-tables.")
		if log_method == "BT":
			method_name = "log base 10"
		elif log_method == "LN":
			method_name = "natural log"

		print("\t### We will be using %s to correct the values." % (method_name))
		if not os.path.exists(output_path):
			os.mkdir(output_path)
			log_correct(intermediate_path, log_method, print_avg_stv, adjust, av_threshold, output_path)
		else:
			print("### The path %s already exists." % (output_path))
		
		#print("## Collapsing files in the path, %s" % (intermediate_path))
		#generate_collapse_tables(intermediate_path, collapsed_table_path)
