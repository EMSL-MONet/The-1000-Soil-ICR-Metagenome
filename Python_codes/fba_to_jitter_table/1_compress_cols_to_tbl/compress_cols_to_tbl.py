# compress_cols_to_tbl.py by Young C. Song (05/22/24)
# last edited on 04/24/25
# This script reads multi-column files (TSVs) in a designated path and collapses all the columns
# into a CSV file.
# Input table should have name and value columns. The value column should also have a header


from optparse import OptionParser
import string
import os
import re
import sys
import pandas
import configparser
import os.path

def generate_collapse_tables(intermediate_path, collapsed_table_path):
	collapsed_dict = {}
	sorted_bin_list = []

	column_counter = 0
	for column_file in sorted(os.listdir(intermediate_path)):
		column_counter += 1
		col_file_path = intermediate_path + "/" + column_file

		print("### Processing file, %s." % (col_file_path))

		with open(col_file_path, 'r') as col_file_handle:
			#create a list of bin names, they will server as a header of the table
			header = col_file_handle.readline()
			header = header.rstrip()
			#print(header)
			header_split = header.split("\t")
			sorted_bin_list.append(header_split[1])

			# processing name and the value lines
			for name_value in col_file_handle.readlines():
				column_value_list = [] # store values for each row here
				name_value = name_value.rstrip()
				name_val_split = name_value.split("\t")

				#name becomes the key of collapsed dict
				name_col = name_val_split[0]
				value_col = name_val_split[1]

				if not name_col in collapsed_dict.keys():
					if column_counter > 1:
						num_zeroes = column_counter - 1
						for i in range(0, num_zeroes):
							column_value_list.append(str(0))
					column_value_list.append(value_col)
					collapsed_dict[name_col] = column_value_list
				else:
					curr_list = collapsed_dict[name_col]
					num_zeroes = column_counter - len(curr_list)
					if num_zeroes > 1:
						for i in range(0, num_zeroes-1):
							curr_list.append(str(0))
					curr_list.append(value_col)
					collapsed_dict[name_col] = curr_list

	with open(collapsed_table_path, 'w') as table_handle:
		header_line = "" + "," + ",".join(sorted_bin_list)
		table_handle.write(header_line)
		table_handle.write("\n")
		
		for name_key in collapsed_dict.keys():
			value_list = collapsed_dict[name_key]
			if len(value_list) < column_counter:
				num_zeroes = column_counter - len(value_list)
				for i in range(0, num_zeroes):
					value_list.append(str(0))
			collapsed_dict[name_key] = value_list
			value_line = name_key + "," + ",".join(value_list)
			table_handle.write(value_line)
			table_handle.write("\n")


def break_to_pieces(column_file_path):
	intermediate_path = "./intermediate_files"
	if not os.path.exists(intermediate_path):
		#mkdir = "mkdir %s" % (intermediate_path)
		os.mkdir(intermediate_path)

		file_counter = 0
		for column_file in sorted(os.listdir(column_file_path)):
			file_counter += 1
			col_file_path = column_file_path + "/" + column_file
			print("### Processing file, %s." % (col_file_path))

			file_read = pandas.read_table(col_file_path, sep='\t', encoding='utf-8', header=0, low_memory=False)

			col_name_list = file_read.columns.tolist()
			first_col_name = col_name_list[0]
			first_col_list = file_read[first_col_name].tolist()

			for j in range(1, len(col_name_list)):
				other_col_name = col_name_list[j]
				other_col_list = file_read[other_col_name].tolist()
				inter_file_path = intermediate_path + "/" + str(file_counter) + "_" + other_col_name + ".tsv"
				print("\t### Generating file, %s." % (inter_file_path))

				with open(inter_file_path,'w') as inter_handle:
					header = first_col_name + "\t" + other_col_name
					#print(header)
					inter_handle.write("%s\n" % header)

					for k in range(0, len(other_col_list)):
						line_to_print = first_col_list[k] + "\t" + str(other_col_list[k])
						#print(line_to_print)
						inter_handle.write("%s\n" % line_to_print)
	else:
		print("The path, %s already exists" % (intermediate_path))


#def header_list(column_file_path):
#	collapsed_dict = {}
#	sorted_bin_list = []
	
#	for column_file in sorted(os.listdir(column_file_path)):
#		col_file_path = column_file_path + "/" + column_file
#		with open(col_file_path, 'r') as col_file_handle:
			


if __name__ == '__main__':
	parser = OptionParser(usage='%prog ...')
	parser.add_option("-c","--config_file", help="Configuration File")
	(options, args) = parser.parse_args()

	config_file = options.config_file

	config = configparser.ConfigParser()
	config.read(config_file)

	column_file_path = config.get('INPUT', 'INPUT_FOLDER')
	collapsed_table_path = config.get('OUTPUT', 'COMP_TBL')

	if not os.path.exists(column_file_path):
		print("## The specified path, %s doesn't exist.\n" % (column_file_path))
	else:
		break_to_pieces(column_file_path)
		intermediate_path = "./intermediate_files"
		print("## Collapsing files in the path, %s" % (intermediate_path))
		generate_collapse_tables(intermediate_path, collapsed_table_path)
