# organize_tbl_rows_from_tbl.py by Young C. Song (04/14/24)
# provide list of ids and a reference table (TSV) file and
# generates a smaller table that has rows corresponding to your
# input list.

from optparse import OptionParser
import string
import os
import re
import sys
import pandas
import configparser
import os.path
import subprocess


# Using the list of header columns and the target columns, generate a list of indices
# which will be used in the awk command to pull the columns of interest from the input tsv.
def pull_cols(tsv_path, header_list, target_list, output_tsv):
#	tsv_parse = pandas.read_table(tsv_path, sep='\t', encoding='utf-8', header=0)

	awk_target_string = "$1 \"\\t\""

	# This will obtain index of target columns minus the id column
	# Note that in awk, index starts with '1', so whatever index you get from list.index, we have to
	# add 1 to it
	awk_target_list = []
	for target_col in target_list:
		target_index = header_list.index(target_col)
		awk_index   = "$%s" % str(target_index + 1)
		awk_target_list.append(str(awk_index))

	awk_target_concat = " \"\\t\" ".join(awk_target_list)
	awk_target_string = awk_target_string + " " + awk_target_concat
	#print(awk_target_string)

	awk_command = "awk -F \"\\t\" '{print %s}' %s > %s" % (awk_target_string, tsv_path, output_tsv)
	print(awk_command)

	subprocess.call(awk_command, shell=True)

		
# For pulling columns. Assumes that the input table has the header.
def get_header(tsv_path):
	header_list = []
	tsv_parse = pandas.read_table(tsv_path, sep='\t', encoding='utf-8', header=0)

	for col_string in  tsv_parse.columns:
		header_list.append(col_string)

	return header_list

# Parses the table and generates a dictionary, where key is the row id and the values are the list of columns associated
# with the key. If there are duplicate row ids, it will combine the values across the columns.
def organize_tbl_rows(tsv_path):
	ref_parsed_tbl = {}

	tsv_parse = pandas.read_table(tsv_path, sep='\t', encoding='utf-8', header=None, low_memory=False)
	id_field = tsv_parse[0]


	# 'i' is going down the rows; 'j' is going across the columns (starting at col 1, from left to right).
	for i in range(0, len(id_field)):
		ref_id = id_field[i]
		#print(">> %s" % (ref_id))
		value_list = []

		
		for j in range(1, len(tsv_parse.columns)):
			numerical_field  = tsv_parse[j]
			num_val          = numerical_field[i]
			value_list.append(str(num_val))

		if not ref_id in ref_parsed_tbl.keys():
			ref_parsed_tbl[ref_id] = value_list
		else:
			existing_list = ref_parsed_tbl[ref_id]
			for k in range(0, len(existing_list)):
				#print("> %s" % (str(k)))
				curr_val  = float(value_list[k])
				exist_val = float(existing_list[k])

				#print("curr val: %s" % (str(curr_val)))
				#print("exist val: %s" % (str(exist_val)))

				new_val = curr_val + exist_val

				existing_list[k] = str(new_val)

			ref_parsed_tbl[ref_id] = existing_list

	return ref_parsed_tbl


def get_list(target_id):
	target_id_list = []

	with open(target_id, 'r') as input_handle:
		target_id_lines = input_handle.readlines()

		for target_id in target_id_lines:
			target_id = target_id.rstrip()
			target_id_list.append(target_id)

	return target_id_list


def pull_rows(ref_tbl, target_list):
	reduced_tbl = {}

	for target_id in target_list:
		if target_id in ref_tbl.keys():
			ref_tbl_value_list = ref_tbl[target_id]
			reduced_tbl[target_id] = ref_tbl_value_list

	return reduced_tbl


if __name__ == '__main__':
	parser = OptionParser(usage='%prog ...')
	parser.add_option("-c","--config_file", help="Configuration File")
	(options, args) = parser.parse_args()

	config_file = options.config_file

	config = configparser.ConfigParser()
	config.read(config_file)

	working_path = config.get('INPUT', 'Working_Path')
	tsv_path     = working_path + "/" + config.get('INPUT', 'TSV_FILE') # full path of your input tsv file.
	target_id    = working_path + "/" + config.get('INPUT', 'TARGET_ID') # list of ids that match the some of the first column of your TSV
	col_or_row   = config.get('INPUT','COL_OR_ROW')

	output_tsv  = "./"  + config.get('OUTPUT', 'OUTPUT_TSV')


	if os.path.exists(tsv_path):
		target_list = get_list(target_id)
		reduced_tbl = {}
		if col_or_row == 'r':
			print("## The input file, %s exists.\n" % (tsv_path))
			print("## Compressing table based on shared values in the first column.\n")
			ref_tbl = organize_tbl_rows(tsv_path)
			reduced_tbl = pull_rows(ref_tbl, target_list)
		elif col_or_row == 'c':
			header_list = get_header(tsv_path)
			pull_cols(tsv_path, header_list, target_list, output_tsv) # This method asssumes that header exists in the table.

		#for tbl_key in ref_tbl.keys():
		#	columns = "\t".join(ref_tbl[tbl_key])
		#	print("%s\t%s" % (tbl_key, columns))
			
		with open(output_tsv, 'w') as output_handle:
			for tbl_key in reduced_tbl.keys():
				columns = "\t".join(reduced_tbl[tbl_key])
				output_handle.write("%s\t%s\n" % (tbl_key, columns))
	else:
		print("## The input file, %s does not exist.\n" % (tsv_path))
