'''
import statements

pandas = pd

'''

import pandas as pd


def load_states():
	'''
	load CSV files to df 

	all_states_df = ALL the states listed by BLS
	states_w_metro_areas_df = ALL states that have a metro area within them

'''
	#create all_states_df by loading it

	all_states_df = pd.read_csv("/Users/rodrigoguerrero/Documents/GitHub/inflation_calculator/full_state_list.csv")
	#print(all_states_df)

	#create states_w_metro_areas_df by loading it

	states_w_metro_areas_df = pd.read_csv("/Users/rodrigoguerrero/Documents/GitHub/inflation_calculator/states_w_metro_areas.csv")
	#print(states_w_metro_areas_df)

	return all_states_df, states_w_metro_areas_df

def map_metro_areas_to_states():

	#load full state list by using load_states() function and selecting the second return value
	states = (load_states()[1])

	#convert states variable to a list

	states = states.values.tolist()

	#make outer dictionary, called geo_data
	geo_data = {'states':states}
	'''

	print(geo_data)
	'''

	#create geo_dic dictionary with states as key

	geo_dic = {'' : {'states':''}} 

	#put states into geo_dic dictionary as a list into the key of states

	geo_dic['states'] = states

	print(geo_data)


	#geo_dic{}

	'''
	states_to_metro_areas = {}

	states_w_metro_areas_df = (load_states()[1])
	print(states_w_metro_areas_df)
	for i in range(len(states_w_metro_areas_df)):
		if i == "Alaska":
			states_to_metro_areas['']
	'''
	#print(Alaska)

def example():
	Dict = { }
	print("Initial nested dictionary:-")
	print(Dict)
	Dict['Dict1'] = {}

	# Adding elements one at a time
	Dict['Dict1']['name'] = 'Bob'
	Dict['Dict1']['age'] = 21
	print("\nAfter adding dictionary Dict1")
	print(Dict)

	# Adding whole dictionary
	Dict['Dict2'] = {'name': 'Cara', 'age': 25}
	print("\nAfter adding dictionary Dict1")
	print(Dict)


def main():
	#print(load_states()[])
	#print(map_metro_areas_to_states())
	map_metro_areas_to_states()
	#example()

if __name__ == '__main__':
		main()
	
