#folder title - ./version of simulation/(parameters).csv
#format of parameters - population 20, start_infected 2, treatment_time = 10, time_max = 20000, infection_rate = 0.01, time_step = 1 
#file title - number of simulation	
import sys
import os, os.path
import time
import subprocess
import numpy as np
import threading
import datetime


class myThread (threading.Thread):
	def __init__(self, i, cmd):
		threading.Thread.__init__(self)
		self.threadID = i
		self.threadCMD = cmd


	def run(self):
		print("i = ", self.threadID)
		run_simulation(self.threadCMD)		


def run_simulation(cmd):
	print("cmd : ", cmd)
	subprocess.call(cmd, universal_newlines = True)


def test(amount, size, start_infected, infection_rates, time_step, time_max, network_type, folder, virus_types, death_rate_type, amount_of_edges, prob_reconnection, number_of_threads = 2):
	print(folder)
	command = "C:\\Program Files\\R\\R-4.0.3\\bin\\Rscript.exe"
	path2script = "Simulation.R"
	for i in range(0,amount, number_of_threads):
		threads = list()
		for j in range(0, number_of_threads):
			args = [str(size), str(start_infected), str(time_max), infection_rates, str(time_step), str(i + j), folder, str(network_type), virus_types, death_rate_type, str(amount_of_edges), str(prob_reconnection)]
			cmd = [command, path2script] + args
			thread = myThread(i + j, cmd)
			threads.append(thread)
			thread.start()

		for thread in threads:
			thread.join()


def main():

	amount = 30
	#sizes = [20, 100, 200]
	sizes = [100]
	infection_rate_min = 0.001
	infection_rate_max = 0.102
	infection_rate_step = 0.02
	#infection_rate_min = 0.005
	#infection_rate_max = 0.02
	#infection_rate_step = 0.005

	network_types = ["compleet", "barabasi"]
	start_infected = 1
	time_step = 1
	time_max = 100

	for size in sizes:
		for infection_rate in np.arange(infection_rate_min, infection_rate_max, infection_rate_step):
			for network_type in network_types:

				folder = "output_data/simulations/size {}, start_infected {}, infection_rate {}, time_step {}, time_max {}, network_type {}".format(
					size, start_infected, infection_rate, time_step, time_max, network_type)
				#folder2 = "output_data\\simulations\\size {}, start_infected {}, infection_rate {}, time_step {}, time_max {}, network_type {}".format(
				#	size, start_infected, infection_rate, time_step, time_max, network_type)
				start_time = time.time()

				test(amount, size, start_infected, infection_rate, time_step, time_max, network_type, folder)

				s = "size {}, start_infected {}, infection_rate {}, time_step {}, time_max {}, network_type {}, time = {}\n".format(
					size, start_infected, infection_rate, time_step, time_max, network_type, (time.time() - start_time)/(int((amount+1)/3	)))
				f= open("output_data\\time.txt","a+")
				f.write(s)
				f.close() 

#main()

def test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges = [0], number_of_threads = 2, prob_reconnection = 0):
	for infection_rate in range_of_infection_rates:
		infection_rate = round(infection_rate,4)
		#infection_rates = "{} {}". format(infection_rate, round(1.7 * infection_rate, 5))
		infection_rates = str(infection_rate)

		for death_rate_type in death_rate_types:
			for amount_of_edges in range_of_amount_of_edges:
				folder = "output_data/simulations/size {}, start_infected {}, infection_rates {}, time_step {}, time_max {}, network_type {}, amount_of_edges {}, virus_types {} death_rate_type {}, prob_reconnection {}".format(
							size, start_infected, infection_rates, time_step, time_max, network_type, amount_of_edges, virus_types, death_rate_type, prob_reconnection)
			
				test(amount, size, start_infected, infection_rates, time_step, time_max, network_type, folder, virus_types, death_rate_type, amount_of_edges, prob_reconnection, number_of_threads)


amount = 32
size = 200
virus_types = "1"#"1 2"
network_type = "barabasi" #"compleet"#"clustered", "erdos_renyi", "barabasi", "smallworld"
start_infected = 1
time_step = 1
time_max = 100
range_of_infection_rates = list(np.arange(0.02, 0.021, 0.005)) #list(np.arange(0.005, 0.021, 0.005))# + list(np.arange(0.03, 0.09, 0.02)) + list(np.arange(0.1, 0.3, 0.1)) + list(np.arange(0.5, 0.3, 0.1))
#range_of_infection_rates = [0.03, 0.09, 0.015]
#range_of_infection_rates = [0.5, 1.0]

#range_of_amount_of_edges = [size, 2 * size, 5 * size, 10 * size]
range_of_amount_of_edges = [1,2,4,10]
number_of_threads = 4
death_rate_types = ["same"]

# test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads)
# range_of_infection_rates = list(np.arange(0.5, 0.8, 0.2))
# range_of_amount_of_edges = [1,2]
# test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads)
# range_of_infection_rates = list(np.arange(0.3, 0.4, 0.2))
# range_of_amount_of_edges = [1,2,4]
# test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads)
# range_of_infection_rates = list(np.arange(0.003, 0.004, 0.002))
# range_of_amount_of_edges = [10]
# test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads)

'''
network_type = "smallworld"
#range_of_amount_of_edges = [2,4]
range_of_infection_rates = list(np.arange(0.01, 0.021, 0.01)) + list(np.arange(0.03, 0.09, 0.02))#
 + list(np.arange(0.1, 0.4, 0.2)) + list(np.arange(0.5, 1.1, 0.5))
prob_reconnection = 0.1

test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads, prob_reconnection)
'''

amount = 1
size = 200
virus_types = "1"#"1 2"
network_type = "barabasi"#"clustered", "erdos_renyi", "barabasi", "smallworld"
death_rate_types = ["same"] # ["same", "different"]
range_of_infection_rates = [0.1, 0.2]
number_of_threads = 1
range_of_amount_of_edges = [0]
prob_reconnection = 0
#test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads)

#network_type = "smallworld"#"clustered", "erdos_renyi", "barabasi", "smallworld"
#range_of_infection_rates = [0.6]

#range_of_infection_rates = [0.15, 0.2, 0.25, 0.35, 0.4, 0.45, 0.6]
# range_of_infection_rates = list(np.arange(0.15, 0.49, 0.05))
# range_of_amount_of_edges = [1]
# test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads, prob_reconnection)
# amount = 32
# range_of_infection_rates = list(np.arange(0.6, 1.1, 0.2))
# range_of_infection_rates.append(0.5)
# test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads, prob_reconnection)

range_of_infection_rates = [0.2]
network_type = "compleet_same"
test_simulation(size, virus_types, network_type, start_infected, time_step, time_max, death_rate_types, range_of_infection_rates, range_of_amount_of_edges, number_of_threads, prob_reconnection)

