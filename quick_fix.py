import glob

def process(file):
	change = False
	lines = []
	with open(file) as f:
		lines = f.readlines()
		for i,line in enumerate(lines):
			if "//" in line:
				# if line[-2] is ".":
				# 	change = True
				# 	lines[i] = line[:-2] + "\n"

				if "Cannot use 'super' outside of a class" in line:
					change = True
					lines[i] = line.replace("Cannot use 'super' outside of a class", "Cannot use keyword 'super' outside of a class")
	if change:
		print(file,lines)
		with open(file,"w") as f:
			for line in lines:
				f.write(line)
	#else:
		#print("eee")

#process("test/super/super_in_top_level_function.lox")

for file in glob.glob("test/*/*"):
	process(file)
